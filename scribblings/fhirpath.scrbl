#lang scribble/manual
@(require (for-label racket/base racket/contract
                     fhirpath/parser/parse fhirpath/eval fhirpath/value fhirpath/print
                     fhirpath/analyze fhirpath/terminology fhirpath/environment
                     fhirpath/decimal fhirpath/quantity fhirpath/ucum fhirpath/temporal-ops
                     fhirpath/types))

@title{fhirpath: FHIRPath for Racket}

A @hyperlink["https://hl7.org/fhirpath/"]{FHIRPath} engine over the typed node model of the
@hyperlink["https://github.com/oloveluck/fhir"]{fhir} package: parser, evaluator, the System types
(precision-aware decimals, partial dates and times, quantities with a full UCUM engine) and a static
analysis for strict mode. It passes the official HL7 suites (R5 1044/1044, R4 932/934) and the UCUM
functional tests (573/573).

The package is a library: it has no @hash-lang[]. The @tt{cql}, @tt{fml} and @tt{fsh} packages embed it, and
@tt{fhir-validation} evaluates invariants and search parameters with it.

@table-of-contents[]

@section{Install and use}

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath      # from source, with the fhir repo alongside
}

Expressions navigate typed @racket[fhir] nodes, so a FHIR version tower must be loaded first:
@racket[(require fhir/r5/schemas)] (or @racket[fhir/r4/schemas], @racket[fhir/r6/types]) registers the
types.

@racketblock[
(require fhir/r5/schemas fhir/serialize/from-json fhirpath/parser/parse fhirpath/eval)
(define patient (ok-or-raise (from-json (string->jsexpr "{\"resourceType\":\"Patient\",\"name\":[{\"given\":[\"Peter\"]}]}"))))
(eval-fhirpath (parse-fhirpath "Patient.name.given") (list patient))   (code:comment "a collection: a list of items")
]

A collection is a Racket list. Its items are typed nodes, FHIR primitives, or System values
(@secref["values"]). An error during evaluation raises @racket[exn:fp] with a @racket[kind] of
@racket['syntax], @racket['semantic] or @racket['execution]; a parse error raises
@racket[exn:fail:diagnostic] (@tt{fhir/diagnostic}), positioned in the expression.

@section{Parsing}
@defmodule[fhirpath/parser/parse]

@defproc[(parse-fhirpath [str string?]) any/c]{
Parses one expression to its AST (immutable prefab structs). Memoized, so the same expression parsed
repeatedly (invariants, search parameters) costs one parse. An empty string or a program of several
expressions is an error.}

@defproc[(parse-fhirpath-program [str string?]) list?]{
Parses a @tt{;}-separated program to a list of ASTs. A syntax error raises
@racket[exn:fail:diagnostic] with the position, and "the expression ends after @tt{(}" when the input
ran out.}

@section{Evaluation}
@defmodule[fhirpath/eval]

@defproc[(eval-fhirpath [ast any/c] [root list?]
                        [#:externals externals hash? (hash)]
                        [#:strict? strict? boolean? #t]) list?]{
Evaluates @racket[ast] with @racket[root] as the input collection and returns the result collection.
@racket[externals] maps names (without the @tt{%}) to collections; @racket["terminologies"] may be a
@racket[tx-service]. When @racket[root] is a single typed node, @racket[analyze] first checks the
expression against that type (@racket[strict?] forbids direct access to an expanded choice element
such as @tt{Observation.valueQuantity}).}

@defproc[(fp-eval [ast any/c] [focus list?] [env env?]) list?]{
The evaluator proper: @racket[ast] over the focus collection in an environment built with
@racket[make-env]. @racket[eval-fhirpath] wraps it.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/analyze}}
@defmodule[fhirpath/analyze]

@defproc[(analyze [ast any/c] [root-type (or/c symbol? #f)] [#:strict? strict? boolean? #t]) void?]{
The static pass: raises a @racket['semantic] error for an element the type does not have, a string
function applied to a complex type, an order-dependent function on an unordered source, and in strict
mode an expanded choice access. It stops wherever the type becomes unknown, so it never rejects a valid
expression it cannot fully type.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/print}}
@defmodule[fhirpath/print]

@defproc[(fhirpath->string [ast any/c]) string?]{
The AST back as text. Parentheses are conservative: every operand that is itself a binary operation is
wrapped, so @racket[(fhirpath->string (parse-fhirpath "a and b or c"))] is @racket["(a and b) or c"].
Printing then parsing is a fixpoint.}

@section{The environment}
@defmodule[fhirpath/environment]

@defstruct*[env ([this list?] [index exact-nonnegative-integer?] [total list?] [vars hash?]
                 [externals hash?] [root list?])]{
The evaluation context: the focus (@tt{$this}), the iteration index (@tt{$index}), the aggregate
accumulator (@tt{$total}), the variables @tt{defineVariable} bound, the caller's externals, and the
root collection (@tt{%context}, @tt{%resource}).}

@defproc[(make-env [root any/c] [#:root root* any/c root] [#:externals externals hash? (hash)]) env?]{
A fresh environment whose focus is @racket[root]. The variable table is shared by every environment
derived from it, so a @tt{defineVariable} binding stays visible for the rest of the expression.}

@deftogether[(
@defproc[(env-with-item [e env?] [item any/c] [index exact-nonnegative-integer?]) env?]
@defproc[(env-with-this [e env?] [coll list?]) env?]
@defproc[(env-with-total [e env?] [total any/c]) env?])]{
The same environment with a new focus item and index, a new focus collection, or a new accumulator.}

@defproc[(env-clone-vars [e env?]) env?]{
A new variable scope seeded with the current bindings. Used at the operands of @tt{|} and per
projection, so a @tt{defineVariable} inside a branch does not leak to its siblings.}

@deftogether[(
@defproc[(env-define-var [e env?] [name string?] [coll any/c]) env?]
@defproc[(env-var-bound? [e env?] [name string?]) boolean?])]{
Bind a variable in the current scope; test whether a name is bound.}

@defproc[(env-lookup-external [e env?] [name string?]) (or/c list? #f)]{
Resolves @tt{%name}: a user variable, then a caller external, then the well-known constants
(@tt{context}, @tt{resource}, @tt{rootResource}, @tt{ucum}, @tt{sct}, @tt{loinc}, @tt{vs-X},
@tt{ext-X}). @racket[#f] when nothing matches.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/terminology}}
@defmodule[fhirpath/terminology]

@defstruct*[tx-service ([expand procedure?] [validate procedure?] [translate procedure?])]{
The @tt{%terminologies} handle. Each closure returns a collection of typed nodes:
@racket[expand] is @tt{url [version] [params] -> (listof ValueSet)}, @racket[validate] is
@tt{url code [system] -> (listof Parameters)}, @racket[translate] is
@tt{url code [system] -> (listof Parameters)}. The @tt{fhir-validation} package's
@tt{fhir/terminology/service} builds one over FHIR packages.}

@section[#:tag "values"]{Values}
@defmodule[fhirpath/value]

An item is a typed @racket[fhir] node, a FHIR primitive, or a System value: a boolean, an exact
integer, an @racket[fp-long], an @racket[fp-decimal], a string, a @racket[moment], or an
@racket[fp-quantity].

@defstruct*[fp-quantity ([value fp-decimal?] [unit string?] [calendar? boolean?])]{
A System.Quantity; @racket[unit] is a UCUM code or a calendar keyword (@racket["year"] …).}
@defstruct*[fp-long ([v exact-integer?])]{A System.Long (R5), distinct from Integer for @tt{is}.}
@defstruct*[typeinfo ([namespace symbol?] [name symbol?])]{The result of @tt{type()}.}

@defproc[(item->system [item any/c]) any/c]{A FHIR primitive collapsed to its System value; System values pass through. Quantity-like nodes become an @racket[fp-quantity].}
@deftogether[(
@defproc[(item-fhir-type [item any/c]) (or/c symbol? #f)]
@defproc[(item-system-type [item any/c]) (or/c symbol? #f)])]{
The FHIR type of an item (@racket[#f] for a System value), and its System type
(@racket['Boolean], @racket['Integer], @racket['Long], @racket['Decimal], @racket['String],
@racket['Date], @racket['DateTime], @racket['Time], @racket['Quantity]; @racket[#f] for a complex
node).}
@deftogether[(
@defproc[(system-value? [x any/c]) boolean?]
@defproc[(prim-item? [item any/c]) boolean?]
@defproc[(value-less? [item any/c]) boolean?])]{
Is @racket[x] a System scalar; is @racket[item] a FHIR primitive; is it a primitive with no value
(extensions only).}
@defproc[(as-decimal [item any/c]) (or/c fp-decimal? #f)]{A numeric item as a decimal, else @racket[#f].}

@subsection[#:style 'unnumbered]{@tt{fhirpath/types}}
@defmodule[fhirpath/types]

@deftogether[(
@defproc[(item-is? [item any/c] [type symbol?]) boolean?]
@defproc[(item-of-type? [item any/c] [type symbol?]) boolean?])]{
@tt{is} uses the full ancestry (a @tt{code} is a @tt{string}); @tt{as} and @tt{ofType} use the strict
one. A type is @tt{System.X}, @tt{FHIR.x} or bare (FHIR first, then System).}
@defproc[(known-type? [type symbol?]) boolean?]{Is the specifier a real FHIR or System type.}
@defproc[(fhir-ancestors [type (or/c symbol? #f)]) (listof symbol?)]{The ancestor chain, self first: @racket[(fhir-ancestors 'date)] is @racket['(date PrimitiveType DataType Element Base)].}
@defproc[(type-info-of [item any/c]) (values symbol? symbol?)]{Namespace and name for @tt{type()}.}

@section{Decimals, quantities, dates}
@defmodule[fhirpath/decimal]

@defstruct*[fp-decimal ([rat rational?] [scale exact-nonnegative-integer?])]{
An exact value with the number of fractional digits it was written with. Addition and subtraction keep
the larger scale, multiplication the sum, division a working precision of at least 8.}
@deftogether[(
@defproc[(parse-decimal [s string?]) (or/c fp-decimal? #f)]
@defproc[(integer->decimal [n exact-integer?]) fp-decimal?]
@defproc[(decimal->exact [d fp-decimal?]) rational?]
@defproc[(decimal->string [d fp-decimal?]) string?]
@defproc[(decimal-precision [d fp-decimal?]) exact-nonnegative-integer?])]{
Conversions; @racket[decimal->string] keeps the scale's trailing zeros.}
@deftogether[(
@defproc[(decimal-add [a fp-decimal?] [b fp-decimal?]) fp-decimal?]
@defproc[(decimal-sub [a fp-decimal?] [b fp-decimal?]) fp-decimal?]
@defproc[(decimal-mul [a fp-decimal?] [b fp-decimal?]) fp-decimal?]
@defproc[(decimal-div [a fp-decimal?] [b fp-decimal?]) (or/c fp-decimal? #f)]
@defproc[(decimal-negate [d fp-decimal?]) fp-decimal?]
@defproc[(decimal-cmp [a fp-decimal?] [b fp-decimal?]) (or/c -1 0 1)]
@defproc[(decimal-round [d fp-decimal?] [precision exact-nonnegative-integer?]) fp-decimal?])]{
Arithmetic. Division by zero is @racket[#f]; comparison is by value, so @racket["1.0"] and
@racket["1.00"] compare equal; rounding is half away from zero, as FHIRPath's @tt{round}.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/quantity}}
@defmodule[fhirpath/quantity]

@defproc[(make-quantity [value fp-decimal?] [unit string?]) fp-quantity?]{A quantity; @racket[calendar?] follows the unit.}
@defproc[(calendar-unit? [unit string?]) boolean?]{@racket["year"] … @racket["millisecond"], singular or plural.}
@deftogether[(
@defproc[(quantity-compare [a fp-quantity?] [b fp-quantity?]) (or/c -1 0 1 'empty)]
@defproc[(quantity-equiv? [a fp-quantity?] [b fp-quantity?]) boolean?]
@defproc[(quantity-add [a fp-quantity?] [b fp-quantity?]) (or/c fp-quantity? #f)]
@defproc[(quantity-sub [a fp-quantity?] [b fp-quantity?]) (or/c fp-quantity? #f)]
@defproc[(quantity->string [q fp-quantity?]) string?])]{
Comparison and arithmetic convert through UCUM; @racket['empty] or @racket[#f] when the units are not
commensurable. Calendar years and months compare only with the same keyword. Equivalence rounds both
to the least precision. Rendering is @tt{value 'unit'}, or @tt{value unit} for a calendar duration.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/ucum}}
@defmodule[fhirpath/ucum]

@defproc[(ucum-valid? [unit string?]) boolean?]{Parses per the UCUM syntax with every atom known. Special units such as @tt{Cel} are valid though they have no linear canonical form.}
@defproc[(ucum-canonical [unit string?]) (or/c (cons/c rational? vector?) #f)]{The magnitude and 7-dimension vector of a unit, or @racket[#f] for a special unit.}
@defproc[(ucum-commensurable? [u1 string?] [u2 string?]) boolean?]{Same dimensions.}
@defproc[(ucum-convert [value rational?] [from string?] [to string?]) (or/c rational? #f)]{Exact conversion: @racket[(ucum-convert 4 "g" "mg")] is @racket[4000].}
@defproc[(ucum-display [unit string?]) (or/c string? #f)]{The display name the UCUM suite expects: @racket["m3.kg-1"] gives @racket["(meter ^ 3) * (kilogram ^ -1)"].}

@subsection[#:style 'unnumbered]{@tt{fhirpath/temporal-ops}}
@defmodule[fhirpath/temporal-ops]

@defstruct*[moment ([kind (or/c 'date 'dateTime 'time)] [year any/c] [month any/c] [day any/c]
                    [hour any/c] [minute any/c] [second any/c] [frac any/c] [tzmin any/c] [prec exact-integer?])]{
A partial date, time or dateTime. Absent components are @racket[#f]; @racket[prec] is 1 (year) to 6
(second); @racket[frac] is the fractional-second digits as a string; @racket[tzmin] is the offset in
minutes or @racket[#f].}
@deftogether[(
@defproc[(parse-fp-date [s string?]) (or/c moment? #f)]
@defproc[(parse-fp-datetime [s string?]) (or/c moment? #f)]
@defproc[(parse-fp-time [s string?]) (or/c moment? #f)]
@defproc[(value->moment [v any/c]) (or/c moment? #f)]
@defproc[(moment->string [m moment?]) string?])]{
Parse a literal without its @tt{@"@"}; convert a stored FHIR temporal; render.}
@deftogether[(
@defproc[(moment-compare [a moment?] [b moment?]) (or/c -1 0 1 'empty)]
@defproc[(moment-equal? [a moment?] [b moment?]) (or/c 'true 'false 'empty)]
@defproc[(moment-equiv? [a moment?] [b moment?]) boolean?])]{
FHIRPath's partial-precision rules: differing precision is @racket['empty]; equivalence compares to
the least precision. Timezones normalize only when both operands carry a time.}
@deftogether[(
@defproc[(moment-plus [m moment?] [n exact-integer?] [unit string?]) moment?]
@defproc[(moment-add-ms [m moment?] [ms exact-integer?]) moment?]
@defproc[(moment-boundary [m moment?] [digits exact-integer?] [low? boolean?]) (or/c moment? #f)])]{
Add @racket[n] calendar or time units at the moment's precision; add milliseconds, carrying through
the date; the low or high boundary filled to the precision @racket[digits] implies.}

@section{Command line}

@verbatim{
raco fhirpath '<expression>' [resource.json] [--r4|--r5]   evaluate; one result per line
raco fhirpath --tck [r5|r4]                               the HL7 FHIRPath suite (floor-ratcheted)
raco fhirpath --ucum-tck [-v]                             the UCUM functional tests
}

The tower is R5 unless @tt{--r4}. With no resource the expression runs over the empty collection.
Primitives print as text, complex items as FHIR JSON. @tt{--format text|json|outcome} chooses how a
parse or evaluation error is reported (text on stderr; JSON or an OperationOutcome on stdout). Exit
codes: 0, 1 on a FHIRPath error or a suite regression, 2 on usage.
