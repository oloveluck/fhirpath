#lang scribble/manual
@(require (for-label racket/base racket/contract
                     fhirpath/parser/parse fhirpath/eval fhirpath/value fhirpath/print
                     fhirpath/analyze fhirpath/terminology fhirpath/environment
                     fhirpath/decimal fhirpath/quantity fhirpath/ucum fhirpath/temporal-ops
                     fhirpath/types fhirpath/error fhirpath/workspace fhirpath/support))

@title{fhirpath: FHIRPath}

A @hyperlink["https://hl7.org/fhirpath/"]{FHIRPath} engine over typed FHIR resources: parser,
evaluator, the System types (precision-aware decimals, partial dates and times, quantities with a
full UCUM engine) and a static check for strict mode. It is a library, not a @hash-lang[]: the
@tt{cql}, @tt{fml} and @tt{fsh} packages embed it, and the validator evaluates invariants and search
parameters with it. It passes the official HL7 suites (R5 1044 of 1044, R4 932 of 934) and the UCUM
functional tests (573 of 573). Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir}
family.

@table-of-contents[]

@section{Quick start}

@bold{1. Install.} The packages are not on the Racket package server yet; clone the repositories
side by side and install from source:

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath
}

@bold{2. Evaluate an expression over a resource.}

@verbatim|{
$ raco fhirpath 'Patient.name.given' patient.json
Peter
James
$ raco fhirpath 'birthDate < @1980' patient.json
true
$ raco fhirpath 'Patient.birthDate' patient.json
@1974-12-25
$ raco fhirpath '(1 + 2) * 3'
9
$ raco fhirpath "5 'mg' + 1 'g'"
1005 'mg'
}|

Strings print as themselves, complex items as FHIR JSON, and any other value as FHIRPath writes it
(@tt{152.5}, @tt{@"@"1974-12-25}, @tt{5 'mg'}). R5 by default, @litchar{--r4} for R4. A syntax
error is positioned: @tt{<expression>:1:14: fhirpath parse error: the expression ends after `(`}.

@section{Using it}

@subsection{From Racket}

Expressions evaluate over typed nodes, so load a version tower first (@racket[fhir/r5/schemas],
@racket[fhir/r4/schemas] or @racket[fhir/r6/types]); then parse the resource, parse the expression,
evaluate.

@racketblock[
(require fhir/r5/schemas fhir/serialize/from-json fhir/serialize/read-json fhir/result/result
         fhirpath/parser/parse fhirpath/eval fhirpath/value)
(define patient
  (ok-value (from-json (call-with-input-file "patient.json" read-fhir-json) #:lenient? #t)))
(define given (eval-fhirpath (parse-fhirpath "Patient.name.given") (list patient)))
(map item->system given)                                       (code:comment "'(\"Peter\" \"James\")")
(eval-fhirpath (parse-fhirpath "birthDate < @1980") (list patient))   (code:comment "'(#t)")
]

Every result is a collection, a Racket list; an empty collection is @racket['()]. Items are typed
nodes (navigate on, or @racket[node->jsexpr] them), FHIR primitives, or System values
(@secref["values"]); @racket[item->system] collapses a primitive to its System value.
@racket[parse-fhirpath] is memoized, so parsing inside a loop costs nothing after the first time.

@itemlist[
@item{@bold{Externals} are passed by name without the @tt{%}:
@racket[#:externals (hash "ucum" '("http://unitsofmeasure.org"))]. @racket["terminologies"] may hold
a @racket[tx-service] from fhir-tx, as @racket[(list tx)], which makes @tt{%terminologies.expand()}
and friends work.}
@item{@bold{Strict mode.} With one typed node as input, @racket[eval-fhirpath] first runs
@racket[analyze]: an element the type does not have, or a direct expanded-choice access
(@tt{Observation.valueQuantity}), raises a @racket['semantic] error before anything runs.
@racket[#:strict? #f] allows the choice access, as the spec's lenient mode does.}
@item{@bold{Errors.} A syntax error raises @racket[exn:fail:diagnostic] (@tt{fhir/diagnostic}) with
the position; an evaluation error raises @racket[exn:fp] (@tt{fhirpath/error}) whose
@racket[exn:fp-kind] is @racket['syntax], @racket['semantic] or @racket['execution].}]

@subsection{Command line}

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @tt{raco fhirpath '<expr>' resource.json} "one result per line (R5; --r4 for R4)")
      (list @tt{raco fhirpath '<expr>'} "no resource: over the empty collection")
      (list @tt{raco fhirpath --tck [r5|r4]} "the HL7 FHIRPath suite, floor-ratcheted")
      (list @tt{raco fhirpath --ucum-tck [-v]} "the UCUM functional tests"))]

@litchar{--format text|json|outcome} chooses how an error is reported. Exit codes: 0, 1 on a
FHIRPath error or a suite regression, 2 on usage.

@section{In an editor, for an agent}

@tt{fhirpath/support} is what the family's language server (fhir-lsp) loads for FHIRPath: a
document's diagnostics (one expression; a syntax error positioned). @tt{fhirpath/workspace} is what
fhir-mcp runs for an agent: an expression over the data's resources, and every part of it explained.
It also provides the repository's @tt{guide.md} (the language for agents: a cheat sheet, worked
examples with their exact output, the common problems; its tests replay the examples) as
@racket[guide], which fhir-mcp serves as @tt{learn://fhirpath}. Without MCP, the repository's Claude
skill (@tt{.claude/skills/fhirpath}) gives an agent the same workflow with the command line and the
Racket API.

@section{Limits}

@itemlist[
@item{Two R4 suite cases fail by design: fractional seconds in @tt{@"@"T…+0.1 's'} (R4 truncates,
R5 keeps them; the engine follows R5) and @tt{(-0.0034).lowBoundary(1) = -0.0} (decimals have no
signed zero).}
@item{@tt{resolve()} follows @tt{#id} references into the root's @tt{contained} resources only.
@tt{conformsTo(url)} checks the base type the url names, not a profile's constraints. @tt{trace()}
passes through without logging.}
@item{@tt{%terminologies} has @tt{expand()}, @tt{validateVS()} and @tt{translate()}; not
@tt{lookup()}, @tt{validateCS()}, @tt{subsumes()} or @tt{memberOf()}.}
@item{Not implemented from FHIRPath 3.0: @tt{lastIndexOf}, @tt{coalesce}, @tt{sum}, @tt{min},
@tt{max}, @tt{avg}, @tt{toLong}, the @tt{yearOf} … @tt{millisecondOf} components, @tt{dateOf},
@tt{timeOf}, @tt{duration}, @tt{difference}, @tt{repeatAll}. An unknown function is a
@racket['semantic] error.}
@item{Strict mode is conservative: it stops once a type is unknown (after @tt{resolve()}, a
polymorphic step, or a function it does not model), so it never rejects a valid expression and does
not catch everything.}]

@section{Reference}

@subsection[#:style 'unnumbered]{@tt{fhirpath/parser/parse}}
@defmodule[fhirpath/parser/parse]

@defproc[(parse-fhirpath [str string?]) any/c]{
One expression to its AST (immutable prefab structs). An empty string or a program of several
expressions is an error.}
@defproc[(parse-fhirpath-program [str string?]) list?]{
A @tt{;}-separated program to a list of ASTs. A syntax error raises @racket[exn:fail:diagnostic]
with the position, and "the expression ends after @tt{(}" when the input ran out.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/eval}}
@defmodule[fhirpath/eval]

@defproc[(eval-fhirpath [ast any/c] [root list?]
                        [#:externals externals hash? (hash)]
                        [#:strict? strict? boolean? #t]) list?]{
Evaluates @racket[ast] with @racket[root] as the input collection and returns the result collection.
When @racket[root] is a single typed node, @racket[analyze] checks the expression against its type
first.}
@defproc[(fp-eval [ast any/c] [focus list?] [env env?]) list?]{
The evaluator proper: @racket[ast] over the focus collection in an environment built with
@racket[make-env].}

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
The AST back as text, every nested binary operation parenthesized:
@racket[(fhirpath->string (parse-fhirpath "a and b or c"))] is @racket["(a and b) or c"]. Printing
then parsing is a fixpoint.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/error}}
@defmodule[fhirpath/error]

@defstruct*[(exn:fp exn:fail) ([kind (or/c 'syntax 'semantic 'execution)])]{
An evaluation error: @racket['semantic] for what the expression asks that cannot be (an unknown
element, function or variable; a choice read by its JSON key in strict mode), @racket['execution]
for what the data makes impossible (a singleton expected, a bad conversion).}
@defproc[(fp-error [kind symbol?] [message string?]) any]{Raises an @racket[exn:fp].}

@subsection[#:style 'unnumbered]{@tt{fhirpath/workspace}}
@defmodule[fhirpath/workspace]
@defthing[workspace-support any/c]{The @tt{fhir/lang/workspace} support fhir-mcp loads for FHIRPath.}
@defthing[guide string?]{The repository's @tt{guide.md}: the language for agents (fhir-mcp's
@tt{learn://fhirpath}).}
@subsection[#:style 'unnumbered]{@tt{fhirpath/support}}
@defmodule[fhirpath/support]
@defthing[support any/c]{The @tt{fhir/lang/support} value the language server loads.}

@subsection{The environment}
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
A new variable scope seeded with the current bindings, used at the operands of @tt{|} and per
projection so a @tt{defineVariable} inside a branch does not leak to its siblings.}
@deftogether[(
@defproc[(env-define-var [e env?] [name string?] [coll any/c]) env?]
@defproc[(env-var-bound? [e env?] [name string?]) boolean?])]
@defproc[(env-lookup-external [e env?] [name string?]) (or/c list? #f)]{
Resolves @tt{%name}: a user variable, then a caller external, then the well-known constants
(@tt{context}, @tt{resource}, @tt{rootResource}, @tt{ucum}, @tt{sct}, @tt{loinc}, @tt{vs-X},
@tt{ext-X}). @racket[#f] when nothing matches.}

@subsubsection[#:style 'unnumbered]{@tt{fhirpath/terminology}}
@defmodule[fhirpath/terminology]

@defstruct*[tx-service ([expand procedure?] [validate procedure?] [translate procedure?])]{
The @tt{%terminologies} handle. Each closure returns a collection of typed nodes:
@racket[expand] is @tt{url [version] [params] -> (listof ValueSet)}, @racket[validate] is
@tt{url code [system] -> (listof Parameters)}, @racket[translate] is
@tt{url code [system] -> (listof Parameters)}. The @tt{fhir-tx} package builds one over FHIR
packages.}

@subsection[#:tag "values"]{Values}
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

@subsubsection[#:style 'unnumbered]{@tt{fhirpath/types}}
@defmodule[fhirpath/types]

@deftogether[(
@defproc[(item-is? [item any/c] [type symbol?]) boolean?]
@defproc[(item-of-type? [item any/c] [type symbol?]) boolean?])]{
@tt{is} uses the full ancestry (a @tt{code} is a @tt{string}); @tt{as} and @tt{ofType} use the strict
one. A type is @tt{System.X}, @tt{FHIR.x} or bare (FHIR first, then System).}
@defproc[(known-type? [type symbol?]) boolean?]
@defproc[(fhir-ancestors [type (or/c symbol? #f)]) (listof symbol?)]{Self first: @racket[(fhir-ancestors 'date)] is @racket['(date PrimitiveType DataType Element Base)].}
@defproc[(type-info-of [item any/c]) (values symbol? symbol?)]{Namespace and name for @tt{type()}.}

@subsection{Decimals, quantities, dates}
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
Division by zero is @racket[#f]; comparison is by value, so @racket["1.0"] and @racket["1.00"]
compare equal; rounding is half away from zero, as FHIRPath's @tt{round}.}

@subsubsection[#:style 'unnumbered]{@tt{fhirpath/quantity}}
@defmodule[fhirpath/quantity]

@defproc[(make-quantity [value fp-decimal?] [unit string?]) fp-quantity?]
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

@subsubsection[#:style 'unnumbered]{@tt{fhirpath/ucum}}
@defmodule[fhirpath/ucum]

@defproc[(ucum-valid? [unit string?]) boolean?]{Parses per the UCUM syntax with every atom known. Special units such as @tt{Cel} are valid though they have no linear canonical form.}
@defproc[(ucum-canonical [unit string?]) (or/c (cons/c rational? vector?) #f)]{The magnitude and 7-dimension vector of a unit, or @racket[#f] for a special unit.}
@defproc[(ucum-commensurable? [u1 string?] [u2 string?]) boolean?]
@defproc[(ucum-convert [value rational?] [from string?] [to string?]) (or/c rational? #f)]{Exact: @racket[(ucum-convert 4 "g" "mg")] is @racket[4000].}
@defproc[(ucum-display [unit string?]) (or/c string? #f)]{The display name the UCUM suite expects: @racket["m3.kg-1"] gives @racket["(meter ^ 3) * (kilogram ^ -1)"].}

@subsubsection[#:style 'unnumbered]{@tt{fhirpath/temporal-ops}}
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
