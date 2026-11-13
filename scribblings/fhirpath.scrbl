#lang scribble/manual
@; Text in @tt is shown exactly as written: no curly quotes, and "--" stays two hyphens.
@(require (only-in scribble/core [element literal-element]))
@(define (tt . text) (literal-element (quote tt) text))
@(require (only-in scribble/core element)
          (for-label racket/base racket/contract
                     fhirpath/parser/parse fhirpath/eval fhirpath/value fhirpath/print
                     fhirpath/analyze fhirpath/terminology fhirpath/environment
                     fhirpath/decimal fhirpath/quantity fhirpath/ucum fhirpath/temporal-ops
                     fhirpath/types fhirpath/error fhirpath/render fhirpath/workspace fhirpath/support))
@; fp, sh : an expression or a command in a sentence, exactly as typed (tt would turn -- into a dash
@; and 'x' into curly quotes)
@(define (fp . text) (element 'tt text))
@(define (sh . text) (element 'tt text))

@title{fhirpath: FHIRPath}

A FHIR @italic{resource} is a small document: a patient, a lab result, a prescription. It is
written as JSON, with named parts inside named parts. Sooner or later you need to point at one of
those parts, or ask something about it: "the given names of the official name", "is any reading
above 140". @hyperlink["https://hl7.org/fhirpath/"]{FHIRPath} is the language FHIR has for that. An
expression such as @fp{Patient.name.given} is a path into a resource, and an expression can test
what it finds: @fp{Patient.birthDate < today() - 18 years}.

You meet FHIRPath all over FHIR, because the standard uses it wherever a rule has to refer to the
inside of a resource: the extra rules of a profile ("a contact has a name, a phone, an address or
an organization"), the definition of a search parameter, the rules of a map, the columns of a
view. @secref["where"] shows each.

This package evaluates FHIRPath. You give it an expression and a resource, and it gives the answer.
The answer is always a list, which FHIRPath calls a @italic{collection}: several values, one value,
or none. An expression that names a part the resource cannot have is refused before it runs, with
the name you probably meant.

Part of the @hyperlink["https://oloveluck.github.io/fhir/"]{fhir} family of FHIR languages and tools.

@table-of-contents[]

@section{What you can do with it}

@itemlist[
@item{@bold{Ask a question of one resource} from the command line: @secref["first"].}
@item{@bold{Try an expression out} before you put it in a profile, a search parameter, a map or a
view (@secref["where"]).}
@item{@bold{Find a misspelt name.} An expression is checked against the resource's type before it
runs, so @fp{Patient.nmae} is an error and not a silent empty answer.}
@item{@bold{Compare measurements in different units}: a pressure in mm[Hg] against a limit in kPa
(@secref["units"]).}
@item{@bold{Evaluate expressions from a program} (@secref["racket"]).}]

@section[#:tag "first"]{A first example}

@bold{1. Install} the tools as the @hyperlink["https://oloveluck.github.io/fhir/"]{family's front
page} describes. The command is then @sh{raco fhirpath} (or @sh{fhirpath}, if you built the
standalone tools).

@bold{2. Have a resource.} Here is a patient with two names, an official one and the one he goes
by. Save it as @tt{patient.json}:

@verbatim|{
{"resourceType": "Patient", "id": "p1", "birthDate": "1974-12-25",
 "name": [{"use": "official", "family": "Chalmers", "given": ["Peter", "James"]},
          {"use": "usual", "given": ["Jim"]}]}
}|

@bold{3. Follow a path.} Give the command an expression and the file:

@verbatim|{
$ raco fhirpath "Patient.name.given" patient.json
Peter
James
Jim
}|

Read the expression from the left. @fp{Patient} says what kind of resource this is. @fp{.name}
goes to its names: there are two. @fp{.given} goes to the given names of each of them, and the
answer collects them all. Each value of the answer is printed on a line of its own.

@bold{4. Narrow it.} @fp{where(…)} keeps the items for which a test is true:

@verbatim|{
$ raco fhirpath "Patient.name.where(use = 'official').given" patient.json
Peter
James
}|

Text is written in single quotes inside the expression, so put the whole expression in double
quotes for the shell.

@bold{5. Ask yes or no, or how many.} An expression can compare and count:

@verbatim|{
$ raco fhirpath "Patient.birthDate < today() - 18 years" patient.json
true
$ raco fhirpath "Patient.name.given.count()" patient.json
3
}|

The first reads "was the patient born more than eighteen years before today": is this an adult.

@bold{6. Ask for something that is not there.} This patient has no phone number or e-mail
(@tt{telecom}):

@verbatim|{
$ raco fhirpath "Patient.telecom.value" patient.json
$ raco fhirpath "Patient.telecom.exists()" patient.json
false
}|

The first command prints nothing. That is an answer, the empty collection, and not an error: a
path that leads to nothing gives nothing. When you want a yes or a no, ask with @fp{exists()}.

@bold{7. Make a mistake.} Misspell @tt{name} and the expression is refused, with the name you
probably meant:

@verbatim|{
$ raco fhirpath "Patient.nmae" patient.json
<expression>: fhirpath run error: fhirpath: no element Patient.nmae (did you mean `name`?)
}|

A patient can have no telecom, so step 6 was a fair question. No patient can have a @tt{nmae}, so
this one is a mistake, and it is caught before anything is read. An expression that is not
well formed is reported where it stops:

@verbatim|{
$ raco fhirpath "Patient.name.given.first(" patient.json
<expression>:1:25: fhirpath parse error: the expression ends after `(`
 1 | Patient.name.given.first(
   |                         ^
}|

@section[#:tag "writing"]{Writing an expression}

This section is the language in brief, by example. The examples use the patient above and this
blood-pressure reading, @tt{bp.json}. It is one Observation with two @italic{components}, the
systolic pressure (code 8480-6) and the diastolic (8462-4):

@verbatim|{
{"resourceType": "Observation", "id": "bp", "status": "final",
 "code": {"coding": [{"system": "http://loinc.org", "code": "85354-9"}]},
 "effectiveDateTime": "2024-05-01T09:30:00Z",
 "component": [
   {"code": {"coding": [{"system": "http://loinc.org", "code": "8480-6"}]},
    "valueQuantity": {"value": 152, "unit": "mm[Hg]", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}},
   {"code": {"coding": [{"system": "http://loinc.org", "code": "8462-4"}]},
    "valueQuantity": {"value": 91, "unit": "mm[Hg]", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}}]}
}|

The full language is described at @hyperlink["http://hl7.org/fhirpath/N1"]{hl7.org/fhirpath}, and
what FHIR adds to it at @hyperlink["https://hl7.org/fhir/fhirpath.html"]{hl7.org/fhir/fhirpath.html}.
The repository's @tt{guide.md} has a cheat sheet, each line of it checked by the tests.

@subsection{Paths}

A path is a chain of names separated by dots. Each name is an @italic{element}: a named part of the
resource, as FHIR defines it for that type.

@itemlist[
@item{@fp{Patient.name.family} gives @tt{Chalmers}. The type at the start is optional:
@fp{name.family} is the same question.}
@item{A step applies to every item the step before it found. That is why @fp{Patient.name.given}
gave the given names of both names.}
@item{@fp{name[0].given} takes the first name only (counting starts at 0), and so does
@fp{name.first().given}: @tt{Peter} and @tt{James}.}]

@subsection{Every answer is a collection}

There are no single values in FHIRPath and there is no "null". Every step takes a collection and
gives a collection, and a collection with nothing in it is how the language says "not there".

This has one consequence that surprises almost everyone. The patient above has no @tt{gender}:

@verbatim|{
$ raco fhirpath "Patient.gender = 'female'" patient.json
$ raco fhirpath "Patient.gender.exists()" patient.json
false
}|

The comparison prints nothing. It is not false: the gender is unknown, so whether it equals
@fp{'female'} is unknown too, and FHIRPath answers with the empty collection. A test on nothing is
nothing. When that matters, say what you mean with @fp{exists()} or @fp{empty()}, which always
answer true or false.

@subsection{Filtering, testing, counting}

These functions do most of the work. Each is shown with what it answers for the patient above.

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @bold{expression} @bold{what it does} @bold{answer})
      (list @fp{name.where(use = 'official').given} "keeps the items that pass a test" @tt{Peter, James})
      (list @fp{name.exists(use = 'official')} "is there an item that passes" @tt{true})
      (list @fp{name.all(use.exists())} "do all items pass" @tt{true})
      (list @fp{name.select(given.first())} "something from each item" @tt{Peter, Jim})
      (list @fp{name.given.first()} "the first item" @tt{Peter})
      (list @fp{name.given.count()} "how many" @tt{3})
      (list @fp{name.given.where($this.startsWith('J'))} @elem{@fp{$this} is the item being tested} @tt{James, Jim})
      (list @fp{name.given.join(' ')} "the items as one text" @tt{Peter James Jim})
      (list @fp{telecom.empty()} "is there nothing" @tt{true}))]

@subsection{Text, numbers, dates and times}

@itemlist[
@item{@bold{Text} is in single quotes: @fp{'Peter' + ' ' + 'Chalmers'} is @tt{Peter Chalmers}.}
@item{@bold{Numbers} are written plainly: @fp{(1 + 2) * 3} is @tt{9}, @fp{10 / 4} is @tt{2.5}.}
@item{@bold{Dates and times} start with @tt{@"@"}: @fp{@"@"2024-05-01}, @fp{@"@"T10:30},
@fp{@"@"2024-05-01T09:30:00Z}. You can add to them and compare them:
@fp{@"@"2024-05-01 + 3 days} is @tt{@"@"2024-05-04}. @fp{today()} and @fp{now()} are the present.}
@item{@bold{A date may be partial}, a year or a year and a month. Comparing dates of different
precision has no answer: @fp{@"@"2024 = @"@"2024-01-01} prints nothing, because 2024 could be any
day of that year. Use @fp{~} ("equivalent") to get a plain true or false; here it gives false.}
@item{@bold{A day the calendar does not have} is refused: @fp{@"@"1940-02-30} is the error
@tt{@"@"1940-02-30 names no day: 1940-02 has 29 days}.}]

@subsection{Elements that can be one of several types}

Some elements can hold different kinds of value. An Observation's value can be a quantity, a piece
of text, a code and more. The JSON shows which by the key it uses (@tt{valueQuantity},
@tt{valueString}). FHIRPath calls the element by its one name, @tt{value}, and you say which type
you want with @fp{ofType(…)}:

@verbatim|{
$ raco fhirpath "Observation.component.where(code.coding.code = '8480-6').value.ofType(Quantity).value" bp.json
152
$ raco fhirpath "Observation.component.first().value.ofType(Quantity)" bp.json
{"code":"mm[Hg]","system":"http://unitsofmeasure.org","unit":"mm[Hg]","value":152}
}|

The first asks for the number inside the systolic component's quantity; the second for the whole
quantity, which is printed as the JSON it is. Writing the JSON key is the most common mistake, and
it is reported:

@verbatim|{
$ raco fhirpath "Observation.valueQuantity" bp.json
<expression>: fhirpath run error: fhirpath: direct access to choice element Observation.valueQuantity
}|

@subsection[#:tag "units"]{Quantities and units}

A quantity is a number with a unit in single quotes: @fp{5 'mg'}, @fp{140 'mm[Hg]'}. The units
are spelt in @hyperlink["https://ucum.org/"]{UCUM}, the Unified Code for Units of Measure, which is
the spelling FHIR uses in data (@tt{mg}, @tt{mm[Hg]}, @tt{kPa}, @tt{mmol/L}). Because the engine
knows what each unit means, it converts between units of the same kind:

@verbatim|{
$ raco fhirpath "5 'mg' + 1 'g'"
1005 'mg'
$ raco fhirpath "Observation.component.where(value.ofType(Quantity) > 19 'kPa').code.coding.code" bp.json
8480-6
}|

The second asks which components are above 19 kilopascals. The reading is in millimetres of
mercury; 152 mm[Hg] is a little over 20 kPa, so the systolic component is the answer.

Units of different kinds cannot be compared or added. @fp{1 'kg' > 3 'cm'} prints nothing (there
is no answer), and @fp{1 'kg' + 3 'cm'} is the error @tt{incompatible quantity units}.

Spans of time are written without quotes: @fp{18 years}, @fp{3 days}.

@subsection{Names that start with %}

A name that starts with @tt{%} is a value given from outside the expression. Some come built in:

@itemlist[
@item{@fp{%resource} is the resource the expression started from, wherever in it you are now.}
@item{@fp{%loinc}, @fp{%sct} and @fp{%ucum} are the addresses of three code systems, so
@fp{Observation.component.code.coding.where(system = %loinc).code} gives @tt{8480-6} and
@tt{8462-4}.}
@item{A program can pass its own (@secref["racket"]).}]

@section[#:tag "where"]{Where FHIR uses FHIRPath}

You rarely write FHIRPath for its own sake. You write it inside something else, and the command
line is the place to try it out first.

@itemlist[
@item{@bold{Invariants in profiles.} A @italic{profile} says what a valid resource looks like, and
an @italic{invariant} is a rule in it that the simple checks ("this element is required") cannot
express. FHIR's own rule that an Observation has a value or a reason why not, but never both, is
the invariant @fp{dataAbsentReason.empty() or value.empty()}. An invariant holds when it answers
true:
@verbatim|{
$ raco fhirpath "dataAbsentReason.empty() or value.empty()" bp.json
true
}|
In FHIR Shorthand you write one as an @tt{Invariant:} with an @tt{Expression:}
(@hyperlink["https://oloveluck.github.io/fsh/"]{fsh}), and the family's validator evaluates it with
this engine.}
@item{@bold{Search parameters.} A search such as @tt{Patient?given=Peter} works because FHIR
defines the parameter @tt{given} for patients as the expression @fp{Patient.name.given}: a server
evaluates it over each stored resource and indexes what comes out. A parameter that serves several
types is written with a bar between them, each part beginning with its type. Over a resource of
one of the types, the parts for the others give nothing:
@verbatim|{
$ raco fhirpath "Patient.name.given | Practitioner.name.given" patient.json
Peter
James
Jim
}|
(The bar is a union, so a name that appears twice is given once.)}
@item{@bold{Mapping rules.} In the FHIR Mapping Language the condition of a rule is FHIRPath:
@fp{src.item as item where (linkId = 'gender')}
(@hyperlink["https://oloveluck.github.io/fml/"]{fml}).}
@item{@bold{View columns.} A SQL on FHIR view turns resources into a table, and each column is a
FHIRPath expression over the resource: a column with the path @fp{name.family.first()} holds
@tt{Chalmers} for the patient above
(@hyperlink["https://oloveluck.github.io/sql-on-fhir/"]{sql-on-fhir}).}
@item{@bold{Clinical logic.} CQL, the language for clinical rules and quality measures, builds on
FHIRPath's expressions (@hyperlink["https://oloveluck.github.io/cql/"]{cql}).}]

Within this family there is one FHIRPath engine, this one. The validator, the server's search, the
mapping language, FHIR Shorthand's checks and the views all use it, so an expression means the same
thing wherever you put it.

@section{Running it}

@subsection{Command line}

The command evaluates one expression over one resource.

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @sh{raco fhirpath '<expr>' resource.json} "the answer, one value per line; the resource is read as FHIR R5")
      (list @sh{raco fhirpath '<expr>' resource.json --r4} "the same, the resource read as FHIR R4")
      (list @sh{raco fhirpath '<expr>'} "no resource: the expression runs over the empty collection, which is enough for arithmetic")
      (list @sh{raco fhirpath --tck [r5|r4]} "run HL7's FHIRPath test suite (r5 when not said)")
      (list @sh{raco fhirpath --ucum-tck [-v]} "run UCUM's functional tests"))]

How a value is printed: text as itself; a number, a date, a quantity or true/false the way FHIRPath
writes it (@tt{152}, @tt{@"@"1974-12-25}, @fp{5 'mg'}, @tt{true}); an item that has parts of its own
(a name, a quantity taken from a resource) as its FHIR JSON on one line. An empty answer prints
nothing.

Use @litchar{--r4} for data written to FHIR R4 (version 4.0.1), which is what US Core and the
International Patient Summary use. The versions differ in which elements a type has, so the check
for misspelt names depends on it.

An error is reported as text by default. @litchar{--format json} writes it as JSON and
@litchar{--format outcome} as a FHIR OperationOutcome, for a program to read. The command exits
with 0 when all is well, 1 on a FHIRPath error (or when a test suite does worse than before), and 2
when it was called wrongly or cannot read the file.

FHIRPath is one expression. Two of them, or none, are syntax errors like any other:

@verbatim|{
$ raco fhirpath "Patient.name.given; Patient.birthDate" patient.json
<expression>:1:19: fhirpath parse error: unexpected `;`: one expression is expected, this is 2
 1 | Patient.name.given; Patient.birthDate
   |                   ^
$ raco fhirpath "" patient.json
<expression>: fhirpath parse error: the expression is empty
}|

@subsection[#:tag "racket"]{From Racket}

From a program you do three things: read the resource, read the expression, evaluate. The resource
must be read as a @italic{typed} value, one that knows it is a Patient and what a Patient has, so
load the definitions of a FHIR version first (@racket[fhir/r5/schemas], @racket[fhir/r4/schemas] or
@racket[fhir/r6/types]).

@racketblock[
(require fhir/r5/schemas fhir/serialize/from-json fhir/serialize/read-json fhir/result/result
         fhirpath/parser/parse fhirpath/eval fhirpath/value)
(define patient
  (ok-value (from-json (call-with-input-file "patient.json" read-fhir-json) #:lenient? #t)))
(define given (eval-fhirpath (parse-fhirpath "Patient.name.given") (list patient)))
(map item->system given)                                       (code:comment "'(\"Peter\" \"James\" \"Jim\")")
(eval-fhirpath (parse-fhirpath "birthDate < @1980") (list patient))   (code:comment "'(#t)")
(eval-fhirpath (parse-fhirpath "Patient.telecom") (list patient))     (code:comment "'()")
]

Every result is a collection, a Racket list; the empty collection is @racket['()]. An item is a
typed node (navigate on from it, or turn it into JSON with @racket[node->jsexpr]), a FHIR
primitive, or a plain value (@secref["values"]). @racket[item->system] turns a FHIR primitive into
its plain value, and @racket[item->text] (@racketmodname[fhirpath/render]) prints any item the way
the command line does. @racket[parse-fhirpath] remembers what it has parsed, so parsing inside a
loop costs nothing after the first time.

There is no @hash-lang[] for FHIRPath: an expression is a string you evaluate, not a module.

@itemlist[
@item{@bold{Your own % names} are passed by name without the @tt{%}, each as a collection:
with @racket[#:externals (hash "limit" '(2))], the expression @fp{name.given.count() > %limit}
answers @racket['(#t)] for the patient above.}
@item{@bold{Terminology.} The external @racket["terminologies"] may hold a @racket[tx-service]
from the fhir-tx package, as @racket[(list tx)]. That makes @fp{%terminologies.expand(…)},
@fp{validateVS(…)} and @fp{translate(…)} work: the expression can ask whether a code is in a value
set.}
@item{@bold{The check before running.} With one typed node as input, @racket[eval-fhirpath] first
runs @racket[analyze]. An element the type does not have, or a choice element read by its JSON key
(@fp{Observation.valueQuantity}), raises a @racket['semantic] error before anything is evaluated.
@racket[#:strict? #f] allows the JSON key, as the specification's lenient mode does; a name the
type does not have is still an error.}
@item{@bold{Errors.} A syntax error raises @racket[exn:fail:diagnostic] (@tt{fhir/diagnostic}) with
the position. An evaluation error raises @racket[exn:fp] (@racketmodname[fhirpath/error]), whose
@racket[exn:fp-kind] is @racket['syntax], @racket['semantic] or @racket['execution]: for example
@racket['semantic] for @fp{Patient.nmae}, and @racket['execution] for @fp{name.given.single()} when
there are three.}]

@section{In an editor, and for an AI agent}

@itemlist[
@item{@bold{In an editor.} The family's language server,
@hyperlink["https://oloveluck.github.io/fhir/fhir-lsp/index.html"]{fhir-lsp}, treats a
@tt{.fhirpath} file as one expression and underlines a syntax error where it is. It loads
@racketmodname[fhirpath/support] for this.}
@item{@bold{For an agent.} In an
@hyperlink["https://oloveluck.github.io/fhir/fhir-mcp/index.html"]{fhir-mcp} workspace an expression
runs over every resource of a type at once (@fp{Patient.name.given} reads every Patient), and
@tt{explain} gives the value of each part of the expression for one resource, so you can see the
step where a value disappears. This is @racketmodname[fhirpath/workspace]. The repository's
@tt{guide.md} is the language written for an agent to read (fhir-mcp serves it as
@tt{learn://fhirpath}), and @tt{.claude/skills/fhirpath} is the same workflow for an agent using
the command line.}]

@section{Limits}

@itemlist[
@item{@bold{One expression, one resource.} The command line evaluates a single expression over a
single resource, as FHIR R4 or R5. To ask the same question of many resources, use a workspace or a
program; from a program FHIR R6 works too.}
@item{@bold{Following references.} @fp{resolve()} follows a @tt{#id} reference into the resource's
own @tt{contained} resources only. It does not fetch another resource.}
@item{@bold{Profiles.} @fp{conformsTo(url)} checks that the resource is of the base type the url
names, not that it meets a profile's rules. Use the validator for that.}
@item{@bold{Tracing.} @fp{trace()} passes its input through and logs nothing.}
@item{@bold{Terminology.} @fp{%terminologies} has @fp{expand()}, @fp{validateVS()} and
@fp{translate()}; it does not have @fp{lookup()}, @fp{validateCS()}, @fp{subsumes()} or
@fp{memberOf()}.}
@item{@bold{Newer functions.} These additions of FHIRPath 3.0 are not implemented: @tt{lastIndexOf},
@tt{coalesce}, @tt{sum}, @tt{min}, @tt{max}, @tt{avg}, @tt{toLong}, the @tt{yearOf} …
@tt{millisecondOf} components, @tt{dateOf}, @tt{timeOf}, @tt{duration}, @tt{difference},
@tt{repeatAll}. Using one is the error @tt{unknown function sum()}.}
@item{@bold{The check before running is cautious.} It stops as soon as it no longer knows the type
it is looking at (after @fp{resolve()}, a step that can be several types, or a function it does not
model). So it never refuses a valid expression of that kind, and it does not catch every mistake.}
@item{@bold{A path through another type} gives nothing only as a part of a union, the way search
parameters are written (@fp{Patient.name.given | Practitioner.name.given} over a Patient). On its
own it is refused, as HL7's suite expects: over a Patient, @fp{Practitioner.name.given} is the
error @tt{no element Patient.Practitioner}.}
@item{@bold{Two cases of HL7's R4 suite fail on purpose.} Adding a fraction of a second to a time
(@fp{@"@"T… + 0.1 's'}): R4 drops the fraction and R5 keeps it, and the engine follows R5. And
@fp{(-0.0034).lowBoundary(1)} is expected to be @tt{-0.0}; decimals here have no negative zero.}]

@section[#:tag "words"]{Words used here}

@itemlist[
@item{@bold{Resource}: one FHIR document, such as a Patient or an Observation, written as JSON.}
@item{@bold{Element}: a named part of a resource, as FHIR defines it for the resource's type
(@tt{name}, @tt{birthDate}).}
@item{@bold{Expression}: a piece of FHIRPath; what you give the command. A @bold{path} is the
simplest kind: element names joined by dots.}
@item{@bold{Collection}: a list of values. Every FHIRPath answer is one. The @bold{empty
collection} is how FHIRPath says "nothing there"; it is written @fp{{}}.}
@item{@bold{Choice element}: an element that can hold one of several types, such as an
Observation's @tt{value}. In JSON it is @tt{valueQuantity} or @tt{valueString}; in FHIRPath it is
@fp{value.ofType(Quantity)}.}
@item{@bold{Profile}: a set of rules a resource must follow in some setting, on top of FHIR's own.
An @bold{invariant} is one such rule written as a FHIRPath expression that must answer true.}
@item{@bold{Search parameter}: a name a FHIR server lets you search by (@tt{given},
@tt{birthdate}), defined by the FHIRPath expression that finds its values.}
@item{@bold{Quantity}: a number with a unit. @bold{UCUM}: the Unified Code for Units of Measure,
the standard spelling of units (@tt{mg}, @tt{mm[Hg]}) that FHIR uses.}
@item{@bold{Code system}: where codes come from, such as LOINC or SNOMED CT. A @bold{value set} is
a named set of codes; a @bold{terminology service} answers questions about them.}
@item{@bold{System types}: FHIRPath's own plain values: Boolean, Integer, Long, Decimal, String,
Date, DateTime, Time, Quantity. A FHIR @bold{primitive} (a FHIR @tt{date}, a FHIR @tt{code}) wraps
one of them.}
@item{@bold{Strict mode}: the check of an expression against the resource's type before it runs.}
@item{@bold{External}: a value passed to an expression under a @tt{%} name.}
@item{@bold{Typed node}, @bold{tower}: for programmers. A resource read into a value that knows its
FHIR type is a typed node; the definitions of one FHIR version's types (R4, R5, R6) are a tower.}]

@section{How it is tested}

Against the suites HL7 and UCUM publish, kept in the repository as published:

@itemlist[
@item{HL7's FHIRPath suite for R5: all 1044 tests pass. For R4: 932 of 934; the two that do not are
described under Limits.}
@item{UCUM's functional tests (validity, display names, conversion, arithmetic on units): all 573.}
@item{Every expression in the repository's @tt{guide.md} is parsed and its worked examples are
replayed through the command line, so what it shows is what you get.}]

@sh{raco fhirpath --tck r5}, @sh{raco fhirpath --tck r4} and @sh{raco fhirpath --ucum-tck} run the
suites; each fails if the count drops below what it was. @sh{raco test .} in the repository runs
the unit tests that every module carries. The repository's @tt{TESTING.md} has the table.

@section{Reference}

For programmers: the modules of the package and what each provides.

@subsection[#:style 'unnumbered]{@tt{fhirpath/parser/parse}}
@defmodule[fhirpath/parser/parse]

@defproc[(parse-fhirpath [str string?]) any/c]{
One expression to its AST (immutable prefab structs). A text with no expression in it ("the
expression is empty") or with several separated by @tt{;} (positioned at the first @tt{;}) is a
syntax error: it raises @racket[exn:fail:diagnostic], as any other does.}
@defproc[(parse-fhirpath-program [str string?]) list?]{
A @tt{;}-separated program to a list of ASTs, at least one. A syntax error raises
@racket[exn:fail:diagnostic] with the position, "the expression ends after @tt{(}" when the input
ran out, and "the expression is empty" when there is nothing to parse.}

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
The static pass: raises a @racket['semantic] error for an element the type does not have (with the
nearest name), a string function applied to a complex type, an order-dependent function on an
unordered source, and in strict mode an expanded choice access. It looks inside both operands of an
operator and inside function arguments such as @tt{where}'s. It stops wherever the type becomes
unknown, so it never rejects a valid expression it cannot fully type. A part of a union (@tt{|})
that begins with another resource type's name is checked against that type, as a search parameter
of several types is written; anywhere else such a name is an element the type does not have.}
@defproc[(path-head [ast any/c]) (or/c symbol? #f)]{
The name a path begins with, seen through the steps, functions, indexers and type operators applied
to it (@racket['Practitioner] for @fp{Practitioner.name.where(use = 'official')[0]}); @racket[#f]
for an expression that does not begin with a name.}
@defproc[(union-branch [ast any/c]) any/c]{
A part of a union as the evaluator runs it: when it begins with a resource type's name, that name
selects the input of that type (@fp{Practitioner.name.given} becomes
@fp{ofType(Practitioner).name.given}), so over a resource of another type the part is empty and
the rest of its path runs over nothing. Any other expression is returned as it is.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/print}}
@defmodule[fhirpath/print]

@defproc[(fhirpath->string [ast any/c]) string?]{
The AST back as text, every nested binary operation parenthesized:
@racket[(fhirpath->string (parse-fhirpath "a and b or c"))] is @racket["(a and b) or c"]. Printing
then parsing is a fixpoint.}

@subsection[#:style 'unnumbered]{@tt{fhirpath/render}}
@defmodule[fhirpath/render]

@defproc[(item->text [item any/c]) string?]{
One item of a result as the command line prints it: a complex item as FHIR JSON, any other value as
FHIRPath writes it (@racket["152.5"], @racket["@1974-12-25"], @racket["5 'mg'"],
@racket["System.Integer"]).}
@defproc[(item->json [item any/c]) jsexpr?]{
The same for a JSON answer: FHIR JSON for a complex item; a number, a boolean or a string stays
one; a date or a quantity is its text.}

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

What an expression can see while it runs.

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

What the items of a result are. An item is a typed @racket[fhir] node, a FHIR primitive, or a
System value: a boolean, an exact integer, an @racket[fp-long], an @racket[fp-decimal], a string, a
@racket[moment], or an @racket[fp-quantity].

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

The arithmetic under the values: exact decimals that remember how they were written, quantities
with units, and partial dates and times.

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
to the least precision. Rendering is @fp{value 'unit'}, or @tt{value unit} for a calendar duration.}

@subsubsection[#:style 'unnumbered]{@tt{fhirpath/ucum}}
@defmodule[fhirpath/ucum]

@defproc[(ucum-valid? [unit string?]) boolean?]{Parses per the UCUM syntax with every atom known. Special units such as @tt{Cel} are valid though they have no linear canonical form.}
@defproc[(ucum-suggestion [unit string?]) string?]{For a unit that is not UCUM, what to write
instead, as the end of a message: @racket[(ucum-suggestion "mmHg")] is
@racket[" (mercury is written mm[Hg])"], @racket[(ucum-suggestion "msec")] is
@racket[" (did you mean `ms`?)"]. The validator, FHIR Shorthand and CQL all say it this way.}
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
@defproc[(calendar-problem [m (or/c moment? #f)]) (or/c string? #f)]{
Why a moment names no day, or @racket[#f] when it does: for @tt{1940-02-30},
@racket["1940-02 has 29 days"]. The literal grammar allows day 31 in every month; this is the check
of the day against the month and the leap year.}
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
