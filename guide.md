# FHIRPath

FHIRPath navigates and tests the inside of a resource: "the official name's given names", "is any
component's value over 140". An expression reads a collection and answers a collection; FHIR uses it
for invariants, search parameters, mappings and CQL's paths. This engine passes HL7's suites (R5 1044
of 1044, R4 932 of 934) and UCUM's (573 of 573).

This guide is for writing FHIRPath here: the language in brief, worked examples with their answers,
the ways to run it, and what to do when something goes wrong.

## Official references
- FHIRPath 3.0.0 (the current release; its N1 parts normative): http://hl7.org/fhirpath/
- FHIR's use of FHIRPath (additional functions, `resolve()`, `%resource`): https://hl7.org/fhir/fhirpath.html

## Cheat sheet
One expression per line:
```fhirpath
Patient.name.where(use = 'official').given.first()
Observation.component.where(code.coding.code = '8480-6').value.ofType(Quantity).value > 140
Condition.code.coding.exists(system = 'http://snomed.info/sct')
Patient.birthDate < @1960-01-01
MedicationRequest.dosageInstruction.count()
Observation.value.ofType(Quantity).value
Encounter.period.start.toString().substring(0, 4)
Observation.component.code.coding.where(system = %loinc).code
Patient.name.sort(family desc, given.first())
Patient.select(Coding { system: 'http://terminology.hl7.org/CodeSystem/v2-0203', code: gender })
(42L + 1L).type().name
```
Every value is a collection, and an empty collection is "nothing": a test on nothing is nothing,
not false. `where`, `exists`, `all`, `select`, `first`, `count`, `ofType`, `is` and `as` are the
workhorses. `value` is a choice, so read `value.ofType(Quantity)`, not `valueQuantity`. Strings use
single quotes; dates and times start with `@` (`@2024-01-01`, `@T10:30`); a quantity is a number and
a unit: a UCUM unit in quotes (`5 'mg'`), a calendar duration bare (`3 days`); a Long ends in `L`
(`42L`). `sort()` takes keys, each `asc` (the default) or `desc`. `Type { element: value, … }`
makes a value of a FHIR type (`Period { : }` is an empty one), held to the type: an element it
lacks, or several values for an element of one, is an error. `%loinc`, `%sct` and `%ucum` name the code systems; `%resource` is the
resource the expression started from.

## Worked examples
`patient.json`:
```json
{"resourceType": "Patient", "id": "p1", "birthDate": "1974-12-25",
 "name": [{"use": "official", "family": "Chalmers", "given": ["Peter", "James"]},
          {"use": "usual", "given": ["Jim"]}]}
```
`bp.json`:
```json
{"resourceType": "Observation", "id": "bp", "status": "final",
 "code": {"coding": [{"system": "http://loinc.org", "code": "85354-9"}]},
 "effectiveDateTime": "2024-05-01T09:30:00Z",
 "component": [
   {"code": {"coding": [{"system": "http://loinc.org", "code": "8480-6"}]},
    "valueQuantity": {"value": 152, "unit": "mm[Hg]", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}},
   {"code": {"coding": [{"system": "http://loinc.org", "code": "8462-4"}]},
    "valueQuantity": {"value": 91, "unit": "mm[Hg]", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}}]}
```
```
$ fhirpath "Patient.name.where(use = 'official').given" patient.json
Peter
James
$ fhirpath "Patient.name.given.count()" patient.json
3
$ fhirpath "Patient.birthDate < @1980-01-01" patient.json
true
$ fhirpath "Patient.birthDate" patient.json
@1974-12-25
$ fhirpath "Observation.component.where(code.coding.code = '8480-6').value.ofType(Quantity).value" bp.json
152
$ fhirpath "Observation.component.value.ofType(Quantity).value.all($this < 160)" bp.json
true
$ fhirpath "Observation.component.code.coding.where(system = %loinc).code" bp.json
8480-6
8462-4
$ fhirpath "Observation.effective.toString().substring(0, 4)" bp.json
2024
$ fhirpath "Observation.subject.reference" bp.json
$ fhirpath "Observation.component.first().value.ofType(Quantity)" bp.json
{"code":"mm[Hg]","system":"http://unitsofmeasure.org","unit":"mm[Hg]","value":152}
```
A string prints as itself, a complex item as FHIR JSON, and any other value as FHIRPath writes it
(`152`, `@1974-12-25`, `5 'mg'`). `Observation.subject.reference` prints nothing: the Observation has
no subject, and nothing is not an error.

## Running it
**Command line.** `fhirpath` (or `raco fhirpath`) evaluates one expression over one resource:
```
fhirpath "Patient.name.given" patient.json          # R5 (the default)
fhirpath "Patient.name.given" patient.json --r4     # read the resource as R4
fhirpath "(1 + 2) * 3"                              # no resource: over the empty collection
fhirpath "Patient.name.given.first(" --format json  # an error as JSON (or outcome: an OperationOutcome)
```
A syntax error is positioned (`<expression>:1:25: fhirpath parse error: the expression ends after
`(``); exit status 1 on any error. FHIRPath is one expression: a second one after `;` is a syntax
error at the `;`, and an empty text is `the expression is empty`.

A search parameter that serves several types is one expression, a union of paths each beginning
with its type: `fhirpath "Patient.name.given | Practitioner.name.given" patient.json` prints the
patient's given names (the Practitioner part gives nothing over a Patient). Only as a part of a
`|`: on its own, `Practitioner.name.given` over a Patient is `no element Patient.Practitioner`.

**From Racket.** `(eval-fhirpath (parse-fhirpath text) (list resource))` over a typed resource
(`fhir/serialize/from-json`), after loading a version tower (`fhir/r4/schemas` or
`fhir/r5/schemas`); `#:externals` passes `%` variables by name; `%terminologies` is a terminology
service from fhir-tx. The skill (`.claude/skills/fhirpath`) and the manual have the details.

**In a workspace** (fhir-mcp). A `.fhirpath` file holds one expression. `run` evaluates it over
every resource of the type it starts with (`Patient.name.given` reads every Patient), or over `over`
(a type, or `Type/id`). `explain` gives the value of each part of the expression for one resource,
so the step where a value disappears is visible. `query("fhirpath", "<expression>")` asks without a
file. Resources are read with the workspace's FHIR version (R4 for 4.0.1).

## Common problems
- `direct access to choice element Observation.valueQuantity`: a choice element is read by its name
  and narrowed by type, `Observation.value.ofType(Quantity)`; the `valueX` key is JSON, not FHIRPath.
- `no element Patient.nmae`: the resource's type has no such element; the check runs before anything
  is evaluated (strict mode), so a typo is an error rather than an empty answer.
- Empty where you expected a value: some step reads an element that is absent. Shorten the
  expression from the end until a value appears (or `explain` it in a workspace).
- A comparison is empty, not false: one side is empty (`{} = 1` is `{}`), or two dates differ in
  precision (`@2024 = @2024-01-01` is `{}`). Use `exists()`, `empty()` or `~` when that matters.
- `` `mg` is no calendar duration ``: a bare word after a number is a calendar duration (`3 days`);
  a UCUM unit is quoted, `5 'mg'`.
- `slice() is not implemented here`: FHIR's `htmlChecks()`,
  `elementDefinition()`, `slice()`, `checkModifiers()`, `weight()` and the current build's
  `duration()`, `difference()`, `pathname()` are not; the message says what each would need.
  FHIR's `memberOf(valueset)` is answered by the terminology service given as `%terminologies`
  (true, false, or empty when the value set or a code system it rests on is not there), and so
  are `subsumes(code)` and `subsumedBy(code)` (empty when either side is empty or a code is not
  known; an error between code systems); without a service each is the same kind of error.
  Beyond N1 (FHIRPath 3.0.0) these are implemented: Long literals, `sort()` keys with `asc`/`desc`,
  instance selectors, `coalesce()`, `lastIndexOf()`, `sum()`, `min()`, `max()`,
  `avg()`, `toLong()`, `yearOf()` … `millisecondOf()`, `timezoneOffsetOf()`, `dateOf()`, `timeOf()`,
  `repeatAll()`, `matches(re, flags)`.
- Regular expressions are read as PCRE (a `]` outside a class is a literal); `replaceMatches` names
  groups in the substitution as `$1` or `${name}`, and `&` there is just `&`.
- `unrecognized text `"…"``: strings are in single quotes, `'text'`.
- In a workspace, `say which resources the expression reads`: start the expression with a type, or
  pass `over`.
