# fhirpath — FHIRPath

A FHIRPath engine: parser, evaluator, full UCUM (unit validation, canonicalisation, conversion),
precision-aware decimals and partial dates and times, over FHIR R4 and R5 resources. It passes the
official HL7 suites: R5 1044/1044, R4 932/934, UCUM 573/573. The validator, CQL, FML and FSH build on
it.

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

`fhirpath` is one of the family's command-line tools, which run without Racket:
[build them once](https://github.com/oloveluck/fhir#getting-the-tools) and put `dist/bin` on your
`PATH`. With Racket and the packages installed, `raco fhirpath` runs the same commands.

```sh
fhirpath 'Patient.name.given' patient.json                                    # R5 (the default)
fhirpath "Patient.name.where(family = 'Chalmers').given.first()" patient.json --r4
fhirpath '1 + 2 * 3'                                                          # no resource needed
```

Each result is printed on its own line, primitives as their values and complex values as FHIR JSON:

```
$ fhirpath 'Patient.name.given' patient.json
Peter
James
```

A syntax error is positioned in the expression, with exit status 1:

```
$ fhirpath 'Patient.name.(' patient.json
<expression>:1:14: fhirpath parse error: unexpected `(`
 1 | Patient.name.(
   |              ^
```

## In Racket

The engine is a library: parse an expression once, then evaluate it against typed FHIR resources.

```racket
#lang racket/base
(require (only-in fhir/r5/schemas)                    ; register the FHIR R5 types (fhir/r4/schemas for R4)
         fhir/serialize/from-json fhir/result/result
         fhirpath/parser/parse fhirpath/eval fhirpath/value)

(define patient
  (ok-value (from-json (hasheq 'resourceType "Patient"
                               'name (list (hasheq 'family "Chalmers" 'given (list "Peter" "James")))))))

(map item->system (eval-fhirpath (parse-fhirpath "Patient.name.given") (list patient)))  ; '("Peter" "James")
```

`eval-fhirpath` takes `#:externals` for `%`-variables, among them `%terminologies`, a terminology
service from `fhir/terminology/service` (the fhir-tx package) that backs `expand()`, `validateVS()`
and `translate()`. `fhirpath->string` (`fhirpath/print`) writes an AST back as canonical text.

## Develop

From source, with the family's repositories side by side:

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec .
raco test .                   # unit tests
raco fhirpath --tck r5        # the official FHIRPath suite (floor 1044; r4: 932)
raco fhirpath --ucum-tck      # the UCUM functional tests (floor 573)
```

Reference documentation: [oloveluck.github.io/fhirpath](https://oloveluck.github.io/fhirpath/).
Architecture notes: `CLAUDE.md`; the family's conventions and conformance table are in the fhir
repository's `CLAUDE.md`. License: MIT (vendored test corpora keep their own terms, see `tests/vendor`).
