# fhirpath — FHIRPath

A [FHIRPath](https://hl7.org/fhirpath/) engine with full UCUM unit support, over FHIR R4 and R5
resources. It passes HL7's official suites; [TESTING.md](TESTING.md) has the details.

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

[Build the family's tools](https://github.com/oloveluck/fhir#getting-the-tools) once and put
`dist/bin` on your `PATH`; they run without Racket. With Racket installed, `raco fhirpath` is the same.

```sh
fhirpath 'Patient.name.given' patient.json           # R5, the default
fhirpath 'Patient.name.given.first()' patient.json --r4
fhirpath '1 + 2 * 3'
```

```
$ fhirpath 'Patient.name.given' patient.json
Peter
James
```

Each result is on its own line. A syntax error points at its column, with exit status 1.

## In Racket

```racket
#lang racket/base
(require (only-in fhir/r5/schemas) fhir/serialize/from-json fhir/result/result
         fhirpath/parser/parse fhirpath/eval fhirpath/value)

(define patient (ok-value (from-json (hasheq 'resourceType "Patient"
                                             'name (list (hasheq 'given (list "Peter" "James")))))))
(map item->system (eval-fhirpath (parse-fhirpath "Patient.name.given") (list patient)))  ; '("Peter" "James")
```

## Develop

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec .   # repos side by side
raco test .
```

Reference: [oloveluck.github.io/fhirpath](https://oloveluck.github.io/fhirpath/). Architecture:
`CLAUDE.md`. License: MIT.
