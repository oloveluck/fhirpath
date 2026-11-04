# fhirpath — FHIRPath for Racket

A FHIRPath engine: parser, evaluator, full UCUM (unit validation, canonicalisation, conversion), precision-aware decimals and partial dates/times — evaluating over the `fhir` typed node model. Passes the official HL7 suites: R5 1044/1044, R4 932/934, UCUM 573/573.

Part of **fhir — FHIR as a family of Racket `#lang`s** ([oloveluck/fhir](https://github.com/oloveluck/fhir)).
Collection `fhirpath`. Library only (no `#lang`); the embedders `cql`, `fml` and `fsh` build on it.

## Install

```sh
raco pkg install fhirpath          # once published on pkgs.racket-lang.org
# from source, with the family repos side by side:
raco pkg install --link --auto ../fhir/fhir .
```

Depends on `fhir` (the foundation; a FHIR version tower such as `fhir/r5/schemas` must be loaded by the caller).

## Use

```racket
(require fhirpath/parser/parse fhirpath/eval)   ; parse-fhirpath : string -> AST ; eval-fhirpath : AST (listof node) -> (listof item)
```

```sh
raco fhirpath 'Patient.name.given' patient.json   # evaluate against a resource (R5; --r4 for R4)
raco fhirpath --tck r5        # official FHIRPath suite (floor 1044; r4: 932)
raco fhirpath --ucum-tck      # UCUM functional tests (floor 573)
```

## Develop

```sh
raco test .          # unit tests (the conformance runners are explicit: see the --tck commands above)
```

Architecture notes: `CLAUDE.md`. Conventions, design style and the family-wide conformance table: the
main repo's `CLAUDE.md`. License: MIT (vendored test corpora keep their own terms — see `tests/vendor`).
