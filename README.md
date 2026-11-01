# fhir-fhirpath

FHIRPath for Racket: a from-scratch, TCK-complete engine (parser, evaluator, full UCUM, precision-aware decimals and temporals) over the fhir-core typed node model.

Part of **fhir — FHIR as a family of Racket `#lang`s** (main repo: `oloveluck/fhir`). This package
contributes `fhirpath/` to the shared `fhir` collection. library only (no `#lang`).

## Depends on

`fhir-core`
(Racket packages; link them first) plus `brag-lib`, `br-parser-tools-lib`.

## Develop

```sh
raco pkg install --link --auto .     # link into the `fhir` collection
raco test fhirpath/
```

The `raco fhir` CLI and the conformance-suite runners are driven from the main `fhir` package:

```
raco fhir fhirpath-tck r5|r4, raco fhir ucum-tck   (official HL7 FHIRPath suites and the UCUM functional tests)
```

Architecture notes: `CLAUDE.md` here; conventions and the ratchet table: the main repo's `CLAUDE.md`.
