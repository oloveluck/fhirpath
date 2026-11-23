# Testing fhirpath

The official suites are vendored (`tests/vendor`) and each run is held to a floor that only rises.

## Results

| Suite | Result |
|---|---|
| HL7 FHIRPath suite, R5 | 1044/1044 (3 tests skipped: their input files are not in the suite) |
| HL7 FHIRPath suite, R4 | 932/934 (1 skipped for the same reason) |
| UCUM functional tests (FHIR/Ucum-java) | 573/573, every section, compared at the suite's own significant digits |
| The guide (`tests/guide-test.rkt`) | every cheat-sheet expression in guide.md parses; its worked examples' transcript is exactly what `raco fhirpath` prints over the guide's two resources | passes |
| The manual (`tests/docs-test.rkt`) | scribblings/fhirpath.scrbl renders with no broken reference to its own sections or bindings | passes |
| FHIR's SearchParameters (`tests/search-parameters-test.rkt`) | every expression of the R5 core package passes the static check for each of its base types (a parameter of several types is a union of paths, each rooted at its type) | 1954/1980 pairs; the 26 are the package's own (`date` writes Appointment's branch without its type) |
| The parser against the published grammar (`tests/descent-test.rkt`) | expressions are read by a hand-written parser; the parser generated from the vendored fhirpath.g4 (brag) stays the reference and the reader of every error: both must give the same tree or the same error at the same place, over the suites, every SearchParameter and constraint expression of six packages, 3,000 generated expressions and 3,000 token soups | 0 disagreements |
| HL7's Java engine (fhir-examples `value-experiment/reference/`) | 172,527 pairs of an expression (R4 core and US Core search parameters and invariants, each search expression continued by one of 116 function or operator calls) and a resource (US Core's examples and changed copies), evaluated by both engines; a fifth of the expressions read, the rest only counted | the same answer on 129,564 of the 145,371 held-out pairs (89.1%; 85.5% before the three fixes it led to); 144,123 (99.1%) when a difference that is the reference's own departure from the specification counts as none. A second pass read those pairs (one more fix) and ran a fresh set nobody read — IPS's and mCODE's examples, 142,488 pairs: the same answer on 127,795 (89.7%), 141,834 (99.5%) with the reference's departures aside, 66 under no class |
| The command line (`tests/cli-test.rkt`) | two expressions and an empty one are one-line syntax errors (exit 1); a search parameter of several types runs over a resource of one of them | passes |

The two R4 misses are known and deliberate:
- `@…+0.1 's'`: R4 truncates fractional seconds in date arithmetic and R5 keeps them. The two
  versions conflict, and matching R4 would break R5.
- `(-0.0034).lowBoundary(1)` expects `-0.0`, a signed zero the decimal type does not carry.

The R5 suite's `%terminologies` tests run against the fhir-tx terminology service when it is
installed; without it the floor is 1041.

## Running them

```sh
raco test .                 # unit tests, in every module
raco fhirpath --tck r5      # the R5 suite (floor 1044)
raco fhirpath --tck r4      # the R4 suite (floor 932)
raco fhirpath --ucum-tck    # the UCUM tests (floor 573)
```
