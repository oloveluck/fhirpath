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
