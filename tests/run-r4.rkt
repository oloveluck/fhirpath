#lang racket/base

;;; run-r4.rkt — run the official FHIRPath R4 test suite (requires the full R4 tower).
;;; `raco fhirpath --tck r4` or `racket tests/run-r4.rkt`.

(require racket/runtime-path
         (only-in fhir/r4/schemas)          ; register every R4 type
         "harness.rkt")

(define-runtime-path here ".")

;; NEVER REGRESS. 932/934: the 2 known misses are R4-vs-R5 spec conflicts (fractional seconds in
;; `@…+0.1 's'`; signed zero in lowBoundary) — see the package docs.
(define floor-pass 932)

(module+ main
  (define r (run-suite (build-path here "vendor" "tests-fhir-r4.xml") (build-path here "inputs-r4")))
  (define pass (car r))
  (when (< pass floor-pass)
    (eprintf "REGRESSION: FHIRPath R4 ~a < floor ~a\n" pass floor-pass)
    (exit 1)))
