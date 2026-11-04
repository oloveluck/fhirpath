#lang racket/base

;;; run-r5.rkt — run the official FHIRPath R5 test suite (requires the full R5 tower so from-json
;;; can parse every input resource). `raco fhirpath --tck r5` or `racket tests/run-r5.rkt`.
;;;
;;; The %terminologies backend (fhir/terminology/service, in the fhir-tx package) is loaded
;;; dynamically: fhir-validation depends on fhirpath, so a static require would be a package cycle. Without
;;; fhir-validation the 3 %terminologies tests fail and the floor drops accordingly — the run still means
;;; "nothing else regressed".

(require racket/runtime-path
         (only-in fhir/r5/schemas)          ; register every R5 type
         "harness.rkt")

(define-runtime-path here ".")

;; NEVER REGRESS: raise when the implementation improves, never lower to make a run pass.
(define floor-with-terminology 1044)
(define floor-without-terminology 1041)

;; optional-terminology : -> (or/c tx-service #f)
(define (optional-terminology)
  (with-handlers ([exn:fail:filesystem:missing-module?
                   (lambda (_)
                     (eprintf "note: fhir-validation is not installed; the 3 %terminologies tests will fail (raco pkg install fhir-validation)\n")
                     #f)])
    ((dynamic-require 'fhir/terminology/service 'make-r5-terminology))))

(module+ main
  (define tx (optional-terminology))
  (define r (run-suite (build-path here "vendor" "tests-fhir-r5.xml") (build-path here "inputs") #:terminology tx))
  (define pass (car r))
  (define floor (if tx floor-with-terminology floor-without-terminology))
  (when (< pass floor)
    (eprintf "REGRESSION: FHIRPath R5 ~a < floor ~a\n" pass floor)
    (exit 1)))
