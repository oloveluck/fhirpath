#lang racket/base

;;; run-r4.rkt — run the official FHIRPath R4 test suite (requires the full R4 tower).
;;; `racket fhirpath/tests/run-r4.rkt`.

(require racket/runtime-path
         (only-in fhir/r4/schemas)          ; register every R4 type
         "harness.rkt")

(define-runtime-path here ".")

(module+ main
  (run-suite (build-path here "vendor" "tests-fhir-r4.xml")
             (build-path here "inputs-r4")))
