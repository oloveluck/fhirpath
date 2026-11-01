#lang racket/base

;;; error.rkt — the FHIRPath error type, carrying a kind so the TCK harness can classify an
;;; `invalid="syntax|semantic|execution"` expectation.

(require racket/contract)

(provide (struct-out exn:fp)
         (contract-out [fp-error (->* (symbol? string?) () any)]))

(struct exn:fp exn:fail (kind) #:transparent)

(define (fp-error kind msg)
  (raise (exn:fp (string-append "fhirpath: " msg) (current-continuation-marks) kind)))
