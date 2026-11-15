#lang racket/base

;;; error.rkt — the FHIRPath error type, carrying a kind so the TCK harness can classify an
;;; `invalid="syntax|semantic|execution"` expectation.

(require racket/contract)

(provide (struct-out exn:fp)
         (contract-out #:unprotected-submodule unchecked [fp-error (->* (symbol? string?) () any)]))
;; In-package callers require (submod "error.rkt" unchecked): the same names without their
;; contracts (the contracts are for callers outside the package).
(module+ unchecked (provide (struct-out exn:fp)))

(struct exn:fp exn:fail (kind) #:transparent)

(define (fp-error kind msg)
  (raise (exn:fp (string-append "fhirpath: " msg) (current-continuation-marks) kind)))
