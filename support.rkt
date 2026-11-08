#lang racket/base

;;; support.rkt — what FHIRPath offers the family's editor and agent hubs (fhir/lang/support): the
;;; diagnostics of a document (one expression; a syntax error is positioned). An expression declares
;;; nothing, so there is no outline. What an agent runs is workspace.rkt.

(require racket/contract racket/string
         fhir/lang/support
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic)
         (only-in "parser/parse.rkt" parse-fhirpath))

(provide (contract-out [support lang-support?]))

(define (diagnose text path)
  (with-handlers ([exn:fail:diagnostic? (lambda (e) (list (exn:fail:diagnostic-diagnostic e)))])
    (parse-fhirpath (string-trim (blank-lang-line text)))
    '()))

(define support (lang-support "fhirpath" diagnose (lambda (p) '()) (lambda (t f) '())
                              (lambda (pr t pos) #f) (lambda (pr t pos) #f) (lambda (pr t pos) '())))

(module+ test
  (require rackunit)
  (check-equal? (diagnose "name.given.first()" "/tmp/nowhere/a.fhirpath") '())
  (check-true (pair? (diagnose "name.given.first(" "/tmp/nowhere/b.fhirpath"))))
