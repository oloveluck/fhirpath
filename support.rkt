#lang racket/base

;;; support.rkt — what FHIRPath offers the family's editor and agent hubs (fhir/lang/support): the
;;; diagnostics of a document (one expression; a syntax error is positioned) and the tool that
;;; evaluates an expression over a resource. An expression declares nothing, so there is no outline.

(require racket/contract racket/string json
         fhir/lang/support
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic)
         (only-in fhir/result/result ok-value)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in fhir/model/node fhir?)
         (only-in "parser/parse.rkt" parse-fhirpath)
         (only-in "eval.rkt" eval-fhirpath)
         (only-in "value.rkt" item->system))

(provide (contract-out [support lang-support?]))

(define (diagnose text path)
  (with-handlers ([exn:fail:diagnostic? (lambda (e) (list (exn:fail:diagnostic-diagnostic e)))])
    (parse-fhirpath (string-trim (blank-lang-line text)))
    '()))

(define tools
  (list
   (tool "fhirpath_evaluate"
         "Evaluate a FHIRPath expression over a FHIR resource (R5 JSON); the result collection as JSON (primitives as their values, complex items as FHIR JSON)."
         (hasheq 'type "object"
                 'properties (hasheq 'expression (hasheq 'type "string") 'resource (hasheq 'type "object" 'description "a FHIR resource as JSON; omitted: the empty collection"))
                 'required '("expression"))
         (lambda (args)
           (define expr (string-arg args 'expression))
           (define res (hash-ref args 'resource #f))
           (cond [(not expr) (missing 'expression)]
                 [else (dynamic-require 'fhir/r5/schemas #f)
                       (define root (if (hash? res) (list (ok-value (from-json res #:lenient? #t))) '()))
                       (define out (eval-fhirpath (parse-fhirpath expr) root))
                       (values (jsexpr->string (for/list ([v (in-list out)])
                                                 (cond [(fhir? v) (node->jsexpr v)]
                                                       [else (define s (item->system v))
                                                             (cond [(or (string? s) (boolean? s) (number? s)) s]
                                                                   [(fhir? s) (node->jsexpr s)]
                                                                   [else (format "~a" s)])])))
                               #f)])))))

(define support (lang-support "fhirpath" diagnose (lambda (p) '()) (lambda (t f) '())
                              (lambda (pr t pos) #f) (lambda (pr t pos) #f) (lambda (pr t pos) '()) tools))

(module+ test
  (require rackunit)
  (check-equal? (diagnose "name.given.first()" "/tmp/nowhere/a.fhirpath") '())
  (check-true (pair? (diagnose "name.given.first(" "/tmp/nowhere/b.fhirpath")))
  (define-values (t e) (call-tool (car tools) (hasheq 'expression "1 + 2")))
  (check-false e) (check-equal? (string->jsexpr t) '(3))
  (define-values (t2 e2) (call-tool (car tools) (hasheq 'expression "name.given" 'resource (hasheq 'resourceType "Patient" 'name (list (hasheq 'given '("Ann")))))))
  (check-false e2) (check-equal? (string->jsexpr t2) '("Ann")))
