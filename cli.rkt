#lang racket/base

;;; cli.rkt — `raco fhirpath`: the command line for the FHIRPath package.
;;;
;;;   raco fhirpath '<expression>' [resource.json] [--r4|--r5]
;;;       evaluate a FHIRPath expression against a FHIR JSON resource (default tower R5; with no
;;;       resource the expression runs over the empty collection) and print one result per line
;;;       (primitives as text, complex items as FHIR JSON)
;;;   raco fhirpath --tck [r5|r4]      run the official HL7 FHIRPath suite (floor-ratcheted; exit 1 on regression)
;;;   raco fhirpath --ucum-tck [-v]    run the official UCUM functional tests (floor-ratcheted)
;;; Exit codes: 0 ok · 1 FHIRPath error / regression · 2 usage.

(require json
         (only-in fhirpath/parser/parse parse-fhirpath)
         (only-in fhirpath/eval eval-fhirpath)
         (only-in fhirpath/value item->system)
         (only-in fhir/model/node fhir?)
         (only-in fhir/serialize/read-json read-fhir-json)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in fhir/result/result ok? ok-value err-error validation-error-message)
         (only-in fhir/diagnostic diagnostic diagnostic-loc loc emit-diagnostics format-flag
                  exn:fail:diagnostic? exn:fail:diagnostic-diagnostic))

(provide evaluate)
;; report : diagnostic text -> exit 1   (format from --format)
(define current-format (make-parameter 'text))
(define (report d text)
  (emit-diagnostics (list d) #:format (current-format) #:text-of (lambda (_) text))
  (exit 1))
(define (with-source d source)
  (define l (diagnostic-loc d))
  (struct-copy diagnostic d [loc (if l (struct-copy loc l [source source]) (loc source #f #f #f #f))]))

;; render : item -> string
(define (render v)
  (cond [(fhir? v) (jsexpr->string (node->jsexpr v))]
        [else (let ([s (item->system v)])
                (cond [(string? s) s]
                      [(boolean? s) (if s "true" "false")]
                      [(fhir? s) (jsexpr->string (node->jsexpr s))]
                      [else (format "~a" s)]))]))

;; evaluate : string (or/c path-string #f) (or/c "r4" "r5") -> void
(define (evaluate expr resource-file version)
  (dynamic-require (if (equal? version "r4") 'fhir/r4/schemas 'fhir/r5/schemas) #f)
  (define root
    (cond [resource-file
           (define r (from-json (call-with-input-file resource-file read-fhir-json) #:lenient? #t))
           (unless (ok? r) (eprintf "~a: ~a\n" resource-file (validation-error-message (err-error r))) (exit 1))
           (list (ok-value r))]
          [else '()]))
  ;; a syntax error is a positioned diagnostic; an evaluation error a run diagnostic
  (define ast
    (with-handlers ([exn:fail:diagnostic? (lambda (e) (report (with-source (exn:fail:diagnostic-diagnostic e) "<expression>") expr))])
      (parse-fhirpath expr)))
  (define results
    (with-handlers ([exn:fail? (lambda (e) (report (diagnostic 'fhirpath 'run 'error (exn-message e) (loc "<expression>" #f #f #f #f)) expr))])
      (eval-fhirpath ast root)))
  (for ([v (in-list results)]) (displayln (render v))))

(define (usage)
  (eprintf "usage: raco fhirpath '<expression>' [resource.json] [--r4|--r5]\n       raco fhirpath --tck [r5|r4]\n       raco fhirpath --ucum-tck [-v]\n")
  (exit 2))

(module+ main
  (define-values (fmt args) (format-flag (vector->list (current-command-line-arguments))))
  (current-format fmt)
  (cond
    [(null? args) (usage)]
    [(equal? (car args) "--tck")
     (define v (if (pair? (cdr args)) (cadr args) "r5"))
     (unless (member v '("r4" "r5")) (usage))
     (dynamic-require `(submod ,(string->symbol (string-append "fhirpath/tests/run-" v)) main) #f)]
    [(equal? (car args) "--ucum-tck")
     (parameterize ([current-command-line-arguments (list->vector (cdr args))])
       (dynamic-require '(submod fhirpath/tests/run-ucum main) #f))]
    [(regexp-match? #rx"^-" (car args)) (usage)]
    [else
     (define version (cond [(member "--r4" args) "r4"] [else "r5"]))
     (define positional (filter (lambda (a) (not (regexp-match? #rx"^--" a))) args))
     (case (length positional)
       [(1) (evaluate (car positional) #f version)]
       [(2) (evaluate (car positional) (cadr positional) version)]
       [else (usage)])]))
