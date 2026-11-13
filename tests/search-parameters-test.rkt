#lang racket/base

;;; search-parameters-test.rkt — FHIR's own SearchParameters as an oracle for the static check.
;;;
;;; A parameter that serves several resource types is ONE expression, a union of paths each rooted
;;; at its type (`Patient.name.given | Practitioner.name.given`); a server evaluates it over a
;;; resource of any of its `base` types. So every expression of the R5 core package must pass
;;; `analyze` seeded with each of its bases. Before `|` operands were checked against the type they
;;; name, 794 of the 1980 (expression, base) pairs were refused (`no element Patient.Practitioner`).
;;; What is still refused is the package's own: the `date` parameter writes Appointment's branch
;;; without its type (`(start | requestedPeriod.start).first()`), so over each of its 26 other bases
;;; `start` is an element the type lacks — a floor that only falls.
;;; Skipped (with a note) when fhir-spec is not installed.

(require rackunit racket/list
         (only-in fhir/r5/schemas)
         (only-in fhir/package read-package)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/model/registry type-registered?)
         (only-in "../parser/parse.rkt" parse-fhirpath)
         (only-in "../analyze.rkt" analyze)
         (only-in "../error.rkt" exn:fp?))

(define core (spec-package-path "hl7.fhir.r5.core.tgz" #:optional? #t))

;; refusals : path -> (values pairs-checked (listof (list code base message)))
(define (refusals tgz)
  (define-values (_manifest resources _examples)
    (read-package tgz #:select (lambda (name) (regexp-match? #rx"SearchParameter-" name))))
  (for*/fold ([n 0] [refused '()]) ([entry (in-list resources)]
                                    [r (in-value (if (pair? entry) (cdr entry) entry))]
                                    #:when (and (hash? r) (string? (hash-ref r 'expression #f)))
                                    [base (in-list (hash-ref r 'base '()))]
                                    #:when (type-registered? (string->symbol base)))
    (define problem
      (with-handlers ([exn:fp? exn-message])
        (analyze (parse-fhirpath (hash-ref r 'expression)) (string->symbol base) #:strict? #f)
        #f))
    (values (add1 n) (if problem (cons (list (hash-ref r 'code) base problem) refused) refused))))

(cond
  [(not core) (printf "search-parameters-test: skipped (fhir-spec is not installed)\n")]
  [else
   (define-values (n refused) (refusals core))
   (printf "R5 core SearchParameters: ~a (expression, base) pairs, ~a refused by the static check\n" n (length refused))
   (check-true (>= n 1900))
   ;; nothing is refused for naming another of the parameter's types
   (check-equal? (filter (lambda (r) (not (regexp-match? #rx"^fhirpath: no element [A-Za-z]+[.]start$" (caddr r)))) refused) '())
   ;; the residue: `date`'s unrooted Appointment branch
   (check-equal? (remove-duplicates (map car refused)) '("date"))
   (check-true (<= (length refused) 26))])
