#lang racket/base

;;; subsumes-test.rkt — FHIR's subsumes(code) and subsumedBy(code), answered by the terminology
;;; service given as %terminologies. The service is fhir-tx's, loaded by name (it depends on this
;;; package); without it the test says so and stops.

(require rackunit
         (only-in fhir/r5/schemas)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok-value)
         (only-in "../eval.rkt" eval-fhirpath)
         (only-in "../parser/parse.rkt" parse-fhirpath)
         (only-in "../terminology.rkt" tx-service))

(define make-terminology/resources
  (with-handlers ([exn:fail:filesystem:missing-module? (lambda (_) #f)])
    (dynamic-require 'fhir/terminology/service 'make-terminology/resources)))

(define sys "http://example.org/cs/body")
;; site > limb > arm, and an unrelated `other`
(define cs (hasheq 'resourceType "CodeSystem" 'url sys 'content "complete" 'status "active"
                   'concept (list (hasheq 'code "site"
                                          'concept (list (hasheq 'code "limb"
                                                                 'concept (list (hasheq 'code "arm")))))
                                  (hasheq 'code "other"))))
(define (coding code [system sys]) (ok-value (from-json (hasheq 'system system 'code code) #:type 'Coding)))
(define (concept . codes)
  (ok-value (from-json (hasheq 'coding (for/list ([c codes]) (hasheq 'system sys 'code c))) #:type 'CodeableConcept)))
(define (ask text input arg #:service [service #f])
  (eval-fhirpath (parse-fhirpath text) (list input)
                 #:externals (hash "terminologies" (if service (list service) '()) "arg" (list arg))
                 #:strict? #f))

;; a service without subsumption (three closures) cannot answer it, and says so
(check-exn #rx"subsumes\\(\\) is not implemented here: FHIR's subsumes\\(\\) needs a terminology service that answers subsumption"
           (lambda () (ask "$this.subsumes(%arg)" (coding "site") (coding "arm")
                           #:service (tx-service void void void))))

(cond
  [(not make-terminology/resources)
   (eprintf "note: fhir-tx is not installed; subsumes() is not tested against a terminology service\n")]
  [else
   (define tx (make-terminology/resources (list cs)))
   (define (subsumes a b) (ask "$this.subsumes(%arg)" a b #:service tx))
   (define (subsumed-by a b) (ask "$this.subsumedBy(%arg)" a b #:service tx))
   ;; an ancestor subsumes its descendants, and a code itself
   (check-equal? (subsumes (coding "site") (coding "arm")) '(#t))
   (check-equal? (subsumes (coding "limb") (coding "limb")) '(#t))
   (check-equal? (subsumes (coding "arm") (coding "site")) '(#f))
   (check-equal? (subsumes (coding "other") (coding "arm")) '(#f))
   (check-equal? (subsumed-by (coding "arm") (coding "site")) '(#t))
   (check-equal? (subsumed-by (coding "site") (coding "arm")) '(#f))
   ;; a CodeableConcept: true when any of its codings does
   (check-equal? (subsumes (concept "other" "limb") (coding "arm")) '(#t))
   ;; a code the service does not know: nothing, not false
   (check-equal? (subsumes (coding "nope") (coding "arm")) '())
   ;; different code systems: no defined relationship, an error
   (check-exn #rx"different code systems"
              (lambda () (subsumes (coding "site") (coding "arm" "http://example.org/cs/other"))))])
