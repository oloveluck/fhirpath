#lang racket/base

;;; memberof-test.rkt — FHIR's memberOf(valueset), answered by the terminology service a caller
;;; gives as %terminologies (found missing by running matchbox's memberof.map beside the
;;; validator's engine: fhir-examples' value-experiment, H part 2, fourth pass). The service is
;;; fhir-tx's, loaded by name (it depends on this package); without it the test says so and stops.

(require rackunit json
         (only-in fhir/r5/schemas)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok-value)
         (only-in "../eval.rkt" eval-fhirpath)
         (only-in "../parser/parse.rkt" parse-fhirpath))

(define tx
  (with-handlers ([exn:fail:filesystem:missing-module? (lambda (_) #f)])
    ((dynamic-require 'fhir/terminology/service 'make-r5-terminology))))

(define patient
  (ok-value (from-json (hasheq 'resourceType "Patient" 'gender "female"
                               'maritalStatus (hasheq 'coding (list (hasheq 'system "http://example.org/none" 'code "M" 'display "married"))))
                       #:lenient? #t)))
(define (ask text #:service [service tx])
  (eval-fhirpath (parse-fhirpath text) (list patient)
                 #:externals (if service (hash "terminologies" (list service)) (hash)) #:strict? #f))
(define gender-vs "'http://hl7.org/fhir/ValueSet/administrative-gender'")

;; without a service it is an error that says what it needs
(check-exn #rx"memberOf\\(\\) is not implemented here: FHIR's memberOf\\(\\) needs a terminology service"
           (lambda () (ask (string-append "gender.memberOf(" gender-vs ")") #:service #f)))

(cond
  [(not tx) (eprintf "note: fhir-tx is not installed; memberOf() is not tested against a terminology service\n")]
  [else
   ;; a code, a string, a Coding and a CodeableConcept; a display that is not the code system's own
   ;; makes no difference to membership
   (check-equal? (ask (string-append "gender.memberOf(" gender-vs ")")) '(#t))
   (check-equal? (ask (string-append "'male'.memberOf(" gender-vs ")")) '(#t))
   (check-equal? (ask (string-append "'mael'.memberOf(" gender-vs ")")) '(#f))
   (check-equal? (ask (string-append "maritalStatus.memberOf(" gender-vs ")")) '(#f))
   ;; "the input is empty or has more than one value, the return value is empty"
   (check-equal? (ask (string-append "birthDate.memberOf(" gender-vs ")")) '())
   (check-equal? (ask (string-append "('male' | 'female').memberOf(" gender-vs ")")) '())
   ;; a value set nothing defines: the service cannot say, so nothing — not false
   (check-equal? (ask "gender.memberOf('http://nowhere.example/ValueSet/x')") '())])
