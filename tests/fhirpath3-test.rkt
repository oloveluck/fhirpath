#lang racket/base

;;; fhirpath3-test.rkt — what FHIRPath 3.0.0 (http://hl7.org/fhirpath/, the grammar vendored as
;;; tests/vendor/fhirpath.g4) added to N1, held to the specification's own examples: Long literals,
;;; sort() keys with asc/desc, and instance selectors. HL7's 3.0.0 test archive has no case of
;;; these (its tests-fhir-r4.xml is a subset of the vendored suite, and not well-formed XML).

(require rackunit
         (only-in fhir/r4/schemas)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in fhir/result/result ok-value)
         (only-in fhir/model/node fhir? fhir-type)
         (only-in "../eval.rkt" eval-fhirpath)
         (only-in "../parser/parse.rkt" parse-fhirpath)
         (only-in "../print.rkt" fhirpath->string)
         (only-in "../render.rkt" item->text)
         (only-in "../error.rkt" exn:fp?))

(define patient
  (ok-value (from-json (hasheq 'resourceType "Patient" 'gender "female"
                               'name (list (hasheq 'family "Brown" 'given (list "Ann"))
                                           (hasheq 'family "Adams" 'given (list "Zoe"))
                                           (hasheq 'family "Adams" 'given (list "Amy")))
                               'telecom (list (hasheq 'system "phone" 'use "work")
                                              (hasheq 'system "email" 'use "home")
                                              (hasheq 'system "phone" 'use "home")))
                       #:lenient? #t)))
(define (ev text [input (list patient)]) (eval-fhirpath (parse-fhirpath text) input))
(define (texts text [input (list patient)]) (map item->text (ev text input)))
(define (json text) (map node->jsexpr (ev text)))

;; ---- Long literals --------------------------------------------------------------------------
(check-equal? (texts "42L") '("42L"))
(check-equal? (texts "42L.toInteger()") '("42"))                      ; the specification's example
(check-equal? (texts "(42L + 1L).type().name") '("Long"))
(check-equal? (texts "(42L + 1).type().name") '("Long"))              ; an Integer meets a Long as a Long
(check-equal? (texts "(42L / 4L).type().name") '("Decimal"))
(check-equal? (texts "(-42L).type().name") '("Long"))
(check-equal? (texts "42L div 5L") '("8L"))
(check-equal? (texts "9223372036854775807L + 1L") '())                ; past a Long's range
(check-equal? (texts "42L = 42") '("true"))

;; ---- sort() keys -----------------------------------------------------------------------------
;; the specification's examples
(check-equal? (texts "(3 | 1 | 2).sort()") '("1" "2" "3"))
(check-equal? (texts "(3 | 1 | 2).sort($this)") '("1" "2" "3"))
(check-equal? (texts "(3 | 1 | 2).sort($this desc)") '("3" "2" "1"))
(check-equal? (texts "('c' | 'a' | 'b').sort($this desc)") '("c" "b" "a"))
(check-equal? (texts "('3' | '1' | '10').sort()") '("1" "10" "3"))
(check-equal? (texts "Patient.name.sort(family desc, given.first()).given") '("Ann" "Amy" "Zoe"))
(check-equal? (texts "Patient.telecom.sort(system, use desc).use") '("home" "work" "home"))
(check-equal? (texts "(3 | 1 | 2).sort($this asc)") '("1" "2" "3"))
;; the words are names too: a member called so is read (and Patient has none)
(check-exn #rx"no element Patient.sort" (lambda () (ev "Patient.sort")))
(check-exn #rx"no element HumanName.asc" (lambda () (ev "Patient.name.asc")))
;; desc says something of a sort key only
(check-exn exn:fail? (lambda () (parse-fhirpath "name.where(family desc)")))

;; ---- instance selectors ----------------------------------------------------------------------
;; the specification's examples
(check-equal? (json "Coding { system : 'http://example.org/demo', code : 'c1' }")
              (list (hasheq 'system "http://example.org/demo" 'code "c1")))
(check-equal? (json "FHIR.Identifier { system : 'http://example.org/demo', value : 'N0001231' }")
              (list (hasheq 'system "http://example.org/demo" 'value "N0001231")))
(check-equal? (json "Identifier { type : CodeableConcept { coding: Coding { system: 'http://terminology.hl7.org/CodeSystem/v2-0203', code: 'MR' } }, system : 'urn:oid:1.2.36.146.595.217.0.1', value : '12345', period : Period { start: @2001-05-06 } }")
              (list (hasheq 'type (hasheq 'coding (list (hasheq 'system "http://terminology.hl7.org/CodeSystem/v2-0203" 'code "MR")))
                            'system "urn:oid:1.2.36.146.595.217.0.1" 'value "12345"
                            'period (hasheq 'start "2001-05-06"))))
(check-equal? (json "Period {:}") (list (hasheq)))
(check-equal? (json "Patient.select( Coding { system: 'http://terminology.hl7.org/CodeSystem/v2-0203', code: gender } )")
              (list (hasheq 'system "http://terminology.hl7.org/CodeSystem/v2-0203" 'code "female")))
(check-equal? (texts "code { value: 'final' }") '("final"))
(check-equal? (texts "CodeableConcept { coding: Coding { system: 'http://terminology.hl7.org/CodeSystem/v2-0203', code: 'MR' } | Coding { system: 'http://example.org/id-types', code: 'mr' } }.coding.code")
              '("MR" "mr"))
;; an empty value adds no element; a choice element takes the member its value's type names
(check-equal? (json "Coding { system: {}, code: 'c' }") (list (hasheq 'code "c")))
(check-equal? (texts "Observation { status: 'final', code: CodeableConcept { text: 't' }, value: 5 'mg' }.value.unit") '("mg"))
(check-equal? (texts "System.Quantity { value: 5, unit: 'mg' } = 5 'mg'") '("true"))
;; what the type does not have, or several values for an element of one, is an error
(check-exn #rx"Coding has no element bogus" (lambda () (ev "Coding { bogus: 1 }")))
(check-exn #rx"code takes one value" (lambda () (ev "Coding { code: 'a' | 'b' }")))
(check-exn #rx"there is no type Nothing" (lambda () (ev "Nothing { a: 1 }")))
;; "If the input collection is empty, the result is empty"; several items are an error
(check-equal? (ev "Coding { code: 'c' }" '()) '())
(check-exn #rx"an instance is made from one" (lambda () (ev "Coding { code: 'c' }" (list patient patient))))

;; ---- written back as it was read --------------------------------------------------------------
(for ([text (list "42L" "name.sort(family desc, given.first())" "Coding { system: 'x', code: gender }" "Period { : }")])
  (check-equal? (parse-fhirpath (fhirpath->string (parse-fhirpath text))) (parse-fhirpath text) text))

;; ---- duration() and difference() (Date and Time Interval Functions, STU) ----------------------
;; the specification's examples: duration counts whole periods, difference the boundaries crossed
(check-equal? (texts "@2025-01-02.duration(@2025-01-07,'week')") '("0"))
(check-equal? (texts "@2025-01-01.duration(@2025-09-01,'year')") '("0"))
(check-equal? (texts "@2024-12-01.duration(@2025-09-01,'year')") '("0"))
(check-equal? (texts "@2025-01-02.difference(@2025-01-07,'week')") '("1"))   ; weeks start on Sunday
(check-equal? (texts "@2025-01-01.difference(@2025-09-01,'year')") '("0"))
(check-equal? (texts "@2024-12-01.difference(@2025-09-01,'year')") '("1"))
;; negative when the input is after the value; timezones normalized; times; fractions dropped
(check-equal? (texts "@2025-09-01.duration(@2024-12-01,'month')") '("-9"))
(check-equal? (texts "@2025-01-31.duration(@2025-02-28,'month')") '("0"))
(check-equal? (texts "@2025-01-01T10:00:00Z.duration(@2025-01-01T08:30:00-02:00,'minute')") '("30"))
(check-equal? (texts "@T10:00.duration(@T12:30,'hour')") '("2"))
(check-equal? (texts "@T10:59.difference(@T11:00,'hour')") '("1"))
(check-equal? (texts "@2025-01-01T10:00:00.000.duration(@2025-01-01T10:00:01.500,'millisecond')") '("1500"))
;; empty: an empty side, or less precision than asked; an error: another kind or precision
(check-equal? (texts "{}.duration(@2025-01-01,'day')") '())
(check-equal? (texts "@2025-01.duration(@2025-09-01,'day')") '())
(check-equal? (texts "@2025-01-01T10:00:00.duration(@2025-01-01T10:00:01.500,'millisecond')") '())
(check-exn #rx"a date takes the precision" (lambda () (ev "@2025-01-01.duration(@2025-09-01,'hour')")))
(check-exn #rx"two values of one kind" (lambda () (ev "@2025-01-01.duration(@T10:00,'day')")))

;; ---- pathname() -------------------------------------------------------------------------------
;; every step indexed, a choice element by its base name; short leaves out the indexer of an
;; element that is no list; a computed value has no path
(define observation
  (ok-value (from-json (hasheq 'resourceType "Observation" 'status "final"
                               'code (hasheq 'coding (list (hasheq 'code "a") (hasheq 'code "b")))
                               'valueQuantity (hasheq 'value 5 'unit "mg"))
                       #:lenient? #t)))
(check-equal? (texts "Observation.code.coding.pathname()" (list observation))
              '("Observation.code[0].coding[0]" "Observation.code[0].coding[1]"))
(check-equal? (texts "Observation.code.coding.pathname(true)" (list observation))
              '("Observation.code.coding[0]" "Observation.code.coding[1]"))
(check-equal? (texts "Observation.value.unit.pathname()" (list observation)) '("Observation.value[0].unit[0]"))
(check-equal? (texts "Patient.name.given.pathname()")
              '("Patient.name[0].given[0]" "Patient.name[1].given[0]" "Patient.name[2].given[0]"))
(check-equal? (texts "(1 | 2).pathname()") '())
(check-equal? (texts "{}.pathname()") '())
