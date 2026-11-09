#lang racket/base

;;; analyze.rkt — a conservative static semantic pass over a FHIRPath AST.
;;;
;;; Seeded with the input resource type, it walks the AST tracking the current element type via the
;;; registry schema and raises `fp-error 'semantic` for cases the lenient runtime can't catch:
;;;   (a) navigating an unknown element on a known concrete type   (name.given1)
;;;   (b) directly accessing an expanded choice member             (Observation.valueQuantity)
;;;   (c) a string function on a complex-typed subject            (Appointment.identifier.startsWith)
;;; DELIBERATELY conservative: whenever the type becomes unknown (#f) — polymorphic bases, resolve(),
;;; unmodeled functions — analysis stops for that branch, so it never raises on a valid expression
;;; it can't fully type.

(require (only-in fhir/suggest did-you-mean)
         racket/contract
         "error.rkt"
         (only-in "parser/ast.rkt"
                  fp-lit? fp-invoke? fp-invoke-target fp-invoke-name
                  fp-call? fp-call-target fp-call-name fp-call-args
                  fp-index? fp-index-target
                  fp-binop? fp-binop-lhs fp-binop-rhs fp-unop? fp-unop-operand
                  fp-typeop? fp-typeop-operand fp-typeop-type fp-var? fp-extern?)
         (only-in "navigate.rkt" all-schema-fields)
         (only-in "types.rkt" fhir-ancestors)
         (only-in fhir/model/registry type-schema-of type-registered?)
         (only-in fhir/model/schema
                  type-schema-parent type-schema-fields schema-field-name schema-field-ftype
                  schema-field-choice-base ftype-tag ftype-prim-code ftype-type-name
                  inherited-field-names inherited-field-ftype))

(provide (contract-out [analyze (->* (any/c (or/c symbol? #f)) (#:strict? boolean?) void?)]))

;; strict mode flags direct access to an expanded choice member (Observation.valueQuantity) as a
;; semantic error; mode="lenient/polymorphics" allows it.
(define current-strict? (make-parameter #t))

(define primitive-codes
  '(base64Binary boolean canonical code date dateTime decimal id instant integer integer64
    markdown oid positiveInt string time unsignedInt uri url uuid xhtml))
(define framework-fields '(id extension modifierExtension meta implicitRules language text contained))
(define string-fns '(startsWith endsWith contains matches matchesFull indexOf substring upper lower
                     replace replaceMatches length toChars split join trim))
(define same-type-fns '(where first last tail single skip take distinct intersect exclude combine
                        union sort trace iif))

;; analyze the AST against the seed element type; raise on a definite semantic error.
(define (analyze ast root-type #:strict? [strict? #t])
  (parameterize ([current-strict? strict?]) (void (infer ast root-type))))

;; infer the element type produced by `ast` given the current element type `T` (or #f = unknown)
(define (infer ast T)
  (cond
    [(fp-lit? ast) #f]
    [(fp-var? ast) T]                                   ; $this keeps T; others unknown enough
    [(fp-extern? ast) #f]
    [(fp-invoke? ast)
     (define Tt (if (fp-invoke-target ast) (infer (fp-invoke-target ast) T) T))
     (resolve (fp-invoke-name ast) Tt)]
    [(fp-index? ast) (infer (fp-index-target ast) T)]
    [(fp-unop? ast) (infer (fp-unop-operand ast) T)]
    [(fp-typeop? ast) (infer (fp-typeop-operand ast) T) (fp-typeop-type ast)]  ; as/ofType/is -> target
    ;; both operands are paths over the same input (`active = true`, `where(famly = 'x')`): checked,
    ;; but the result's type is not modelled
    [(fp-binop? ast) (infer (fp-binop-lhs ast) T) (infer (fp-binop-rhs ast) T) #f]
    [(fp-call? ast) (infer-call ast T)]
    [else #f]))

(define projection-fns '(where select all repeat aggregate iif sort exists trace))
(define (infer-call ast T)
  (define name (fp-call-name ast))
  (define args (fp-call-args ast))
  (define Tt (if (fp-call-target ast) (infer (fp-call-target ast) T) T))
  ;; (c) string function on a complex subject
  (when (and (memq name string-fns) (complex-type? Tt))
    (fp-error 'semantic (format "~a() applied to a complex type ~a" name Tt)))
  ;; (d) an order-dependent subsetter on an unordered source (children/descendants) is strict-invalid
  (when (and (current-strict?) (memq name '(skip take))
             (let ([tgt (fp-call-target ast)])
               (and (fp-call? tgt) (memq (fp-call-name tgt) '(children descendants)))))
    (fp-error 'semantic (format "~a() on an unordered collection" name)))
  ;; recurse into arguments to catch nested errors, EXCEPT type-specifier args (is/as/ofType).
  ;; projection args see the element type Tt; ordinary (eager) args see the outer type T.
  (unless (memq name '(is as ofType))
    (define ctx (if (memq name projection-fns) Tt T))
    (for ([a (in-list args)]) (infer a ctx)))
  (cond
    [(memq name '(ofType as)) (and (pair? args) (arg-type (car args)))]
    [(memq name same-type-fns) Tt]
    [else #f]))                                          ; unmodeled -> unknown

;; a type-specifier argument AST -> its (last) type symbol
(define (arg-type a)
  (cond [(and (fp-invoke? a) (not (fp-invoke-target a))) (fp-invoke-name a)]
        [(fp-invoke? a) (fp-invoke-name a)]
        [else #f]))

;; resolve member `name` on element type `T` -> the member's element type, or raise
(define (resolve name T)
  (cond
    [(not T) #f]
    [(memq T primitive-codes) #f]                       ; primitives: don't check members
    [(not (type-schema-of T)) #f]                       ; unknown/abstract type: don't check
    [(eq? name T) T]                                    ; resource-root self-reference
    [(and (memq name (fhir-ancestors T)) (memq 'Resource (fhir-ancestors name))) T] ; Resource.meta on a Condition
    [(choice-base? name T) #f]                          ; polymorphic base -> unknown, allowed
    [(assq name (field-index T))
     => (lambda (entry)
          (when (and (caddr entry) (current-strict?))    ; (b) direct expanded-choice access (strict)
            (fp-error 'semantic (format "direct access to choice element ~a.~a" T name)))
          (ftype->type (cadr entry)))]
    [(memq name framework-fields) (ftype->type (inherited-field-ftype name))]
    [else (fp-error 'semantic (format "no element ~a.~a~a" T name
                                      (did-you-mean (symbol->string name)
                                                    (map symbol->string
                                                         (append (map car (field-index T))
                                                                 (filter values (map caddr (field-index T)))
                                                                 framework-fields)))))]))   ; (a)

;; field-index of a type: (listof (list name ftype choice-base?)) over generated ancestors
(define index-cache (make-hasheq))
(define (field-index T)
  (hash-ref! index-cache T
             (lambda ()
               (for/list ([f (in-list (all-schema-fields T))])
                 (list (schema-field-name f) (schema-field-ftype f) (schema-field-choice-base f))))))

(define (choice-base? name T)
  (for/or ([e (in-list (field-index T))]) (eq? (caddr e) name)))

(define (ftype->type ft)
  (case (ftype-tag ft)
    [(prim) (string->symbol (ftype-prim-code ft))]
    [(type) (ftype-type-name ft)]
    [else #f]))                                          ; resource / raw / unknown

(define (complex-type? T)
  (and T (type-registered? T) (not (memq T primitive-codes))
       (let ([sch (type-schema-of T)]) (and sch #t))))

(module+ test
  (require rackunit (only-in fhir/r5/schemas) (only-in "parser/parse.rkt" parse-fhirpath))
  (define (ok? s root #:strict? [strict? #t])
    (with-handlers ([exn:fp? (lambda (_) #f)]) (analyze (parse-fhirpath s) root #:strict? strict?) #t))
  ;; valid expressions do not raise
  (check-true (ok? "Patient.name.given" 'Patient))
  (check-true (ok? "Observation.value.ofType(Quantity).unit" 'Observation))
  (check-true (ok? "Patient.telecom.where(system = 'phone')" 'Patient))
  (check-true (ok? "Patient.name.exists()" 'Patient))
  (check-true (ok? "Observation.valueQuantity.exists()" 'Observation #:strict? #f)) ; OK under lenient
  (check-true (ok? "Patient.is(FHIR.Patient)" 'Patient))                ; type-spec arg not navigated
  (check-true (ok? "Patient.name.first().subsetOf($this.name)" 'Patient)) ; eager arg sees outer type
  ;; semantic errors are caught (strict)
  (check-false (ok? "name.given1" 'Patient))                       ; unknown element
  (check-false (ok? "activ = true" 'Patient #:strict? #f))         ; inside an operator
  (check-false (ok? "name.where(famly = 'x')" 'Patient #:strict? #f)) ; inside a projection's operator
  (check-true (ok? "name.where(family = 'x') | telecom.where(value.exists())" 'Patient #:strict? #f))
  (check-false (ok? "Observation.valueQuantity.exists()" 'Observation)) ; strict: direct choice access
  (check-false (ok? "(Observation.value as Period).unit" 'Observation))
  (check-false (ok? "Patient.children().skip(1)" 'Patient))        ; strict: ordered fn on unordered
  (check-false (ok? "Appointment.identifier.startsWith('x')" 'Appointment)))  ; string-fn on complex