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
                  fp-index? fp-index-target fp-index-idx fp-typeop-op
                  fp-binop? fp-binop-op fp-binop-lhs fp-binop-rhs fp-unop? fp-unop-operand
                  fp-typeop? fp-typeop-operand fp-typeop-type fp-var? fp-extern?
                  fp-invoke fp-call fp-index fp-typeop)
         (only-in "navigate.rkt" all-schema-fields resource-type-name?)
         (only-in "types.rkt" fhir-ancestors primitive-codes registry-memo)
         (only-in fhir/model/registry type-schema-of type-registered? type-registry-generation)
         (only-in fhir/model/schema
                  schema-field-name schema-field-ftype schema-field-choice-base
                  ftype-tag ftype-prim-code ftype-type-name inherited-field-ftype))

(provide (contract-out [analyze (->* (any/c (or/c symbol? #f)) (#:strict? boolean?) void?)]
                       [path-head (-> any/c (or/c symbol? #f))]
                       [union-branch (-> any/c any/c)]))

;; strict mode flags direct access to an expanded choice member (Observation.valueQuantity) as a
;; semantic error; mode="lenient/polymorphics" allows it.
(define current-strict? (make-parameter #t))

(define framework-fields '(id extension modifierExtension meta implicitRules language text contained))
(define string-fns '(startsWith endsWith contains matches matchesFull indexOf lastIndexOf substring upper lower
                     replace replaceMatches length toChars split join trim))
(define same-type-fns '(where first last tail single skip take distinct intersect exclude combine
                        union sort trace iif))

;; analyze : ast Symbol|#f [#:strict? Boolean] -> Void
;; check the AST against the seed element type; raise on a definite semantic error. The verdict
;; depends only on the expression, the type and the mode, and an expression is evaluated over many
;; resources of one type (an invariant, a search parameter, a view's column), so it is remembered
;; per AST (by identity: `parse-fhirpath` answers one AST per text) for as long as the type
;; registry is what it was. It was a quarter to a third of an evaluation.
(define (analyze ast root-type #:strict? [strict? #t])
  (define verdict (verdict-of ast root-type strict?))
  (unless (eq? verdict 'ok)
    (raise (exn:fp verdict (current-continuation-marks) 'semantic))))

;; A Verdict is 'ok | String (the message of the semantic error)
;; verdicts : (weak hasheq ast -> (hash (cons Symbol|#f Boolean) -> Verdict))
(define verdicts (make-weak-hasheq))
(define verdicts-generation -1)

;; verdict-of : ast Symbol|#f Boolean -> Verdict
(define (verdict-of ast root-type strict?)
  (unless (eqv? verdicts-generation (type-registry-generation))
    (hash-clear! verdicts)
    (set! verdicts-generation (type-registry-generation)))
  (define key (cons root-type strict?))
  (define known (hash-ref verdicts ast #hash()))
  (or (hash-ref known key #f)
      (let ([verdict (check ast root-type strict?)])
        (hash-set! verdicts ast (hash-set known key verdict))
        verdict)))

;; check : ast Symbol|#f Boolean -> Verdict   the static pass itself
(define (check ast root-type strict?)
  (with-handlers ([(lambda (e) (and (exn:fp? e) (eq? (exn:fp-kind e) 'semantic))) exn-message])
    (parameterize ([current-strict? strict?]) (infer ast root-type))
    'ok))

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
    ;; but the result's type is not modelled. An operand of `|` rooted at ANOTHER resource type is
    ;; checked against that type (see `branch-type`)
    [(fp-binop? ast)
     (define union? (eq? (fp-binop-op ast) 'union))
     (for ([operand (in-list (list (fp-binop-lhs ast) (fp-binop-rhs ast)))])
       (infer operand (if union? (branch-type operand T) T)))
     #f]
    [(fp-call? ast) (infer-call ast T)]
    [else #f]))

(define projection-fns '(where select all repeat repeatAll aggregate iif coalesce sort exists trace))
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
  ;; repeat's projection runs at every level it reaches (`repeat(answer | item)` over a
  ;; QuestionnaireResponse, then its items and answers): no one type to check it against
  (unless (memq name '(is as ofType repeat repeatAll))
    (define ctx (if (memq name projection-fns) Tt T))
    (for ([a (in-list args)]) (infer a ctx)))
  (cond
    [(memq name '(ofType as)) (and (pair? args) (arg-type (car args)))]
    [(memq name same-type-fns) Tt]
    [else #f]))                                          ; unmodeled -> unknown

;; ---- the branches of a union ----------------------------------------------------------------
;; FHIR writes a search parameter that serves several resource types as ONE expression, a union of
;; paths each rooted at its own type (`Patient.name.given | Practitioner.name.given`). Over a
;; Patient the Practitioner branch contributes nothing. So the head of a `|` operand, when it names
;; a resource type, SELECTS the input of that type; anywhere else a head naming another resource
;; type stays the semantic error HL7's suite expects (testSimpleWithWrongContext,
;; `Encounter.name.given` over a Patient).

;; path-head : ast -> (or/c symbol? #f)   the name a path starts with, seen through the steps,
;; functions, indexers and type operators applied to it:
;;   Practitioner.name.where(use = 'official')[0] as HumanName  ->  Practitioner
;; #f when the expression does not start with a name (a literal, a function of the focus, an operator)
(define (path-head ast)
  (cond [(fp-invoke? ast) (if (fp-invoke-target ast) (path-head (fp-invoke-target ast)) (fp-invoke-name ast))]
        [(fp-call? ast) (and (fp-call-target ast) (path-head (fp-call-target ast)))]
        [(fp-index? ast) (path-head (fp-index-target ast))]
        [(fp-typeop? ast) (path-head (fp-typeop-operand ast))]
        [else #f]))

;; with-head : ast ast -> ast   the path with `head` in place of the name it starts with
(define (with-head ast head)
  (cond [(fp-invoke? ast)
         (if (fp-invoke-target ast) (fp-invoke (with-head (fp-invoke-target ast) head) (fp-invoke-name ast)) head)]
        [(fp-call? ast) (fp-call (with-head (fp-call-target ast) head) (fp-call-name ast) (fp-call-args ast))]
        [(fp-index? ast) (fp-index (with-head (fp-index-target ast) head) (fp-index-idx ast))]
        [(fp-typeop? ast) (fp-typeop (fp-typeop-op ast) (with-head (fp-typeop-operand ast) head) (fp-typeop-type ast))]
        [else ast]))

;; union-branch : ast -> ast   an operand of `|` as the evaluator runs it: a head naming a resource
;; type becomes `ofType(<that type>)` over the focus, so the branch of another type is empty and the
;; rest of the path runs over that (`Practitioner.name.exists()` is false, not an error)
;;   Practitioner.name.given  ->  ofType(Practitioner).name.given        name.given  ->  name.given
(define (union-branch ast)
  (define head (path-head ast))
  (if (and head (resource-type-name? head))
      (with-head ast (fp-call #f 'ofType (list (fp-invoke #f head))))
      ast))

;; branch-type : ast (or/c symbol? #f) -> (or/c symbol? #f)   the type to check a `|` operand
;; against: the resource type its head names when that is neither T nor an ancestor of T (the branch
;; is then about resources of that type: a misspelt element in it is still reported), else T
(define (branch-type operand T)
  (define head (path-head operand))
  (if (and T head (not (eq? head T)) (not (memq head (fhir-ancestors T))) (resource-type-name? head))
      head
      T))

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
    [else
     (define ms (members-of T))
     (cond
       [(hash-ref (members-bases ms) name #f) #f]       ; polymorphic base -> unknown, allowed
       [(hash-ref (members-by-name ms) name #f)
        => (lambda (m)
             (when (and (cdr m) (current-strict?))      ; (b) direct expanded-choice access (strict)
               (fp-error 'semantic (format "direct access to choice element ~a.~a" T name)))
             (ftype->type (car m)))]
       [(memq name framework-fields) (ftype->type (inherited-field-ftype name))]
       [else (fp-error 'semantic (format "no element ~a.~a~a" T name
                                         (did-you-mean (symbol->string name) (members-names ms))))])])) ; (a)

;; A Members is (members by-name bases names): the elements of a type, as tables —
;;   by-name : (hasheq Symbol -> (cons ftype Symbol|#f))   an element's type and, for an expanded
;;             choice member (valueQuantity), the base it expands (value)
;;   bases   : (hasheq Symbol -> #t)                       the choice bases (value, deceased)
;;   names   : (listof String)                             every name a path may use, for suggestions
;; Every step of every path asks these; they were association lists scanned from the front
;; (ElementDefinition has some four hundred entries).
(struct members (by-name bases names))

;; members-of : Symbol -> Members   over the type's generated ancestors, once per type
(define members-of
  (registry-memo
   (lambda (T)
     (define fields (all-schema-fields T))
     (define-values (by-name bases)
       (for/fold ([by-name (hasheq)] [bases (hasheq)]) ([f (in-list fields)])
         (define name (schema-field-name f))
         (define base (schema-field-choice-base f))
         (values (if (hash-has-key? by-name name) by-name      ; the first declaration wins
                     (hash-set by-name name (cons (schema-field-ftype f) base)))
                 (if base (hash-set bases base #t) bases))))
     (members by-name bases
              (map symbol->string
                   (append (map schema-field-name fields)
                           (filter values (map schema-field-choice-base fields))
                           framework-fields))))))

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
  (define (ok?/ast ast root #:strict? [strict? #t])
    (with-handlers ([exn:fp? (lambda (_) #f)]) (analyze ast root #:strict? strict?) #t))
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
  (check-true (ok? "repeat(answer | item).linkId" 'QuestionnaireResponse))   ; sdcqr-2's shape
  (check-true (ok? "repeatAll(answer | item).linkId" 'QuestionnaireResponse))
  (check-true (ok? "coalesce(name.where(use = 'official'), name).given" 'Patient))
  (check-false (ok? "coalesce(nmae, name)" 'Patient))                    ; arguments see the input
  (check-false (ok? "identifier.lastIndexOf('x')" 'Patient))
  ;; the verdict is remembered per expression and type, and says the same each time
  (let ([ast (parse-fhirpath "status")])
    (check-false (ok?/ast ast 'Patient))
    (check-false (ok?/ast ast 'Patient))
    (check-true (ok?/ast ast 'Observation) "another type: its own verdict"))
  (let ([ast (parse-fhirpath "Observation.valueQuantity")])
    (check-false (ok?/ast ast 'Observation))
    (check-true (ok?/ast ast 'Observation #:strict? #f) "another mode: its own verdict"))
  (check-false (ok? "Observation.valueQuantity.exists()" 'Observation)) ; strict: direct choice access
  (check-false (ok? "(Observation.value as Period).unit" 'Observation))
  (check-false (ok? "Patient.children().skip(1)" 'Patient))        ; strict: ordered fn on unordered
  (check-false (ok? "Appointment.identifier.startsWith('x')" 'Appointment))  ; string-fn on complex
  ;; a `|` operand rooted at another resource type (FHIR's multi-type search parameters) is checked
  ;; against THAT type; the same head outside a union is still an error (HL7's suite), and so is a
  ;; name that is neither a resource type nor an element
  (define (why s root)
    (with-handlers ([exn:fp? exn-message]) (analyze (parse-fhirpath s) root #:strict? #f) #f))
  (check-false (why "Patient.name.given | Practitioner.name.given" 'Patient))
  (check-false (why "Patient.name.given | Practitioner.name.given" 'Practitioner))
  (check-false (why "Practitioner.name.given | Patient.name.given | RelatedPerson.name.given" 'RelatedPerson))
  (check-false (why "(Practitioner.name.where(use = 'official')[0] as HumanName).given | Patient.name.given" 'Patient))
  (check-false (why "Resource.meta.lastUpdated | Patient.birthDate" 'Patient))     ; an ancestor is the input itself
  (check-equal? (why "Patient.name.given | Practitioner.name.givn" 'Patient)
                "fhirpath: no element HumanName.givn (did you mean `given`?)")
  (check-equal? (why "Patient.name.given | Practitioner.nmae.given" 'Patient)
                "fhirpath: no element Practitioner.nmae (did you mean `name`?)")
  (check-equal? (why "Practitioner.name.given" 'Patient) "fhirpath: no element Patient.Practitioner")
  (check-equal? (why "name.where(Practitioner.name.exists())" 'Patient) "fhirpath: no element HumanName.Practitioner")
  (check-equal? (why "Patient.name.given | Practicioner.name.given" 'Patient)
                "fhirpath: no element Patient.Practicioner")
  (check-equal? (why "name.given | activ" 'Patient) "fhirpath: no element Patient.activ (did you mean `active`?)")
  ;; the head of a path, and a branch as the evaluator runs it
  (check-equal? (path-head (parse-fhirpath "Practitioner.name.where(use = 'official')[0] as HumanName")) 'Practitioner)
  (check-equal? (path-head (parse-fhirpath "name.given")) 'name)
  (check-false (path-head (parse-fhirpath "'a' | 'b'")))
  (check-false (path-head (parse-fhirpath "exists()")))
  (check-equal? (union-branch (parse-fhirpath "Practitioner.name.given")) (parse-fhirpath "ofType(Practitioner).name.given"))
  (check-equal? (union-branch (parse-fhirpath "(Practitioner.name as HumanName)[0]"))
                (parse-fhirpath "(ofType(Practitioner).name as HumanName)[0]"))
  (check-equal? (union-branch (parse-fhirpath "name.given")) (parse-fhirpath "name.given")))