#lang racket/base

;;; navigate.rkt — member navigation over typed nodes, driven by the registry schema.
;;;
;;; The typed node already holds typed children (primitive newtypes / nested nodes), so a direct
;;; field is just a hash lookup. The schema is consulted for the `value[x]` CHOICE rule (bare
;;; `value`/`deceased` resolve to the present `valueQuantity`/`deceasedBoolean`, keeping their branch
;;; type) and for `children()`/`descendants()`. Navigating a primitive reaches its `extension`/`id`.

(require racket/contract
         racket/list
         "error.rkt"
         (only-in fhir/model/node fhir? fhir-type fhir-fields fhir-ref fhir-has?)
         (only-in fhir/model/base primitive-type? element-extension element-id)
         (only-in "value.rkt" typeinfo? typeinfo-namespace typeinfo-name)
         (only-in "types.rkt" fhir-ancestors registry-memo)
         (only-in fhir/model/registry type-schema-of type-registered?)
         (only-in fhir/model/schema
                  type-schema-parent type-schema-fields schema-field-name schema-field-choice-base))

(provide
 (contract-out
  [navigate       (-> list? symbol? list?)]        ; a collection . member -> a collection
  [node-children  (-> any/c list?)]                ; all immediate children (for children()/descendants)
  [all-schema-fields (-> symbol? (listof any/c))]
  [resource-type-name? (-> symbol? boolean?)]))    ; a registered resource type's name

(define (navigate coll name)
  (append-map (lambda (item) (navigate-one item name)) coll))

(define (navigate-one item name)
  (cond
    [(fhir? item)
     (define type (fhir-type item))
     (cond
       [(fhir-has? item name) (listify (fhir-ref item name))]
       ;; resource-root self-reference: `Patient.name` where the focus IS a Patient — or is a
       ;; SUBTYPE of the named type: `Resource.meta.lastUpdated` / `DomainResource.text` on a
       ;; Condition (FHIR's own SearchParameters and invariants are written this way)
       [(or (eq? name type) (and (memq name (fhir-ancestors type)) (memq (quote Resource) (fhir-ancestors name))))
        (list item)]
       ;; a leading resource-type name that doesn't match the context is a semantic error (HL7's
       ;; testSimpleWithWrongContext) — except as the head of a `|` operand, which eval.rkt
       ;; evaluates as a selection by type (`union-branch`) and never brings here
       [(resource-type-name? name) (fp-error 'semantic (format "~a is not the context (~a)" name type))]
       [else
        ;; value[x] choice: collect present expanded members whose choice-base is `name`.
        ;; Otherwise absent -> empty (runtime navigation is lenient; unknown-element detection is
        ;; a static concern handled by analyze.rkt against the known root type).
        (append-map (lambda (m) (if (fhir-has? item m) (listify (fhir-ref item m)) '()))
                    (choice-members-for type name))])]
    [(primitive-type? item)
     (case name
       [(extension) (element-extension item)]
       [(id) (let ([i (element-id item)]) (if i (list i) '()))]
       [else '()])]
    [(typeinfo? item)
     (case name [(namespace) (list (typeinfo-namespace item))] [(name) (list (typeinfo-name item))] [else '()])]
    [else '()]))

(define (listify v) (if (list? v) v (list v)))

;; all-schema-fields : Symbol -> (listof schema-field)
;; every schema-field of a type, climbing generated ancestors (constraint types, etc.); worked out
;; once per type
(define all-schema-fields
  (registry-memo
   (lambda (type)
     (let loop ([t type] [acc '()])
       (define sch (type-schema-of t))
       (if (not sch) acc (loop (type-schema-parent sch) (append acc (type-schema-fields sch))))))))

;; a registered resource type name (Capitalized; distinct from lowercase element names)
(define (resource-type-name? name)
  (and (type-registered? name) (memq 'Resource (fhir-ancestors name)) #t))

;; choice-members : Symbol -> (hasheq Symbol -> (listof Symbol))
;; a type's choice elements: base name -> its expanded members, in schema order (value ->
;; valueQuantity valueString …). Asked for every name a node does not hold, so it is a table per
;; type (it was a scan of every field of the type per miss).
(define choice-members
  (registry-memo
   (lambda (type)
     (for/fold ([h (hasheq)]) ([f (in-list (reverse (all-schema-fields type)))])
       (define base (schema-field-choice-base f))
       (if base (hash-update h base (lambda (ms) (cons (schema-field-name f) ms)) '()) h)))))

;; choice-members-for : Symbol Symbol -> (listof Symbol)
(define (choice-members-for type name) (hash-ref (choice-members type) name '()))

;; all immediate child values of an item (nodes -> every present field value; primitives -> its
;; extensions). Order is unspecified (a hash walk), matching FHIRPath's unordered children().
(define (node-children item)
  (cond
    [(fhir? item)
     (append-map (lambda (v) (listify v)) (hash-values (fhir-fields item)))]
    [(primitive-type? item) (element-extension item)]
    [else '()]))

(module+ test
  (require rackunit (only-in fhir/r5/schemas) (only-in fhir/model/node fhir))
  ;; a choice element's members, in schema order; a name that is no choice base has none
  (check-not-false (memq 'valueQuantity (choice-members-for 'Observation 'value)))
  (check-not-false (memq 'deceasedBoolean (choice-members-for 'Patient 'deceased)))
  (check-equal? (choice-members-for 'Patient 'name) '())
  (check-equal? (choice-members-for 'NoSuchType 'value) '())
  (check-equal? (choice-members-for 'Observation 'value)
                (for/list ([f (in-list (all-schema-fields 'Observation))]
                           #:when (eq? (schema-field-choice-base f) 'value))
                  (schema-field-name f)))
  (check-true (resource-type-name? 'Patient))
  (check-false (resource-type-name? 'HumanName))
  (check-false (resource-type-name? 'name))
  ;; navigation: a present field, a choice base, a type's own name, another type's name
  (define obs (fhir 'Observation (hasheq 'status "final" 'valueString "x")))
  (check-equal? (navigate (list obs) 'status) '("final"))
  (check-equal? (navigate (list obs) 'value) '("x"))
  (check-equal? (navigate (list obs) 'Observation) (list obs))
  (check-equal? (navigate (list obs) 'Resource) (list obs))
  (check-equal? (navigate (list obs) 'subject) '())
  (check-exn #rx"Patient is not the context" (lambda () (navigate (list obs) 'Patient))))
