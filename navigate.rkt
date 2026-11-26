#lang racket/base

;;; navigate.rkt — member navigation over typed nodes, driven by the registry schema.
;;;
;;; The typed node already holds typed children (primitive newtypes / nested nodes), so a direct
;;; field is just a hash lookup. The schema is consulted for the `value[x]` CHOICE rule (bare
;;; `value`/`deceased` resolve to the present `valueQuantity`/`deceasedBoolean`, keeping their branch
;;; type) and for `children()`/`descendants()`. Navigating a primitive reaches its `extension`/`id`.

(require racket/contract
         racket/list
         (submod "error.rkt" unchecked)
         (only-in fhir/model/node fhir? fhir-type)
         (only-in (submod fhir/model/node unchecked) fhir-fields fhir-ref)
         (only-in fhir/model/base primitive-type? element-extension element-id)
         (only-in (submod "value.rkt" unchecked) typeinfo? typeinfo-namespace typeinfo-name)
         (only-in (submod "types.rkt" unchecked) fhir-ancestors registry-memo)
         (only-in fhir/model/registry type-schema-of type-registered?)
         (only-in fhir/model/schema
                  type-schema-parent type-schema-fields schema-field-name schema-field-choice-base))

(provide
 (contract-out #:unprotected-submodule unchecked
  [navigate       (-> list? symbol? list?)]        ; a collection . member -> a collection
  [node-children  (-> any/c list?)]                ; all immediate children (for children()/descendants)
  [all-schema-fields (-> symbol? (listof any/c))]
  [choice-members-for (-> symbol? symbol? (listof symbol?))] ; a choice element's member keys (value -> valueQuantity …)
  [element-name-of (-> symbol? symbol? symbol?)]   ; a field key as FHIRPath names it (valueQuantity -> value)
  [resource-type-name? (-> symbol? boolean?)]))    ; a registered resource type's name
;; In-package callers require (submod "navigate.rkt" unchecked): the same names without their
;; contracts (the contracts are for callers outside the package).

(define (navigate coll name)
  (append-map (lambda (item) (navigate-one item name)) coll))

(define (navigate-one item name)
  (cond
    [(fhir? item)
     (define type (fhir-type item))
     (define held (fhir-ref item name no-field))
     (cond
       [(not (eq? held no-field)) (listify held)]
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
        (append-map (lambda (m) (field-values item m)) (choice-members-for type name))])]
    [(primitive-type? item)
     (case name
       [(extension) (element-extension item)]
       [(id) (let ([i (element-id item)]) (if i (list i) '()))]
       [else '()])]
    [(typeinfo? item)
     (case name [(namespace) (list (typeinfo-namespace item))] [(name) (list (typeinfo-name item))] [else '()])]
    [else '()]))

(define (listify v) (if (list? v) v (list v)))

;; A step asks a node for ONE field (`fhir-ref`, without its contract: one lookup, where
;; `fhir-has?` then `fhir-ref` through fhir/model/node's contracts were about a quarter of
;; evaluating the core constraints — CLAUDE.md, "Contracts at the boundary"). A node read
;; leniently reads that field alone from its JSON (fhir/model/node, a Reader): a path that takes
;; two of a resource's elements no longer reads them all.
(define no-field (string->uninterned-symbol "no-field"))
;; field-values : Node Symbol -> Coll   a field's values; none when it is absent
(define (field-values item name)
  (define held (fhir-ref item name no-field))
  (if (eq? held no-field) '() (listify held)))

;; all-schema-fields : Symbol -> (listof schema-field)
;; every schema-field of a type, climbing generated ancestors (constraint types, etc.); worked out
;; once per type
(define all-schema-fields
  (registry-memo
   (lambda (type)
     (let loop ([t type] [acc '()])
       (define sch (type-schema-of t))
       (if (not sch) acc (loop (type-schema-parent sch) (append acc (type-schema-fields sch))))))))

;; defined-names : Symbol -> (listof Symbol)   a type's element names in the order of its
;; definition: what every resource or element inherits first (the framework's fields, which the
;; schemas do not list), then the type's own, an ancestor's before a descendant's
(define inherited-names '(id meta implicitRules language text contained extension modifierExtension))
(define defined-names
  (registry-memo
   (lambda (type)
     (define own
       (let loop ([t type] [acc '()])
         (define sch (type-schema-of t))
         (if (not sch) acc (loop (type-schema-parent sch) (append (map schema-field-name (type-schema-fields sch)) acc)))))
     ;; a type may define an element of an inherited name itself (CodeableConcept.text): once, in its own place
     (append (filter (lambda (n) (not (memq n own))) inherited-names) own))))

;; resource-type-name? : Symbol -> Boolean   a registered resource type's name (Capitalized; distinct
;; from lowercase element names); asked at every step a node does not hold, so once per name
;; while the registry is what it was
(define resource-type-name?
  (registry-memo
   (lambda (name) (and (type-registered? name) (memq 'Resource (fhir-ancestors name)) #t))))

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

;; element-name-of : Symbol Symbol -> Symbol   the name FHIRPath gives the element a node holds
;; under a key: a choice member (`valueQuantity`) is its choice element (`value`), else the key
(define element-name-of
  (let ([by-type
         (registry-memo
          (lambda (type)
            (for/hasheq ([f (in-list (all-schema-fields type))] #:when (schema-field-choice-base f))
              (values (schema-field-name f) (schema-field-choice-base f)))))])
    (lambda (type key) (hash-ref (by-type type) key key))))

;; all immediate child values of an item (nodes -> every present field value; primitives -> its
;; extensions). FHIRPath leaves the order of children() undefined; here it is the order the
;; type's definition gives its elements (what HL7's Java engine answers too), then any field the
;; type does not define — never the order of a hash walk, which could differ from run to run.
(define (node-children item)
  (cond
    [(fhir? item)
     (define fields (fhir-fields item))
     (define defined (defined-names (fhir-type item)))
     (define (values-of k) (listify (hash-ref fields k)))
     (append (append-map values-of (filter (lambda (k) (hash-has-key? fields k)) defined))
             (append-map values-of
                         (sort (filter (lambda (k) (not (memq k defined))) (hash-keys fields)) symbol<?)))]
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
  ;; children() in the definition's order, whatever order the fields were given in
  (check-equal? (node-children (fhir 'Reference (hasheq 'display "Amy" 'reference "Patient/a" 'id "r1")))
                '("r1" "Patient/a" "Amy"))
  (check-equal? (take (defined-names 'Patient) 4) '(id meta implicitRules language))
  (check-not-false (memq 'birthDate (defined-names 'Patient)))
  (check-equal? (node-children (fhir 'CodeableConcept (hasheq 'text "t" 'coding (list (fhir 'Coding (hasheq 'code "c"))))))
                (list (fhir 'Coding (hasheq 'code "c")) "t"))
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
