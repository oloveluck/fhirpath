#lang racket/base

;;; navigate.rkt — member navigation over typed nodes, driven by the registry schema.
;;;
;;; The typed node already holds typed children (primitive newtypes / nested nodes), so a direct
;;; field is just a hash lookup. The schema is consulted for the `value[x]` CHOICE rule (bare
;;; `value`/`deceased` resolve to the present `valueQuantity`/`deceasedBoolean`, keeping their branch
;;; type) and for `children()`/`descendants()`. Navigating a primitive reaches its `extension`/`id`.

(require racket/contract
         racket/list
         racket/set
         "error.rkt"
         (only-in fhir/model/node fhir? fhir-type fhir-fields fhir-ref fhir-has?)
         (only-in fhir/model/base primitive-type? element-extension element-id)
         (only-in "value.rkt" typeinfo? typeinfo-namespace typeinfo-name)
         (only-in "types.rkt" fhir-ancestors)
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

;; every schema-field of a type, climbing generated ancestors (constraint types, etc.)
(define (all-schema-fields type)
  (let loop ([t type] [acc '()])
    (define sch (type-schema-of t))
    (if (not sch) acc (loop (type-schema-parent sch) (append acc (type-schema-fields sch))))))

;; a registered resource type name (Capitalized; distinct from lowercase element names)
(define (resource-type-name? name)
  (and (type-registered? name) (memq 'Resource (fhir-ancestors name)) #t))

(define (choice-members-for type name)
  (for/list ([f (in-list (all-schema-fields type))] #:when (eq? (schema-field-choice-base f) name))
    (schema-field-name f)))

;; the framework fields present on (almost) every element/resource
(define framework-fields '(id extension modifierExtension meta implicitRules language text contained))

;; is `name` a defined element of `type` (own/ancestor field, a choice base, or a framework field)?
(define valid-cache (make-hash))
(define (valid-member? type name)
  (define valid (hash-ref! valid-cache type
                           (lambda ()
                             (define fs (all-schema-fields type))
                             (for/fold ([s (list->seteq framework-fields)]) ([f (in-list fs)])
                               (set-add (set-add s (schema-field-name f))
                                        (or (schema-field-choice-base f) (schema-field-name f)))))))
  (set-member? valid name))

;; all immediate child values of an item (nodes -> every present field value; primitives -> its
;; extensions). Order is unspecified (a hash walk), matching FHIRPath's unordered children().
(define (node-children item)
  (cond
    [(fhir? item)
     (append-map (lambda (v) (listify v)) (hash-values (fhir-fields item)))]
    [(primitive-type? item) (element-extension item)]
    [else '()]))
