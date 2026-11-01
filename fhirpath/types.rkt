#lang racket/base

;;; types.rkt — the FHIRPath type system: is / as / ofType / type().
;;;
;;; FHIR type ancestry is read from the registry (type-schema-parent chain), extended with a
;;; hard-wired framework spine (DomainResource -> Resource -> Base, DataType -> Element -> Base, …)
;;; and the primitive -> PrimitiveType link (the abstract types have no field schema to walk).
;;; System.* types are a flat lattice. A typeSpecifier may be namespaced (System.X / FHIR.x) or bare
;;; (resolved FHIR-first, then System).

(require racket/contract
         racket/string
         (only-in fhir/model/registry type-schema-of type-registered?)
         (only-in fhir/model/schema type-schema-parent)
         (only-in "value.rkt" item-fhir-type item-system-type system-value?)
         (only-in fhir/model/base primitive-type?)
         (only-in fhir/model/node fhir? fhir-type))

(provide
 (contract-out
  [item-is?       (-> any/c symbol? boolean?)]          ; item `is` typeSpecifier
  [item-of-type?  (-> any/c symbol? boolean?)]          ; item `as`/`ofType` (strict) typeSpecifier
  [known-type?    (-> symbol? boolean?)]                ; is this typeSpecifier a real FHIR/System type?
  [fhir-ancestors (-> (or/c symbol? #f) (listof symbol?))]
  [type-info-of   (-> any/c (values symbol? symbol?))])) ; item -> (values namespace name) for type()

(define framework-parents
  (hash 'DomainResource 'Resource
        'Resource 'Base
        'CanonicalResource 'DomainResource
        'MetadataResource 'CanonicalResource
        'Element 'Base
        'DataType 'Element
        'PrimitiveType 'DataType
        'BackboneElement 'Element
        'BackboneType 'DataType
        'Base #f))

(define primitive-codes
  '(base64Binary boolean canonical code date dateTime decimal id instant integer integer64
    markdown oid positiveInt string time unsignedInt uri url uuid xhtml))

;; FHIR primitive derivation (e.g. code is a specialization of string) — so `code is string` holds
(define prim-parent
  (hash 'code 'string 'id 'string 'markdown 'string
        'url 'uri 'canonical 'uri 'oid 'uri 'uuid 'uri
        'positiveInt 'integer 'unsignedInt 'integer 'instant 'dateTime))

;; the FHIR ancestor chain of a type (self first, up to Base)
(define (fhir-ancestors t)
  (let loop ([t t] [acc '()])
    (cond
      [(not t) (reverse acc)]
      [(hash-ref prim-parent t #f) => (lambda (p) (loop p (cons t acc)))]
      [(memq t primitive-codes) (loop 'PrimitiveType (cons t acc))]
      [(type-schema-of t) => (lambda (sch) (loop (type-schema-parent sch) (cons t acc)))]
      [(hash-has-key? framework-parents t) (loop (hash-ref framework-parents t) (cons t acc))]
      [else (reverse (cons t acc))])))

;; strict ancestry: like fhir-ancestors but WITHOUT the primitive-derivation edge (code<:string).
;; `is` uses the full chain (code is string => true); `as`/`ofType` use strict (as(string) on a code
;; is empty — the "contested" TCK ruling).
(define (fhir-ancestors-strict t)
  (let loop ([t t] [acc '()])
    (cond
      [(not t) (reverse acc)]
      [(memq t primitive-codes) (loop 'PrimitiveType (cons t acc))]
      [(type-schema-of t) => (lambda (sch) (loop (type-schema-parent sch) (cons t acc)))]
      [(hash-has-key? framework-parents t) (loop (hash-ref framework-parents t) (cons t acc))]
      [else (reverse (cons t acc))])))

(define (fhir-is?/anc item name anc)
  (define ft (item-fhir-type item))
  (and ft (and (memq name (anc ft)) #t)))
(define (fhir-is? item name) (fhir-is?/anc item name fhir-ancestors))

;; only a genuine System scalar (not a FHIR primitive/node) matches a System type
(define (system-is? item name)
  (and (system-value? item) (eq? (item-system-type item) name)))

(define system-type-names '(Boolean Integer Long Decimal String Date DateTime Time Quantity SimpleTypeInfo))
(define (known-type? typesym)
  (define parts (string-split (symbol->string typesym) "."))
  (cond
    [(= (length parts) 2)
     (define nm (string->symbol (cadr parts)))
     (cond [(string=? (car parts) "System") (and (memq nm system-type-names) #t)]
           [(string=? (car parts) "FHIR") (or (type-registered? nm) (and (memq nm primitive-codes) #t))]
           [else #f])]
    [else (or (type-registered? typesym) (and (memq typesym primitive-codes) #t)
              (and (memq typesym system-type-names) #t))]))

(define (item-is? item typesym) (item-matches? item typesym fhir-ancestors))
;; as()/ofType() use STRICT ancestry (no primitive-derivation edge)
(define (item-of-type? item typesym) (item-matches? item typesym fhir-ancestors-strict))

(define (item-matches? item typesym anc)
  (define parts (string-split (symbol->string typesym) "."))
  (cond
    [(= (length parts) 2)
     (define ns (car parts)) (define nm (string->symbol (cadr parts)))
     (cond [(string=? ns "System") (system-is? item nm)]
           [(string=? ns "FHIR")   (fhir-is?/anc item nm anc)]
           [else #f])]
    [else (or (fhir-is?/anc item typesym anc) (system-is? item typesym))]))

;; type() : (values namespace name). A FHIR-typed item reports FHIR + its type; a pure System
;; scalar reports System + its System type.
(define (type-info-of item)
  (cond
    [(primitive-type? item) (values 'FHIR (item-fhir-type item))]
    [(fhir? item) (values 'FHIR (fhir-type item))]
    [else (values 'System (or (item-system-type item) 'Any))]))

(module+ test
  (require rackunit
           (only-in fhir/model/node fhir)
           (only-in fhir/r5/schemas))     ; register R5 types for ancestry
  ;; FHIR ancestry via the registry
  (check-true  (and (memq 'DomainResource (fhir-ancestors 'Patient)) #t))
  (check-true  (and (memq 'Resource (fhir-ancestors 'Patient)) #t))
  (check-true  (and (memq 'Element (fhir-ancestors 'HumanName)) #t))
  (check-true  (and (memq 'Quantity (fhir-ancestors 'Age)) #t))       ; constraint type
  (check-equal? (fhir-ancestors 'date) '(date PrimitiveType DataType Element Base))
  ;; is on nodes + system scalars
  (check-true  (item-is? (fhir 'Patient (hasheq)) 'Patient))
  (check-true  (item-is? (fhir 'Patient (hasheq)) 'DomainResource))
  (check-true  (item-is? (fhir 'Patient (hasheq)) 'FHIR.Resource))
  (check-false (item-is? (fhir 'Patient (hasheq)) 'HumanName))
  (check-true  (item-is? 5 'Integer))
  (check-true  (item-is? 5 'System.Integer))
  (check-false (item-is? 5 'Decimal))
  (check-true  (item-is? "x" 'String)))
