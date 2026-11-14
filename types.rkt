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
         (only-in fhir/model/registry type-schema-of type-registered? type-registry-generation)
         (only-in fhir/model/schema type-schema-parent)
         (only-in "value.rkt" item-fhir-type item-system-type system-value?)
         (only-in fhir/model/base primitive-type?)
         (only-in fhir/model/node fhir? fhir-type))

(provide
 (contract-out
  [item-is?       (-> any/c symbol? boolean?)]          ; item `is` typeSpecifier
  [item-of-type?  (-> any/c symbol? boolean?)]          ; item `as`/`ofType` (strict) typeSpecifier
  [known-type?    (-> symbol? boolean?)]                ; is this typeSpecifier a real FHIR/System type?
  ;; the range is checked as a list only: the chain is remembered, and checking every symbol of it
  ;; on every call was a tenth of the static pass
  [fhir-ancestors (-> (or/c symbol? #f) list?)]
  [primitive-codes (listof symbol?)]                    ; the FHIR primitive type names
  ;; registry-memo : (Symbol|#f -> X) -> (Symbol|#f -> X)   f remembered per key for as long as the
  ;; type registry is what it was (a logical model can be registered again under a name)
  [registry-memo  (-> (-> any/c any/c) (-> any/c any/c))]
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

;; registry-memo : (Symbol|#f -> X) -> (Symbol|#f -> X)
;; `f` remembered per key, for as long as the registry is what it was: a logical model can be
;; registered again under a name, and the registry's generation says when. What depends only on a
;; type (its ancestors, its elements, its choice members) is asked at every step of every path.
(define (registry-memo f)
  (define table (make-hasheq))
  (define generation -1)
  (lambda (key)
    (unless (eqv? generation (type-registry-generation))
      (hash-clear! table)
      (set! generation (type-registry-generation)))
    (define known (hash-ref table key none))
    (cond [(eq? known none) (define v (f key)) (hash-set! table key v) v]
          [else known])))
(define none (string->uninterned-symbol "none"))

;; ancestor-chain : Symbol|#f Boolean -> (listof Symbol)   the type and its ancestors, up to Base.
;; With `derivation?` a primitive follows its FHIR derivation first (code -> string): `is` reads
;; the chain that way (code is string), `as`/`ofType` do not (the "contested" TCK ruling:
;; as(string) on a code is empty).
(define (ancestor-chain t derivation?)
  (let loop ([t t] [acc '()])
    (cond
      [(not t) (reverse acc)]
      [(and derivation? (hash-ref prim-parent t #f)) => (lambda (p) (loop p (cons t acc)))]
      [(memq t primitive-codes) (loop 'PrimitiveType (cons t acc))]
      [(type-schema-of t) => (lambda (sch) (loop (type-schema-parent sch) (cons t acc)))]
      [(hash-has-key? framework-parents t) (loop (hash-ref framework-parents t) (cons t acc))]
      [else (reverse (cons t acc))])))

;; fhir-ancestors : Symbol|#f -> (listof Symbol)   the FHIR ancestor chain of a type (self first)
(define fhir-ancestors (registry-memo (lambda (t) (ancestor-chain t #t))))
;; fhir-ancestors-strict : the chain without the primitive-derivation edge (for as/ofType)
(define fhir-ancestors-strict (registry-memo (lambda (t) (ancestor-chain t #f))))

(define (fhir-is?/anc item name anc)
  (define ft (item-fhir-type item))
  (and ft (and (memq name (anc ft)) #t)))
(define (fhir-is? item name) (fhir-is?/anc item name fhir-ancestors))

;; only a genuine System scalar (not a FHIR primitive/node) matches a System type
(define (system-is? item name)
  (and (system-value? item) (eq? (item-system-type item) name)))

(define system-type-names '(Boolean Integer Long Decimal String Date DateTime Time Quantity SimpleTypeInfo))

;; A TypeSpec is (cons Namespace Symbol): a type specifier as it is read —
;;   ('System . Integer)   System.Integer        ('FHIR . Patient)   FHIR.Patient
;;   (#f . Patient)        Patient: FHIR first, then System
;;   ('other . X)          a qualifier that is neither: no type
;; type-spec : Symbol -> TypeSpec   read once per symbol (the specifier was split at its dot for
;; every item an `ofType` looked at)
(define type-specs (make-weak-hasheq))
(define (type-spec typesym)
  (or (hash-ref type-specs typesym #f)
      (let ([spec (read-type-spec typesym)]) (hash-set! type-specs typesym spec) spec)))
(define (read-type-spec typesym)
  (define parts (string-split (symbol->string typesym) "."))
  (cond [(= (length parts) 2)
         (cons (cond [(string=? (car parts) "System") 'System] [(string=? (car parts) "FHIR") 'FHIR] [else 'other])
               (string->symbol (cadr parts)))]
        [else (cons #f typesym)]))

(define (fhir-type-name? nm) (or (type-registered? nm) (and (memq nm primitive-codes) #t)))
(define (system-type-name? nm) (and (memq nm system-type-names) #t))

(define (known-type? typesym)
  (define spec (type-spec typesym))
  (case (car spec)
    [(System) (system-type-name? (cdr spec))]
    [(FHIR) (fhir-type-name? (cdr spec))]
    [(other) #f]
    [else (or (fhir-type-name? typesym) (system-type-name? typesym))]))

(define (item-is? item typesym) (item-matches? item typesym fhir-ancestors))
;; as()/ofType() use STRICT ancestry (no primitive-derivation edge)
(define (item-of-type? item typesym) (item-matches? item typesym fhir-ancestors-strict))

;; item-matches? : Item Symbol (Symbol -> (listof Symbol)) -> Boolean
(define (item-matches? item typesym anc)
  (define spec (type-spec typesym))
  (case (car spec)
    [(System) (system-is? item (cdr spec))]
    [(FHIR) (fhir-is?/anc item (cdr spec) anc)]
    [(other) #f]
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
  (check-true  (item-is? "x" 'String))
  ;; a specifier is read once: qualified, unqualified, and a qualifier that names no model
  (check-equal? (type-spec 'System.Integer) '(System . Integer))
  (check-equal? (type-spec 'Patient) '(#f . Patient))
  (check-equal? (type-spec 'Foo.Patient) '(other . Patient))
  (check-true  (known-type? 'FHIR.Patient))
  (check-true  (known-type? 'code))
  (check-false (known-type? 'System.Patient))
  (check-false (known-type? 'Foo.Patient))
  (check-false (item-is? (fhir 'Patient (hasheq)) 'Foo.Patient))
  ;; `is` follows a primitive's derivation, `as`/`ofType` do not
  (check-equal? (fhir-ancestors 'code) '(code string PrimitiveType DataType Element Base))
  (check-equal? (fhir-ancestors-strict 'code) '(code PrimitiveType DataType Element Base))
  (check-equal? (fhir-ancestors #f) '())
  ;; registry-memo asks once per key (a #f answer is remembered too)
  (let* ([asked 0] [f (registry-memo (lambda (k) (set! asked (add1 asked)) (and k (list k))))])
    (check-equal? (list (f 'a) (f 'a) (f #f) (f #f)) '((a) (a) #f #f))
    (check-equal? asked 2)))
