#lang racket/base

;;; value.rkt — the FHIRPath item/collection model.
;;;
;;; A collection is a Racket list. An item is one of:
;;;   - a `fhir` node                    (a complex/resource value; FHIR-typed)
;;;   - a `fhir-<prim>` newtype instance (a typed FHIR primitive leaf; carries prop:fhir-spec)
;;;   - a raw string                     (base id/language fields kept as strings by from-json)
;;;   - a System scalar: boolean, exact integer, fp-decimal, string, t-date/t-time/t-datetime,
;;;                      fp-quantity, fp-long
;;;
;;; `item->system` collapses a FHIR primitive to its System value; `item-fhir-type` /
;;; `item-system-type` classify an item for the type system (types.rkt).

(require racket/contract
         (only-in fhir/model/node fhir fhir? fhir-type fhir-ref)
         (only-in fhir/model/base primitive-type? primitive-type-value)
         (only-in fhir/model/registry value->fhir-spec fhir-spec-name)
         (only-in fhir/primitives/absent absent?)
         (only-in fhir/primitives/primitives t-date? t-time? t-datetime?)
         (only-in "temporal-ops.rkt" moment? moment-kind)
         (only-in "decimal.rkt" fp-decimal? parse-decimal integer->decimal))

(provide
 (struct-out fp-quantity)
 (struct-out fp-long)
 (struct-out typeinfo)
 (contract-out
  [item->system     (-> any/c any/c)]
  [item-fhir-type   (-> any/c (or/c symbol? #f))]
  [item-system-type (-> any/c (or/c symbol? #f))]
  [system-value?    (-> any/c boolean?)]
  [prim-item?       (-> any/c boolean?)]
  [value-less?      (-> any/c boolean?)]
  [as-decimal       (-> any/c (or/c fp-decimal? #f))]))

;; a System.Quantity ; value is an fp-decimal ; unit a UCUM code or calendar keyword string
(struct fp-quantity (value unit calendar?) #:transparent)
;; a System.Long (R5) — distinguished from Integer for is()
(struct fp-long (v) #:transparent)
;; the result of type() — inspected via .namespace / .name
(struct typeinfo (namespace name) #:transparent)

;; the FHIR primitive type symbol of a fhir-<prim> item (e.g. 'date), or #f
(define (prim-type item)
  (and (primitive-type? item)
       (let ([s (value->fhir-spec item)]) (and s (fhir-spec-name s)))))

(define (prim-item? item) (primitive-type? item))

(define (value-less? item) (and (primitive-type? item) (absent? (primitive-type-value item))))

;; a Quantity-flavored node lifts to an fp-quantity for scalar ops (=, comparison, render)
(define (quantity-node-type? t)
  (memq t '(Quantity Age Count Distance Duration SimpleQuantity MoneyQuantity)))
(define (node->quantity n)
  (define v (fhir-ref n 'value #f))
  (define code (fhir-ref n 'code #f))
  (define unit (fhir-ref n 'unit #f))
  (if (primitive-type? v)
      (fp-quantity (or (parse-decimal (primitive-type-value v)) (integer->decimal 0))
                   (or (and (primitive-type? code) (primitive-type-value code))
                       (and (primitive-type? unit) (primitive-type-value unit)) "1")
                   #f)
      n))

;; collapse a FHIR primitive to its System value; System scalars pass through.
(define (item->system item)
  (cond
    [(and (fhir? item) (quantity-node-type? (fhir-type item))) (node->quantity item)]
    [(primitive-type? item)
     (define v (primitive-type-value item))
     (case (prim-type item)
       [(boolean) v]
       [(integer positiveInt unsignedInt) v]
       [(integer64) (fp-long v)]
       [(decimal) (or (parse-decimal v) v)]
       [(date dateTime instant time) v]           ; already a t-* struct
       [else v])]                                  ; string-likes -> string
    [else item]))

;; System scalar predicate
(define (system-value? x)
  (or (boolean? x) (exact-integer? x) (fp-decimal? x) (string? x)
      (t-date? x) (t-time? x) (t-datetime? x) (moment? x) (fp-quantity? x) (fp-long? x)))

;; the FHIR type of an item (for is/as/ofType); #f for pure System scalars.
(define (item-fhir-type item)
  (cond
    [(fhir? item) (fhir-type item)]
    [(primitive-type? item) (prim-type item)]
    [else #f]))    ; raw System scalars have no FHIR type

;; the System type of an item; #f for a complex fhir node.
(define (item-system-type item)
  (cond
    [(primitive-type? item)
     (case (prim-type item)
       [(boolean) 'Boolean]
       [(integer positiveInt unsignedInt) 'Integer]
       [(integer64) 'Long]
       [(decimal) 'Decimal]
       [(date) 'Date] [(dateTime instant) 'DateTime] [(time) 'Time]
       [else 'String])]
    [(boolean? item) 'Boolean]
    [(exact-integer? item) 'Integer]
    [(fp-long? item) 'Long]
    [(fp-decimal? item) 'Decimal]
    [(string? item) 'String]
    [(t-date? item) 'Date]
    [(t-datetime? item) 'DateTime]
    [(t-time? item) 'Time]
    [(moment? item) (case (moment-kind item) [(date) 'Date] [(dateTime) 'DateTime] [(time) 'Time])]
    [(fp-quantity? item) 'Quantity]
    [else #f]))

;; coerce an item to an fp-decimal if it is numeric (Integer or Decimal), else #f
(define (as-decimal item)
  (define s (item->system item))
  (cond [(fp-decimal? s) s]
        [(exact-integer? s) (integer->decimal s)]
        [(fp-long? s) (integer->decimal (fp-long-v s))]
        [else #f]))
