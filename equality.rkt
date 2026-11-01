#lang racket/base

;;; equality.rkt — FHIRPath equality (=, !=) and equivalence (~, !~).
;;;
;;; Equality is 3-valued (true/false/empty): empty operands or cross-precision temporals yield empty.
;;; Equivalence is 2-valued, order-independent for collections, case/whitespace-insensitive for
;;; strings, and rounds decimals/temporals to least precision. Set operations (distinct/union/in/…)
;;; use equality via `item=?`.

(require racket/contract racket/list racket/string
         (only-in fhir/model/node fhir? fhir-type fhir-ref fhir-field-names fhir-has?)
         (only-in "value.rkt" item->system fp-quantity? fp-long? fp-long-v)
         (only-in "decimal.rkt" fp-decimal? integer->decimal decimal-cmp fp-decimal-scale
                  decimal-round)
         (only-in "temporal-ops.rkt" value->moment moment? moment-equal? moment-equiv?)
         (only-in "quantity.rkt" quantity-compare quantity-equiv?))

(provide
 (contract-out
  [coll-equal (-> list? list? (or/c 'true 'false 'empty))]
  [coll-equiv (-> list? list? boolean?)]
  [item-equal? (-> any/c any/c (or/c 'true 'false 'empty))]
  [item=?      (-> any/c any/c boolean?)]
  [item-equiv? (-> any/c any/c boolean?)]))

;; ---- collection equality (=) ----------------------------------------------------------------
(define (coll-equal a b)
  (cond
    [(or (null? a) (null? b)) 'empty]
    [(not (= (length a) (length b))) 'false]
    [else
     (let loop ([xs a] [ys b] [acc 'true])
       (cond
         [(null? xs) acc]
         [else (define r (item-equal? (car xs) (car ys)))
               (cond [(eq? r 'false) 'false]
                     [(eq? r 'empty) (loop (cdr xs) (cdr ys) 'empty)]
                     [else (loop (cdr xs) (cdr ys) acc)])]))]))

(define (item=? a b) (eq? (item-equal? a b) 'true))

(define (item-equal? a b)
  (define sa (item->system a)) (define sb (item->system b))
  (cond
    [(and (fhir? sa) (fhir? sb)) (node-equal? sa sb)]
    [(or (fhir? sa) (fhir? sb)) 'false]
    [else (system-equal? sa sb)]))

(define (as-dec x)
  (cond [(fp-decimal? x) x] [(exact-integer? x) (integer->decimal x)]
        [(fp-long? x) (integer->decimal (fp-long-v x))] [else #f]))

(define (system-equal? a b)
  (define da (as-dec a)) (define db (as-dec b))
  (cond
    [(and da db) (if (= (decimal-cmp da db) 0) 'true 'false)]
    [(and (boolean? a) (boolean? b)) (if (eq? a b) 'true 'false)]
    [(and (string? a) (string? b)) (if (string=? a b) 'true 'false)]
    [(and (temporalish? a) (temporalish? b)) (moment-equal? (value->moment a) (value->moment b))]
    [(and (fp-quantity? a) (fp-quantity? b))
     (case (quantity-compare a b) [(0) 'true] [(-1 1) 'false] [else 'empty])]
    [else 'false]))

(define (temporalish? x) (or (moment? x) (value->moment x)))

;; deep structural equality of two nodes (order-sensitive lists)
(define (node-equal? a b)
  (cond
    [(not (eq? (fhir-type a) (fhir-type b))) 'false]
    [else
     (define ka (sort (map symbol->string (fhir-field-names a)) string<?))
     (define kb (sort (map symbol->string (fhir-field-names b)) string<?))
     (cond
       [(not (equal? ka kb)) 'false]
       [else
        (let loop ([ks (fhir-field-names a)] [acc 'true])
          (cond
            [(null? ks) acc]
            [else
             (define r (coll-equal (listify (fhir-ref a (car ks))) (listify (fhir-ref b (car ks)))))
             (cond [(eq? r 'false) 'false] [(eq? r 'empty) (loop (cdr ks) 'empty)] [else (loop (cdr ks) acc)])]))])]))

(define (listify v) (if (list? v) v (list v)))

;; ---- equivalence (~) ------------------------------------------------------------------------
(define (coll-equiv a b)
  (cond
    [(and (null? a) (null? b)) #t]
    [(not (= (length a) (length b))) #f]
    [else (multiset-equiv? a b)]))

(define (multiset-equiv? a b)
  (let loop ([xs a] [pool b])
    (cond
      [(null? xs) #t]
      [else
       (define idx (index-where pool (lambda (y) (item-equiv? (car xs) y))))
       (and idx (loop (cdr xs) (append (take pool idx) (drop pool (add1 idx)))))])))

(define (item-equiv? a b)
  (define sa (item->system a)) (define sb (item->system b))
  (cond
    [(and (fhir? sa) (fhir? sb)) (node-equiv? sa sb)]
    [(or (fhir? sa) (fhir? sb)) #f]
    [(and (string? sa) (string? sb)) (string=? (norm-ws sa) (norm-ws sb))]
    [(and (as-dec sa) (as-dec sb))
     (define da (as-dec sa)) (define db (as-dec sb))
     (define p (min (fp-decimal-scale da) (fp-decimal-scale db)))
     (= (decimal-cmp (decimal-round da p) (decimal-round db p)) 0)]
    [(and (boolean? sa) (boolean? sb)) (eq? sa sb)]
    [(and (temporalish? sa) (temporalish? sb)) (moment-equiv? (value->moment sa) (value->moment sb))]
    [(and (fp-quantity? sa) (fp-quantity? sb)) (quantity-equiv? sa sb)]
    [else #f]))

(define (norm-ws s) (string-downcase (string-normalize-spaces s)))

(define (node-equiv? a b)
  (and (eq? (fhir-type a) (fhir-type b))
       (let ([ka (fhir-field-names a)] [kb (fhir-field-names b)])
         (and (= (length ka) (length kb))
              (for/and ([k (in-list ka)])
                (and (fhir-has? b k)
                     (coll-equiv (listify (fhir-ref a k)) (listify (fhir-ref b k)))))))))

(module+ test
  (require rackunit (only-in "decimal.rkt" parse-decimal))
  (check-eq? (coll-equal (list 1) (list 1)) 'true)
  (check-eq? (coll-equal (list 1) (list 2)) 'false)
  (check-eq? (coll-equal (list 1) '()) 'empty)
  (check-eq? (coll-equal (list (parse-decimal "1.0")) (list 1)) 'true)   ; numeric
  (check-eq? (coll-equal (list "a") (list "a")) 'true)
  (check-true (coll-equiv (list "Hello World") (list "hello   world")))  ; case/ws-insensitive
  (check-true (coll-equiv (list 1 2 3) (list 3 2 1)))                    ; order-independent
  (check-false (coll-equiv (list 1 2) (list 1 2 3))))
