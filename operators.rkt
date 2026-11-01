#lang racket/base

;;; operators.rkt — FHIRPath binary/unary operators over already-evaluated collections.
;;;
;;; Arithmetic is exact (Integer stays Integer, Decimal via decimal.rkt); comparison and equality
;;; are 3-valued (empty propagates); boolean operators follow FHIRPath's 3-valued logic with strict
;;; singleton-Boolean operands; `+`/`-` handle numeric, string(+), temporal±duration, and
;;; quantity±quantity. Full UCUM conversion for quantity arithmetic arrives in B5.

(require racket/contract racket/list
         "error.rkt"
         (only-in "value.rkt" item->system as-decimal fp-quantity fp-quantity? fp-quantity-value
                  fp-quantity-unit fp-quantity-calendar? fp-long fp-long?)
         (only-in "decimal.rkt" fp-decimal? integer->decimal decimal->exact decimal-add decimal-sub
                  decimal-mul decimal-div decimal-cmp fp-decimal fp-decimal-scale)
         (only-in "temporal-ops.rkt" value->moment moment? moment-compare moment->string moment-plus
                  moment-add-ms)
         (only-in "equality.rkt" coll-equal coll-equiv item=?)
         (only-in "quantity.rkt" quantity-compare quantity-add quantity-sub make-quantity))

(provide (contract-out
          [apply-binop (-> symbol? list? list? list?)]
          [apply-unary (-> symbol? list? list?)]))

;; ---- dispatch -------------------------------------------------------------------------------
(define (apply-binop op a b)
  (case op
    [(=)  (bool->coll (coll-equal a b))]
    [(!=) (bool->coll (neg3 (coll-equal a b)))]
    [(~)  (list (coll-equiv a b))]
    [(!~) (list (not (coll-equiv a b)))]
    [(and or xor implies) (logic op a b)]
    [(< > <= >=) (compare op a b)]
    [(+ - * / div mod) (arith op a b)]
    [(&) (concat a b)]
    [(union) (dedup (append a b))]
    [(in) (membership (one a) b)]
    [(contains) (membership (one b) a)]
    [else (fp-error 'semantic (format "unknown operator ~a" op))]))

(define (apply-unary op a)
  (cond
    [(null? a) '()]
    [(eq? op '+) a]
    [(eq? op '-)
     (define s (item->system (one a)))
     (cond [(exact-integer? s) (list (- s))]
           [(fp-decimal? s) (list (fp-decimal (- (decimal->exact s)) (fp-decimal-scale s)))]
           [(fp-quantity? s) (list (fp-quantity (fp-decimal (- (decimal->exact (fp-quantity-value s)))
                                                            (fp-decimal-scale (fp-quantity-value s)))
                                                (fp-quantity-unit s) (fp-quantity-calendar? s)))]
           [else (fp-error 'execution "unary - on non-number")])]
    [else a]))

;; ---- helpers --------------------------------------------------------------------------------
(define (one coll)
  (cond [(null? coll) 'empty]
        [(null? (cdr coll)) (car coll)]
        [else (fp-error 'execution "expected a singleton")]))

(define (bool->coll r) (case r [(true) (list #t)] [(false) (list #f)] [else '()]))
(define (neg3 r) (case r [(true) 'false] [(false) 'true] [else 'empty]))

;; ---- 3-valued boolean -----------------------------------------------------------------------
(define (bool3 coll)
  (cond [(null? coll) 'empty]
        [(and (null? (cdr coll)) (boolean? (item->system (car coll))))
         (if (item->system (car coll)) 'true 'false)]
        [(null? (cdr coll)) 'true]     ; a non-Boolean singleton is truthy (lenient)
        [else (fp-error 'execution "expected a singleton Boolean")]))

(define (logic op a b)
  (define x (bool3 a)) (define y (bool3 b))
  (b->c
   (case op
     [(and) (cond [(or (eq? x 'false) (eq? y 'false)) 'false]
                  [(and (eq? x 'true) (eq? y 'true)) 'true] [else 'empty])]
     [(or)  (cond [(or (eq? x 'true) (eq? y 'true)) 'true]
                  [(and (eq? x 'false) (eq? y 'false)) 'false] [else 'empty])]
     [(xor) (if (or (eq? x 'empty) (eq? y 'empty)) 'empty
                (if (eq? x y) 'false 'true))]
     [(implies) (cond [(eq? x 'false) 'true]
                      [(eq? y 'true) 'true]
                      [(and (eq? x 'true) (eq? y 'false)) 'false]
                      [else 'empty])])))
(define (b->c r) (case r [(true) (list #t)] [(false) (list #f)] [else '()]))

;; ---- comparison -----------------------------------------------------------------------------
(define (compare op a b)
  (define oa (one a)) (define ob (one b))
  (cond
    [(or (eq? oa 'empty) (eq? ob 'empty)) '()]
    [else
     (define c (cmp-items oa ob))
     (if (eq? c 'empty) '()
         (list (case op [(<) (< c 0)] [(>) (> c 0)] [(<=) (<= c 0)] [(>=) (>= c 0)])))]))

(define (cmp-items oa ob)
  (define sa (item->system oa)) (define sb (item->system ob))
  (define da (as-decimal oa)) (define db (as-decimal ob))
  (cond
    [(and da db) (decimal-cmp da db)]
    [(and (string? sa) (string? sb)) (cond [(string<? sa sb) -1] [(string>? sa sb) 1] [else 0])]
    [(and (moment-of sa) (moment-of sb)) (moment-compare (moment-of sa) (moment-of sb))]
    [(and (fp-quantity? sa) (fp-quantity? sb)) (quantity-compare sa sb)]
    [else (fp-error 'execution "operands are not comparable")]))

(define (moment-of x) (and (or (moment? x) (value->moment x)) (value->moment x)))

;; ---- arithmetic -----------------------------------------------------------------------------
(define (arith op a b)
  (define oa (one a)) (define ob (one b))
  (cond
    [(or (eq? oa 'empty) (eq? ob 'empty)) '()]
    [else (arith1 op oa ob)]))

(define (arith1 op oa ob)
  (define sa (item->system oa)) (define sb (item->system ob))
  (cond
    ;; string concatenation with +
    [(and (eq? op '+) (string? sa) (string? sb)) (list (string-append sa sb))]
    ;; temporal ± duration (only calendar durations, or precise UCUM time units, may be added)
    [(and (memq op '(+ -)) (moment-of sa) (fp-quantity? sb))
     (define u (fp-quantity-unit sb))
     (unless (or (fp-quantity-calendar? sb) (member u '("s" "ms" "min" "h" "d" "wk")))
       (fp-error 'execution "only calendar/time durations may be added to a date or time"))
     (define m (moment-of sa))
     (define tu (time-unit u))
     (define sign (if (eq? op '-) -1 1))
     (cond
       ;; sub-second precision (ms, or fractional seconds) -> millisecond arithmetic
       [(or (member tu '("millisecond")) (and (member tu '("second")) (not (integer? (qty-rat sb)))))
        (list->moment (moment-add-ms m (* sign (qty-ms sb tu))))]
       [else (list->moment (moment-plus m (* sign (qty-int sb)) tu))])]
    ;; quantity ± quantity
    [(and (fp-quantity? sa) (fp-quantity? sb) (memq op '(+ -)))
     (define r ((if (eq? op '+) quantity-add quantity-sub) sa sb))
     (if r (list r) (fp-error 'execution "incompatible quantity units"))]
    ;; quantity * / scalar
    [(and (fp-quantity? sa) (as-decimal ob) (memq op '(* /)))
     (define nv (if (eq? op '*) (decimal-mul (fp-quantity-value sa) (as-decimal ob))
                    (decimal-div (fp-quantity-value sa) (as-decimal ob))))
     (if nv (list (fp-quantity nv (fp-quantity-unit sa) (fp-quantity-calendar? sa))) '())]
    [(and (as-decimal oa) (fp-quantity? sb) (eq? op '*))
     (list (fp-quantity (decimal-mul (as-decimal oa) (fp-quantity-value sb))
                        (fp-quantity-unit sb) (fp-quantity-calendar? sb)))]
    ;; quantity * / quantity -> a compound unit (canonicalized on comparison)
    [(and (fp-quantity? sa) (fp-quantity? sb) (eq? op '*))
     (list (fp-quantity (decimal-mul (fp-quantity-value sa) (fp-quantity-value sb))
                        (string-append (uc sa) "." (uc sb)) #f))]
    [(and (fp-quantity? sa) (fp-quantity? sb) (eq? op '/))
     (define dv (decimal-div (fp-quantity-value sa) (fp-quantity-value sb)))
     (if dv (list (fp-quantity dv (string-append (uc sa) "/" (uc sb)) #f)) '())]
    ;; numeric
    [(and (as-decimal oa) (as-decimal ob)) (numeric op sa sb (as-decimal oa) (as-decimal ob))]
    [else (fp-error 'execution (format "operator ~a not defined for these operands" op))]))

(define (numeric op sa sb da db)
  (define int? (and (exact-integer? sa) (exact-integer? sb)))
  (case op
    [(+) (if int? (list (+ sa sb)) (list (decimal-add da db)))]
    [(-) (if int? (list (- sa sb)) (list (decimal-sub da db)))]
    [(*) (if int? (list (* sa sb)) (list (decimal-mul da db)))]
    [(/) (let ([r (decimal-div da db)]) (if r (list r) '()))]      ; div by zero -> empty
    [(div) (let ([d (decimal->exact db)]) (if (zero? d) '() (list (truncate/ (decimal->exact da) d))))]
    [(mod) (let ([d (decimal->exact db)]) (if (zero? d) '() (list (mod-num (decimal->exact da) d int? sa sb))))]))

(define (truncate/ a b) (let ([q (/ a b)]) (if (< q 0) (- (floor (- q))) (floor q))))
(define (mod-num a b int? sa sb)
  (define q (truncate/ a b))
  (define r (- a (* q b)))
  (if int? (inexact->exact r) (fp-decimal r (max (num-scale sa) (num-scale sb)))))
(define (num-scale x) (if (fp-decimal? x) (fp-decimal-scale x) 0))

;; integer value of a quantity (for calendar/time arithmetic)
;; the UCUM code of a quantity (calendar units mapped to their UCUM code)
(define (uc q)
  (if (fp-quantity-calendar? q)
      (hash-ref calendar-ucum (fp-quantity-unit q) (fp-quantity-unit q))
      (fp-quantity-unit q)))
(define calendar-ucum (hash "week" "wk" "day" "d" "hour" "h" "minute" "min" "second" "s" "millisecond" "ms"))
(define (qty-int q) (let ([r (decimal->exact (fp-quantity-value q))]) (if (integer? r) (inexact->exact r) (truncate/ r 1))))
(define (qty-rat q) (decimal->exact (fp-quantity-value q)))
(define (qty-ms q tu) (round (* (qty-rat q) (if (equal? tu "second") 1000 1))))
(define (list->moment m) (list m))     ; a moment is itself a System.DateTime/Date/Time item

;; UCUM time codes -> calendar keyword the moment arithmetic understands
(define (time-unit u)
  (cond [(member u '("a" "year" "years")) "year"] [(member u '("mo" "month" "months")) "month"]
        [(member u '("wk" "week" "weeks")) "week"] [(member u '("d" "day" "days")) "day"]
        [(member u '("h" "hour" "hours")) "hour"] [(member u '("min" "minute" "minutes")) "minute"]
        [(member u '("s" "second" "seconds")) "second"] [(member u '("ms" "millisecond" "milliseconds")) "millisecond"]
        [else u]))

;; ---- string & / union / membership ----------------------------------------------------------
(define (concat a b) (list (string-append (str-or-empty a) (str-or-empty b))))
(define (str-or-empty coll)
  (cond [(null? coll) ""]
        [else (define s (item->system (car coll))) (if (string? s) s (fp-error 'execution "& expects strings"))]))

(define (dedup coll)
  (let loop ([xs coll] [acc '()])
    (cond [(null? xs) (reverse acc)]
          [(for/or ([y (in-list acc)]) (item=? (car xs) y)) (loop (cdr xs) acc)]
          [else (loop (cdr xs) (cons (car xs) acc))])))

(define (membership x ys)
  (cond [(eq? x 'empty) '()]
        [else (list (and (for/or ([y (in-list ys)]) (item=? x y)) #t))]))
