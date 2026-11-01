#lang racket/base

;;; quantity.rkt — FHIRPath System.Quantity (basic). Full UCUM conversion arrives in ucum/ (B5);
;;; for now: literal construction, calendar-duration normalization, same-dimension comparison, and
;;; value+unit rendering. UCUM cross-unit conversion is wired here once the engine lands.

(require racket/contract
         (only-in "value.rkt" fp-quantity fp-quantity? fp-quantity-value fp-quantity-unit
                  fp-quantity-calendar?)
         (only-in "decimal.rkt" fp-decimal fp-decimal? decimal-cmp decimal->string decimal-add
                  decimal-sub decimal->exact decimal-precision integer->decimal parse-decimal)
         (only-in "ucum.rkt" ucum-canonical ucum-commensurable?))

(provide
 (contract-out
  [make-quantity   (-> fp-decimal? string? fp-quantity?)]
  [calendar-unit?  (-> string? boolean?)]
  [quantity-compare (-> fp-quantity? fp-quantity? (or/c -1 0 1 'empty))]
  [quantity-equiv?  (-> fp-quantity? fp-quantity? boolean?)]
  [quantity->string (-> fp-quantity? string?)]
  [quantity-add    (-> fp-quantity? fp-quantity? (or/c fp-quantity? #f))]
  [quantity-sub    (-> fp-quantity? fp-quantity? (or/c fp-quantity? #f))]))

(define calendar-singular '("year" "month" "week" "day" "hour" "minute" "second" "millisecond"))
(define (calendar-unit? u)
  (and (member (normalize-calendar u) calendar-singular) #t))

;; plural -> singular; other strings unchanged
(define (normalize-calendar u)
  (cond [(member u calendar-singular) u]
        [(and (> (string-length u) 1) (char=? (string-ref u (sub1 (string-length u))) #\s)
              (member (substring u 0 (sub1 (string-length u))) calendar-singular))
         (substring u 0 (sub1 (string-length u)))]
        [else u]))

(define (make-quantity value unit)
  (fp-quantity value unit (calendar-unit? unit)))

;; calendar keyword -> its UCUM code (only the EXACT-length durations; year/month are variable and
;; are NOT convertible to UCUM a/mo, so they only compare to the same calendar keyword)
(define calendar->ucum
  (hash "week" "wk" "day" "d" "hour" "h" "minute" "min" "second" "s" "millisecond" "ms"))
(define (ucum-unit q)
  (define u (fp-quantity-unit q))
  (if (fp-quantity-calendar? q) (hash-ref calendar->ucum (normalize-calendar u) u) u))

;; the comparability key when UCUM can't canonicalize (calendar year/month, unknown units)
(define (raw-key q) (if (fp-quantity-calendar? q) (normalize-calendar (fp-quantity-unit q)) (fp-quantity-unit q)))

;; canonicalize a quantity to (cons magnitude*value dim-vector), or #f
(define (canon q)
  (define c (ucum-canonical (ucum-unit q)))
  (and c (cons (* (decimal->exact (fp-quantity-value q)) (car c)) (cdr c))))

;; -1|0|1|'empty (empty = not commensurable)
(define (quantity-compare a b)
  (define ca (canon a)) (define cb (canon b))
  (cond
    [(and ca cb (equal? (cdr ca) (cdr cb)))
     (cond [(< (car ca) (car cb)) -1] [(> (car ca) (car cb)) 1] [else 0])]
    [(equal? (raw-key a) (raw-key b)) (decimal-cmp (fp-quantity-value a) (fp-quantity-value b))]
    [else 'empty]))

;; equivalence: convert b into a's unit, round both to the least original decimal precision, compare
(define (quantity-equiv? a b)
  (define conv (convert-value b a))
  (and conv
       (let ([p (min (decimal-precision (fp-quantity-value a)) (decimal-precision (fp-quantity-value b)))])
         (= (round-to (decimal->exact (fp-quantity-value a)) p) (round-to conv p)))))
(define (convert-value b a)          ; b's magnitude expressed in a's unit (exact), or #f
  (define ca (ucum-canonical (ucum-unit a))) (define cb (ucum-canonical (ucum-unit b)))
  (cond [(and ca cb (equal? (cdr ca) (cdr cb)))
         (/ (* (decimal->exact (fp-quantity-value b)) (car cb)) (car ca))]
        [(equal? (raw-key a) (raw-key b)) (decimal->exact (fp-quantity-value b))]
        [else #f]))
(define (round-to r p) (let ([f (expt 10 p)]) (/ (round (* r f)) f)))

(define (quantity-add a b) (qty-combine + a b))
(define (quantity-sub a b) (qty-combine - a b))
(define (qty-combine op a b)
  ;; add/subtract in a's unit when commensurable
  (define ca (ucum-canonical (ucum-unit a))) (define cb (ucum-canonical (ucum-unit b)))
  (and ca cb (equal? (cdr ca) (cdr cb))
       (let* ([va (decimal->exact (fp-quantity-value a))]
              [vb-in-a (/ (* (decimal->exact (fp-quantity-value b)) (car cb)) (car ca))]
              [r (op va vb-in-a)])
         (fp-quantity (fp-decimal r (guess-scale r)) (fp-quantity-unit a) (fp-quantity-calendar? a)))))
(define (guess-scale r) (let loop ([s 0]) (cond [(> s 8) 8] [(integer? (* r (expt 10 s))) s] [else (loop (add1 s))])))

;; render as `value 'unit'` (UCUM) or `value unit` (calendar duration, unquoted)
(define (quantity->string q)
  (define v (decimal->string (fp-quantity-value q)))
  (if (fp-quantity-calendar? q)
      (string-append v " " (fp-quantity-unit q))
      (string-append v " '" (fp-quantity-unit q) "'")))
