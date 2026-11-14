#lang racket/base

;;; operators.rkt — FHIRPath binary/unary operators over already-evaluated collections.
;;;
;;; Arithmetic is exact (Integer stays Integer, Decimal via decimal.rkt); comparison and equality
;;; are 3-valued (empty propagates); boolean operators follow FHIRPath's 3-valued logic with strict
;;; singleton-Boolean operands; `+`/`-` handle numeric, string(+), temporal±duration, and
;;; quantity±quantity (quantity.rkt converts units through UCUM).
;;;
;;; A Coll is a (listof Item), as in value.rkt. Every operator takes Colls and answers a Coll: an
;;; empty operand answers '() (the FHIRPath empty), a singleton is demanded with `one`.
;;; A Bool3 is 'true | 'false | 'empty — the three-valued result equality.rkt and the logic answer
;;; before it becomes a Coll (bool3->coll).

(require racket/contract racket/list
         "error.rkt"
         (only-in "value.rkt" item->system as-decimal fp-quantity fp-quantity? fp-quantity-value
                  fp-quantity-unit fp-quantity-calendar? fp-long fp-long?)
         (only-in "decimal.rkt" fp-decimal? integer->decimal decimal->exact decimal-add decimal-sub
                  decimal-mul decimal-div decimal-cmp fp-decimal fp-decimal-scale)
         (only-in "temporal-ops.rkt" value->moment moment? moment-compare moment->string moment-plus
                  moment-add-ms)
         (only-in "equality.rkt" coll-equal coll-equiv member=? distinct=)
         (only-in "quantity.rkt" quantity-compare quantity-add quantity-sub make-quantity calendar-ucum))

(provide (contract-out
          [apply-binop (-> symbol? list? list? list?)]
          [apply-unary (-> symbol? list? list?)]))

;; ---- dispatch -------------------------------------------------------------------------------
;; apply-binop : Symbol Coll Coll -> Coll   the binary operator `op` over evaluated operands
(define (apply-binop op a b)
  (case op
    [(=)  (bool3->coll (coll-equal a b))]
    [(!=) (bool3->coll (neg3 (coll-equal a b)))]
    [(~)  (list (coll-equiv a b))]
    [(!~) (list (not (coll-equiv a b)))]
    [(and or xor implies) (logic op a b)]
    [(< > <= >=) (compare op a b)]
    [(+ - * / div mod) (arith op a b)]
    [(&) (concat a b)]
    [(union) (distinct= (append a b))]
    [(in) (membership (one a) b)]
    [(contains) (membership (one b) a)]
    [else (fp-error 'semantic (format "unknown operator ~a" op))]))

;; apply-unary : Symbol Coll -> Coll   unary + and - (polarity on a number or quantity)
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
;; one : Coll -> Item | 'empty   the one item, or an execution error on several
(define (one coll)
  (cond [(null? coll) 'empty]
        [(null? (cdr coll)) (car coll)]
        [else (fp-error 'execution "expected a singleton")]))

;; bool3->coll : Bool3 -> Coll ; neg3 : Bool3 -> Bool3
(define (bool3->coll r) (case r [(true) (list #t)] [(false) (list #f)] [else '()]))
(define (neg3 r) (case r [(true) 'false] [(false) 'true] [else 'empty]))

;; ---- 3-valued boolean -----------------------------------------------------------------------
;; bool3 : Coll -> Bool3   a singleton Boolean operand; a non-Boolean singleton is truthy (lenient)
(define (bool3 coll)
  (cond [(null? coll) 'empty]
        [(and (null? (cdr coll)) (boolean? (item->system (car coll))))
         (if (item->system (car coll)) 'true 'false)]
        [(null? (cdr coll)) 'true]     ; a non-Boolean singleton is truthy (lenient)
        [else (fp-error 'execution "expected a singleton Boolean")]))

;; logic : Symbol Coll Coll -> Coll   and / or / xor / implies, three-valued
(define (logic op a b)
  (define x (bool3 a)) (define y (bool3 b))
  (bool3->coll
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

;; ---- comparison -----------------------------------------------------------------------------
;; compare : Symbol Coll Coll -> Coll   < > <= >= ; empty when an operand is empty or the order undecidable
(define (compare op a b)
  (define oa (one a)) (define ob (one b))
  (cond
    [(or (eq? oa 'empty) (eq? ob 'empty)) '()]
    [else
     (define c (cmp-items oa ob))
     (if (eq? c 'empty) '()
         (list (case op [(<) (< c 0)] [(>) (> c 0)] [(<=) (<= c 0)] [(>=) (>= c 0)])))]))

;; cmp-items : Item Item -> -1 | 0 | 1 | 'empty   two items of one kind (numbers, strings, temporals, quantities)
(define (cmp-items oa ob)
  (define sa (item->system oa)) (define sb (item->system ob))
  (define da (as-decimal oa)) (define db (as-decimal ob))
  (cond
    [(and da db) (decimal-cmp da db)]
    [(and (string? sa) (string? sb)) (cond [(string<? sa sb) -1] [(string>? sa sb) 1] [else 0])]
    [(and (value->moment sa) (value->moment sb)) (moment-compare (value->moment sa) (value->moment sb))]
    [(and (fp-quantity? sa) (fp-quantity? sb)) (quantity-compare sa sb)]
    [else (fp-error 'execution "operands are not comparable")]))

;; ---- arithmetic -----------------------------------------------------------------------------
;; arith : Symbol Coll Coll -> Coll   + - * / div mod over singletons; empty propagates
(define (arith op a b)
  (define oa (one a)) (define ob (one b))
  (cond
    [(or (eq? oa 'empty) (eq? ob 'empty)) '()]
    [else (arith1 op oa ob)]))

;; arith1 : Symbol Item Item -> Coll   dispatch on the operands' kinds
(define (arith1 op oa ob)
  (define sa (item->system oa)) (define sb (item->system ob))
  (cond
    ;; string concatenation with +
    [(and (eq? op '+) (string? sa) (string? sb)) (list (string-append sa sb))]
    ;; temporal ± duration (only calendar durations, or precise UCUM time units, may be added)
    [(and (memq op '(+ -)) (value->moment sa) (fp-quantity? sb))
     (define u (fp-quantity-unit sb))
     (unless (or (fp-quantity-calendar? sb) (member u '("s" "ms" "min" "h" "d" "wk")))
       (fp-error 'execution "only calendar/time durations may be added to a date or time"))
     (define m (value->moment sa))
     (define tu (time-unit u))
     (define sign (if (eq? op '-) -1 1))
     (cond
       ;; sub-second precision (ms, or fractional seconds) -> millisecond arithmetic
       [(or (member tu '("millisecond")) (and (member tu '("second")) (not (integer? (qty-rat sb)))))
        (list (moment-add-ms m (* sign (qty-ms sb tu))))]
       [else (list (moment-plus m (* sign (qty-int sb)) tu))])]
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

;; numeric : Symbol SystemValue SystemValue Decimal Decimal -> Coll
;; Integer op Integer stays Integer; otherwise Decimal. / div mod by zero answer empty.
(define (numeric op sa sb da db)
  (define int? (and (exact-integer? sa) (exact-integer? sb)))
  (case op
    [(+) (if int? (list (+ sa sb)) (list (decimal-add da db)))]
    [(-) (if int? (list (- sa sb)) (list (decimal-sub da db)))]
    [(*) (if int? (list (* sa sb)) (list (decimal-mul da db)))]
    [(/) (let ([r (decimal-div da db)]) (if r (list r) '()))]      ; div by zero -> empty
    [(div) (let ([d (decimal->exact db)]) (if (zero? d) '() (list (truncate/ (decimal->exact da) d))))]
    [(mod) (let ([d (decimal->exact db)]) (if (zero? d) '() (list (mod-num (decimal->exact da) d int? sa sb))))]))

;; truncate/ : Rational Rational -> Integer   the quotient toward zero
(define (truncate/ a b) (let ([q (/ a b)]) (if (< q 0) (- (floor (- q))) (floor q))))
;; mod-num : Rational Rational Boolean SystemValue SystemValue -> Integer | Decimal   the remainder, at the operands' scale
(define (mod-num a b int? sa sb)
  (define q (truncate/ a b))
  (define r (- a (* q b)))
  (if int? (inexact->exact r) (fp-decimal r (max (num-scale sa) (num-scale sb)))))
(define (num-scale x) (if (fp-decimal? x) (fp-decimal-scale x) 0))

;; uc : Quantity -> String   the UCUM code of a quantity (calendar units mapped to their UCUM code)
(define (uc q)
  (if (fp-quantity-calendar? q) (calendar-ucum (fp-quantity-unit q)) (fp-quantity-unit q)))
;; qty-int : Quantity -> Integer   the value toward zero (calendar/time arithmetic counts whole units)
(define (qty-int q) (let ([r (decimal->exact (fp-quantity-value q))]) (if (integer? r) (inexact->exact r) (truncate/ r 1))))
;; qty-rat / qty-ms : Quantity -> Rational | Integer   the exact value; the value in milliseconds
(define (qty-rat q) (decimal->exact (fp-quantity-value q)))
(define (qty-ms q tu) (round (* (qty-rat q) (if (equal? tu "second") 1000 1))))

;; time-unit : String -> String   a UCUM time code or calendar word -> the calendar keyword moment-plus takes
(define (time-unit u)
  (cond [(member u '("a" "year" "years")) "year"] [(member u '("mo" "month" "months")) "month"]
        [(member u '("wk" "week" "weeks")) "week"] [(member u '("d" "day" "days")) "day"]
        [(member u '("h" "hour" "hours")) "hour"] [(member u '("min" "minute" "minutes")) "minute"]
        [(member u '("s" "second" "seconds")) "second"] [(member u '("ms" "millisecond" "milliseconds")) "millisecond"]
        [else u]))

;; ---- string & / union / membership ----------------------------------------------------------
;; concat : Coll Coll -> Coll   `&`: an empty operand reads as ''
(define (concat a b) (list (string-append (str-or-empty a) (str-or-empty b))))
(define (str-or-empty coll)
  (cond [(null? coll) ""]
        [else (define s (item->system (car coll))) (if (string? s) s (fp-error 'execution "& expects strings"))]))

;; membership : Item | 'empty Coll -> Coll   `in` / `contains`: is the item = to one of the collection's?
(define (membership x ys)
  (cond [(eq? x 'empty) '()]
        [else (list (member=? x ys))]))

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit
           (only-in "decimal.rkt" parse-decimal decimal->string)
           (only-in "temporal-ops.rkt" parse-fp-date parse-fp-datetime moment->string)
           (only-in "quantity.rkt" make-quantity))
  (define (dec s) (parse-decimal s)) (define (q v u) (make-quantity (dec v) u))
  (define (fails? rx) (lambda (e) (and (exn:fp? e) (regexp-match? rx (exn-message e)))))
  ;; equality and equivalence are three-valued, empty propagating through = but not ~
  (check-equal? (apply-binop '= '(1) '(1)) '(#t))
  (check-equal? (apply-binop '!= '(1) '(2)) '(#t))
  (check-equal? (apply-binop '= '(1) '()) '())
  (check-equal? (apply-binop '~ '() '()) '(#t))
  (check-equal? (apply-binop '!~ '("a") '("A")) '(#f))
  ;; logic
  (check-equal? (apply-binop 'and '(#t) '()) '())
  (check-equal? (apply-binop 'and '(#f) '()) '(#f))
  (check-equal? (apply-binop 'or '(#t) '()) '(#t))
  (check-equal? (apply-binop 'xor '(#t) '(#f)) '(#t))
  (check-equal? (apply-binop 'implies '(#f) '()) '(#t))
  (check-equal? (apply-binop 'implies '(#t) '()) '())
  (check-equal? (apply-binop 'and '("x") '(#t)) '(#t))                 ; a non-Boolean singleton is truthy
  (check-exn (fails? #rx"singleton Boolean") (lambda () (apply-binop 'and '(#t #t) '(#t))))
  ;; comparison, across the kinds of one item
  (check-equal? (apply-binop '< '(1) (list (dec "1.5"))) '(#t))
  (check-equal? (apply-binop '>= '("b") '("a")) '(#t))
  (check-equal? (apply-binop '< (list (parse-fp-date "2024-01")) (list (parse-fp-date "2024-02"))) '(#t))
  (check-equal? (apply-binop '< (list (parse-fp-date "2024")) (list (parse-fp-date "2024-02"))) '())   ; undecidable
  (check-equal? (apply-binop '< (list (q "1" "m")) (list (q "150" "cm"))) '(#t))
  (check-equal? (apply-binop '< '() '(1)) '())
  (check-exn (fails? #rx"not comparable") (lambda () (apply-binop '< '(1) '("a"))))
  ;; arithmetic: Integer stays Integer, Decimal keeps its scale, zero divisors answer empty
  (check-equal? (apply-binop '+ '(1) '(2)) '(3))
  (check-equal? (map decimal->string (apply-binop '+ '(1) (list (dec "0.50")))) '("1.50"))
  (check-equal? (map decimal->string (apply-binop '/ '(1) '(4))) '("0.25"))
  (check-equal? (apply-binop '/ '(1) '(0)) '())
  (check-equal? (apply-binop 'div '(-7) '(2)) '(-3))
  (check-equal? (apply-binop 'mod '(7) '(3)) '(1))
  (check-equal? (map decimal->string (apply-binop 'mod (list (dec "7.5")) '(2))) '("1.5"))
  (check-equal? (apply-binop '+ '("a") '("b")) '("ab"))
  (check-equal? (apply-binop '& '("a") '()) '("a"))
  (check-exn (fails? #rx"not defined") (lambda () (apply-binop '* '("a") '(2))))
  ;; quantities: same dimension adds through UCUM, scalars scale, products and quotients compound
  (check-equal? (decimal->string (fp-quantity-value (car (apply-binop '+ (list (q "1" "m")) (list (q "50" "cm")))))) "1.5")
  (check-exn (fails? #rx"incompatible") (lambda () (apply-binop '+ (list (q "1" "m")) (list (q "1" "s")))))
  (check-equal? (fp-quantity-unit (car (apply-binop '* (list (q "2" "m")) '(3)))) "m")
  (check-equal? (fp-quantity-unit (car (apply-binop '* (list (q "2" "m")) (list (q "3" "s"))))) "m.s")
  (check-equal? (fp-quantity-unit (car (apply-binop '/ (list (q "6" "m")) (list (q "3" "s"))))) "m/s")
  ;; temporal ± duration: calendar words and the precise UCUM time codes only
  (check-equal? (map moment->string (apply-binop '+ (list (parse-fp-date "2024-01-31")) (list (q "1" "month")))) '("@2024-02-29"))
  (check-equal? (map moment->string (apply-binop '- (list (parse-fp-datetime "2024-03-05T10:00:00")) (list (q "90" "min")))) '("@2024-03-05T08:30:00"))
  (check-equal? (map moment->string (apply-binop '+ (list (parse-fp-datetime "2024-03-05T10:00:00")) (list (q "1.5" "s")))) '("@2024-03-05T10:00:01.500"))
  (check-exn (fails? #rx"calendar/time durations") (lambda () (apply-binop '+ (list (parse-fp-date "2024")) (list (q "1" "m")))))
  ;; collections
  (check-equal? (apply-binop 'union '(1 2) '(2 3)) '(1 2 3))
  (check-equal? (apply-binop 'in '(2) '(1 2)) '(#t))
  (check-equal? (apply-binop 'contains '(1 2) '(3)) '(#f))
  (check-equal? (apply-binop 'in '() '(1 2)) '())
  (check-exn (fails? #rx"unknown operator") (lambda () (apply-binop 'nope '() '())))
  ;; unary
  (check-equal? (apply-unary '- '(5)) '(-5))
  (check-equal? (map decimal->string (apply-unary '- (list (dec "1.50")))) '("-1.50"))
  (check-equal? (decimal->string (fp-quantity-value (car (apply-unary '- (list (q "2" "m")))))) "-2")
  (check-equal? (apply-unary '+ '(5)) '(5))
  (check-equal? (apply-unary '- '()) '())
  (check-exn (fails? #rx"unary -") (lambda () (apply-unary '- '("a")))))
