#lang racket/base

;;; decimal.rkt — FHIRPath System.Decimal with exact value + tracked precision.
;;;
;;; Racket reals lose trailing zeros ("1.50" -> 1.5), which FHIRPath needs for `precision()`, the
;;; `~` (round to least precision) rule, and faithful `toString`. We keep an exact rational `rat`
;;; plus `scale` (fractional-digit count from the lexeme). Arithmetic is exact; scale follows the
;;; FHIRPath rules (add/sub: max scale; mul: sum; div: a working precision).

(require racket/contract racket/math)

(provide
 (struct-out fp-decimal)
 (contract-out
  [parse-decimal    (-> string? (or/c fp-decimal? #f))]
  [integer->decimal (-> exact-integer? fp-decimal?)]
  [decimal->exact   (-> fp-decimal? rational?)]
  [decimal->string  (-> fp-decimal? string?)]
  [decimal-precision (-> fp-decimal? exact-nonnegative-integer?)]
  [decimal-add  (-> fp-decimal? fp-decimal? fp-decimal?)]
  [decimal-sub  (-> fp-decimal? fp-decimal? fp-decimal?)]
  [decimal-mul  (-> fp-decimal? fp-decimal? fp-decimal?)]
  [decimal-div  (-> fp-decimal? fp-decimal? (or/c fp-decimal? #f))]
  [decimal-cmp  (-> fp-decimal? fp-decimal? (or/c -1 0 1))]
  [decimal-round (-> fp-decimal? exact-nonnegative-integer? fp-decimal?)]
  [decimal-negate (-> fp-decimal? fp-decimal?)]))

;; rat : exact rational value ; scale : fractional digits carried (for precision/rendering)
(struct fp-decimal (rat scale) #:transparent)

;; parse a decimal lexeme -> fp-decimal (or #f if malformed). Handles optional exponent.
(define (parse-decimal s)
  (define m (regexp-match #px"^([+-]?)([0-9]+)(?:[.]([0-9]+))?(?:[eE]([+-]?[0-9]+))?$" s))
  (and m
       (let* ([sign (if (equal? (list-ref m 1) "-") -1 1)]
              [ip (list-ref m 2)]
              [fp (or (list-ref m 3) "")]
              [ex (let ([e (list-ref m 4)]) (if e (string->number e) 0))]
              [digits (string-append ip fp)]
              [scale0 (string-length fp)]
              [unscaled (string->number digits)]
              [rat (* sign unscaled (expt 10 (- ex scale0)))]
              [scale (max 0 (- scale0 ex))])
         (fp-decimal rat scale))))

(define (integer->decimal n) (fp-decimal n 0))
(define (decimal->exact d) (fp-decimal-rat d))
(define (decimal-precision d) (fp-decimal-scale d))
(define (decimal-negate d) (fp-decimal (- (fp-decimal-rat d)) (fp-decimal-scale d)))

;; render with exactly `scale` fractional digits (trailing zeros preserved)
(define (decimal->string d)
  (define rat (fp-decimal-rat d))
  (define sc (fp-decimal-scale d))
  (define neg? (< rat 0))
  (define a (abs rat))
  (cond
    [(= sc 0) (string-append (if neg? "-" "") (number->string (numerator a)))]
    [else
     (define scaled (* a (expt 10 sc)))                 ; exact integer
     (define i (quotient scaled (expt 10 sc)))
     (define f (remainder scaled (expt 10 sc)))
     (define fs (let ([s (number->string f)])
                  (string-append (make-string (- sc (string-length s)) #\0) s)))
     (string-append (if neg? "-" "") (number->string i) "." fs)]))

(define (decimal-add a b)
  (fp-decimal (+ (fp-decimal-rat a) (fp-decimal-rat b))
              (max (fp-decimal-scale a) (fp-decimal-scale b))))
(define (decimal-sub a b)
  (fp-decimal (- (fp-decimal-rat a) (fp-decimal-rat b))
              (max (fp-decimal-scale a) (fp-decimal-scale b))))
(define (decimal-mul a b)
  (fp-decimal (* (fp-decimal-rat a) (fp-decimal-rat b))
              (+ (fp-decimal-scale a) (fp-decimal-scale b))))

;; division to a working precision; #f on divide-by-zero. Trailing zeros trimmed from scale.
(define (decimal-div a b)
  (define bd (fp-decimal-rat b))
  (and (not (zero? bd))
       (let* ([q (/ (fp-decimal-rat a) bd)]
              [ws (max 8 (+ (fp-decimal-scale a) (fp-decimal-scale b)))]
              [scaled (round (* q (expt 10 ws)))]         ; nearest at working scale
              [rat (/ scaled (expt 10 ws))])
         (fp-decimal rat (trimmed-scale rat ws)))))

;; the minimal scale (<= cap) that represents rat exactly, else cap
(define (trimmed-scale rat cap)
  (let loop ([s 0])
    (cond [(> s cap) cap]
          [(integer? (* rat (expt 10 s))) s]
          [else (loop (add1 s))])))

(define (decimal-cmp a b)
  (define ra (fp-decimal-rat a)) (define rb (fp-decimal-rat b))
  (cond [(< ra rb) -1] [(> ra rb) 1] [else 0]))

;; round half-to-even-free: FHIRPath round is half-up away from zero
(define (decimal-round d prec)
  (define rat (fp-decimal-rat d))
  (define f (expt 10 prec))
  (define scaled (* rat f))
  (define r (floor (+ scaled 1/2)))               ; half up
  (define r* (if (< rat 0) (- (floor (+ (- scaled) 1/2))) r))
  (fp-decimal (/ r* f) prec))

(module+ test
  (require rackunit)
  (check-equal? (decimal->string (parse-decimal "1.50")) "1.50")   ; trailing zero kept
  (check-equal? (decimal-precision (parse-decimal "1.50")) 2)
  (check-equal? (decimal->exact (parse-decimal "1.50")) 3/2)
  (check-equal? (decimal->string (decimal-add (parse-decimal "1.5") (parse-decimal "2.25"))) "3.75")
  (check-equal? (decimal->string (decimal-mul (parse-decimal "1.5") (parse-decimal "2.0"))) "3.00")
  (check-equal? (decimal-cmp (parse-decimal "1.0") (parse-decimal "1.00")) 0)   ; equal by value
  (check-equal? (decimal->string (decimal-round (parse-decimal "3.14159") 2)) "3.14")
  (check-equal? (decimal->string (decimal-div (parse-decimal "1") (parse-decimal "8"))) "0.125")
  (check-false (decimal-div (parse-decimal "1") (parse-decimal "0"))))
