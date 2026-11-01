#lang racket/base

;;; temporal-ops.rkt — FHIRPath Date/DateTime/Time with explicit precision: parse, compare,
;;; equality/equivalence, render. FHIRPath temporals are partial (a date may be year-only), so
;;; comparisons across differing precision yield empty, and equivalence compares to least precision.
;;;
;;; A `moment` is a uniform representation across all three kinds. Timezone-bearing dateTimes are
;;; normalized to UTC before component comparison.

(require racket/contract racket/match
         (only-in fhir/primitives/primitives t-date? t-time? t-datetime?
                  t-date-year t-date-month t-date-day t-date-precision
                  t-time-hour t-time-minute t-time-second t-time-frac
                  t-datetime-year t-datetime-month t-datetime-day t-datetime-hour t-datetime-minute
                  t-datetime-second t-datetime-frac t-datetime-tz t-datetime-precision))

(provide
 (struct-out moment)
 (contract-out
  [parse-fp-date     (-> string? (or/c moment? #f))]
  [parse-fp-datetime (-> string? (or/c moment? #f))]
  [parse-fp-time     (-> string? (or/c moment? #f))]
  [value->moment     (-> any/c (or/c moment? #f))]
  [moment-compare    (-> moment? moment? (or/c -1 0 1 'empty))]
  [moment-equal?     (-> moment? moment? (or/c 'true 'false 'empty))]
  [moment-equiv?     (-> moment? moment? boolean?)]
  [moment->string    (-> moment? string?)]
  [moment-plus       (-> moment? exact-integer? string? moment?)]
  [moment-add-ms     (-> moment? exact-integer? moment?)]
  [moment-boundary   (-> moment? exact-integer? boolean? (or/c moment? #f))]))

;; kind: 'date|'dateTime|'time ; components #f when absent ; prec: 1=year…6=second (time:4..6)
;; tzmin: timezone offset in minutes, or #f
(struct moment (kind year month day hour minute second frac tzmin prec) #:transparent)

;; ---- parsing FHIRPath temporal lexemes -------------------------------------------------------
(define date-rx #px"^([0-9]{4})(?:-([0-9]{2})(?:-([0-9]{2}))?)?$")
(define time-rx #px"^([0-9]{2})(?::([0-9]{2})(?::([0-9]{2})(?:\\.([0-9]+))?)?)?$")
(define dt-rx   #px"^([0-9]{4})(?:-([0-9]{2})(?:-([0-9]{2}))?)?(?:T([0-9]{2})?(?::([0-9]{2})(?::([0-9]{2})(?:\\.([0-9]+))?)?)?(Z|[+-][0-9]{2}:[0-9]{2})?)?$")

(define (n s) (and s (string->number s)))
(define (tz->min s)
  (cond [(not s) #f]
        [(string=? s "Z") 0]
        [else (let ([m (regexp-match #px"^([+-])([0-9]{2}):([0-9]{2})$" s)])
                (and m (* (if (string=? (cadr m) "-") -1 1)
                          (+ (* 60 (string->number (caddr m))) (string->number (cadddr m))))))]))

(define (parse-fp-date s)
  (define m (regexp-match date-rx s))
  (and m (match-let ([(list _ y mo d) m])
           (moment 'date (n y) (n mo) (n d) #f #f #f #f #f
                   (cond [d 3] [mo 2] [else 1])))))

(define (parse-fp-time s)
  (define m (regexp-match time-rx s))
  (and m (match-let ([(list _ h mi sec fr) m])
           (moment 'time #f #f #f (n h) (n mi) (n sec) fr #f
                   (cond [sec 6] [mi 5] [else 4])))))

(define (parse-fp-datetime s)
  (define m (regexp-match dt-rx s))
  (and m (match-let ([(list _ y mo d h mi sec fr tz) m])
           (moment 'dateTime (n y) (n mo) (n d) (n h) (n mi) (n sec) fr (tz->min tz)
                   (cond [sec 6] [mi 5] [h 4] [d 3] [mo 2] [else 1])))))

;; convert a stored FHIR temporal (t-*) or a moment into a moment
(define (value->moment v)
  (cond
    [(moment? v) v]
    [(t-date? v) (moment 'date (t-date-year v) (t-date-month v) (t-date-day v) #f #f #f #f #f
                         (case (t-date-precision v) [(year) 1] [(month) 2] [else 3]))]
    [(t-time? v) (moment 'time #f #f #f (t-time-hour v) (t-time-minute v) (t-time-second v)
                         (t-time-frac v) #f 6)]
    [(t-datetime? v)
     (moment 'dateTime (t-datetime-year v) (t-datetime-month v) (t-datetime-day v)
             (t-datetime-hour v) (t-datetime-minute v) (t-datetime-second v) (t-datetime-frac v)
             (tz->min (t-datetime-tz v))
             (case (t-datetime-precision v) [(year) 1] [(month) 2] [(day) 3] [else 6]))]
    [else #f]))

;; ---- comparison ------------------------------------------------------------------------------
;; total minutes-from-epoch-ish key for tz normalization (date+time to minutes, ignoring frac)
(define (norm m)
  ;; returns a moment with tz applied (shifted to UTC) when tzmin present and time is known
  (if (and (moment-tzmin m) (moment-hour m))
      (shift-minutes m (- (moment-tzmin m)))
      m))

;; shift-minutes : moment × minutes -> moment   shift h:mi, carrying day overflow through the
;; CALENDAR (add-days) — a tz normalization that crosses midnight must move the date too
(define (shift-minutes m delta)
  (define total (+ (* 60 (or (moment-hour m) 0)) (or (moment-minute m) 0) delta))
  (define t (modulo total (* 24 60)))
  (define day-carry (quotient (- total t) (* 24 60)))
  (define shifted (struct-copy moment m [hour (quotient t 60)] [minute (modulo t 60)] [tzmin 0]))
  (if (and (moment-day m) (not (zero? day-carry))) (add-days shifted day-carry) shifted))

(define (comps m)
  (list (moment-year m) (moment-month m) (moment-day m)
        (moment-hour m) (moment-minute m) (moment-second m)))

(define (frac-num m) (if (moment-frac m) (or (string->number (string-append "0." (moment-frac m))) 0) 0))
;; a tz on one side but not the other (both timed) makes the comparison indeterminate
(define (tz-mismatch? a b)
  (and (moment-hour a) (moment-hour b)
       (not (eq? (and (moment-tzmin a) #t) (and (moment-tzmin b) #t)))))

;; -1|0|1|'empty per FHIRPath partial-precision rules. Timezone normalization applies only when
;; BOTH operands carry a time: a date is timezone-less, so a dateTime compared against it uses its
;; STATED components (shifting it to UTC could move its day across midnight and manufacture a
;; difference — `now() > today()` must be 'empty at any hour, not true in the local evening).
(define (moment-compare a0 b0)
  (cond
    [(tz-mismatch? a0 b0) 'empty]
    [else
     (define both-timed? (and (moment-hour a0) (moment-hour b0)))
     (define a (if both-timed? (norm a0) a0))
     (define b (if both-timed? (norm b0) b0))
     (define ca (append (comps a) (list (frac-num a))))
     (define cb (append (comps b) (list (frac-num b))))
     (let loop ([xs ca] [ys cb])
       (cond
         [(or (null? xs) (null? ys)) 0]
         [(and (car xs) (car ys))
          (cond [(< (car xs) (car ys)) -1] [(> (car xs) (car ys)) 1] [else (loop (cdr xs) (cdr ys))])]
         [(and (not (car xs)) (not (car ys))) (loop (cdr xs) (cdr ys))]  ; absent in both -> skip
         [else 'empty]))]))                                              ; present in one only -> undecidable

;; Date and DateTime are comparable (by value + precision); Time is incompatible with either.
(define (moment-equal? a b)
  (define ta (moment-kind a)) (define tb (moment-kind b))
  (cond
    [(not (eq? (eq? ta 'time) (eq? tb 'time))) 'false]     ; one is Time, the other isn't
    [(not (= (moment-prec a) (moment-prec b))) 'empty]     ; differing precision -> empty
    [else (let ([c (moment-compare a b)])
            (cond [(eq? c 'empty) 'empty] [(= c 0) 'true] [else 'false]))]))

;; equivalence: compare to least precision; frac ignored beyond common precision
(define (moment-equiv? a b)
  (define c (moment-compare a b))
  (and (not (eq? c 'empty)) (= c 0)))

;; ---- rendering ------------------------------------------------------------------------------
(define (p2 x) (if (< x 10) (string-append "0" (number->string x)) (number->string x)))
(define (p4 x) (let ([s (number->string x)]) (string-append (make-string (max 0 (- 4 (string-length s))) #\0) s)))

(define (moment->string m)
  (define date
    (case (moment-kind m)
      [(time) ""]
      [else (string-append "@" (p4 (moment-year m))
                           (if (>= (moment-prec m) 2) (string-append "-" (p2 (moment-month m))) "")
                           (if (>= (moment-prec m) 3) (string-append "-" (p2 (moment-day m))) ""))]))
  (define time
    (cond
      [(and (eq? (moment-kind m) 'time)) (string-append "@T" (time-part m))]
      [(>= (moment-prec m) 4) (string-append "T" (time-part m) (tz-part m))]
      [else ""]))
  (string-append date time))

(define (time-part m)
  (string-append (p2 (moment-hour m))
                 (if (>= (moment-prec m) 5) (string-append ":" (p2 (moment-minute m))) "")
                 (if (>= (moment-prec m) 6) (string-append ":" (p2 (moment-second m))) "")
                 (if (moment-frac m) (string-append "." (moment-frac m)) "")))

(define (tz-part m)
  (cond [(not (moment-tzmin m)) ""]
        [(= (moment-tzmin m) 0) "Z"]
        [else (let* ([t (moment-tzmin m)] [sign (if (< t 0) "-" "+")] [a (abs t)])
                (string-append sign (p2 (quotient a 60)) ":" (p2 (modulo a 60))))]))

;; ---- calendar/time arithmetic --------------------------------------------------------------
;; proleptic Gregorian day number
(define (ymd->jdn y m d)
  (let* ([a (quotient (- 14 m) 12)] [yy (- (+ y 4800) a)] [mm (+ m (* 12 a) -3)])
    (+ d (quotient (+ (* 153 mm) 2) 5) (* 365 yy) (quotient yy 4)
       (- (quotient yy 100)) (quotient yy 400) -32045)))
(define (jdn->ymd j)
  (let* ([a (+ j 32044)] [b (quotient (+ (* 4 a) 3) 146097)] [c (- a (quotient (* 146097 b) 4))]
         [dd (quotient (+ (* 4 c) 3) 1461)] [e (- c (quotient (* 1461 dd) 4))]
         [mm (quotient (+ (* 5 e) 2) 153)])
    (values (+ (* 100 b) dd -4800 (quotient mm 10))
            (+ mm 3 (* -12 (quotient mm 10)))
            (+ e (- (quotient (+ (* 153 mm) 2) 5)) 1))))

(define (days-in-month y m) (let-values ([(y2 m2 d2) (jdn->ymd (sub1 (ymd->jdn y (if (= m 12) 1 (add1 m)) 1)))]) d2))
(define (clamp-day y m d) (min d (days-in-month y m)))

;; add n units (calendar or time) to a moment, respecting its precision.
(define (moment-plus m n unit)
  (define u (singular unit))
  (case u
    [("year") (add-months m (* n 12))]
    [("month") (add-months m n)]
    [("week") (add-days m (* n 7))]
    [("day") (add-days m n)]
    [("hour") (add-seconds m (* n 3600))]
    [("minute") (add-seconds m (* n 60))]
    [("second") (add-seconds m n)]
    [("millisecond") (add-seconds m (quotient n 1000))]
    [else m]))

;; add whole milliseconds, carrying into the date and producing a 3-digit fraction
(define (moment-add-ms m ms)
  (define base (+ (* 3600000 (or (moment-hour m) 0)) (* 60000 (or (moment-minute m) 0))
                  (* 1000 (or (moment-second m) 0)) (frac->ms (moment-frac m)) ms))
  (define day-delta (floor (/ base 86400000)))
  (define t (modulo base 86400000))
  (define m2 (if (and (moment-day m) (not (= day-delta 0))) (add-days m day-delta) m))
  (struct-copy moment m2
               [hour (quotient t 3600000)] [minute (quotient (modulo t 3600000) 60000)]
               [second (quotient (modulo t 60000) 1000)] [frac (pad3 (modulo t 1000))]
               [prec 6]))
(define (frac->ms f)
  (if f (inexact->exact (round (* (or (string->number (string-append "0." f)) 0) 1000))) 0))
(define (pad3 n) (let ([s (number->string n)]) (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))

(define (singular u)
  (if (and (> (string-length u) 0) (char=? (string-ref u (sub1 (string-length u))) #\s))
      (substring u 0 (sub1 (string-length u))) u))

(define (add-months m n)
  (define total (+ (* (or (moment-year m) 0) 12) (- (or (moment-month m) 1) 1) n))
  (define y (quotient total 12)) (define mo (add1 (modulo total 12)))
  (define d (and (moment-day m) (clamp-day y mo (moment-day m))))
  (struct-copy moment m [year y] [month (and (>= (moment-prec m) 2) mo)] [day d]))

(define (add-days m n)
  (define-values (y mo d) (jdn->ymd (+ (ymd->jdn (or (moment-year m) 0) (or (moment-month m) 1) (or (moment-day m) 1)) n)))
  (struct-copy moment m [year y] [month mo] [day d]))

(define (add-seconds m n)
  (define base (+ (* 3600 (or (moment-hour m) 0)) (* 60 (or (moment-minute m) 0)) (or (moment-second m) 0) n))
  (define day-delta (floor (/ base 86400)))
  (define t (modulo base 86400))
  (define m2 (if (and (moment-day m) (not (= day-delta 0))) (add-days m day-delta) m))
  (struct-copy moment m2 [hour (quotient t 3600)] [minute (quotient (modulo t 3600) 60)] [second (modulo t 60)]))

;; low/high boundary of a partial temporal, filled to the precision implied by `digits`.
(define (moment-boundary m0 digits low?)
  ;; an hour-only time/dateTime normalizes to :00 minutes before expansion (T08 -> 08:00)
  (define m (if (and (moment-hour m0) (not (moment-minute m0)))
                (struct-copy moment m0 [minute 0] [prec (max (moment-prec m0) 5)])
                m0))
  (define time? (eq? (moment-kind m) 'time))
  (define tp (digits->prec digits time?))           ; target precision level (1..6)
  (and tp
       (let* ([y (if time? #f (or (moment-year m) 1))]        ; a time carries no date components
              [mo (if time? #f (fill (moment-month m) tp 2 low? 1 12))]
              [d0 (if time? #f (fill (moment-day m) tp 3 low? 1 31))]
              [d (if (and mo (>= tp 3) (not (moment-day m))) (if low? 1 (days-in-month y (or mo 1))) d0)]
              [h (fill (moment-hour m) tp 4 low? 0 23)]
              [mi (fill (moment-minute m) tp 5 low? 0 59)]
              [sec (fill (moment-second m) tp 6 low? 0 59)]
              [frac (cond [(> digits (if time? 6 14)) (or (moment-frac m) (if low? "000" "999"))] [else (moment-frac m)])]
              [tz (cond [(and (not time?) (>= digits 17)) (or (moment-tzmin m) (if low? (* 14 60) (* -12 60)))]
                        [else (moment-tzmin m)])])
         (moment (moment-kind m) y mo d h mi sec frac tz tp))))

(define (fill v tp level low? lo hi)
  (cond [(< tp level) #f]      ; below target precision -> clear (don't keep a stray sub-precision value)
        [v v] [low? lo] [else hi]))

(define (digits->prec digits time?)
  (if time?
      (cond [(<= digits 2) 4] [(<= digits 4) 5] [else 6])
      (cond [(<= digits 4) 1] [(<= digits 6) 2] [(<= digits 8) 3]
            [(<= digits 10) 4] [(<= digits 12) 5] [else 6])))

(module+ test
  (require rackunit)
  (check-equal? (moment-equal? (parse-fp-date "2014") (parse-fp-date "2014")) 'true)
  (check-equal? (moment-equal? (parse-fp-date "2014") (parse-fp-date "2014-01")) 'empty)
  (check-equal? (moment-compare (parse-fp-date "2014-01-01") (parse-fp-date "2014-02-01")) -1)
  ;; tz normalization: 10:00Z == 12:00+02:00
  (check-equal? (moment-compare (parse-fp-datetime "2014-01-01T10:00:00Z")
                                (parse-fp-datetime "2014-01-01T12:00:00+02:00")) 0)
  ;; tz normalization CROSSING MIDNIGHT carries the day: Jan1 23:00-05:00 is Jan2 04:00Z
  (check-equal? (moment-compare (parse-fp-datetime "2014-01-01T23:00:00-05:00")
                                (parse-fp-datetime "2014-01-02T03:00:00Z")) 1)
  (check-equal? (moment-compare (parse-fp-datetime "2014-01-01T01:00:00+03:00")   ; = Dec31 22:00Z
                                (parse-fp-datetime "2013-12-31T22:00:00Z")) 0)
  ;; ...including across a month/year boundary (calendar add-days, not modulo arithmetic)
  (check-equal? (moment-compare (parse-fp-datetime "2013-12-31T23:30:00-01:00")   ; = Jan1 00:30Z
                                (parse-fp-datetime "2014-01-01T00:30:00Z")) 0)
  (check-equal? (moment->string (parse-fp-date "2014-01")) "@2014-01")
  (check-equal? (moment->string (parse-fp-time "12:00:00")) "@T12:00:00"))
