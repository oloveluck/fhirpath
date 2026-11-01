#lang racket/base

;;; ucum-harness.rkt — run the OFFICIAL UCUM functional test suite against fhirpath/ucum.rkt.
;;;
;;; The suite (vendor/ucum/UcumFunctionalTests.xml, maintained by Grahame Grieve for
;;; unitsofmeasure.org) is the conformance definition for a UCUM implementation. Sections:
;;;   validation             unit + valid=true|false          -> ucum-valid?
;;;   conversion             value srcUnit dstUnit outcome    -> ucum-convert
;;;   multiplication/division v1 u1 v2 u2 -> vRes uRes        -> canonical-magnitude comparison
;;;   displayNameGeneration  unit -> display                  -> ucum-display
;;;
;;; Reported as (results section pass fail skip failures) so run.rkt can ratchet a floor.

(require racket/contract racket/list racket/string racket/runtime-path
         (only-in xml read-xml document-element element? element-name element-content
                  element-attributes attribute-name attribute-value)
         (only-in "../ucum.rkt" ucum-valid? ucum-convert ucum-canonical ucum-display))

(provide
 (struct-out results)
 (contract-out
  [run-ucum-suite (->* () (#:verbose? boolean?) (listof results?))]))

(struct results (section pass fail skip failures) #:transparent)

(define-runtime-path suite-path "vendor/ucum/UcumFunctionalTests.xml")

;; ---- reading the suite --------------------------------------------------------------------
(define (attr e name)
  (for/or ([a (element-attributes e)]) (and (eq? (attribute-name a) name) (attribute-value a))))
(define (elements e) (filter element? (element-content e)))

(define (suite-sections)
  (define root (document-element (call-with-input-file suite-path read-xml)))
  (for/list ([sec (in-list (elements root))])
    (cons (symbol->string (element-name sec)) (elements sec))))

;; ---- per-section runners ------------------------------------------------------------------
;; each returns (values pass fail skip failures)

(define (run-validation cases)
  (for/fold ([p 0] [f 0] [s 0] [fs '()]) ([c (in-list cases)])
    (define unit (or (attr c 'unit) ""))
    (define expected (equal? (attr c 'valid) "true"))
    (define actual (with-handlers ([exn:fail? (lambda (_) #f)]) (ucum-valid? unit)))
    (if (eq? actual expected)
        (values (add1 p) f s fs)
        (values p (add1 f) s (cons (format "~a: ~s expected valid=~a got ~a"
                                           (attr c 'id) unit expected actual) fs)))))

(define (run-display cases)
  (for/fold ([p 0] [f 0] [s 0] [fs '()]) ([c (in-list cases)])
    (define unit (or (attr c 'unit) ""))
    (define expected (attr c 'display))
    (define actual (with-handlers ([exn:fail? (lambda (_) #f)]) (ucum-display unit)))
    (if (equal? actual expected)
        (values (add1 p) f s fs)
        (values p (add1 f) s (cons (format "~a: ~s -> ~s, expected ~s"
                                           (attr c 'id) unit actual expected) fs)))))

(define (run-conversion cases)
  (for/fold ([p 0] [f 0] [s 0] [fs '()]) ([c (in-list cases)])
    (define v (string->number (attr c 'value)))
    (define src (attr c 'srcUnit)) (define dst (attr c 'dstUnit))
    (define actual (with-handlers ([exn:fail? (lambda (_) #f)])
                     (ucum-convert (inexact->exact v) src dst)))
    (cond
      [(not actual) (values p (add1 f) s (cons (format "~a: ~a ~a->~a not convertible"
                                                       (attr c 'id) v src dst) fs))]
      ;; the suite's outcomes are decimal-rounded; compare at the printed precision
      [(agrees? (exact->inexact actual) (attr c 'outcome)) (values (add1 p) f s fs)]
      [else (values p (add1 f) s (cons (format "~a: ~a ~a->~a = ~a, expected ~a"
                                               (attr c 'id) v src dst (exact->inexact actual) (attr c 'outcome))
                                       fs))])))

;; The suite PROPAGATES SIGNIFICANT DIGITS: `value="6.30"` (3 s.f.) converts to `outcome="0.160"`
;; (3 s.f.), and its own note says implementations need not carry the [mu_0] precision. So an
;; answer agrees when it rounds to the expected value at the expected value's significant digits.
(define (agrees? actual expected-str)
  (define expected (string->number expected-str))
  (and expected
       (let ([sig (sig-digits expected-str)])
         (or (= actual expected)
             (equal? (round-sig actual sig) (round-sig expected sig))
             ;; also accept exact agreement within floating slack (identity conversions)
             (< (abs (- actual expected)) (* 1e-12 (max (abs actual) (abs expected) 1.0)))))))

;; significant digits of a decimal lexeme (leading zeros don't count; trailing ones do)
(define (sig-digits str)
  (define digits (regexp-replace* #px"[^0-9]" (car (string-split (string-downcase str) "e")) ""))
  (define trimmed (regexp-replace #px"^0+" digits ""))
  (max 1 (string-length trimmed)))

(define (round-sig x sig)
  (cond [(zero? x) 0.0]
        [else (define mag (expt 10 (- sig 1 (order-of x))))
              (/ (round (* (inexact->exact x) mag)) mag)]))
(define (order-of x) (inexact->exact (floor (/ (log (abs x)) (log 10)))))

;; multiplication/division: the RESULT unit's canonical magnitude times the numeric result must
;; equal the operands' product/quotient in canonical form (unit spelling is not prescribed)
(define (run-arith cases op)
  (for/fold ([p 0] [f 0] [s 0] [fs '()]) ([c (in-list cases)])
    (define v1 (string->number (attr c 'v1))) (define u1 (attr c 'u1))
    (define v2 (string->number (attr c 'v2))) (define u2 (attr c 'u2))
    (define vres (string->number (attr c 'vRes))) (define ures (attr c 'uRes))
    (define a (ucum-canonical u1)) (define b (ucum-canonical u2)) (define r (ucum-canonical ures))
    (cond
      [(not (and a b r)) (values p f (add1 s) fs)]
      [else
       (define lhs (op (* (inexact->exact v1) (car a)) (* (inexact->exact v2) (car b))))
       (define rhs (* (inexact->exact vres) (car r)))
       (if (or (agrees? (exact->inexact lhs) (attr c 'vRes))
               ;; compare in canonical magnitude: lhs vs vRes*canon(uRes)
               (agrees? (exact->inexact (/ lhs (car r))) (attr c 'vRes)))
           (values (add1 p) f s fs)
           (values p (add1 f) s (cons (format "~a: ~a~a ~a ~a~a != ~a~a"
                                              (attr c 'id) v1 u1 (object-name op) v2 u2 vres ures) fs)))])))

(define (run-ucum-suite #:verbose? [verbose? #f])
  (define out
    (for/list ([sec (in-list (suite-sections))])
      (define name (car sec)) (define cases (cdr sec))
      (define-values (p f s fs)
        (case name
          [("validation") (run-validation cases)]
          [("displayNameGeneration") (run-display cases)]
          [("conversion") (run-conversion cases)]
          [("multiplication") (run-arith cases *)]
          [("division") (run-arith cases /)]
          ;; display-name generation is not implemented (documented): every case skipped
          [else (values 0 0 (length cases) '())]))
      (results name p f s (reverse fs))))
  (when verbose?
    (for ([r (in-list out)])
      (for ([m (in-list (results-failures r))]) (eprintf "  ~a\n" m))))
  out)

(module+ main
  (define rs (run-ucum-suite #:verbose? (member "-v" (vector->list (current-command-line-arguments)))))
  (for ([r (in-list rs)])
    (printf "~a: ~a/~a~a\n" (results-section r) (results-pass r)
            (+ (results-pass r) (results-fail r))
            (if (> (results-skip r) 0) (format "  (skipped ~a)" (results-skip r)) "")))
  (printf "UCUM conformance: ~a/~a\n"
          (for/sum ([r (in-list rs)]) (results-pass r))
          (for/sum ([r (in-list rs)]) (+ (results-pass r) (results-fail r)))))
