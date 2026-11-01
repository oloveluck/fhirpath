#lang racket/base

;;; run-ucum.rkt — `raco fhir ucum-tck`: the official UCUM functional suite with a ratcheted floor.

(require racket/list
         (only-in "ucum-harness.rkt" run-ucum-suite results results-section results-pass
                  results-fail results-skip results-failures))

;; NEVER REGRESS: raise when the implementation improves, never lower to make a run pass.
(define floor-pass 573)

(module+ main
  (define verbose? (and (member "-v" (vector->list (current-command-line-arguments))) #t))
  (define rs (run-ucum-suite #:verbose? verbose?))
  (define pass (for/sum ([r (in-list rs)]) (results-pass r)))
  (define total (for/sum ([r (in-list rs)]) (+ (results-pass r) (results-fail r))))
  (define skipped (for/sum ([r (in-list rs)]) (results-skip r)))
  (for ([r (in-list rs)] #:when (> (+ (results-pass r) (results-fail r)) 0))
    (printf "  ~a: ~a/~a\n" (results-section r) (results-pass r)
            (+ (results-pass r) (results-fail r))))
  (printf "UCUM conformance: ~a/~a  (skipped ~a: display-name generation, not implemented)\n"
          pass total skipped)
  (when (< pass floor-pass)
    (eprintf "REGRESSION: ~a < floor ~a\n" pass floor-pass)
    (exit 1)))
