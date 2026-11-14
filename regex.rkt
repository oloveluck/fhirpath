#lang racket/base

;;; regex.rkt — FHIRPath's regular expressions, read by Racket.
;;;
;;; FHIRPath prescribes no dialect but recommends PCRE ("matches", "replaceMatches": case-sensitive,
;;; single-line mode, Unicode); FHIR's own invariants are written for it (eld-20's `(\\[x])?`).
;;; Racket's pregexp is close, with four differences a published expression meets:
;;;   - a `]` outside a character class, and a `{`/`}` that starts no repetition, are literal in PCRE
;;;     and errors in pregexp (eld-20 failed on every ElementDefinition) — they are escaped;
;;;   - named groups `(?<name>…)` / `(?P<name>…)` — plain groups here, their names remembered;
;;;   - a substitution names groups as `$1` or `${name}` (the specification's example), where
;;;     Racket's own syntax is `\1` and `&` for the whole match, so a literal `&` was the match —
;;;     substitutions are applied by `substitute`, never through Racket's syntax;
;;;   - the current build's `flags`: `i` (case-insensitive), `m` (multi-line).
;;; A compiled pattern is remembered by its text and flags (`compiled`): a pattern is a literal in
;;; nearly every expression, and compiling it was 40% of evaluating one that uses it.
;;; A pattern that does not compile is an execution error, said as FHIRPath's.

(require racket/contract racket/string racket/list
         "error.rkt")

(provide
 (contract-out
  ;; regex-matches? : String String [String] [#:full? Boolean] -> Boolean
  [regex-matches?  (->* (string? string?) ((or/c string? #f) #:full? boolean?) boolean?)]
  ;; regex-replace : String String String [String] -> String   every match replaced
  [regex-replace   (->* (string? string? string?) ((or/c string? #f)) string?)]
  ;; translate-pattern : String -> (values String (listof (or/c String #f)))
  ;; the pattern as pregexp reads it, and each group's name (#f for an unnamed group), in order
  [translate-pattern (-> string? (values string? list?))]))

;; A Compiled is (compiled rx names): the pregexp and the names of its groups by number (1-based).
(struct compiled (rx names))

;; compile-pattern : String String|#f Boolean -> Compiled
(define cache (make-hash))
(define (compile-pattern pattern flags full?)
  (define key (vector pattern flags full?))
  (or (hash-ref cache key #f)
      (let ([c (build-pattern pattern flags full?)])
        (when (> (hash-count cache) 2000) (hash-clear! cache))   ; bounded: patterns can be data
        (hash-set! cache key c)
        c)))

(define (build-pattern pattern flags full?)
  (define-values (text names) (translate-pattern pattern))
  (define body (if full? (string-append "^(?:" text ")$") text))
  (define rx (with-handlers ([exn:fail? (lambda (e) (fp-error 'execution (format "invalid regular expression '~a': ~a" pattern (first-line (exn-message e)))))])
               (pregexp (string-append (flag-prefix flags) body (flag-suffix flags)))))
  (compiled rx names))

(define (first-line s) (car (string-split (regexp-replace #rx"^pregexp: " s "") "\n")))

;; flag-prefix : String|#f -> String   the current build's flags as pregexp's mode syntax
(define (flag-prefix flags)
  (cond
    [(or (not flags) (string=? flags "")) ""]
    [else
     (for ([c (in-string flags)])
       (unless (memv c '(#\i #\m))
         (fp-error 'execution (format "invalid regular expression flag '~a' (the flags are i and m)" c))))
     (string-append (if (regexp-match? #rx"i" flags) "(?i:" "")
                    (if (regexp-match? #rx"m" flags) "(?m:" ""))]))
;; flag-suffix : String|#f -> String   closes what flag-prefix opened
(define (flag-suffix flags)
  (if (not flags) "" (make-string (+ (if (regexp-match? #rx"i" flags) 1 0) (if (regexp-match? #rx"m" flags) 1 0)) #\))))

;; translate-pattern : String -> (values String (listof String|#f))
;; one pass over the pattern, tracking escapes and character classes (where `]` first, or after
;; `^`, is literal); outside a class a `]` and a `{`/`}` that is no repetition are escaped, and a
;; named group's name is dropped and recorded
(define (translate-pattern p)
  (define n (string-length p))
  (define (at i) (and (< i n) (string-ref p i)))
  (let loop ([i 0] [in-class? #f] [out '()] [names '()])
    (define c (at i))
    (cond
      [(not c) (values (apply string-append (reverse out)) (reverse names))]
      [(char=? c #\\)
       (define next (at (add1 i)))
       (loop (+ i (if next 2 1)) in-class? (cons (if next (string c next) "\\\\") out) names)]
      [(and in-class? (char=? c #\[) (regexp-match #px"^\\[:[a-z]+:\\]" p i))   ; [:alpha:]
       => (lambda (m) (loop (+ i (string-length (car m))) #t (cons (car m) out) names))]
      [in-class?
       (loop (add1 i) (not (char=? c #\])) (cons (string c) out) names)]
      [(char=? c #\[)
       ;; a `]` right after `[` or `[^` is a member, not the end of the class
       (define start (if (eqv? (at (add1 i)) #\^) (+ i 2) (add1 i)))
       (define lead (if (eqv? (at start) #\]) (add1 start) start))
       (loop lead #t (cons (substring p i lead) out) names)]
      [(char=? c #\]) (loop (add1 i) #f (cons "\\]" out) names)]
      [(char=? c #\{)
       (define m (regexp-match #px"^\\{[0-9]*(?:,[0-9]*)?\\}" p i))
       (if (and m (regexp-match? #px"[0-9]" (car m)))
           (loop (+ i (string-length (car m))) #f (cons (car m) out) names)
           (loop (add1 i) #f (cons "\\{" out) names))]
      [(char=? c #\}) (loop (add1 i) #f (cons "\\}" out) names)]
      [(char=? c #\()
       (define named (regexp-match #px"^\\(\\?P?<([A-Za-z_][A-Za-z0-9_]*)>" p i))
       (cond
         [named (loop (+ i (string-length (car named))) #f (cons "(" out) (cons (cadr named) names))]
         [(eqv? (at (add1 i)) #\?) (loop (add1 i) #f (cons "(" out) names)]   ; (?: (?= (?i … : no group
         [else (loop (add1 i) #f (cons "(" out) (cons #f names))])]
      [else (loop (add1 i) #f (cons (string c) out) names)])))

;; regex-matches? : String String [String|#f] -> Boolean   matches / matchesFull
(define (regex-matches? s pattern [flags #f] #:full? [full? #f])
  (regexp-match? (compiled-rx (compile-pattern pattern flags full?)) s))

;; regex-replace : String String String [String|#f] -> String
;; every match replaced by `substitution`, in which `$n` and `${name}` are the groups (`$0` the
;; match) and everything else is literal; an empty pattern changes nothing
(define (regex-replace s pattern substitution [flags #f])
  (cond
    [(string=? pattern "") s]
    [else
     (define c (compile-pattern pattern flags #f))
     (define parts (substitution-parts substitution (compiled-names c)))
     (regexp-replace* (compiled-rx c) s
                      (lambda groups
                        (apply string-append
                               (for/list ([part (in-list parts)])
                                 (if (string? part) part (or (list-ref-or groups part) ""))))))]))

(define (list-ref-or l k) (and (< k (length l)) (list-ref l k)))

;; A Part is a String (literal) or a Nat (a group: 0 the whole match)
;; substitution-parts : String (listof String|#f) -> (listof Part)
;; `$n` / `${n}` / `${name}` are groups; `$$` and `\$` a dollar; `\\` a backslash; a `$` naming
;; no group of the pattern stays literal
(define (substitution-parts sub names)
  (define n (string-length sub))
  (define (group-of name)
    (cond [(regexp-match? #px"^[0-9]+$" name) (let ([k (string->number name)]) (and (<= k (length names)) k))]
          [(index-of names name) => add1]
          [else #f]))
  (let loop ([i 0] [acc '()] [lit '()])
    (define (flush) (if (null? lit) acc (cons (list->string (reverse lit)) acc)))
    (cond
      [(>= i n) (reverse (flush))]
      [(and (char=? (string-ref sub i) #\\) (< (add1 i) n) (memv (string-ref sub (add1 i)) '(#\$ #\\)))
       (loop (+ i 2) acc (cons (string-ref sub (add1 i)) lit))]
      [(char=? (string-ref sub i) #\$)
       (define braced (regexp-match #px"^\\$\\{([A-Za-z0-9_]+)\\}" sub i))
       (define digits (regexp-match #px"^\\$([0-9]+)" sub i))
       (define m (or braced digits))
       (define g (and m (group-of (cadr m))))
       (cond [(and (< (add1 i) n) (char=? (string-ref sub (add1 i)) #\$)) (loop (+ i 2) acc (cons #\$ lit))]
             [g (loop (+ i (string-length (car m))) (cons g (flush)) '())]
             [else (loop (add1 i) acc (cons #\$ lit))])]
      [else (loop (add1 i) acc (cons (string-ref sub i) lit))])))

(module+ test
  (require rackunit)
  (define (fails? rx) (lambda (e) (and (exn:fp? e) (eq? (exn:fp-kind e) 'execution) (regexp-match? rx (exn-message e)))))
  ;; PCRE's literal brackets and braces (eld-20 failed to compile: `unmatched ]`)
  (check-true (regex-matches? "Patient.value[x]" "^[A-Za-z][A-Za-z0-9]{0,63}(\\.[a-z][A-Za-z0-9]{0,63}(\\[x])?)*$"))
  (check-false (regex-matches? "Patient.Value[x]" "^[A-Za-z][A-Za-z0-9]{0,63}(\\.[a-z][A-Za-z0-9]{0,63}(\\[x])?)*$"))
  (check-true (regex-matches? "a]" "a]"))
  (check-true (regex-matches? "a{b}" "^a{b}$"))
  (check-true (regex-matches? "]" "[]]"))
  (check-true (regex-matches? "x" "[^]a]"))
  (check-true (regex-matches? "aa" "^a{2}$"))
  (check-true (regex-matches? "aaa" "^a{2,}$"))
  (check-true (regex-matches? "b" "^[[:alpha:]]$"))
  (check-true (regex-matches? "a]" "^[a\\]]+$"))
  ;; matchesFull is the whole string; flags
  (check-false (regex-matches? "abc" "ab" #:full? #t))
  (check-true (regex-matches? "abc" "a|abc" #:full? #t))
  (check-true (regex-matches? "first line\nsecond line" "^second" "m"))
  (check-false (regex-matches? "first line\nsecond line" "^second" ""))
  (check-true (regex-matches? "first line\nsecond line" "^SECOND" "im"))
  (check-exn (fails? #rx"flag 'x'") (lambda () (regex-matches? "a" "a" "x")))
  ;; a pattern that is no regular expression is FHIRPath's execution error, not Racket's
  (check-exn (fails? #rx"invalid regular expression '\\(a'") (lambda () (regex-matches? "a" "(a")))
  ;; substitutions: groups by number and by name, `&` and `\` literal
  (check-equal? (regex-replace "11/30/1972" "\\b(?<month>\\d{1,2})/(?<day>\\d{1,2})/(?<year>\\d{2,4})\\b"
                               "${day}-${month}-${year}")
                "30-11-1972")
  (check-equal? (regex-replace "aaabaa" "aa" "\"aa\"") "\"aa\"ab\"aa\"")
  (check-equal? (regex-replace "abc" "(b)" "[$1$0]") "a[bb]c")
  (check-equal? (regex-replace "a.b" "\\." "&") "a&b")
  (check-equal? (regex-replace "ab" "b" "\\1") "a\\1")
  (check-equal? (regex-replace "ab" "b" "$$ $9 \\$") "a$ $9 $")
  (check-equal? (regex-replace "abc" "" "x") "abc")
  (check-equal? (regex-replace "aBc" "b" "-" "i") "a-c")
  ;; the translation itself
  (let-values ([(t names) (translate-pattern "(?<y>\\d+)-(\\d+)(?:x)")])
    (check-equal? t "(\\d+)-(\\d+)(?:x)")
    (check-equal? names '("y" #f))))
