#lang racket/base

;;; ucum.rkt — a compact UCUM engine ported from ucum-essence.xml (the fhir4s data + approach).
;;;
;;; Canonicalizes a unit expression to (magnitude · 7-dimension-vector) using exact rationals, so
;;; commensurability, conversion, and quantity comparison are exact. Handles prefixes (longest
;;; match on metric units), derived units (recursive substitution), compound expressions
;;; (`.`/`/`, integer exponents, `[bracketed]` atoms, `{annotations}`). Special (function) units
;;; (Cel, [degF], pH, …) are reported as non-canonicalizable (#f).

(require racket/contract racket/list racket/string racket/runtime-path
         (only-in xml read-xml document-element element? element-name element-content
                  element-attributes attribute-name attribute-value pcdata? pcdata-string
                  entity? entity-text))

(provide
 (contract-out
  [ucum-canonical    (-> string? (or/c (cons/c rational? vector?) #f))]
  [ucum-commensurable? (-> string? string? boolean?)]
  [ucum-convert      (-> rational? string? string? (or/c rational? #f))]
  ;; syntactic + atom validity, INDEPENDENT of canonicalizability: a special (function) unit
  ;; like Cel/[degF]/[pH] is valid UCUM even though it has no linear canonical form
  [ucum-valid?       (-> string? boolean?)]
  ;; the official suite's displayNameGeneration: "m" -> "(meter)", "m3.kg-1" ->
  ;; "(meter ^ 3) * (kilogram ^ -1)"; #f when an atom has no name in the essence
  [ucum-display      (-> string? (or/c string? #f))]))

(define-runtime-path essence-path "ucum/ucum-essence.xml")

;; dimension order: L T M A C Q F
(define dim-index (hash "L" 0 "T" 1 "M" 2 "A" 3 "C" 4 "Q" 5 "F" 6))
(define (zero-dim) (make-vector 7 0))
(define (dim-add a b) (build-vector 7 (lambda (i) (+ (vector-ref a i) (vector-ref b i)))))
(define (dim-scale a k) (build-vector 7 (lambda (i) (* k (vector-ref a i)))))

;; ---- load the essence -----------------------------------------------------------------------
(define prefixes (make-hash))     ; code -> exact factor
(define base-units (make-hash))   ; code -> dim-vector
(define units (make-hash))        ; code -> (cons value-num unit-string) | 'special
;; human-readable names, for display generation (the official suite's displayNameGeneration)
(define prefix-names (make-hash))  ; prefix code -> name  ("m" -> "milli")
(define unit-names (make-hash))    ; unit/base code -> name ("m" -> "meter")

(define (attr e name) (for/or ([a (element-attributes e)]) (and (eq? (attribute-name a) name) (attribute-value a))))
(define (child-text e tag)
  (for/or ([c (element-content e)] #:when (and (element? c) (eq? (element-name c) tag)))
    c))

;; the character data directly inside an element, resolving XML entities (`&#232;` in "ampère")
(define (element-text e)
  (apply string-append
         (for/list ([c (in-list (element-content e))]
                    #:when (or (pcdata? c) (entity? c)))
           (cond [(pcdata? c) (pcdata-string c)]
                 [else (let ([t (entity-text c)])
                         (if (exact-integer? t) (string (integer->char t)) (format "&~a;" t)))]))))

(define (parse-ucum-num s)
  (define m (regexp-match #px"^([+-]?[0-9]*[.]?[0-9]+)(?:[eE]([+-]?[0-9]+))?$" (string-trim s)))
  (and m (let* ([mant (list-ref m 1)] [ex (let ([e (list-ref m 2)]) (if e (string->number e) 0))]
                [dm (regexp-match #px"^([+-]?)([0-9]*)(?:[.]([0-9]+))?$" mant)])
           (and dm
                (let* ([sign (if (equal? (list-ref dm 1) "-") -1 1)]
                       [ip (list-ref dm 2)] [fp (or (list-ref dm 3) "")]
                       [digits (string->number (string-append (if (equal? ip "") "0" ip) fp))]
                       [scale (string-length fp)])
                  (* sign digits (expt 10 (- ex scale))))))))

(define (load-essence!)
  (define doc (call-with-input-file essence-path read-xml))
  (let walk ([x (document-element doc)])
    (when (element? x)
      (case (element-name x)
        [(prefix) (let ([code (attr x 'Code)] [v (child-text x 'value)])
                    (when (and code v) (hash-set! prefixes code (parse-ucum-num (attr v 'value))))
                    (let ([n (child-text x 'name)])
                      (when (and code n) (hash-set! prefix-names code (element-text n)))))]
        [(base-unit) (let ([code (attr x 'Code)] [d (attr x 'dim)])
                       (when (and code d (hash-has-key? dim-index d))
                         (let ([v (zero-dim)]) (vector-set! v (hash-ref dim-index d) 1)
                           (hash-set! base-units code v)))
                       (let ([n (child-text x 'name)])
                         (when (and code n) (hash-set! unit-names code (element-text n)))))]
        [(unit) (let ([code (attr x 'Code)] [v (child-text x 'value)])
                  (let ([n (child-text x 'name)])
                    (when (and code n) (hash-set! unit-names code (element-text n))))
                  (when code
                    (define unit-ref (and v (attr v 'Unit)))
                    (define val (and v (attr v 'value)))
                    (define num (and val (parse-ucum-num val)))
                    (hash-set! units code
                               (if (and unit-ref num) (cons num unit-ref) 'special))))]
        [else (void)])
      (for ([c (element-content x)]) (walk c)))))

(define loaded? #f)
(define (ensure-loaded!) (unless loaded? (load-essence!) (set! loaded? #t)))

;; ---- canonicalization -----------------------------------------------------------------------
;; resolve a single atom code (possibly prefixed) -> (cons magnitude dim-vector) | #f
(define (resolve-atom code)
  (cond
    [(string=? code "1") (cons 1 (zero-dim))]
    ;; a bare integer is a numeric factor: `4.s/m` = 4 second per metre
    [(regexp-match? #px"^[0-9]+$" code) (cons (string->number code) (zero-dim))]
    [(hash-ref base-units code #f) => (lambda (dv) (cons 1 dv))]
    [(hash-ref units code #f)
     => (lambda (u) (and (pair? u)
                         (let ([inner (canon-expr (cdr u))])
                           (and inner (cons (* (car u) (car inner)) (cdr inner))))))]
    [else (resolve-prefixed code)]))

(define (resolve-prefixed code)
  ;; longest prefix whose remainder is a known metric unit/base
  (for/or ([n (in-range (sub1 (string-length code)) 0 -1)])
    (define p (substring code 0 n)) (define r (substring code n))
    (define pf (hash-ref prefixes p #f))
    (and pf (or (hash-ref base-units r #f) (and (pair? (hash-ref units r 'x)) #t))
         (let ([inner (resolve-atom r)])
           (and inner (cons (* pf (car inner)) (cdr inner)))))))

;; canonicalize a compound unit expression -> (cons magnitude dim-vector) | #f
(define (canon-expr str)
  (ensure-loaded!)
  (define s (regexp-replace* #px"[{][^}]*[}]" str ""))     ; drop annotations
  (cond
    [(or (string=? s "") (string=? s "1")) (cons 1 (zero-dim))]
    [else
     (let loop ([i 0] [sign 1] [mag 1] [dim (zero-dim)])
       (cond
         [(>= i (string-length s)) (cons mag dim)]
         [else
          (define c (string-ref s i))
          (cond
            [(char=? c #\.) (loop (add1 i) 1 mag dim)]
            [(char=? c #\/) (loop (add1 i) -1 mag dim)]
            [else
             (define-values (atom exp next) (read-term s i))
             (define r (resolve-atom atom))
             (cond [(not r) #f]
                   [else (define e (* sign exp))
                         (loop next 1 (* mag (expt (car r) e)) (dim-add dim (dim-scale (cdr r) e)))])])]))]))

;; read one term at position i: an atom (letters, digits, [brackets], %) + optional integer exponent
(define (read-term s i)
  (define n (string-length s))
  ;; a term STARTING with digits is either a plain factor (`4`) or the `10*`/`10^` arbitrary-power
  ;; atom, whose exponent follows (`10*-7`) — both need the digits kept together
  (define digit-end
    (and (< i n) (char-numeric? (string-ref s i))
         (let d ([k i]) (if (and (< k n) (char-numeric? (string-ref s k))) (d (add1 k)) k))))
  (cond
    [(and digit-end (< digit-end n) (memv (string-ref s digit-end) '(#\* #\^)))
     (finish s i (add1 digit-end))]                       ; atom is "10*" ; exponent parsed after
    [digit-end (values (substring s i digit-end) 1 digit-end)]
    [else (read-atom-term s i n)]))

(define (read-atom-term s i n)
  (let loop ([j i])
    (cond
      [(>= j n) (finish s i j)]
      [(char=? (string-ref s j) #\[)                       ; bracketed atom: consume to ]
       (let bloop ([k (add1 j)]) (if (or (>= k n) (char=? (string-ref s k) #\])) (loop (add1 k)) (bloop (add1 k))))]
      [(memv (string-ref s j) '(#\. #\/)) (finish s i j)]
      [(and (or (char=? (string-ref s j) #\+) (char=? (string-ref s j) #\-) (char-numeric? (string-ref s j)))
            (> j i) (not (char=? (string-ref s (sub1 j)) #\[)))
       ;; start of an exponent (a sign/digit after the atom body)
       (finish s i j)]
      [else (loop (add1 j))])))

(define (finish s start atom-end)
  (define n (string-length s))
  ;; parse trailing exponent from atom-end
  (let eloop ([k atom-end])
    (cond
      [(and (< k n) (or (char=? (string-ref s k) #\+) (char=? (string-ref s k) #\-) (char-numeric? (string-ref s k))))
       (eloop (add1 k))]
      [else
       (define atom (substring s start atom-end))
       (define exp-str (substring s atom-end k))
       (values atom (if (or (string=? exp-str "") (string=? exp-str "+") (string=? exp-str "-"))
                        1 (or (string->number exp-str) 1))
               k)])))

;; ---- validity (the official functional-test notion) ------------------------------------------
;; A unit expression is VALID when it parses per the UCUM syntax and every atom is known —
;; independent of canonicalizability: a special (function) unit (Cel, [degF], [pH]) is valid but
;; has no linear canonical form, so `ucum-canonical` returns #f for it while `ucum-valid?` is #t.
;;
;; The UCUM grammar (unitsofmeasure.org §, recursive descent below):
;;   main-term  := '/' term | term
;;   term       := component (('.' | '/') component)*
;;   component  := annotatable annotation? | annotation | factor | '(' term ')'
;;   annotatable:= simple-unit exponent?
;;   simple-unit:= ATOM | PREFIX ATOM        (metric units only, longest prefix)
;;   exponent   := ('+' | '-')? digits ; factor := digits ; annotation := '{' … '}'
(define (ucum-valid? unit)
  (ensure-loaded!)
  (define s unit)
  (define n (string-length s))
  ;; each parser returns the next index, or #f
  (define (main-term i)
    (define j (if (and (< i n) (char=? (string-ref s i) #\/)) (add1 i) i))
    (term j))
  (define (term i)
    (let loop ([i (component i)])
      (cond [(not i) #f]
            [(and (< i n) (memv (string-ref s i) '(#\. #\/)))
             (define next (component (add1 i)))
             (if next (loop next) #f)]
            [else i])))
  (define (component i)
    (cond
      [(>= i n) #f]
      [(char=? (string-ref s i) #\()
       (define j (term (add1 i)))
       (and j (< j n) (char=? (string-ref s j) #\)) (add1 j))]
      [(char=? (string-ref s i) #\{) (annotation i)]     ; a bare annotation is a component
      [else
       (define j (annotatable i))
       (cond [(not j) #f]
             [(and (< j n) (char=? (string-ref s j) #\{)) (annotation j)]
             [else j])]))
  ;; a simple unit plus an optional exponent. A pure FACTOR takes no exponent — `10+3` is
  ;; invalid while `10*3` is valid, because `10*` is a unit atom and `10` is a number.
  (define (annotatable i)
    (define r (simple-unit i))
    (and r (if (cdr r) (car r) (exponent (car r)))))
  (define (exponent i)
    (define j (if (and (< i n) (memv (string-ref s i) '(#\+ #\-))) (add1 i) i))
    (let loop ([k j]) (cond [(and (< k n) (char-numeric? (string-ref s k))) (loop (add1 k))]
                            [(> k j) k]                  ; digits consumed
                            [(= j i) i]                  ; no sign, no digits -> no exponent
                            [else #f])))                 ; a lone sign is invalid
  ;; ATOM: bracketed [..], or a run of unit characters; a pure digit run is a factor
  ;; -> (cons end factor?) | #f   factor? = the token is a bare number, not a unit atom
  (define (simple-unit i)
    (cond
      [(>= i n) #f]
      [(char-numeric? (string-ref s i))
       ;; digits: a plain factor, or the `10*`/`10^` arbitrary-power atom (10*3 = 10^3)
       (define d (let loop ([k i]) (if (and (< k n) (char-numeric? (string-ref s k))) (loop (add1 k)) k)))
       (cond [(and (< d n) (memv (string-ref s d) '(#\* #\^)) (known-atom? (substring s i (add1 d))))
              (cons (add1 d) #f)]
             [else (cons d #t)])]
      [else
       (define j (atom-end i))
       (and j (> j i) (known-atom? (substring s i j)) (cons j #f))]))
  ;; consume one atom: bracket groups are atomic; stop at an operator/paren/brace/exponent sign
  (define (atom-end i)
    (let loop ([k i] [seen-alpha? #f])
      (cond
        [(>= k n) k]
        [(char=? (string-ref s k) #\[)
         (define close (let b ([m (add1 k)])
                         (cond [(>= m n) #f]
                               [(char=? (string-ref s m) #\]) (add1 m)]
                               [else (b (add1 m))])))
         (and close (loop close #t))]
        [(memv (string-ref s k) '(#\. #\/ #\( #\) #\{ #\} #\])) k]
        [(and (memv (string-ref s k) '(#\+ #\-)) seen-alpha?) k]   ; start of an exponent
        [(and (char-numeric? (string-ref s k)) seen-alpha?) k]     ; digits after letters = exponent
        [else (loop (add1 k) #t)])))
  ;; an annotation's content must be ASCII printable (the suite: "Unicode is not allowed")
  (define (annotation i)
    (and (< i n) (char=? (string-ref s i) #\{)
         (let loop ([k (add1 i)])
           (cond [(>= k n) #f]
                 [(char=? (string-ref s k) #\}) (add1 k)]
                 [(let ([c (char->integer (string-ref s k))]) (or (< c 32) (> c 126))) #f]
                 [else (loop (add1 k))]))))
  ;; a known atom: base unit, defined unit (INCLUDING special), or prefix + metric unit
  (define (known-atom? code)
    (or (string=? code "1")
        (and (hash-ref base-units code #f) #t)
        (and (hash-ref units code #f) #t)
        (known-prefixed? code)))
  (define (known-prefixed? code)
    (for/or ([k (in-range (sub1 (string-length code)) 0 -1)])
      (define p (substring code 0 k)) (define r (substring code k))
      (and (hash-ref prefixes p #f)
           (or (and (hash-ref base-units r #f) #t) (and (hash-ref units r #f) #t)))))
  (cond
    [(string=? s "") #t]                                  ; the empty unit is unity
    [else (let ([end (main-term 0)]) (and end (= end n)))]))

;; ---- display-name generation -----------------------------------------------------------------
;; Render a unit expression as the suite's human-readable form: each term becomes "(name)" or
;; "(name ^ exponent)", a numeric factor stays bare, terms join with " * " and a division with
;; " / " (the exponent stays positive after a slash, per the suite's `N/A2` -> "/ (ampère ^ 2)").
(define (ucum-display unit)
  (ensure-loaded!)
  (define s (regexp-replace* #px"[{][^}]*[}]" unit ""))
  (cond
    [(string=? s "") "(unity)"]
    [else
     (let loop ([i 0] [op #f] [parts '()])
       (cond
         [(>= i (string-length s))
          (and (pair? parts) (apply string-append (reverse parts)))]
         [else
          (define c (string-ref s i))
          (cond
            [(char=? c #\.) (loop (add1 i) " * " parts)]
            [(char=? c #\/) (loop (add1 i) " / " parts)]
            [else
             (define-values (atom exp next) (read-term s i))
             (define rendered (render-atom atom exp))
             (and rendered
                  (loop next #f (cons rendered (if op (cons op parts) parts))))])]))]))

;; one atom + exponent -> "(name)" / "(name ^ e)" / a bare factor
(define (render-atom atom exp)
  (define name (atom-name atom))
  (cond
    [(not name) #f]
    [(and (regexp-match? #px"^[0-9]+$" atom) (= exp 1)) atom]     ; a numeric factor prints bare
    [(= exp 1) (string-append "(" name ")")]
    [else (format "(~a ^ ~a)" name exp)]))

;; an atom's human name: a known unit/base, a prefixed metric unit ("mm" -> "millimeter"), or #f
(define (atom-name code)
  (or (hash-ref unit-names code #f)
      (and (regexp-match? #px"^[0-9]+$" code) code)
      (for/or ([n (in-range (sub1 (string-length code)) 0 -1)])
        (define p (substring code 0 n)) (define r (substring code n))
        (define pn (hash-ref prefix-names p #f))
        (define un (hash-ref unit-names r #f))
        (and pn un (string-append pn un)))))

;; ---- public API -----------------------------------------------------------------------------
(define (ucum-canonical unit) (canon-expr unit))

(define (ucum-commensurable? u1 u2)
  (define a (ucum-canonical u1)) (define b (ucum-canonical u2))
  (and a b (equal? (cdr a) (cdr b))))

;; value in u1 expressed in u2 (exact), or #f if not commensurable / special
(define (ucum-convert value u1 u2)
  (define a (ucum-canonical u1)) (define b (ucum-canonical u2))
  (and a b (equal? (cdr a) (cdr b)) (/ (* value (car a)) (car b))))

(module+ test
  (require rackunit)
  (check-true  (ucum-commensurable? "g" "mg"))
  (check-true  (ucum-commensurable? "wk" "d"))
  (check-false (ucum-commensurable? "g" "s"))
  (check-equal? (ucum-convert 4 "g" "mg") 4000)
  (check-equal? (ucum-convert 1 "wk" "d") 7)
  (check-equal? (ucum-convert 1 "min" "s") 60)
  (check-equal? (ucum-convert 1 "km" "m") 1000))
