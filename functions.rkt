#lang racket/base

;;; functions.rkt — the FHIRPath function library.
;;;
;;; A registry maps name -> (fn proc lazy? min max). Eager functions receive already-evaluated
;;; argument collections; lazy functions (where/select/repeat/all/iif/aggregate/trace/defineVariable
;;; and the type functions is/as/ofType) receive the raw argument ASTs plus the evaluator `ev` so
;;; they control evaluation and per-item $this/$index binding.

(require racket/contract racket/list racket/string racket/math racket/date
         "error.rkt"
         (only-in fhir/model/node fhir? fhir-type fhir-ref)
         (only-in fhir/model/base primitive-type? element-extension element-id)
         (only-in "value.rkt" item->system item-fhir-type value-less? fp-quantity fp-quantity?
                  fp-quantity-value fp-quantity-unit fp-quantity-calendar? fp-long fp-long? fp-long-v as-decimal)
         (only-in "types.rkt" item-is? item-of-type? known-type? type-info-of)
         (only-in "value.rkt" typeinfo)
         (only-in "navigate.rkt" node-children navigate)
         (only-in "equality.rkt" item=? item-equiv?)
         (only-in "decimal.rkt" fp-decimal? parse-decimal integer->decimal decimal->exact
                  decimal->string decimal-round decimal-precision fp-decimal decimal-cmp)
         (only-in "temporal-ops.rkt" value->moment moment? moment-kind moment->string parse-fp-date
                  parse-fp-datetime parse-fp-time moment moment-year moment-month moment-day
                  moment-compare moment-prec moment-frac moment-boundary)
         (only-in "quantity.rkt" make-quantity quantity->string calendar-unit? quantity-compare)
         (only-in "environment.rkt" env-with-item env-with-this env-with-total env-define-var
                  env-clone-vars env-var-bound? env-this env-root)
         (only-in "terminology.rkt" tx-service? tx-service-expand tx-service-validate tx-service-translate)
         (only-in "parser/ast.rkt" fp-invoke fp-invoke? fp-invoke-name fp-invoke-target
                  fp-lit fp-lit? fp-lit-value fp-unop fp-unop? fp-unop-op fp-unop-operand)
         (only-in net/base64 base64-encode base64-decode))

(provide (contract-out
          [function-known? (-> symbol? boolean?)]
          [apply-function  (-> symbol? list? (listof any/c) any/c procedure? list? list?)]))

;; ---- dispatch -------------------------------------------------------------------------------
(struct fn (proc lazy? min max) #:transparent)

(define (function-known? name) (and (hash-ref TABLE name #f) #t))

;; `focus` is the collection the call appears in (the outer focus); eager arguments are evaluated
;; against it, not against the function's subject — so e.g. combine(name.family) resolves name.family
;; from the invocation context, and inside select() from the per-item projection context.
(define (apply-function name subject arg-asts env ev focus)
  (define f (hash-ref TABLE name #f))
  (unless f (fp-error 'semantic (format "unknown function ~a()" name)))
  (define n (length arg-asts))
  (when (or (< n (fn-min f)) (> n (fn-max f)))
    (fp-error 'semantic (format "~a() arity: got ~a args" name n)))
  (if (fn-lazy? f)
      ((fn-proc f) subject arg-asts env ev)
      ;; each argument evaluates in its own scope frame (sibling args are independent)
      ((fn-proc f) subject (map (lambda (a) (ev a focus (env-clone-vars env))) arg-asts) env ev)))

;; ---- helpers --------------------------------------------------------------------------------
(define (sing coll) (cond [(null? coll) 'empty] [(null? (cdr coll)) (car coll)]
                          [else (fp-error 'execution "expected a singleton")]))
(define (bool-coll b) (list b))
(define (as-sys coll) (let ([x (sing coll)]) (if (eq? x 'empty) 'empty (item->system x))))
(define (truthy coll) ; strict: singleton boolean
  (define x (as-sys coll))
  (cond [(eq? x 'empty) 'empty] [(boolean? x) x] [else (fp-error 'execution "expected Boolean")]))
;; resolve one item: a Reference node or a reference string -> the contained resource(s) it points to
(define (resolve-one item e)
  (define ref (ref-of item))
  (cond [(and ref (string-prefix? ref "#")) (find-contained (substring ref 1) e)]
        [else '()]))                              ; external / bundle refs unsupported
(define (ref-of item)
  (cond
    [(fhir? item) (let ([r (navigate (list item) 'reference)])
                    (and (pair? r) (let ([v (item->system (car r))]) (and (string? v) v))))]
    [else (let ([v (item->system item)]) (and (string? v) v))]))
(define (find-contained id e)
  (define root (env-root e))
  (if (and (pair? root) (fhir? (car root)))
      (filter (lambda (r) (equal? (contained-id r) id)) (contained-of (car root)))
      '()))
(define (contained-of n) (let ([c (fhir-ref n 'contained #f)]) (cond [(list? c) c] [c (list c)] [else '()])))
(define (contained-id r)
  (and (fhir? r) (let ([i (fhir-ref r 'id #f)]) (and i (let ([v (item->system i)]) (and (string? v) v))))))

(define (require-booleans s)
  (for ([x (in-list s)])
    (unless (boolean? (item->system x)) (fp-error 'execution "expected a collection of Booleans"))))
(define (str1 coll) (let ([x (as-sys coll)])
                      (cond [(eq? x 'empty) #f] [(string? x) x]
                            [else (fp-error 'execution "expected a String")])))
(define (int1 coll) (let ([x (as-sys coll)]) (cond [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)] [else #f])))

;; a type-specifier AST -> symbol, validated to be a real type (else a semantic error)
(define (typ a)
  (define t (ast->type a))
  (unless (known-type? t) (fp-error 'semantic (format "unknown type ~a" t)))
  t)

;; a type-specifier AST -> symbol (Quantity, System.Decimal, …)
(define (ast->type a)
  (let loop ([a a])
    (cond
      [(and (fp-invoke? a) (not (fp-invoke-target a))) (fp-invoke-name a)]
      [(fp-invoke? a) (string->symbol (string-append (symbol->string (loop (fp-invoke-target a)))
                                                      "." (symbol->string (fp-invoke-name a))))]
      [else (fp-error 'semantic "expected a type specifier")])))

;; render an item to a String (toString / trace / string coercion)
(define (item->string x)
  (define s (item->system x))
  (cond
    [(string? s) s]
    [(boolean? s) (if s "true" "false")]
    [(exact-integer? s) (number->string s)]
    [(fp-long? s) (number->string (fp-long-v s))]
    [(fp-decimal? s) (decimal->string s)]
    [(moment? s) (let ([str (moment->string s)]) (substring str 1))] ; drop leading @
    [(value->moment s) => (lambda (m) (substring (moment->string m) 1))]
    [(fp-quantity? s) (quantity->string s)]
    [(fhir? x) (fp-error 'execution "cannot convert complex value to String")]
    [else (format "~a" s)]))

;; ---- the table ------------------------------------------------------------------------------
(define TABLE (make-hasheq))
(define (def! name proc #:lazy? [lazy? #f] #:min [mn 0] #:max [mx 0])
  (hash-set! TABLE name (fn proc lazy? mn mx)))
(define +inf 999)

;; existence / subsetting -----------------------------------------------------------------------
(def! 'empty     (lambda (s a e ev) (bool-coll (null? s))))
(def! 'exists    #:lazy? #t #:max 1
      (lambda (s a e ev) (bool-coll (pair? (if (null? a) s (filter-where s (car a) e ev))))))
(def! 'count     (lambda (s a e ev) (list (length s))))
(def! 'first     (lambda (s a e ev) (if (null? s) '() (list (car s)))))
(def! 'last      (lambda (s a e ev) (if (null? s) '() (list (last s)))))
(def! 'tail      (lambda (s a e ev) (if (null? s) '() (cdr s))))
(def! 'single    (lambda (s a e ev) (cond [(null? s) '()] [(null? (cdr s)) s]
                                          [else (fp-error 'execution "single() on multiple")])))
(def! 'skip      #:min 1 #:max 1 (lambda (s a e ev) (let ([n (int1 (car a))]) (if (and n (< n (length s))) (drop s (max 0 n)) (if (and n (>= n (length s))) '() s)))))
(def! 'take      #:min 1 #:max 1 (lambda (s a e ev) (let ([n (int1 (car a))]) (if n (take s (max 0 (min n (length s)))) '()))))
(def! 'distinct  (lambda (s a e ev) (dedup= s)))
(def! 'isDistinct (lambda (s a e ev) (bool-coll (= (length s) (length (dedup= s))))))
(def! 'intersect #:min 1 #:max 1 (lambda (s a e ev) (dedup= (filter (lambda (x) (member-of? x (car a))) s))))
(def! 'exclude   #:min 1 #:max 1 (lambda (s a e ev) (filter (lambda (x) (not (member-of? x (car a)))) s)))
(def! 'combine   #:min 1 #:max 1 (lambda (s a e ev) (append s (car a))))
(def! 'union     #:min 1 #:max 1 (lambda (s a e ev) (dedup= (append s (car a)))))
(def! 'subsetOf  #:min 1 #:max 1 (lambda (s a e ev) (bool-coll (for/and ([x s]) (member-of? x (car a))))))
(def! 'supersetOf #:min 1 #:max 1 (lambda (s a e ev) (bool-coll (for/and ([x (car a)]) (member-of? x s)))))
(def! 'allTrue   (lambda (s a e ev) (require-booleans s) (bool-coll (for/and ([x s]) (eq? (item->system x) #t)))))
(def! 'anyTrue   (lambda (s a e ev) (require-booleans s) (bool-coll (for/or  ([x s]) (eq? (item->system x) #t)))))
(def! 'allFalse  (lambda (s a e ev) (require-booleans s) (bool-coll (for/and ([x s]) (eq? (item->system x) #f)))))
(def! 'anyFalse  (lambda (s a e ev) (require-booleans s) (bool-coll (for/or  ([x s]) (eq? (item->system x) #f)))))

;; filtering / projection (lazy) ---------------------------------------------------------------
(def! 'where  #:lazy? #t #:min 1 #:max 1 (lambda (s a e ev) (filter-where s (car a) e ev)))
(def! 'all    #:lazy? #t #:min 1 #:max 1
      (lambda (s a e ev) (bool-coll (for/and ([x s] [i (in-naturals)])
                                      (eq? #t (as-sys (ev (car a) (list x) (proj-env e x i))))))))
(def! 'select #:lazy? #t #:min 1 #:max 1
      (lambda (s a e ev) (append-map (lambda (x i) (ev (car a) (list x) (proj-env e x i))) s (range (length s)))))
(def! 'repeat #:lazy? #t #:min 1 #:max 1 (lambda (s a e ev) (repeat-proj s (car a) e ev)))

;; boolean / iif -------------------------------------------------------------------------------
(def! 'not (lambda (s a e ev)
             (cond [(null? s) '()]
                   [(and (null? (cdr s)) (boolean? (item->system (car s)))) (list (not (item->system (car s))))]
                   [(null? (cdr s)) (list #f)]              ; a non-boolean singleton is truthy
                   [else (fp-error 'execution "not() on multiple items")])))
(def! 'iif #:lazy? #t #:min 2 #:max 3
      (lambda (s a e ev)
        (when (> (length s) 1) (fp-error 'execution "iif() requires a singleton input"))
        (define e* (env-with-this e s))                 ; criterion/branches see $this = input
        (define c (truthy (ev (car a) s e*)))
        (cond [(eq? c #t) (ev (cadr a) s e*)]
              [(>= (length a) 3) (ev (caddr a) s e*)]    ; false OR empty -> else branch
              [else '()])))

;; conversions ---------------------------------------------------------------------------------
(def! 'toString (lambda (s a e ev) (if (null? s) '() (list (item->string (sing s))))))
(def! 'toInteger (lambda (s a e ev) (conv s ->integer)))
(def! 'toDecimal (lambda (s a e ev) (conv s ->decimal)))
(def! 'toBoolean (lambda (s a e ev) (conv s ->boolean)))
(def! 'toQuantity #:max 1 (lambda (s a e ev) (conv s ->quantity)))
(def! 'convertsToInteger (lambda (s a e ev) (converts s ->integer)))
(def! 'convertsToDecimal (lambda (s a e ev) (converts s ->decimal)))
(def! 'convertsToBoolean (lambda (s a e ev) (converts s ->boolean)))
(def! 'convertsToString  (lambda (s a e ev) (converts s (lambda (x) (item->string x)))))
(def! 'convertsToQuantity #:max 1 (lambda (s a e ev) (converts s ->quantity)))
(def! 'toDate     (lambda (s a e ev) (conv s ->date)))
(def! 'toDateTime (lambda (s a e ev) (conv s ->datetime)))
(def! 'toTime     (lambda (s a e ev) (conv s ->time)))
(def! 'convertsToDate     (lambda (s a e ev) (converts s ->date)))
(def! 'convertsToDateTime (lambda (s a e ev) (converts s ->datetime)))
(def! 'convertsToTime     (lambda (s a e ev) (converts s ->time)))

;; current date/time
(def! 'today     (lambda (s a e ev) (list (today-moment))))
(def! 'now       (lambda (s a e ev) (list (now-moment))))
(def! 'timeOfDay (lambda (s a e ev) (list (timeofday-moment))))

;; strings -------------------------------------------------------------------------------------
(def! 'length (lambda (s a e ev) (let ([x (str1 s)]) (if x (list (string-length x)) '()))))
(def! 'upper  (lambda (s a e ev) (let ([x (str1 s)]) (if x (list (string-upcase x)) '()))))
(def! 'lower  (lambda (s a e ev) (let ([x (str1 s)]) (if x (list (string-downcase x)) '()))))
(def! 'startsWith #:min 1 #:max 1 (lambda (s a e ev) (str2 s a (lambda (x p) (string-prefix? x p)))))
(def! 'endsWith   #:min 1 #:max 1 (lambda (s a e ev) (str2 s a (lambda (x p) (string-suffix? x p)))))
(def! 'contains   #:min 1 #:max 1 (lambda (s a e ev) (str2 s a (lambda (x p) (string-contains? x p)))))
(def! 'indexOf    #:min 1 #:max 1 (lambda (s a e ev) (let ([x (str1 s)] [p (str1 (car a))])
                                                       (if (and x p) (list (index-of-substring x p)) '()))))
(def! 'substring  #:min 1 #:max 2 (lambda (s a e ev) (do-substring s a)))
(def! 'toChars    (lambda (s a e ev) (let ([x (str1 s)]) (if x (map string (string->list x)) '()))))
(def! 'upper      (lambda (s a e ev) (let ([x (str1 s)]) (if x (list (string-upcase x)) '()))))
(def! 'replace    #:min 2 #:max 2 (lambda (s a e ev) (let ([x (str1 s)] [p (str1 (car a))] [r (str1 (cadr a))])
                                                       (if (and x p r) (list (string-replace x p r)) '()))))
(def! 'matches    #:min 1 #:max 1 (lambda (s a e ev) (let ([x (str1 s)] [p (str1 (car a))])
                                                       (if (and x p) (list (regexp-match? (pregexp p) x)) '()))))
(def! 'replaceMatches #:min 2 #:max 2 (lambda (s a e ev) (let ([x (str1 s)] [p (str1 (car a))] [r (str1 (cadr a))])
                                                           (cond [(not (and x p r)) '()]
                                                                 [(string=? p "") (list x)]     ; empty pattern -> no-op
                                                                 [else (list (regexp-replace* (pregexp p) x r))]))))
(def! 'split  #:min 1 #:max 1 (lambda (s a e ev) (let ([x (str1 s)] [p (str1 (car a))])
                                                   (if (and x p) (string-split x p #:trim? #f) '()))))
;; join: empty input -> empty result (per spec; SQL-on-FHIR's null columns rely on it)
(def! 'join   #:max 1 (lambda (s a e ev)
                        (if (null? s) '()
                            (list (string-join (map item->string s) (if (null? a) "" (or (str1 (car a)) "")))))))
(def! 'trim   (lambda (s a e ev) (let ([x (str1 s)]) (if x (list (string-trim x)) '()))))

;; math ----------------------------------------------------------------------------------------
(def! 'abs (lambda (s a e ev)
             (if (null? s) '()
                 (let ([x (item->system (car s))])
                   (cond [(fp-quantity? x) (list (fp-quantity (dec-abs (fp-quantity-value x))
                                                              (fp-quantity-unit x) #f))]
                         [(as-decimal (car s)) => (lambda (d) (list (dec-abs d)))] [else '()])))))
(def! 'truncate (lambda (s a e ev) (num-d1 s (lambda (r) (integer->decimal (trunc r))))))
(def! 'floor    (lambda (s a e ev) (num-d1 s (lambda (r) (integer->decimal (floor r))))))
(def! 'ceiling  (lambda (s a e ev) (num-d1 s (lambda (r) (integer->decimal (ceiling r))))))
(def! 'round    #:max 1 (lambda (s a e ev) (num1 s (lambda (d) (decimal-round d (or (int1 (if (null? a) '() (car a))) 0))) values)))
(def! 'sqrt (lambda (s a e ev) (num-flo1 s (lambda (x) (and (>= x 0) (sqrt x))))))
(def! 'exp  (lambda (s a e ev) (num-flo1 s exp)))
(def! 'ln   (lambda (s a e ev) (num-flo1 s (lambda (x) (and (> x 0) (log x))))))
(def! 'log  #:min 1 #:max 1 (lambda (s a e ev) (num-flo2 s a (lambda (x b) (and (> x 0) (> b 0) (/ (log x) (log b)))))))
(def! 'power #:min 1 #:max 1 (lambda (s a e ev) (num-flo2 s a (lambda (x b) (let ([r (expt x b)]) (and (real? r) r))))))

;; tree / types / utility ----------------------------------------------------------------------
(def! 'children    (lambda (s a e ev) (append-map node-children s)))
(def! 'descendants (lambda (s a e ev) (let loop ([xs (append-map node-children s)] [acc '()])
                                        (if (null? xs) (reverse acc)
                                            (loop (append (node-children (car xs)) (cdr xs)) (cons (car xs) acc))))))
(def! 'ofType #:lazy? #t #:min 1 #:max 1 (lambda (s a e ev) (let ([t (typ (car a))]) (filter (lambda (x) (item-of-type? x t)) s))))
(def! 'is     #:lazy? #t #:min 1 #:max 1
      (lambda (s a e ev)
        (define t (ast->type (car a))) (define x (sing s))
        (cond [(eq? x 'empty) '()]
              [(known-type? t) (list (item-is? x t))]
              [else (list #f)])))    ; unresolvable type -> false (not an error)
(def! 'as     #:lazy? #t #:min 1 #:max 1
      (lambda (s a e ev)
        (when (> (length s) 1) (fp-error 'execution "as() requires a singleton input"))
        (let ([t (typ (car a))]) (filter (lambda (x) (item-of-type? x t)) s))))
(def! 'type   (lambda (s a e ev) (map (lambda (x) (let-values ([(ns nm) (type-info-of x)])
                                                    (make-typeinfo ns nm))) s)))
(def! 'trace  #:lazy? #t #:min 1 #:max 2 (lambda (s a e ev) s))  ; no-op passthrough
(def! 'hasValue (lambda (s a e ev) (bool-coll (and (pair? s) (primitive-type? (car s)) (not (value-less? (car s)))))))
(def! 'getValue (lambda (s a e ev) (if (and (pair? s) (primitive-type? (car s))) (list (item->system (car s))) s)))
(def! 'extension #:min 1 #:max 1 (lambda (s a e ev)
      (define url (str1 (car a)))
      (append-map (lambda (x) (filter (lambda (ex) (equal? (ext-url ex) url)) (get-extensions x))) s)))
;; hasExtension(url): does the input carry an extension with this url? Not in the FHIRPath spec
;; proper but used by FHIR-published SearchParameters (US Core `item-subject`) and supported by the
;; reference engines; = extension(url).exists(), empty on an empty input.
(def! 'hasExtension #:min 1 #:max 1 (lambda (s a e ev)
      (define url (str1 (car a)))
      (if (null? s) '()
          (bool-coll (for/or ([x (in-list s)])
                       (for/or ([ex (in-list (get-extensions x))]) (equal? (ext-url ex) url)))))))
(def! 'defineVariable #:lazy? #t #:min 1 #:max 2
      (lambda (s a e ev)
        (define name (str1 (ev (car a) s (env-clone-vars e))))     ; name/value args are own scopes
        (when (or (not name) (system-var? name) (env-var-bound? e name))
          (fp-error 'semantic (format "cannot define variable ~s" name)))
        (define val (if (>= (length a) 2) (ev (cadr a) s (env-clone-vars (env-with-this e s))) s))
        (env-define-var e name val)        ; mutate the shared scope; visible downstream
        s))
(define (system-var? n)
  (and (member n '("context" "resource" "rootResource" "ucum" "sct" "loinc" "this" "index" "total")) #t))
(def! 'aggregate #:lazy? #t #:min 1 #:max 2
      (lambda (s a e ev)
        (define init (if (>= (length a) 2) (ev (cadr a) s e) '()))
        (for/fold ([total init] #:result total) ([x s] [i (in-naturals)])
          (ev (car a) (list x) (env-with-total (proj-env e x i) total)))))
;; resolve() — follow References/reference-strings to contained resources (#id) in the root resource
(def! 'resolve (lambda (s a e ev) (append-map (lambda (x) (resolve-one x e)) s)))
;; %terminologies.expand/validateVS/translate — the subject is the injected terminology handle.
(define (tx-subject s who)
  (cond [(and (pair? s) (null? (cdr s)) (tx-service? (car s))) (car s)]
        [else (fp-error 'execution (format "~a() requires %terminologies" who))]))
(def! 'expand #:min 1 #:max 2
      (lambda (s a e ev) ((tx-service-expand (tx-subject s 'expand)) (str1 (car a)))))
(def! 'validateVS #:min 2 #:max 3
      (lambda (s a e ev) ((tx-service-validate (tx-subject s 'validateVS)) (str1 (car a)) (str1 (cadr a)))))
(def! 'translate #:min 2 #:max 3
      (lambda (s a e ev) ((tx-service-translate (tx-subject s 'translate)) (str1 (car a)) (str1 (cadr a)))))
(def! 'conformsTo #:min 1 #:max 1
      (lambda (s a e ev)
        (define url (str1 (car a)))
        (define tn (and url (string->symbol (last (string-split url "/")))))
        (cond [(null? s) '()]
              [(not (and tn (known-type? tn))) (fp-error 'semantic "conformsTo: unknown profile")]
              [else (list (and (for/and ([x (in-list s)]) (item-is? x tn)) #t))])))

;; R5: sort / matchesFull / precision / escape / unescape / encode / decode
(def! 'sort #:lazy? #t #:min 0 #:max 20
      (lambda (s a e ev)
        (if (null? a)
            (sort s (lambda (x y) (< (sys-cmp x y) 0)))
            (sort s (lambda (x y) (< (keys-cmp a x y e ev) 0))))))
(def! 'matchesFull #:min 1 #:max 1
      (lambda (s a e ev) (let ([x (str1 s)] [p (str1 (car a))])
                           (if (and x p) (list (regexp-match? (pregexp (string-append "^(?:" p ")$")) x)) '()))))
(def! 'precision (lambda (s a e ev)
                   (if (null? s) '()
                       (let ([v (item->system (car s))])
                         (cond [(fp-decimal? v) (list (decimal-precision v))]
                               [(value->moment v) => (lambda (m) (list (moment-precision-digits m)))]
                               [else '()])))))
(def! 'escape   #:min 1 #:max 1 (lambda (s a e ev) (str-map s (car a) escape-str)))
(def! 'unescape #:min 1 #:max 1 (lambda (s a e ev) (str-map s (car a) unescape-str)))
(def! 'encode   #:min 1 #:max 1 (lambda (s a e ev) (str-map s (car a) encode-str)))
(def! 'decode   #:min 1 #:max 1 (lambda (s a e ev) (str-map s (car a) decode-str)))
(def! 'lowBoundary  #:max 1 (lambda (s a e ev) (boundary s a #t)))
(def! 'highBoundary #:max 1 (lambda (s a e ev) (boundary s a #f)))
(def! 'comparable   #:min 1 #:max 1
      (lambda (s a e ev)
        (cond [(or (null? s) (null? (car a))) '()]
              [else (define qa (item->system (car s))) (define qb (item->system (car (car a))))
                    (list (and (fp-quantity? qa) (fp-quantity? qb)
                               (not (eq? (quantity-compare qa qb) 'empty))))])))

;; ---- shared impl helpers --------------------------------------------------------------------
;; each projection element evaluates in its own variable scope (defineVariable stays local)
(define (proj-env e x i) (env-with-item (env-clone-vars e) x i))

(define (filter-where s crit e ev)
  (for/list ([x s] [i (in-naturals)] #:when (eq? #t (as-sys (ev crit (list x) (proj-env e x i))))) x))

;; repeat(projection): accumulate the projection applied repeatedly until no new items appear.
;; The result is the projected items (NOT the seed), deduplicated, in discovery order.
(define (repeat-proj s crit e ev)
  (let loop ([current s] [seen '()] [out '()])
    (define proj (append-map (lambda (x i) (ev crit (list x) (proj-env e x i)))
                             current (range (length current))))
    (define fresh (dedup= (filter (lambda (n) (not (member-of? n seen))) proj)))  ; distinct new items
    (if (null? fresh) (reverse out)
        (loop fresh (append fresh seen) (append (reverse fresh) out)))))

(define (member-of? x coll) (for/or ([y coll]) (item=? x y)))
(define (dedup= coll) (let loop ([xs coll] [acc '()])
                        (cond [(null? xs) (reverse acc)]
                              [(member-of? (car xs) acc) (loop (cdr xs) acc)]
                              [else (loop (cdr xs) (cons (car xs) acc))])))

(define (str2 s a pred) (let ([x (str1 s)] [p (str1 (car a))]) (if (and x p) (list (pred x p)) '())))
(define (index-of-substring x p)
  (let loop ([i 0]) (cond [(> (+ i (string-length p)) (string-length x)) -1]
                          [(string=? (substring x i (+ i (string-length p))) p) i]
                          [else (loop (add1 i))])))
(define (do-substring s a)
  (define x (str1 s))
  (cond [(not x) '()]
        [else (define start (int1 (car a))) (define len (and (>= (length a) 2) (int1 (cadr a))))
              (cond [(not start) '()]
                    [(or (< start 0) (>= start (string-length x))) '()]
                    [else (define end (if len (min (string-length x) (+ start len)) (string-length x)))
                          (list (substring x start (max start end)))])]))

(define (conv s f) (if (null? s) '() (let ([r (f (sing s))]) (if (eq? r 'no) '() (list r)))))
(define (converts s f) (if (null? s) '() (list (not (eq? (f (sing s)) 'no)))))

(define (->integer x)
  (define v (item->system x))
  (cond [(exact-integer? v) v] [(fp-long? v) (fp-long-v v)]
        [(boolean? v) (if v 1 0)]
        [(string? v) (let ([n (string->number v)]) (if (exact-integer? n) n 'no))]
        [else 'no]))
(define (->decimal x)
  (define v (item->system x))
  (cond [(fp-decimal? v) v] [(exact-integer? v) (integer->decimal v)] [(fp-long? v) (integer->decimal (fp-long-v v))]
        [(boolean? v) (integer->decimal (if v 1 0))]
        [(string? v) (or (parse-decimal v) 'no)] [else 'no]))
(define (->boolean x)
  (define v (item->system x))
  (cond [(boolean? v) v]
        [(exact-integer? v) (cond [(= v 1) #t] [(= v 0) #f] [else 'no])]
        [(fp-decimal? v) (let ([r (decimal->exact v)]) (cond [(= r 1) #t] [(= r 0) #f] [else 'no]))]
        [(string? v) (cond [(member (string-downcase v) '("true" "t" "yes" "y" "1" "1.0")) #t]
                           [(member (string-downcase v) '("false" "f" "no" "n" "0" "0.0")) #f] [else 'no])]
        [else 'no]))
(define (->quantity x)
  (define v (item->system x))
  (cond [(fp-quantity? v) v]
        [(fp-decimal? v) (make-quantity v "1")]
        [(exact-integer? v) (make-quantity (integer->decimal v) "1")]
        [(boolean? v) (make-quantity (integer->decimal (if v 1 0)) "1")]
        ;; "1", "1.0", "4 'mg'" (UCUM quoted), or "1 day" (calendar word — bare word must be calendar)
        [(string? v) (let ([m (regexp-match #px"^(-?[0-9]+(?:[.][0-9]+)?)(?:\\s+(?:'([^']*)'|([A-Za-z]+)))?$" v)])
                       (cond [(not m) 'no]
                             [(caddr m) (make-quantity (parse-decimal (cadr m)) (caddr m))]        ; quoted UCUM
                             [(and (cadddr m) (calendar-unit? (cadddr m))) (make-quantity (parse-decimal (cadr m)) (cadddr m))]
                             [(cadddr m) 'no]                                                       ; bare non-calendar word invalid
                             [else (make-quantity (parse-decimal (cadr m)) "1")]))]
        [else 'no]))

(define (->date x)
  (define v (item->system x))
  (cond [(and (moment? v) (eq? (moment-kind v) 'date)) v]
        [(and (moment? v) (eq? (moment-kind v) 'dateTime))
         (moment 'date (moment-year v) (moment-month v) (moment-day v) #f #f #f #f #f 3)]
        [(string? v) (or (parse-fp-date (strip@ v)) 'no)]
        [else 'no]))
(define (->datetime x)
  (define v (item->system x))
  (cond [(and (moment? v) (memq (moment-kind v) '(date dateTime))) v]
        [(string? v) (or (parse-fp-datetime (strip@ v)) 'no)]
        [else 'no]))
(define (->time x)
  (define v (item->system x))
  (cond [(and (moment? v) (eq? (moment-kind v) 'time)) v]
        [(string? v) (or (parse-fp-time (strip@ (strip-T v))) 'no)]
        [else 'no]))
(define (strip@ s) (if (and (> (string-length s) 0) (char=? (string-ref s 0) #\@)) (substring s 1) s))
(define (strip-T s) (if (and (> (string-length s) 0) (char=? (string-ref s 0) #\T)) (substring s 1) s))

(define (p2 n) (if (< n 10) (string-append "0" (number->string n)) (number->string n)))
(define (today-moment)
  (define d (seconds->date (current-seconds) #t))
  (parse-fp-date (format "~a-~a-~a" (date-year d) (p2 (date-month d)) (p2 (date-day d)))))
(define (now-moment)
  ;; LOCAL time with the real utc offset — now()/today()/timeOfDay() must read ONE clock
  ;; (spec: "the timezone offset of the evaluation environment"); mixing local and UTC made
  ;; `now() > today()` flip answers near a UTC day boundary
  (define d (seconds->date (current-seconds) #t))
  (define tz (date-time-zone-offset d))             ; seconds east of UTC
  (define a (abs (quotient tz 60)))
  (parse-fp-datetime (format "~a-~a-~aT~a:~a:~a~a~a:~a"
                             (date-year d) (p2 (date-month d)) (p2 (date-day d))
                             (p2 (date-hour d)) (p2 (date-minute d)) (p2 (date-second d))
                             (if (< tz 0) "-" "+") (p2 (quotient a 60)) (p2 (modulo a 60)))))
(define (timeofday-moment)
  (define d (seconds->date (current-seconds) #t))
  (parse-fp-time (format "~a:~a:~a" (p2 (date-hour d)) (p2 (date-minute d)) (p2 (date-second d)))))

;; sort helpers
(define (sys-cmp x y)
  (define a (item->system x)) (define b (item->system y))
  (define da (as-decimal x)) (define db (as-decimal y))
  (cond [(and da db) (decimal-cmp da db)]
        [(and (string? a) (string? b)) (cond [(string<? a b) -1] [(string>? a b) 1] [else 0])]
        [(and (boolean? a) (boolean? b)) (cond [(eq? a b) 0] [b -1] [else 1])]
        [(and (value->moment a) (value->moment b))
         (let ([c (moment-compare (value->moment a) (value->moment b))]) (if (eq? c 'empty) 0 c))]
        [else 0]))
(define (keys-cmp keys x y e ev)
  (let loop ([ks keys])
    (cond [(null? ks) 0]
          [else
           (define k (car ks))
           (define desc? (and (fp-unop? k) (eq? (fp-unop-op k) '-)))
           (define inner (if desc? (fp-unop-operand k) k))
           (define kx (ev inner (list x) (env-with-item e x 0)))
           (define ky (ev inner (list y) (env-with-item e y 0)))
           ;; empty keys always sort first (not affected by ascending/descending)
           (cond
             [(and (null? kx) (null? ky)) (loop (cdr ks))]
             [(null? kx) -1]
             [(null? ky) 1]
             [else (define cc (sys-cmp (car kx) (car ky)))
                   (define c (if desc? (- cc) cc))
                   (if (= c 0) (loop (cdr ks)) c)])])))

(define (moment-precision-digits m0)
  (define m (value->moment m0))
  (cond
    [(not m) 0]
    [else
     (define base (case (moment-prec m)
                    [(1) 4] [(2) 6] [(3) 8]        ; year / year-month / date
                    [(4) (if (eq? (moment-kind m) 'time) 2 10)]
                    [(5) (if (eq? (moment-kind m) 'time) 4 12)]
                    [(6) (if (eq? (moment-kind m) 'time) 6 14)] [else 8]))
     (+ base (if (moment-frac m) (string-length (moment-frac m)) 0))]))

;; string escape/encode helpers
(define (str-map s argcoll f) (let ([x (str1 s)] [kind (str1 argcoll)]) (if (and x kind) (list (f x kind)) '())))
(define (escape-str x kind)
  (case kind
    [("html") (fold-replace x '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;") ("'" . "&#39;")))]
    [("json") (fold-replace x '(("\\" . "\\\\") ("\"" . "\\\"") ("\n" . "\\n") ("\r" . "\\r") ("\t" . "\\t")))]
    [else x]))
(define (unescape-str x kind)
  (case kind
    [("html") (fold-replace x '(("&quot;" . "\"") ("&#39;" . "'") ("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&")))]
    [("json") (fold-replace x '(("\\\"" . "\"") ("\\n" . "\n") ("\\r" . "\r") ("\\t" . "\t") ("\\\\" . "\\")))]
    [else x]))
(define (fold-replace x pairs) (for/fold ([s x]) ([p (in-list pairs)]) (string-replace s (car p) (cdr p))))
(define (encode-str x kind)
  (case kind
    [("base64") (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 x) #""))]
    [("urlbase64") (string-replace (string-replace (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 x) #"")) "+" "-") "/" "_")]
    [("hex") (apply string-append (for/list ([b (in-bytes (string->bytes/utf-8 x))]) (~hex b)))]
    [else x]))
(define (decode-str x kind)
  (case kind
    [("base64") (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 x)))]
    [("urlbase64") (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 (string-replace (string-replace x "-" "+") "_" "/"))))]
    [("hex") (bytes->string/utf-8 (list->bytes (for/list ([i (in-range 0 (string-length x) 2)]) (string->number (substring x i (+ i 2)) 16))))]
    [else x]))
(define (~hex b) (let ([s (number->string b 16)]) (if (= (string-length s) 1) (string-append "0" s) s)))

;; lowBoundary/highBoundary — dispatch on the item's kind (decimal, temporal, or quantity).
(define (boundary s a low?)
  (if (null? s) '()
      (let* ([item (car s)] [sv (item->system item)]
             [digits (and (pair? a) (int1 (car a)))])
        (cond
          [(value->moment sv)   ; temporals default to maximum precision (full instant)
           => (lambda (m) (let ([r (moment-boundary m (or digits 17) low?)]) (if r (list r) '())))]
          [(fp-quantity? sv)
           (let ([db (dec-boundary (fp-quantity-value sv) (or digits 8) low?)])
             (if db (list (fp-quantity db (fp-quantity-unit sv) (fp-quantity-calendar? sv))) '()))]
          [(as-decimal item) => (lambda (d) (let ([db (dec-boundary d (or digits 8) low?)])
                                              (if db (list db) '())))]
          [else '()]))))

(define (dec-boundary d prec low?)
  (and (>= prec 0) (<= prec 28)
       (let* ([scale (decimal-precision d)]
              [half (/ 1 (* 2 (expt 10 scale)))]
              [val ((if low? - +) (decimal->exact d) half)]
              [f (expt 10 prec)])
         ;; a boundary whose magnitude is below one unit at the target precision collapses to zero
         (if (< (abs val) (/ 1 f))
             (fp-decimal 0 prec)
             (fp-decimal (/ ((if low? floor ceiling) (* val f)) f) prec)))))

(define (num1 s f wrap)
  (if (null? s) '()
      (let ([d (as-decimal (sing s))]) (if d (list (wrap (f d))) '()))))
(define (dec-abs d) (if (< (decimal->exact d) 0) (neg d) d))
(define (num-d1 s f)
  (if (null? s) '() (let ([d (as-decimal (car s))]) (if d (list (f (decimal->exact d))) '()))))
(define (num-flo1 s f)
  (if (null? s) '()
      (let ([d (as-decimal (car s))])
        (if d (let ([r (f (exact->inexact (decimal->exact d)))]) (if (and r (real? r)) (list (flo->dec r)) '())) '()))))
(define (num-flo2 s a f)
  (if (or (null? s) (null? (car a))) '()
      (let ([d (as-decimal (car s))] [b (as-decimal (car (car a)))])
        (if (and d b)
            (let ([r (f (exact->inexact (decimal->exact d)) (exact->inexact (decimal->exact b)))])
              (if (and r (real? r)) (list (flo->dec r)) '())) '()))))
(define (neg d) (fp-decimal (- (decimal->exact d)) (decimal-precision d)))
(define (trunc r) (if (< r 0) (- (floor (- r))) (floor r)))
(define (flo->dec fl) (or (parse-decimal (number->string (exact->inexact fl))) (integer->decimal 0)))

;; extensions helpers (extension() / .extension)
(define (get-extensions x)
  (cond [(primitive-type? x) (element-extension x)]
        [(fhir? x) (navigate (list x) 'extension)]
        [else '()]))
(define (ext-url ex) (cond [(fhir? ex) (let ([u (navigate (list ex) 'url)]) (and (pair? u) (item->system (car u))))]
                           [(hash? ex) (hash-ref ex 'url #f)] [else #f]))

;; type() returns a typeinfo (from value.rkt) inspected as .namespace / .name
(define (make-typeinfo ns nm) (typeinfo (symbol->string ns) (symbol->string nm)))
