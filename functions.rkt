#lang racket/base

;;; functions.rkt — the FHIRPath function library.
;;;
;;; A Coll is a (listof Item): the collection model of value.rkt (an Item is a typed node, a
;;; System value, or a terminology handle). Every function takes a Coll and answers a Coll.
;;;
;;; A Fn is (fn proc lazy? min max) — one entry of the catalogue, which is DATA (`TABLE`,
;;; name -> Fn, filled by `def!`):
;;;   proc  : Coll (listof Arg) Env Evaluator -> Coll
;;;   lazy? : when #f the Args are the already-evaluated argument Colls; when #t they are the raw
;;;           argument ASTs, and proc calls `ev` itself (where/select/repeat/all/iif/aggregate/
;;;           trace/defineVariable/sort and the type functions is/as/ofType — the ones that bind
;;;           $this/$index per item or read a type specifier)
;;;   min/max : the arity the call must satisfy
;;; An Evaluator is (ast Coll Env -> Coll): eval.rkt's fp-eval, passed in so this module does not
;;; depend on it.
;;;
;;; Conventions below: `s` is the subject Coll, `a` the Args, `e` the Env, `ev` the Evaluator.
;;; A function on a missing or empty input answers '(); a singleton is demanded with `sing`.

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
         (only-in "temporal-ops.rkt" value->moment moment? moment-kind moment->string parse-fp-date calendar-problem
                  parse-fp-datetime parse-fp-time moment moment-year moment-month moment-day
                  moment-compare moment-prec moment-frac moment-boundary)
         (only-in "quantity.rkt" make-quantity quantity->string calendar-unit? quantity-compare)
         (only-in "environment.rkt" env-with-item env-with-this env-with-total env-define-var
                  env-clone-vars env-var-bound? env-this env-root)
         (only-in "terminology.rkt" tx-service? tx-service-expand tx-service-validate tx-service-translate)
         (only-in "parser/ast.rkt" fp-invoke? fp-invoke-name fp-invoke-target
                  fp-unop? fp-unop-op fp-unop-operand)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in fhir/operation parameters->inputs)
         (only-in net/base64 base64-encode base64-decode))

(provide (contract-out
          [function-known? (-> symbol? boolean?)]
          [apply-function  (-> symbol? list? (listof any/c) any/c procedure? list? list?)]))

;; ---- dispatch -------------------------------------------------------------------------------
(struct fn (proc lazy? min max) #:transparent)

;; function-known? : Symbol -> Boolean   is there a function of this name?
(define (function-known? name) (and (hash-ref TABLE name #f) #t))

;; apply-function : Symbol Coll (listof ast) Env Evaluator Coll -> Coll
;; call the named function on `subject`: check the arity, then hand a lazy function the ASTs and an
;; eager one their values. `focus` is the collection the call appears in (the outer focus); eager
;; arguments are evaluated against it, not against the function's subject — so e.g.
;; combine(name.family) resolves name.family from the invocation context, and inside select() from
;; the per-item projection context.
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

;; ---- reading the subject and the arguments --------------------------------------------------
;; sing : Coll -> Item | 'empty   the one item, or an execution error on several
(define (sing coll) (cond [(null? coll) 'empty] [(null? (cdr coll)) (car coll)]
                          [else (fp-error 'execution "expected a singleton")]))
;; bool-coll : Boolean -> Coll
(define (bool-coll b) (list b))
;; as-sys : Coll -> SystemValue | 'empty   the singleton's System value
(define (as-sys coll) (let ([x (sing coll)]) (if (eq? x 'empty) 'empty (item->system x))))
;; truthy : Coll -> Boolean | 'empty   a singleton Boolean (strict: anything else is an error)
(define (truthy coll)
  (define x (as-sys coll))
  (cond [(eq? x 'empty) 'empty] [(boolean? x) x] [else (fp-error 'execution "expected Boolean")]))
;; str1 : Coll -> String | #f   a singleton String, #f when empty, an error on another type
(define (str1 coll) (let ([x (as-sys coll)])
                      (cond [(eq? x 'empty) #f] [(string? x) x]
                            [else (fp-error 'execution "expected a String")])))
;; int1 : Coll -> Integer | #f   a singleton Integer (or Long), #f otherwise
(define (int1 coll) (let ([x (as-sys coll)]) (cond [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)] [else #f])))
;; string-of : Item | #f -> String | #f   the item's System value when it is a String
(define (string-of item) (and item (let ([v (item->system item)]) (and (string? v) v))))
;; require-booleans : Coll -> Void   an execution error unless every item is a Boolean
(define (require-booleans s)
  (for ([x (in-list s)])
    (unless (boolean? (item->system x)) (fp-error 'execution "expected a collection of Booleans"))))

;; ---- resolve() ------------------------------------------------------------------------------
;; resolve-one : Item Env -> Coll
;; a Reference node or a reference string -> the resource(s) it points to: `#id` in the root's
;; contained resources, anything else among the entries of a root Bundle
(define (resolve-one item e)
  (define ref (ref-of item))
  (cond [(not ref) '()]
        [(string-prefix? ref "#") (find-contained (substring ref 1) e)]
        [else (find-in-bundle ref e)]))
;; ref-of : Item -> String | #f   a Reference's `reference`, or the item itself when it is a string
(define (ref-of item)
  (if (fhir? item)
      (let ([r (navigate (list item) 'reference)]) (and (pair? r) (string-of (car r))))
      (string-of item)))
;; root-resource : Env -> Node | #f   the root collection's single resource
(define (root-resource e)
  (define root (env-root e))
  (and (pair? root) (fhir? (car root)) (car root)))
;; ref-list : Node Symbol -> (listof Item)   a field as a list, whether absent, one or many
(define (ref-list n key)
  (define x (fhir-ref n key #f))
  (cond [(list? x) x] [x (list x)] [else '()]))
;; find-in-bundle : String Env -> Coll
;; the entries of the root Bundle matching the reference: by fullUrl, by the entry resource's
;; Type/id (a relative reference), or by an absolute reference ending in that Type/id
(define (find-in-bundle ref e)
  (define root (root-resource e))
  (if (and root (eq? (fhir-type root) 'Bundle))
      (for/list ([en (in-list (ref-list root 'entry))]
                 #:when (entry-matches? en ref))
        (fhir-ref en 'resource #f))
      '()))
;; entry-matches? : Node String -> Boolean
(define (entry-matches? en ref)
  (define full (string-of (fhir-ref en 'fullUrl #f)))
  (define res (fhir-ref en 'resource #f))
  (define tid (and (fhir? res) (contained-id res) (format "~a/~a" (fhir-type res) (contained-id res))))
  (or (and full (equal? full ref))
      (and tid (or (equal? tid ref) (string-suffix? ref (string-append "/" tid))))
      #f))
;; find-contained : String Env -> Coll   the root resource's contained resources with this id
(define (find-contained id e)
  (define root (root-resource e))
  (if root
      (filter (lambda (r) (equal? (contained-id r) id)) (ref-list root 'contained))
      '()))
;; contained-id : Item -> String | #f   a resource's id
(define (contained-id r) (and (fhir? r) (string-of (fhir-ref r 'id #f))))

;; ---- type specifiers and rendering ----------------------------------------------------------
;; typ : ast -> Symbol   a type-specifier AST, validated to be a real type (else a semantic error)
(define (typ a)
  (define t (ast->type a))
  (unless (known-type? t) (fp-error 'semantic (format "unknown type ~a" t)))
  t)

;; ast->type : ast -> Symbol   a type-specifier AST (Quantity, System.Decimal, …)
(define (ast->type a)
  (let loop ([a a])
    (cond
      [(and (fp-invoke? a) (not (fp-invoke-target a))) (fp-invoke-name a)]
      [(fp-invoke? a) (string->symbol (string-append (symbol->string (loop (fp-invoke-target a)))
                                                      "." (symbol->string (fp-invoke-name a))))]
      [else (fp-error 'semantic "expected a type specifier")])))

;; item->string : Item -> String   render an item (toString / join / string coercion)
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

;; ---- the catalogue --------------------------------------------------------------------------
;; TABLE : (hash Symbol -> Fn)
(define TABLE (make-hasheq))
;; def! : Symbol Proc [#:lazy? Boolean] [#:min Nat] [#:max Nat] -> Void   register one function
(define (def! name proc #:lazy? [lazy? #f] #:min [mn 0] #:max [mx 0])
  (hash-set! TABLE name (fn proc lazy? mn mx)))

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
(def! 'skip      #:min 1 #:max 1 (lambda (s a e ev) (let ([n (int1 (car a))])
                                                       (cond [(not n) s]                ; a non-integer count: unchanged
                                                             [(< n (length s)) (drop s (max 0 n))]
                                                             [else '()]))))
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
(def! 'length (lambda (s a e ev) (str-op s string-length)))
(def! 'upper  (lambda (s a e ev) (str-op s string-upcase)))
(def! 'lower  (lambda (s a e ev) (str-op s string-downcase)))
(def! 'trim   (lambda (s a e ev) (str-op s string-trim)))
(def! 'toChars    (lambda (s a e ev) (let ([x (str1 s)]) (if x (map string (string->list x)) '()))))
(def! 'startsWith #:min 1 #:max 1 (lambda (s a e ev) (str2 s a string-prefix?)))
(def! 'endsWith   #:min 1 #:max 1 (lambda (s a e ev) (str2 s a string-suffix?)))
(def! 'contains   #:min 1 #:max 1 (lambda (s a e ev) (str2 s a string-contains?)))
(def! 'indexOf    #:min 1 #:max 1 (lambda (s a e ev) (str2 s a index-of-substring)))
(def! 'matches    #:min 1 #:max 1 (lambda (s a e ev) (str2 s a (lambda (x p) (regexp-match? (pregexp p) x)))))
(def! 'matchesFull #:min 1 #:max 1
      (lambda (s a e ev) (str2 s a (lambda (x p) (regexp-match? (pregexp (string-append "^(?:" p ")$")) x)))))
(def! 'substring  #:min 1 #:max 2 (lambda (s a e ev) (do-substring s a)))
(def! 'replace    #:min 2 #:max 2 (lambda (s a e ev) (str3 s a string-replace)))
(def! 'replaceMatches #:min 2 #:max 2
      (lambda (s a e ev) (str3 s a (lambda (x p r) (if (string=? p "") x (regexp-replace* (pregexp p) x r))))))
(def! 'split  #:min 1 #:max 1 (lambda (s a e ev) (let ([x (str1 s)] [p (str1 (car a))])
                                                   (if (and x p) (string-split x p #:trim? #f) '()))))
;; join: empty input -> empty result (per spec; SQL-on-FHIR's null columns rely on it)
(def! 'join   #:max 1 (lambda (s a e ev)
                        (if (null? s) '()
                            (list (string-join (map item->string s) (if (null? a) "" (or (str1 (car a)) "")))))))
(def! 'escape   #:min 1 #:max 1 (lambda (s a e ev) (str2 s a escape-str)))
(def! 'unescape #:min 1 #:max 1 (lambda (s a e ev) (str2 s a unescape-str)))
(def! 'encode   #:min 1 #:max 1 (lambda (s a e ev) (str2 s a encode-str)))
(def! 'decode   #:min 1 #:max 1 (lambda (s a e ev) (str2 s a decode-str)))

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
(def! 'round    #:max 1 (lambda (s a e ev) (num1 s (lambda (d) (decimal-round d (or (int1 (if (null? a) '() (car a))) 0))))))
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
;; system-var? : String -> Boolean   a name defineVariable may not take
(define (system-var? n)
  (and (member n '("context" "resource" "rootResource" "ucum" "sct" "loinc" "this" "index" "total")) #t))
(def! 'aggregate #:lazy? #t #:min 1 #:max 2
      (lambda (s a e ev)
        (define init (if (>= (length a) 2) (ev (cadr a) s e) '()))
        (for/fold ([total init] #:result total) ([x s] [i (in-naturals)])
          (ev (car a) (list x) (env-with-total (proj-env e x i) total)))))
;; resolve() — follow References/reference-strings to contained resources (#id) in the root resource
(def! 'resolve (lambda (s a e ev) (append-map (lambda (x) (resolve-one x e)) s)))
;; ---- terminology ----------------------------------------------------------------------------
;; %terminologies.expand/validateVS/translate — the subject is the injected terminology handle.
;; tx-subject : Coll Symbol -> tx-service
(define (tx-subject s who)
  (cond [(and (pair? s) (null? (cdr s)) (tx-service? (car s))) (car s)]
        [else (fp-error 'execution (format "~a() requires %terminologies" who))]))
;; tx-params : Coll -> Hash   a Parameters resource argument -> name -> value (or values)
(define (tx-params coll)
  (define it (and (pair? coll) (null? (cdr coll)) (car coll)))
  (cond [(and (fhir? it) (eq? (fhir-type it) 'Parameters))
         (for/hash ([(k vs) (in-hash (parameters->inputs (node->jsexpr it)))])
           (values k (if (and (pair? vs) (null? (cdr vs))) (car vs) vs)))]
        [else (hash)]))
;; tx-coded : Coll -> (values String|#f Jsexpr|#f Jsexpr|#f)
;; a coded argument as (code coding codeable-concept): a Coding, a CodeableConcept, or a code string
(define (tx-coded coll)
  (define it (and (pair? coll) (null? (cdr coll)) (car coll)))
  (cond [(and (fhir? it) (eq? (fhir-type it) 'Coding)) (values #f (node->jsexpr it) #f)]
        [(and (fhir? it) (eq? (fhir-type it) 'CodeableConcept)) (values #f #f (node->jsexpr it))]
        [else (values (str1 coll) #f #f)]))
(def! 'expand #:min 1 #:max 2
      (lambda (s a e ev)
        ((tx-service-expand (tx-subject s 'expand)) (str1 (car a)) #f
                                                   (if (pair? (cdr a)) (tx-params (cadr a)) (hash)))))
(def! 'validateVS #:min 2 #:max 3
      (lambda (s a e ev)
        (define-values (code coding cc) (tx-coded (cadr a)))
        (define system (and (pair? (cddr a)) (str1 (caddr a))))
        ((tx-service-validate (tx-subject s 'validateVS)) (str1 (car a)) code system
                                                         #:coding coding #:codeable-concept cc)))
(def! 'translate #:min 2 #:max 3
      (lambda (s a e ev)
        (define-values (code coding cc) (tx-coded (cadr a)))
        (define first-coding (or coding (and cc (let ([cs (hash-ref cc 'coding '())]) (and (pair? cs) (car cs))))))
        (define system (or (and (pair? (cddr a)) (str1 (caddr a))) (and first-coding (hash-ref first-coding 'system #f))))
        ((tx-service-translate (tx-subject s 'translate)) (str1 (car a))
                                                          (or code (and first-coding (hash-ref first-coding 'code #f)))
                                                          system)))
(def! 'conformsTo #:min 1 #:max 1
      (lambda (s a e ev)
        (define url (str1 (car a)))
        (define tn (and url (string->symbol (last (string-split url "/")))))
        (cond [(null? s) '()]
              [(not (and tn (known-type? tn))) (fp-error 'semantic "conformsTo: unknown profile")]
              [else (list (and (for/and ([x (in-list s)]) (item-is? x tn)) #t))])))

;; R5: sort / precision / boundaries / comparable ----------------------------------------------
(def! 'sort #:lazy? #t #:min 0 #:max 20
      (lambda (s a e ev)
        (if (null? a)
            (sort s (lambda (x y) (< (sys-cmp x y) 0)))
            (sort s (lambda (x y) (< (keys-cmp a x y e ev) 0))))))
(def! 'precision (lambda (s a e ev)
                   (if (null? s) '()
                       (let ([v (item->system (car s))])
                         (cond [(fp-decimal? v) (list (decimal-precision v))]
                               [(value->moment v) => (lambda (m) (list (moment-precision-digits m)))]
                               [else '()])))))
(def! 'lowBoundary  #:max 1 (lambda (s a e ev) (boundary s a #t)))
(def! 'highBoundary #:max 1 (lambda (s a e ev) (boundary s a #f)))
(def! 'comparable   #:min 1 #:max 1
      (lambda (s a e ev)
        (cond [(or (null? s) (null? (car a))) '()]
              [else (define qa (item->system (car s))) (define qb (item->system (car (car a))))
                    (list (and (fp-quantity? qa) (fp-quantity? qb)
                               (not (eq? (quantity-compare qa qb) 'empty))))])))

;; ---- projection -----------------------------------------------------------------------------
;; proj-env : Env Item Nat -> Env
;; the env of one projected item: $this/$index bound, and its own variable scope (a
;; defineVariable inside the projection stays local)
(define (proj-env e x i) (env-with-item (env-clone-vars e) x i))

;; filter-where : Coll ast Env Evaluator -> Coll   the items whose criterion is true
(define (filter-where s crit e ev)
  (for/list ([x s] [i (in-naturals)] #:when (eq? #t (as-sys (ev crit (list x) (proj-env e x i))))) x))

;; repeat-proj : Coll ast Env Evaluator -> Coll
;; repeat(projection): apply the projection again and again until no new items appear. The result
;; is the projected items (NOT the seed), deduplicated, in discovery order.
(define (repeat-proj s crit e ev)
  (let loop ([current s] [seen '()] [out '()])
    (define proj (append-map (lambda (x i) (ev crit (list x) (proj-env e x i)))
                             current (range (length current))))
    (define fresh (dedup= (filter (lambda (n) (not (member-of? n seen))) proj)))  ; distinct new items
    (if (null? fresh) (reverse out)
        (loop fresh (append fresh seen) (append (reverse fresh) out)))))

;; member-of? : Item Coll -> Boolean   is an equal (=) item in the collection?
(define (member-of? x coll) (for/or ([y coll]) (item=? x y)))
;; dedup= : Coll -> Coll   the distinct items under =, first occurrences in order
(define (dedup= coll) (let loop ([xs coll] [acc '()])
                        (cond [(null? xs) (reverse acc)]
                              [(member-of? (car xs) acc) (loop (cdr xs) acc)]
                              [else (loop (cdr xs) (cons (car xs) acc))])))

;; ---- strings --------------------------------------------------------------------------------
;; str-op : Coll (String -> X) -> Coll   f over the subject string; empty when there is none
(define (str-op s f) (let ([x (str1 s)]) (if x (list (f x)) '())))
;; str2 : Coll Args (String String -> X) -> Coll   f over the subject and one string argument
(define (str2 s a f) (let ([x (str1 s)] [p (str1 (car a))]) (if (and x p) (list (f x p)) '())))
;; str3 : Coll Args (String String String -> X) -> Coll   f over the subject and two string arguments
(define (str3 s a f)
  (let ([x (str1 s)] [p (str1 (car a))] [r (str1 (cadr a))]) (if (and x p r) (list (f x p r)) '())))
;; index-of-substring : String String -> Integer   the first position of p in x, -1 when absent
(define (index-of-substring x p)
  (let loop ([i 0]) (cond [(> (+ i (string-length p)) (string-length x)) -1]
                          [(string=? (substring x i (+ i (string-length p))) p) i]
                          [else (loop (add1 i))])))
;; do-substring : Coll Args -> Coll   substring(start [, length]); empty when start is outside
(define (do-substring s a)
  (define x (str1 s))
  (cond [(not x) '()]
        [else (define start (int1 (car a))) (define len (and (>= (length a) 2) (int1 (cadr a))))
              (cond [(not start) '()]
                    [(or (< start 0) (>= start (string-length x))) '()]
                    [else (define end (if len (min (string-length x) (+ start len)) (string-length x)))
                          (list (substring x start (max start end)))])]))

;; ---- conversions ----------------------------------------------------------------------------
;; A Converter is (Item -> SystemValue | 'no): the converted value, or 'no when the item does not
;; convert. conv wraps one as toX(), converts as convertsToX().
;; conv : Coll Converter -> Coll
(define (conv s f) (if (null? s) '() (let ([r (f (sing s))]) (if (eq? r 'no) '() (list r)))))
;; converts : Coll Converter -> Coll
(define (converts s f) (if (null? s) '() (list (not (eq? (f (sing s)) 'no)))))

;; ->integer : Converter

(define (->integer x)
  (define v (item->system x))
  (cond [(exact-integer? v) v] [(fp-long? v) (fp-long-v v)]
        [(boolean? v) (if v 1 0)]
        [(string? v) (let ([n (string->number v)]) (if (exact-integer? n) n 'no))]
        [else 'no]))
;; ->decimal : Converter
(define (->decimal x)
  (define v (item->system x))
  (cond [(fp-decimal? v) v] [(exact-integer? v) (integer->decimal v)] [(fp-long? v) (integer->decimal (fp-long-v v))]
        [(boolean? v) (integer->decimal (if v 1 0))]
        [(string? v) (or (parse-decimal v) 'no)] [else 'no]))
;; ->boolean : Converter
(define (->boolean x)
  (define v (item->system x))
  (cond [(boolean? v) v]
        [(exact-integer? v) (cond [(= v 1) #t] [(= v 0) #f] [else 'no])]
        [(fp-decimal? v) (let ([r (decimal->exact v)]) (cond [(= r 1) #t] [(= r 0) #f] [else 'no]))]
        [(string? v) (cond [(member (string-downcase v) '("true" "t" "yes" "y" "1" "1.0")) #t]
                           [(member (string-downcase v) '("false" "f" "no" "n" "0" "0.0")) #f] [else 'no])]
        [else 'no]))
;; ->quantity : Converter
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

;; ->date : Converter   a DateTime truncates to its date
(define (->date x)
  (define v (item->system x))
  (cond [(and (moment? v) (eq? (moment-kind v) 'date)) v]
        [(and (moment? v) (eq? (moment-kind v) 'dateTime))
         (moment 'date (moment-year v) (moment-month v) (moment-day v) #f #f #f #f #f 3)]
        [(string? v) (let ([m (parse-fp-date (strip@ v))]) (if (and m (not (calendar-problem m))) m 'no))]
        [else 'no]))
;; ->datetime : Converter
(define (->datetime x)
  (define v (item->system x))
  (cond [(and (moment? v) (memq (moment-kind v) '(date dateTime))) v]
        [(string? v) (let ([m (parse-fp-datetime (strip@ v))]) (if (and m (not (calendar-problem m))) m 'no))]
        [else 'no]))
;; ->time : Converter
(define (->time x)
  (define v (item->system x))
  (cond [(and (moment? v) (eq? (moment-kind v) 'time)) v]
        [(string? v) (or (parse-fp-time (strip@ (strip-T v))) 'no)]
        [else 'no]))
;; strip@ / strip-T : String -> String   a literal's leading `@` / `T`, dropped
(define (strip@ s) (if (and (> (string-length s) 0) (char=? (string-ref s 0) #\@)) (substring s 1) s))
(define (strip-T s) (if (and (> (string-length s) 0) (char=? (string-ref s 0) #\T)) (substring s 1) s))

;; ---- the clock ------------------------------------------------------------------------------
;; p2 : Nat -> String   two digits
(define (p2 n) (if (< n 10) (string-append "0" (number->string n)) (number->string n)))
;; today-moment / now-moment / timeofday-moment : -> Moment
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

;; ---- sort -----------------------------------------------------------------------------------
;; sys-cmp : Item Item -> -1 | 0 | 1   the natural order of two items of one kind; 0 across kinds
(define (sys-cmp x y)
  (define a (item->system x)) (define b (item->system y))
  (define da (as-decimal x)) (define db (as-decimal y))
  (cond [(and da db) (decimal-cmp da db)]
        [(and (string? a) (string? b)) (cond [(string<? a b) -1] [(string>? a b) 1] [else 0])]
        [(and (boolean? a) (boolean? b)) (cond [(eq? a b) 0] [b -1] [else 1])]
        [(and (value->moment a) (value->moment b))
         (let ([c (moment-compare (value->moment a) (value->moment b))]) (if (eq? c 'empty) 0 c))]
        [else 0]))
;; keys-cmp : (listof ast) Item Item Env Evaluator -> -1 | 0 | 1
;; the order of x and y under the sort keys in turn (`-key` descends; an empty key sorts first)
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

;; moment-precision-digits : Moment -> Nat   precision(): the digits a temporal carries
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

;; ---- escape / encode ------------------------------------------------------------------------
;; escape-str / unescape-str : String String -> String   for the "html" and "json" targets
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
;; fold-replace : String (listof (cons String String)) -> String   each pair replaced in turn
(define (fold-replace x pairs) (for/fold ([s x]) ([p (in-list pairs)]) (string-replace s (car p) (cdr p))))
;; encode-str / decode-str : String String -> String   for "base64", "urlbase64" and "hex"
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

;; ---- numbers --------------------------------------------------------------------------------
;; boundary : Coll Args Boolean -> Coll
;; lowBoundary/highBoundary — dispatch on the item's kind (temporal, quantity, or decimal)
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

;; dec-boundary : Decimal Nat Boolean -> Decimal | #f   the boundary at `prec` digits
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

;; num1 : Coll (Decimal -> X) -> Coll   f over the singleton as a Decimal
(define (num1 s f)
  (if (null? s) '()
      (let ([d (as-decimal (sing s))]) (if d (list (f d)) '()))))
;; dec-abs : Decimal -> Decimal
(define (dec-abs d) (if (< (decimal->exact d) 0) (neg d) d))
;; num-d1 : Coll (Rational -> X) -> Coll   f over the first item's exact value
(define (num-d1 s f)
  (if (null? s) '() (let ([d (as-decimal (car s))]) (if d (list (f (decimal->exact d))) '()))))
;; num-flo1 : Coll (Flonum -> Real | #f) -> Coll   f in floating point, back to a Decimal
(define (num-flo1 s f)
  (if (null? s) '()
      (let ([d (as-decimal (car s))])
        (if d (let ([r (f (exact->inexact (decimal->exact d)))]) (if (and r (real? r)) (list (flo->dec r)) '())) '()))))
;; num-flo2 : Coll Args (Flonum Flonum -> Real | #f) -> Coll   the two-argument form
(define (num-flo2 s a f)
  (if (or (null? s) (null? (car a))) '()
      (let ([d (as-decimal (car s))] [b (as-decimal (car (car a)))])
        (if (and d b)
            (let ([r (f (exact->inexact (decimal->exact d)) (exact->inexact (decimal->exact b)))])
              (if (and r (real? r)) (list (flo->dec r)) '())) '()))))
;; neg : Decimal -> Decimal   negated, same scale
(define (neg d) (fp-decimal (- (decimal->exact d)) (decimal-precision d)))
;; trunc : Rational -> Integer   toward zero
(define (trunc r) (if (< r 0) (- (floor (- r))) (floor r)))
;; flo->dec : Real -> Decimal
(define (flo->dec fl) (or (parse-decimal (number->string (exact->inexact fl))) (integer->decimal 0)))

;; ---- extensions and types -------------------------------------------------------------------
;; get-extensions : Item -> Coll   the extensions of a primitive or a complex node
(define (get-extensions x)
  (cond [(primitive-type? x) (element-extension x)]
        [(fhir? x) (navigate (list x) 'extension)]
        [else '()]))
;; ext-url : Item -> String | #f
(define (ext-url ex) (cond [(fhir? ex) (let ([u (navigate (list ex) 'url)]) (and (pair? u) (item->system (car u))))]
                           [(hash? ex) (hash-ref ex 'url #f)] [else #f]))

;; make-typeinfo : Symbol Symbol -> typeinfo   what type() answers, inspected as .namespace / .name
(define (make-typeinfo ns nm) (typeinfo (symbol->string ns) (symbol->string nm)))

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit
           (only-in "environment.rkt" make-env)
           (only-in "parser/ast.rkt" fp-lit fp-lit-value fp-invoke)
           (only-in "decimal.rkt" parse-decimal decimal->string)
           (only-in "temporal-ops.rkt" moment->string)
           (only-in "value.rkt" fp-quantity-unit fp-quantity-value))
  ;; call : Symbol Coll Any ... -> Coll   an eager or lazy call whose arguments are literals
  (define (lit-ev ast focus env) (list (fp-lit-value ast)))
  (define (call name subject . args)
    (apply-function name subject (map fp-lit args) (make-env subject) lit-ev subject))

  ;; dispatch: the catalogue, arity, and the error kinds
  (check-true (function-known? 'where))
  (check-false (function-known? 'nope))
  (check-exn #rx"unknown function" (lambda () (call 'nope '())))
  (check-exn #rx"arity" (lambda () (call 'startsWith '("a"))))
  (check-exn #rx"expected a singleton" (lambda () (call 'upper '("a" "b"))))

  ;; existence / subsetting
  (check-equal? (call 'empty '()) '(#t))
  (check-equal? (call 'count '(1 2 3)) '(3))
  (check-equal? (call 'first '(1 2)) '(1))
  (check-equal? (call 'last '()) '())
  (check-equal? (call 'tail '(1 2 3)) '(2 3))
  (check-equal? (call 'skip '(1 2 3) 1) '(2 3))
  (check-equal? (call 'skip '(1 2 3) 5) '())
  (check-equal? (call 'skip '(1 2 3) "x") '(1 2 3))      ; a non-integer count leaves the input
  (check-equal? (call 'take '(1 2 3) 2) '(1 2))
  (check-equal? (call 'distinct '(1 2 1)) '(1 2))
  (check-equal? (call 'isDistinct '(1 2 1)) '(#f))
  (check-equal? (call 'union '(1 2) 2) '(1 2))
  (check-equal? (call 'combine '(1 2) 2) '(1 2 2))
  (check-equal? (call 'allTrue '(#t #t)) '(#t))
  (check-exn #rx"Booleans" (lambda () (call 'anyTrue '(1))))

  ;; conversions: the Converters and their toX / convertsToX wrappers
  (check-equal? (->integer "12") 12)
  (check-equal? (->integer "1.5") 'no)
  (check-equal? (->boolean "yes") #t)
  (check-equal? (->boolean "0.0") #f)
  (check-equal? (->boolean 2) 'no)
  (check-equal? (decimal->string (->decimal "1.50")) "1.50")
  (check-equal? (fp-quantity-unit (->quantity "4 'mg'")) "mg")
  (check-equal? (fp-quantity-unit (->quantity "1 day")) "day")
  (check-equal? (->quantity "1 bogus") 'no)
  (check-equal? (moment->string (->date "2024-03")) "@2024-03")
  (check-equal? (moment->string (->date (->datetime "2024-03-05T10:00"))) "@2024-03-05")
  (check-equal? (moment->string (->time "T10:30")) "@T10:30")
  (check-equal? (call 'toInteger '("7")) '(7))
  (check-equal? (call 'toInteger '("x")) '())
  (check-equal? (call 'convertsToInteger '("x")) '(#f))
  (check-equal? (call 'toString '(#t)) '("true"))

  ;; strings
  (check-equal? (call 'upper '("ab")) '("AB"))
  (check-equal? (call 'length '()) '())
  (check-equal? (call 'startsWith '("abc") "ab") '(#t))
  (check-equal? (call 'indexOf '("abcab") "ca") '(2))
  (check-equal? (call 'indexOf '("abc") "z") '(-1))
  (check-equal? (call 'substring '("abcdef") 2 2) '("cd"))
  (check-equal? (call 'substring '("abc") 5) '())
  (check-equal? (call 'replace '("a.b.c") "." "-") '("a-b-c"))
  (check-equal? (call 'replaceMatches '("a1b22") "[0-9]+" "#") '("a#b#"))
  (check-equal? (call 'replaceMatches '("abc") "" "#") '("abc"))   ; an empty pattern is a no-op
  (check-equal? (call 'matchesFull '("abc") "ab") '(#f))
  (check-equal? (call 'matches '("abc") "ab") '(#t))
  (check-equal? (call 'split '("a,b,,c") ",") '("a" "b" "" "c"))
  (check-equal? (call 'join '("a" "b") "-") '("a-b"))
  (check-equal? (call 'join '()) '())
  (check-equal? (call 'toChars '("ab")) '("a" "b"))
  (check-equal? (call 'escape '("<a&b>") "html") '("&lt;a&amp;b&gt;"))
  (for ([kind '("html" "json")] [x '("<'\"&>" "a\"\\\n")])
    (check-equal? (unescape-str (escape-str x kind) kind) x kind))
  (for ([kind '("base64" "urlbase64" "hex")])
    (check-equal? (decode-str (encode-str "hello, world?/+" kind) kind) "hello, world?/+" kind))
  (check-equal? (call 'encode '("hi") "hex") '("6869"))

  ;; math
  (check-equal? (map decimal->string (call 'abs (list (parse-decimal "-1.50")))) '("1.50"))
  (check-equal? (map decimal->string (call 'round (list (parse-decimal "2.345")) 2)) '("2.35"))
  (check-equal? (map decimal->string (call 'truncate (list (parse-decimal "-2.7")))) '("-2"))
  (check-equal? (map decimal->string (call 'sqrt '(16))) '("4.0"))     ; through floating point
  (check-equal? (call 'sqrt '(-1)) '())
  (check-equal? (map decimal->string (call 'log '(8) 2)) '("3.0"))
  (check-equal? (decimal->string (dec-boundary (parse-decimal "1.5") 2 #t)) "1.45")
  (check-equal? (decimal->string (dec-boundary (parse-decimal "1.5") 2 #f)) "1.55")
  (check-false (dec-boundary (parse-decimal "1") 29 #t))
  (check-equal? (map decimal->string (call 'lowBoundary (list (parse-decimal "1.5")) 2)) '("1.45"))

  ;; sort, precision
  (check-equal? (call 'sort '(3 1 2)) '(1 2 3))
  (check-equal? (call 'sort '("b" "a")) '("a" "b"))
  (check-equal? (call 'precision (list (parse-decimal "1.250"))) '(3))
  (check-equal? (moment-precision-digits (->date "2024-03")) 6)
  (check-equal? (moment-precision-digits (->time "10:30:15.25")) 8)

  ;; boolean, iif (lazy: the literal evaluator stands in for fp-eval)
  (check-equal? (call 'not '(#f)) '(#t))
  (check-equal? (call 'iif '(1) #t "yes" "no") '("yes"))
  (check-equal? (call 'iif '(1) #f "yes") '())
  (check-exn #rx"singleton" (lambda () (call 'iif '(1 2) #t "yes")))

  ;; type specifiers
  (check-equal? (ast->type (fp-invoke #f 'Quantity)) 'Quantity)
  (check-equal? (ast->type (fp-invoke (fp-invoke #f 'System) 'Decimal)) 'System.Decimal)
  (check-exn #rx"type specifier" (lambda () (ast->type (fp-lit 1))))

  ;; the clock reads one zone: today() is now()'s date
  (check-equal? (moment->string (->date (now-moment))) (moment->string (today-moment))))
