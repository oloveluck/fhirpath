#lang racket/base

;;; eval.rkt — the FHIRPath tree-walking evaluator (everything is a collection).
;;;
;;; Navigation is schema-driven (navigate.rkt); operators live in operators.rkt; functions in
;;; functions.rkt (receiving `fp-eval` for lazy args + per-item $this binding). and/or short-circuit
;;; so an unreached erroring branch doesn't fire.

(require racket/contract racket/match
         "error.rkt"
         "parser/ast.rkt"
         "environment.rkt"
         (only-in "navigate.rkt" navigate)
         (only-in "value.rkt" item->system fp-long)
         (only-in "operators.rkt" apply-binop apply-unary)
         (only-in "types.rkt" item-is? item-of-type? known-type?)
         (only-in "analyze.rkt" analyze)
         (only-in fhir/model/node fhir? fhir-type)
         (only-in "functions.rkt" apply-function)
         (only-in "decimal.rkt" parse-decimal integer->decimal)
         (only-in "quantity.rkt" make-quantity)
         (only-in "temporal-ops.rkt" parse-fp-date parse-fp-datetime parse-fp-time))

(provide (contract-out
          [eval-fhirpath (->* (any/c list?) (#:externals hash? #:strict? boolean?) list?)]
          [fp-eval       (-> any/c list? env? list?)]))

(define (eval-fhirpath ast root #:externals [ext (hash)] #:strict? [strict? #t])
  ;; static semantic pass, seeded with the input resource type (when the root is a single node)
  (when (and (pair? root) (null? (cdr root)) (fhir? (car root)))
    (analyze ast (fhir-type (car root)) #:strict? strict?))
  (fp-eval ast root (make-env root #:externals ext)))

(define (fp-eval node coll e)
  (match node
    [(fp-lit v) (lit->coll v)]
    [(fp-var '$this) (env-this e)]
    [(fp-var '$index) (list (env-index e))]
    [(fp-var '$total) (env-total e)]
    [(fp-var k) (fp-error 'semantic (format "unknown variable ~a" k))]
    [(fp-extern name)
     (or (env-lookup-external e name) (fp-error 'semantic (format "unknown environment variable %~a" name)))]
    [(fp-invoke #f name) (navigate coll name)]
    [(fp-invoke target name) (navigate (fp-eval target coll e) name)]
    [(fp-index target idx)
     (define t (fp-eval target coll e))
     (define iv (item->system-1 (fp-eval idx coll e)))
     (if (and (exact-integer? iv) (>= iv 0) (< iv (length t))) (list (list-ref t iv)) '())]
    [(fp-unop op operand) (apply-unary op (fp-eval operand coll e))]
    [(fp-typeop op operand type) (eval-typeop op (fp-eval operand coll e) type)]
    [(fp-binop op l r) (eval-binop op l r coll e)]
    [(fp-call target name args)
     (define subject (if target (fp-eval target coll e) coll))
     (apply-function name subject args e fp-eval coll)]
    [_ (fp-error 'semantic (format "cannot evaluate ~s" node))]))

;; and/or short-circuit; everything else is eager
(define (eval-binop op l r coll e)
  (case op
    [(and)
     (define lv (fp-eval l coll e))
     (if (equal? lv (list #f)) (list #f) (apply-binop (quote and) lv (fp-eval r coll e)))]
    [(or)
     (define lv (fp-eval l coll e))
     (if (equal? lv (list #t)) (list #t) (apply-binop (quote or) lv (fp-eval r coll e)))]
    [(union)   ; each operand gets its own variable scope (defineVariable must not leak across |)
     (apply-binop 'union (fp-eval l coll (env-clone-vars e)) (fp-eval r coll (env-clone-vars e)))]
    [else (apply-binop op (fp-eval l coll e) (fp-eval r coll e))]))

;; is/as as operators: `expr is Type` -> boolean ; `expr as Type` -> filtered
(define (eval-typeop op coll type)
  (case op
    [(is) (cond [(null? coll) '()]
                [(not (null? (cdr coll))) (fp-error 'execution "is on multiple items")]
                [(known-type? type) (list (item-is? (car coll) type))]
                [else (list #f)])]                                  ; unresolvable type -> false
    [(as) (unless (known-type? type) (fp-error 'semantic (format "unknown type ~a" type)))
          (filter (lambda (x) (item-of-type? x type)) coll)]))

(define (item->system-1 coll)
  (cond [(null? coll) 'empty] [(null? (cdr coll)) (item->system (car coll))]
        [else (fp-error 'execution "expected a singleton index")]))

;; literal -> collection
(define (lit->coll v)
  (cond
    [(eq? v 'empty) '()]
    [(exact-integer? v) (list v)]
    [(boolean? v) (list v)]
    [(string? v) (list v)]
    [(dec-lit? v) (list (parse-decimal (dec-lit-lexeme v)))]
    [(qty-lit? v) (list (make-quantity (num->dec (qty-lit-number v)) (qty-lit-unit v)))]
    [(date-lit? v) (let ([m (parse-fp-date (date-lit-str v))]) (if m (list m) '()))]
    [(datetime-lit? v) (let ([m (parse-fp-datetime (datetime-lit-str v))]) (if m (list m) '()))]
    [(time-lit? v) (let ([m (parse-fp-time (time-lit-str v))]) (if m (list m) '()))]
    [else (list v)]))

(define (num->dec n) (if (dec-lit? n) (parse-decimal (dec-lit-lexeme n)) (integer->decimal n)))
