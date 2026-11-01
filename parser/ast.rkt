#lang racket/base

;;; ast.rkt — the FHIRPath AST + lowering from brag's parse-tree datum.
;;;
;;; AST structs are #:prefab so they survive the reader->expander boundary as literal data.
;;; `lower` collapses the precedence cascade (chains of single-child wrappers) into compact nodes
;;; and left-folds operator chains into fp-binop. Literal precision is preserved: a decimal keeps
;;; its lexeme (dec-lit), a quantity keeps number + unit (qty-lit), temporals keep their lexeme.

(require racket/match
         racket/contract
         (only-in racket/list add-between))

(provide (struct-out fp-lit)
         (struct-out fp-invoke)
         (struct-out fp-call)
         (struct-out fp-index)
         (struct-out fp-binop)
         (struct-out fp-unop)
         (struct-out fp-typeop)
         (struct-out fp-var)
         (struct-out fp-extern)
         (struct-out dec-lit)
         (struct-out qty-lit)
         (struct-out date-lit)
         (struct-out datetime-lit)
         (struct-out time-lit)
         (contract-out
          [lower         (-> any/c any/c)]
          [lower-program (-> any/c list?)]))

;; AST -----------------------------------------------------------------
(struct fp-lit    (value)            #:prefab) ; int | dec-lit | string | bool | 'empty | *-lit | qty-lit
(struct fp-invoke (target name)      #:prefab) ; target.name ; target=#f means "from focus" ; name symbol
(struct fp-call   (target name args) #:prefab) ; target.name(args…)
(struct fp-index  (target idx)       #:prefab)
(struct fp-binop  (op lhs rhs)       #:prefab) ; op symbol
(struct fp-unop   (op operand)       #:prefab)
(struct fp-typeop (op operand type)  #:prefab) ; op 'is|'as ; type symbol (qualified via ".")
(struct fp-var    (kind)             #:prefab) ; '$this '$index '$total
(struct fp-extern (name)             #:prefab) ; %name (string)

;; literal tags
(struct dec-lit      (lexeme) #:prefab)  ; a decimal literal, precision preserved
(struct qty-lit      (number unit) #:prefab) ; number: exact-int|dec-lit ; unit: string
(struct date-lit     (str) #:prefab)
(struct datetime-lit (str) #:prefab)
(struct time-lit     (str) #:prefab)

;; operator string -> symbol
(define (op->sym s)
  (cond [(symbol? s) s]
        [(equal? s "|") 'union]
        [else (string->symbol s)]))

(define (id->sym x) (if (symbol? x) x (string->symbol x)))

;; -------------------------------------------------------------------
(define (lower-program datum)
  (match datum
    [(list 'fp-program exprs ...) (map lower exprs)]
    [_ (list (lower datum))]))

(define (lower datum)
  (match datum
    [(list 'fp-program e) (lower e)]
    [(list 'fp-expr e) (lower e)]

    ;; binary levels: operand (op operand)* — left associative
    [(list (or 'implies-expr 'or-expr 'and-expr 'membership-expr 'equality-expr
               'inequality-expr 'union-expr 'additive-expr 'multiplicative-expr)
           first rest ...)
     (fold-binops (lower first) rest)]

    ;; type test: additive (op typespec)*
    [(list 'type-expr operand) (lower operand)]
    [(list 'type-expr operand rest ...) (fold-typeops (lower operand) rest)]

    ;; unary: (op? postfix)
    [(list 'unary-expr operand) (lower operand)]
    [(list 'unary-expr (? string? op) operand) (fp-unop (op->sym op) (lower operand))]

    ;; postfix: primary tail*
    [(list 'postfix-expr prim tails ...)
     (for/fold ([acc (lower prim)]) ([t (in-list tails)]) (apply-tail acc t))]

    [(list 'primary inner) (lower inner)]

    ;; leaves
    [(list 'literal "{" "}") (fp-lit 'empty)]
    [(list 'literal (list 'quantity qparts ...)) (fp-lit (lower-quantity qparts))]
    [(list 'literal v) (fp-lit (lit-value v))]
    [(list 'member name) (fp-invoke #f (id->sym (unwrap-id name)))]
    [(list 'identifier x) (id->sym x)]
    [(list 'function-call name args ...) (fp-call #f (id->sym (unwrap-id name)) (lower-args args))]
    [(list 'special-var v) (fp-var (string->symbol v))]
    [(list 'external-constant name) (fp-extern name)]

    [_ (error 'fhirpath-lower "unexpected node: ~s" datum)]))

(define (fold-binops acc rest)
  (match rest
    ['() acc]
    [(list-rest op operand more)
     (fold-binops (fp-binop (op->sym op) acc (lower operand)) more)]))

(define (fold-typeops acc rest)
  (match rest
    ['() acc]
    [(list-rest op typespec more)
     (fold-typeops (fp-typeop (op->sym op) acc (lower-type-spec typespec)) more)]))

(define (apply-tail target tail)
  (match tail
    [(list 'postfix-tail (list 'invocation inner))
     (match inner
       [(list 'member name) (fp-invoke target (id->sym (unwrap-id name)))]
       [(list 'function-call name args ...) (fp-call target (id->sym (unwrap-id name)) (lower-args args))]
       [(list 'special-var v) (fp-invoke target (string->symbol v))])]
    [(list 'postfix-tail (list 'indexer idx)) (fp-index target (lower idx))]))

(define (lower-args rest)
  (match rest
    ['() '()]
    [(list (list 'arg-list xs ...)) (map lower xs)]
    [_ (map lower rest)]))

;; a grammar `identifier` node is (identifier IDENTIFIER|"as"|"is"|…) ; unwrap to its atom
(define (unwrap-id x)
  (match x
    [(list 'identifier a) a]
    [_ x]))

(define (lower-type-spec ts)
  (match ts
    [(list 'type-spec parts ...)
     (string->symbol
      (apply string-append
             (add-between (map (lambda (p) (symbol->string (id->sym (unwrap-id p))))
                               (filter (lambda (p) (or (symbol? p) (pair? p))) parts))
                          ".")))]
    [_ (lower ts)]))

;; a quantity node's parts: (NUMBER) or (NUMBER (unit u))
(define (lower-quantity qparts)
  (match qparts
    [(list num) (num-value num)]
    [(list num (list 'unit u)) (qty-lit (num-value num) (unit->string u))]))

(define (num-value tok)
  ;; tok is the NUMBER lexeme string
  (if (regexp-match? #rx"[.]" tok) (dec-lit tok) (string->number tok)))

(define (unit->string u)
  (cond [(string? u) u]                              ; a UCUM STRING
        [(symbol? u) (symbol->string u)]             ; a bare calendar keyword (IDENTIFIER)
        [(and (pair? u) (eq? (car u) 'identifier)) (symbol->string (id->sym (cadr u)))]
        [else (format "~a" u)]))

(define (lit-value v)
  (cond
    [(or (dec-lit? v) (qty-lit? v) (date-lit? v) (datetime-lit? v) (time-lit? v)) v]
    [(string? v) v]
    [(boolean? v) v]
    [(number? v) v]
    [(eq? v 'empty) 'empty]
    [else v]))
