#lang racket/base

;;; descent.rkt — FHIRPath read by recursive descent, straight to the AST.
;;;
;;; grammar.rkt (brag, from the published fhirpath.g4) is the specification of what FHIRPath text
;;; is, and stays so: it reports every syntax error, and tests/descent-test.rkt holds this reader to
;;; it — the same AST as brag's tree lowered by ast.rkt's `lower`, over the HL7 suites, every
;;; expression of the core packages, and generated text. What this reader adds is speed: brag's
;;; general parser costs ~1 ms an expression; this one reads the grammar's precedence cascade
;;; directly, a token at a time.
;;;
;;; It only ever ACCEPTS. On any token it does not expect it answers #f, and parse.rkt hands the
;;; tokens to brag, whose answer — a tree, or the positioned error — is the one given. So a text
;;; this reader refuses is still read exactly as the grammar reads it (the few the grammar reads two
;;; ways, `5 is` or a sort key `5 desc`, a NUMBER with a keyword as its unit, are left to brag on purpose).
;;;
;;; A Token is a brag token-struct whose type is a symbol: a named token (NUMBER STRING BOOL
;;; IDENTIFIER DATE DATETIME TIME EXTERNAL) or the text of an operator/keyword (`and`, `.`, `<=` …),
;;; which `kind` gives as a string.
;;; A Cursor is (cursor tokens i): a vector of the text's tokens (spaces and comments left out) and
;;; the index of the next one; reading advances it by mutation, local to one parse.
;;;
;;; The grammar, lowest precedence first (grammar.rkt):
;;;   expr        : implies
;;;   implies     : or ("implies" or)*            or     : and (("or"|"xor") and)*
;;;   and         : membership ("and" membership)*
;;;   membership  : equality (("in"|"contains") equality)*
;;;   equality    : inequality (("="|"!="|"~"|"!~") inequality)*
;;;   inequality  : union (("<="|">="|"<"|">") union)*
;;;   union       : type ("|" type)*
;;;   type        : additive (("is"|"as") type-spec)*
;;;   additive    : multiplicative (("+"|"-"|"&") multiplicative)*
;;;   multiplicative : unary (("*"|"/"|"div"|"mod") unary)*
;;;   unary       : ("+"|"-")? postfix
;;;   postfix     : primary ("." (function-call | member | special-var) | "[" expr "]")*
;;;   primary     : literal | instance-selector | function-call | member | special-var | EXTERNAL
;;;               | "(" expr ")"
;;;   function-call : "sort" "(" (expr ("asc"|"desc")? ("," …)*)? ")" | identifier "(" args? ")"
;;;   member      : identifier | "sort"
;;;   instance-selector : identifier ("." identifier)* "{" (":" | identifier ":" expr ("," …)*) "}"
;;;   identifier  : IDENTIFIER | "as" | "is" | "in" | "contains" | "asc" | "desc"
;;;   literal     : NUMBER unit? | LONG | STRING | BOOL | DATE | DATETIME | TIME | "{" "}"
;;;   unit        : IDENTIFIER | STRING        (a keyword identifier as a unit is left to brag)

(require racket/contract
         (only-in brag/support token-struct-type token-struct-val)
         "ast.rkt")

(provide
 (contract-out
  ;; descend-program : (listof Token) -> (listof AST) | #f   the `;`-separated expressions, or #f
  ;; when the text is anything this reader does not take (brag then reads it)
  [descend-program (-> list? (or/c #f (listof any/c)))]))

(struct cursor (tokens [i #:mutable]))

;; ---- the cursor ---------------------------------------------------------------------------------
;; kind : Token -> Symbol|String   a token's type: a named token's symbol, an operator's or
;; keyword's text (brag keeps both as symbols: `|and|`, `|.|`)
(define named-kinds '(NUMBER LONG STRING BOOL IDENTIFIER DATE DATETIME TIME EXTERNAL))
(define (kind t)
  (define k (token-struct-type t))
  (if (memq k named-kinds) k (symbol->string k)))
;; peek : Cursor [Nat] -> Token|#f   the token `ahead` places on, without reading it
(define (peek c [ahead 0])
  (define j (+ (cursor-i c) ahead))
  (and (< j (vector-length (cursor-tokens c))) (vector-ref (cursor-tokens c) j)))
;; peek-kind : Cursor [Nat] -> kind|#f
(define (peek-kind c [ahead 0]) (let ([t (peek c ahead)]) (and t (kind t))))
;; next! : Cursor -> Token   reads one (the text ending here is not ours to say)
(define (next! c)
  (define t (peek c))
  (unless t (refuse))
  (set-cursor-i! c (add1 (cursor-i c)))
  t)
;; at? : Cursor kinds -> Boolean   the next token is one of these kinds
(define (at? c kinds) (and (member (peek-kind c) kinds) #t))

;; refused : the escape when the text is not what this reader takes
(struct refusal ())
(define (refuse) (raise (refusal)))
;; expect! : Cursor kind -> Token   reads a token of this kind, else refuses
(define (expect! c k) (if (equal? (peek-kind c) k) (next! c) (refuse)))

;; ---- the program --------------------------------------------------------------------------------
(define (descend-program tokens)
  (with-handlers ([refusal? (lambda (_) #f)])
    (define c (cursor (list->vector tokens) 0))
    (define first (expr c))
    (define rest
      (let loop ()
        (cond [(equal? (peek-kind c) ";") (next! c) (let ([e (expr c)]) (cons e (loop)))]
              [else '()])))
    (if (peek c) (refuse) (cons first rest))))   ; a token left over: not ours to say why

;; ---- the precedence cascade ---------------------------------------------------------------------
;; binary : Cursor (listof op) (Cursor -> AST) -> AST   operand (op operand)*, folded to the left
(define ((binary ops operand) c)
  (let loop ([acc (operand c)])
    (if (at? c ops)
        (let* ([op (op->symbol (kind (next! c)))] [rhs (operand c)])
          (loop (fp-binop op acc rhs)))
        acc)))
;; op->symbol : String -> Symbol   the AST's name of an operator (`|` is union)
(define (op->symbol s) (if (equal? s "|") 'union (string->symbol s)))

(define (expr c) (implies-expr c))
(define (implies-expr c) ((binary '("implies") or-expr) c))
(define (or-expr c) ((binary '("or" "xor") and-expr) c))
(define (and-expr c) ((binary '("and") membership-expr) c))
(define (membership-expr c) ((binary '("in" "contains") equality-expr) c))
(define (equality-expr c) ((binary '("=" "!=" "~" "!~") inequality-expr) c))
(define (inequality-expr c) ((binary '("<=" ">=" "<" ">") union-expr) c))
(define (union-expr c) ((binary '("|") type-expr) c))
(define (additive-expr c) ((binary '("+" "-" "&") multiplicative-expr) c))
(define (multiplicative-expr c) ((binary '("*" "/" "div" "mod") unary-expr) c))

;; type-expr : additive (("is"|"as") type-spec)*
(define (type-expr c)
  (let loop ([acc (additive-expr c)])
    (if (at? c '("is" "as"))
        (let* ([op (string->symbol (kind (next! c)))] [type (type-spec c)])
          (loop (fp-typeop op acc type)))
        acc)))

;; type-spec : identifier ("." identifier)*   -> the dotted name as one symbol (`System.Integer`)
(define (type-spec c)
  (let loop ([parts (list (identifier c))])
    (if (and (equal? (peek-kind c) ".") (identifier-kind? (peek-kind c 1)))
        (begin (next! c) (loop (cons (identifier c) parts)))
        (string->symbol (apply string-append (add-dots (reverse (map symbol->string parts))))))))
(define (add-dots xs) (if (or (null? xs) (null? (cdr xs))) xs (list* (car xs) "." (add-dots (cdr xs)))))

;; unary-expr : ("+"|"-")? postfix
(define (unary-expr c)
  (if (at? c '("+" "-"))
      (let ([op (string->symbol (kind (next! c)))]) (fp-unop op (postfix-expr c)))
      (postfix-expr c)))

;; postfix-expr : primary tail*
(define (postfix-expr c)
  (let loop ([acc (primary c)])
    (case (peek-kind c)
      [(".")
       (next! c)
       (cond [(at? c '("$this" "$index" "$total")) (loop (fp-invoke acc (string->symbol (kind (next! c)))))]
             [(equal? (peek-kind c) "sort")
              (next! c)
              (if (equal? (peek-kind c) "(")
                  (loop (fp-call acc 'sort (sort-keys c)))
                  (loop (fp-invoke acc 'sort)))]
             [else (define name (identifier c))
                   (if (equal? (peek-kind c) "(")
                       (loop (fp-call acc name (arguments c)))
                       (loop (fp-invoke acc name)))])]
      [("[") (next! c)
             (define index (expr c))
             (expect! c "]")
             (loop (fp-index acc index))]
      [else acc])))

;; primary : literal | function-call | identifier | special-var | EXTERNAL | "(" expr ")"
(define (primary c)
  (define k (peek-kind c))
  (cond
    [(not k) (refuse)]
    [(equal? k "(") (next! c) (begin0 (expr c) (expect! c ")"))]
    [(member k '("$this" "$index" "$total")) (fp-var (string->symbol (kind (next! c))))]
    [(eq? k 'EXTERNAL) (fp-extern (token-struct-val (next! c)))]
    [(equal? k "sort")
     (next! c)
     (if (equal? (peek-kind c) "(") (fp-call #f 'sort (sort-keys c)) (fp-invoke #f 'sort))]
    [(and (identifier-kind? k) (selector-ahead? c)) (instance-selector c)]
    [(identifier-kind? k)
     (define name (identifier c))
     (if (equal? (peek-kind c) "(") (fp-call #f name (arguments c)) (fp-invoke #f name))]
    [else (literal c)]))

;; selector-ahead? : Cursor -> Boolean   identifier ("." identifier)* then "{": an instance selector
(define (selector-ahead? c)
  (let loop ([j 1])
    (define k (peek-kind c j))
    (cond [(equal? k "{") #t]
          [(and (equal? k ".") (identifier-kind? (peek-kind c (add1 j)))) (loop (+ j 2))]
          [else #f])))

;; instance-selector : qualified-name "{" (":" | element (", " element)*) "}"
(define (instance-selector c)
  (define type (type-spec c))
  (expect! c "{")
  (cond
    [(equal? (peek-kind c) ":") (next! c) (expect! c "}") (fp-new type '())]
    [else
     (let loop ([elements '()])
       (define name (identifier c))
       (expect! c ":")
       (define el (cons name (expr c)))
       (cond [(equal? (peek-kind c) ",") (next! c) (loop (cons el elements))]
             [else (expect! c "}") (fp-new type (reverse (cons el elements)))]))]))

;; sort-keys : Cursor -> (listof AST)   "(" (key ("," key)*)? ")", a key `expr ("asc"|"desc")?`
(define (sort-keys c)
  (expect! c "(")
  (define (key)
    (define e (expr c))
    (case (peek-kind c)
      [("asc") (next! c) e]
      [("desc") (next! c) (fp-desc e)]
      [else e]))
  (cond
    [(equal? (peek-kind c) ")") (next! c) '()]
    [else (let loop ([keys (list (key))])
            (cond [(equal? (peek-kind c) ",") (next! c) (loop (cons (key) keys))]
                  [else (expect! c ")") (reverse keys)]))]))

;; arguments : Cursor -> (listof AST)   "(" (expr ("," expr)*)? ")"
(define (arguments c)
  (expect! c "(")
  (cond
    [(equal? (peek-kind c) ")") (next! c) '()]
    [else (let loop ([args (list (expr c))])
            (cond [(equal? (peek-kind c) ",") (next! c) (loop (cons (expr c) args))]
                  [else (expect! c ")") (reverse args)]))]))

;; identifier : Cursor -> Symbol   IDENTIFIER, or a keyword the grammar lets name a member or type
(define (identifier-kind? k) (or (eq? k 'IDENTIFIER) (and (member k '("as" "is" "in" "contains" "asc" "desc")) #t)))
(define (identifier c)
  (define t (next! c))
  (define v (token-struct-val t))
  (cond [(eq? (kind t) 'IDENTIFIER) (if (symbol? v) v (string->symbol v))]
        [(identifier-kind? (kind t)) (string->symbol (kind t))]
        [else (refuse)]))

;; literal : NUMBER unit? | STRING | BOOL | DATE | DATETIME | TIME | "{" "}"
(define (literal c)
  (define t (next! c))
  (define v (token-struct-val t))
  (case (kind t)
    [(NUMBER)
     (define n (if (regexp-match? #rx"[.]" v) (dec-lit v) (string->number v)))
     (case (peek-kind c)
       [(IDENTIFIER) (fp-lit (qty-lit n (symbol->string (token-struct-val (next! c)))))]
       [(STRING) (fp-lit (qty-lit n (token-struct-val (next! c))))]
       [else (if (identifier-kind? (peek-kind c))
                 ;; `5 is Integer` and `5 is` (a unit named `is`): the grammar reads either; brag decides
                 (if (unit-keyword-certain? c) (fp-lit n) (refuse))
                 (fp-lit n))])]
    [(LONG) (fp-lit v)]
    [(STRING) (fp-lit v)]
    [(BOOL) (fp-lit v)]
    [(DATE DATETIME TIME) (fp-lit v)]
    [("{") (expect! c "}") (fp-lit 'empty)]
    [else (refuse)]))

;; unit-keyword-certain? : Cursor -> Boolean   after a NUMBER, the next token is a keyword the grammar
;; also allows as a unit identifier; it is an operator for sure when what follows it could not
;; follow a quantity: an operand begins there (`5 in x`, `5 is Integer`), so the keyword cannot be
;; the unit. Otherwise (end, or an operator after it) leave it to brag.
(define (unit-keyword-certain? c)
  (define after (peek-kind c 1))
  (and after
       (or (identifier-kind? after) (memq after '(NUMBER LONG STRING BOOL DATE DATETIME TIME EXTERNAL))
           (member after '("(" "{" "$this" "$index" "$total" "+" "-")))
       #t))

(module+ test
  (require rackunit (only-in brag/support token))
  ;; tokens by hand: (kind value) pairs
  (define (toks . kvs) (for/list ([kv (in-list kvs)]) (token (car kv) (cdr kv))))
  (define (id s) (cons 'IDENTIFIER (string->symbol s)))
  (define (op s) (cons s s))
  (check-equal? (descend-program (toks (id "Patient") (op ".") (id "name") (op ".") (id "given")))
                (list (fp-invoke (fp-invoke (fp-invoke #f 'Patient) 'name) 'given)))
  ;; precedence and associativity
  (check-equal? (descend-program (toks (id "a") (op "or") (id "b") (op "and") (id "c")))
                (list (fp-binop 'or (fp-invoke #f 'a) (fp-binop 'and (fp-invoke #f 'b) (fp-invoke #f 'c)))))
  (check-equal? (descend-program (toks (cons 'NUMBER "1") (op "-") (cons 'NUMBER "2") (op "-") (cons 'NUMBER "3")))
                (list (fp-binop '- (fp-binop '- (fp-lit 1) (fp-lit 2)) (fp-lit 3))))
  (check-equal? (descend-program (toks (op "-") (id "a") (op "*") (id "b")))
                (list (fp-binop '* (fp-unop '- (fp-invoke #f 'a)) (fp-invoke #f 'b))))
  ;; type specifiers, keywords as names
  (check-equal? (descend-program (toks (id "x") (op "is") (id "System") (op ".") (id "Decimal")))
                (list (fp-typeop 'is (fp-invoke #f 'x) 'System.Decimal)))
  (check-equal? (descend-program (toks (id "Patient") (op ".") (op "contains")))
                (list (fp-invoke (fp-invoke #f 'Patient) 'contains)))
  ;; quantities; a keyword after a number is an operator when an operand follows, else brag's
  (check-equal? (descend-program (toks (cons 'NUMBER "4") (cons 'STRING "mg"))) (list (fp-lit (qty-lit 4 "mg"))))
  (check-equal? (descend-program (toks (cons 'NUMBER "5") (op "in") (id "x")))
                (list (fp-binop 'in (fp-lit 5) (fp-invoke #f 'x))))
  (check-false (descend-program (toks (cons 'NUMBER "5") (op "is"))))
  ;; programs; what is not ours
  (check-equal? (length (descend-program (toks (id "a") (op ";") (id "b")))) 2)
  (check-false (descend-program (toks (id "a") (op ")"))))
  (check-false (descend-program (toks (id "a") (op "."))))
  (check-false (descend-program '())))
