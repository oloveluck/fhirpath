#lang racket/base

;;; print.rkt — FHIRPath AST -> canonical expression text.
;;;
;;; The StructureMap resource carries conditions/expressions as STRINGS; the FML parser produces
;;; ASTs (fp-* prefabs), so emission needs a printer. A structural recursion over the FHIRPath data
;;; definition; parenthesization is conservative (every binop operand that is itself a binop is
;;; wrapped), which is always correct if not always minimal.

(require racket/contract racket/match racket/string
         (only-in "parser/ast.rkt"
                  fp-lit fp-invoke fp-call fp-index fp-binop fp-unop fp-typeop fp-var fp-extern
                  dec-lit dec-lit-lexeme qty-lit qty-lit-number qty-lit-unit
                  date-lit date-lit-str datetime-lit datetime-lit-str time-lit time-lit-str))

(provide (contract-out [fhirpath->string (-> any/c string?)]))

(define (fhirpath->string ast)
  (match ast
    [(fp-lit v) (lit->string v)]
    [(fp-invoke #f name) (symbol->string name)]
    [(fp-invoke t name) (format "~a.~a" (wrap-target t) name)]
    [(fp-call #f name args) (format "~a(~a)" name (args->string args))]
    [(fp-call t name args) (format "~a.~a(~a)" (wrap-target t) name (args->string args))]
    [(fp-index t i) (format "~a[~a]" (wrap-target t) (fhirpath->string i))]
    [(fp-binop op l r) (format "~a ~a ~a" (wrap l) (op->string op) (wrap r))]
    [(fp-unop op o) (format "~a~a" op (wrap o))]
    [(fp-typeop op o ty) (format "~a ~a ~a" (wrap o) op ty)]
    [(fp-var kind) (symbol->string kind)]
    [(fp-extern name) (format "%~a" name)]
    [_ (format "~a" ast)]))

(define (wrap x) (if (fp-binop? x) (format "(~a)" (fhirpath->string x)) (fhirpath->string x)))
;; the target of `.member`, `.f()` or `[i]` binds tighter than every operator, so an operator
;; target must keep its parentheses: `(value as CodeableConcept).text` printed bare re-parses as
;; the qualified TYPE `CodeableConcept.text`, and `(a | b).first()` as `a | b.first()`
(define (wrap-target x)
  (match x
    [(or (fp-binop _ _ _) (fp-unop _ _) (fp-typeop _ _ _)) (format "(~a)" (fhirpath->string x))]
    [_ (fhirpath->string x)]))
(define fp-binop? (lambda (x) (match x [(fp-binop _ _ _) #t] [_ #f])))
(define (args->string args) (string-join (map fhirpath->string args) ", "))
(define (op->string op) (if (eq? op 'union) "|" (symbol->string op)))

(define (lit->string v)
  (match v
    [(? string?) (format "'~a'" (escape-string v))]
    [#t "true"] [#f "false"]
    ['empty "{}"]
    [(? exact-integer?) (number->string v)]
    [(dec-lit lex) lex]
    [(qty-lit n u) (format "~a '~a'" (if (dec-lit? n) (dec-lit-lexeme n) n) u)]
    [(date-lit s) (format "@~a" s)]
    [(datetime-lit s) (format "@~a" s)]
    [(time-lit s) (format "@T~a" s)]
    [_ (format "~a" v)]))
(define dec-lit? (lambda (x) (match x [(dec-lit _) #t] [_ #f])))
;; the FHIRPath string escapes, so a printed literal lexes back to the same value (`'\\d+'`)
(define (escape-string s)
  (apply string-append
         (for/list ([c (in-string s)])
           (case c
             [(#\\) "\\\\"] [(#\') "\\'"] [(#\newline) "\\n"] [(#\return) "\\r"] [(#\tab) "\\t"] [(#\page) "\\f"]
             [else (string c)]))))

(module+ test
  (require rackunit (only-in "parser/parse.rkt" parse-fhirpath))
  (define (roundtrip s) (fhirpath->string (parse-fhirpath s)))
  (check-equal? (roundtrip "linkId = 'patient.sex'") "linkId = 'patient.sex'")
  (check-equal? (roundtrip "item.answer.valueString") "item.answer.valueString")
  (check-equal? (roundtrip "a and b or c") "(a and b) or c")
  (check-equal? (roundtrip "name.where(use = 'official')") "name.where(use = 'official')")
  (check-equal? (roundtrip "(Observation.value as CodeableConcept).text") "(Observation.value as CodeableConcept).text")
  (check-equal? (roundtrip "(a | b).first()") "(a | b).first()")
  ;; a string literal keeps its escapes (the lexer unescapes, the printer escapes again)
  (check-equal? (roundtrip "a.matches('\\\\d+')") "a.matches('\\\\d+')")
  (check-equal? (parse-fhirpath (roundtrip "x = 'it\\'s'")) (parse-fhirpath "x = 'it\\'s'"))
  (check-equal? (roundtrip "(-x).abs()") "(-x).abs()")
  ;; print∘parse is a fixpoint: re-parsing the printed text yields the same AST
  (for ([s '("linkId.value in ('patient.sex')" "%value + 5 days" "iif(x, 'a', 'b')")])
    (check-equal? (parse-fhirpath (fhirpath->string (parse-fhirpath s)))
                  (parse-fhirpath s))))
