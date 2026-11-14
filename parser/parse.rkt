#lang racket/base

;;; parse.rkt — the FHIRPath parse API: string -> AST.
;;;
;;; Shared by the library and the (future) #lang reader. Goes through brag's parse-to-datum then
;;; lowers to the compact prefab AST. A parse failure raises exn:fail (the harness maps this to a
;;; `syntax` invalid-test outcome).

(require racket/contract
         (only-in brag/support token srcloc-token? srcloc-token-token token-struct-type token-struct-val token-struct? token-struct-skip? token-struct-offset
                  token-struct-line token-struct-column token-struct-span)
         (only-in fhir/diagnostic raise-diagnostic exn->diagnostic exn:fail:diagnostic? tokens->last-position error-position
                  diagnostic loc)
         (prefix-in g: "grammar.rkt")
         "lexer.rkt"
         "ast.rkt")

(provide
 (contract-out
  [parse-fhirpath         (-> string? any/c)]     ; one expression -> AST
  [parse-fhirpath-program (-> string? list?)]))    ; ;-separated program -> (listof AST)

;; Memoized: the same expressions (invariants, search params, navigation) are parsed thousands of times
;; by the validator and capability search. ASTs are immutable prefab values, so sharing is safe.
;; FHIRPath is ONE expression: several separated by `;` are a parse error at the first `;`.
(define parse-cache (make-hash))
(define (parse-fhirpath str)
  (hash-ref! parse-cache str
             (lambda ()
               (define program (parse-fhirpath-program str))
               (if (null? (cdr program))
                   (car program)
                   (raise-diagnostic
                    (diagnostic 'fhirpath 'parse 'error
                                (format "unexpected `;`: one expression is expected, this is ~a" (length program))
                                (separator-loc (lex str))))))))

;; lex : string -> (listof token)   every token of the text, skipped ones (space, comments) included
(define (lex str) (collect (tokenize (open-input-string str))))

;; separator-loc : (listof token) -> loc|#f   where the first `;` stands (the grammar has `;` between
;; the expressions of a program only; one inside a string is part of a STRING token)
(define (separator-loc toks)
  (for/first ([t (in-list toks)]
              #:when (let ([k (probe-token t)]) (and k (equal? (format "~a" (token-struct-type k)) ";"))))
    (define k (probe-token t))
    (loc #f (token-struct-line k) (let ([c (token-struct-column k)]) (and c (add1 c)))
         (token-struct-offset k) (token-struct-span k))))

;; a syntax error raises exn:fail:diagnostic (fhir/diagnostic): positioned, "the expression ends
;; after `x`" when the input runs out; a text with no token at all (nothing, space, a comment) is
;; "the expression is empty" (brag's own words for it were "unexpected no-tokens `#f`")
(define (parse-fhirpath-program str)
  (define toks (lex str))
  (unless (ormap probe-token toks)
    (raise-diagnostic (diagnostic 'fhirpath 'parse 'error "the expression is empty" #f)))
  (with-handlers ([(lambda (e) (and (exn:fail? e) (not (exn:fail:diagnostic? e))))
                   (lambda (e)
                     (define last-pos (tokens->last-position toks real-position))
                     (raise-diagnostic
                      (exn->diagnostic e 'fhirpath #:last-position last-pos #:unit "expression"
                                       #:ran-out? (ran-out-probe toks last-pos))))])
    (define datum (g:parse-to-datum (let ([ts toks]) (lambda () (if (null? ts) (void) (begin0 (car ts) (set! ts (cdr ts))))))))
    (check-bare-units! toks)
    (lower-program datum)))

;; check-bare-units! : (listof token) -> Void
;; a quantity's unit is a calendar duration word written bare (`5 days`) or a UCUM unit written as a
;; string (`5 'mg'`): the grammar's `unit` is dateTimePrecision | pluralDateTimePrecision | STRING.
;; `5 mg` was read as `5 'mg'`; it is a parse error at the word. A NUMBER followed by an IDENTIFIER
;; is always a quantity (`as`, `is`, `div`, … are tokens of their own), so the tokens decide it.
(define (check-bare-units! toks)
  (let loop ([ks (filter values (map probe-token toks))])
    (when (and (pair? ks) (pair? (cdr ks)))
      (define num (car ks)) (define unit (cadr ks))
      (when (and (eq? (token-struct-type num) 'NUMBER) (eq? (token-struct-type unit) 'IDENTIFIER)
                 (not (calendar-word? (symbol->string (token-struct-val unit)))))
        (define word (symbol->string (token-struct-val unit)))
        (raise-diagnostic
         (diagnostic 'fhirpath 'parse 'error
                     (format "`~a` is no calendar duration (year … millisecond, or a plural): a UCUM unit is quoted, ~a '~a'"
                             word (token-struct-val num) word)
                     (loc #f (token-struct-line unit) (let ([c (token-struct-column unit)]) (and c (add1 c)))
                          (token-struct-offset unit) (token-struct-span unit)))))
      (loop (cdr ks)))))
;; probe-token : token|srcloc-token -> token-struct|#f   (skip tokens excluded)
(define (probe-token t)
  (define k (if (srcloc-token? t) (srcloc-token-token t) t))
  (and (token-struct? k) (not (token-struct-skip? k)) k))
;; ran-out-probe : tokens last-position -> (-> boolean)   re-parse with a sentinel token after the
;; last one: if the error moves to the sentinel, the input was a valid prefix that ran out
(define ((ran-out-probe toks last-pos) )
  ;; the sentinel is a COPY of the last real token (a type the grammar knows), placed after it
  (define last-real (for/last ([t (in-list toks)] #:when (probe-token t)) (probe-token t)))
  (and last-pos last-real
       (with-handlers ([exn:fail? (lambda (e) (let ([p (error-position e)]) (and p (> p last-pos))))])
         (g:parse-to-datum (let ([ts (append toks (list (token (token-struct-type last-real) (token-struct-val last-real)
                                                                #:position (+ last-pos 100000) #:line 1 #:column 0)))])
                             (lambda () (if (null? ts) (void) (begin0 (car ts) (set! ts (cdr ts)))))))
         #f)))
(define (collect next) (let loop ([acc '()]) (define t (next)) (if (void? t) (reverse acc) (loop (cons t acc)))))
(define (real-position t) (and (token-struct? t) (not (token-struct-skip? t)) (token-struct-offset t)))

(module+ test
  (require fhir/diagnostic)
  (require rackunit)
  ;; path navigation lowers to nested invokes
  (check-equal? (parse-fhirpath "Patient.name.given")
                (fp-invoke (fp-invoke (fp-invoke #f 'Patient) 'name) 'given))
  ;; precedence: a or b and c  =>  a or (b and c)
  (check-equal? (parse-fhirpath "a or b and c")
                (fp-binop 'or (fp-invoke #f 'a)
                          (fp-binop 'and (fp-invoke #f 'b) (fp-invoke #f 'c))))
  ;; function call with args, unevaluated later
  (check-equal? (parse-fhirpath "name.where(use = 'official')")
                (fp-call (fp-invoke #f 'name) 'where
                         (list (fp-binop '= (fp-invoke #f 'use) (fp-lit "official")))))
  ;; indexer
  (check-equal? (parse-fhirpath "name[0]") (fp-index (fp-invoke #f 'name) (fp-lit 0)))
  ;; type ops + qualified type
  (check-equal? (parse-fhirpath "value is Quantity")
                (fp-typeop 'is (fp-invoke #f 'value) 'Quantity))
  (check-equal? (parse-fhirpath "x is System.Decimal")
                (fp-typeop 'is (fp-invoke #f 'x) 'System.Decimal))
  ;; literals: integer, decimal (precision), string, boolean, empty
  (check-equal? (parse-fhirpath "1") (fp-lit 1))
  (check-equal? (parse-fhirpath "1.50") (fp-lit (dec-lit "1.50")))
  (check-equal? (parse-fhirpath "{}") (fp-lit 'empty))
  (check-equal? (parse-fhirpath "true") (fp-lit #t))
  ;; quantities: calendar keyword + UCUM string
  (check-equal? (parse-fhirpath "1 year") (fp-lit (qty-lit 1 "year")))
  (check-equal? (parse-fhirpath "4 'mg'") (fp-lit (qty-lit 4 "mg")))
  ;; temporal literals keep their lexeme; a trailing dot is navigation
  (check-equal? (parse-fhirpath "@2014-01-25") (fp-lit (date-lit "2014-01-25")))
  (check-equal? (parse-fhirpath "@2014-01-25T14:30:00Z") (fp-lit (datetime-lit "2014-01-25T14:30:00Z")))
  (check-equal? (parse-fhirpath "@T12:00") (fp-lit (time-lit "12:00")))
  (check-equal? (parse-fhirpath "@2014.toString()")
                (fp-call (fp-lit (date-lit "2014")) 'toString '()))
  ;; keyword as member name
  (check-equal? (parse-fhirpath "Patient.contains") (fp-invoke (fp-invoke #f 'Patient) 'contains))
  ;; externals + special vars
  (check-equal? (parse-fhirpath "%context") (fp-extern "context"))
  (check-equal? (parse-fhirpath "$this") (fp-var '$this))
  ;; unary minus
  (check-equal? (parse-fhirpath "-3") (fp-unop '- (fp-lit 3)))
  ;; diagnostics
  (define (diag s) (with-handlers ([exn:fail:diagnostic? (lambda (e) (diagnostic->string (exn:fail:diagnostic-diagnostic e)))]) (parse-fhirpath s) #f))
  (check-equal? (diag "Patient.name.where(") "<input>:1:19: fhirpath parse error: the expression ends after `(`")
  (check-equal? (diag "Patient.name.where(use = )") "<input>:1:26: fhirpath parse error: unexpected `)`")
  (check-equal? (diag "Patient.name ## x") "<input>:1:14: fhirpath lex error: unrecognized text `##`")
  ;; the text quoted is where the lexer stopped, not what followed it (`@@` quoted nothing)
  (check-equal? (diag "@@") "<input>:1:1: fhirpath lex error: unrecognized text `@@`")
  (check-equal? (diag "a = @x") "<input>:1:5: fhirpath lex error: unrecognized text `@x`")
  (check-equal? (diag "code.contains(\":\")") "<input>:1:15: fhirpath lex error: unrecognized text `\":\")` (a string is written in single quotes: 'text')")
  ;; one expression: a second one after `;` is a parse error at the `;` (it was a plain exn:fail, a
  ;; stack trace at the command line); a `;` inside a string is text
  (check-equal? (diag "1 = 1; 2") "<input>:1:6: fhirpath parse error: unexpected `;`: one expression is expected, this is 2")
  (check-equal? (diag "name;\n given; 3") "<input>:1:5: fhirpath parse error: unexpected `;`: one expression is expected, this is 3")
  (check-equal? (parse-fhirpath "'a; b'") (fp-lit "a; b"))
  (check-equal? (diag "1 = 1;") "<input>:1:6: fhirpath parse error: the expression ends after `;`")
  ;; no expression at all (it was "unexpected no-tokens `#f`")
  (check-equal? (diag "") "fhirpath parse error: the expression is empty")
  (check-equal? (diag "  \n") "fhirpath parse error: the expression is empty")
  (check-equal? (diag "// only a comment") "fhirpath parse error: the expression is empty")
  ;; a bare unit is a calendar word; a UCUM unit is quoted (`5 mg` was read as 5 'mg')
  (check-equal? (diag "value > 5 mg") "<input>:1:11: fhirpath parse error: `mg` is no calendar duration (year … millisecond, or a plural): a UCUM unit is quoted, 5 'mg'")
  (check-equal? (diag "1.5 kg") "<input>:1:5: fhirpath parse error: `kg` is no calendar duration (year … millisecond, or a plural): a UCUM unit is quoted, 1.5 'kg'")
  (check-equal? (parse-fhirpath "5 days") (fp-lit (qty-lit 5 "days")))
  (check-equal? (parse-fhirpath "5 'days'") (fp-lit (qty-lit 5 "days")))
  (check-equal? (parse-fhirpath "5 div 2") (fp-binop 'div (fp-lit 5) (fp-lit 2))))
