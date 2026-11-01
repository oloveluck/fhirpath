#lang racket/base

;;; parse.rkt — the FHIRPath parse API: string -> AST.
;;;
;;; Shared by the library and the (future) #lang reader. Goes through brag's parse-to-datum then
;;; lowers to the compact prefab AST. A parse failure raises exn:fail (the harness maps this to a
;;; `syntax` invalid-test outcome).

(require racket/contract
         (prefix-in g: "grammar.rkt")
         "lexer.rkt"
         "ast.rkt")

(provide
 (contract-out
  [parse-fhirpath         (-> string? any/c)]     ; one expression -> AST
  [parse-fhirpath-program (-> string? list?)]))    ; ;-separated program -> (listof AST)

;; Memoized: the same expressions (invariants, search params, navigation) are parsed thousands of times
;; by the validator and capability search. ASTs are immutable prefab values, so sharing is safe.
(define parse-cache (make-hash))
(define (parse-fhirpath str)
  (hash-ref! parse-cache str
             (lambda ()
               (define program (parse-fhirpath-program str))
               (cond
                 [(null? program) (error 'parse-fhirpath "empty expression")]
                 [(null? (cdr program)) (car program)]
                 [else (error 'parse-fhirpath "expected a single expression, got ~a" (length program))]))))

(define (parse-fhirpath-program str)
  (lower-program (g:parse-to-datum (tokenize (open-input-string str)))))

(module+ test
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
  (check-equal? (parse-fhirpath "-3") (fp-unop '- (fp-lit 3))))
