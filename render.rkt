#lang racket/base

;;; render.rkt — an evaluation result item as text (the command line) or as JSON (the workspace,
;;; fhir-mcp). A complex item is its FHIR JSON; a System value is written as FHIRPath writes it
;;; (152.5, 2L, 5 'mg', @2024-01-01, @T10:30, System.Integer), and in JSON a number stays a number.
;;; One definition for both: the workspace used to print a decimal as `#(struct:fp-decimal 150 0)`.

(require racket/contract json
         (only-in fhir/model/node fhir?)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in "value.rkt" item->system fp-long? fp-long-v fp-quantity? typeinfo? typeinfo-namespace typeinfo-name)
         (only-in "decimal.rkt" fp-decimal? decimal->string)
         (only-in "quantity.rkt" quantity->string)
         (only-in "temporal-ops.rkt" value->moment moment->string))

(provide
 (contract-out
  ;; item->text : item -> string   for a terminal: FHIR JSON text, or the FHIRPath form
  [item->text (-> any/c string?)]
  ;; item->json : item -> jsexpr   for a JSON answer: FHIR JSON, a JSON number/boolean/string
  [item->json (-> any/c jsexpr?)]))

;; system-text : System value -> string   as FHIRPath writes it
(define (system-text s)
  (cond [(string? s) s]
        [(boolean? s) (if s "true" "false")]
        [(fp-decimal? s) (decimal->string s)]
        [(fp-quantity? s) (quantity->string s)]
        [(fp-long? s) (format "~aL" (fp-long-v s))]
        [(typeinfo? s) (format "~a.~a" (typeinfo-namespace s) (typeinfo-name s))]
        [(value->moment s) => moment->string]
        [else (format "~a" s)]))

(define (item->text v)
  (cond [(fhir? v) (jsexpr->string (node->jsexpr v))]
        [else (let ([s (item->system v)])
                (if (fhir? s) (jsexpr->string (node->jsexpr s)) (system-text s)))]))

(define (item->json v)
  (cond [(fhir? v) (node->jsexpr v)]
        [else (let ([s (item->system v)])
                (cond [(fhir? s) (node->jsexpr s)]
                      [(or (string? s) (boolean? s) (exact-integer? s)) s]
                      [(and (real? s) (rational? s)) s]
                      [(fp-decimal? s) (or (string->number (decimal->string s) 10) (decimal->string s))]
                      [(fp-long? s) (fp-long-v s)]
                      [else (system-text s)]))]))

(module+ test
  (require rackunit (only-in "decimal.rkt" parse-decimal))
  ;; a decimal is a JSON number (it was "#(struct:fp-decimal 150 0)"), its text the FHIRPath lexeme
  (check-equal? (item->json (parse-decimal "150")) 150)
  (check-equal? (item->json (parse-decimal "8.1")) 8.1)
  (check-equal? (item->text (parse-decimal "8.10")) "8.10")
  (check-equal? (item->json "a") "a")
  (check-equal? (item->json #t) #t)
  (check-equal? (item->json 3) 3)
  (check-equal? (item->text #f) "false"))
