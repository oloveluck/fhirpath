#lang racket/base

;;; harness.rkt — the data-driven runner over the official FHIRPath test suites.
;;;
;;; Loads tests-fhir-r{4,5}.xml, parses each <test> (name/inputfile/expression/outputs + invalid/
;;; predicate/ordered/mode), evaluates against the input resource (parsed to a typed node), and
;;; compares results to expected outputs by type. The tower for the target version must already be
;;; required by the caller (run-r4.rkt / run-r5.rkt) so from-json can parse the inputs.

(require racket/contract racket/list racket/string racket/math xml
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/read-json read-fhir-json)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in "../parser/parse.rkt" parse-fhirpath)
         (only-in "../eval.rkt" eval-fhirpath)
         (only-in "../error.rkt" exn:fp? exn:fp-kind)
         (only-in "../value.rkt" item->system fp-quantity? fp-long? fp-long-v)
         (only-in "../decimal.rkt" fp-decimal? parse-decimal decimal->string decimal-cmp)
         (only-in "../temporal-ops.rkt" value->moment moment? moment->string moment-equiv?
                  parse-fp-date parse-fp-datetime parse-fp-time)
         (only-in "../quantity.rkt" quantity->string))

(provide (contract-out [run-suite (->* (path-string? path-string?) (#:terminology any/c) any)]))

;; ---- XML helpers ----------------------------------------------------------------------------
(define (children e name)
  (filter (lambda (c) (and (element? c) (eq? (element-name c) name))) (element-content e)))
(define (child e name) (let ([cs (children e name)]) (and (pair? cs) (car cs))))
(define (attr e name) (let ([a (assq name (map (lambda (a) (cons (attribute-name a) (attribute-value a)))
                                               (element-attributes e)))]) (and a (cdr a))))
(define (elem-text e) (apply string-append (for/list ([c (element-content e)] #:when (pcdata? c)) (pcdata-string c))))
(define (all-tests doc)
  (let loop ([x (document-element doc)])
    (cond [(and (element? x) (eq? (element-name x) 'test)) (list x)]
          [(element? x) (append-map loop (element-content x))]
          [else '()])))

;; ---- running --------------------------------------------------------------------------------
(define (run-suite xml-path inputs-dir #:terminology [tx #f])
  (define doc (call-with-input-file xml-path read-xml))
  (define tests (all-tests doc))
  (define cache (make-hash))
  (define (load-input file0)
    (define file (and file0 (regexp-replace #rx"[.]xml$" file0 ".json")))
    (and file
         (hash-ref! cache file
                    (lambda ()
                      (define p (build-path inputs-dir file))
                      (if (file-exists? p)
                          (let ([r (from-json (call-with-input-file p read-fhir-json) #:lenient? #t)])
                            (and (ok? r) (list (ok-value r))))
                          'missing)))))
  (define pass 0) (define fail 0) (define errs 0) (define skip 0)
  (define failures '())
  (define by-outcome (make-hash))
  (for ([t (in-list tests)])
    (define expr-el (child t 'expression))
    (define expr (and expr-el (elem-text expr-el)))
    (define invalid-raw (and expr-el (or (attr expr-el 'invalid) (attr t 'invalid))))
    ;; only these denote an expected failure; "false"/absent = a normal (valid) test
    (define invalid (and (member invalid-raw '("syntax" "semantic" "execution" "true")) invalid-raw))
    (define predicate? (equal? (attr t 'predicate) "true"))
    (define inputfile (attr t 'inputfile))
    (define outputs (for/list ([o (children t 'output)]) (cons (attr o 'type) (elem-text o))))
    (define input (load-input inputfile))
    (cond
      [(eq? input 'missing) (set! skip (add1 skip))]
      [(not expr) (set! skip (add1 skip))]
      [else
       (define mode (attr t 'mode))
     (define strict? (not (and mode (regexp-match? #rx"lenient" mode))))
     (define outcome (run-one expr (or input '()) invalid predicate? outputs strict? tx))
       (define key (if (pair? outcome) (car outcome) outcome))
       (cond
         [(eq? key 'pass) (set! pass (add1 pass))]
         [else (set! fail (add1 fail))
               (hash-update! by-outcome key add1 0)
               (set! failures (cons (list (attr t 'name) expr outcome (map cdr outputs)) failures))])]))
  (printf "\n==== ~a ====\n" xml-path)
  (printf "pass ~a | fail ~a | skipped(no-input) ~a | total ~a\n" pass fail skip (length tests))
  (printf "pass rate (of runnable): ~a%\n"
          (if (> (+ pass fail) 0) (exact-round (* 100.0 (/ pass (+ pass fail)))) 0))
  (printf "outcomes: ~s\n" (sort (hash->list by-outcome) > #:key cdr))
  ;; dump all failures for analysis
  (call-with-output-file "/tmp/fp-failures.txt" #:exists 'replace
    (lambda (o) (for ([f (in-list (reverse failures))])
                  (fprintf o "[~a] ~a\n  ~s expected=~s\n" (caddr f) (cadr f) (caddr f) (cadddr f)))))
  (list pass fail skip))

(define (run-one expr input invalid predicate? outputs strict? [tx #f])
  (with-handlers ([exn:fp? (lambda (e) (if (and invalid (member invalid (list "true" "semantic" "execution" (symbol->string (exn:fp-kind e)))))
                                           'pass (list 'error (exn-message e))))]
                  [exn:fail? (lambda (e) (if invalid 'pass (list 'error (exn-message e))))])
    (define ast (parse-fhirpath expr))
    (cond
      [(equal? invalid "syntax") 'unexpected-parse]   ; should have failed to parse
      [else
       (define result (eval-fhirpath ast input #:strict? strict?
                                     #:externals (if tx (hash "terminologies" (list tx)) (hash))))
       (cond
         [invalid 'unexpected-ok]                     ; should have errored
         [predicate? (if (equal? (pred-bool result) (expected-bool outputs)) 'pass
                         (list 'mismatch (list (pred-bool result))))]
         [(outputs-match? result outputs) 'pass]
         [else (list 'mismatch (map (lambda (i) (with-handlers ([(lambda (_) #t) (lambda (_) "?")]) (render i))) result))])])))

;; ---- output comparison ----------------------------------------------------------------------
(define (outputs-match? result outputs)
  (and (= (length result) (length outputs))
       (for/and ([item (in-list result)] [o (in-list outputs)])
         (output-item=? item (car o) (cdr o)))))

(define (output-item=? item type text)
  (cond
    [(not type)
     ;; typeless output: a temporal item is written with a leading @, so compare as a moment
     (define m (value->moment (item->system item)))
     (cond
       [(and m (string-prefix? text "@"))
        (define em (parse-any-temporal text))
        (and em (moment-equiv? m em))]
       [else (equal? (render item) text)])]
    [(member type '("boolean")) (equal? (render item) text)]
    [(member type '("integer" "unsignedInt" "positiveInt" "long" "integer64")) (equal? (render item) text)]
    [(equal? type "decimal") (let ([a (num->dec item)] [b (parse-decimal text)])
                               (and a b (= (decimal-cmp a b) 0)))]
    [(member type '("date" "dateTime" "time"))
     (define im (value->moment (item->system item)))
     (define em (parse-expected-temporal text type))
     (and im em (moment-equiv? im em))]
    [(equal? type "Quantity") (equal? (render item) (normalize-qty text))]
    [else (equal? (render item) text)]))   ; string/code/uri/id/…

(define (num->dec item)
  (define s (item->system item))
  (cond [(fp-decimal? s) s] [(exact-integer? s) (parse-decimal (number->string s))]
        [(fp-long? s) (parse-decimal (number->string (fp-long-v s)))] [else #f]))

(define (parse-expected-temporal text type)
  (define s (if (string-prefix? text "@") (substring text 1) text))
  (case type
    [("date") (parse-fp-date s)]
    [("dateTime") (parse-fp-datetime s)]
    [("time") (parse-fp-time (if (string-prefix? s "T") (substring s 1) s))]))

(define (normalize-qty text) text)   ; refine in B5

;; parse an @-prefixed temporal literal of unknown kind (date / dateTime / time)
(define (parse-any-temporal text)
  (define s (substring text 1))       ; drop @
  (cond [(string-prefix? s "T") (parse-fp-time (substring s 1))]
        [(regexp-match? #rx"T" s) (parse-fp-datetime s)]
        [else (parse-fp-date s)]))

(define (render item)
  (define s (item->system item))
  (cond
    [(string? s) s]
    [(boolean? s) (if s "true" "false")]
    [(exact-integer? s) (number->string s)]
    [(fp-long? s) (number->string (fp-long-v s))]
    [(fp-decimal? s) (decimal->string s)]
    [(fp-quantity? s) (quantity->string s)]
    [(value->moment s) => (lambda (m) (substring (moment->string m) 1))]
    [else "«complex»"]))

(define (pred-bool result)
  (cond [(null? result) #f]
        [(and (null? (cdr result)) (boolean? (item->system (car result)))) (item->system (car result))]
        [else #t]))
(define (expected-bool outputs)
  (and (pair? outputs) (equal? (cdr (car outputs)) "true")))
