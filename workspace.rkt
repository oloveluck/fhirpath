#lang racket/base

;;; workspace.rkt — what FHIRPath offers a workspace (fhir/lang/workspace): an expression run over the
;;; resources of the data source, and the value of every part of it over one resource.
;;;
;;; The resources a run reads are `over` (a type, or "Type/id"), else the type the expression starts
;;; with (`Patient.name.given` reads every Patient). Resources are typed by the workspace's FHIR
;;; version (the R4 tower for 4.0.1, R5 otherwise).
;;; A Row is {resource: "Type/id", value: [item …]}; an item is a primitive's value, or FHIR JSON.
;;; An explanation is a Part: {expression, value, parts: [Part]} — each sub-expression that is
;;; evaluated against the same input (a path's target, an operator's operands); a function's
;;; arguments are evaluated per item, so they are listed by their text, not a value.
;;; An expression declares nothing, so it has no artifacts.

(require racket/contract racket/list racket/string racket/match racket/runtime-path racket/file json
         fhir/lang/workspace
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic)
         (only-in fhir/data-source current-data-source data-source-all data-source-read)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in fhir/model/node fhir?)
         (only-in "parser/parse.rkt" parse-fhirpath)
         (only-in "parser/ast.rkt" fp-invoke fp-invoke? fp-invoke-target fp-invoke-name fp-call fp-call-target fp-call-args
                  fp-index fp-binop fp-unop fp-typeop)
         (only-in "print.rkt" fhirpath->string)
         (only-in "eval.rkt" eval-fhirpath)
         (only-in "value.rkt" item->system))

(provide (contract-out [workspace-support ws-support?]
                       ;; guide : the language guide (guide.md) a hub serves to agents (learn://fhirpath)
                       [guide string?]))

(define-runtime-path guide-file "guide.md")
(define guide (file->string guide-file))

(define (expression text) (string-trim (regexp-replace #px"^#lang[^\n]*" text "")))

(define (load-tower! ws) (dynamic-require (if (workspace-r4? ws) 'fhir/r4/schemas 'fhir/r5/schemas) #f))

(define (artifacts ws files)
  (values '()
          (for*/list ([f (in-list files)]
                      [d (in-value (with-handlers ([exn:fail:diagnostic? exn:fail:diagnostic-diagnostic])
                                     (parse-fhirpath (expression (cdr f))) #f))]
                      #:when d)
            d)))

;; head-type : ast -> string|#f   the type an expression starts with (`Patient` in Patient.name)
(define (head-type ast)
  (match ast
    [(fp-invoke #f name) (let ([s (symbol->string name)]) (and (char-upper-case? (string-ref s 0)) s))]
    [(fp-invoke t _) (head-type t)]
    [(fp-call t _ _) (and t (head-type t))]
    [(fp-index t _) (head-type t)]
    [(fp-binop _ l r) (or (head-type l) (head-type r))]
    [(fp-unop _ x) (head-type x)]
    [(fp-typeop _ x _) (head-type x)]
    [_ #f]))

;; resources : workspace ast options -> (listof resource jsexpr)
(define (resources ast opts)
  (define ds (current-data-source))
  (define over (or (hash-ref opts 'over #f) (head-type ast)))
  (match over
    [#f (error 'fhirpath "say which resources the expression reads: `over` a type or Type/id, or start the expression with the type (Patient.name)")]
    [(pregexp #px"^([A-Za-z]+)/(.+)$" (list _ type id))
     (define r ((data-source-read ds) type id))
     (unless r (error 'fhirpath (format "no ~a/~a in the data source" type id)))
     (list r)]
    [type ((data-source-all ds) type)]))

(define (typed r)
  (define n (from-json r #:lenient? #t))
  (unless (ok? n) (error 'fhirpath (format "~a/~a does not read as FHIR" (hash-ref r 'resourceType "?") (hash-ref r 'id "?"))))
  (ok-value n))

;; item->json : item -> jsexpr
(define (item->json v)
  (cond [(fhir? v) (node->jsexpr v)]
        [else (define s (item->system v))
              (cond [(or (string? s) (boolean? s) (exact-integer? s)) s]
                    [(and (real? s) (rational? s)) s]
                    [(fhir? s) (node->jsexpr s)]
                    [else (format "~a" s)])]))
(define (reference r) (format "~a/~a" (hash-ref r 'resourceType "?") (hash-ref r 'id "?")))

(define (run ws path text opts)
  (load-tower! ws)
  (define ast (parse-fhirpath (expression text)))
  (define rows
    (for/list ([r (in-list (resources ast opts))])
      (hasheq 'resource (reference r)
              'value (with-handlers ([exn:fail? (lambda (e) (hasheq 'error (exn-message e)))])
                       (map item->json (eval-fhirpath ast (list (typed r)) #:strict? #f))))))
  (define (count p) (for/sum ([row (in-list rows)]) (if (p (hash-ref row 'value)) 1 0)))
  (run-result "resources" '("resource" "value") rows
              (hasheq 'resources (length rows)
                      'nonEmpty (count pair?)
                      'true (count (lambda (v) (equal? v '(#t))))
                      'errors (count hash?))))

;; parts : ast -> (listof ast)   the sub-expressions evaluated against the same input
(define (parts ast)
  (match ast
    [(fp-invoke t _) (if t (list t) '())]
    [(fp-call t _ _) (if t (list t) '())]
    [(fp-index t i) (list t i)]
    [(fp-binop _ l r) (list l r)]
    [(fp-unop _ x) (list x)]
    [(fp-typeop _ x _) (list x)]
    [_ '()]))

(define (explain ws path text subject opts)
  (load-tower! ws)
  (define ast (parse-fhirpath (expression text)))
  (define rs (resources ast (hash-set opts 'over (if (regexp-match? #rx"/" subject) subject (hash-ref opts 'over #f)))))
  (define r (if (regexp-match? #rx"/" subject) (car rs)
                (or (findf (lambda (x) (equal? (hash-ref x 'id #f) subject)) rs)
                    (error 'fhirpath (format "no resource with id ~a among those the expression reads" subject)))))
  (define root (list (typed r)))
  (define (part a)
    (hasheq 'expression (fhirpath->string a)
            'value (with-handlers ([exn:fail? (lambda (e) (hasheq 'error (exn-message e)))])
                     (map item->json (eval-fhirpath a root #:strict? #f)))
            'parts (map part (parts a))
            'arguments (match a [(fp-call _ _ args) (map fhirpath->string args)] [_ '()])))
  (hash-set (part ast) 'resource (reference r)))

(define workspace-support (ws-support "fhirpath" artifacts run explain))

(module+ test
  (require rackunit (only-in fhir/data-source resources->data-source))
  (define-values (ws _) (open-workspace (find-system-path 'temp-dir)))
  (define-values (as ds) (artifacts ws (list (cons "/tmp/a.fhirpath" "name.given") (cons "/tmp/b.fhirpath" "name.given("))))
  (check-equal? (length ds) 1)
  (define data (list (hasheq 'resourceType "Patient" 'id "a" 'name (list (hasheq 'use "official" 'given '("Ann" "B"))) 'active #t)
                     (hasheq 'resourceType "Patient" 'id "b" 'active #f)))
  (parameterize ([current-data-source (resources->data-source data)])
    (define r (run ws #f "Patient.name.where(use = 'official').given.first()" (hasheq)))
    (check-equal? (run-result-rows r) (list (hasheq 'resource "Patient/a" 'value '("Ann")) (hasheq 'resource "Patient/b" 'value '())))
    (check-equal? (hash-ref (run-result-summary r) 'nonEmpty) 1)
    (define r2 (run ws #f "active" (hasheq 'over "Patient")))
    (check-equal? (hash-ref (run-result-summary r2) 'true) 1)
    (check-exn #rx"say which resources" (lambda () (run ws #f "active" (hasheq))))
    (check-exn #rx"no Patient/zz" (lambda () (run ws #f "active" (hasheq 'over "Patient/zz"))))
    (define e (explain ws #f "Patient.name.given.count() > 1 and active" "a" (hasheq)))
    (check-equal? (hash-ref e 'value) '(#t))
    (check-equal? (map (lambda (p) (hash-ref p 'expression)) (hash-ref e 'parts)) '("Patient.name.given.count() > 1" "active"))
    (define count-part (car (hash-ref (car (hash-ref e 'parts)) 'parts)))
    (check-equal? (hash-ref count-part 'value) '(2))
    (check-equal? (hash-ref (car (hash-ref count-part 'parts)) 'value) '("Ann" "B"))))
