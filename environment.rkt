#lang racket/base

;;; environment.rkt — the evaluation context: $this/$index/$total, user variables (defineVariable),
;;; and `%` external constants.

(require racket/contract)

(provide
 (struct-out env)
 (contract-out
  [make-env       (->* (any/c) (#:root any/c #:externals hash?) env?)]
  [env-with-item  (-> env? any/c exact-nonnegative-integer? env?)]
  [env-with-this  (-> env? list? env?)]
  [env-with-total (-> env? any/c env?)]
  [env-clone-vars (-> env? env?)]
  [env-define-var (-> env? string? any/c env?)]
  [env-var-bound? (-> env? string? boolean?)]
  [env-lookup-external (-> env? string? (or/c list? #f))]))

;; this  : the current focus collection (for $this)
;; index : the current iteration index (for $index)
;; total : the aggregate accumulator collection (for $total)
;; vars  : a Scope — (box (hash string -> collection))  (defineVariable + seeded %context/%resource)
;; externals : hash string -> collection  (caller-supplied env vars / terminology)
;; root  : the original root collection (for %context / %resource / resource-root)
(struct env (this index total vars externals root) #:transparent)

;; A Scope is a box holding an immutable hash: every env derived from one (a path's steps) shares
;; the box, so defineVariable's binding is visible for the remainder of the expression (which is
;; evaluation order in this collection model); a new scope frame is a new box over the same hash,
;; so opening one costs nothing (one opens at every `|` operand, argument and projected item — it
;; was a copy of a mutable table each time).
(define (make-env root #:root [root* root] #:externals [externals (hash)])
  (env root 0 '() (box (hash)) externals root*))

(define (env-with-item e item idx)
  (struct-copy env e [this (list item)] [index idx]))

(define (env-with-this e coll)
  (struct-copy env e [this coll]))

(define (env-with-total e total)
  (struct-copy env e [total total]))

;; a fresh scope frame seeded with the current bindings — used at `|` operands and per projection so
;; a defineVariable inside a branch does not leak to sibling branches or back out of the branch.
(define (env-clone-vars e)
  (struct-copy env e [vars (box (unbox (env-vars e)))]))

(define (env-var-bound? e name) (hash-has-key? (unbox (env-vars e)) name))

(define (env-define-var e name coll)
  (define scope (env-vars e))
  (set-box! scope (hash-set (unbox scope) name coll))     ; extend the current scope frame
  e)

;; resolve %name: user var, then caller external, then the well-known constants.
(define (env-lookup-external e name)
  (cond
    [(hash-ref (unbox (env-vars e)) name #f) => values]
    [(hash-ref (env-externals e) name #f) => values]
    [(string=? name "context") (env-root e)]
    [(string=? name "resource") (env-root e)]        ; nearest resource ≈ root for the TCK
    [(string=? name "rootResource") (env-root e)]
    [(string=? name "ucum") (list "http://unitsofmeasure.org")]
    [(string=? name "sct") (list "http://snomed.info/sct")]
    [(string=? name "loinc") (list "http://loinc.org")]
    [(regexp-match #rx"^vs-(.+)$" name)
     => (lambda (m) (list (string-append "http://hl7.org/fhir/ValueSet/" (cadr m))))]
    [(regexp-match #rx"^ext-(.+)$" name)
     => (lambda (m) (list (string-append "http://hl7.org/fhir/StructureDefinition/" (cadr m))))]
    [else #f]))

(module+ test
  (require rackunit)
  (define e (make-env '(root) #:externals (hash "x" '(1))))
  ;; a variable defined in a scope is seen by every env sharing it, not by the scope it was cloned from
  (define inner (env-clone-vars e))
  (env-define-var inner "v" '(2))
  (check-true (env-var-bound? (env-with-item inner 'item 0) "v"))
  (check-false (env-var-bound? e "v"))
  (check-equal? (env-lookup-external inner "v") '(2))
  ;; a clone starts with the bindings there were, and what it adds stays its own
  (define deeper (env-clone-vars inner))
  (env-define-var deeper "w" '(3))
  (check-true (env-var-bound? deeper "v"))
  (check-false (env-var-bound? inner "w"))
  ;; lookup order: a variable, an external, the well-known constants
  (check-equal? (env-lookup-external e "x") '(1))
  (check-equal? (env-lookup-external e "context") '(root))
  (check-equal? (env-lookup-external e "ucum") '("http://unitsofmeasure.org"))
  (check-equal? (env-lookup-external e "vs-administrative-gender") '("http://hl7.org/fhir/ValueSet/administrative-gender"))
  (check-false (env-lookup-external e "nope")))
