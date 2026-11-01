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
;; vars  : hash string -> collection  (defineVariable + seeded %context/%resource)
;; externals : hash string -> collection  (caller-supplied env vars / terminology)
;; root  : the original root collection (for %context / %resource / resource-root)
(struct env (this index total vars externals root) #:transparent)

;; vars is a MUTABLE hash shared across all derived envs, so defineVariable's binding is visible for
;; the remainder of the expression (which is evaluation order in this collection model).
(define (make-env root #:root [root* root] #:externals [externals (hash)])
  (env root 0 '() (make-hash) externals root*))

(define (env-with-item e item idx)
  (struct-copy env e [this (list item)] [index idx]))

(define (env-with-this e coll)
  (struct-copy env e [this coll]))

(define (env-with-total e total)
  (struct-copy env e [total total]))

;; a fresh scope frame seeded with the current bindings — used at `|` operands and per projection so
;; a defineVariable inside a branch does not leak to sibling branches or back out of the branch.
(define (env-clone-vars e)
  (struct-copy env e [vars (hash-copy (env-vars e))]))

(define (env-var-bound? e name) (hash-has-key? (env-vars e) name))

(define (env-define-var e name coll)
  (hash-set! (env-vars e) name coll)     ; mutate the current scope frame
  e)

;; resolve %name: user var, then caller external, then the well-known constants.
(define (env-lookup-external e name)
  (cond
    [(hash-ref (env-vars e) name #f) => values]
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
