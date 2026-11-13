#lang racket/base

;;; eval.rkt — the FHIRPath tree-walking evaluator (everything is a collection).
;;;
;;; Navigation is schema-driven (navigate.rkt); operators live in operators.rkt; functions in
;;; functions.rkt (receiving `fp-eval` for lazy args + per-item $this binding). and/or short-circuit
;;; so an unreached erroring branch doesn't fire.

(require racket/contract racket/match
         "error.rkt"
         "parser/ast.rkt"
         "environment.rkt"
         (only-in "navigate.rkt" navigate)
         (only-in "value.rkt" item->system fp-long)
         (only-in "operators.rkt" apply-binop apply-unary)
         (only-in "types.rkt" item-is? item-of-type? known-type?)
         (only-in "analyze.rkt" analyze union-branch)
         (only-in fhir/model/node fhir? fhir-type)
         (only-in "functions.rkt" apply-function)
         (only-in "decimal.rkt" parse-decimal integer->decimal)
         (only-in "quantity.rkt" make-quantity)
         (only-in "temporal-ops.rkt" parse-fp-date parse-fp-datetime parse-fp-time calendar-problem))

(provide (contract-out
          [eval-fhirpath (->* (any/c list?) (#:externals hash? #:strict? boolean?) list?)]
          [fp-eval       (-> any/c list? env? list?)]))

(define (eval-fhirpath ast root #:externals [ext (hash)] #:strict? [strict? #t])
  ;; static semantic pass, seeded with the input resource type (when the root is a single node)
  (when (and (pair? root) (null? (cdr root)) (fhir? (car root)))
    (analyze ast (fhir-type (car root)) #:strict? strict?))
  (fp-eval ast root (make-env root #:externals ext)))

(define (fp-eval node coll e)
  (match node
    [(fp-lit v) (lit->coll v)]
    [(fp-var '$this) (env-this e)]
    [(fp-var '$index) (list (env-index e))]
    [(fp-var '$total) (env-total e)]
    [(fp-var k) (fp-error 'semantic (format "unknown variable ~a" k))]
    [(fp-extern name)
     (or (env-lookup-external e name) (fp-error 'semantic (format "unknown environment variable %~a" name)))]
    [(fp-invoke #f name) (navigate coll name)]
    [(fp-invoke target name) (navigate (fp-eval target coll e) name)]
    [(fp-index target idx)
     (define t (fp-eval target coll e))
     (define iv (item->system-1 (fp-eval idx coll e)))
     (if (and (exact-integer? iv) (>= iv 0) (< iv (length t))) (list (list-ref t iv)) '())]
    [(fp-unop op operand) (apply-unary op (fp-eval operand coll e))]
    [(fp-typeop op operand type) (eval-typeop op (fp-eval operand coll e) type)]
    [(fp-binop op l r) (eval-binop op l r coll e)]
    [(fp-call target name args)
     (define subject (if target (fp-eval target coll e) coll))
     (apply-function name subject args e fp-eval coll)]
    [_ (fp-error 'semantic (format "cannot evaluate ~s" node))]))

;; and/or short-circuit; everything else is eager
(define (eval-binop op l r coll e)
  (case op
    [(and)
     (define lv (fp-eval l coll e))
     (if (equal? lv (list #f)) (list #f) (apply-binop (quote and) lv (fp-eval r coll e)))]
    [(or)
     (define lv (fp-eval l coll e))
     (if (equal? lv (list #t)) (list #t) (apply-binop (quote or) lv (fp-eval r coll e)))]
    ;; each operand gets its own variable scope (defineVariable must not leak across |); an operand
    ;; whose head names a resource type selects the input of that type (analyze.rkt `union-branch`:
    ;; over a Patient, `Patient.name.given | Practitioner.name.given` is the Patient's given names)
    [(union)
     (apply-binop 'union (fp-eval (union-branch l) coll (env-clone-vars e))
                  (fp-eval (union-branch r) coll (env-clone-vars e)))]
    [else (apply-binop op (fp-eval l coll e) (fp-eval r coll e))]))

;; is/as as operators: `expr is Type` -> boolean ; `expr as Type` -> filtered
(define (eval-typeop op coll type)
  (case op
    [(is) (cond [(null? coll) '()]
                [(not (null? (cdr coll))) (fp-error 'execution "is on multiple items")]
                [(known-type? type) (list (item-is? (car coll) type))]
                [else (list #f)])]                                  ; unresolvable type -> false
    [(as) (unless (known-type? type) (fp-error 'semantic (format "unknown type ~a" type)))
          (filter (lambda (x) (item-of-type? x type)) coll)]))

(define (item->system-1 coll)
  (cond [(null? coll) 'empty] [(null? (cdr coll)) (item->system (car coll))]
        [else (fp-error 'execution "expected a singleton index")]))

;; literal -> collection
(define (lit->coll v)
  (cond
    [(eq? v 'empty) '()]
    [(exact-integer? v) (list v)]
    [(boolean? v) (list v)]
    [(string? v) (list v)]
    [(dec-lit? v) (list (parse-decimal (dec-lit-lexeme v)))]
    [(qty-lit? v) (list (make-quantity (num->dec (qty-lit-number v)) (qty-lit-unit v)))]
    [(date-lit? v) (calendar-checked (parse-fp-date (date-lit-str v)) (date-lit-str v))]
    [(datetime-lit? v) (calendar-checked (parse-fp-datetime (datetime-lit-str v)) (datetime-lit-str v))]
    [(time-lit? v) (let ([m (parse-fp-time (time-lit-str v))]) (if m (list m) '()))]
    [else (list v)]))

;; calendar-checked : moment|#f string -> (listof moment)   a literal naming a day its month lacks
;; (@1940-02-30) is an error, not an empty collection
(define (calendar-checked m text)
  (cond [(calendar-problem m)
         => (lambda (why) (fp-error 'semantic (format "@~a names no day: ~a" text why)))]
        [m (list m)]
        [else '()]))

(define (num->dec n) (if (dec-lit? n) (parse-decimal (dec-lit-lexeme n)) (integer->decimal n)))

(module+ test
  (require rackunit (only-in fhir/r5/schemas)
           (only-in fhir/serialize/from-json from-json)
           (only-in fhir/result/result ok-value)
           (only-in "parser/parse.rkt" parse-fhirpath))
  (define (node js) (ok-value (from-json js #:lenient? #t)))
  (define patient
    (node (hasheq 'resourceType "Patient" 'id "p1" 'gender "male"
                  'name (list (hasheq 'family "Chalmers" 'given (list "Peter" "James"))))))
  (define practitioner
    (node (hasheq 'resourceType "Practitioner" 'id "d1" 'name (list (hasheq 'given (list "Adam"))))))
  ;; run : string node -> (listof system value) | the error's message
  (define (run s root)
    (with-handlers ([exn:fp? exn-message])
      (map item->system (eval-fhirpath (parse-fhirpath s) (list root)))))
  ;; a search parameter that serves several types is a union of paths, each rooted at its type: over
  ;; one resource the branches of the other types contribute nothing (it was `no element
  ;; Patient.Practitioner`)
  (check-equal? (run "Patient.name.given | Practitioner.name.given" patient) '("Peter" "James"))
  (check-equal? (run "Patient.name.given | Practitioner.name.given" practitioner) '("Adam"))
  (check-equal? (run "Practitioner.name.given | Patient.name.family | RelatedPerson.name.family" patient) '("Chalmers"))
  (check-equal? (run "(Practitioner.name.where(given.exists())[0] as HumanName).given | Patient.gender" patient) '("male"))
  ;; the rest of an empty branch still runs: exists() of nothing is false, not nothing
  (check-equal? (run "Practitioner.name.exists() | Patient.name.exists()" patient) '(#f #t))
  (check-equal? (run "(Patient.name | Practitioner.name).given.count()" patient) '(2))
  ;; an ancestor's name is the input itself, in a union as elsewhere
  (check-equal? (run "Resource.id | Patient.gender" patient) '("p1" "male"))
  ;; what stays an error: another type's name outside a union (HL7's testSimpleWithWrongContext), a
  ;; misspelt element in the other type's branch, a name that is neither a type nor an element
  (check-equal? (run "Practitioner.name.given" patient) "fhirpath: no element Patient.Practitioner")
  (check-equal? (run "Encounter.name.given" patient) "fhirpath: no element Patient.Encounter")
  (check-equal? (run "Patient.name.given | Practitioner.nmae" patient)
                "fhirpath: no element Practitioner.nmae (did you mean `name`?)")
  (check-equal? (run "Patient.name.given | Practicioner.name.given" patient)
                "fhirpath: no element Patient.Practicioner")
  (check-equal? (run "activ" patient) "fhirpath: no element Patient.activ (did you mean `active`?)")
  ;; without the static pass (several inputs: no one type to seed it with) navigation agrees
  (check-equal? (map item->system (eval-fhirpath (parse-fhirpath "Patient.name.given | Practitioner.name.given")
                                                 (list patient practitioner)))
                '("Peter" "James" "Adam"))
  (check-exn #rx"Practitioner is not the context"
             (lambda () (eval-fhirpath (parse-fhirpath "Practitioner.name.given") (list patient patient)))))
