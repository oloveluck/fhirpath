#lang racket/base

;;; contracts-test.rkt — contracts at the package's boundary, none inside it.
;;;
;;; Each engine module provides its functions with `contract-out` for callers outside the package
;;; (cql, fml, fsh, sql-on-fhir, fhir-validation, fhir-server) and, through
;;; `#:unprotected-submodule unchecked`, the same names without contracts for the engine's own
;;; modules: the wrappers on eval -> navigate/functions/operators/types/… calls were a third to two
;;; fifths of evaluating an expression. Two things are held here:
;;;   * a bad argument from outside is still refused by the contract, blaming the caller;
;;;   * no engine module imports another engine module's contracted interface (a new require of
;;;     "navigate.rkt" instead of (submod "navigate.rkt" unchecked) would bring the wrappers back).

(require rackunit racket/list syntax/modcollapse
         (only-in racket/contract exn:fail:contract:blame?)
         (only-in fhirpath/error fp-error)
         (only-in fhirpath/value item->system)
         (only-in fhirpath/decimal decimal-add)
         (only-in fhirpath/temporal-ops parse-fp-date)
         (only-in fhirpath/environment env-with-item)
         (only-in fhirpath/quantity make-quantity)
         (only-in fhirpath/equality coll-equal)
         (only-in fhirpath/operators apply-binop)
         (only-in fhirpath/functions apply-function)
         (only-in fhirpath/navigate navigate)
         (only-in fhirpath/types known-type?)
         (only-in fhirpath/regex regex-matches?)
         (only-in fhirpath/ucum ucum-valid?)
         (only-in fhirpath/analyze analyze))

;; the engine's modules: each provides an `unchecked` submodule
(define engine-modules
  '("error" "value" "decimal" "temporal-ops" "environment" "quantity" "equality" "operators"
    "functions" "navigate" "types" "regex" "ucum" "analyze"))

;; refused-from-outside? : (-> Any) -> Boolean   does the call raise a contract violation that blames
;; this module (the caller), not the engine?
(define (refused-from-outside? thunk)
  (with-handlers ([exn:fail:contract:blame?
                   (lambda (e) (regexp-match? #rx"blaming: [^\n]*contracts-test[.]rkt" (exn-message e)))])
    (thunk)
    #f))

(test-case "a bad argument from outside the package is refused, blaming the caller"
  (check-true (refused-from-outside? (lambda () (fp-error "semantic" "x"))))
  (check-true (refused-from-outside? (lambda () (decimal-add 1 2))))
  (check-true (refused-from-outside? (lambda () (parse-fp-date 2024))))
  (check-true (refused-from-outside? (lambda () (env-with-item 'not-an-env 1 0))))
  (check-true (refused-from-outside? (lambda () (make-quantity 1 "mg"))))
  (check-true (refused-from-outside? (lambda () (coll-equal 1 2))))
  (check-true (refused-from-outside? (lambda () (apply-binop "+" '() '()))))
  (check-true (refused-from-outside? (lambda () (apply-function 'count '() '() #f 'not-a-procedure '()))))
  (check-true (refused-from-outside? (lambda () (navigate 'not-a-list 'name))))
  (check-true (refused-from-outside? (lambda () (known-type? "Patient"))))
  (check-true (refused-from-outside? (lambda () (regex-matches? 1 "a"))))
  (check-true (refused-from-outside? (lambda () (ucum-valid? 5))))
  (check-true (refused-from-outside? (lambda () (analyze #f "Patient"))))
  ;; a good argument still answers as before
  (check-equal? (item->system 5) 5))

;; module-imports : String -> (listof ModulePath)   the phase-0 imports of an engine module, each
;; collapsed against the module's own file: an absolute path, or (submod path unchecked)
(define (module-imports name)
  (define file (build-path (collection-path "fhirpath") (string-append name ".rkt")))
  (define mp `(file ,(path->string file)))
  (dynamic-require mp (void))
  (define imports (module->imports mp))
  (for/list ([mpi (in-list (cond [(assv 0 imports) => cdr] [else '()]))])
    (collapse-module-path-index mpi file)))

;; engine-file : ModulePath -> String|#f   the engine module a plain (not submodule) import is, if any
(define (engine-file mp)
  (and (path? mp)
       (let-values ([(dir file _) (split-path mp)])
         (define stem (regexp-replace #rx"[.]rkt$" (path->string file) ""))
         (and (member stem engine-modules)
              (equal? (simplify-path dir) (simplify-path (collection-path "fhirpath")))
              stem))))

(test-case "the engine's modules import one another without contracts"
  (for ([m (in-list (cons "eval" engine-modules))])
    (define checked (filter-map engine-file (module-imports m)))
    (check-equal? checked '() (format "~a.rkt imports the contracted ~a" m checked))))

(test-case "every engine module offers the unchecked submodule"
  (for ([m (in-list engine-modules)])
    (define mp `(submod (lib ,(string-append "fhirpath/" m ".rkt")) unchecked))
    (check-true (module-declared? mp #t) m)))
