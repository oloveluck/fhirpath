#lang racket/base

;;; cli-test.rkt — `raco fhirpath` as a person calls it, on three things it once got wrong:
;;;   * two expressions (`1 = 1; 2`) were a Racket stack trace: a parse error at the `;`, exit 1;
;;;   * no expression (`''`) was "unexpected no-tokens `#f`": "the expression is empty", exit 1
;;;     (an argument WAS given and its text is the problem, so it is reported like any other
;;;     syntax error — `--format json` included — where no argument at all is the usage, exit 2);
;;;   * a search parameter that serves several types (`Patient.name.given |
;;;     Practitioner.name.given`) was refused over a Patient with `no element
;;;     Patient.Practitioner`: the other type's branch contributes nothing.

(require rackunit racket/runtime-path racket/port racket/string racket/system json)

(define-runtime-path patient "inputs/patient-example.json")

;; fhirpath : string ... -> (values exit-code stdout stderr)   `raco fhirpath <args>`
(define (fhirpath . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code (parameterize ([current-output-port out] [current-error-port err])
                 (apply system*/exit-code (find-executable-path (find-system-path 'exec-file)) "-l" "raco" "fhirpath" args)))
  (values code (get-output-string out) (get-output-string err)))
(define (lines s) (string-split s "\n"))

(test-case "two expressions are a parse error at the `;`, not a stack trace"
  (define-values (code out err) (fhirpath "1 = 1; 2"))
  (check-equal? code 1)
  (check-equal? out "")
  (check-equal? (lines err)
                '("<expression>:1:6: fhirpath parse error: unexpected `;`: one expression is expected, this is 2"
                  " 1 | 1 = 1; 2"
                  "   |      ^"))
  (define-values (code2 out2 err2) (fhirpath "--format" "json" "1 = 1; 2"))
  (check-equal? code2 1)
  (define d (car (hash-ref (string->jsexpr out2) 'diagnostics)))
  (check-equal? (list (hash-ref d 'phase) (hash-ref d 'line) (hash-ref d 'column)) '("parse" 1 6)))

(test-case "an empty expression is said to be empty, in one line"
  (for ([e (in-list '("" "   " "// nothing"))])
    (define-values (code out err) (fhirpath e))
    (check-equal? code 1 e)
    (check-equal? out "" e)
    (check-equal? (lines err) '("<expression>: fhirpath parse error: the expression is empty") e))
  ;; no argument at all is the usage
  (define-values (code out err) (fhirpath))
  (check-equal? code 2)
  (check-regexp-match #rx"^usage: raco fhirpath" err))

(test-case "a search parameter of several types runs over one of them"
  (define-values (code out err) (fhirpath "Patient.name.given | Practitioner.name.given" (path->string patient)))
  (check-equal? code 0 err)
  (check-equal? (lines out) '("Peter" "James" "Jim"))
  ;; the same answer as the Patient's own branch
  (define-values (code1 out1 err1) (fhirpath "Patient.name.given.distinct()" (path->string patient)))
  (check-equal? out1 out)
  ;; still errors: another type's name outside a union, a name that is no type and no element
  (define-values (code2 out2 err2) (fhirpath "Practitioner.name.given" (path->string patient)))
  (check-equal? code2 1)
  (check-equal? (lines err2) '("<expression>: fhirpath run error: fhirpath: no element Patient.Practitioner"))
  (define-values (code3 out3 err3) (fhirpath "Patient.name.given | activ" (path->string patient)))
  (check-equal? code3 1)
  (check-equal? (lines err3) '("<expression>: fhirpath run error: fhirpath: no element Patient.activ (did you mean `active`?)")))
