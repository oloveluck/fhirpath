#lang racket/base

;;; guide-test.rkt — guide.md (the language guide agents read: fhir-mcp serves it as
;;; learn://fhirpath, the fhirpath skill points to it) keeps its promises: every line of a ```fhirpath
;;; block parses, and the worked examples' transcript is exactly what the command line prints over
;;; the two resources the guide shows.

(require rackunit racket/runtime-path racket/file racket/port racket/string racket/list json
         (only-in fhirpath/parser/parse parse-fhirpath)
         (only-in fhirpath/workspace guide)
         (only-in fhirpath/cli evaluate))

(define-runtime-path guide-file "../guide.md")
(define text (file->string guide-file))

(test-case "the guide the workspace serves is this file"
  (check-equal? guide text))

(test-case "every cheat-sheet expression parses"
  (define lines (append* (for/list ([b (regexp-match* #px"```fhirpath\n(.*?)```" text #:match-select cadr)])
                           (filter (lambda (l) (not (string=? (string-trim l) ""))) (string-split b "\n")))))
  (check-true (>= (length lines) 8))
  (for ([l (in-list lines)])
    (check-not-exn (lambda () (parse-fhirpath l)) l)))

;; the resources: a ```json block right after a line naming its file (`patient.json`:)
(define resources
  (for/list ([m (regexp-match* #px"`([a-z]+[.]json)`:\n```json\n(.*?)```" text #:match-select values)])
    (cons (cadr m) (caddr m))))

(test-case "the worked examples print what the guide says"
  (check-equal? (map car resources) '("patient.json" "bp.json"))
  (define dir (make-temporary-directory))
  (for ([r (in-list resources)])
    (check-true (hash? (string->jsexpr (cdr r))) (car r))
    (call-with-output-file (build-path dir (car r)) (lambda (o) (write-string (cdr r) o))))
  ;; the transcript: `$ fhirpath "<expr>" <file>` lines, each followed by its output lines
  (define transcript
    (for/first ([b (regexp-match* #px"```\n(\\$ fhirpath .*?)```" text #:match-select cadr)]) b))
  (define steps
    (let loop ([ls (string-split transcript "\n")] [acc '()])
      (match-define-values (cmd rest) (values (car ls) (cdr ls)))
      (define-values (out more) (splitf-at rest (lambda (l) (not (string-prefix? l "$ ")))))
      (define next (cons (cons cmd out) acc))
      (if (null? more) (reverse next) (loop more next))))
  (check-true (>= (length steps) 10))
  (for ([s (in-list steps)])
    (define m (regexp-match #px"^\\$ fhirpath \"(.*)\" ([a-z]+[.]json)$" (car s)))
    (check-true (and m #t) (car s))
    (when m
      (define printed (with-output-to-string (lambda () (evaluate (cadr m) (build-path dir (caddr m)) "r5"))))
      (check-equal? (string-split printed "\n") (cdr s) (car s))))
  (delete-directory/files dir))

(require (only-in racket/match match-define-values))
