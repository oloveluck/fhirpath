#lang racket/base

;;; descent-test.rkt — the recursive-descent reader held to the published grammar.
;;;
;;; parse.rkt reads FHIRPath with descent.rkt and hands to brag (grammar.rkt, from fhirpath.g4)
;;; only what descent does not take. This test keeps brag as the reference: for every text, the
;;; production reading (`parse-fhirpath-program`) and brag's alone (`parse-fhirpath-program/brag`)
;;; must give the same AST, or the same error, word for word and at the same place. Texts:
;;;   - every expression of HL7's suites (R4 and R5), valid and invalid;
;;;   - every FHIRPath expression of the core packages and the guides in fhir-spec: each
;;;     SearchParameter's `expression` and each StructureDefinition constraint's;
;;;   - generated: expressions grown from the grammar's productions (every operator, literal and
;;;     postfix form, keywords as names, nesting), and token soups that are mostly not FHIRPath —
;;;     where a reader that accepts too much would show.
;;; It also counts how much descent read itself (the rest went to brag): a reader that refused
;;; everything would pass the agreement and buy nothing, so its share has a floor.

(require rackunit racket/list racket/file racket/string json
         (only-in fhir/package read-package)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostic->string)
         (only-in brag/support token-struct? token-struct-skip? srcloc-token? srcloc-token-token)
         (only-in "../parser/parse.rkt" parse-fhirpath-program parse-fhirpath-program/brag)
         (only-in "../parser/lexer.rkt" tokenize)
         (only-in "../parser/descent.rkt" descend-program))

;; ---- one text, both ways ------------------------------------------------------------------------
;; An Outcome is (list 'ok (listof AST)) | (list 'error String)
(define (outcome read str)
  (with-handlers ([exn:fail:diagnostic? (lambda (e) (list 'error (diagnostic->string (exn:fail:diagnostic-diagnostic e))))]
                  [exn:fail? (lambda (e) (list 'error (exn-message e)))])
    (list 'ok (read str))))

;; descended? : String -> Boolean   descent read the text itself
(define (descended? str)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define next (tokenize (open-input-string str)))
    (define toks (let loop () (define t (next)) (if (void? t) '()
                                                     (let ([k (if (srcloc-token? t) (srcloc-token-token t) t)])
                                                       (if (and (token-struct? k) (not (token-struct-skip? k))) (cons k (loop)) (loop))))))
    (and (descend-program toks) #t)))

;; A Tally is (tally texts brag-ok descended disagreements)
(struct tally (texts brag-ok descended disagreements) #:transparent)
;; compare : (listof String) -> Tally
(define (compare texts)
  (for/fold ([t (tally 0 0 0 '())]) ([s (in-list (remove-duplicates texts))])
    (define production (outcome parse-fhirpath-program s))
    (define reference (outcome parse-fhirpath-program/brag s))
    (tally (add1 (tally-texts t))
           (+ (tally-brag-ok t) (if (eq? (car reference) 'ok) 1 0))
           (+ (tally-descended t) (if (descended? s) 1 0))
           (if (equal? production reference) (tally-disagreements t)
               (cons (list s production reference) (tally-disagreements t))))))

(define (report label t)
  (printf "  ~a: ~a texts, ~a read by the grammar, ~a by descent itself, ~a disagreements\n"
          label (tally-texts t) (tally-brag-ok t) (tally-descended t) (length (tally-disagreements t)))
  (for ([d (in-list (take (tally-disagreements t) (min 5 (length (tally-disagreements t)))))])
    (printf "    ~s\n      production ~s\n      grammar    ~s\n" (car d) (cadr d) (caddr d)))
  (check-equal? (tally-disagreements t) '() label))

;; ---- the HL7 suites -----------------------------------------------------------------------------
(define (unescape-xml s)
  (for/fold ([s s]) ([e (in-list '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'") ("&amp;" . "&")))])
    (string-replace s (car e) (cdr e))))
(define suite-texts
  (for*/list ([f (in-list '("tests-fhir-r4.xml" "tests-fhir-r5.xml"))]
              [m (in-list (regexp-match* #px"<expression[^>]*>([^<]*)</expression>"
                                         (file->string (collection-file-path (string-append "tests/vendor/" f) "fhirpath"))
                                         #:match-select cadr))])
    (unescape-xml m)))
(define suites (compare suite-texts))
(report "HL7 suites" suites)
;; every suite expression the grammar reads, descent reads too (it gives up only on the forms the
;; grammar itself reads two ways, `5 is`, none of which the suites hold)
(check-equal? (tally-descended suites) (tally-brag-ok suites) "descent reads every suite expression the grammar reads")

;; ---- the packages ---------------------------------------------------------------------------------
;; package-expressions : path -> (listof String)   SearchParameter and constraint expressions
(define (package-expressions tgz)
  (define-values (_m resources examples)
    (read-package tgz #:select (lambda (name) (regexp-match? #rx"(SearchParameter|StructureDefinition)-" name))))
  (define (walk v)
    (cond [(hash? v) (append (let ([e (hash-ref v 'expression #f)]) (if (and (string? e) (or (hash-has-key? v 'key) (hash-has-key? v 'base))) (list e) '()))
                             (append-map walk (hash-values v)))]
          [(list? v) (append-map walk v)]
          [else '()]))
  (append-map (lambda (r) (walk (if (pair? r) (cdr r) r))) (append resources examples)))
(define package-names
  '("hl7.fhir.r4.core.tgz" "hl7.fhir.r5.core.tgz" "hl7.fhir.us.core.tgz" "hl7.fhir.uv.ips.tgz"
    "hl7.fhir.us.mcode-4.0.0.tgz" "hl7.fhir.uv.crmi-2.0.0.tgz"))
(define package-texts
  (append* (for*/list ([n (in-list package-names)] [p (in-value (spec-package-path n #:optional? #t))] #:when p)
             (package-expressions p))))
(cond
  [(null? package-texts) (printf "  packages: fhir-spec not installed, skipped\n")]
  [else
   (define packages (compare package-texts))
   (report "packages" packages)
   (check-true (> (tally-texts packages) 2000))
   (check-equal? (tally-descended packages) (tally-brag-ok packages) "descent reads every package expression the grammar reads")])

;; ---- generated -------------------------------------------------------------------------------------
;; grown from the grammar: a pseudo-random generator with a fixed seed, so a failure repeats
(define rng (make-pseudo-random-generator))
(parameterize ([current-pseudo-random-generator rng]) (random-seed 20261114))
(define (pick xs) (list-ref xs (random (length xs) rng)))
(define (chance n) (< (random 100 rng) n))
(define names '("name" "given" "Patient" "value" "code" "as" "is" "in" "contains" "`div`" "_x1" "System"))
(define (gen-literal)
  (pick (list "1" "0.50" "'text'" "'a\\'b'" "true" "false" "@2014-01-25" "@2014-01-25T14:30:00Z" "@T12:00"
              "{}" "4 'mg'" "5 days" "1 year" "2.5 'cm'" "%context" "%`us-zip`" "$this" "$index" "$total")))
(define binary-ops '("implies" "or" "xor" "and" "in" "contains" "=" "!=" "~" "!~" "<=" ">=" "<" ">" "|" "+" "-" "&" "*" "/" "div" "mod"))
(define (gen-expr depth)
  (cond
    [(or (<= depth 0) (chance 25)) (if (chance 50) (gen-literal) (pick names))]
    [else
     (case (random 9 rng)
       [(0 1) (format "~a ~a ~a" (gen-expr (sub1 depth)) (pick binary-ops) (gen-expr (sub1 depth)))]
       [(2) (format "~a.~a" (gen-expr (sub1 depth)) (pick (append names '("$this" "$index"))))]
       [(3) (format "~a.~a(~a)" (gen-expr (sub1 depth)) (pick '("where" "exists" "as" "is" "ofType" "iif" "contains"))
                    (string-join (for/list ([_ (in-range (random 3 rng))]) (gen-expr (sub1 depth))) ", "))]
       [(4) (format "~a[~a]" (gen-expr (sub1 depth)) (gen-expr (sub1 depth)))]
       [(5) (format "(~a)" (gen-expr (sub1 depth)))]
       [(6) (format "~a~a" (pick '("-" "+")) (gen-expr (sub1 depth)))]
       [(7) (format "~a ~a ~a" (gen-expr (sub1 depth)) (pick '("is" "as")) (pick '("Quantity" "System.Integer" "FHIR.string" "is")))]
       [else (format "~a(~a)" (pick '("today" "now" "iif" "where")) (gen-expr (sub1 depth)))])]))
;; a token soup: the pieces of FHIRPath, in no particular order
(define pieces (append binary-ops names '("." "(" ")" "[" "]" "," ";" "{" "}" "1" "'s'" "@2014" "%x" "$this" "5" "days" "'mg'" "is" "as")))
(define (gen-soup) (string-join (for/list ([_ (in-range (add1 (random 7 rng)))]) (pick pieces)) " "))

(define generated (compare (for/list ([_ (in-range 3000)]) (gen-expr 4))))
(report "generated expressions" generated)
(check-true (> (tally-brag-ok generated) 1000) "the generator writes mostly FHIRPath")
(define soup-texts (for/list ([_ (in-range 3000)]) (gen-soup)))
(define soups (compare soup-texts))
(report "token soups" soups)
;; what the grammar reads and descent leaves to it: only a number with a keyword where its unit may
;; stand (`5 is`, `1 in`), the one form the grammar reads two ways
(check-equal? (for/list ([s (in-list (remove-duplicates soup-texts))]
                         #:when (and (eq? (car (outcome parse-fhirpath-program/brag s)) 'ok) (not (descended? s)))
                         #:unless (regexp-match? #px"(^|\\s)[0-9]+ (is|as|in|contains)(\\s|$)" s))
                s)
              '()
              "descent leaves to brag only a number followed by a keyword")
