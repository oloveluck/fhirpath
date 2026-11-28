#lang racket/base

;;; ucum/essence.rkt — UCUM's essence file (ucum-essence.xml) read into plain data.
;;;
;;; ucum.rkt reads it when it is COMPILED and keeps the answer as a literal, so a process builds its
;;; unit tables from a datum instead of parsing 200 KB of XML the first time a unit is checked (90 ms
;;; with the XML library's load, in every validation that meets a Quantity).
;;;
;;; Data definition
;;;   An Essence is (list Prefixes PrefixNames BaseUnits UnitNames Units), each an association list
;;;   sorted by code:
;;;     Prefixes     (code . exact factor)            "m" -> 1/1000
;;;     PrefixNames  (code . name)                    "m" -> "milli"
;;;     BaseUnits    (code . dimension letter)        "m" -> "L"
;;;     UnitNames    (code . name), units and base units both
;;;     Units        (code . (cons factor unit-text)) or (code . special): a unit whose value the
;;;                  essence states as a factor of another unit, or one it defines by a function

(require racket/string racket/list xml)
(provide essence-of)

;; essence-of : path -> Essence
(define (essence-of file)
  (define doc (call-with-input-file file read-xml))
  (define prefixes '()) (define prefix-names '()) (define base-units '()) (define unit-names '()) (define units '())
  (define-syntax-rule (push! l x) (set! l (cons x l)))
  (let walk ([x (document-element doc)])
    (when (element? x)
      (case (element-name x)
        [(prefix) (let ([code (attr x 'Code)] [v (child x 'value)] [n (child x 'name)])
                    (when (and code v) (push! prefixes (cons code (parse-ucum-num (attr v 'value)))))
                    (when (and code n) (push! prefix-names (cons code (element-text n)))))]
        [(base-unit) (let ([code (attr x 'Code)] [d (attr x 'dim)] [n (child x 'name)])
                       (when (and code d) (push! base-units (cons code d)))
                       (when (and code n) (push! unit-names (cons code (element-text n)))))]
        [(unit) (let ([code (attr x 'Code)] [v (child x 'value)] [n (child x 'name)])
                  (when (and code n) (push! unit-names (cons code (element-text n))))
                  (when code
                    (define unit-ref (and v (attr v 'Unit)))
                    (define num (let ([val (and v (attr v 'value))]) (and val (parse-ucum-num val))))
                    (push! units (cons code (if (and unit-ref num) (cons num unit-ref) 'special)))))]
        [else (void)])
      (for ([c (element-content x)]) (walk c))))
  ;; a code stated twice keeps its LAST statement, as setting a table in document order did (the
  ;; lists are newest first, and remove-duplicates keeps the first it meets)
  (define (table l) (sort (remove-duplicates l #:key car) string<? #:key car))
  (list (table prefixes) (table prefix-names) (table base-units) (table unit-names) (table units)))

(define (attr e name) (for/or ([a (element-attributes e)]) (and (eq? (attribute-name a) name) (attribute-value a))))
(define (child e tag)
  (for/or ([c (element-content e)] #:when (and (element? c) (eq? (element-name c) tag))) c))

;; element-text : element -> string   the character data directly inside it, entities resolved
;; (`&#232;` in "ampère")
(define (element-text e)
  (apply string-append
         (for/list ([c (in-list (element-content e))] #:when (or (pcdata? c) (entity? c)))
           (cond [(pcdata? c) (pcdata-string c)]
                 [else (let ([t (entity-text c)])
                         (if (exact-integer? t) (string (integer->char t)) (format "&~a;" t)))]))))

;; parse-ucum-num : string -> exact rational | #f   "6.0221367e23", "1e-3", "0.01"
(define (parse-ucum-num s)
  (define m (regexp-match #px"^([+-]?[0-9]*[.]?[0-9]+)(?:[eE]([+-]?[0-9]+))?$" (string-trim s)))
  (and m (let* ([mant (list-ref m 1)] [ex (let ([e (list-ref m 2)]) (if e (string->number e) 0))]
                [dm (regexp-match #px"^([+-]?)([0-9]*)(?:[.]([0-9]+))?$" mant)])
           (and dm
                (let* ([sign (if (equal? (list-ref dm 1) "-") -1 1)]
                       [ip (list-ref dm 2)] [fp (or (list-ref dm 3) "")]
                       [digits (string->number (string-append (if (equal? ip "") "0" ip) fp))]
                       [scale (string-length fp)])
                  (* sign digits (expt 10 (- ex scale))))))))

(module+ test
  (require rackunit racket/runtime-path)
  (define-runtime-path file "ucum-essence.xml")
  (define e (essence-of file))
  (check-equal? (cdr (assoc "m" (car e))) 1/1000)
  (check-equal? (cdr (assoc "m" (cadr e))) "milli")
  (check-equal? (cdr (assoc "m" (caddr e))) "L")
  (check-equal? (cdr (assoc "A" (cadddr e))) "ampère")
  (check-equal? (cdr (assoc "Cel" (list-ref e 4))) 'special)
  (check-equal? (parse-ucum-num "6.0221367e23") 602213670000000000000000)
  (check-equal? (parse-ucum-num "1e-3") 1/1000))
