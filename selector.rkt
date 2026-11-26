#lang racket/base

;;; selector.rkt — FHIRPath 3.0.0's instance selector: `Coding { system: 'x', code: gender }`,
;;; `FHIR.Identifier { value: 'N1' }`, `Period { : }`, `code { value: 'final' }`.
;;;
;;; "Creates a new object of type «typename» … Any elements listed are set as children on the
;;; newly created object. If any of child element's value is evaluated to an empty collection, then
;;; that element will not be added." The type is a FHIR type of the process's tower (an unqualified
;;; name is the model's, `FHIR.` names it explicitly), or System.Quantity. A FHIR type is built as its
;;; FHIR JSON and read by the strict reader (fhir/serialize/from-json), so the object is held to the
;;; type: an element the type lacks, a value that is no value of its element, or several values for
;;; an element of one ("If not the engine MAY throw an error") is an execution error. A choice
;;; element (`value`) takes the member its value's type names (`valueQuantity`). A FHIR primitive is
;;; built from its `value` by the registry's constructor of that type.
;;;
;;; An Element is (cons name Coll): an element's name and the values its expression gave.

(require racket/list racket/string
         (submod "error.rkt" unchecked)
         (only-in fhir/model/node fhir? fhir-type)
         (only-in fhir/model/registry constructor-of)
         (only-in fhir/model/schema schema-field-name schema-field-max)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/read-json jnum jnum? jnum-lexeme)
         (only-in fhir/result/result ok? ok-value err? err-error validation-error? validation-error-message)
         (only-in (submod "value.rkt" unchecked) item->system item-fhir-type prim-item?
                  fp-quantity fp-quantity? fp-quantity-value fp-quantity-unit fp-quantity-calendar?
                  fp-long? fp-long-v)
         (only-in (submod "decimal.rkt" unchecked) fp-decimal? decimal->string integer->decimal)
         (only-in (submod "temporal-ops.rkt" unchecked) moment? moment-kind moment->string)
         (only-in (submod "types.rkt" unchecked) fhir-ancestors known-type? primitive-codes)
         (only-in (submod "navigate.rkt" unchecked) all-schema-fields choice-members-for))

(provide new-instance)

;; new-instance : Symbol (listof Element) -> Coll   the object a selector names, as a collection of one
(define (new-instance type elements)
  (define given (filter (lambda (el) (pair? (cdr el))) elements))   ; an empty value adds nothing
  (define name (symbol->string type))
  (cond
    [(member name '("System.Quantity")) (list (system-quantity given))]
    [else
     (define t (string->symbol (if (string-prefix? name "FHIR.") (substring name 5) name)))
     (unless (known-type? t)
       (fp-error 'semantic (format "~a { … }: there is no type ~a" type type)))
     (if (memq t primitive-codes)
         (list (primitive-instance t given))
         (list (complex-instance t given)))]))

;; ---- FHIR types --------------------------------------------------------------------------------
(define (complex-instance t elements)
  (define fields (for/hasheq ([f (in-list (all-schema-fields t))]) (values (schema-field-name f) f)))
  (define resource? (and (memq 'Resource (fhir-ancestors t)) #t))
  (define json
    (for/fold ([js (if resource? (hasheq 'resourceType (symbol->string t)) (hasheq))])
              ([el (in-list elements)])
      (define-values (key many?) (element-key t fields (car el) (cdr el)))
      (define values (map item->json (cdr el)))
      (when (and (not many?) (pair? (cdr values)))
        (fp-error 'execution (format "~a { … }: ~a takes one value, and was given ~a" t (car el) (length values))))
      (hash-set js key (if many? values (car values)))))
  (define read (from-json json #:type t))
  (if (ok? read)
      (ok-value read)
      (fp-error 'execution (format "~a { … } is no ~a: ~a" t t (reason (err-error read))))))

;; element-key : Symbol (hasheq Symbol -> schema-field) Symbol Coll -> (values Symbol Boolean)
;; the JSON key an element is written under, and whether it repeats
(define (element-key t fields name coll)
  (cond
    [(hash-ref fields name #f) => (lambda (f) (values name (many? f)))]
    [(pair? (choice-members-for t name))
     (define members (choice-members-for t name))
     (define suffix (type-suffix (car coll)))
     (define key (and suffix (string->symbol (string-append (symbol->string name) suffix))))
     (if (and key (memq key members))
         (values key #f)
         (fp-error 'execution (format "~a { … }: ~a takes ~a, not a value of type ~a" t name
                                      (string-join (map (lambda (m) (substring (symbol->string m) (string-length (symbol->string name)))) members) ", ")
                                      (or suffix "unknown"))))]
    [else (fp-error 'semantic (format "~a { … }: ~a has no element ~a" t t name))]))
(define (many? f) (let ([m (schema-field-max f)]) (or (eq? m '*) (and (number? m) (> m 1)))))

;; type-suffix : Item -> String|#f   the FHIR type name a choice key ends in (valueQuantity, valueString)
(define (type-suffix it)
  (define (capitalized s) (string-append (string-upcase (substring s 0 1)) (substring s 1)))
  (cond
    [(fhir? it) (capitalized (symbol->string (fhir-type it)))]
    [(prim-item? it) (let ([t (item-fhir-type it)]) (and t (capitalized (symbol->string t))))]
    [(string? it) "String"]
    [(boolean? it) "Boolean"]
    [(exact-integer? it) "Integer"]
    [(fp-long? it) "Integer64"]
    [(fp-decimal? it) "Decimal"]
    [(fp-quantity? it) "Quantity"]
    [(moment? it) (case (moment-kind it) [(date) "Date"] [(dateTime) "DateTime"] [else "Time"])]
    [else #f]))

;; item->json : Item -> jsexpr   a FHIRPath value as the FHIR JSON of the element it is set on
(define (item->json it)
  (cond
    [(fhir? it) (node->jsexpr it)]
    [(prim-item? it) (item->json (item->system it))]
    [(or (string? it) (boolean? it) (exact-integer? it)) it]
    [(fp-long? it) (fp-long-v it)]
    [(fp-decimal? it) (jnum (decimal->string it))]
    [(moment? it) (moment-text it)]
    [(fp-quantity? it)
     (define unit (fp-quantity-unit it))
     (define code (if (fp-quantity-calendar? it) (calendar-code unit) unit))
     (hasheq 'value (jnum (decimal->string (fp-quantity-value it))) 'unit unit
             'system "http://unitsofmeasure.org" 'code code)]
    [else (fp-error 'execution (format "~a cannot be set on an element of an instance" it))]))

;; a moment as FHIR writes it: no `@`, and a time without its `T`
(define (moment-text m)
  (define s (substring (moment->string m) 1))
  (if (and (eq? (moment-kind m) 'time) (string-prefix? s "T")) (substring s 1) s))
(define (calendar-code word)
  (define w (if (string-suffix? word "s") (substring word 0 (sub1 (string-length word))) word))
  (case w [("year") "a"] [("month") "mo"] [("week") "wk"] [("day") "d"] [("hour") "h"]
    [("minute") "min"] [("second") "s"] [("millisecond") "ms"] [else word]))

;; a FHIR primitive from its `value` (`code { value: 'final' }`)
(define (primitive-instance t elements)
  (define bad (filter (lambda (el) (not (eq? (car el) 'value))) elements))
  (unless (null? bad)
    (fp-error 'semantic (format "~a { … }: a primitive is set by its value, not ~a" t (car (car bad)))))
  (define make (constructor-of t))
  (define value (let ([el (assq 'value elements)]) (and el (cdr el))))
  (cond
    [(not make) (fp-error 'execution (format "~a { … }: no constructor for ~a" t t))]
    [(not value) (fp-error 'execution (format "~a { : } has no value" t))]
    [(pair? (cdr value)) (fp-error 'execution (format "~a { … }: value takes one value" t))]
    [else
     (define v (item->json (car value)))
     (define made (make (if (jnum? v) (jnum-lexeme v) v)))
     (if (ok? made) (ok-value made)
         (fp-error 'execution (format "~a { … } is no ~a: ~a" t t (reason (err-error made)))))]))

;; System.Quantity { value: 5, unit: 'mg' }
(define (system-quantity elements)
  (define (one name)
    (define el (assq name elements))
    (and el (if (pair? (cddr el)) (fp-error 'execution (format "System.Quantity { … }: ~a takes one value" name)) (item->system (cadr el)))))
  (for ([el (in-list elements)] #:unless (memq (car el) '(value unit)))
    (fp-error 'semantic (format "System.Quantity { … } has no element ~a (it has value and unit)" (car el))))
  (define v (one 'value))
  (define u (or (one 'unit) "1"))
  (define dec (cond [(fp-decimal? v) v] [(exact-integer? v) (integer->decimal v)]
                    [else (fp-error 'execution "System.Quantity { … }: value is a number")]))
  (unless (string? u) (fp-error 'execution "System.Quantity { … }: unit is a string"))
  (fp-quantity dec u #f))

(define (reason e) (if (validation-error? e) (validation-error-message e) (format "~a" e)))
