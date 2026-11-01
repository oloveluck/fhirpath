#lang racket/base

;;; lexer.rkt — FHIRPath source text -> the token stream brag consumes.
;;;
;;; Named tokens (NUMBER, STRING, BOOL, IDENTIFIER, DATE, DATETIME, TIME, EXTERNAL) carry a value;
;;; operators/keywords are emitted with their literal string as the token TYPE so the grammar's
;;; "and"/"."/"<=" literals match. NUMBER keeps its lexeme (decimal precision). Temporal literals
;;; are tagged (date-lit/datetime-lit/time-lit) at lex time so they survive as data. WS + // and
;;; /* */ comments are skipped.

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         racket/contract
         (only-in "ast.rkt" date-lit datetime-lit time-lit))

(provide (contract-out [tokenize (-> input-port? any/c)]))

(define (trim-ends s) (substring s 1 (sub1 (string-length s))))

;; unescape FHIRPath string escapes, incl. \uXXXX
(define (unescape s)
  (regexp-replace* #px"\\\\(u[0-9a-fA-F]{4}|.)" s
    (lambda (_ g)
      (cond
        [(char=? (string-ref g 0) #\u) (string (integer->char (string->number (substring g 1) 16)))]
        [else (case g
                [("n") "\n"] [("t") "\t"] [("r") "\r"] [("f") "\f"]
                [("\\") "\\"] [("'") "'"] [("\"") "\""] [("`") "`"] [("/") "/"]
                [else g])]))))

(define-lex-abbrevs
  [tz (:or "Z" (:: (:or "+" "-") numeric numeric ":" numeric numeric))]
  [timeformat (:: numeric numeric
                  (:? (:: ":" numeric numeric
                          (:? (:: ":" numeric numeric (:? (:: "." (:+ numeric))))))))]
  [dateformat (:: numeric numeric numeric numeric
                  (:? (:: "-" numeric numeric (:? (:: "-" numeric numeric)))))])

(define fhirpath-lexer
  (lexer
   [(eof) (void)]
   [(:or " " "\t" "\r" "\n" "\f") (token 'WS lexeme #:skip? #t)]
   [(:: "//" (:* (:~ "\n"))) (token 'COMMENT lexeme #:skip? #t)]
   [(:: "/*" (:* (:or (:~ "*") (:: "*" (:~ "/")))) (:* "*") "*/") (token 'COMMENT lexeme #:skip? #t)]

   ;; string literal (single-quoted)
   [(:: "'" (:* (:or (:: "\\" any-char) (:~ "'" "\\"))) "'")
    (token 'STRING (unescape (trim-ends lexeme)))]

   ;; temporal literals — precise so a trailing "." stays a navigation dot
   [(:: "@" "T" timeformat) (token 'TIME (time-lit (substring lexeme 2)))]
   [(:: "@" dateformat "T" (:? (:: timeformat (:? tz)))) (token 'DATETIME (datetime-lit (substring lexeme 1)))]
   [(:: "@" dateformat) (token 'DATE (date-lit (substring lexeme 1)))]

   ;; number (integer or decimal) — keep the lexeme for precision
   [(:: (:+ numeric) (:? (:: "." (:+ numeric)))) (token 'NUMBER lexeme)]

   ;; boolean before identifiers
   [(:or "true" "false") (token 'BOOL (string=? lexeme "true"))]

   ;; keyword operators / special vars before identifiers
   [(:or "implies" "and" "or" "xor" "in" "contains" "is" "as" "div" "mod") (token lexeme lexeme)]
   [(:or "$this" "$index" "$total") (token lexeme lexeme)]

   ;; external constants: %name | %`delim` | %'string'
   [(:: "%" (:or alphabetic "_") (:* (:or alphabetic numeric "_"))) (token 'EXTERNAL (substring lexeme 1))]
   [(:: "%" "`" (:* (:~ "`")) "`") (token 'EXTERNAL (trim-ends (substring lexeme 1)))]
   [(:: "%" "'" (:* (:or (:: "\\" any-char) (:~ "'" "\\"))) "'")
    (token 'EXTERNAL (unescape (trim-ends (substring lexeme 1))))]

   ;; identifiers (plain and backtick-delimited)
   [(:: (:or alphabetic "_") (:* (:or alphabetic numeric "_"))) (token 'IDENTIFIER (string->symbol lexeme))]
   [(:: "`" (:* (:or (:: "\\" any-char) (:~ "`" "\\"))) "`")
    (token 'IDENTIFIER (string->symbol (unescape (trim-ends lexeme))))]

   ;; multi-char operators before single-char
   [(:or "<=" ">=" "!=" "!~") (token lexeme lexeme)]
   [(:or "." "[" "]" "(" ")" "," ";" "{" "}" "|" "&" "+" "-" "*" "/" "=" "<" ">" "~")
    (token lexeme lexeme)]))

(define (tokenize ip)
  (port-count-lines! ip)
  (lambda () (fhirpath-lexer ip)))
