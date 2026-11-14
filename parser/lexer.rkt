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
         (only-in fhir/diagnostic diagnostic loc raise-diagnostic exn:fail:diagnostic?)
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
  (lexer-srcloc
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
    (token lexeme lexeme)]

   ;; text no rule above reads: one character, so it loses to every rule that reads one or more
   ;; (a tie goes to the earlier rule). The token carries the text from that character to the next
   ;; space, so the error quotes what was written there (`@@`, `"a"`) — reading it after the lexer
   ;; had failed quoted what FOLLOWED the failure (nothing for `@@`, `a"` for `"a"`).
   [any-char (token 'UNRECOGNIZED (string-append lexeme (rest-of-word input-port)))]))

;; rest-of-word : input-port -> String   the characters up to the next space, without reading them
(define (rest-of-word ip)
  (define s (peek-string 40 0 ip))
  (if (eof-object? s) "" (car (regexp-split #rx"[ \t\r\n]" s))))

;; unrecognized-message : String -> String   what to say of text no token reads
(define (unrecognized-message text)
  (format "unrecognized text `~a`~a" text
          (if (regexp-match? #rx"^\"" text) " (a string is written in single quotes: 'text')" "")))

;; positioned : lexer-srcloc result -> token   the token with its line/column/position/span, so brag
;; reports parse errors there (token accessors downstream are unchanged); unmatched text is a
;; positioned LEX diagnostic (fhir/diagnostic) at the character where it starts
(define (positioned t)
  (cond [(srcloc-token? t)
         (define tok (srcloc-token-token t))
         (define sl (srcloc-token-srcloc t))
         (when (and (token-struct? tok) (eq? (token-struct-type tok) 'UNRECOGNIZED))
           (raise-diagnostic (diagnostic 'fhirpath 'lex 'error (unrecognized-message (token-struct-val tok))
                                         (loc #f (srcloc-line sl) (let ([c (srcloc-column sl)]) (and c (add1 c)))
                                              (srcloc-position sl) (string-length (token-struct-val tok))))))
         (if (token-struct? tok)
             (token (token-struct-type tok) (token-struct-val tok)
                    #:line (srcloc-line sl) #:column (srcloc-column sl)
                    #:position (srcloc-position sl) #:span (srcloc-span sl)
                    #:skip? (token-struct-skip? tok))
             tok)]
        [else t]))
(define ((positioned-lexer lang lex) ip)
  (define-values (line col pos) (port-next-location ip))
  (with-handlers ([(lambda (e) (and (exn:fail? e) (not (exn:fail:diagnostic? e))))
                   (lambda (e)
                     (define rest (let ([s (peek-string 30 0 ip)]) (if (eof-object? s) "" s)))
                     (raise-diagnostic (diagnostic lang 'lex 'error
                                                   (format "unrecognized text `~a`" (car (regexp-split #rx"[ \t\r\n]" rest)))
                                                   (loc #f line (and col (add1 col)) pos #f))))])
    (positioned (lex ip))))

(define (tokenize ip)
  (port-count-lines! ip)
  (define next (positioned-lexer 'fhirpath fhirpath-lexer))
  (lambda () (next ip)))
