#lang brag

;; FHIRPath grammar (http://hl7.org/fhirpath/N1), translated to brag.
;;
;; brag has no precedence directives, so precedence is a rule cascade — lowest (implies) at the
;; entry, highest (path/index) at the leaves. Punctuation is hidden with `/`; operator-keyword
;; wrappers are spliced with `@` so the operator surfaces inline for lowering. Literal tokens
;; (".", "and", …) and named tokens (NUMBER, STRING, IDENTIFIER, DATE, …) come from lexer.rkt.

fp-program          : fp-expr (/";" fp-expr)*

fp-expr             : implies-expr

implies-expr        : or-expr             (@implies-op or-expr)*
or-expr             : and-expr            (@or-op and-expr)*
and-expr            : membership-expr     (@and-op membership-expr)*
membership-expr     : equality-expr       (@membership-op equality-expr)*
equality-expr       : inequality-expr     (@equality-op inequality-expr)*
inequality-expr     : union-expr          (@inequality-op union-expr)*
union-expr          : type-expr           (@union-op type-expr)*
type-expr           : additive-expr       (@type-op type-spec)*
additive-expr       : multiplicative-expr (@additive-op multiplicative-expr)*
multiplicative-expr : unary-expr          (@multiplicative-op unary-expr)*
unary-expr          : @polarity-op? postfix-expr

postfix-expr        : primary postfix-tail*
postfix-tail        : invocation | indexer
invocation          : /"." (function-call | member | special-var)
indexer             : /"[" fp-expr /"]"

primary             : literal
                    | function-call
                    | member
                    | special-var
                    | external-constant
                    | /"(" fp-expr /")"

function-call       : identifier /"(" arg-list? /")"
arg-list            : fp-expr (/"," fp-expr)*
member              : identifier
special-var         : "$this" | "$index" | "$total"
external-constant   : EXTERNAL
type-spec           : identifier (/"." identifier)*

;; `as`/`is`/`in`/`contains` are also usable as identifiers (member/function/type names)
identifier          : IDENTIFIER | "as" | "is" | "in" | "contains"

literal             : quantity | STRING | BOOL | DATE | DATETIME | TIME | "{" "}"
quantity            : NUMBER unit?
unit                : identifier | STRING

implies-op          : "implies"
and-op              : "and"
union-op            : "|"
or-op               : "or" | "xor"
membership-op       : "in" | "contains"
equality-op         : "=" | "!=" | "~" | "!~"
inequality-op       : "<=" | ">=" | "<" | ">"
polarity-op         : "+" | "-"
additive-op         : "+" | "-" | "&"
multiplicative-op   : "*" | "/" | "div" | "mod"
type-op             : "is" | "as"
