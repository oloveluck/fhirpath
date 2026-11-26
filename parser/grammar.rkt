#lang brag

;; FHIRPath grammar (FHIRPath 3.0.0, http://hl7.org/fhirpath/ — tests/vendor/fhirpath.g4), translated
;; to brag. 3.0.0 added to N1: Long literals (`42L`), instance selectors (`Coding { code: 'x' }`,
;; `Period { : }`), and sort() keys with `asc`/`desc`, those three words also being names. One
;; difference of form: the g4 lets `sort` be any identifier (ANTLR takes its sort rule first where
;; both read a text); here `sort` is a member's name only, so no text reads two ways.
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
                    | instance-selector
                    | function-call
                    | member
                    | special-var
                    | external-constant
                    | /"(" fp-expr /")"

function-call       : sort-call | identifier /"(" arg-list? /")"
sort-call           : /"sort" /"(" (sort-key (/"," sort-key)*)? /")"
sort-key            : fp-expr ("asc" | "desc")?
arg-list            : fp-expr (/"," fp-expr)*
member              : identifier | "sort"
instance-selector   : qualified-name /"{" (/":" | element-selector (/"," element-selector)*) /"}"
element-selector    : identifier /":" fp-expr
qualified-name      : identifier (/"." identifier)*
special-var         : "$this" | "$index" | "$total"
external-constant   : EXTERNAL
type-spec           : identifier (/"." identifier)*

;; `as`/`is`/`in`/`contains`/`asc`/`desc` are also usable as identifiers (member/function/type names)
identifier          : IDENTIFIER | "as" | "is" | "in" | "contains" | "asc" | "desc"

literal             : quantity | LONG | STRING | BOOL | DATE | DATETIME | TIME | "{" "}"
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
