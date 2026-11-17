# CLAUDE.md — fhirpath

Conventions, HTDP design style, and the conformance ratchet table live in the main `fhir` repo's
CLAUDE.md (`~/Projects/fhir`); this file holds only the notes for this package. This repo root IS the
`fhirpath` collection (`(define collection "fhirpath")`): modules are `fhirpath/compile` etc., and imports of the
foundation use collection paths (`fhir/model/node`, never a relative path into another package).
The CLI is `cli.rkt` (`raco fhirpath`); the conformance runner(s) live in `tests/` behind `--tck`.

## FHIRPath (`fhirpath/`)
A from-scratch engine in `fhirpath/`, evaluating over the typed `fhir` node model. Layers:
`parser/` (brag precedence-cascade grammar + br-parser-tools lexer + prefab AST), `value.rkt`
(item/collection model + System types + `item->system`), `decimal.rkt` (exact + scale),
`temporal-ops.rkt` (partial Date/Time/DateTime: compare/equal/arith/boundary), `quantity.rkt` +
`ucum.rkt` (a compact UCUM engine ported from `ucum-essence.xml` — exact canonicalization/
conversion), `types.rkt` (FHIR ancestry from the registry + System lattice + is/as/ofType),
`navigate.rkt` (schema-driven, value[x] choice, resource-root, `_field` extensions), `equality.rkt`
(= ~ 3-valued + set ops), `operators.rkt`, `functions.rkt` (the full library), `environment.rkt`
(`$this/$index/$total`, `%` externals, defineVariable via a shared mutable scope), `eval.rkt`.

Data-driven conformance harness in `fhirpath/tests/` runs the official HL7 suites:
`raco fhirpath --tck r5` (1044) / `... r4` (934). Current: **R5 1044/1044, R4 932/934** — 100% of
in-scope tests. Each run requires the matching full tower (`run-r5` requires `fhir/r5/schemas`);
`tests/vendor/` has the suite XML + `.g4`, `tests/inputs*/` the JSON resources.
The modules carry their own `module+ test`s. Input loading is **lenient** (`from-json
#:lenient? #t` — unknown/mismatched fields are skipped) so examples from a newer FHIR version than
the tower still load.

Strict-mode semantics: `fhirpath/analyze.rkt` is a conservative static pass (seeded from the input
resource type via `eval-fhirpath #:strict?`) that raises `'semantic` for unknown elements, direct
expanded-choice access (`Observation.valueQuantity` — allowed only under `mode="lenient"`), string
functions on complex types, and order-dependent functions on unordered sources. It only raises when
confident (unknown type ⇒ stop), so it never flags a valid expression it can't fully type. UCUM
product/quotient units, quantity `~` least-precision, defineVariable scope (via `env-clone-vars` at
`|`/projection boundaries), and `resolve()` over `contained` are all implemented.

`%terminologies` is a terminology-service handle: the `terminology/` package is version-neutral
(`make-terminology <core.tgz>`, with `make-r5-terminology` / `make-r4-terminology` conveniences) —
it lazily loads a core package, indexes every CodeSystem/ValueSet/ConceptMap by canonical URL, and
implements `$expand` / `$validate-code` (vs a ValueSet) / `$translate` (ported clean-room from
fhir4s's `TerminologyOps`). Algorithms run on raw jsexpr; only the result nodes go through `from-json`,
so they build against whichever tower the caller registered (R5 core is what the TCK exercises; R4
works identically — the R4 suite just has no `%terminologies` tests, so `run-r4` doesn't wire it). The engine side is a version-neutral `tx-service` struct
(`fhirpath/terminology.rkt`) with three closures; `expand`/`validateVS`/`translate`
(`fhirpath/functions.rkt`) dispatch on it. The service is injected via `eval-fhirpath #:externals`
as `%terminologies` (see `run-r5.rkt`); `env-lookup-external` already checks `#:externals` first, so
no engine-core change was needed. Results are typed nodes (built via `from-json`) that navigate
normally (`.expansion.contains`, `.parameter.where(...)`).

Remaining known failures: 1 R4 `@…+0.1 's'` (a genuine R4-vs-R5 spec conflict — R4 truncates fractional seconds, R5
keeps them; fixing R4 would regress R5); 1 R4 signed-zero `(-0.0034).lowBoundary(1)` = `-0.0`
(`fp-decimal` has no signed zero).

## Package notes
- `fhirpath/print.rkt` (FHIRPath AST -> canonical text, print∘parse fixpoint-tested) moved here
  from `fml/fhirpath-print.rkt`: the store's search-param narrowing and FML's StructureMap
  emission both use it.
- `fhirpath/tests/run-r5.rkt` loads the `%terminologies` backend (`fhir/terminology/service`,
  the fhir-tx package) with `dynamic-require`: the terminology service depends on fhirpath, so a static
  require would make the packages cyclic.

## Editor and agent support (`support.rkt`)
Through `fhir/lang/support`: a document's diagnostics (one expression; a syntax error positioned);
the language server loads it by collection name.

## Workspace support (`workspace.rkt`)
A `.fhirpath` file is one expression: `run` evaluates it over the data source's resources of `over`
(a type or Type/id) or of the type the expression starts with, typed by the workspace's tower (R4 for
4.0.1); `explain` over one resource gives every sub-expression evaluated against the same input (a
path's target, an operator's operands) with its value; function arguments are listed by text.

## Documentation for users and agents
The family's pattern (the main repo's CLAUDE.md): `guide.md` is the language for agents — cheat
sheet, worked examples as a `raco fhirpath` transcript over two resources it shows, running it,
common problems — provided by `fhirpath/workspace` as `guide` (fhir-mcp's `learn://fhirpath`);
`tests/guide-test.rkt` parses every cheat-sheet line and replays the transcript through `cli.rkt`'s
`evaluate`. `.claude/skills/fhirpath/SKILL.md` is the workflow without MCP (every command and snippet
in it was run). `scribblings/fhirpath.scrbl` is the manual; `tests/docs-test.rkt` renders it and fails
on a broken reference to its own sections or `fhirpath/…` bindings. Writing them fixed the command
line's printing: every value that was not a string or boolean printed as a Racket struct
(`#(struct:fp-decimal 152 0)`, `#(struct:moment date 2024 1 1 …)`); it now prints as FHIRPath writes
it (`152`, `@2024-01-01`, `5 'mg'`, `System.Integer`).


## Claude as the user (2026-11-09)
`analyze.rkt` (the static pass `eval-fhirpath` runs by default) enters both operands of an operator:
`activ = true` and `name.where(famly = 'x')` were never checked, the most common shapes an agent
writes. `trace`'s projection argument sees its input's type (the R5 suite's
`name.trace('test', given)` caught it); `repeat`'s argument is not checked, since it runs at every level
it reaches (US Core's sdcqr-2, `repeat(answer | item)`, caught that). Floors unchanged: R5 1044/1044, R4 932/934. A problem's
message names the type and suggests (`no element Patient.activ (did you mean `active`?)`);
`render.rkt` prints values as FHIRPath writes them; the workspace reports the static problem of a
`.fhirpath` file at its position.

## A day the calendar lacks (2026-11-10)
`@1940-02-30` is an error (`calendar-problem` in temporal-ops.rkt, raised by eval's literal), and
`'1940-02-30'.toDate()` is empty: the literal grammar allows day 31 in every month. R5 1044/1044, R4
932/934 unchanged.

## One expression; unions over several types (2026-11-13)
Found writing the manual, each with a test that fails without the fix:
- **Several expressions, or none, are parse diagnostics** (parser/parse.rkt). `parse-fhirpath "1 = 1; 2"`
  raised a plain `exn:fail` (a stack trace at the command line): it is `unexpected `;`: one
  expression is expected, this is 2`, positioned at the first `;` (`separator-loc`, from the token
  stream; the grammar's `fp-program` still reads `;`, for `parse-fhirpath-program`). A text with no
  token (nothing, space, a comment) is `the expression is empty` (it was brag's `unexpected
  no-tokens `#f``), from `parse-fhirpath-program`, so every caller gets it. At the command line both
  exit 1: an argument was given and its text is the problem, so they are reported like any syntax
  error (`--format json` too); no argument at all is the usage, exit 2.
- **A `|` operand rooted at another resource type** (analyze.rkt, eval.rkt). FHIR writes a search
  parameter that serves several types as ONE expression (`Patient.name.given |
  Practitioner.name.given`); over a Patient it was `no element Patient.Practitioner`. The head of a
  union operand (`path-head`: seen through steps, functions, indexers, `as`), when it names a
  resource type, selects the input of that type: the evaluator runs the operand with the head as
  `ofType(<Type>)` (`union-branch`, an AST rewrite in eval's union clause — the rest of the path runs
  over the empty collection, so `Practitioner.name.exists()` is false), and the static pass checks
  the operand against the type it names (`branch-type`: `Practitioner.nmae` in the other branch is
  still `no element Practitioner.nmae`). ONLY in a union: HL7's suite requires `Encounter.name.given`
  over a Patient to be a semantic error (testSimpleWithWrongContext, R4 and R5, strict mode — every
  mode here), so a foreign head anywhere else is the error it was, and so is a name that is neither
  a type nor an element (`no element Patient.activ (did you mean `active`?)`).
  `tests/search-parameters-test.rkt` is the oracle: every R5 core SearchParameter expression against
  each of its bases — 794 of 1980 pairs were refused, 26 are now, all the package's own (`date`
  writes Appointment's branch without its type, `(start | requestedPeriod.start).first()`); R4: 369
  of 1715 before, 1 now (the example parameter `part-agree`, whose base is not its expression's type).
- `tests/cli-test.rkt` runs the three through `raco fhirpath`. Floors unchanged: R5 1044/1044, R4
  932/934, UCUM 573/573.

## Quality, performance and specification pass (2026-11-14)
Held to a dump of what the engine produces over its corpora (every HL7 suite expression: AST, print
fixpoint, value strict and lenient, R5 and R4; every core SearchParameter: the static check per base
and its values over ~390 resources; every core root constraint over every node of those resources;
UCUM validity/canonical/display/suggestion over the functional tests' units): identical after each
refactoring; every other difference is one of the changes below. Tests 299 -> 432; floors unchanged
(R5 1044/1044, R4 932/934, UCUM 573/573, SearchParameters 26 refused).
- **Speed** (same machine, medians; the machine's load rose during the pass, so later numbers are
  pessimistic): the static pass over the 1,980 (search expression, base) pairs x20 630 -> 25 ms;
  58,396 constraint evaluations 2.1 -> 1.1 s; 8,195 search-parameter evaluations 3.9 -> 0.84-1.4 s.
  - `analyze` remembers its verdict per AST (by identity: `parse-fhirpath` answers one AST per text),
    type and mode (`verdict-of`, a weak table cleared when `type-registry-generation` moves): it ran
    on every evaluation and was a quarter to a third of one.
  - `registry-memo` (types.rkt, exported): what depends only on a type, remembered per type for as
    long as the registry is what it was — `fhir-ancestors` and `fhir-ancestors-strict` (one
    `ancestor-chain`; the strict chain was rebuilt per `ofType` item, 15% of search),
    `all-schema-fields`, `choice-members` (navigate.rkt: a table per type; a miss scanned every field),
    `members-of` (analyze.rkt: the elements as hash tables; `resolve` scanned association lists,
    ElementDefinition's ~400 entries, a third of the static pass). `fhir-ancestors`'s range contract
    is `list?` (checking every symbol was a tenth of the pass).
  - A type specifier is read once (`type-spec`: `System.Integer` -> `(System . Integer)`; it was split
    at its dot per item). Literals are read from their lexemes once per literal (`lit->coll`, weak by
    identity; an error literal is not remembered). A variable scope is a box over an immutable hash
    (environment.rkt): opening one at every `|` operand, argument and projected item was a copy of a
    mutable table.
  - Parsing was brag's: lexing the 2,389 distinct expressions takes 45 ms, brag ~3.6 s (98%,
    ~140 µs a token), lowering 4 ms. Done since: see "Read by descent, held to the grammar" below.
    Contract wrappers on fhirpath's own cross-module calls (eval -> navigate/functions/operators/
    types) were then ~40% of evaluation: gone since, see "Contracts at the boundary" below.
- **Regular expressions** (regex.rkt, new): FHIRPath recommends PCRE; Racket's pregexp refuses a
  `]` outside a class and a `{`/`}` that is no repetition, has no named groups, and reads `&`/`\1` in
  a substitution. `translate-pattern` escapes those, drops `(?<name>`/`(?P<name>` keeping the names;
  `regex-replace` applies `$n`/`${name}` itself (`&` is literal: it was the match); `compile-pattern`
  remembers by (pattern, flags) — compiling was 42% of a view with reference keys and a quarter of
  the constraint workload. A bad pattern is `exn:fp` 'execution (it was a raw `pregexp:` exn:fail).
  BEHAVIOUR: FHIR's eld-20 (`(\\[x])?`) compiled on no ElementDefinition: it now holds on all 2,602
  (R5) and 1,719 (R4) of the dump — the validator evaluates it now.
- **Spec alignment** (each with a test that fails without it):
  - `truncate()`/`floor()`/`ceiling()` answer an Integer (N1), not a scale-0 Decimal; a non-singleton
    input is an error; `round(-1)` is an `exn:fp` (it was a contract violation).
  - `toInteger()`/`toDecimal()` (and convertsTo…) read strings by N1's patterns: `'#x10'` was 16
    (Racket's reader), `'1e3'` 1000.
  - A bare unit is a calendar word (the grammar's dateTimePrecision | plural): `5 mg` was read as
    `5 'mg'`; it is a parse error at the word (`check-bare-units!`, parse.rkt, over the tokens:
    NUMBER then IDENTIFIER is always a quantity). 13,584 published expressions: none affected.
  - `print.rkt` writes a calendar duration bare (`5 days`, it was `5 'days'`); this engine reads both
    as the same value, and print∘parse stays a fixpoint. `calendar-word?` (parser/ast.rkt) is the one
    list; quantity.rkt's `calendar-unit?` and operators.rkt use it and `calendar-ucum`.
  - Text no rule lexes is quoted from where it starts (`@@`, `"a"`; a last single-character rule,
    `UNRECOGNIZED`): it quoted what followed the failure (nothing for `@@`); a double quote adds that
    strings take single quotes (eld-11 writes `contains(":")`).
  - Beyond N1 (the current build, STU): `coalesce` (lazy), `lastIndexOf`, `toLong`/`convertsToLong`,
    `sum`/`min`/`max`/`avg` (one type, else an execution error), `yearOf` … `millisecondOf`,
    `timezoneOffsetOf`, `dateOf`, `timeOf`, `repeatAll` (stopped past 100,000 items), the `i`/`m`
    flags of matches/matchesFull/replaceMatches, `combine`'s second argument; the static pass knows
    repeatAll/coalesce/lastIndexOf. FHIR's memberOf/subsumes/subsumedBy/htmlChecks/elementDefinition/
    slice/checkModifiers/weight and duration/difference/pathname say they are not implemented and
    what they need (`not-implemented`), instead of "unknown function".
- **Quality**: `member=?`/`distinct=` in equality.rkt (operators.rkt's `dedup` and functions.rkt's
  `dedup=`/`member-of?` were two copies); equality.rkt's `as-dec` is value.rkt's `as-decimal`;
  `primitive-codes` defined once (types.rkt); navigate.rkt lost its dead `valid-member?` and
  `framework-fields`; analyze.rkt lost unused imports.
- **Decided against**: the `as` OPERATOR on several items stays a filter (the current build makes it
  an error; FHIR's search parameters write `Observation.component.value as Quantity`); the signed
  zero (`fp-decimal` would need a sign on exact zero — its struct is used by cql — for one R4 case
  the R5 suite passes by value); double-quoted strings (the current grammar adds `\"` as an escape,
  not a string form). Left: `duration`/`difference` (calendar boundaries and whole periods per
  precision, with the timezone rules), `sort`'s `asc`/`desc` keywords (a grammar change; `-` works),
  the LONGNUMBER literal (`2L`), instance selectors, `pathname`.

## Read by descent, held to the grammar (2026-11-14)
The owner's rule: keep brag "for testing since it helps tie the implementation to the published
antlr grammar". So `parser/grammar.rkt` (brag, from the vendored fhirpath.g4) stays the definition
of FHIRPath text, and `parser/descent.rkt` (`descend-program`) reads it fast:
- **descent.rkt** reads the grammar's precedence cascade by recursive descent, a token at a time,
  straight to the AST (`binary` folds each level's operators to the left; `type-expr`, `unary-expr`,
  `postfix-expr`, `primary`, `literal` are the grammar's rules). It only ever ACCEPTS: on a token it
  does not expect it answers #f (`refuse`), and parse.rkt gives the tokens to brag, whose answer —
  its tree lowered by ast.rkt's `lower`, or its positioned error — is the one returned. Every syntax
  error therefore comes from brag, as before. The grammar reads one form two ways — a NUMBER
  followed by `is`/`as`/`in`/`contains` with nothing that begins an operand after it (`5 is`: a unit
  named `is`, or the start of a type test) — and descent leaves exactly that to brag
  (`unit-keyword-certain?`).
- **parse.rkt**: `parse-fhirpath-program` = descent, else `brag-program` (the old path, unchanged:
  `check-bare-units!` runs on both); `parse-fhirpath-program/brag` is brag alone, provided for tests.
- **tests/descent-test.rkt** holds the two to the same Outcome — the same AST, or the same error
  word for word at the same place — over the HL7 suites (1,042 texts), every SearchParameter and
  constraint expression of the R4 and R5 cores, US Core, IPS, mCODE and CRMI (2,327), 3,000
  expressions grown from the grammar's productions (seeded) and 3,000 token soups: 0
  disagreements. It also counts what descent read itself: every text the grammar reads, in the
  suites, the packages and the generated expressions; among the soups all but the `5 is` form. A
  planted bug (`mod`'s operands swapped) fails it in all four groups.
- **Measured** (one machine, back to back): the first parse of the 1,042 suite expressions 1,282 ->
  113 ms (lexing now most of it); `raco fhirpath --tck r5` 4.3 -> 3.0 s, `--tck r4` 2.6 -> 0.8 s.
  Downstream the gain is small: `raco fsh check` on mCODE ~7.0 -> ~6.1 s and one
  `raco fhir-validate` ~3.1 -> ~2.8 s — within run-to-run noise; their time is startup and the
  package indexes, and repeated expressions were already memoised. Floors unchanged (R5 1044/1044,
  R4 932/934, UCUM 573/573); tests 432 -> 454.

## Contracts at the boundary (2026-11-15)
The engine's modules call one another without contracts; callers outside the package (cql, fml,
fsh, sql-on-fhir, fhir-validation, fhir-server) get every contract they had.
- **The profile** (sampler, before any change; `contract-profile` is not installed): on the core
  constraints over every node of 388 resources, 43% of the samples were in racket/contract's own
  frames (arrow wrappers, keyword checking, `or/c`/`listof` range checks), on the search parameters
  40%. They were the wrappers on eval -> navigate/operators/functions/types/value/equality/
  environment calls, and one per call of every `registry-memo` table imported by navigate.rkt and
  analyze.rkt (the memo was provided as `(-> (-> any/c any/c) (-> any/c any/c))`, so the remembered
  function came back wrapped).
- **The change**: each engine module (error, value, decimal, temporal-ops, environment, quantity,
  equality, operators, functions, navigate, types, regex, ucum, analyze) provides with
  `(contract-out #:unprotected-submodule unchecked …)` — one list — and a module with `struct-out`s
  adds them to the same submodule (`(module+ unchecked (provide (struct-out …)))`), so an engine
  module requires `(submod "x.rkt" unchecked)` with the `only-in` list it had. eval.rkt (whose
  `eval-fhirpath`/`fp-eval` are the boundary), print, render, the CLI, workspace and support keep the
  contracted interfaces. `tests/contracts-test.rkt`: a bad argument from outside is still refused,
  blaming the caller (one function per module), every engine module offers `unchecked`, and no
  engine module (or eval.rkt) imports another's contracted interface — a planted
  `(only-in "navigate.rkt" navigate)` in eval.rkt fails it.
- **Then, what the profile showed next** (behaviour-preserving): the foundation's `fhir-has?` then
  `fhir-ref` on every step (two lookups, each through fhir/model/node's contract: a quarter of what
  remained) are one `hash-ref` on `fhir-fields`, the struct's own accessor (navigate.rkt
  `field-values`); `known-type?` (every `is`/`as`/`ofType`) and `resource-type-name?` (every step a
  node does not hold, every union) are remembered by `registry-memo` (the registry's answer changes
  only with its generation); `registry-memo` reads the generation once a call (it was twice, each
  through the foundation's contract). `analyze`'s `resolve` asks `fhir-ancestors` of two different
  types, both memo hits, and the static pass does not show in the profiles; regex.rkt's
  `compile-pattern` cache already covers matches/replaceMatches (its lookup is ~3%; the rest is the
  matching itself). What remains in contract frames (~10%) is the boundary itself (the bench's
  calls of `eval-fhirpath`) and the foundation's `type-registry-generation`.
- **Measured** (one machine, back to back, medians of 3): the constraint workload (58,396
  evaluations x3) 792 -> 356 ms (the unchecked submodules alone: -> ~500); the search parameters
  (8,195 x5) 1,006 -> 379 ms (alone: -> ~494); the HL7 suite's expressions x20 82 -> 44 ms; a
  ViewDefinition of nine columns and a forEach over 3,440 eCQM Encounters read as R4, in process (x3)
  456 -> 383 ms. Whole commands are startup: `raco fhirpath --tck r5` 1.91 -> 1.88 s, `--tck r4`
  0.60 -> 0.58 s, loading fhirpath/eval 0.34 s both, `raco sql-on-fhir run` of that view 1.52 ->
  1.55–1.68 s over three runs of nine (noise: reading the files and from-json are its time). The
  behaviour dump (every suite expression strict and lenient, every SearchParameter's static check and
  values, every root constraint on every node, R5 and R4; UCUM) is identical. Floors unchanged (R5
  1044/1044, R4 932/934, UCUM 573/573, SearchParameters 26 refused); tests 454 -> 457.

## What resolve() did not find (2026-11-17)
`resolve()` answers nothing for a reference whose resource is not in the input (not contained, not
an entry of a root Bundle), which an expression cannot tell from a reference to nothing:
`provider.all(resolve() is Practitioner)` is false of a reference to a Practitioner on a server.
`current-unresolved-references` (functions.rkt, provided by eval.rkt too) is #f, or a box a caller
sets: each reference `resolve-one` looked for and did not find is put in it. The engine's answers
are unchanged; fhir-validation reads the box to leave such an invariant undecided. Floors
unchanged: R5 1044/1044, R4 932/934.
