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
