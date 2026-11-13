---
name: fhirpath
description: Evaluate and debug FHIRPath expressions without an MCP server — `raco fhirpath` over a FHIR JSON resource (R4 or R5), the Racket API (`eval-fhirpath` with `%` externals, `%terminologies`, strict mode), and the HL7/UCUM conformance suites. Use when writing or checking a FHIRPath expression (an invariant, a search parameter, a mapping path), evaluating one over a resource, or working on the FHIRPath engine.
---

# FHIRPath without MCP

Read [`guide.md`](../../../guide.md) first: the language in brief, a cheat sheet, worked examples
with their exact output, and the common problems (it is what fhir-mcp serves as `learn://fhirpath`).
This skill is the workflow around it.

`fhirpath` and `raco fhirpath` are the same tool (the standalone build is `dist/bin/fhirpath`).

## The loop
1. **Evaluate over a resource.** One result per line: strings as themselves, complex items as FHIR
   JSON, other values as FHIRPath writes them (`152`, `@1974-12-25`, `5 'mg'`):
   ```
   raco fhirpath "Observation.component.where(code.coding.code = '8480-6').value.ofType(Quantity).value" bp.json
   152
   ```
   R5 is the default; `--r4` reads the resource as R4 (FHIR 4.0.1 data, US Core, IPS). No resource:
   the expression runs over the empty collection (`raco fhirpath "(1 + 2) * 3"` prints `9`).
2. **Read the error.** A syntax error is positioned and exits 1:
   ```
   <expression>:1:25: fhirpath parse error: the expression ends after `(`
   ```
   FHIRPath is one expression: a second one after `;` is a syntax error at the `;`, an empty text
   is `the expression is empty`.
   The type check runs before evaluation, so `no element Patient.nmae` and `direct access to choice
   element Observation.valueQuantity` are errors, not empty answers. A search parameter of several
   types (`Patient.name.given | Practitioner.name.given`) runs over a resource of one of them: the
   other types' parts of the union give nothing (on its own, `Practitioner.name.given` over a
   Patient is still `no element Patient.Practitioner`). `--format json` (or `outcome`)
   writes the diagnostic as JSON (an OperationOutcome) on stdout.
3. **Empty answer?** Nothing printed means the empty collection. Cut the expression back from the end
   until something prints; the step after that reads an element that is absent (or a choice read
   without `ofType`).

## From Racket
Load a version tower, read the resource as a typed node, evaluate:
```racket
(require fhir/r4/schemas fhir/serialize/from-json fhir/serialize/read-json fhir/result/result
         fhirpath/parser/parse fhirpath/eval fhirpath/value)
(define obs (ok-value (from-json (call-with-input-file "bp.json" read-fhir-json) #:lenient? #t)))
(define (fp text #:externals [ext (hash)] #:strict? [strict? #t])
  (map item->system (eval-fhirpath (parse-fhirpath text) (list obs) #:externals ext #:strict? strict?)))
(fp "Observation.component.code.coding.where(system = %loinc).code")     ; => '("8480-6" "8462-4")
(fp "Observation.valueQuantity" #:strict? #f)                            ; lenient: the choice key is allowed
```
- Results are lists (collections); `item->system` turns FHIR primitives into System values
  (strings, booleans, exact integers, `fp-decimal`, `moment`, `fp-quantity`).
- `#:externals` binds `%name` (no `%`), each value a collection: `(hash "threshold" '(140))`.
  `%resource`, `%context`, `%ucum`, `%loinc`, `%sct` are built in.
- `%terminologies` is a service from fhir-tx:
  ```racket
  (define tx ((dynamic-require 'fhir/terminology/service 'make-r4-terminology)))   ; over the R4 core package
  (fp "%terminologies.validateVS('http://hl7.org/fhir/ValueSet/observation-status', 'final').parameter.where(name = 'result').value"
      #:externals (hash "terminologies" (list tx)))                        ; => '(#t)
  ```
  `expand()`, `validateVS()` and `translate()` are implemented.
- Errors: a syntax error raises `exn:fail:diagnostic` (with the position); an evaluation or type
  error raises `exn:fp` (fhirpath/error; `exn:fp-kind` is `'syntax`, `'semantic` or `'execution`),
  an `exn:fail`.
- `fhirpath->string` (fhirpath/print) prints an AST back; parse∘print is a fixpoint.

## Conformance and working on the engine
```
raco fhirpath --tck r5      # HL7's R5 suite: 1044/1044 (floor-ratcheted; exit 1 on a regression)
raco fhirpath --tck r4      # R4: 932/934 (the 2 are named in the manual's Limits)
raco fhirpath --ucum-tck    # UCUM's functional tests: 573/573
```
`CLAUDE.md` is the architecture; `TESTING.md` the suites. The engine is shared by cql, fml, fsh, the
validator and the server, so a change must keep every floor; `tests/guide-test.rkt` keeps guide.md's
examples true and `tests/docs-test.rkt` keeps the manual building.
