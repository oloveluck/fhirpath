# fhirpath — FHIRPath

> Part of [fhir](https://github.com/oloveluck/fhir), a FHIR implementation designed to be used by
> AI agents: one line per problem, at its place, with what would fix it; a guide written for an
> agent to read; the same language inside the `fhir-mcp` workspace.

[FHIRPath](https://hl7.org/fhirpath/) is FHIR's small language for asking a question of one
resource: "the patient's official given names", "is this patient an adult". FHIR itself uses it
wherever a rule has to point inside a resource: the invariants of a profile, search parameters,
mapping rules, the columns of a view. This package evaluates it. You give an expression and a
resource as JSON and get the answer, which is always a list of values; a mistake in the expression
is reported at its place.

```
$ fhirpath "Patient.name.where(use = 'official').given" patient.json
Peter
James
$ fhirpath "Patient.birthDate < today() - 18 years" patient.json
true
```

It reads FHIR R4 and R5 resources and understands units of measure (UCUM), so
`1 'kg' = 1000 'g'` is true.

## Install

Build the [fhir family's tools](https://github.com/oloveluck/fhir#install) once; `fhirpath` is one
of them. With Racket, `raco fhirpath` is the same command.

## Documentation

**[oloveluck.github.io/fhirpath](https://oloveluck.github.io/fhirpath/)**: a first example explained
step by step, how to write an expression, where FHIR uses the language, using it from a program, and
the reference.

- For agents: [guide.md](guide.md), a cheat sheet and worked examples, each one tested; and the
  [fhirpath skill](.claude/skills/fhirpath/SKILL.md).
- How well it works (HL7's FHIRPath suites: all 1044 tests for R5, 932 of 934 for R4; all 573 of
  UCUM's): [TESTING.md](TESTING.md).
- How it is built: [CLAUDE.md](CLAUDE.md).

License: MIT.
