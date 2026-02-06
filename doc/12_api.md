# Bankster Front API

This document describes the front API exposed by `io.randomseed.bankster.api.*`.
It is a curated, stable facade over the core namespaces (`currency`, `money`,
`registry`) and should be the default entry point for application code.

Frozen API for major 2:

- `io.randomseed.bankster.api.v2` and its sub-namespaces mirror
  `io.randomseed.bankster.api.*` for the Bankster 2.x line.
- When Bankster 3 appears, the v2 API will remain available for compatibility.
- In the current release, `io.randomseed.bankster.api.v2.*` is equivalent to
  `io.randomseed.bankster.api.*`.

Note: this is an overview, not an exhaustive arity-level reference.

## Namespaces

- `io.randomseed.bankster.api` - umbrella facade (money/currency/scale helpers).
- `io.randomseed.bankster.api.currency` - currency resolution, predicates,
  registry operations, and serialization helpers.
- `io.randomseed.bankster.api.money` - money constructors, arithmetic,
  parsing/formatting, and serialization helpers.
- `io.randomseed.bankster.api.registry` - registry helpers (default/state/with,
  hierarchy helpers) plus selected registry aliases.
- `io.randomseed.bankster.api.ops` - operator-style functions intended for
  intentional `:refer :all`.

## Soft vs strict

- *Strict* functions throw on missing currency/registry matches.
- *Soft* functions return `nil` when no match is found.

Examples:

- `api.currency/resolve` (strict) vs `api.currency/resolve-try` (soft)
- `api.money/resolve` (strict) vs `api.money/resolve-try` (soft)

## `io.randomseed.bankster.api` (umbrella)

- `money`, `money-try` - front constructors for Money (strict/soft).
- `currency` - front constructor for Currency (strict).
- `rounding-mode`, `scale-apply`, `amount`, `scale`, `auto-scaled?` - convenience helpers over `scale`.
- `info` - polymorphic info helper (`currency/info` / `money/info`).

## `io.randomseed.bankster.api.currency`

**Resolution / constructors**

- `resolve`, `resolve-try`, `resolve-all`, `new`
- `of` (macro alias), `normalize`
- `with`, `with-default`, `with-registry`

**Accessors**

- `id`, `id-str`, `code`, `code-str`, `nr`, `scale`, `domain`, `kind`, `symbol`
- `info`, `to-map` (and JSON/EDN helpers)

**Predicates / traits / kinds**

- `currency?`, `possible?`, `definitive?`, `auto-scaled?`, `crypto?`, `iso?`
- `has-trait?`, `of-trait?`, `has-kind?`, `of-kind?`, `of-domain?`

**Registry operations**

- `register`, `register!`, `unregister`, `unregister!`
- `add-traits`, `set-traits`, `remove-traits`

## `io.randomseed.bankster.api.money`

**Constructors / coercion**

- `resolve`, `resolve-try`
- `major`, `minor`, `of-registry`, `cast`, `cast-try`
- `amount`, `currency`, `info`, `normalize`
- `rounding-mode`, `scale-apply`

**Arithmetic**

- `add`, `sub`, `mul`, `div`
- `round`, `round-to`, `allocate`, `distribute`

**Comparisons / predicates**

- `compare`, `eq?`, `ne?`, `gt?`, `ge?`, `lt?`, `le?`
- `pos?`, `neg?`, `zero?`, `same-currencies?`

**Parsing / formatting**

- `parse`, `parse-major`, `parse-minor`
- `format`, `unparse`

**Serialization**

- `to-map`, `to-json-string`, `from-json`, `from-edn`, etc.

## `io.randomseed.bankster.api.registry`

- `default`, `or-default`
- `with`, `state`
- `hierarchy-derive`, `hierarchy-derive!`
- plus selected aliases to registry helpers (see public vars in the namespace).

## `io.randomseed.bankster.api.ops`

Operator-style functions aligned with core arithmetic and comparisons:

- `+`, `-`, `*`, `/`
- `=`, `not=`, `<`, `<=`, `>`, `>=`
- `pos?`, `neg?`, `compare`, numeric casts

Use this namespace when you *intentionally* want `:refer :all`.

## Notes

- Prefer `api.*` in application code for stability and ergonomics.
- The core namespaces (`io.randomseed.bankster.money`, `currency`, `registry`)
  remain available for lower-level control and internal use.
