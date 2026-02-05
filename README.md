# Bankster /ˈbæŋkstə/

**Money as data, done right.**

*A pragmatic, EDN-friendly money & currency toolkit for Clojure: ISO 4217 / crypto /
custom currencies, with precision-first arithmetic and an expressive DSL layer.*

[![Bankster on Clojars](https://img.shields.io/clojars/v/io.randomseed/bankster.svg)](https://clojars.org/io.randomseed/bankster)
[![Bankster on cljdoc](https://cljdoc.org/badge/io.randomseed/bankster)](https://cljdoc.org/d/io.randomseed/bankster/CURRENT)
[![CircleCI](https://circleci.com/gh/randomseed-io/bankster.svg?style=svg)](https://circleci.com/gh/randomseed-io/bankster)

This is a Clojure library for operating on monetary units with cryptocurrency and
custom currency support.

## Who is this for?

Bankster shines when you need *money as data* — predictable, comparable, composable —
without sacrificing ergonomics.

Typical users and use-cases:

- **Fintech / payments / wallets / exchanges**: model fiat + crypto under one coherent API (including non-standard codes).
- **Accounting-ish domains**: operate on amounts with fixed, explicit scale and controlled rounding.
- **Data pipelines & ETL**: parse/emit EDN safely (tagged literals, data readers) and keep currency semantics attached to numbers.
- **Systems integrating many “currency worlds”**: keep separate registries per source/environment (dynamic/global/local registries).
- **Apps that want a small “money DSL”**: concise literals/macros for currencies and amounts.

If your primary need is exchange-rate discovery, market data, or full-blown
accounting/ledger rules — Bankster is intentionally smaller and focuses on
representing currency + amount and doing safe operations around that.

## Why Bankster (in one breath)

- **Precision-first arithmetic**: `BigDecimal` everywhere, currency scale-aware operations, and tooling for non-terminating expansions.
- **Allocation and distribution**: built-in functions for allocating and distributing monetary amounts.
- **One model for ISO + crypto + custom**: namespaced identifiers like `crypto/ETH`, plus conflict-safe code resolution via weights.
- **EDN-native ergonomics**: tagged literals (`#money`, `#currency`), readers, macros, and parsing that make data pipelines pleasant.
- **Registry concept**: treat currencies as a database you can extend, swap, or scope per computation.
- **Polymorphism**: protocols (`Scalable`, `Monetary`, `Accountable`) keep it extensible and composition-friendly.

## Features

* Pure Clojure implementation based on Java's **BigDecimal**.
* Uses **records** to organize data: `Registry`, `Currency`, `Money`.
* Built-in standard **currencies database**, extendable with EDN file.
* Ability to create *ad hoc* currencies (with optional registering).
* Different sources of **currency registries** (dynamic, global or local).
* Registry **hierarchies** for classification (`:domain`, `:kind`, custom)  
  and optional per-currency **traits**.
* **Polymorphic** interfaces (`Scalable`, `Monetary`, `Accountable` protocols).
* Ability to **cast** and **convert** monetary amounts.
* Useful **macros** to express currencies and monetary amounts with various forms.
* **Namespaced identifiers** for non-ISO currencies (e.g. `crypto/ETH`).
* Common **math operators**.
* **Auto-rescaling** in math operations to handle non-terminating decimal expansion.
* Variants of **variadic math functions** with rescaling after each consecutive operation.
* Optional **rescaling** of monetary amounts with keeping track of nominal scales.
* Functions for **rounding** to a scale or to the given **interval**.
* Functions for **allocating** and **distributing** of monetary amounts.
* **Tagged literals** for currencies and monetary amounts.
* **Data readers** for currencies and monetary amounts expressed with EDN.
* Customizable currency and money **formatting** with **locale support**.
* **JSON and EDN serialization** with per-format protocols, minimal/full representations, and Cheshire integration.

## Contracts

See [Bankster Contracts](doc/15_contracts.md) for practical contracts (what is guaranteed,
what is "soft" vs "strict", how the default registry is chosen, when exceptions are
thrown, how the protocols behave) for Bankster's core axis: `Currency`, `Money`,
`Registry` records and the `Monetary`, `Scalable` and `Accountable` protocols.

## Front API

Bankster provides a curated front API under `io.randomseed.bankster.api.*`
(recommended for application code). See [API](doc/12_api.md) for an overview.

For major-version stability there is a frozen API namespace:
`io.randomseed.bankster.api.v2` and its sub-namespaces. It mirrors
`io.randomseed.bankster.api` for the Bankster 2.x line. When Bankster 3 appears,
the v2 API will remain available for compatibility. In the current release,
`io.randomseed.bankster.api.v2.*` is equivalent to `io.randomseed.bankster.api.*`.

## Installation

To use Bankster in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/bankster "2.2.2"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/bankster {:mvn/version "2.2.2"}
```

You can also download JAR from [Clojars](https://clojars.org/io.randomseed/bankster).

## Design

The core design principle: **money is a value with a currency attached**, and
currencies live in a **registry** that can be global, dynamic, or explicitly passed
around. This keeps both *precision* and *context* explicit.

### Registry of currencies

There is a global, shared **registry** (`Registry`) of currencies, which is
thread-safe and encapsulated in an Atom. It consists of databases (maps) for quickly
accessing important currency properties. Most of the functions will use this global
registry, unless a registry is explicitly passed as an argument or set as a dynamic
variable.

Registry is implemented as a record of maps keeping the following associations:

* currency ID to currency object;
* currency ID to a set of country IDs;
* currency numeric ID to currency object;
* country ID to currency object;
* currency ID to localized properties map;
* currency ID to traits set (advisory tags/features);
* currency ID to weight (an integer used for conflict resolution);
* currency code to a sorted set of currency objects (weighted);
* currency numeric ID to a sorted set of currency objects (weighted);
* currency domain to a sorted set of currency objects (weighted).

Registry also carries:

* `:hierarchies` – classification hierarchies (e.g. `:domain`, `:kind`, `:traits`, and any custom axes);
* `:ext` – a free-form extension map for registry-level metadata.

In most cases you won't have to worry about the internals of a registry. However,
when working with multiple data sources or data processing engines (like currency
exchange platforms), you may find it useful to have **different registries** (with
the same currencies but of different scales). Most of the functions operating on
monetary units and currencies will accept a registry object as their additional
argument. The exceptions are math operations (especially variadic) for which the only
way to use different registry is to set a dynamic variable
`io.randomseed.bankster.registry/*default*` (which can be done using the macro
`io.randomseed.bankster.api.registry/with`).

`registry/new-registry` expects *base maps* (e.g. `:cur-id->cur`, `:ctr-id->cur`,
`:cur-id->localized`, `:cur-id->traits`, `:cur-id->weight`) and builds all derived
index maps during initialization (`:cur-code->curs`, `:cur-nr->cur`, `:cur-nr->curs`,
`:cur-dom->curs`, `:cur-id->ctr-ids`). Even the arity that accepts a Registry
map/record ignores any derived index fields (they are recomputed during
initialization).

When the library loads, its predefined configuration is read from a default EDN file
and its contents populate the default, global registry. This registry can be
modified too.

#### Custom registry initialization (disable auto-init)

```clojure
;; Disable Bankster's default registry auto-initialization (config.edn)
;; at namespace load time. You typically want this when you plan
;; to fully control which registry is used.

(binding [io.randomseed.bankster/*initialize-registry* false]
  (require '[io.randomseed.bankster.api.currency :as currency]
           '[io.randomseed.bankster.api.registry :as registry]))

;; Load your registry from a classpath resource
;; (e.g. resources/my/app/currencies.edn).

(def my-registry
  (currency/config->registry "my/app/currencies.edn"))

;; Option A: pass the registry explicitly (front API).

(currency/resolve     :EUR my-registry)
(currency/resolve-try :EUR my-registry)  ; nil when missing

;; Option B: install it as the global registry.

(registry/set! my-registry)
```

#### Registry loader helpers (`io.randomseed.bankster.init`)

If you want to load a registry in a single call (optionally overlaying Bankster's
distribution config), use `io.randomseed.bankster.init`:

```clojure
(require '[io.randomseed.bankster.init :as init])

;; Load ONLY the provided config (no dist overlay).

(def r1 (init/load-registry "my/app/currencies.edn"))

;; Load dist config first, then overlay user config
;; using importer/merge-registry.

(def r2 (init/load-registry "my/app/currencies.edn"
                            {:keep-dist? true
                             :merge-opts {:verbose? true
                                          :preserve-fields [:domain :kind]
                                          :iso-like? false}}))

;; Side-effecting variant: installs the result as the global registry.

(init/load-registry! "my/app/currencies.edn" {:keep-dist? true})
```

Configuration (`config.edn`) is branch-oriented by default (`:currencies`, plus
top-level branches like `:countries`, `:localized`, `:traits`, and `:weights`).
`:countries` is keyed by country ID (country -> currency), whereas `:localized`,
`:traits`, and `:weights` are keyed by currency ID. For convenience, each currency
entry may also carry inline `:countries` (seq of country IDs), `:localized` and
`:traits` which are merged into the global branches while loading (they do not
become currency fields; per-currency values take precedence). Similarly, a currency
entry may include inline `:weight` (or `:we`) which is normalized into the top-level
`:weights` branch.

If you need selected extra keys from currency maps to be preserved on the constructed
`Currency` objects (extension fields), use `:propagate-keys` in the config (global
allowlist) or per-currency `:propagate-keys` (override).

### Hierarchies and traits

Bankster can store classification hierarchies in a registry and use them for semantic
queries via `clojure.core/isa?`.

Registry key `:hierarchies` is a `CurrencyHierarchies` record that may contain:

* `:domain` – hierarchy of domains (e.g. `:ISO-4217-LEGACY` is a kind of `:ISO-4217`);
* `:kind` – hierarchy of currency kinds (your taxonomy, including namespaced kinds);
* `:traits` – optional hierarchy of traits (advisory; useful for tags like token standards).

Hierarchy specs are expressed as a parent-map `{child parent}` and may also use a
vector/set of parents for multiple inheritance.

Traits are stored separately as a registry map `currency-id -> traits` (sets/vectors
of keywords). They are intentionally *not* part of currency identity and do not
affect money equality or arithmetic.

```clojure
(require '[io.randomseed.bankster.api.registry :as registry]
         '[io.randomseed.bankster.api.currency :as currency])

(def r
  (assoc (registry/new
          {:hierarchies {:domain {:ISO-4217-LEGACY :ISO-4217
                                  :CRYPTO          :VIRTUAL}
                         :traits {:token/erc20 :token/fungible
                                  :token/bep20 :token/fungible
                                  :stable/coin [:stable :token/fungible]}}})
         :cur-id->traits {:crypto/USDC #{:token/erc20 :stable/coin}}))

;; Domains can be queried using a hierarchy-aware predicate.

(currency/of-domain? :ISO-4217 (currency/new :iso-4217-legacy/ADP) r)
;; => true

;; Traits can be queried via the traits hierarchy.

(currency/of-trait? :token/fungible :crypto/USDC r)
;; => true
```

To extend a hierarchy programmatically use `api-registry/hierarchy-derive` (pure) or
`api-registry/hierarchy-derive!` (mutates the global registry).

### Currency

Each **currency** (`Currency`) is a record having the following fields, reflecting
its properties:

* `id` – a keyword **identifying** a currency unit (e.g. `:EUR` or `:crypto/ETH`);
* `numeric` – a long value being a **numeric identifier** of ISO-standardized currencies;
* `scale` – an integer of supported **scale** (decimal places);
* `kind` – a keyword with currency **kind**;
* `domain` – a keyword with currency **domain**.

Additionally, currency may carry a non-inherent attribute `:weight` (an integer),
used only to resolve conflicts when multiple currencies share the same currency code
or numeric ID (lower weight wins).

The source of truth for weights is the registry base map `:cur-id->weight` (exported
to EDN under top-level `:weights`). Currency instances stored in registries carry the
weight in metadata as an optimization (hot-path: weighted buckets), but weight does
not affect `=` / `hash` semantics. Use `api-currency/weight` to read it.

To mutate weights and traits (which are registry attributes) use:
`api-currency/set-weight`/`api-currency/clear-weight` and
`api-currency/set-traits`/`api-currency/add-traits`/`api-currency/remove-traits` (plus `!`
variants operating on the global registry).

**Currency ID** is a unique identifier of a currency within a registry. Internally it
is a keyword and optionally it can have a namespace. By default Bankster identifies
standard currencies with IDs reflecting their official currency codes and
cryptocurrencies with identifiers having `crypto` namespace.

**Currency code** is a name of currency ID without its namespace. It is potentially
conflicting attribute, therefore the mapping of currency codes to sets of currency
objects exists in a registry. It allows to get currencies using their codes (and not
add namespace, especially when interacting with some external API) and still maintain
uniqueness of identifiers. If custom currency is created with the same code as
already existing currency, it is possible to give it a **weight** (lower weight wins)
which will decide whether its code will have priority during resolution (and getting
from a registry).  Currency `:weight` is a registry resolution mechanism and is
treated as non-semantic in equality and arithmetic on monetary values.

**Currency domain** is a keyword which groups currencies into separate "worlds". By
default it is derived from the namespace of currency ID (upper-cased), e.g.
`:crypto/ETH` implies `:domain :CRYPTO`. Namespace `ISO-4217` is treated specially:
it is stripped (case-insensitive) and implies `:domain :ISO-4217`. Additionally,
ISO-like currencies may get `:ISO-4217` inferred from their code + numeric ID.

Domains can be organized in a registry hierarchy (`api-registry/hierarchy :domain`) and
queried with `api-currency/of-domain?`. To list or select currencies by domain, use
`api-currency/domains` and `api-currency/of-domain`.

**Currency kind** is a case-sensitive keyword that should describe what the currency
is. It can be set to anything, even nil. Some common values are:

  - `:iso`       - ISO-4217 currencies, funds, commodities, special markers.
  - `:virtual`   - Virtual units (stable tokens, credits, native tokens, special).
  - `:currency`  - Meta: currency-like units; parent for `:fiduciary` money.
  - `:asset`     - Meta: value-bearing units (`:assets`, `:claims`, `:stable`, reference-based).
  - `:fiat`      - Meta umbrella: fiat-related tags (issuer fiats vs fiat-as-anchor).
  - `:funds`     - Meta: funds, settlement units, units of account.
  - `:commodity` - Meta: commodity-based units and commodity anchoring.
  - `:special`   - Meta: special-purpose markers (`:experimental`, `:test`, `:null`).

In new code it is recommended to use namespaced kinds (for example: `:iso/fiat`,
`:iso/funds`, `:virtual/native`, `:virtual/token`, `:virtual.stable.peg/fiat`).

Kinds can be organized in a registry hierarchy (`api-registry/hierarchy :kind`) and
queried with `api-currency/of-kind?`.

Currencies can can be tested against their ancestors in a hierarchy of kinds using
API functions, so doing `(api-currency/of-kind :fiat :PLN)` will return `true` since this
currency has its kind set to `:iso/fiat`.

The default kind taxonomy and its relationships (as shipped in the default
`config.edn`) are documented in [Currency Kinds](doc/30_currency-kinds.md).
The default traits taxonomy is documented in [Currency Traits](doc/32_currency_traits.md).

**Currency scale** is the nominal scale of a currency. If it is not set, the
automatic scale will be used on monetary amounts using such a currency.

Registry-related functions are accepting currency representations based on their
**identifiers**. Other functions and macros will usually accept **currency
codes**. In case of conflict the currency with lower **currency weight** will be
picked up.

Currencies can also have **additional**, external properties, like relations to
countries, localized (l10n) settings etc. They are stored in registries too.

To inspect a currency including registry-associated metadata (countries, localized
properties, traits) use `api-currency/info`.

### Money

Having a currency defined we can create **money** objects (`Money`) which are based
on records having 2 fields:

* `currency` – a `Currency` object;
* `amount` – a `BigDecimal` value.

The initial scale (number of decimal places) of an amount will be set to nominal
scale of the currency. Math operations will then respect this scale during
calculations and preserve it. In rare cases it is possible to rescale the amount,
check whether the monetary object is rescaled and scale it back to a scale of the
currency.

#### Rounding mode (performance note)

Rounding in Bankster is controlled by dynamic vars from `io.randomseed.bankster.scale`:
`scale/*rounding-mode*` (and for some multi-arg operations also `scale/*each*`).

Prefer using Bankster macros:

* `api-money/with-rounding` (alias for `scale/with-rounding`) to set the rounding mode;
* `api-money/with-rescaling` / `scale/with-rescaling` to also rescale after each step.

These macros keep semantics consistent with a plain `binding`, but they also set an
internal thread-local fast path for rounding-mode lookups. Direct `binding` of
`scale/*rounding-mode*` is supported, but does not use this fast path and may be
noticeably slower in tight numeric loops.

### Importing and merging registries

Namespace `io.randomseed.bankster.util.importer` provides practical tooling for
building and merging currency registries from external sources (most notably: Joda
Money CSV files).

Highlights:

* CSV loader supports comment lines and inline `# ...` comments (preserved for
  post-processing).

* Joda currency comments are used to infer ISO legacy IDs ("Old, now ...") and ISO
  funds kinds ("FundsCode ...").

* When ISO legacy is inferred, Bankster also tags the currency with trait `:legacy`.

* When merging `seed.edn` with Joda CSV imports, a practical default is to treat
  `seed.edn` as the semantic authority (domain/kind/traits/weights/localized), while
  refreshing `:numeric` and `:scale` from the CSV. This is expressed via
  `importer/merge-registry` + `preserve-fields` (and optionally `::importer/countries`
  if you want to preserve assigned countries from the seed).

* `importer/merge-registry` merges registries together with `:hierarchies` and
  `:ext`, supports preserving selected currency fields (and/or localized properties
  and assigned countries), and can align ISO vs legacy classification in "ISO-like"
  mode.

* Export helpers:
  - `importer/export` writes a branch-oriented config (as produced by `registry->map`),
  - `importer/export-currency-oriented` writes a currency-oriented config (per-currency
    `:countries/:localized/:traits` embedded into `:currencies`, top-level branches keep
    only orphaned entries),
  - `importer/map->currency-oriented` is the post-process transformer (map -> map).

```clojure
(require '[io.randomseed.bankster.util.importer :as importer])

;; Generate a Bankster EDN export from Joda Money CSV resources.
(importer/joda->bankster-export)

;; Merge two registries (verbose + preserve :domain/:kind and localized props, while
;; letting ISO-like source align ISO vs legacy classification).
;;
;; In this setup `seed.edn` stays authoritative for domain/kind and localized props,
;; while numeric/scale (and countries, unless preserved) are refreshed from CSV.
(let [dst (importer/seed-import)
      src (importer/joda-import)]
  (importer/merge-registry dst src true [:domain :kind ::importer/localized] true))

;; Export a registry in two equivalent shapes.
(importer/export dst)                    ; branch-oriented
(importer/export-currency-oriented dst)  ; currency-oriented
```

### Experimental: JSR-354 semantic bridge

Namespace `io.randomseed.bankster.jsr-354` is an experimental, work-in-progress
Clojure semantic bridge inspired by JSR-354 (JavaMoney). It is not a Java
implementation/interface of the standard. The goal is to progressively cover more
of JSR-354 semantics in future Bankster releases; until then, treat this namespace
as unstable.

## Quickstart

Bankster models currencies and monetary amounts. The recommended entrypoint is the
Front API (`io.randomseed.bankster.api.*`) and the default registry.

```clojure
;; deps.edn

{io.randomseed/bankster {:mvn/version "2.2.2"}}

;; Leiningen

[io.randomseed/bankster "2.2.2"]
```

```clojure
(require
  '[io.randomseed.bankster.api          :as   b]
  '[io.randomseed.bankster.api.money    :as   m]
  '[io.randomseed.bankster.api.currency :as   c]
  '[io.randomseed.bankster.api.ops      :as ops])
```

```clojure
(def usd (c/resolve "USD"))
(def pln (c/resolve "PLN"))
(def a   (m/resolve 12.34M usd))
(def b   (m/resolve 10M pln))
(ops/+ a (m/resolve 1.66M usd))
(c/resolve-try "NOT_A_CURRENCY") ; => nil
```

**Strict** variants throw on missing currency/match; **soft** variants (usually
`-try`) return `nil`. Use soft at boundaries (parsing/import), strict in core
domain logic. Quickstart uses the default registry; registry scoping options are
described in the docs.

* For demonstrative snippets see [Sneak Peeks](doc/11_sneak-peeks.md).

* For more complete, runnable examples see the `examples/` directory in the [source
  repository](https://github.com/randomseed-io/bankster/tree/main/examples).

* To get the list of available Front API forms visit [API](doc/12_api.md).

* To know contracts governing the library see [Bankster Contracts](doc/15_contracts.md).

## Pitfalls

A few things that can bite you in production if you treat money/currency like "just
numbers and strings":

* **Don't use `clojure.core/=` to compare Money.**

BigDecimal equality is scale-sensitive (1.0 vs 1.00), so value-equality can surprise
you. Use Bankster’s dedicated predicates (e.g. `money/eq?`, `money/==`) or the
operator layer (`money.inter-ops`, `money.api.inter-ops`) depending on the semantics
you want.

* **Be careful with untrusted input – avoid keyword interning.**

Turning arbitrary user data into keywords can leak memory (interning is
global). Prefer string IDs/codes and the helper functions intended for untrusted
values (e.g. `to-id-str`, `to-code-str`), then resolve currencies through the
registry.

* **Non-terminating division requires an explicit policy.**

BigDecimal division can throw for non-terminating results unless you provide
a rounding/rescaling policy. In Bankster, use the rounding/rescaling helpers (or the
Front API variants that accept a rounding mode) so the behavior is deterministic and
documented.

* **Know whether you want strict or soft behavior at the boundaries.**

Bankster provides strict (throwing) and soft (nil-returning) variants (e.g. `resolve`
vs `resolve-try`, `money` vs `money-try`). A good pattern is: soft at system
boundaries (parsing/import), strict in the core business logic.

* **Registries are a feature – but choose the scoping deliberately.**

Registry can be global, dynamically scoped, or passed explicitly. Pick one policy per
subsystem to avoid "it works on my machine" issues (especially in tests and
multi-tenant setups).

### Warning about literal amounts

Clojure changes number literals into objects of various numeric data types.
Some of them will have fixed precision when there is a decimal separator
present, yet they will not be big decimals before entering monetary functions of
Bankster.

Putting a decimal number having more than 16–17 digits will often result in
**accidental approximation** and casting it to a double value. This value may
become the amount of money which probably is not what you want:

```clojure
1234.5678910111213000001
; => 1234.5678910111212
```

To work around that you should:

* Use **big decimal literals** (e.g. `(api-money/of XXX 1234.56789101112M)` – note the `M`).
* Use **strings** (e.g. `(api-money/of "1234.56789101112 XXX")`).
* Use `api-money/of` macro or `#money` tagged literal with amount and currency in joint
  form (or with the above tactics applied), e.g.:
  * `(api-money/of XXX123.45678)`,
  * `#money XXX123.45678`,
  * `#money "XXX123.45678"`,
  * `#money "123.456789101112 XXX"`,
  * `#money[123.45678M XXX]`.

As it may not be a problem in case of regular currencies, it may pop-up when using
scale-wide cryptocurrencies, like Ether or Ethereum tokens, having 18 decimal places.

## Documentation

Full documentation is available at:

* https://randomseed.io/software/bankster/
* https://cljdoc.org/d/io.randomseed/bankster/

## Why?

In one of my personal projects I needed support for both, ISO-standardized and custom
currencies. My first try was Money (by Clojurewerkz), which is quite mature library
based on Java's Joda Money. However, I needed cryptocurrencies support, and I mean
all of them, including those having non-standard codes (like `DASH`).

First I tried to modify Money and work-around this limitation by imitating such
currencies with an additional map translating custom codes into standardized
ones. Then I looked at Joda Money to see that the important classes are marked as
final and the support for currencies is limited to the "official" ones.

## License

Copyright © 2021–2026 Paweł Wilk

This project is dual-licensed: you may use it under the terms of either

- the GNU Lesser General Public License v3.0 or later (LGPL-3.0-or-later), or
- the Apache License, Version 2.0 (Apache-2.0),

at your option.

See the license texts in:

- [`LICENSES/LGPL-3.0-or-later.txt`](LICENSES/LGPL-3.0-or-later.txt)
- [`LICENSES/Apache-2.0.txt`](LICENSES/Apache-2.0.txt)

SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

## Development

### Building docs

```bash
make docs
```

### Building JAR

```bash
make jar
```

### Updating POM

```bash
make pom
```

### Preparing for release

```bash
make release
```

### Deploying to Clojars

```bash
make deploy
```

### Interactive development

```bash
bin/repl
```

Starts REPL (and optionally nREPL server with port number is stored in `.nrepl-port`).
