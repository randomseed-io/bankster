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

## Contracts

See [Bankster Contracts][CONTRACTS] for practical contracts (what is guaranteed,
what is "soft" vs "strict", how the default registry is chosen, when exceptions are
thrown, how the protocols behave) for Bankster's core axis: `Currency`, `Money`,
`Registry` records and the `Monetary`, `Scalable` and `Accountable` protocols.

## Installation

To use Bankster in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/bankster "2.0.0"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/bankster {:mvn/version "2.0.0"}
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
* currency numeric ID to a sorted set of currency objects (weighted).

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
`io.randomseed.bankster.currency/with-registry`).

When the library loads, its predefined configuration is read from a default EDN file
and its contents populate the default, global registry. This registry can be
modified too.

Configuration (`config.edn`) is branch-oriented by default (`:currencies`, plus
top-level indices like `:countries`, `:localized` and `:traits`). For convenience,
each currency entry may also carry inline `:countries`, `:localized` and `:traits`
which are merged into the global branches while loading (they do not become currency
fields). Similarly, a currency entry may include inline `:weight` (or `:we`) which is
normalized into the top-level `:weights` branch.

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
(require '[io.randomseed.bankster.registry :as registry]
         '[io.randomseed.bankster.currency :as currency])

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

To extend a hierarchy programmatically use `registry/hierarchy-derive` (pure) or
`registry/hierarchy-derive!` (mutates the global registry).

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
not affect `=` / `hash` semantics. Use `currency/weight` to read it.

To mutate weights and traits (which are registry attributes) use:
`currency/set-weight`/`currency/clear-weight` and `currency/set-traits`/`currency/add-traits`/`currency/remove-traits`
(plus `!` variants operating on the global registry).

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
which will decide
whether its code will have priority during resolution (and getting from a registry).
Currency `:weight` is a registry resolution mechanism and is treated as non-semantic
in equality and arithmetic on monetary values.

**Currency domain** is a keyword which groups currencies into separate "worlds". By
default it is derived from the namespace of currency ID (upper-cased), e.g.
`:crypto/ETH` implies `:domain :CRYPTO`. Namespace `ISO-4217` is treated specially:
it is stripped (case-insensitive) and implies `:domain :ISO-4217`. Additionally,
ISO-like currencies may get `:ISO-4217` inferred from their code + numeric ID.

Domains can be organized in a registry hierarchy (`registry/hierarchy :domain`) and
queried with `currency/of-domain?`.

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

Kinds can be organized in a registry hierarchy (`registry/hierarchy :kind`) and
queried with `currency/of-kind?`.

Currencies can can be tested against their ancestors in a hierarchy of kinds using
API functions, so doing `(currency/of-kind :fiat :PLN)` will return `true` since this
currency has its kind set to `:iso/fiat`.

The default kind taxonomy and its relationships (as shipped in the default
`config.edn`) are documented in `doc/30_currency-kinds.md`. The default traits
taxonomy is documented in `doc/32_currency_traits.md`.

**Currency scale** is the nominal scale of a currency. If it is not set, the
automatic scale will be used on monetary amounts using such a currency.

Registry-related functions are accepting currency representations based on their
**identifiers**. Other functions and macros will usually accept **currency
codes**. In case of conflict the currency with lower **currency weight** will be
picked up.

Currencies can also have **additional**, external properties, like relations to
countries, localized (l10n) settings etc. They are stored in registries too.

To inspect a currency including registry-associated metadata (countries, localized
properties, traits) use `currency/info`.

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

* `money/with-rounding` (alias for `scale/with-rounding`) to set the rounding mode;
* `money/with-rescaling` / `scale/with-rescaling` to also rescale after each step.

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

## Sneak peeks

* It **shows information** about a currency:

```clojure
;; global registry lookup with a keyword
(currency/of :PLN)
#currency{:id :PLN, :domain :ISO-4217, :kind :iso/fiat, :numeric 985, :scale 2}

;; global registry lookup using namespaced symbol
(currency/of crypto/ETH)
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :virtual/native, :scale 18, :weight 5}

;; global registry lookup with a string (incl. namespace a.k.a domain)
(currency/of "crypto/BTC")
#currency{:id :crypto/BTC, :domain :CRYPTO, :kind :virtual/native, :scale 8, :weight 5}

;; global registry lookup with a currency code
;; (weight solves potential conflicts when two currencies have the same currency code)
(currency/of ETH)
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :virtual/native, :scale 18, :weight 5}

;; global registry lookup using ISO currency number
(currency/of 840)
#currency{:id :USD, :domain :ISO-4217, :kind :iso/fiat, :numeric 840, :scale 2}

;; global registry lookup using tagged literal with a currency code
#currency XLM
#currency{:id :crypto/XLM, :domain :CRYPTO, :kind :virtual/native, :scale 8}

;; global registry lookup using tagged literal with a namespaced identifier
#currency crypto/XLM
#currency{:id :crypto/XLM, :domain :CRYPTO, :kind :virtual/native, :scale 8}

;; global registry lookup using tagged literal with an ISO currency number
#currency 978
#currency{:id :EUR, :domain :ISO-4217, :kind :iso/fiat, :numeric 978, :scale 2}

;; Full currency information (including registry metadata).
(currency/info :PLN)
{:id :PLN,
 :numeric 985,
 :scale 2,
 :domain :ISO-4217,
 :kind :iso/fiat,
 :weight 0,
 :countries #{:PL},
 :localized {:pl {:name "złoty polski", :symbol "zł"}}}

(currency/info :crypto/USDC)
{:id :crypto/USDC,
 :numeric -1,
 :scale 8,
 :domain :CRYPTO,
 :kind :virtual.stable.peg/fiat,
 :weight 4,
 :localized {:* {:name "USD Coin", :symbol "USDC"}},
 :traits #{:peg/fiat :stable/coin :token/erc20}}
```

* It allows to **create a currency** and **register it**:

```clojure
;; ad hoc currency creation using constructor function
(currency/new :petro/USD 999 2 :COMBANK)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :numeric 999, :scale 2}

;; ad-hoc currency creation using tagged literal
#currency{:id :crypto/ETH :scale 18}
#currency{:id :crypto/ETH, :domain :CRYPTO, :scale 18}

;; putting new currency into a global, shared registry
(currency/register! (currency/new :petro/USD 9999 2 :COMBANK) :USA)
#Registry[{:currencies 221, :countries 250, :version "2021022121170359"} 0x11efe93f]

;; getting currency from a global registry
(currency/of :petro/USD)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :numeric 9999, :scale 2}

;; registering new currency expressed as a tagged literal
(currency/register! #currency{:id :crypto/AAA :scale 8})
#Registry[{:currencies 221, :countries 249, :version "2021022121170359"} 0x7eaf7a70]

;; creating an ISO currency (must have: a simple 3-letter code w/o ns and a numeric ID)
(currency/new :XOX 999 2 :COMBANK)
#currency{:id :XOX, :domain :ISO-4217, :kind :COMBANK, :numeric 999, :scale 2}

;; creating a strange ISO currency (forced by a namespace but w/o a numerical ID)
(currency/new :ISO-4217/XOX nil 2 :COMBANK)
#currency{:id :XOX, :domain :ISO-4217, :kind :COMBANK, :scale 2}
```

* It allows to create **monetary amounts**:

```clojure
;; using money/of macro with keyword ID and an amount
(money/of :EUR 25)
#money[25.00 EUR]

;; using money/of macro with keyword ID and an amount as a first argument
(money/of 25 :EUR)
#money[25.00 EUR]

;; using money/of macro with joint keyword ID and an amount as a first argument
(money/of :25_EUR)
#money[25.00 EUR]

(money/of :25EUR)
#money[25.00 EUR]

;; using money/of macro with unquoted symbolic ID and an amount
(money/of EUR 25)
#money[25.00 EUR]

;; using money/of macro with joint unquoted symbolic ID and an amount
(money/of EUR_25)
#money[25.00 EUR]

(money/of EUR25)
#money[25.00 EUR]

;; using money/of macro with namespaced keyword ID and an amount
(money/of crypto/BTC 10.1)
#money/crypto[10.10000000 BTC]

;; using money/of macro with currency code and an amount
(money/of BTC 10.1)
#money/crypto[10.10000000 BTC]

;; using tagged literals
#money EUR
#money[0.00 EUR]

#money/crypto ETH
#money/crypto[0.000000000000000000 ETH]

#money[PLN 2.50]
#money[2.50 PLN]

;; using tagged literal with a namespace
#money/crypto[1.31337 ETH]
#money/crypto[1.313370000000000000 ETH]

;; using tagged literal with a currency code
#money[1.31337 ETH]
#money/crypto[1.313370000000000000 ETH]

;; using tagged literal with a namespace but the amount goes first
#money/crypto[BTC 1.31337]
#money/crypto[1.31337000 BTC]

;; using default currency in a lexical context
(currency/with EUR (money/of 1000))
#money[1000.00 EUR]

;; using default currency in a lexical context (alias for the above)
(money/with-currency EUR (money/of 1000))
#money[1000.00 EUR]

;; using composed amounts and currencies
#money EUR100
#money[100 EUR]

#money :100_EUR
#money[100 EUR]

#money :100EUR
#money[100 EUR]

#money "100 EUR"
#money[100 EUR]

(money/of "100EUR")
#money[100 EUR]
```

It allows to perform **logical operations** on monetary amounts:

``` clojure
(money/eq? #money[5 GBP] #money[GBP 5])
true

(money/ne? #money[5 GBP] #money[GBP 5])
false

(money/eq? #money[5 GBP] #money/crypto[5 ETH])
false

(money/gt? #money[1 JPY] #money[0 JPY])
true

(money/ge? #money[1 JPY] #money[1 JPY])
true

(money/lt? #money[1 JPY] #money[0 JPY])
false

(money/le? #money[1 JPY] #money[0 JPY])
false

(money/zero? #money[0 USD])
true

(money/neg? #money[-2 XXX])
true

(money/pos? #money[-2 XXX])
false
```

It allows to perform **math operations** on monetary amounts:

``` clojure
;; adding money expressed with tagged literals and with a macro call
(money/add #money[EUR 7] #money[0.54 EUR] (money/of 4.40 EUR))
#money[11.94 EUR]

;; dividing money by a number
(money/div #money/crypto[5 USDT] 2)
#money/crypto[2.50000000 USDT]

;; dividing money by numbers that separately would require rounding
(money/div #money[1 GBP] 8 0.5)
#money[0.25 GBP]

;; dividing money by numbers with rounding after each consecutive calculation
(money/with-rescaling HALF_UP
  (money/div #money[1 GBP] 8 0.5))
#money[0.26 GBP]

;; dividing money by money (of the same currency)
(money/div #money/crypto[5 BTC] #money/crypto[2 BTC])
2.5M

;; dividing causing scale to exceed in one of the steps
;; but no rounding is necessary due to later operation
(money/div #money[1 PLN] 8 0.5)
#money[0.25 PLN]

;; dividing, scaled and rounded with each operation
(scale/with-rounding HALF_UP
  (money/div-scaled #money[1 PLN] 8 0.5))
#money[0.26 PLN]

;; same as the above but shorter
(scale/with-rescaling HALF_UP
  (money/div #money[1 PLN] 8 0.5))
#money[0.26 PLN]

;; handling non-terminating decimal expansion
(scale/with-rounding HALF_UP
  (money/div #money[1 PLN] 3))
#money[0.33 PLN]

;; rounding with unit reduction
(scale/with-rounding HALF_UP
  (money/div #money[1 PLN] #money[3 PLN]))
0.33M

;; rounding and unit reduction (regular numbers, dynamic scale)
(scale/with-rounding HALF_UP
  (money/div 1 3))
0.33333M

;; multiplying money by numbers
(money/mul #money/crypto[5 ETH] 1 2 3 4 5)
#money/crypto[600.000000000000000000 ETH]

;; adding to major part
(money/add-major #money[1.23 PLN] 100)
#money[101.23 PLN]

;; adding to minor part
(money/add-minor #money[1.23 PLN] 77)
#money[2.00 PLN]

;; converting
(money/convert #money/crypto[1.5 ETH] :crypto/USDT 1646.75)
#money/crypto[2470.12500000 USDT]

;; comparing
(sort money/compare [(money/of 10    PLN)
                     (money/of  0    PLN)
                     (money/of 30    PLN)
                     (money/of  1.23 PLN)])
(#money[0.00 PLN]
 #money[1.23 PLN]
 #money[10.00 PLN]
 #money[30.00 PLN])

;; rounding to the given interval

(money/round-to #money[31.33 USD] 0.5)
#money[31.50 USD]

;; allocation

(money/allocate #money[10.00 PLN] [1 1 1])
[#money[3.34 PLN]
 #money[3.33 PLN]
 #money[3.33 PLN]]

(money/allocate #money[1.00 PLN] [1 2 3])
[#money[0.17 PLN]
 #money[0.33 PLN]
 #money[0.50 PLN]]

;; distribution

(money/distribute (money/of 1 PLN) 3)
[#money[0.34 PLN]
 #money[0.33 PLN]
 #money[0.33 PLN]]

(money/distribute (money/of 3 PLN) 3)
[#money[1.00 PLN]
 #money[1.00 PLN]
 #money[1.00 PLN]]

;;
;; using inter-ops
;;

(require '[io.randomseed.bankster.money.inter-ops :refer :all])

(+ 1 2 3)
6

(+ #money[USD 8] #money[USD 7.12])
#money[15.12 USD]

(* 1 2 3 4 5 #money/crypto[0.7 ETH])
#money/crypto[84.000000000000000000 ETH]
```

It allows to perform **generic, polymorphic operations** on monetary amounts and
currencies:

```clojure
(scale/of #currency PLN)
2

(scale/of #currency crypto/ETH)
18

(scale/of 123.45)
2

(scale/of #money[100 EUR])
2

(scale/of :GBP)
2

;; scale of the currency (low-level)
(scale/of :XXX)
-1

;; scale of the amount
(scale/of #money[12.34567 XXX])
5 ; current scale

;; nominal scale of the currency
(currency/scale #money[12.34567 XXX])
nil ; auto-scaled

(currency/auto-scaled? :XXX)
true

(currency/auto-scaled? #money[12.34567 XXX])
true

(scale/apply #money[10 USD] 8) ;; use with caution
#money[10.00000000 USD]

(scale/apply #currency USD 8)  ;; use with caution
#currency{:id :USD, :domain :ISO-4217, :kind :iso/fiat, :numeric 840, :scale 8}

(money/rescale #money[10 USD] 8)
#money[10.00000000 USD]

;; unary variant of money/rescale
;; rescales back to nominal scale
(money/rescale
 (money/rescale #money[10 USD] 8))
#money[10.00 USD]

(scale/amount #money[108.11 CHF])
108.11M

(scale/integer #money[108.11 CHF])
108M

(scale/fractional #money[108.11 CHF])
11M

(currency/iso? #money[1 GBP])
true

(currency/code #money[1 GBP])
"GBP"
```

And more…


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

* Use **big decimal literals** (e.g. `(money/of XXX 1234.56789101112M)` – note the `M`).
* Use **strings** (e.g. `(money/of "1234.56789101112 XXX")`).
* Use `money/of` macro or `#money` tagged literal with amount and currency in joint
  form (or with the above tactics applied), e.g.:
  * `(money/of XXX123.45678)`,
  * `#money XXX123.45678`,
  * `#money "XXX123.45678"`,
  * `#money "123.456789101112 XXX"`,
  * `#money[123.45678M XXX]`.

As it may not be a problem in case of regular currencies, it may pop-up when using
scale-wide cryptocurrencies, like Ether or Ethereum tokens, having 18 decimal places.

## Documentation

Full documentation including usage examples is available at:

* https://randomseed.io/software/bankster/

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

Copyright © 2021–2025 Paweł Wilk

Bankster is copyrighted software owned by Paweł Wilk (pw@gnu.org). You may
redistribute and/or modify this software as long as you comply with the terms of
the [GNU Lesser General Public License][LICENSE] (version 3).

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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

[LICENSE]:    https://github.com/randomseed-io/bankster/blob/master/LICENSE
[CONTRACTS]:  https://github.com/randomseed-io/bankster/blob/master/CONTRACTS.md
