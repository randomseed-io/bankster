# History of Bankster releases

## 2.0.0 (2025-01-31)

**BREAKING CHANGES**:

- Registry record changed (added `ext`, added weighted numeric-ID buckets, added `:hierarchies`
  and currency traits base map).
- Macro-based registry accessors renamed to `*` variants.
- Monetary protocol refactored/extended (new coercions, stricter contractual behavior).
- Currency weight (`:weight`) moved out of the `Currency` record fields into metadata, so it
  no longer participates in `=` / `hash` semantics.
  Weight is now treated as a registry attribute stored in `Registry.cur-id->weight`
  and exported under top-level `:weights` in EDN configs/exports; registry `Currency`
  instances also carry weight in metadata as an optimization. Use `currency/weight`
  (and `currency/with-weight`) to work with it.

Currency / registry:

- Registry hierarchies support added (stored in `:hierarchies`), including custom axes and
  multi-parent relationships. New helpers: `registry/hierarchies`, `registry/hierarchy`,
  `registry/hierarchy-derive`, `registry/hierarchy-derive!`.
- Side-effectful global registry initialization performed at
  `io.randomseed.bankster.currency` namespace load time can now be disabled by binding
  `io.randomseed.bankster/*initialize-registry*` to `false` around `require` (default is
  `true`).
- Currency kind can now be namespaced and case-sensitive (e.g. `:iso/fiat`, `:virtual/token`);
  `currency/of-kind?` can consult the kind hierarchy (renamed from `currency/kind-of?`).
- Added `currency/of-domain?` (domain-aware predicate) and `currency/null?` / `currency/none?`.
- Added `currency/info` (full currency info map: fields + registry metadata).
- Added currency traits base map (`:traits` in config, `cur-id->traits` in registry) + export/import support.
- Added `currency/has-trait?` (exact membership) and `currency/of-trait?` (hierarchy-aware) predicates.
- Added narrow public APIs to mutate registry-level weights and traits:
  `currency/set-weight`, `currency/clear-weight`, `currency/set-traits`, `currency/add-traits`,
  `currency/remove-traits` (+ `!` variants).
- Fixed a bug that caused currency ID to be used instead of currency code during code-bucket cleanup
  (stale entries in `:cur-code->curs` after unregister).
- Currency constructor improved (nil handling, automatic `:ISO-4217` domain inference); ISO helpers tightened.
- Registry diagnostics added (`*warn-on-inconsistency*` + `inconsistency-warning`).

Importer / data:

- CSV reader now supports comment lines and inline comments (preserved for post-processing).
- Joda CSV importer recognizes "Old, now ..." (ISO legacy) and "FundsCode ..." (sets kind to `:iso/funds`).
- When ISO legacy is inferred ("Old, now ...", ISO-like merges), Bankster also tags the currency with
  trait `:legacy`.
- EDN config loader supports inline per-currency `:countries`, `:localized`, and `:traits` keys (merged into
  the top-level branches) and `:propagate-keys` (allowlist for propagating selected extra currency-map keys
  into Currency extension fields).
- Added currency-oriented export helpers: `importer/map->currency-oriented`,
  `importer/registry->map-currency-oriented`, `importer/export-currency-oriented`.
- `merge-registry` enhanced: merges `:hierarchies` and `:ext`, supports preserving selected currency fields
  (including localized properties and countries), adds ISO-like mode for ISO vs legacy alignment, and supports
  verbose reporting of new/updated currencies.

Money:

- Money currency comparisons/equality ignore currency `:weight` (weight is for code resolution only).
- Varargs `money/add` and `money/sub` now also treat `:weight` as non-semantic (regression test added).
- Unary `money/div` no longer allows `Money` arguments (throws like `number / Money`).
- `io.randomseed.bankster.money.inter-ops` now mirrors `clojure.core` behavior when no `Money` is involved;
  any `Money` argument switches to Bankster arithmetic/comparators.
- Assertions replaced with `ExceptionInfo` (exception data added/normalized).
- Rounding-mode lookups optimized: `scale/with-rounding` / `scale/with-rescaling` (and aliases in `money`)
  now set a thread-local fast path for rounding mode. Prefer these macros over a raw `binding` of
  `scale/*rounding-mode*` in performance-sensitive code.

Tooling / DX:

- Added `CONTRACTS.md` (practical contracts for Currency/Money/Registry + protocols).
- Added experimental `io.randomseed.bankster.jsr-354` namespace (Clojure semantic bridge inspired by JSR-354;
  not a Java interface/implementation; work in progress).
- nREPL support improved; piggieback removed; Logback added for `:dev` and `:test`.
- Coverage/CI fixes and initial perf benchmarks added.

## 1.2.19 (2025-01-19)

- Performance improvements in `money/allocate`.
- AOT compilation enabled for `io.randomseed.bankster` namespace.

## 1.2.18 (2025-01-19)

- Allocation and distribution of monetary amounts via
  `money/allocate` and `money/distribute`.

## 1.2.17 (2025-01-19)

- Building and deployment workflow changed.
- Lazy map support is now realized (sic!) by `io.randomseed/lazy-map`.
- Documentation updated.

## 1.2.16 (2022-11-25)

- `io.randomseed.bankster.util.fs/default-encoding` is no longer private.
- nREPL server is no longer automatically started when loading `infra.clj`.
- Excluded `abs` from being aliased in `io.randomseed.bankster.money`.
- Removed Midje tests, added clojure.test.
- CircleCI configuration updated.
- Deps updated.

## 1.2.15 (2021-12-29)

- Deps updated (codox).

## 1.2.14 (2021-09-24)

- Nil punning added to: `currency/ns-code`, `currency/code`, `currency/weight`,
  `currency/domain`, `currency/kind`, `currency/sc`, `currency/nr`.

## 1.2.13 (2021-09-23)

- Fixed a bug that caused an amount to be a kind of `Money` instead of `BigDecimal`
  when `money/value` was called on currency and money.

## 1.2.12 (2021-07-18)

- Type hinting and assertion tests improved in `money/compare` and `money/compare-amount`.

## 1.2.10, 1.2.11 (2021-07-13)

- Fixed a bug which caused rounding mode parsing to not make use of evaluated results
  of the given expression.

- Improved rounding mode parsing macros.

## 1.2.9 (2021-07-06)

- Fixed type hinting in `fs/absoute-path?`.

- Monetary comparators (`money/compare` and `money/compare-amounts`) now accept `nil` values.

- Added `money/round-to` for interval-based rounding.

## 1.2.8 (2021-06-28)

- Rewritten `fs/absolute-path?` and `fs/relative-path?`

## 1.2.7 (2021-06-17)

- Improved `money/on-amount` (now it rescales results to the scale of a money,
  not the nominal scale of a currency).

- Added alias `money/apply` that points to `money/on-amount`.

- Added `money/set-amount`.

## 1.2.6 (2021-06-02)

- `Monetary` implementation for `Currency` objects fixed for `nil` registry argument.
  Affected function: `currency/unit`.

- Fixed type hinting in `currency/sc` and `currency/nr`.

## 1.2.5 (2021-05-02)

- `Monetary` implementation for maps is more restrictive when registry is passed as an
  argument. Previously maps were causing some functions to create *ad hoc*
  currencies, now the `:id` key is used to get the existing currency from a registry.
  This is in line with the protocol's guidelines. Affected functions: `currency/unit`,
  `currency/of-id`.

- `Monetary` implementation for `Currency` objects is now fulfilling protocol
  specification, making ID-only look-ups. Affected functions: `currency/unit`,
  `currency/of-id`.

- `Monetary` implementation for keywords now falls back to currency code look-up even
  if the given identifier is not namespace-qualified. Affected functions:
  `currency/unit`, `currency/present?`. It fixes *the chicken or the egg* problem during
  registry building.

- Functions `currency/add-countries`, `currency/remove-localized-properties`,
  `currency/add-localized-properties`, `currency/add-weighted-code` and `currency/update`
  are now more strict and require currency ID to be given (not currency code).

**BREAKING CHANGES**:

- Low-level function `currency/add-weighted-currency` renamed to
  `currency/add-weighted-code`.

## 1.2.4 (2021-04-18)

- Added `money/data-readers` and `money/code-readers` constants.

- Added `data_readers_edn.clj` (with generator in `importer/readers-export`) containing
  data readers for parsing EDN files.

- Fixed a bug causing registry to not be properly set in `currency/with-registry` and
  `registry/with`.

- Added `fs/get-resource` utility function.

**BREAKING CHANGES**:

- Data readers for tagged literals are split into code and data related handlers:

  - Functions `currency/code-literal` and `money/code-literal` are now emitting Clojure
    forms that are to be evaluated to produce `Currency` and `Money` objects.

  - Functions `currency/data-literal` and `money/data-literal` are now returning `Currency`
    and `Money` objects.

  - Tagged literal handlers (for both Clojure code and data) can now be controlled by
    the environment (e.g. dynamic variables for setting alternate registry or
    rounding mode during parsing).

  - Constant `bankster/tagged-literals` moved to `money/tagged-literals`.

## 1.2.3 (2021-04-16)

- Fixed issue with data-readers file visibility by adding a copy to resources directory.

## 1.2.2 (2021-04-16)

- Added `io.randomseed.bankster/data-readers` map to expose tagged literal handlers.

## 1.2.1 (2021-04-15)

- Fixed a bug causing `money/of-registry` to not rescale amounts.

##  1.2.0 (2021-04-13)

- Function `money/div-rem` renamed to `money/rem` and improved to behave like `money/div` in
  terms of accepted argument types, rounding and rescaling.

- Added function `money/abs`.

- Added aliases: `money/+`, `money/-`, `money/*`, `money//`, `money/min`, `money/max`.

- Added aliases: `money/>`, `money/>=`, `money/<`, `money/<=`, `money/=`, `money/==`.

- Added aliases: `money/pos?`, `money/neg?`, `money/zero?`.

- Conversion functions `scale/->int`, `scale/->long`, `scale/->double`, `scale/->float`,
  `scale/to-plain-string`, `scale/to-clojure-string`, `scale/to-symbol`,
  `scale/to-clojure-symbol` are now using `scale/amount` to get the actual amount of the
  given scalable instead of expecting `BigDecimal` values.

**BREAKING CHANGES**:

- Function `money/pos` now returns its argument. Use `money/abs` to get the absolute
  value.

## 1.1.3 (2021-04-12)

- Dependencies updated.

## 1.1.2 (2021-04-05)

- `Monetary` protocol methods are now more strict about the origin of currency
  if a registry is given (by-ID resolving and getting from a registry happens
  even for `Currency` objects).

- Unary variants of `money/div` and `money/div-scaled` are now able to take `Money` as
  arguments (common operation when recalculating exchange rates).

- Protocol method implementations of `money/value` short-circuit on `nil` given as an
  amount.

- Added functions: `money/auto-scaled?`, `money/of-registry` and `money/on-amount`.

- Fixed a bug causing division and multiplication of auto-scaled currencies to
  rescale results to latest scales of the amounts instead of performing scale-free
  calculations.

- Added stripping of trailing zeros before performing scale-free divisions – causes
  non-terminal decimal expansion to end after more optimal (lesser) number of decimal
  digits.

## 1.1.1 (2021-03-26)

- Improved parsing of rounding modes.

## 1.1.0 (2021-03-25)

- Added `currency/update` for updating currencies with additional data preservation
  (localized properties, countries, etc.).
- Added `money/cast` for casting monetary amounts across currencies
  (different or slightly different, e.g. sourced in another registries).
- Added `scale/to-clojure-string`, `scale/to-symbol`, `scale/to-clojure-symbol`.
- Added `money/->symbol`, `money/->clojure-symbol`, `money/->double`, `money/->float`.
- Improved `money/value` to be more polymorphic (currency argument).
- Improved conversion functions: `scale/->int`, `scale/->long`, `scale/->float`, `scale/->double`.
- Type hinting improved in multiple functions and protocol methods.

**BREAKING CHANGES**:

- Conversion functions now have different arities:
  `scale/->int`, `scale/->long`, `scale/->float`, `scale/->double`.
- Function `scale/to-plain-string` is now converting decimals to plain strings
  without adding `M` suffix when there is too many digits. This functionality
  was moved to `scale/to-clojure-string`.

## 1.0.8 (2021-03-23)

- Function `money/convert` now accepts a price and is more polymorphic.
- Added the function `currency/localized-properties`.

## 1.0.7 (2021-03-19)

- Improved and unified rescaling of `Money` objects when their currency is auto-scaled.
- Functions `money/amount`, `money/stripped-amount` and `money/currency` are now polymorphic.
- Currencies database updated.

## 1.0.6 (2021-03-19)

- Better printing of monetary amounts of big precision (adding `M` letter to a number).
- Better parsing of monetary amounts of big precision (early conversion to big decimals to avoid scientific notation).

## 1.0.5 (2021-03-18)

- Fixed a bug reversing order of weighted currencies.

## 1.0.4 (2021-03-18)

- Added missing `Monetary` implementation functions for `Money`.
- Improved `currency/id` `Monetary` implementation to support currency codes.

## 1.0.3 (2021-03-17)

- Fixed a bug that caused wrong locale detection on some environments,
  caused by redundant localized properties preparation (double parsing
  of locale object initially expressed with a keyword).

## 1.0.2 (2021-03-16)

- Currency codes are now default fallback identifiers of registered currencies.
- Fixed (re-)setting of localized properties during currency registering.

## 1.0.1 (2021-03-15)

- Removed accidental databases from the resources.

## 1.0.0 (2021-03-15)

- Initial release.
