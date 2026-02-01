# Bankster Contracts

This document describes practical contracts (what is guaranteed, what is "soft" vs
"strict", how the default registry is chosen, when exceptions are thrown, how the
protocols behave) for Bankster's core axis: `Currency`, `Money`, `Registry` records
and the `Monetary`, `Scalable` and `Accountable` protocols.

This is not an API reference (it does not list all arities), but a guide to behavior.

## 1. Foundations (model and global rules)

- Bankster models money as data: `Money = (Currency, BigDecimal amount)`.
- `Registry` is the source of truth for currencies (resolving by ID/code/numeric-id,
  localization, country relations).
- Computations are based on `BigDecimal` (no silent `double`).
- The "soft" API returns `nil` when no match is found; the "strict" API throws
  `ExceptionInfo`.
- Loading `io.randomseed.bankster.currency` auto-initializes the global registry from
  `config.edn` by default. To disable this side effect (and initialize explicitly),
  bind `io.randomseed.bankster/*initialize-registry*` to `false` around `require`.
- Namespace `io.randomseed.bankster.jsr-354` is an experimental, work-in-progress
  Clojure semantic bridge inspired by JSR-354 (JavaMoney). It is not a Java
  implementation/interface of the standard. The goal is to progressively cover more
  of JSR-354 semantics in future Bankster releases; until then, treat this namespace
  as unstable.
- `:weight` is a *registry* attribute used to resolve conflicts when resolving by
  code and numeric ID (in `:cur-code->curs` and `:cur-nr->curs`). Weight is stored
  in the registry base map `:cur-id->weight` (exported as top-level `:weights` in
  EDN). Currency instances stored in registries also carry the weight in metadata as
  an optimization. Weight is ignored by `Currency`/`Money` equality and arithmetic.

### 1.1 Terminology

- Currency ID: a keyword, e.g. `:EUR` or `:crypto/ETH` (namespaced).
- Currency code: an *unqualified* keyword, e.g. `:EUR`, `:ETH` (from `:crypto/ETH`
  you get `:ETH`).
- Domain: a classification of a "currency world" (e.g. `:ISO-4217`, `:CRYPTO`). For
  namespaced currencies it is derived from the namespace (upper-case).
- Kind: a case-sensitive keyword classifying what a currency *is*. It may be
  namespaced (e.g. `:iso/fiat`, `:virtual/token`).
- Traits: advisory tags/features associated with a currency (stored in a registry),
  independent from currency identity and `Money` semantics.
- Scale:
  - for a currency: nominal number of decimal places,
  - for `Money`: the scale of the amount (`BigDecimal` scale).
- Auto-scaled currency: a currency with an "automatic" scale (no fixed nominal
  scale; the amount carries scale).
- Weight: an integer where *lower weight wins* when resolving code / numeric-ID
  conflicts in a registry.

### 1.2 Error model

- Contract violations (wrong arity/typing combination, mismatched currencies, missing
  currency in registry, missing rounding-mode, etc.) are signaled via
  `clojure.lang.ExceptionInfo` (`ex-info`).
- In `ex-data` you will typically find keys like `:registry`, `:currency`, `:value`,
  `:op`, and/or domain-specific keys (`:augend`, `:addend`, `:minuend`,
  `:subtrahend`, `:dividend`, `:divisor`).
- When an error originates from `BigDecimal` arithmetic (Java `ArithmeticException`),
  Bankster rethrows it as `ExceptionInfo` and marks it in `ex-data` with:
  - `:arithmetic-exception true`,
  - `:arithmetic-exception/cause` (the original `ArithmeticException`, also used as
    the exception cause).

## 2. Records (core data)

### 2.1 `io.randomseed.bankster/Currency`

Fields:

- `:id` (keyword) - unique currency identifier within a registry.
- `:numeric` (long) - numeric id (e.g. ISO 4217); absence is represented by a
  sentinel (`no-numeric-id`).
- `:scale` (int) - nominal currency scale; auto-scale is represented by a sentinel
  (`auto-scaled`).
- `:kind` (keyword or nil) - classification (case-sensitive; may be namespaced, e.g.
  `:iso/fiat`, `:virtual/token`).
- `:domain` (keyword or nil) - domain (e.g. `:ISO-4217`, `:CRYPTO`).

Non-inherent attribute:

- `:weight` (int) - weight used to resolve conflicts when resolving by code and/or
  numeric ID (lower wins).
  - Source of truth: registry base map `:cur-id->weight` (`:weights` in EDN config).
  - Registry `Currency` instances carry weight in metadata for hot paths
    (bucket sorting), accessible via `currency/weight`.
  - In EDN config `:weight` may be omitted (implicit 0).
  - Branch-oriented export (`importer/registry->map`) emits weights under top-level
    `:weights` (presence is meaningful; explicit `0` is supported).
  - Currency-oriented export embeds per-currency `:weight` into `:currencies` while
    keeping orphaned `:weights` entries.

Contracts:

- `Currency` is "data first": field values are explicit; there are no hidden side
  effects.
- `(.toString Currency)` returns the *code* (i.e. `(name :id)`), not the full ID. Do
  not rely on `toString` in logs. For stable identification use `currency/id`
  (keyword) or `currency/to-id-str` (string without keyword interning).
- `Currency` values may carry an extension map (extra keys) because they are Clojure
  records. Bankster itself keeps core semantics in record fields and treats
  extension keys as non-semantic metadata.
  - When loading currencies from EDN configuration, extra keys present in currency
    maps are ignored by default.
  - To allow selected extra keys to be preserved on `Currency` objects, use
    `:propagate-keys` (global allowlist) and/or per-currency `:propagate-keys`
    (override). Keys reserved by core currency construction and configuration
    pre-population (e.g. `:id`, `:kind`, `:scale`, `:numeric`, `:domain`, `:weight`,
    `:countries`, `:localized`, `:traits`, and the directive `:propagate-keys`) are
    never propagated.

### 2.2 `io.randomseed.bankster/Money`

Fields:

- `:currency` - a `Currency` object.
- `:amount` - `java.math.BigDecimal`.

Contracts:

- `Money` always carries a `BigDecimal` (inputs are coerced through the `scale`
  layer).
- `(.toString Money)` returns `"AMOUNT CODE"` (code comes from `Currency/toString`).
- The amount scale in `Money` is part of the data (it may differ from the currency's
  nominal scale). This is supported, but has consequences (see `money/rescaled?`,
  `money/rescale`, `money/strip`).

### 2.3 `io.randomseed.bankster/Registry`

The registry is a record with indices (maps), including:

- `:cur-id->cur` - currency ID -> `Currency` (canonical entry).
- `:cur-code->curs` - currency code -> a set of currencies (sorted, "weighted").
- `:cur-nr->cur` - numeric-id -> `Currency`.
- `:cur-nr->curs` - numeric-id -> a set of currencies (sorted, "weighted").
- `:ctr-id->cur` - country ID -> `Currency`.
- `:cur-id->ctr-ids` - currency ID -> a set of country IDs.
- `:cur-id->localized` - currency ID -> localization/properties map.
- `:cur-id->traits` - currency ID -> traits (a set/vector of keywords; advisory).
- `:cur-id->weight` - currency ID -> int weight (conflict resolution).
- `:hierarchies` - a `CurrencyHierarchies` record holding per-axis hierarchies
  (usually at least `:domain` and `:kind`, optionally `:traits` and/or custom axes).
- `:version` - string (timestamp-style).
- `:ext` - extra data (map).

Contracts:

- The global registry lives in `io.randomseed.bankster.registry/R` (Atom).
- In many operations, "default registry" means: the dynamic `registry/*default*`
  (when bound), otherwise the global `registry/R`.
- Registry hierarchies are used by higher-level predicates that rely on `isa?` (e.g.
  `currency/of-domain?`, `currency/of-kind?`, `currency/of-trait?`). Custom axes may be introduced by
  consumers (stored under additional keys in `:hierarchies`).
- When multiple currencies share the same numeric ID, the registry keeps them all in
  `:cur-nr->curs` and picks a canonical one for `:cur-nr->cur` using weight (lower
  wins).

## 3. Protocols

### 3.1 `io.randomseed.bankster.currency/Monetary`

Monetary is the "coercion + currency resolution" layer.

General rule:

- `to-*` methods are cheap and registry-free (may return `nil`).
- `resolve`/`resolve-all` consult a registry and are "soft" (may return `nil`).
- `unit`/`of-id` are "strict" (throw when the currency is missing from the registry),
  with an explicit convention `registry=nil` for already constructed `Currency`
  values (see below).

Key methods and their contracts:

- `to-id` -> keyword:
  - registry-free,
  - may intern keywords (e.g. from a string) - do not use on untrusted input.
- `to-code` -> keyword:
  - registry-free,
  - returns an unqualified code.
- `to-id-str` / `to-code-str` -> String:
  - registry-free,
  - should not intern keywords (safer for untrusted input),
  - canonicalization: upper-case name, namespace preserved (for ID).
- `to-numeric-id` -> long/nil:
  - registry-free hint (for objects that carry a numeric id).
- `to-currency` -> Currency/nil:
  - registry-free; may create an ad-hoc `Currency` (e.g. from a map).
- `to-map` -> map/nil:
  - registry-free; the map may be partial.
- `definitive?` -> boolean:
  - whether the representation carries enough information to make meaningful negative
    property checks (e.g. "definitely not ISO").
- `resolve` -> Currency/nil:
  - soft: returns `nil` when it cannot be resolved.
  - when `registry` is `nil`, uses the default registry.
- `resolve-all` -> set/nil:
  - soft; returns a set of matches or `nil`.
- `id` -> keyword:
  - unary:
    - advisory / soft: returns what it can infer locally,
    - may consult the default registry to disambiguate unqualified currency codes
      (e.g. `:BTC` -> `:crypto/BTC` when such currency exists),
    - does not throw on missing currencies (note: numeric IDs are different - see below).
  - binary: `registry=nil` means "do not consult a registry" (return local
    ID/coercion).
  - binary: when a registry is actually consulted, missing currency -> exception.
    For already constructed `Currency` values the registry is ignored and `.id` is
    returned.
  - binary: `registry` should be a `Registry` (or `nil` as above). The boolean
    sentinel `true` works only syntactically via `(registry/get true)` (macro-level);
    passing `true` as a runtime value is not supported.
  - note: for numeric IDs (numbers), `id` always consults a registry and throws when
    the mapping is missing.
- `of-id` -> Currency:
  - strict: missing currency -> exception,
  - when the argument is a `Currency`: `registry=nil` means "return as-is".
- `unit` -> Currency:
  - strict: missing currency -> exception,
  - when the argument is a `Currency`: `registry=nil` means "return as-is",
  - for maps: maps are treated as masks/constraints (match by key presence), and
    when multiple matches exist, the best match is selected by weight (lower wins).
- `defined?` -> boolean:
  - existence of a currency in a registry (by ID/numeric/code); this is a "does
    anything exist" check, without validating full field-level consistency.
- `present?` -> boolean:
  - checks whether the representation is consistent with the entry in the registry
    (field match).

Soft helpers:

- `currency/attempt` and `currency/attempt*`:
  - soft coercion: returns a `Currency` or `nil`, does not throw.
  - preferred in predicates and "try" paths.

### 3.2 `io.randomseed.bankster.scale/Scalable`

Scalable is the "what is scale" + "how to safely produce BigDecimal" layer.

Methods:

- `scalable?` -> boolean: can this value be coerced to a scalable value?
- `applied?` -> boolean: does the value already carry scale information (e.g.
  `BigDecimal`, `Money`, `Currency`)?
- `of` -> long: scale (for `Money` it's the amount scale; for `Currency` it's the
  currency scale).
- `apply` -> scaled value:
  - for numbers: returns a `BigDecimal`,
  - for `Money`: may rescale the amount; unary `apply` reapplies the currency's
    nominal scale (when the currency is not auto-scaled).
  - for `Currency`: returns a `Currency` with updated `:scale`.
- `amount` -> BigDecimal:
  - for `Money`: returns the amount,
  - for `Currency`: returns `nil` (a currency does not have an "amount"),
  - for numbers: returns a `BigDecimal` (after coercion).

Dynamic vars and rounding:

- `scale/*rounding-mode*`:
  - default rounding mode used by scaling operations when rounding is needed.
- `scale/*each*`:
  - when true, some operations (e.g. multi-arg division in money) rescale after each
    step.
- `scale/with-rounding`:
  - binds `*rounding-mode*` and sets a thread-local fast path for rounding-mode
    lookups (performance).
- `scale/with-rescaling`:
  - binds `*each*` + `*rounding-mode*` and sets a thread-local fast path for
    rounding-mode lookups (performance).

Recommendation:

- Prefer using `scale/with-rounding` / `scale/with-rescaling` (or the aliases in
  `io.randomseed.bankster.money`) instead of a raw `binding` on
  `scale/*rounding-mode*` in performance-sensitive code. Plain `binding` remains
  supported, but it does not use the fast path and may be noticeably slower in
  tight numeric loops.

### 3.3 `io.randomseed.bankster.money/Accountable`

Accountable is the "what can become Money" + "how to convert Money across
currencies/registries" layer.

Methods:

- `value` -> Money/nil:
  - builds a `Money` from numbers/strings/currency identifiers, etc.,
  - when the amount is `nil` it typically returns `nil`,
  - when currency is missing and cannot be inferred, it throws,
  - a rounding mode is required when coercion/scaling needs rounding and there is no
    explicit rounding mode and `scale/*rounding-mode*` is not set.
- `cast` -> Money:
  - changes currency (with rescaling/rounding preserved),
  - if you only need to ensure the currency comes from a given registry, prefer
    `money/of-registry`.

## 4. Public function families (practical classification)

### 4.1 Registry API (`io.randomseed.bankster.registry`)

Creation and global state:

- `registry/new`, `registry/new-registry` - create a new registry.
- `registry/R` - Atom holding the global registry.
- `registry/get` - returns a registry:
  - `(registry/get)` uses the default registry (dynamic or global),
  - `(registry/get registry)` prefers the provided registry unless it is `nil` or `false`,
  - `(registry/get true)` works as a syntactic sentinel (macro-level): use the default registry.
- `registry/state` - `@R` (global).
- `registry/set!` - sets the global registry.
- `registry/update`, `registry/update!` - functional / global update.
- `registry/with` - lexically binds the default registry (`registry/*default*`).
- `registry/hierarchies`, `registry/hierarchy` - access registry hierarchies.
- `registry/hierarchy-derive`, `registry/hierarchy-derive!` - update a selected
  hierarchy axis in a registry (pure / global mutation).

Read-only indices:

- `registry/currency-id->currency`, `registry/currency-code->currencies`,
  `registry/currency-nr->currency`, etc.
- `registry/currency-id->traits` - access per-currency traits.
- `registry/version`, `registry/ext`.

Diagnostics:

- `registry/*warn-on-inconsistency*`, `registry/*warnings-logger*`,
  `registry/inconsistency-warning`.

### 4.2 Currency API (`io.randomseed.bankster.currency`)

Construction:

- `currency/new-currency`, `currency/new`, `currency/map->new`:
  - canonicalize IDs (upper-case name; namespace preserved, except `ISO-4217` which
    is stripped),
  - may infer `:domain` as `:ISO-4217` under typical conditions,
  - `:weight` defaults to 0.

Default currency / registry:

- `currency/*default*`, `currency/set-default!`, `currency/unset-default!`.
- `currency/set-default-registry!`, `currency/config->registry`.

Resolution and coercion:

- `currency/of` (macro) - convenient currency retrieval (from registry or ad-hoc for
  maps).
- `currency/unit`, `currency/of-id` - strict; throw when currency is missing from
  the registry.
- `currency/resolve`, `currency/resolve-all` - soft; return `nil` when nothing
  matches.
- `currency/attempt`, `currency/attempt*`, `currency/with-attempt` - soft helpers.

Registry operations (mutate a registry value functionally; no side effects unless you
use `!` variants):

- `currency/register`, `currency/unregister`, `currency/update` (+ `!` variants on
  the global registry).
- `currency/add-countries`, `currency/remove-countries` (+ `!`).
- `currency/add-localized-properties`, `currency/remove-localized-properties` (+ `!`).
- `currency/add-weighted-code` (associate code with currency; conflicts resolved by
  weight).
- `currency/set-weight`, `currency/clear-weight` (+ `!`) - narrow API to mutate the
  registry weight base (`:cur-id->weight`) while keeping weighted indices and
  canonical numeric resolution synchronized.
- `currency/set-traits`, `currency/add-traits`, `currency/remove-traits` (+ `!`) -
  narrow API to mutate the registry traits base (`:cur-id->traits`).

Predicates and classification:

- `currency/currency?`, `currency/possible?`, `currency/definitive?`,
  `currency/defined?`, `currency/present?`.
- `currency/iso?`, `currency/iso-strict?`, `currency/iso-legacy?`, `currency/crypto?`,
  `currency/fiat?`, etc.
- `currency/of-domain?` - hierarchy-aware domain predicate (uses `registry/:hierarchies` when present).
- `currency/of-kind?` - hierarchy-aware kind predicate (uses `registry/:hierarchies` when present).
- `currency/has-trait?` (exact membership) and `currency/of-trait?` (hierarchy-aware) - trait predicates
  backed by `registry/:cur-id->traits` and optionally `registry/:hierarchies/:traits`.
- `currency/null?`, `currency/none?` - nil/empty/null-currency helpers.
- `currency/same-ids?` - identity comparison by ID (soft, symmetric).

Properties and localization:

- `currency/id`, `currency/code`, `currency/ns-code`, `currency/nr`, `currency/sc`,
  `currency/domain`, `currency/kind`, `currency/weight`.
- `currency/info` - full currency info map (fields + registry metadata).
- `currency/countries`, `currency/localized-properties`, `currency/localized-property`.
- `currency/symbol`, `currency/display-name` (+ `*-native`).
- `currency/formatter`, `currency/formatter-extended`.

Tagged literals:

- `#currency ...` (via `currency/code-literal` / `currency/data-literal`).

### 4.3 Money API (`io.randomseed.bankster.money`)

Creation / parsing:

- `money/value` (Accountable) - the primary constructor function.
- `money/of`, `money/of-major`, `money/of-minor` (macro) - ergonomic creation.
- `money/major-value`, `money/minor-value` - creation via major/minor parts.
- `money/parse`, `money/parse-major`, `money/parse-minor` - internal parsers (public,
  but mostly low-level).
- `money/of-registry` - forces the currency in Money to come from the given registry
  (and aligns scale).

Properties and inspection:

- `money/amount`, `money/currency`, `money/stripped-amount`, `money/unparse`.
- `money/money?`, `money/rescaled?`, `money/auto-scaled?`.

Comparisons and predicates:

- `money/compare`, `money/compare-amounts` (strict: require matching currency; `nil`
  is comparable and is the "lowest").
- `money/eq?`, `money/eq-am?` (`==`), `money/ne?`, `money/ne-am?` (`not==`).
- `money/gt?`, `money/ge?`, `money/lt?`, `money/le?`.
- `money/is-pos?`, `money/is-neg?`, `money/is-zero?`, `money/is-pos-or-zero?`,
  `money/is-neg-or-zero?` (+ aliases `pos?`, `neg?`, `zero?`).

Arithmetic (general rule: currencies must be "the same currency", but weight is
ignored):

- `money/add` (`+`), `money/sub` (`-`):
  - accept `Money` and require matching currency,
  - do not allow adding/subtracting plain numbers to/from `Money`.
- `money/mul` (`*`):
  - allows `Money` * numbers,
  - supports at most one `Money` argument in the whole expression; otherwise throws
    `ExceptionInfo`.
- `money/div` (`/`):
  - `Money / number` -> Money,
  - `Money / Money` (same currency) -> `BigDecimal`,
  - `number / Money` -> exception.
  - unary `money/div` works like `clojure.core//` for numbers, but throws for `Money`
    (because it would be equivalent to `number / Money`).
- `money/rem`:
  - analogous to `div`: for `Money/Money` the result is `BigDecimal`, for
    `Money/number` the result is `Money`.

Scaling / rounding:

- `money/scale`, `money/rescale`, `money/round`, `money/round-to`, `money/strip` (use
  consciously).
- Context macros: `money/with-rounding`, `money/with-rescaling` (aliases to
  `scale/...`).

Allocation:

- `money/allocate`:
  - splits the amount into parts according to integer-like ratios,
  - the sum of parts equals the original exactly (sum-preserving),
  - the remainder is distributed deterministically left-to-right.
- `money/distribute`:
  - `allocate` with ratios `(repeat n 1)`.

Formatting:

- `money/format`, `money/format-with` (use formatters from `currency`).

Tagged literals / readers:

- `#money ...` (via `money/code-literal` / `money/data-literal` + `*-crypto` variants).
- `money/data-readers`, `money/code-readers`.

### 4.4 Inter-op layers (operators)

- `io.randomseed.bankster.money.ops`:
  - operator aliases (`+`, `-`, `*`, `/`, `=`, etc.) that always mean Money semantics.
- `io.randomseed.bankster.money.inter-ops`:
  - if there is *no* `Money` argument, behaves 1:1 like `clojure.core`,
  - if there is any `Money` argument, it "taints" the operation and switches to
    Bankster semantics.

## 5. Recommendations and pitfalls (practical)

- Do not identify currencies by `toString`. Use `currency/id` or `currency/to-id-str`.
- Avoid `clojure.core/=` for `Money` comparisons (it uses record/map equality, compares
  `BigDecimal` values using Clojure numeric equality (scale-insensitive), and may
  reflect non-semantic differences like currency extension keys). Prefer `money/eq?`
  (or `money/=`) / `money/eq-am?` (`money/==`), or the inter-op layer
  `io.randomseed.bankster.money.inter-ops/=` for mixed numeric expressions.
- For untrusted input (e.g. from an API) prefer `to-id-str` / `to-code-str` and
  validate, instead of calling `keyword` (interning).
- Be explicit about rounding: set `scale/*rounding-mode*` via `scale/with-rounding`
  or pass rounding-mode explicitly.
- Beware Clojure floating-point literals (precision). For money, prefer:
    - BigDecimal literals with `M`, or
    - strings (parsed into BigDecimal).
- Auto-scaled currencies carry scale in the amount. This can be convenient, but
  requires care when interoperating with fixed-scale systems.

## 6. Serialization (`io.randomseed.bankster.serializers.*`)

Bankster provides per-format serialization protocols for JSON and EDN. For detailed
usage and examples see `doc/50_serialization.md`.

### 6.1 Protocols

JSON (`io.randomseed.bankster.serializers.json`):
- `JsonSerializable`: `to-json-map`, `to-json-full-map`, `to-json-string`
- `JsonDeserializable`: `from-json-map`, `from-json-string`

EDN (`io.randomseed.bankster.serializers.edn`):
- `EdnSerializable`: `to-edn-map`, `to-edn-full-map`, `to-edn-string`
- `EdnDeserializable`: `from-edn-map`, `from-edn-string`

Implementations: `Money`, `Currency`, `Class` (type token for deserialization).

### 6.2 Serialization contracts

- `to-*-map` -> map/nil:
  - returns **minimal** representation by default (currency ID + amount only),
  - with `:full? true` delegates to `to-*-full-map`,
  - returns `nil` when input is `nil`.
- `to-*-full-map` -> map/nil:
  - returns **full** representation with all fields,
  - for `Money`: currency is serialized as a nested map (not string ID),
  - for `Money`: includes extended fields (from record extension map),
  - accepts `:keys` option for filtering; supports nested opts via map elements,
  - returns `nil` when input is `nil`.
- `to-*-string` -> String/nil:
  - returns canonical string representation,
  - JSON: `"<amount> <currency-id>"` (e.g. `"12.30 PLN"`),
  - EDN: tagged literal (e.g. `#money[12.30M PLN]`, `#money/crypto[1.5M ETH]`),
  - returns `nil` when input is `nil`.

### 6.3 Deserialization contracts

- `from-*-map` -> Money/Currency:
  - strict: throws `ExceptionInfo` when:
    - input is not a map,
    - required key is missing (`:currency`, `:amount` for Money; `:id` for Currency),
    - currency cannot be resolved in registry,
    - `:rounding-mode` cannot be parsed (when provided). Accepts: `RoundingMode`,
      keywords (`:HALF_UP`), or strings (`"HALF_UP"`).
  - returns `nil` when input is `nil`.
- `from-*-string` -> Money/Currency:
  - strict: throws `ExceptionInfo` when:
    - input is not a string,
    - string cannot be parsed,
    - currency cannot be resolved in registry.
  - returns `nil` when input is `nil`.

### 6.4 Registry, rounding, and rescaling behavior

- Deserialization uses the default registry (`registry/get`) unless `:registry` is
  provided in opts.
- Amount is rescaled to currency's nominal scale during deserialization, unless
  `:rescale` option overrides it.
- When rescaling requires rounding and no `:rounding-mode` is provided (neither in
  opts nor in `scale/*rounding-mode*`), an `ArithmeticException` is thrown (wrapped
  as `ExceptionInfo`).
- `:rounding-mode` accepts: `java.math.RoundingMode` objects, keywords (`:HALF_UP`),
  or strings (`"HALF_UP"`, `"ROUND_HALF_UP"`). Parsed via `scale/post-parse-rounding`.
- Invalid `:rounding-mode` values (provided but not parseable) throw `ExceptionInfo`
  in both serialization and deserialization paths.

### 6.4.1 The `:rescale` option

**Serialization** with `:rescale`:
- Rescales the amount to the specified scale before outputting.
- Downscaling requires `:rounding-mode` or `scale/*rounding-mode*`.
- Does not modify the Money object itself, only the serialized output.

**Deserialization** with `:rescale`:
- Overrides the currency's nominal scale from the registry.
- The Currency object is cloned with the `:rescale` value as its scale.
- The resulting Money carries this modified Currency (not the registry's version).
- Use case: preserving precision when incoming data has more decimal places than
  the registry currency's scale.

Contract:
- `:rescale` must be a non-negative integer when provided.
- Invalid `:rescale` (non-integer or negative) throws `ExceptionInfo` on both
  serialization and deserialization.
- When `:rescale` is `nil`, standard behavior applies (use currency's nominal scale).
- `:rescale` applies to both JSON and EDN serializers identically.

### 6.5 Nil handling

All serialization and deserialization functions return `nil` when given `nil` input.
They do not throw on `nil`.

### 6.6 Map key acceptance (JSON)

JSON deserialization accepts both keyword and string keys in input maps:
- `:currency` or `"currency"`
- `:amount` or `"amount"`
- `:id` or `"id"`

EDN deserialization expects keyword keys only.

**JSON numeric precision caveat:** parsing JSON numbers into `double` loses precision
before Bankster sees the data. For production, prefer amount as a string, or configure
your JSON parser to emit `BigDecimal` (e.g. Cheshire via `cheshire.parse/*use-bigdecimals?*`).

### 6.7 Extended fields

- `Money` records may carry extended fields via `assoc`.
- `to-*-full-map` includes them in output.
- JSON: extended field values are stringified (BigDecimal → `.toPlainString`,
  keyword → `name`, other → `str`).
- EDN: extended field values are preserved as-is.
- Extended fields are **not** restored during deserialization (they are not part of
  the core `Money` schema).

### 6.8 Cheshire integration

- `register-cheshire-codecs!` registers a Cheshire encoder for `Money`.
- Throws `ExceptionInfo` when Cheshire is not on the classpath.
- Does not register a decoder; decoding must be done explicitly via
  `json-map->money` / `json-string->money`.
