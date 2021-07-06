# History of Bankster releases

## 1.2.9 (2021-07-06)

- Fixed type hinting in fs/absoute-path?.

- Money comparators (money/compare and money/compare-amounts) now accept nil values.

- Added money/round-to for interval-based rounding.

## 1.2.8 (2021-06-28)

- Rewritten fs/absolute-path? and fs/relative-path?

## 1.2.7 (2021-06-17)

- Improved money/on-amount (now it rescales results to the scale of a money,
  not the nominal scale of a currency).

- Added alias money/apply that points to money/on-amount.

- Added money/set-amount.

## 1.2.6 (2021-06-02)

- Monetary implementation for Currency objects fixed for nil registry argument.
  Affected function: currency/unit.

- Fixed type hinting in currency/sc and currency/nr.

## 1.2.5 (2021-05-02)

- Monetary implementation for maps is more restrictive when registry is passed as an
  argument. Previously maps were causing some functions to create *ad hoc*
  currencies, now the `:id` key is used to get the existing currency from a registry.
  This is in line with the protocol's guidelines. Affected functions: currency/unit,
  currency/of-id.

- Monetary implementation for Currency objects is now fulfilling protocol
  specification, making ID-only look-ups. Affected functions: currency/unit,
  currency/of-id.

- Monetary implementation for keywords now falls back to currency code look-up even
  if the given identifier is not namespace-qualified. Affected functions:
  currency/unit, currency/present? It fixes *the chicken or the egg* problem during
  registry building.

- Functions currency/add-countries, currency/remove-localized-properties,
  currency/add-localized-properties, currency/add-weighted-code and currency/update
  are now more strict and require currency ID to be given (not currency code).

**BREAKING CHANGES**:

- Low-level function currency/add-weighted-currency renamed to
  currency/add-weighted-code.

## 1.2.4 (2021-04-18)

- Added money/data-readers and money/code-readers constants.

- Added data_readers_edn.clj (with generator in importer/readers-export) containing
  data readers for parsing EDN files.

- Fixed a bug causing registry to not be properly set in currency/with-registry and
  registry/with.

- Added fs/get-resource utility function.

**BREAKING CHANGES**:

- Data readers for tagged literals are split into code and data related handlers:

  - Functions currency/code-literal and money/code-literal are now emitting Clojure
    forms that are to be evaluated to produce Currency and Money objects.

  - Functions currency/data-literal and money/data-literal are now returning Currency
    and Money objects.

  - Tagged literal handlers (for both Clojure code and data) can now be controlled by
    the environment (e.g. dynamic variables for setting alternate registry or
    rounding mode during parsing).

  - Constant bankster/tagged-literals moved to money/tagged-literals.

## 1.2.3 (2021-04-16)

- Fixed issue with data-readers file visibility by adding a copy to resources directory.

## 1.2.2 (2021-04-16)

- Added io.randomseed.bankster/data-readers map to expose tagged literal handlers.

## 1.2.1 (2021-04-15)

- Fixed a bug causing money/of-registry to not rescale amounts.

##  1.2.0 (2021-04-13)

- Function money/div-rem renamed to money/rem and improved to behave like div in
  terms of accepted argument types, rounding and rescaling.

- Added function money/abs.

- Added aliases: money/+, money/-, money/*, money//, money/min, money/max.

- Added aliases: money/>, money/>=, money/<, money/<=, money/=, money/==.

- Added aliases: money/pos?, money/neg?, money/zero?.

- Conversion functions scale/->int, scale/->long, scale/->double, scale/->float,
  scale/to-plain-string, scale/to-clojure-string, scale/to-symbol,
  scale/to-clojure-symbol are now using scale/amount to get the actual amount of the
  given scalable instead of expecting BigDecimal values.

**BREAKING CHANGES**:

- Function money/pos now returns its argument. Use money/abs to get the absolute
  value.

## 1.1.3 (2021-04-12)

- Dependencies updated.

## 1.1.2 (2021-04-05)

- Monetary protocol methods are now more strict about the origin of currency
  if a registry is given (by-ID resolving and getting from a registry happens
  even for Currency objects).

- Unary variants of money/div and money/div-scaled are now able to take money as an
  argument (common operation when recalculating exchange rates).

- Protocol method implementations of money/value short-circuit on nil given as an
  amount.

- Added functions: money/auto-scaled?, money/of-registry and money/on-amount.

- Fixed a bug causing division and multiplication of auto-scaled currencies to
  rescale results to latest scales of the amounts instead of performing scale-free
  calculations.

- Added stripping of trailing zeros before performing scale-free divisions – causes
  non-terminal decimal expansion to end after more optimal (lesser) number of decimal
  digits.

## 1.1.1 (2021-03-26)

- Improved parsing of rounding modes.

## 1.1.0 (2021-03-25)

- Added currency/update for updating currencies with additional data preservation
  (localized properties, countries, etc.).
- Added money/cast for casting monetary amounts across currencies
  (different or slightly different, e.g. sourced in another registries).
- Added scale/to-clojure-string, scale/to-symbol, scale/to-clojure-symbol.
- Added money/->symbol, money/->clojure-symbol, money/->double, money/->float.
- Improved money/value to be more polymorphic (currency argument).
- Improved conversion functions: scale/->int, scale/->long, scale/->float, scale/->double.
- Type hinting improved in multiple functions and protocol methods.

**BREAKING CHANGES**:

- Conversion functions now have different arities:
  scale/->int, scale/->long, scale/->float, scale/->double.
- Function scale/to-plain-string is now converting decimals to plain strings
  without adding M suffix when there is too many digits. This functionality
  was moved to scale/to-clojure-string.

## 1.0.8 (2021-03-23)

- Function money/convert now accepts a price and is more polymorphic.
- Added the function currency/localized-properties.

## 1.0.7 (2021-03-19)

- Improved and unified rescaling of Money objects when their currency is auto-scaled.
- Functions money/amount, money/stripped-amount and money/currency are now polymorphic.
- Currencies database updated.

## 1.0.6 (2021-03-19)

- Better printing of monetary amounts of big precisions (adding M letter to a number).
- Better parsing of monetary amounts of big precisions (early conversion to big decimals to avoid scientific notation).

## 1.0.5 (2021-03-18)

- Fixed a bug reversing order of weighted currencies.

## 1.0.4 (2021-03-18)

- Added missing Monetary implementation functions for Money.
- Improved currency/id Monetary implementation to support currency codes.

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

