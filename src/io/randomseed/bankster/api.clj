(ns

    ^{:doc    "Bankster library, front API."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api

  (:require [io.randomseed.bankster]
            [io.randomseed.bankster.scale            :as            scale]
            [io.randomseed.bankster.money            :as            money]
            [io.randomseed.bankster.currency         :as         currency]
            [io.randomseed.bankster.registry         :as         registry]
            [io.randomseed.bankster.serializers.json :as serializers-json])

  (:import  (io.randomseed.bankster       Currency
                                          Registry
                                          Money)
            (io.randomseed.bankster.scale Scalable)
            (java.math                    BigDecimal
                                          BigInteger
                                          MathContext
                                          RoundingMode)
            (java.text                    NumberFormat
                                          DecimalFormat
                                          DecimalFormatSymbols)
            (java.util                    Locale)))

(defn currency
  "Strict currency coercion (throwing).

  Delegates to `io.randomseed.bankster.currency/unit`.

  Arity:
  - `()` uses `currency/*default*`.
  - `([currency])` resolves the given currency representation.
  - `([currency registry])` resolves using the given registry (`nil` -> default).

  Accepts currency identifiers (keyword/symbol/string/number), `Currency` values, and
  currency maps (treated as registry lookup specs).

  When a registry is consulted and no match is found, it throws. Returns `nil` for
  `nil` input.

  When the input is already a `Currency` and `registry` is `nil`, or omitted, it is
  returned as-is.

  When the input is a `Currency` but registry is explicitly given and not `nil`, the
  given currency will be used to perform a registry lookup (and its properties will
  be used as a mask).

  Setting registry to `true` will cause default registry to be used and lookup
  enforced."
  {:tag   Currency
   :added "2.2.0"}
  (^Currency []                  (currency/unit currency/*default*))
  (^Currency [currency]          (currency/unit currency))
  (^Currency [currency registry] (if (nil? registry)
                                   (currency/unit currency)
                                   (if (true? registry)
                                     (currency/unit currency (registry/get))
                                     (currency/unit currency registry)))))

(defn currency-try
  "Non-throwing currency resolution.

  Delegates to `io.randomseed.bankster.currency/unit-try` and returns a registered
  `Currency` or `nil` when it cannot be resolved.

  Arity:
  - `()` uses `currency/*default*`.
  - `([currency])` resolves the given representation using the default registry.
  - `([currency registry])` resolves using the given registry (`nil` -> default).

  Accepts currency identifiers (keyword/symbol/string/number), `Currency` values, and
  currency maps (treated as registry lookup specs).

  When a registry is consulted and no match is found, it returns `nil`.

  When the input is already a `Currency` and `registry` is `nil` or omitted, it is
  returned as-is.

  When the input is a `Currency` but registry is explicitly given and not `nil`, the
  given currency will be used to perform a registry lookup (and its properties will
  be used as a mask).

  Setting registry to `true` will cause default registry to be used and lookup
  enforced."
  {:added "2.2.0"}
  (^Currency []                            (currency/unit-try currency/*default*))
  (^Currency [currency]                    (currency/unit-try currency))
  (^Currency [currency ^Registry registry] (if (nil? registry)
                                             (currency/unit-try currency)
                                             (if (true? registry)
                                               (currency/unit-try currency (registry/get))
                                               (currency/unit-try currency registry)))))

(defn money
  "First-class Money constructor with amount-first argument order.

  Delegates to `io.randomseed.bankster.money/value` and resolves currency via
  `io.randomseed.bankster.currency/unit` when a currency argument is provided.

  Arity:

  - `()` creates zero in the default currency.
  - `([amount])` delegates to unary `money/value`.
  - `([amount currency])` uses amount + currency.
  - `([amount currency rounding])` uses amount + currency + rounding.
  - `([amount currency registry])` uses amount + currency (resolved using registry).
  - `([amount currency rounding registry])` explicit rounding + registry for resolution.

  `currency` may be a `Currency`, an identifier (keyword/symbol/string/number), or a
  currency map (lookup spec).

  When a registry is consulted and the currency cannot be resolved, it
  throws.

  `rounding` may be a `java.math.RoundingMode` or a keyword/symbol/string like
  \"HALF_UP\" (see `scale/post-parse-rounding`). If no default currency is configured
  and the amount does not encode one, `money/value` will throw."
  {:added    "2.2.0"
   :tag      Money
   :arglists '(^Money []
               ^Money [amount]
               ^Money [amount currency]
               ^Money [amount currency rounding]
               ^Money [amount currency registry]
               ^Money [amount currency rounding registry])}
  (^Money []
   (money/value 0))
  (^Money [amount]
   (money/value amount))
  (^Money [amount c]
   (money/value amount (currency c)))
  (^Money [amount c rounding-or-registry]
   (if (instance? Registry rounding-or-registry)
     (money/value amount (currency c rounding-or-registry))
     (if rounding-or-registry
       (money/value amount (currency c) (scale/post-parse-rounding rounding-or-registry))
       (money/value amount (currency c)))))
  (^Money [amount c rounding ^Registry registry]
   (if rounding
     (money/value amount (currency c registry) (scale/post-parse-rounding rounding))
     (money/value amount (currency c registry)))))

(defn try-money
  "Non-throwing variant of `money` for currency resolution.

  Uses `currency-try` (`io.randomseed.bankster.currency/resolve`) and returns `nil`
  when the currency cannot be resolved in the given (or default) registry. Accepts
  the same argument shapes as `money`. Rounding is parsed."
  {:tag      Money
   :arglists '([]
               [amount]
               [amount currency]
               [amount currency rounding]
               [amount currency registry]
               [amount currency rounding registry])}
  ([]
   (some->> (currency-try) (money/value 0)))
  ([amount]
   (some->> (currency-try) (money/value amount)))
  ([amount c]
   (some->> c currency-try (money/value amount)))
  ([amount c rounding-or-registry]
   (if (instance? Registry rounding-or-registry)
     (when (some? c) (some->> (currency-try c rounding-or-registry) (money/value amount)))
     (if-some [rounding (scale/post-parse-rounding rounding-or-registry)]
       (when-some [c (currency-try c)] (money/value amount c rounding))
       (some->> c currency-try (money/value amount)))))
  ([amount c rounding ^Registry registry]
   (when-some [c (if registry (currency-try c registry) (currency-try c))]
     (if-some [rounding (scale/post-parse-rounding rounding)]
       (money/value amount c rounding)
       (money/value amount c)))))
