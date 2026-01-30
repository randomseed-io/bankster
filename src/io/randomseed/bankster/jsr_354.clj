(ns

    ^{:author "Paweł Wilk"
      :added  "2.0.0"
      :doc
      "JSR-354 (javax.money) naming-compatible façade for Bankster.

  This namespace is intentionally **Clojure-only**: it does not implement
  `javax.money.*` interfaces. Instead, it provides functions whose names (and arities,
  after translating receiver->first-arg) mirror the JSR-354 API so that people
  familiar with JSR-354 can quickly orient themselves in the Bankster world.

  Naming:

  - kebab-case vars are the canonical ones,
  - camelCase vars are provided as simple aliases.

  Currency resolution policy defaults to Bankster's registry semantics (weight-based
  canonical selection)."}

    io.randomseed.bankster.jsr-354

  (:refer-clojure :exclude [abs])

  (:require [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.money    :as    money]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.scale    :as    scale])

  (:import (io.randomseed.bankster Currency Money)
           (java.math BigDecimal)))

;;
;; MonetaryContext.
;;

(defrecord
    ^{:added "2.0.0"
      :doc
      "Clojure-only representation of JSR-354's MonetaryContext.

  The goal is full coverage, i.e. a stable core + an open-world attributes map:

  - core fields roughly mirror the canonical aspects exposed by `javax.money.MonetaryContext`,
  - `attrs` is an extension map for provider/implementation specific keys,
  - `bankster` is an explicitly-namespaced bucket for Bankster runtime metadata
    (e.g. the registry and rounding configuration), kept separate from the JSR-ish
    fields on purpose.

  Notes:

  - `precision` / `max-scale` may be nil when unspecified.
  - `fixed-scale?` may be nil when unknown/unspecified.
  - `attrs` is intended to use String keys to avoid keyword interning on untrusted inputs."}
    MonetaryContext
    [^Class amount-type
     ^Class number-type
     ^Long  precision
     ^Long  max-scale
     ^Boolean fixed-scale?
     attrs
     bankster])

(def ^{:const true :tag String :added "2.0.0"}
  attr-amount-type
  "Standard MonetaryContext attribute key for amount type."
  "amountType")

(def ^{:const true :tag String :added "2.0.0"}
  attr-number-type
  "Standard MonetaryContext attribute key for number type."
  "numberType")

(def ^{:const true :tag String :added "2.0.0"}
  attr-precision
  "Standard MonetaryContext attribute key for precision."
  "precision")

(def ^{:const true :tag String :added "2.0.0"}
  attr-max-scale
  "Standard MonetaryContext attribute key for max scale."
  "maxScale")

(def ^{:const true :tag String :added "2.0.0"}
  attr-fixed-scale
  "Standard MonetaryContext attribute key for fixed-scale flag."
  "fixedScale")

(defn- attr-key
  "Normalizes MonetaryContext attribute keys to a string.

  Uses `ns/name` for keywords/symbols (without the leading colon)."
  {:tag String :added "2.0.0" :private true}
  [k]
  (cond
    (string? k)
    k

    (keyword? k)
    (let [ns (namespace k)]
      (if (some? ns)
        (str ns "/" (name k))
        (name k)))

    (symbol? k)
    (let [ns (namespace k)]
      (if (some? ns)
        (str ns "/" (name k))
        (name k)))

    :else
    (str k)))

(defn- normalize-attrs
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [m]
  (cond
    (nil? m) {}
    (map? m) (reduce-kv (fn [acc k v] (assoc acc (attr-key k) v)) {} m)
    :else    (throw (ex-info "MonetaryContext attrs must be a map or nil."
                             {:value m
                              :hint  "Pass {:attrs {\"key\" value}} or omit :attrs."}))))

(defn context
  "Builds a `MonetaryContext`.

  Options (all optional):

  - `:amount-type`       -> Class (default: `io.randomseed.bankster.Money`)
  - `:number-type`       -> Class (default: `java.math.BigDecimal`)
  - `:precision`         -> integer/long or nil
  - `:max-scale`         -> integer/long or nil
  - `:fixed-scale?`      -> boolean or nil
  - `:attrs`             -> map (keys normalized to strings; see `attr-*` vars)

  Bankster runtime extras (stored under `:bankster` in the resulting record):

  - `:registry`          -> `io.randomseed.bankster.Registry` (or nil)
  - `:rounding-mode`     -> rounding spec accepted by `scale/post-parse-rounding`
  - `:rescale-each?`     -> boolean
  - `:currency-resolution` -> keyword (default: `:weight`)

  The builder also standardizes a small set of string keys in `attrs`:
  `attr-amount-type`, `attr-number-type`, and (when present) `attr-precision`,
  `attr-max-scale`, `attr-fixed-scale`."
  {:tag MonetaryContext :added "2.0.0"}
  (^MonetaryContext []
   (context nil))
  (^MonetaryContext [opts]
   (let [opts                (or opts {})
         amount-type         (or (:amount-type opts) Money)
         number-type         (or (:number-type opts) BigDecimal)
         precision           (:precision opts)
         max-scale           (:max-scale opts)
         fixed-scale?        (:fixed-scale? opts)
         registry-opt        (when (contains? opts :registry) (:registry opts))
         rounding-mode-opt   (when (contains? opts :rounding-mode) (:rounding-mode opts))
         rescale-each-opt    (when (contains? opts :rescale-each?) (:rescale-each? opts))
         currency-resolution (or (:currency-resolution opts) :weight)
         attrs0              (normalize-attrs (:attrs opts))
         attrs1              (cond-> attrs0
                               true                 (assoc attr-amount-type amount-type
                                           attr-number-type number-type)
                               (some? precision)    (assoc attr-precision    (long precision))
                               (some? max-scale)    (assoc attr-max-scale    (long max-scale))
                               (some? fixed-scale?) (assoc attr-fixed-scale  (boolean fixed-scale?)))
         bankster0           (merge {:currency-resolution currency-resolution}
                                    (let [b (:bankster opts)] (if (map? b) b {})))
         bankster1           (cond-> bankster0
                               (contains? opts :registry)
                               (assoc :registry registry-opt)

                               (contains? opts :rounding-mode)
                               (assoc :rounding-mode (scale/post-parse-rounding rounding-mode-opt))

                               (contains? opts :rescale-each?)
                               (assoc :rescale-each? (boolean rescale-each-opt)))
         registry-val (:registry bankster1)]
     (when (and (some? registry-val)
                (not (instance? io.randomseed.bankster.Registry registry-val)))
       (throw
        (ex-info
         "Bankster registry must be an instance of io.randomseed.bankster.Registry."
         {:value registry-val
          :hint  "Use (registry/get) or (registry/state) to obtain a Registry instance."})))
     (let [bankster2 (cond
                       (and (contains? opts :registry) (some? registry-val))
                       (assoc bankster1 :registry-version (registry/version registry-val))

                       (and (some? registry-val) (not (contains? bankster1 :registry-version)))
                       (assoc bankster1 :registry-version (registry/version registry-val))

                       :else
                       bankster1)]
       (->MonetaryContext amount-type number-type
                          (when (some? precision) (long precision))
                          (when (some? max-scale) (long max-scale))
                          (when (some? fixed-scale?) (boolean fixed-scale?))
                          attrs1
                          bankster2)))))

(defmacro with-context
  "Applies a JSR-354-style context in a lexical scope.

  In this Clojure-only compat layer, we interpret the context as *inputs to dynamic
  Bankster runtime settings* (registry and scaling/rounding), not as mutable state.

  Uses `(:bankster ctx)` as an optional map of settings.
  Supported keys:

  - `:registry`       -> `io.randomseed.bankster.Registry` (binds `registry/*default*`)
  - `:rounding-mode`  -> `java.math.RoundingMode` or a value accepted by
                        `io.randomseed.bankster.scale/post-parse-rounding`
                        (binds `scale/*rounding-mode*`)
  - `:rescale-each?`  -> boolean (binds `scale/*each*`)

  Unspecified keys do not override current bindings. Keys may be present with `nil`
  values to explicitly clear a binding (e.g. set rounding-mode to nil)."
  [ctx & body]
  {:added "2.0.0"}
    `(let [ctx# ~ctx
           b#   (:bankster ctx#)
           m?#  (map? b#)
           r?#  (and m?# (contains? b# :registry))
           rm?# (and m?# (contains? b# :rounding-mode))
           e?#  (and m?# (contains? b# :rescale-each?))
           rm#  (if rm?# (scale/post-parse-rounding (:rounding-mode b#))
                    (scale/rounding-mode))]
       (binding [registry/*default*    (if r?#  (:registry      b#) registry/*default*)
                 scale/*rounding-mode* rm#
                 scale/*each*          (if e?#  (boolean (:rescale-each? b#)) scale/*each*)]
         (let [prev# (.get ^ThreadLocal scale/thread-rounding-mode)]
           (.set ^ThreadLocal scale/thread-rounding-mode rm#)
           (try
             ~@body
             (finally
               (if (nil? prev#)
                 (.remove ^ThreadLocal scale/thread-rounding-mode)
                 (.set ^ThreadLocal scale/thread-rounding-mode prev#))))))))

(defmacro withContext
  "Alias for `with-context`."
  {:added "2.0.0"}
  [ctx & body]
  `(with-context ~ctx ~@body))

;;
;; Monetary (static-style helpers).
;;

(defn get-currency
  "JSR-354-ish: `Monetary.getCurrency(String)` and `MonetaryAmount.getCurrency()`.

  Returns a Bankster `Currency` resolved in the default registry (weight-based)."
  {:added "2.0.0" :tag Currency}
  ^Currency [currency-or-money]
  (currency/unit currency-or-money))

(def getCurrency get-currency)

(defn get-currencies
  "JSR-354-ish: `Monetary.getCurrencies()`.

  Returns a set of currencies known to the default registry."
  {:added "2.0.0" :tag clojure.lang.IPersistentSet}
  ^clojure.lang.IPersistentSet
  []
  (into #{} (vals (registry/currency-id->currency))))

(def getCurrencies get-currencies)

(defn get-default-rounding
  "JSR-354-ish: `Monetary.getDefaultRounding()`.

  Returns a MonetaryOperator-like function `(Money -> Money)` that rescales a money
  amount to its currency's nominal scale.

  Uses `io.randomseed.bankster.scale/*rounding-mode*` when bound, otherwise defaults
  to `io.randomseed.bankster.scale/ROUND_HALF_EVEN`."
  {:added "2.0.0"}
  ^clojure.lang.IFn
  []
    (fn default-rounding
      ^Money [^Money amount]
      (scale/with-rounding (or (scale/rounding-mode) scale/ROUND_HALF_EVEN)
        (money/rescale amount))))

(def getDefaultRounding get-default-rounding)

;;
;; CurrencyUnit.
;;

(defn get-currency-code
  "JSR-354-ish: `CurrencyUnit.getCurrencyCode()`."
  {:added "2.0.0"}
  ^String [currency-or-money]
  (currency/to-code-str currency-or-money))

(def getCurrencyCode get-currency-code)

(defn get-numeric-code
  "JSR-354-ish: `CurrencyUnit.getNumericCode()`.

  Returns Bankster's numeric-id (ISO 4217) or `-1` when missing."
  {:added "2.0.0"}
  ^long [currency-or-money]
  (long (.numeric ^Currency (currency/unit currency-or-money))))

(def getNumericCode get-numeric-code)

(defn get-default-fraction-digits
  "JSR-354-ish: `CurrencyUnit.getDefaultFractionDigits()`.

  Returns Bankster's currency scale (decimal places) or `-1` for auto-scaled."
  {:added "2.0.0"}
  ^long [currency-or-money]
  (long (.scale ^Currency (currency/unit currency-or-money))))

(def getDefaultFractionDigits get-default-fraction-digits)

;;
;; MonetaryAmount.
;;

(def create
  "JSR-354-ish: MonetaryAmountFactory-like constructor.

  Delegates to `io.randomseed.bankster.money/value` (Accountable)."
  money/value)

(defn get-number
  "JSR-354-ish: `MonetaryAmount.getNumber()`.

  Returns the money amount as `java.math.BigDecimal`."
  {:added "2.0.0"}
  ^BigDecimal [money]
  (money/amount money))

(def getNumber get-number)

(defn get-number-stripped
  "JSR-354-ish: `MonetaryAmount.getNumberStripped()`.

  Returns the money amount with trailing zeros stripped."
    {:added "2.0.0"}
  ^BigDecimal [money]
  (money/stripped-amount money))

(def getNumberStripped get-number-stripped)

(defn add
  "JSR-354-ish: `MonetaryAmount.add(MonetaryAmount)`."
  {:added "2.0.0"}
  ^Money [^Money a ^Money b]
  (money/add a b))

(defn subtract
  "JSR-354-ish: `MonetaryAmount.subtract(MonetaryAmount)`."
  {:added "2.0.0"}
  ^Money [^Money a ^Money b]
  (money/sub a b))

(defn- ensure-not-money!
  {:added "2.0.0"}
  [op x]
  (when (money/money? x)
    (throw
     (ex-info
      "JSR-354 compat method expects a number-like value, not a Money."
      {:op op :value x})))
  x)

(defn multiply
  "JSR-354-ish: `MonetaryAmount.multiply(Number)`."
  {:added "2.0.0"}
  ^Money [^Money a multiplicand]
  (money/mul a (ensure-not-money! :multiply multiplicand)))

(defn divide
  "JSR-354-ish: `MonetaryAmount.divide(Number)` and `MonetaryAmount.divide(Number, RoundingMode)`."
  {:added "2.0.0"}
  (^Money [^Money a divisor]
   (money/div a (ensure-not-money! :divide divisor)))
  (^Money [^Money a divisor rounding-mode]
   (scale/with-rounding rounding-mode
     (money/div a (ensure-not-money! :divide divisor)))))

(defn remainder
  "JSR-354-ish: `MonetaryAmount.remainder(Number)` and `MonetaryAmount.remainder(Number, RoundingMode)`."
  {:added "2.0.0"}
  (^Money [^Money a divisor]
   (money/rem a (ensure-not-money! :remainder divisor)))
  (^Money [^Money a divisor rounding-mode]
   (money/rem a (ensure-not-money! :remainder divisor) rounding-mode)))

(defn abs
  "JSR-354-ish: `MonetaryAmount.abs()`."
  {:added "2.0.0"}
  ^Money [^Money a]
  (money/abs a))

(defn negate
  "JSR-354-ish: `MonetaryAmount.negate()`."
  {:added "2.0.0"}
  ^Money [^Money a]
  (money/neg a))

(defn plus
  "JSR-354-ish: `MonetaryAmount.plus()`."
  {:added "2.0.0"}
  ^Money [^Money a]
  (money/pos a))

(defn strip-trailing-zeros
  "JSR-354-ish: `MonetaryAmount.stripTrailingZeros()`."
  {:added "2.0.0"}
  ^Money [^Money a]
  (money/strip a))

(def stripTrailingZeros strip-trailing-zeros)

(defn signum
  "JSR-354-ish: `MonetaryAmount.signum()`."
  {:added "2.0.0"}
  ^long [^Money a]
  (long (.signum ^BigDecimal (money/amount a))))

(defn is-zero
  "JSR-354-ish: `MonetaryAmount.isZero()`."
  {:added "2.0.0"}
  ^Boolean [^Money a]
  (money/is-zero? a))

(def isZero is-zero)

(defn is-positive
  "JSR-354-ish: `MonetaryAmount.isPositive()`."
  {:added "2.0.0"}
  ^Boolean [^Money a]
  (money/is-pos? a))

(def isPositive is-positive)

(defn is-negative
  "JSR-354-ish: `MonetaryAmount.isNegative()`."
  {:added "2.0.0"}
  ^Boolean [^Money a]
  (money/is-neg? a))

(def isNegative is-negative)

(defn is-positive-or-zero
  "JSR-354-ish: `MonetaryAmount.isPositiveOrZero()`."
  {:added "2.0.0"}
  ^Boolean [^Money a]
  (money/is-pos-or-zero? a))

(def isPositiveOrZero is-positive-or-zero)

(defn is-negative-or-zero
  "JSR-354-ish: `MonetaryAmount.isNegativeOrZero()`."
  {:added "2.0.0"}
  ^Boolean [^Money a]
  (money/is-neg-or-zero? a))

(def isNegativeOrZero is-negative-or-zero)

(defn compare-to
  "JSR-354-ish: `MonetaryAmount.compareTo(MonetaryAmount)`."
  {:added "2.0.0"}
  ^long [^Money a ^Money b]
  (money/compare-amounts a b))

(def compareTo compare-to)

(defn is-equal-to
  "JSR-354-ish: `MonetaryAmount.isEqualTo(MonetaryAmount)`.

  Scale-insensitive comparison of amounts (currency must match)."
  {:added "2.0.0"}
  ^Boolean [^Money a ^Money b]
  (money/eq-am? a b))

(def isEqualTo is-equal-to)

(defn is-greater-than
  "JSR-354-ish: `MonetaryAmount.isGreaterThan(MonetaryAmount)`."
  {:added "2.0.0"}
  ^Boolean [^Money a ^Money b]
  (money/gt? a b))

(def isGreaterThan is-greater-than)

(defn is-greater-than-or-equal-to
  "JSR-354-ish: `MonetaryAmount.isGreaterThanOrEqualTo(MonetaryAmount)`."
  {:added "2.0.0"}
  ^Boolean [^Money a ^Money b]
  (money/ge? a b))

(def isGreaterThanOrEqualTo is-greater-than-or-equal-to)

(defn is-less-than
  "JSR-354-ish: `MonetaryAmount.isLessThan(MonetaryAmount)`."
  {:added "2.0.0"}
  ^Boolean [^Money a ^Money b]
  (money/lt? a b))

(def isLessThan is-less-than)

(defn is-less-than-or-equal-to
  "JSR-354-ish: `MonetaryAmount.isLessThanOrEqualTo(MonetaryAmount)`."
  {:added "2.0.0"}
  ^Boolean [^Money a ^Money b]
  (money/le? a b))

(def isLessThanOrEqualTo is-less-than-or-equal-to)

(defn with
  "JSR-354-ish: `MonetaryAmount.with(MonetaryOperator)`.

  In Clojure, a MonetaryOperator is just a function `(Money -> Money)`."
  {:added "2.0.0"}
  [^Money amount operator]
  (operator amount))

(defn query
  "JSR-354-ish: `MonetaryAmount.query(MonetaryQuery)`.

  In Clojure, a MonetaryQuery is just a function `(Money -> any)`."
  {:added "2.0.0"}
  [^Money amount monetary-query]
  (monetary-query amount))
