(ns

    ^{:doc    "Bankster library, front API money helpers."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.money

  (:refer-clojure :exclude [* + - / < <= = == > >= abs apply max min not= rem
                            resolve cast format compare pos? neg? zero?])

  (:require [io.randomseed.bankster.api.registry     :as     api-registry]
            [io.randomseed.bankster.api.currency     :as     api-currency]
            [io.randomseed.bankster.registry         :as         registry]
            [io.randomseed.bankster.currency         :as         currency]
            [io.randomseed.bankster.money            :as            money]
            [io.randomseed.bankster.scale            :as            scale]
            [io.randomseed.bankster.util             :as               bu]
            [io.randomseed.bankster.serializers.edn  :as  serializers-edn]
            [io.randomseed.bankster.serializers.json :as serializers-json])

  (:import  (io.randomseed.bankster Currency
                                    Registry
                                    Money)
            (java.math              BigDecimal)))

;;
;; Contextual helpers
;;

(bu/defalias with-currency  io.randomseed.bankster.currency/with)
(bu/defalias with-registry  api-registry/with)
(bu/defalias with-rounding  io.randomseed.bankster.scale/with-rounding)
(bu/defalias with-rescaling io.randomseed.bankster.scale/with-rescaling)

(defn rounding-mode
  "Returns the current rounding mode.

  Delegates to `io.randomseed.bankster.scale/rounding-mode`."
  {:added "2.2.0"}
  (^java.math.RoundingMode []
   (scale/rounding-mode))
  (^java.math.RoundingMode [default]
   (scale/rounding-mode default)))

(defn scale-apply
  "Applies scaling to a value.

  Delegates to `io.randomseed.bankster.scale/apply`."
  {:added "2.2.0"}
  ([x]
   (scale/apply x))
  ([x sc]
   (scale/apply x sc))
  ([x sc rounding]
   (scale/apply x sc rounding)))

(defn default-registry
  "Returns the default registry (honors `io.randomseed.bankster.registry/*default*`)."
  {:tag Registry :added "2.2.0"}
  []
  (registry/get))

(defn registry-or-default
  "Resolves `true` or `nil` into the current default registry, otherwise returns the
  given value."
  {:tag Registry :added "2.2.0"}
  [registry]
  (if (or (nil? registry) (true? registry)) (registry/get) registry))

(defn resolve
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
  {:added     "2.2.0"
   :tag       Money
   :arglists  '(^Money []
                ^Money [amount]
                ^Money [amount currency]
                ^Money [amount currency rounding]
                ^Money [amount currency registry]
                ^Money [amount currency rounding registry])
   :ex/strict true}
  (^Money []
   (money/value 0))
  (^Money [amount]
   (money/value amount))
  (^Money [amount c]
   (money/value amount (api-currency/resolve c)))
  (^Money [amount c rounding-or-registry]
   (if (or (instance? Registry rounding-or-registry) (true? rounding-or-registry))
     (money/value amount (api-currency/resolve c rounding-or-registry))
     (if rounding-or-registry
       (money/value amount (api-currency/resolve c) (scale/post-parse-rounding rounding-or-registry))
       (money/value amount (api-currency/resolve c)))))
  (^Money [amount c rounding ^Registry registry]
   (if rounding
     (money/value amount (api-currency/resolve c registry) (scale/post-parse-rounding rounding))
     (money/value amount (api-currency/resolve c registry)))))

(defn resolve-try
  "Non-throwing variant of `resolve` for currency resolution.

  Uses `io.randomseed.bankster.currency/unit-try` and returns `nil` when the
  currency cannot be resolved in the given (or default) registry. Accepts the same
  argument shapes as `resolve`. Rounding is parsed."
  {:tag      Money
   :arglists '([]
               [amount]
               [amount currency]
               [amount currency rounding]
               [amount currency registry]
               [amount currency rounding registry])
   :ex/soft  true}
  ([]
   (some->> (api-currency/resolve-try) (money/value 0)))
  ([amount]
   (some->> (api-currency/resolve-try) (money/value amount)))
  ([amount c]
   (some->> c api-currency/resolve-try (money/value amount)))
  ([amount c rounding-or-registry]
   (if (or (instance? Registry rounding-or-registry) (true? rounding-or-registry))
     (when (some? c) (some->> (api-currency/resolve-try c rounding-or-registry) (money/value amount)))
     (if-some [rounding (scale/post-parse-rounding rounding-or-registry)]
       (when-some [c (api-currency/resolve-try c)] (money/value amount c rounding))
       (some->> c api-currency/resolve-try (money/value amount)))))
  ([amount c rounding ^Registry registry]
   (when-some [c (if registry (api-currency/resolve-try c registry) (api-currency/resolve-try c))]
     (if-some [rounding (scale/post-parse-rounding rounding)]
       (money/value amount c rounding)
       (money/value amount c)))))

(defn of-registry
  "Ensures that a currency of the given money originates from the given registry. If
  the registry is not given and a dynamic registry is not set, the default one is
  used. Rescales the amount if needed to match the nominal scale. Optional
  rounding-mode can be supplied to be used when downscaling is needed (nominal
  currency from a registry has lower number of decimal places than the amount of
  money).

  Money can be expressed as a `Money` object or any other object that will create
  `Money` when passed to the `value` function. Returns money."
  {:tag Money :added "2.2.0" :ex/strict true}
  (^Money [money]
   (money/of-registry money))
  (^Money [registry money]
   (money/of-registry (registry-or-default registry) money))
  (^Money [registry money rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/of-registry (registry-or-default registry) money rounding)
     (money/of-registry (registry-or-default registry) money))))

(defn cast
  "Casts an existing Money object to another having a different currency, rescaling
  the amount and optionally rounding it. This is useful when operating on multiple
  currency registries compatible with different data sources or processing engines.
  For simply ensuring that a currency is sourced in the right registry, use
  `of-registry`.

  Delegates to `io.randomseed.bankster.money/cast`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [money]
               ^Money [money currency]
               ^Money [money currency rounding])
   :ex/strict true}
  (^Money [money]
   (money/cast money))
  (^Money [money currency]
   (money/cast money currency))
  (^Money [money currency rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/cast money currency rounding)
     (money/cast money currency))))

(defn cast-try
  "Soft variant of `cast`.

  Returns `nil` when casting fails (e.g. missing currency or required rounding)."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [money]
               ^Money [money currency]
               ^Money [money currency rounding])
   :ex/soft true}
  (^Money [money]
   (money/cast money))
  (^Money [money currency]
   (try
     (money/cast money currency)
     (catch clojure.lang.ExceptionInfo _e nil)))
  (^Money [money currency rounding]
   (try
     (if-some [rounding (scale/post-parse-rounding rounding)]
       (money/cast money currency rounding)
       (money/cast money currency))
     (catch clojure.lang.ExceptionInfo _e nil))))

(defn major
  "First-class constructor for major-part amounts (amount-first).

  Delegates to `io.randomseed.bankster.money/major-value`."
  {:added     "2.2.0"
   :tag       Money
   :arglists  '(^Money [amount]
                ^Money [amount currency]
                ^Money [amount currency rounding]
                ^Money [amount currency registry]
                ^Money [amount currency rounding registry])
   :ex/strict true}
  (^Money [amount]
   (money/major-value amount))
  (^Money [amount c]
   (money/major-value (api-currency/resolve c) amount))
  (^Money [amount c rounding-or-registry]
   (if (or (instance? Registry rounding-or-registry) (true? rounding-or-registry))
     (money/major-value (api-currency/resolve c rounding-or-registry) amount)
     (if rounding-or-registry
       (money/major-value (api-currency/resolve c) amount (scale/post-parse-rounding rounding-or-registry))
       (money/major-value (api-currency/resolve c) amount))))
  (^Money [amount c rounding ^Registry registry]
   (if rounding
     (money/major-value (api-currency/resolve c registry) amount (scale/post-parse-rounding rounding))
     (money/major-value (api-currency/resolve c registry) amount))))

(defn minor
  "First-class constructor for minor-part amounts (amount-first).

  Delegates to `io.randomseed.bankster.money/minor-value`."
  {:added     "2.2.0"
   :tag       Money
   :arglists  '(^Money [amount]
                ^Money [amount currency]
                ^Money [amount currency rounding]
                ^Money [amount currency registry]
                ^Money [amount currency rounding registry])
   :ex/strict true}
  (^Money [amount]
   (money/minor-value amount))
  (^Money [amount c]
   (money/minor-value (api-currency/resolve c) amount))
  (^Money [amount c rounding-or-registry]
   (if (or (instance? Registry rounding-or-registry) (true? rounding-or-registry))
     (money/minor-value (api-currency/resolve c rounding-or-registry) amount)
     (if rounding-or-registry
       (money/minor-value (api-currency/resolve c) amount (scale/post-parse-rounding rounding-or-registry))
       (money/minor-value (api-currency/resolve c) amount))))
  (^Money [amount c rounding ^Registry registry]
   (if rounding
     (money/minor-value (api-currency/resolve c registry) amount (scale/post-parse-rounding rounding))
     (money/minor-value (api-currency/resolve c registry) amount))))

(defn amount
  "Returns Money amount as `BigDecimal`."
  {:tag BigDecimal :added "2.2.0" :ex/strict true}
  (^BigDecimal [money]
   (money/amount money))
  (^BigDecimal [a b]
   (money/amount a b))
  (^BigDecimal [a b rounding]
   (money/amount a b rounding)))

(defn currency
  "Returns Money currency."
  {:tag Currency :added "2.2.0" :ex/strict true}
  (^Currency [money]
   (money/currency money))
  (^Currency [a b]
   (money/currency a b))
  (^Currency [a b rounding]
   (money/currency a b rounding)))

(defn info
  "Returns a map with `:currency` and `:amount` for the given money-like input.

  Delegates to `io.randomseed.bankster.money/info`."
  {:tag clojure.lang.IPersistentMap :added "2.2.0" :ex/strict true}
  [money]
  (money/info money))


(defn auto-scaled?
  "Returns `true` if the scale returned by `scale` is auto-scaled.

  Delegates to `io.randomseed.bankster.scale/auto?`."
  {:tag Boolean :added "2.2.0"}
  [money]
  (scale/auto? (money/scale money)))

(defn strip
  "Strips trailing zeros from a `Money` amount."
  {:tag Money :added "2.2.0" :ex/strict true}
  [money]
  (when-some [^Money m (money/value money)]
    (money/strip m)))

(defn round-to
  "Rounds a Money to a given interval.

  Delegates to `io.randomseed.bankster.money/round-to`."
  {:tag Money :added "2.2.0"}
  (^Money [^Money money]
   (money/round-to money))
  (^Money [^Money money interval]
   (money/round-to money interval))
  (^Money [^Money money interval rounding]
   (money/round-to money interval (scale/post-parse-rounding rounding))))

(defn allocate
  "Allocates a monetary amount into parts according to integer ratios.

  Delegates to `io.randomseed.bankster.money/allocate`."
  {:tag clojure.lang.IPersistentVector :added "2.2.0"}
  [^Money money ratios]
  (money/allocate money ratios))

(defn distribute
  "Distributes a monetary amount into `n` as-even-as-possible parts.

  Delegates to `io.randomseed.bankster.money/distribute`."
  {:tag clojure.lang.IPersistentVector :added "2.2.0"}
  [^Money money n]
  (money/distribute money n))

(defn format
  "Formats the given amount of money as a string according to localization rules.

  Delegates to `io.randomseed.bankster.money/format`."
  {:tag      String
   :added    "2.2.0"
   :arglists '(^String [^Money money]
               ^String [^Money money locale]
               ^String [^Money money locale opts])}
  (^String [^Money money]
   (money/format money))
  (^String [^Money money locale]
   (money/format money locale))
  (^String [^Money money locale opts]
   (money/format money locale opts)))

(defn parse
  "Parses money-like input into a `Money` value.

  Delegates to `io.randomseed.bankster.money/parse`. Rounding is parsed via
  `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [amount]
               ^Money [currency amount]
               ^Money [currency amount rounding])
   :ex/strict true}
  (^Money [amount]
   (money/parse amount))
  (^Money [currency amount]
   (money/parse currency amount))
  (^Money [currency amount rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/parse currency amount rounding)
     (money/parse currency amount))))

(defn parse-major
  "Parses money-like input and treats the amount as major part.

  Delegates to `io.randomseed.bankster.money/parse-major`. Rounding is parsed via
  `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [amount]
               ^Money [currency amount]
               ^Money [currency amount rounding])
   :ex/strict true}
  (^Money [amount]
   (money/parse-major amount))
  (^Money [currency amount]
   (money/parse-major currency amount))
  (^Money [currency amount rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/parse-major currency amount rounding)
     (money/parse-major currency amount))))

(defn parse-minor
  "Parses money-like input and treats the amount as minor part.

  Delegates to `io.randomseed.bankster.money/parse-minor`. Rounding is parsed via
  `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [amount]
               ^Money [currency amount]
               ^Money [currency amount rounding])
   :ex/strict true}
  (^Money [amount]
   (money/parse-minor amount))
  (^Money [currency amount]
   (money/parse-minor currency amount))
  (^Money [currency amount rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/parse-minor currency amount rounding)
     (money/parse-minor currency amount))))

(defn normalize
  "Normalizes money-like input into a `Money` value.

  - `([amount])` accepts `Money`, EDN-like map (via `money/of-map`), or a literal
    amount (delegates to `money/parse`).
  - `([amount currency])` and `([amount currency rounding])` use amount-first order
    and delegate to `money/parse`. Rounding is parsed via
    `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '([amount] [amount currency] [amount currency rounding])
   :ex/strict true}
  [amount & args]
  (let [argc (count args)]
    (cond
      (clojure.core/zero? argc) (cond
                                (nil? amount) nil
                                (map? amount) (money/of-map amount)
                                :else         (money/parse amount))
      (clojure.core/> argc 2) (throw (clojure.lang.ArityException. (inc argc) "normalize"))
      :else                   (let [currency (nth args 0)]
                                (if (clojure.core/== 2 argc)
                                  (money/parse currency amount (scale/post-parse-rounding (nth args 1)))
                                  (money/parse currency amount))))))

(def ^{:tag      Money
       :arglists (:arglists (meta #'money/add))
       :added    "2.2.0"}
  add
  "Adds two or more monetary amounts of the same currency.

  Delegates to `io.randomseed.bankster.money/add`."
  money/add)

(def ^{:tag      Money
       :arglists (:arglists (meta #'money/sub))
       :added    "2.2.0"}
  sub
  "Subtracts monetary amounts of the same currency.

  Delegates to `io.randomseed.bankster.money/sub`."
  money/sub)

(def ^{:tag      Money
       :arglists (:arglists (meta #'money/mul))
       :added    "2.2.0"}
  mul
  "Multiplies monetary amount by number or other values as allowed.

  Delegates to `io.randomseed.bankster.money/mul`."
  money/mul)

(def ^{:arglists (:arglists (meta #'money/div))
       :added    "2.2.0"}
  div
  "Divides monetary amounts or numbers according to Money rules.

  Delegates to `io.randomseed.bankster.money/div`."
  money/div)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/eq?))
       :added    "2.2.0"}
  eq?
  "Returns true if monetary amounts and currencies are equal.

  Delegates to `io.randomseed.bankster.money/eq?`."
  money/eq?)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/ne?))
       :added    "2.2.0"}
  ne?
  "Returns true if monetary amounts or currencies are not equal.

  Delegates to `io.randomseed.bankster.money/ne?`."
  money/ne?)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/gt?))
       :added    "2.2.0"}
  gt?
  "Returns true if the first monetary amount is greater than the second.

  Delegates to `io.randomseed.bankster.money/gt?`."
  money/gt?)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/ge?))
       :added    "2.2.0"}
  ge?
  "Returns true if the first monetary amount is greater than or equal to the second.

  Delegates to `io.randomseed.bankster.money/ge?`."
  money/ge?)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/lt?))
       :added    "2.2.0"}
  lt?
  "Returns true if the first monetary amount is less than the second.

  Delegates to `io.randomseed.bankster.money/lt?`."
  money/lt?)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/le?))
       :added    "2.2.0"}
  le?
  "Returns true if the first monetary amount is less than or equal to the second.

  Delegates to `io.randomseed.bankster.money/le?`."
  money/le?)

(def ^{:arglists (:arglists (meta #'money/compare))
       :added    "2.2.0"}
  compare
  "Compares monetary amounts of the same currency and scale.

  Delegates to `io.randomseed.bankster.money/compare`."
  money/compare)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/is-pos?))
       :added    "2.2.0"}
  pos?
  "Returns true if the monetary amount is positive.

  Delegates to `io.randomseed.bankster.money/is-pos?`."
  money/is-pos?)

(def ^{:tag      Boolean
       :arglists (:arglists (meta #'money/is-neg?))
       :added    "2.2.0"}
  neg?
  "Returns true if the monetary amount is negative.

  Delegates to `io.randomseed.bankster.money/is-neg?`."
  money/is-neg?)

(defn zero?
  "Returns `true` if the given monetary amount is zero.

  Delegates to `io.randomseed.bankster.money/is-zero?`."
  {:tag Boolean :added "2.2.0"}
  [^Money money]
  (money/is-zero? money))

(defn same-currencies?
  "Returns `true` if both currencies are the same for the given money objects.

  Delegates to `io.randomseed.bankster.money/same-currencies?`."
  {:tag Boolean :added "2.2.0"}
  [^Money a ^Money b]
  (money/same-currencies? a b))

(defn money?
  "Returns `true` when `x` is an instance of `io.randomseed.bankster.Money`."
  {:tag Boolean :added "2.2.0"}
  [x]
  (money/money? x))

;;
;; Serialization
(defn ->map
  "Coerces a money-like value into an EDN-friendly map."
  {:tag clojure.lang.IPersistentMap :added "2.2.0" :ex/strict true}
  [money]
  (money/to-map money))

(defn ->edn
  "Serializes money-like value to an EDN tagged literal string."
  {:tag String :added "2.2.0" :ex/strict true}
  (^String [money]
   (when-some [^Money m (money/value money)]
     (serializers-edn/money->edn-string m)))
  (^String [money opts]
   (when-some [^Money m (money/value money)]
     (serializers-edn/money->edn-string m opts))))

(defn ->json
  "Serializes money-like value to a JSON string."
  {:tag String :added "2.2.0" :ex/strict true}
  (^String [money]
   (money/to-json-string money))
  (^String [money opts]
   (money/to-json-string money opts)))

(defn from-edn
  "Deserializes Money from EDN string or map."
  {:tag Money :added "2.2.0" :ex/strict true}
  (^Money [x]
   (from-edn x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (map? x)    (serializers-edn/edn-map->money x opts)
     (string? x) (serializers-edn/edn-string->money x opts)
     :else
     (throw
      (ex-info
       "Unsupported EDN money representation."
       {:op    :bankster.api.money/from-edn
        :value x
        :class (class x)})))))

(defn from-edn-text
  "Deserializes Money from raw EDN text."
  {:tag Money :added "2.2.0" :ex/strict true}
  (^Money [x]
   (from-edn-text x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-edn/edn-string->money x opts)
     :else
     (throw
      (ex-info
       "Money EDN text must be a string."
       {:op    :bankster.api.money/from-edn-text
        :value x
        :class (class x)})))))

(defn from-json
  "Deserializes Money from JSON string or map."
  {:tag Money :added "2.2.0" :ex/strict true}
  (^Money [x]
   (from-json x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (map? x)    (serializers-json/json-map->money x opts)
     (string? x) (serializers-json/json-string->money x opts)
     :else
     (throw
      (ex-info
       "Unsupported JSON money representation."
       {:op    :bankster.api.money/from-json
        :value x
        :class (class x)})))))

(defn from-json-text
  "Deserializes Money from raw JSON text."
  {:tag Money :added "2.2.0" :ex/strict true}
  (^Money [x]
   (from-json-text x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-json/json-text->money x opts)
     :else
     (throw
      (ex-info
       "Money JSON text must be a string."
       {:op    :bankster.api.money/from-json-text
        :value x
        :class (class x)})))))

(bu/defalias major-of io.randomseed.bankster.money/of-major)
(bu/defalias minor-of io.randomseed.bankster.money/of-minor)

(bu/auto-alias 'io.randomseed.bankster.money)

(doseq [[_ v] (ns-interns *ns*)]
  (alter-meta! v assoc :auto-alias true))
