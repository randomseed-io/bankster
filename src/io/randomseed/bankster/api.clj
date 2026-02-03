(ns

    ^{:doc    "Bankster library, front API."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api

  (:refer-clojure :exclude [+ - * / = not= int long double float > < >= <= compare pos? neg?])

  (:require [io.randomseed.bankster]
            [io.randomseed.bankster.scale            :as            scale]
            [io.randomseed.bankster.money            :as            money]
            [io.randomseed.bankster.money.inter-ops  :as        money-ops]
            [io.randomseed.bankster.currency         :as         currency]
            [io.randomseed.bankster.registry         :as         registry]
            [io.randomseed.bankster.serializers.edn  :as    serializers-edn]
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

;;
;; Helpers
;;

(declare money-scale)

(defmacro registry-or-default
  "Resolves `true` into the current default registry, otherwise returns the given
  value."
  {:tag Registry :private true :added "2.2.0"}
  [registry]
  `(let [r# ~registry]
     (if (true? r#) (registry/get) r#)))

;;
;; Registry
;;

(defmacro with-registry
  "Alias for `io.randomseed.bankster.registry/with`.

  Sets a registry in a lexical context of the body to be used instead of a global one
  in functions which require the registry and it was not passed as an argument."
  {:added "2.2.0"}
  [registry & body]
  `(registry/with ~registry ~@body))

(defn default-registry
  "Returns the default registry (honors `registry/*default*`)."
  {:tag Registry :added "2.2.0"}
  []
  (registry/get))

(defn registry-state
  "Alias for `io.randomseed.bankster.registry/state`.

  Returns current state of the global registry."
  {:tag Registry :added "2.2.0"}
  []
  (registry/state))

(defn registry-hierarchy-derive
  "Alias for `io.randomseed.bankster.registry/hierarchy-derive`.

  Returns `registry` updated by deriving `tag` from `parent` inside a hierarchy
  identified by `hierarchy-name`."
  {:tag Registry :added "2.2.0"}
  [hierarchy-name tag parent registry]
  (if (true? registry)
    (registry/hierarchy-derive hierarchy-name tag parent (registry/get))
    (registry/hierarchy-derive hierarchy-name tag parent registry)))

(defn registry-hierarchy-derive!
  "Alias for `io.randomseed.bankster.registry/hierarchy-derive!`.

  Updates global registry by deriving `tag` from `parent` inside a hierarchy
  identified by `hierarchy-name`."
  {:tag Registry :added "2.2.0"}
  [hierarchy-name tag parent]
  (registry/hierarchy-derive! hierarchy-name tag parent))

;;
;; Scale
;;

(defmacro with-rounding
  "Alias for `io.randomseed.bankster.scale/with-rounding`.

  Sets the rounding mode for operations on scaled values.

  The first argument should be a valid rounding (from `io.randomseed.bankster.scale`
  or `java.math.RoundingMode`) or one of the following:

  CEILING     - rounds towards positive infinity.
  DOWN        - rounds towards zero.
  FLOOR       - rounds towards negative infinity.
  HALF_DOWN   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  HALF_EVEN   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  HALF_UP     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  UP          - rounds away from zero
  UNNECESSARY - asserts that the requested operation has an exact result, hence no rounding is necessary."
  {:added "2.2.0"}
  [rounding-mode & body]
  `(scale/with-rounding ~rounding-mode ~@body))

(defmacro with-rescaling
  "Alias for `io.randomseed.bankster.scale/with-rescaling`.

  Enables re-scaling on some consecutive operations which support it and sets the
  rounding mode for operations on scaled values. Internally sets `scale/*each*` to
  true and `scale/*rounding-mode*` to the given value.

  The first argument should be a valid rounding (from `io.randomseed.bankster.scale`
  or `java.math.RoundingMode`) or one of the following:

  CEILING     - rounds towards positive infinity.
  DOWN        - rounds towards zero.
  FLOOR       - rounds towards negative infinity.
  HALF_DOWN   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  HALF_EVEN   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  HALF_UP     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  UP          - rounds away from zero
  UNNECESSARY - asserts that the requested operation has an exact result, hence no rounding is necessary."
  {:added "2.2.0"}
  [rounding-mode & body]
  `(scale/with-rescaling ~rounding-mode ~@body))

(defn amount
  "Returns the amount of a scalable as a `BigDecimal`.

  Delegates to `io.randomseed.bankster.scale/amount`."
  {:tag BigDecimal :added "2.2.0"}
  (^BigDecimal [x]
   (scale/amount x))
  (^BigDecimal [x sc]
   (scale/amount x sc))
  (^BigDecimal [x sc rounding]
   (scale/amount x sc rounding)))

(defn scale
  "Returns scale for Money and Currency values.

  - For `Money`: returns the amount scale (may differ from currency nominal scale).
  - For `Currency`: returns the nominal scale (or `nil` when auto-scaled).

  Delegates to `io.randomseed.bankster.api/money-scale`."
  {:tag Long :added "2.2.0"}
  [x]
  (money-scale x))

(defn auto-scaled?
  "Returns `true` if a scalable's derived scale is auto-scaled.

  Delegates to `io.randomseed.bankster.scale/auto?`."
  {:tag Boolean :added "2.2.0"}
  ^Boolean [x]
  (scale/auto? x))

;;
;; Currency
;;

(defmacro with-currency
  "Alias for `io.randomseed.bankster.currency/with`.

  Sets a default currency in a lexical context of the body."
  {:added "2.2.0"}
  [currency & body]
  `(currency/with ~currency ~@body))

(defmacro currency-of
  "Alias for `io.randomseed.bankster.currency/of`.

  Returns a currency for the given value by querying the given registry or a global
  registry, which may be shadowed by `io.randomseed.bankster.registry/*default*`
  (see `io.randomseed.bankster.registry/with` or `currency/with-registry`)."
  {:added "2.2.0"}
  [& args]
  `(currency/of ~@args))

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

(defn currency-resolve-all
  "Returns all currencies matching the given hint in a registry.

  Delegates to `io.randomseed.bankster.currency/resolve-all` (soft; returns `nil` on
  no match).

  `registry` may be `true` to force the default registry."
  {:tag clojure.lang.IPersistentSet :added "2.2.0"}
  (^clojure.lang.IPersistentSet [currency]
   (currency/resolve-all currency))
  (^clojure.lang.IPersistentSet [currency registry]
   (if (nil? registry)
     (currency/resolve-all currency)
     (if (true? registry)
       (currency/resolve-all currency (registry/get))
       (currency/resolve-all currency registry)))))

(defn currency-all
  "Returns a sequence of all `Currency` objects in a registry or `nil` if there are
  no currencies.

  Delegates to `io.randomseed.bankster.currency/all`."
  {:tag clojure.lang.APersistentMap$ValSeq :added "2.2.0"}
  (^clojure.lang.APersistentMap$ValSeq []
   (currency/all))
  (^clojure.lang.APersistentMap$ValSeq [registry]
   (if (nil? registry)
     (currency/all)
     (if (true? registry)
       (currency/all (registry/get))
       (currency/all registry)))))

(defn currency-of-domain
  "Returns a set of currencies assigned to the given domain or `nil` if none exist.

  Delegates to `io.randomseed.bankster.currency/of-domain`."
  {:tag clojure.lang.PersistentTreeSet :added "2.2.0"}
  (^clojure.lang.PersistentTreeSet [domain]
   (currency/of-domain domain))
  (^clojure.lang.PersistentTreeSet [domain registry]
   (if (nil? registry)
     (currency/of-domain domain)
     (if (true? registry)
       (currency/of-domain domain (registry/get))
       (currency/of-domain domain registry)))))

(defn currency-of-kind
  "Returns a set of currencies of the given kind or `nil` if none exist.

  Uses `io.randomseed.bankster.currency/of-kind?` to filter `currency/all`."
  {:tag clojure.lang.IPersistentSet :added "2.2.0"}
  (^clojure.lang.IPersistentSet [kind]
   (when-some [currencies (currency/all)]
     (let [matches (filter #(currency/of-kind? kind %) currencies)]
       (when (seq matches) (set matches)))))
  (^clojure.lang.IPersistentSet [kind registry]
   (if (nil? registry)
     (currency-of-kind kind)
     (let [registry  (if (true? registry) (registry/get) registry)
           currencies (currency/all registry)
           matches    (when (seq currencies)
                        (filter #(currency/of-kind? kind % registry) currencies))]
       (when (seq matches) (set matches))))))

(defn currency-id
  "Returns currency ID (keyword).

  Delegates to `io.randomseed.bankster.currency/id`.

  - `([currency])` is registry-light: it may return a keyword even if the currency
    is not registered.
  - `([currency registry])` is strict when `registry` is non-nil."
  {:tag clojure.lang.Keyword :added "2.2.0"}
  (^clojure.lang.Keyword [currency]
   (currency/id currency))
  (^clojure.lang.Keyword [currency registry]
   (currency/id currency (registry-or-default registry))))

(defn currency-id-str
  "Returns a currency identifier string without interning keywords.

  Delegates to `io.randomseed.bankster.currency/to-id-str`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-id-str currency)))

(defn currency-code
  "Returns currency code as a string (without namespace).

  Delegates to `io.randomseed.bankster.currency/code`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/code currency))
  (^String [currency registry]
   (currency/code currency (registry-or-default registry))))

(defn currency-code-str
  "Returns a currency code string without interning keywords.

  Delegates to `io.randomseed.bankster.currency/to-code-str`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-code-str currency)))

(defn currency-nr
  "Returns currency numeric ID (ISO 4217 numeric code) as a long number or `nil`."
  {:tag Long :added "2.2.0"}
  (^Long [currency]
   (currency/nr currency))
  (^Long [currency registry]
   (currency/nr currency (registry-or-default registry))))

(defn currency-scale
  "Returns nominal currency scale (decimal places) or `nil` for auto-scaled."
  {:tag Long :added "2.2.0"}
  (^Long [currency]
   (currency/sc currency))
  (^Long [currency registry]
   (currency/sc currency (registry-or-default registry))))

(defn currency-domain
  "Returns currency domain as a keyword or `nil`."
  {:tag clojure.lang.Keyword :added "2.2.0"}
  (^clojure.lang.Keyword [currency]
   (currency/domain currency))
  (^clojure.lang.Keyword [currency registry]
   (currency/domain currency (registry-or-default registry))))

(defn currency-kind
  "Returns currency kind as a keyword or `nil`."
  {:tag clojure.lang.Keyword :added "2.2.0"}
  (^clojure.lang.Keyword [currency]
   (currency/kind currency))
  (^clojure.lang.Keyword [currency registry]
   (currency/kind currency (registry-or-default registry))))

(defn currency-symbol
  "Returns a currency symbol for the given currency and locale.

  Delegates to `io.randomseed.bankster.currency/symbol`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/symbol currency))
  (^String [currency locale]
   (currency/symbol currency locale))
  (^String [currency locale registry]
   (currency/symbol currency locale (registry-or-default registry))))

(defn currency-name
  "Returns a currency display name for the given currency and locale.

  Delegates to `io.randomseed.bankster.currency/display-name`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/display-name currency))
  (^String [currency locale]
   (currency/display-name currency locale))
  (^String [currency locale registry]
   (currency/display-name currency locale (registry-or-default registry))))

(defn currency-defined?
  "Returns `true` if any currency can be resolved from the given value."
  {:tag Boolean :added "2.2.0"}
  (^Boolean [currency]
   (currency/defined? currency))
  (^Boolean [currency registry]
   (currency/defined? currency (registry-or-default registry))))

(defn currency-present?
  "Returns `true` if a currency can be resolved and is present in a registry."
  {:tag Boolean :added "2.2.0"}
  (^Boolean [currency]
   (currency/present? currency))
  (^Boolean [currency registry]
   (currency/present? currency (registry-or-default registry))))

(defn currency-possible?
  "Returns `true` if the given value is a possible currency representation."
  {:tag Boolean :added "2.2.0"}
  (^Boolean [currency]
   (currency/possible? currency))
  (^Boolean [currency registry]
   (currency/possible? currency (registry-or-default registry))))

(defn currency-definitive?
  "Returns `true` if the given value is a definitive currency representation."
  {:tag Boolean :added "2.2.0"}
  ^Boolean [currency]
  (currency/definitive? currency))

(defn currency-normalize
  "Normalizes currency literal input to a canonical representation.

  Unwraps single-element vectors, keywordizes `:id` in maps, and normalizes symbols
  to keywords when possible. Delegates to `currency/parse-currency-code`."
  {:added "2.2.0"}
  [currency]
  (currency/parse-currency-code currency))

(defn currency?
  "Returns `true` when `x` is an instance of `io.randomseed.bankster.Currency`."
  {:tag Boolean :added "2.2.0"}
  [x]
  (currency/currency? x))

(defn currency-auto-scaled?
  "Returns `true` if the given currency has an automatic (unknown) scale."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/auto-scaled? currency))
  ([currency registry]
   (currency/auto-scaled? currency (registry-or-default registry))))

(defn iso-currency?
  "Returns `true` when the given currency is a kind of ISO currency."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/iso? currency))
  ([currency registry]
   (currency/iso? currency (registry-or-default registry))))

(defn currency-crypto?
  "Returns `true` if the given currency is a cryptocurrency.

  Delegates to `io.randomseed.bankster.currency/crypto?`."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/crypto? currency))
  ([currency registry]
   (currency/crypto? currency (registry-or-default registry))))

(defn currency-stable?
  "Returns `true` if the given currency is a kind of stable currency.

  Delegates to `io.randomseed.bankster.currency/stable?`."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/stable? currency))
  ([currency registry]
   (currency/stable? currency (registry-or-default registry))))

(defn currency-decentralized?
  "Returns `true` if the given currency is a kind of decentralized currency.

  Delegates to `io.randomseed.bankster.currency/decentralized?`."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/decentralized? currency))
  ([currency registry]
   (currency/decentralized? currency (registry-or-default registry))))

(def ^{:tag      Boolean
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/has-trait?))}
  currency-has-trait?
  "Alias for `io.randomseed.bankster.currency/has-trait?`.

  Returns `true` if a currency has any trait (or a specific trait tag when provided)."
  currency/has-trait?)

(def ^{:tag      Boolean
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/of-trait?))}
  currency-of-trait?
  "Alias for `io.randomseed.bankster.currency/of-trait?`.

  Checks if any trait of the given currency equals (or derives from) the given tag."
  currency/of-trait?)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/register))}
  currency-register
  "Alias for `io.randomseed.bankster.currency/register`.

  Adds a currency and optional associated data to the given registry. Returns an
  updated registry."
  currency/register)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/unregister))}
  currency-unregister
  "Alias for `io.randomseed.bankster.currency/unregister`.

  Removes a currency from the given registry and returns updated registry."
  currency/unregister)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/register!))}
  currency-register!
  "Alias for `io.randomseed.bankster.currency/register!`.

  Adds a currency to the global registry (mutating). Returns updated registry."
  currency/register!)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/unregister!))}
  currency-unregister!
  "Alias for `io.randomseed.bankster.currency/unregister!`.

  Removes a currency from the global registry (mutating). Returns updated registry."
  currency/unregister!)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/set-traits))}
  currency-set-traits
  "Alias for `io.randomseed.bankster.currency/set-traits`.

  Sets traits for the given currency in the registry."
  currency/set-traits)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/set-traits!))}
  currency-set-traits!
  "Alias for `io.randomseed.bankster.currency/set-traits!`.

  Sets traits for the given currency in the global registry (mutating)."
  currency/set-traits!)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/add-traits))}
  currency-add-traits
  "Alias for `io.randomseed.bankster.currency/add-traits`.

  Adds traits for the given currency in the registry (union)."
  currency/add-traits)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/add-traits!))}
  currency-add-traits!
  "Alias for `io.randomseed.bankster.currency/add-traits!`.

  Adds traits for the given currency in the global registry (mutating)."
  currency/add-traits!)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/remove-traits))}
  currency-remove-traits
  "Alias for `io.randomseed.bankster.currency/remove-traits`.

  Removes traits for the given currency in the registry."
  currency/remove-traits)

(def ^{:tag      Registry
       :added    "2.2.0"
       :arglists (:arglists (meta #'currency/remove-traits!))}
  currency-remove-traits!
  "Alias for `io.randomseed.bankster.currency/remove-traits!`.

  Removes traits for the given currency in the global registry (mutating)."
  currency/remove-traits!)

;;
;; Info
;;

(defn info
  "Returns info map for currency or money.

  - For `Money` values, delegates to `io.randomseed.bankster.money/info`.
  - Otherwise delegates to `io.randomseed.bankster.currency/info`.

  Registry arguments are ignored for `Money`. When `registry` is `true`, the default
  registry is used."
  {:tag clojure.lang.IPersistentMap :added "2.2.0"}
  ([x]
   (if (money/money? x)
     (money/info x)
     (currency/info x)))
  ([x registry]
   (if (money/money? x)
     (money/info x)
     (currency/info x (registry-or-default registry))))
  ([x locale registry]
   (if (money/money? x)
     (money/info x)
     (currency/info x locale (registry-or-default registry)))))

;;
;; Money
;;

(defmacro money-of
  "Alias for `io.randomseed.bankster.money/of`.

  Returns the amount of money as a Money object consisting of a currency and a
  value. Currency can be a currency object and for registered currencies: a keyword,
  a symbol or a string (e.g. `EUR`, `:EUR`, \"PLN\" or crypto/ETH), or even a
  number (for ISO-compliant currencies).

  The given amount can be any numeric value or a string that can be converted to
  `java.math.BigDecimal`.

  When a number must be downscaled to fulfill the number of decimal places for a
  currency, rounding mode must be given, which may be a symbol, a keyword or a string
  of the following:

  * `CEILING`     - rounds towards positive infinity.
  * `DOWN`        - rounds towards zero.
  * `FLOOR`       - rounds towards negative infinity.
  * `HALF_DOWN`   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  * `HALF_EVEN`   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  * `HALF_UP`     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  * `UP`          - rounds away from zero
  * `UNNECESSARY` - asserts that the requested operation has an exact result, hence no rounding is necessary.

  To create a monetary object using function, call `io.randomseed.bankster.money/value`.

  Be careful about using number literals for big-scale amounts (16-17 digits). Use
  either big decimal literals, e.g. 1234.45689101112M, or strings."
  {:added "2.2.0"}
  [& args]
  `(money/of ~@args))

(defmacro money-major-of
  "Alias for `io.randomseed.bankster.money/of-major`.

  Like `io.randomseed.bankster.money/of` but sets the amount of major part only."
  {:added "2.2.0"}
  [& args]
  `(money/of-major ~@args))

(defmacro money-minor-of
  "Alias for `io.randomseed.bankster.money/of-minor`.

  Like `io.randomseed.bankster.money/of` but sets the amount of minor part only."
  {:added "2.2.0"}
  [& args]
  `(money/of-minor ~@args))

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

(defn money-of-registry
  "Ensures that a currency of the given money originates from the given registry. If
  the registry is not given and a dynamic registry is not set, the default one is
  used. Rescales the amount if needed to match the nominal scale. Optional
  rounding-mode can be supplied to be used when downscaling is needed (nominal
  currency from a registry has lower number of decimal places than the amount of
  money).

  Money can be expressed as a `Money` object or any other object that will create
  `Money` when passed to the `value` function. Returns money."
  {:tag Money :added "2.2.0"}
  (^Money [money]
   (money/of-registry money))
  (^Money [registry money]
   (money/of-registry (registry-or-default registry) money))
  (^Money [registry money rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/of-registry (registry-or-default registry) money rounding)
     (money/of-registry (registry-or-default registry) money))))

(defn money-cast
  "Casts an existing Money object to another having a different currency, rescaling
  the amount and optionally rounding it. This is useful when operating on multiple
  currency registries compatible with different data sources or processing engines.
  For simply ensuring that a currency is sourced in the right registry, use
  `money-of-registry`.

  Delegates to `io.randomseed.bankster.money/cast`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [money]
               ^Money [money currency]
               ^Money [money currency rounding])}
  (^Money [money]
   (money/cast money))
  (^Money [money currency]
   (money/cast money currency))
  (^Money [money currency rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/cast money currency rounding)
     (money/cast money currency))))

(defn money-cast-try
  "Soft variant of `money-cast`.

  Returns `nil` when casting fails (e.g. missing currency or required rounding)."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [money]
               ^Money [money currency]
               ^Money [money currency rounding])}
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

(defn money-major
  "First-class constructor for major-part amounts (amount-first).

  Delegates to `io.randomseed.bankster.money/major-value`."
  {:added    "2.2.0"
   :tag      Money
   :arglists '(^Money [amount]
               ^Money [amount currency]
               ^Money [amount currency rounding]
               ^Money [amount currency registry]
               ^Money [amount currency rounding registry])}
  (^Money [amount]
   (money/major-value amount))
  (^Money [amount c]
   (money/major-value (currency c) amount))
  (^Money [amount c rounding-or-registry]
   (if (instance? Registry rounding-or-registry)
     (money/major-value (currency c rounding-or-registry) amount)
     (if rounding-or-registry
       (money/major-value (currency c) amount (scale/post-parse-rounding rounding-or-registry))
       (money/major-value (currency c) amount))))
  (^Money [amount c rounding ^Registry registry]
   (if rounding
     (money/major-value (currency c registry) amount (scale/post-parse-rounding rounding))
     (money/major-value (currency c registry) amount))))

(defn money-minor
  "First-class constructor for minor-part amounts (amount-first).

  Delegates to `io.randomseed.bankster.money/minor-value`."
  {:added    "2.2.0"
   :tag      Money
   :arglists '(^Money [amount]
               ^Money [amount currency]
               ^Money [amount currency rounding]
               ^Money [amount currency registry]
               ^Money [amount currency rounding registry])}
  (^Money [amount]
   (money/minor-value amount))
  (^Money [amount c]
   (money/minor-value (currency c) amount))
  (^Money [amount c rounding-or-registry]
   (if (instance? Registry rounding-or-registry)
     (money/minor-value (currency c rounding-or-registry) amount)
     (if rounding-or-registry
       (money/minor-value (currency c) amount (scale/post-parse-rounding rounding-or-registry))
       (money/minor-value (currency c) amount))))
  (^Money [amount c rounding ^Registry registry]
   (if rounding
     (money/minor-value (currency c registry) amount (scale/post-parse-rounding rounding))
     (money/minor-value (currency c registry) amount))))

(defn money-amount
  "Returns Money amount as `BigDecimal`."
  {:tag BigDecimal :added "2.2.0"}
  (^BigDecimal [money]
   (money/amount money))
  (^BigDecimal [a b]
   (money/amount a b))
  (^BigDecimal [a b rounding]
   (money/amount a b rounding)))

(defn money-currency
  "Returns Money currency."
  {:tag Currency :added "2.2.0"}
  (^Currency [money]
   (money/currency money))
  (^Currency [a b]
   (money/currency a b))
  (^Currency [a b rounding]
   (money/currency a b rounding)))

(defn money-scale
  "Returns the scale of a monetary amount.

  - For `Money`: returns the amount scale (may differ from currency nominal scale).
  - For `Currency`: returns the nominal scale (or `nil` when auto-scaled)."
  {:tag Long :added "2.2.0"}
  [x]
  (cond
    (instance? Money x)
    (clojure.core/long (money/scale ^Money x))

    (instance? Currency x)
    (currency/scale x)

    :else
    (when-some [^Money m (money/value x)]
      (clojure.core/long (money/scale m)))))

(defn money-strip
  "Strips trailing zeros from a `Money` amount."
  {:tag Money :added "2.2.0"}
  [money]
  (when-some [^Money m (money/value money)]
    (money/strip m)))

(defn money-round-to
  "Rounds a Money to a given interval.

  Delegates to `io.randomseed.bankster.money/round-to`."
  {:tag Money :added "2.2.0"}
  (^Money [^Money money]
   (money/round-to money))
  (^Money [^Money money interval]
   (money/round-to money interval))
  (^Money [^Money money interval rounding]
   (money/round-to money interval (scale/post-parse-rounding rounding))))

(defn money-allocate
  "Allocates a monetary amount into parts according to integer ratios.

  Delegates to `io.randomseed.bankster.money/allocate`."
  {:tag clojure.lang.IPersistentVector :added "2.2.0"}
  [^Money money ratios]
  (money/allocate money ratios))

(defn money-distribute
  "Distributes a monetary amount into `n` as-even-as-possible parts.

  Delegates to `io.randomseed.bankster.money/distribute`."
  {:tag clojure.lang.IPersistentVector :added "2.2.0"}
  [^Money money n]
  (money/distribute money n))

(defn money-format
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

(defn money-parse
  "Parses money-like input into a `Money` value.

  Delegates to `io.randomseed.bankster.money/parse`. Rounding is parsed via
  `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [amount]
               ^Money [currency amount]
               ^Money [currency amount rounding])}
  (^Money [amount]
   (money/parse amount))
  (^Money [currency amount]
   (money/parse currency amount))
  (^Money [currency amount rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/parse currency amount rounding)
     (money/parse currency amount))))

(defn money-parse-major
  "Parses money-like input and treats the amount as major part.

  Delegates to `io.randomseed.bankster.money/parse-major`. Rounding is parsed via
  `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [amount]
               ^Money [currency amount]
               ^Money [currency amount rounding])}
  (^Money [amount]
   (money/parse-major amount))
  (^Money [currency amount]
   (money/parse-major currency amount))
  (^Money [currency amount rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/parse-major currency amount rounding)
     (money/parse-major currency amount))))

(defn money-parse-minor
  "Parses money-like input and treats the amount as minor part.

  Delegates to `io.randomseed.bankster.money/parse-minor`. Rounding is parsed via
  `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '(^Money [amount]
               ^Money [currency amount]
               ^Money [currency amount rounding])}
  (^Money [amount]
   (money/parse-minor amount))
  (^Money [currency amount]
   (money/parse-minor currency amount))
  (^Money [currency amount rounding]
   (if-some [rounding (scale/post-parse-rounding rounding)]
     (money/parse-minor currency amount rounding)
     (money/parse-minor currency amount))))

(defn money-normalize
  "Normalizes money-like input into a `Money` value.

  - `([amount])` accepts `Money`, EDN-like map (via `money/of-map`), or a literal
    amount (delegates to `money/parse`).
  - `([amount currency])` and `([amount currency rounding])` use amount-first order
    and delegate to `money/parse`. Rounding is parsed via
    `scale/post-parse-rounding`."
  {:tag      Money
   :added    "2.2.0"
   :arglists '([amount] [amount currency] [amount currency rounding])}
  [amount & args]
  (let [argc (count args)]
    (cond
      (zero? argc)            (cond
                                (nil? amount) nil
                                (map? amount) (money/of-map amount)
                                :else         (money/parse amount))
      (clojure.core/> argc 2) (throw (clojure.lang.ArityException. (inc argc) "money-normalize"))
      :else                   (let [currency (nth args 0)]
                                (if (== 2 argc)
                                  (money/parse currency amount (scale/post-parse-rounding (nth args 1)))
                                  (money/parse currency amount))))))

(defn money-is-zero?
  "Returns `true` if the given monetary amount is zero.

  Delegates to `io.randomseed.bankster.money/is-zero?`."
  {:tag Boolean :added "2.2.0"}
  [^Money money]
  (money/is-zero? money))

(defn money-same-currencies?
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
;;

(defn currency->map
  "Coerces a currency representation to a map of fields."
  {:tag clojure.lang.IPersistentMap :added "2.2.0"}
  [currency]
  (currency/to-map currency))

(defn currency->edn
  "Serializes currency to an EDN tagged literal string."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-edn-string currency))
  (^String [currency opts]
   (currency/to-edn-string currency opts)))

(defn currency->json
  "Serializes currency to a JSON string identifier."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-json-string currency))
  (^String [currency opts]
   (currency/to-json-string currency opts)))

(defn currency-from-edn
  "Deserializes Currency from EDN string, keyword, or map."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (currency-from-edn x nil))
  (^Currency [x opts]
   (cond
     (nil? x)     nil
     (map? x)     (serializers-edn/edn-map->currency x opts)
     (string? x)  (serializers-edn/edn-string->currency x opts)
     (keyword? x) (serializers-edn/edn-keyword->currency x opts)
     :else
     (throw
      (ex-info
       "Unsupported EDN currency representation."
       {:op    :bankster.api/currency-from-edn
        :value x
        :class (class x)})))))

(defn currency-from-edn-text
  "Deserializes Currency from raw EDN text."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (currency-from-edn-text x nil))
  (^Currency [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-edn/edn-string->currency x opts)
     :else
     (throw
      (ex-info
       "Currency EDN text must be a string."
       {:op    :bankster.api/currency-from-edn-text
        :value x
        :class (class x)})))))

(defn currency-from-json
  "Deserializes Currency from JSON string or map."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (currency-from-json x nil))
  (^Currency [x opts]
   (cond
     (nil? x)    nil
     (map? x)    (serializers-json/json-map->currency x opts)
     (string? x) (serializers-json/json-string->currency x opts)
     :else
     (throw
      (ex-info
       "Unsupported JSON currency representation."
       {:op    :bankster.api/currency-from-json
        :value x
        :class (class x)})))))

(defn currency-from-json-text
  "Deserializes Currency from raw JSON text."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (currency-from-json-text x nil))
  (^Currency [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-json/json-text->currency x opts)
     :else
     (throw
      (ex-info
       "Currency JSON text must be a string."
       {:op    :bankster.api/currency-from-json-text
        :value x
        :class (class x)})))))

(defn money->map
  "Coerces a money-like value into an EDN-friendly map."
  {:tag clojure.lang.IPersistentMap :added "2.2.0"}
  [money]
  (money/to-map money))

(defn money->edn
  "Serializes money-like value to an EDN tagged literal string."
  {:tag String :added "2.2.0"}
  (^String [money]
   (when-some [^Money m (money/value money)]
     (serializers-edn/money->edn-string m)))
  (^String [money opts]
   (when-some [^Money m (money/value money)]
     (serializers-edn/money->edn-string m opts))))

(defn money->json
  "Serializes money-like value to a JSON string."
  {:tag String :added "2.2.0"}
  (^String [money]
   (money/to-json-string money))
  (^String [money opts]
   (money/to-json-string money opts)))

(defn money-from-edn
  "Deserializes Money from EDN string or map."
  {:tag Money :added "2.2.0"}
  (^Money [x]
   (money-from-edn x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (map? x)    (serializers-edn/edn-map->money x opts)
     (string? x) (serializers-edn/edn-string->money x opts)
     :else
     (throw
      (ex-info
       "Unsupported EDN money representation."
       {:op    :bankster.api/money-from-edn
        :value x
        :class (class x)})))))

(defn money-from-edn-text
  "Deserializes Money from raw EDN text."
  {:tag Money :added "2.2.0"}
  (^Money [x]
   (money-from-edn-text x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-edn/edn-string->money x opts)
     :else
     (throw
      (ex-info
       "Money EDN text must be a string."
       {:op    :bankster.api/money-from-edn-text
        :value x
        :class (class x)})))))

(defn money-from-json
  "Deserializes Money from JSON string or map."
  {:tag Money :added "2.2.0"}
  (^Money [x]
   (money-from-json x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (map? x)    (serializers-json/json-map->money x opts)
     (string? x) (serializers-json/json-string->money x opts)
     :else
     (throw
      (ex-info
       "Unsupported JSON money representation."
       {:op    :bankster.api/money-from-json
        :value x
        :class (class x)})))))

(defn money-from-json-text
  "Deserializes Money from raw JSON text."
  {:tag Money :added "2.2.0"}
  (^Money [x]
   (money-from-json-text x nil))
  (^Money [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-json/json-text->money x opts)
     :else
     (throw
      (ex-info
       "Money JSON text must be a string."
       {:op    :bankster.api/money-from-json-text
        :value x
        :class (class x)})))))

;;
;; Arithmetic (Money-aware)
;;

(def ^{:arglists (:arglists (meta #'money-ops/+))
       :added    "2.2.0"}
  +
  "Alias for `io.randomseed.bankster.money.inter-ops/+`.

  Calls `io.randomseed.bankster.money/add` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/+`."
  money-ops/+)

(def ^{:arglists (:arglists (meta #'money-ops/-))
       :added    "2.2.0"}
  -
  "Alias for `io.randomseed.bankster.money.inter-ops/-`.

  Calls `io.randomseed.bankster.money/sub` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/-`."
  money-ops/-)

(def ^{:arglists (:arglists (meta #'money-ops/*))
       :added    "2.2.0"}
  *
  "Alias for `io.randomseed.bankster.money.inter-ops/*`.

  Calls `io.randomseed.bankster.money/mul` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/*`."
  money-ops/*)

(def ^{:arglists (:arglists (meta #'money-ops//))
       :added    "2.2.0"}
  /
  "Alias for `io.randomseed.bankster.money.inter-ops//`.

  Calls `io.randomseed.bankster.money/div` when any argument is a kind of Money,
  otherwise delegates to `clojure.core//`."
  money-ops//)

(def ^{:arglists (:arglists (meta #'money-ops/=))
       :added    "2.2.0"}
  =
  "Alias for `io.randomseed.bankster.money.inter-ops/=`.

  Calls `io.randomseed.bankster.money/eq?` for Money argument, `clojure.core/=` for
  other data types."
  money-ops/=)

(def ^{:arglists (:arglists (meta #'money-ops/not=))
       :added    "2.2.0"}
  not=
  "Alias for `io.randomseed.bankster.money.inter-ops/not=`.

  Calls `io.randomseed.bankster.money/ne?` for Money argument, `clojure.core/not=`
  for other data types."
  money-ops/not=)

(def ^{:arglists (:arglists (meta #'money-ops/>))
       :added    "2.2.0"}
  >
  "Alias for `io.randomseed.bankster.money.inter-ops/>`.

  Calls `io.randomseed.bankster.money/gt?` for Money argument, `clojure.core/>` for
  other data types."
  money-ops/>)

(def ^{:arglists (:arglists (meta #'money-ops/>=))
       :added    "2.2.0"}
  >=
  "Alias for `io.randomseed.bankster.money.inter-ops/>=`.

  Calls `io.randomseed.bankster.money/ge?` for Money argument, `clojure.core/>=` for
  other data types."
  money-ops/>=)

(def ^{:arglists (:arglists (meta #'money-ops/<))
       :added    "2.2.0"}
  <
  "Alias for `io.randomseed.bankster.money.inter-ops/<`.

  Calls `io.randomseed.bankster.money/lt?` for Money argument, `clojure.core/<` for
  other data types."
  money-ops/<)

(def ^{:arglists (:arglists (meta #'money-ops/<=))
       :added    "2.2.0"}
  <=
  "Alias for `io.randomseed.bankster.money.inter-ops/<=`.

  Calls `io.randomseed.bankster.money/le?` for Money argument, `clojure.core/<=` for
  other data types."
  money-ops/<=)

(def ^{:arglists (:arglists (meta #'money-ops/compare))
       :added    "2.2.0"}
  compare
  "Alias for `io.randomseed.bankster.money.inter-ops/compare`.

  Calls `io.randomseed.bankster.money/compare` for Money argument,
  `clojure.core/compare` for other data types."
  money-ops/compare)

(def ^{:arglists (:arglists (meta #'money-ops/pos?))
       :added    "2.2.0"}
  pos?
  "Alias for `io.randomseed.bankster.money.inter-ops/pos?`.

  Calls `io.randomseed.bankster.money/is-pos?` for Money argument,
  `clojure.core/pos?` for other data types."
  money-ops/pos?)

(def ^{:arglists (:arglists (meta #'money-ops/neg?))
       :added    "2.2.0"}
  neg?
  "Alias for `io.randomseed.bankster.money.inter-ops/neg?`.

  Calls `io.randomseed.bankster.money/is-neg?` for Money argument,
  `clojure.core/neg?` for other data types."
  money-ops/neg?)

(def ^{:arglists (:arglists (meta #'money-ops/long))
       :added    "2.2.0"}
  long
  "Alias for `io.randomseed.bankster.money.inter-ops/long`.

  Calls `io.randomseed.bankster.money/major->long` for Money argument, falls back to
  `clojure.core/long` for other data types."
  money-ops/long)

(def ^{:arglists (:arglists (meta #'money-ops/int))
       :added    "2.2.0"}
  int
  "Alias for `io.randomseed.bankster.money.inter-ops/int`.

  Calls `io.randomseed.bankster.money/major->int` for Money argument, falls back to
  `clojure.core/int` for other data types."
  money-ops/int)

(def ^{:arglists (:arglists (meta #'money-ops/double))
       :added    "2.2.0"}
  double
  "Alias for `io.randomseed.bankster.money.inter-ops/double`.

  Calls `io.randomseed.bankster.money/->double` for Money argument, falls back to
  `clojure.core/double` for other data types."
  money-ops/double)

(def ^{:arglists (:arglists (meta #'money-ops/float))
       :added    "2.2.0"}
  float
  "Alias for `io.randomseed.bankster.money.inter-ops/float`.

  Calls `io.randomseed.bankster.money/->float` for Money argument, falls back to
  `clojure.core/float` for other data types."
  money-ops/float)
