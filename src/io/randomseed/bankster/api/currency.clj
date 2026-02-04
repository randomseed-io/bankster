(ns

    ^{:doc    "Bankster library, front API currency helpers."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.currency

  (:refer-clojure :exclude [resolve name symbol ns update])

  (:require [io.randomseed.bankster.api.registry     :as     api-registry]
            [io.randomseed.bankster.currency         :as         currency]
            [io.randomseed.bankster.registry         :as         registry]
            [io.randomseed.bankster.util             :as               bu]
            [io.randomseed.bankster.serializers.edn  :as  serializers-edn]
            [io.randomseed.bankster.serializers.json :as serializers-json])

  (:import  (io.randomseed.bankster Currency
                                    Registry)))

;;
;; Contextual helpers
;;

(bu/defalias with-registry api-registry/with)
(bu/defalias new           io.randomseed.bankster.currency/new-currency)

(defmacro with-default
  "Sets the dynamic binding of `io.randomseed.bankster.currency/*default*` to
  `currency` and evaluates body in an implicit `do`."
  [currency & body]
  `(binding [io.randomseed.bankster.currency/*default* ~currency]
     ~@body))

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
  "Strict currency coercion (throwing).

  Delegates to `io.randomseed.bankster.currency/unit`.

  Arity:
  - `()` uses `io.randomseed.bankster.currency/*default*`.
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

  Setting registry to `true` will cause a default registry to be used and lookup
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

(defn resolve-try
  "Non-throwing currency resolution (soft).

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

  Setting registry to `true` will cause a default registry to be used and lookup
  enforced."
  {:added "2.2.0"}
  (^Currency []                            (currency/unit-try currency/*default*))
  (^Currency [currency]                    (currency/unit-try currency))
  (^Currency [currency ^Registry registry] (if (nil? registry)
                                             (currency/unit-try currency)
                                             (if (true? registry)
                                               (currency/unit-try currency (registry/get))
                                               (currency/unit-try currency registry)))))

(defn resolve-all
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

(defn all
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

(defn of-domain
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

(defn of-kind
  "Returns a set of currencies of the given kind or `nil` if none exist.

  Uses `io.randomseed.bankster.currency/of-kind?` to filter `currency/all`."
  {:tag clojure.lang.IPersistentSet :added "2.2.0"}
  (^clojure.lang.IPersistentSet [kind]
   (when-some [currencies (currency/all)]
     (let [matches (filter #(currency/of-kind? kind %) currencies)]
       (when (seq matches) (set matches)))))
  (^clojure.lang.IPersistentSet [kind registry]
   (if (nil? registry)
     (of-kind kind)
     (let [registry  (if (true? registry) (registry/get) registry)
           currencies (currency/all registry)
           matches    (when (seq currencies)
                        (filter #(currency/of-kind? kind % registry) currencies))]
       (when (seq matches) (set matches))))))

(defn id
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

(defn id-str
  "Returns a currency identifier string without interning keywords.

  Delegates to `io.randomseed.bankster.currency/to-id-str`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-id-str currency)))

(defn code
  "Returns currency code as a string (without namespace).

  Delegates to `io.randomseed.bankster.currency/code`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/code currency))
  (^String [currency registry]
   (currency/code currency (registry-or-default registry))))

(defn code-str
  "Returns a currency code string without interning keywords.

  Delegates to `io.randomseed.bankster.currency/to-code-str`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-code-str currency)))

(defn nr
  "Returns currency numeric ID (ISO 4217 numeric code) as a long number or `nil`."
  {:tag Long :added "2.2.0"}
  (^Long [currency]
   (currency/nr currency))
  (^Long [currency registry]
   (currency/nr currency (registry-or-default registry))))


(defn auto-scaled?
  "Returns `true` if the currency scale is auto-scaled.

  Delegates to `io.randomseed.bankster.currency/auto-scaled?`."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/auto-scaled? currency))
  ([currency registry]
   (currency/auto-scaled? currency (registry-or-default registry))))

(defn info
  "Returns a map describing the given currency, including registry-associated
  properties when available.

  Base fields come from the currency's map representation (via `into {}`) and include
  any extension fields. When a registry can be consulted (default or explicitly
  passed), this function also adds:

  - `:countries` - a set of associated country IDs,
  - `:localized` - a localized properties map (locale keywords, including `:*`),
  - `:traits`    - a set of associated traits.

  Returns `nil` when the given value points to a registry currency but cannot be
  resolved. Locale argument is ignored.

  When `registry` is `true`, the default registry is used."
  {:tag clojure.lang.IPersistentMap :added "2.2.0"}
  ([currency]
   (currency/info currency))
  ([currency registry]
   (currency/info currency (registry-or-default registry)))
  ([currency locale registry]
   (currency/info currency locale (registry-or-default registry))))

(defn domain
  "Returns currency domain as a keyword or `nil`."
  {:tag clojure.lang.Keyword :added "2.2.0"}
  (^clojure.lang.Keyword [currency]
   (currency/domain currency))
  (^clojure.lang.Keyword [currency registry]
   (currency/domain currency (registry-or-default registry))))

(defn kind
  "Returns currency kind as a keyword or `nil`."
  {:tag clojure.lang.Keyword :added "2.2.0"}
  (^clojure.lang.Keyword [currency]
   (currency/kind currency))
  (^clojure.lang.Keyword [currency registry]
   (currency/kind currency (registry-or-default registry))))

(defn symbol
  "Returns a currency symbol for the given currency and locale.

  Delegates to `io.randomseed.bankster.currency/symbol`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/symbol currency))
  (^String [currency locale]
   (currency/symbol currency locale))
  (^String [currency locale registry]
   (currency/symbol currency locale (registry-or-default registry))))

(defn name
  "Returns a currency display name for the given currency and locale.

  Delegates to `io.randomseed.bankster.currency/display-name`."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/display-name currency))
  (^String [currency locale]
   (currency/display-name currency locale))
  (^String [currency locale registry]
   (currency/display-name currency locale (registry-or-default registry))))

(defn defined?
  "Returns `true` if any currency can be resolved from the given value."
  {:tag Boolean :added "2.2.0"}
  (^Boolean [currency]
   (currency/defined? currency))
  (^Boolean [currency registry]
   (currency/defined? currency (registry-or-default registry))))

(defn present?
  "Returns `true` if a currency can be resolved and is present in a registry."
  {:tag Boolean :added "2.2.0"}
  (^Boolean [currency]
   (currency/present? currency))
  (^Boolean [currency registry]
   (currency/present? currency (registry-or-default registry))))

(defn possible?
  "Returns `true` if the given value is a possible currency representation."
  {:tag Boolean :added "2.2.0"}
  (^Boolean [currency]
   (currency/possible? currency))
  (^Boolean [currency registry]
   (currency/possible? currency (registry-or-default registry))))

(defn definitive?
  "Returns `true` if the given value is a definitive currency representation."
  {:tag Boolean :added "2.2.0"}
  ^Boolean [currency]
  (currency/definitive? currency))

(defn normalize
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

(defn crypto?
  "Returns `true` if the given currency is a cryptocurrency.

  Delegates to `io.randomseed.bankster.currency/crypto?`."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/crypto? currency))
  ([currency registry]
   (currency/crypto? currency (registry-or-default registry))))

(defn stable?
  "Returns `true` if the given currency is a kind of stable currency.

  Delegates to `io.randomseed.bankster.currency/stable?`."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/stable? currency))
  ([currency registry]
   (currency/stable? currency (registry-or-default registry))))

(defn decentralized?
  "Returns `true` if the given currency is a kind of decentralized currency.

  Delegates to `io.randomseed.bankster.currency/decentralized?`."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/decentralized? currency))
  ([currency registry]
   (currency/decentralized? currency (registry-or-default registry))))

(defn ->map
  "Coerces a currency representation to a map of fields."
  {:tag clojure.lang.IPersistentMap :added "2.2.0"}
  [currency]
  (currency/to-map currency))

(defn ->edn
  "Serializes currency to an EDN tagged literal string."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-edn-string currency))
  (^String [currency opts]
   (currency/to-edn-string currency opts)))

(defn ->json
  "Serializes currency to a JSON string identifier."
  {:tag String :added "2.2.0"}
  (^String [currency]
   (currency/to-json-string currency))
  (^String [currency opts]
   (currency/to-json-string currency opts)))

(defn from-edn
  "Deserializes Currency from EDN string, keyword, or map."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (from-edn x nil))
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
       {:op    :bankster.api.currency/from-edn
        :value x
        :class (class x)})))))

(defn from-edn-text
  "Deserializes Currency from raw EDN text."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (from-edn-text x nil))
  (^Currency [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-edn/edn-string->currency x opts)
     :else
     (throw
      (ex-info
       "Currency EDN text must be a string."
       {:op    :bankster.api.currency/from-edn-text
        :value x
        :class (class x)})))))

(defn from-json
  "Deserializes Currency from JSON string or map."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (from-json x nil))
  (^Currency [x opts]
   (cond
     (nil? x)    nil
     (map? x)    (serializers-json/json-map->currency x opts)
     (string? x) (serializers-json/json-string->currency x opts)
     :else
     (throw
      (ex-info
       "Unsupported JSON currency representation."
       {:op    :bankster.api.currency/from-json
        :value x
        :class (class x)})))))

(defn from-json-text
  "Deserializes Currency from raw JSON text."
  {:tag Currency :added "2.2.0"}
  (^Currency [x]
   (from-json-text x nil))
  (^Currency [x opts]
   (cond
     (nil? x)    nil
     (string? x) (serializers-json/json-text->currency x opts)
     :else
     (throw
      (ex-info
       "Currency JSON text must be a string."
       {:op    :bankster.api.currency/from-json-text
        :value x
        :class (class x)})))))

(bu/auto-alias 'io.randomseed.bankster.currency)

(doseq [[_ v] (ns-interns *ns*)]
  (alter-meta! v assoc :auto-alias true))
