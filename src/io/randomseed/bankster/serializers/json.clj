;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Bankster library, JSON-oriented serialization helpers."
      :author "Paweł Wilk"
      :added  "2.1.0"}

    io.randomseed.bankster.serializers.json

  (:require [clojure.string                            :as      str]
            [io.randomseed.bankster.currency           :as currency]
            [io.randomseed.bankster.registry           :as registry]
            [io.randomseed.bankster.scale              :as    scale]
            [io.randomseed.bankster.serializers.common :as   common])

  (:import  (io.randomseed.bankster Currency
                                    Registry
                                    Money)
            (java.math              BigDecimal
                                    RoundingMode)))

;;
;; Protocols.
;;

(defprotocol ^{:added "2.1.0"} JsonSerializable
  "Protocol for JSON serialization of Bankster types.

  Options map supports:
  - `:code-only?` - when truthy, namespace is omitted: `:crypto/ETH` → `\"ETH\"`
  - `:full?`      - when truthy, delegates to `to-json-full-map` for full serialization
  - `:keys`       - vector of keys to include; supports nested opts via map elements
  - `:rescale`    - integer scale to rescale amounts to (requires `:rounding-mode` for downscaling)"

  (^{:tag clojure.lang.IPersistentMap :added "2.1.0" :ex/soft true}
   to-json-map
   [this] [this opts]
   "Serializes the value to a minimal JSON-friendly map.
   For Money: {:currency \"PLN\", :amount \"12.30\"}
   For Currency: {:id \"PLN\"}")

  (^{:tag clojure.lang.IPersistentMap :added "2.1.0" :ex/soft true}
   to-json-full-map
   [this] [this opts]
   "Serializes the value to a full JSON-friendly map with all available fields.
   For Currency: {:id, :numeric, :scale, :kind, :domain}
   For Money: {:currency {...}, :amount} plus any extended fields.

   Options:
   - `:keys` - vector of keys to include; nested opts via map elements
              e.g. {:keys [:amount {:currency {:keys [:id :numeric]}}]}")

  (^{:tag String :added "2.1.0" :ex/soft true}
   to-json-string
   [this] [this opts]
   "Serializes the value to a canonical JSON string."))

(defprotocol ^{:added "2.1.0"} JsonDeserializable
  "Protocol for JSON deserialization of Bankster types.

  Options map supports:
  - `:registry`       - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode`  - rounding mode for rescaling (RoundingMode, keyword, or string)
  - `:rescale`        - integer scale to use instead of currency's nominal scale.
                        When provided, the Currency object is cloned with this scale
                        before creating Money. This prevents data loss when the registry
                        currency has a smaller scale than the incoming data."

  (^{:added "2.1.0" :ex/soft true}
   from-json-map
   [type-token m] [type-token m opts]
   "Deserializes a value from a JSON-friendly map.")

  (^{:added "2.1.0" :ex/soft true}
   from-json-string
   [type-token s] [type-token s opts]
   "Deserializes a value from a JSON string."))

;;
;; Shared helpers.
;;

(def ^{:tag clojure.lang.Keyword :const true :private true :added "2.1.0"}
  default-representation
  :map)

(defn- normalize-representation
  {:private true :tag clojure.lang.Keyword :added "2.1.0"}
  [rep]
  (let [rep (or rep default-representation)]
    (if (or (= rep :map) (= rep :string))
      rep
      (throw
       (ex-info
        "Invalid JSON representation; expected :map or :string."
        {:op             :bankster.serializers.json/normalize-representation
         :representation rep})))))

(defn- mget
  "Fetches a value from map `m` by keyword `k` accepting both keyword and string keys."
  {:private true :added "2.1.0"}
  [m k]
  (or (get m k)
      (get m (name k))))

(defn- parse-bigdec
  "Parses a value to BigDecimal. Delegates to `common/parse-bigdec`."
  {:private true :tag BigDecimal :added "2.1.0"}
  ^BigDecimal [x]
  (common/parse-bigdec x :bankster.serializers.json/parse-bigdec))

(defn- money-map->parts
  {:private true :added "2.1.0"}
  [m]
  (when (some? m)
    (let [cur (or (mget m :currency) (mget m :cur))
          amt (mget m :amount)
          rm  (or (mget m :rounding-mode) (mget m :rounding))]
      {:currency cur :amount amt :rounding-mode rm})))

;;
;; Aliases to common helpers.
;;

(def ^{:private true :tag Money :added "2.1.0"} make-money
  "Creates Money from currency and amount. Delegates to `common/make-money`."
  common/make-money)

(def ^{:private true :tag BigDecimal :added "2.1.0"} rescale-amount
  "Rescales a BigDecimal amount. Delegates to `common/rescale-amount`."
  common/rescale-amount)

(def ^{:private true :added "2.1.0"} extract-keys-spec
  "Extracts nested opts from :keys vector. Delegates to `common/extract-keys-spec`."
  common/extract-keys-spec)

(def ^{:private true :added "2.1.0"} filter-map-by-keys
  "Filters map by keys spec. Delegates to `common/filter-map-by-keys`."
  common/filter-map-by-keys)

;;
;; Currency JSON serialization helpers.
;;

(defn- currency-id->json-string
  "Converts currency ID keyword to JSON string representation.

  When `:code-only?` is truthy, namespace is omitted."
  {:private true :tag String :added "2.1.0"}
  ^String [^clojure.lang.Keyword id code-only?]
  (if code-only?
    (name id)
    (if-some [ns (namespace id)]
      (str ns "/" (name id))
      (name id))))

(defn currency->json-string
  "Converts Currency to a JSON string identifier.

  Options:
  - `:code-only?` - when truthy, namespace is omitted: `:crypto/ETH` → `\"ETH\"`"
  {:tag String :added "2.1.0"}
  (^String [^Currency c]
   (currency->json-string c nil))
  (^String [^Currency c opts]
   (when (some? c)
     (currency-id->json-string (.id ^Currency c) (:code-only? opts)))))

(defn currency->json-full-map
  "Converts Currency to a full JSON-friendly map with all fields.

  Canonical shape:
  - `:id`      - currency identifier string (e.g. \"PLN\" or \"crypto/ETH\")
  - `:numeric` - ISO numeric code (or -1 if absent)
  - `:scale`   - decimal places (or -1 for auto-scaled)
  - `:kind`    - kind string (e.g. \"FIAT\", \"DECENTRALIZED\")
  - `:domain`  - domain string (e.g. \"ISO-4217\", \"CRYPTO\")

  Options:
  - `:code-only?` - when truthy, namespace is omitted in `:id`
  - `:keys`       - vector of keys to include (filters output)"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Currency c]
   (currency->json-full-map c nil))
  (^clojure.lang.IPersistentMap [^Currency c opts]
   (when (some? c)
     (let [full-map  {:id      (currency-id->json-string (.id ^Currency c) (:code-only? opts))
                      :numeric (.numeric ^Currency c)
                      :scale   (.scale ^Currency c)
                      :kind    (some-> (.kind ^Currency c) (as-> k (if-some [ns (namespace k)]
                                                                     (str ns "/" (name k))
                                                                     (name k))))
                      :domain  (some-> (.domain ^Currency c) name)}
           keys-spec (extract-keys-spec (:keys opts))]
       (if keys-spec
         (filter-map-by-keys full-map keys-spec currency->json-full-map)
         full-map)))))

(defn currency->json-map
  "Converts Currency to a minimal JSON-friendly map.

  Minimal shape (by default):
  - `:id` - currency identifier string

  When `:full?` is truthy, delegates to `currency->json-full-map`.

  Options:
  - `:code-only?` - when truthy, namespace is omitted in `:id`
  - `:full?`      - when truthy, returns full map via `currency->json-full-map`
  - `:keys`       - (only with `:full?`) vector of keys to include"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Currency c]
   (currency->json-map c nil))
  (^clojure.lang.IPersistentMap [^Currency c opts]
   (when (some? c)
     (if (:full? opts)
       (currency->json-full-map c opts)
       {:id (currency-id->json-string (.id ^Currency c) (:code-only? opts))}))))

(defn json-string->currency
  "Builds Currency from a JSON string identifier.

  Options:
  - `:registry` - registry to use for lookup (default: `registry/get`)"
  {:tag Currency :added "2.1.0" :ex/strict true}
  (^Currency [s]
   (json-string->currency s nil))
  (^Currency [s opts]
   (when (some? s)
     (let [^Registry reg (or (:registry opts) (registry/get))]
       (currency/unit s reg)))))

(defn json-map->currency
  "Builds Currency from a JSON-friendly map.

  Accepts both keyword and string keys. Uses `:id` for lookup.

  Options:
  - `:registry` - registry to use for lookup (default: `registry/get`)"
  {:tag Currency :added "2.1.0" :ex/strict true}
  (^Currency [m]
   (json-map->currency m nil))
  (^Currency [m opts]
   (when (some? m)
     (when-not (map? m)
       (throw
        (ex-info
         "Currency JSON representation must be a map."
         {:op    :bankster.serializers.json/json-map->currency
          :value m
          :class (class m)})))
     (let [id (mget m :id)]
       (when-not (some? id)
         (throw
          (ex-info
           "Currency JSON map is missing :id."
           {:op    :bankster.serializers.json/json-map->currency
            :value m})))
       (json-string->currency id opts)))))

;;
;; Money <-> JSON map.
;;

(defn money->json-full-map
  "Converts Money to a full JSON-friendly map with all fields.

  Full shape:
  - `:currency` - full currency map (via `currency->json-full-map`)
  - `:amount`   - amount as string
  - Plus any extended fields from the Money record

  Options:
  - `:code-only?`    - when truthy, namespace is omitted in currency `:id`
  - `:keys`          - vector of keys to include; supports nested opts for `:currency`
                       e.g. {:keys [:amount {:currency {:keys [:id :numeric]}}]}
  - `:rescale`       - integer scale to rescale amounts to before serialization
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Money money]
   (money->json-full-map money nil))
  (^clojure.lang.IPersistentMap [^Money money opts]
   (when (some? money)
     (let [keys-vec        (:keys opts)
           keys-spec       (extract-keys-spec keys-vec)
           code-only?      (:code-only? opts)
           rescale         (:rescale opts)
           rounding-mode   (:rounding-mode opts)
           ;; Get nested currency opts from :keys spec
           cur-opts        (when keys-spec (get keys-spec :currency))
           ;; Build currency opts: merge code-only? + nested opts
           cur-opts        (cond-> (or cur-opts {})
                             code-only? (assoc :code-only? true))
           ;; Base full map with currency as full map
           ^Currency cur   (.currency ^Money money)
           ^BigDecimal amt (rescale-amount (.amount ^Money money) rescale rounding-mode)
           base-map        {:currency (currency->json-full-map cur cur-opts)
                            :amount   (.toPlainString amt)}
           ;; Add extended fields (from __extmap)
           ext-fields      (dissoc (into {} money) :currency :amount)
           full-map        (reduce-kv
                            (fn [m k v]
                              (assoc m k (cond
                                           (instance? BigDecimal v) (.toPlainString ^BigDecimal v)
                                           (keyword? v)             (name v)
                                           :else                    (str v))))
                            base-map
                            ext-fields)]
       (if keys-spec
         ;; Filter by keys, with nested serialization for currency
         (reduce-kv
          (fn [acc k nested-opts]
            (if-some [v (get full-map k)]
              (if (and (some? nested-opts) (= k :currency))
                ;; For currency, re-serialize with nested opts
                (assoc acc k (currency->json-full-map cur (merge cur-opts nested-opts)))
                (assoc acc k v))
              acc))
          {}
          keys-spec)
         full-map)))))

(defn money->json-map
  "Converts Money to a minimal JSON-friendly map.

  Minimal shape:
  - `:currency` - currency identifier string (e.g. \"PLN\" or \"crypto/USDT\")
  - `:amount`   - amount as a string (plain BigDecimal, scale preserved)

  When `:full?` is truthy, delegates to `money->json-full-map`.

  Options:
  - `:code-only?`    - when truthy, namespace is omitted: `:crypto/ETH` → `\"ETH\"`
  - `:full?`         - when truthy, returns full map via `money->json-full-map`
  - `:keys`          - (only with `:full?`) vector of keys to include
  - `:rescale`       - integer scale to rescale amounts to before serialization
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Money money]
   (money->json-map money nil))
  (^clojure.lang.IPersistentMap [^Money money opts]
   (when (some? money)
     (if (:full? opts)
       (money->json-full-map money opts)
       (let [^BigDecimal amt (rescale-amount (.amount ^Money money)
                                             (:rescale opts)
                                             (:rounding-mode opts))]
         {:currency (currency-id->json-string (.id ^Currency (.currency ^Money money))
                                              (:code-only? opts))
          :amount   (.toPlainString amt)})))))

(defn json-map->money
  "Builds Money from a JSON-friendly map.

  Accepts both keyword and string keys.

  Options:
  - `:registry`      - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)
  - `:rescale`       - integer scale to use instead of currency's nominal scale.
                       When provided, the Currency is cloned with this scale before
                       creating Money. This prevents data loss when the registry
                       currency has a smaller scale than the incoming data.
                       Downscaling requires `:rounding-mode` or `scale/*rounding-mode*`."
  {:tag Money :added "2.1.0" :ex/strict true}
  (^Money [m]
   (json-map->money m nil))
  (^Money [m opts]
   (when (some? m)
     (when-not (map? m)
       (throw
        (ex-info
         "Money JSON representation must be a map."
         {:op    :bankster.serializers.json/json-map->money
          :value m
          :class (class m)})))
     (let [{:keys [currency amount rounding-mode]} (money-map->parts m)
           ^Registry reg                           (or (:registry opts) (registry/get))
           rm                                      (or (:rounding-mode opts) rounding-mode)
           rescale                                 (:rescale opts)]
       (when-not (some? currency)
         (throw
          (ex-info
           "Money JSON map is missing :currency."
           {:op    :bankster.serializers.json/json-map->money
            :value m})))
       (when-not (some? amount)
         (throw
          (ex-info
           "Money JSON map is missing :amount."
           {:op    :bankster.serializers.json/json-map->money
            :value m})))
       (make-money currency (parse-bigdec amount) reg rm rescale)))))

;;
;; Money <-> JSON string.
;;

(defn money->json-string
  "Converts Money to a canonical JSON string.

  Canonical form: `<amount> <currency-id-str>` (e.g. \"12.30 PLN\").
  Locale-independent.

  Options:
  - `:code-only?`    - when truthy, namespace is omitted: `:crypto/ETH` → `\"ETH\"`
  - `:rescale`       - integer scale to rescale amounts to before serialization
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)"
  {:tag String :added "2.1.0"}
  (^String [^Money money]
   (money->json-string money nil))
  (^String [^Money money opts]
   (when (some? money)
     (let [^BigDecimal amt (rescale-amount (.amount ^Money money)
                                           (:rescale opts)
                                           (:rounding-mode opts))]
       (str (.toPlainString amt)
            " "
            (currency-id->json-string (.id ^Currency (.currency ^Money money))
                                      (:code-only? opts)))))))

(defn- numericish?
  {:private true :tag Boolean :added "2.1.0"}
  [^String s]
  (boolean (re-matches #"[+-]?(?:\d+(?:\.\d*)?|\.\d+)" (str/replace s "_" ""))))

(defn- split-money-string
  {:private true :added "2.1.0"}
  [^String s]
  (let [s (-> s str/trim (str/replace "_" ""))]
    (when-not (seq s)
      (throw
       (ex-info
        "Empty money string."
        {:op    :bankster.serializers.json/split-money-string
         :value s})))
    (let [parts (->> (str/split s #"\s+")
                     (remove str/blank?)
                     (vec))
          cnt   (count parts)]
      (case cnt
        2
        (let [a (nth parts 0)
              b (nth parts 1)]
          (if (numericish? a)
            {:amount a :currency b}
            (if (numericish? b)
              {:amount b :currency a}
              (throw
               (ex-info
                "Money string must contain exactly one numeric token."
                {:op    :bankster.serializers.json/split-money-string
                 :value s
                 :parts parts})))))

        ;; No whitespace: allow currency-first (e.g. \"PLN12.30\") or amount-first (\"12.30PLN\").
        1
        (let [^String token (nth parts 0)
              idx           (let [len (long (.length token))]
                              (cond
                                (or (= (.charAt token 0) \-)
                                    (= (.charAt token 0) \+))
                                0

                                :else
                                (loop [i 0]
                                  (if (>= i len)
                                    nil
                                    (let [ch (.charAt token i)]
                                      (if (or (Character/isDigit ch)
                                              (= ch \.))
                                        i
                                        (recur (inc i))))))))]
          (when-not (some? idx)
            (throw
             (ex-info
              "Money string without whitespace must contain digits."
              {:op    :bankster.serializers.json/split-money-string
               :value token})))
          (let [pfx (subs token 0 idx)
                sfx (subs token idx)]
            (cond
              ;; amount-first: "12.30PLN" / "-12.30PLN"
              (zero? idx)
              (let [^String t token
                    len       (long (.length t))
                    split     (loop [i 0]
                                (if (>= i len)
                                  nil
                                  (let [ch (.charAt t i)]
                                    (if (or (Character/isDigit ch)
                                            (= ch \.)
                                            (and (zero? i) (or (= ch \-) (= ch \+))))
                                      (recur (inc i))
                                      i))))]
                (when-not (some? split)
                  (throw
                   (ex-info
                    "Cannot split money string into currency and amount."
                    {:op    :bankster.serializers.json/split-money-string
                     :value token})))
                (let [a (str/trim (subs t 0 (int split)))
                      c (str/trim (subs t (int split)))]
                  (if (and (numericish? a) (seq c))
                    {:amount a :currency c}
                    (throw
                     (ex-info
                      "Cannot split money string into currency and amount."
                      {:op    :bankster.serializers.json/split-money-string
                       :value token})))))

              ;; currency-first: "PLN12.30"
              (numericish? sfx)
              {:currency (str/trim pfx) :amount (str/trim sfx)}

              :else
              (throw
               (ex-info
                "Cannot split money string into currency and amount."
                {:op    :bankster.serializers.json/split-money-string
                 :value token})))))

        (throw
         (ex-info
          "Money string must contain exactly two tokens."
          {:op    :bankster.serializers.json/split-money-string
           :value s
           :parts parts}))))))

(defn json-string->money
  "Builds Money from a JSON string.

  Accepts:
  - amount-first: \"12.30 PLN\"
  - currency-first: \"PLN 12.30\"
  - no-whitespace: \"PLN12.30\" (best-effort)

  Options:
  - `:registry`      - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)
  - `:rescale`       - integer scale to use instead of currency's nominal scale.
                       When provided, the Currency is cloned with this scale before
                       creating Money. This prevents data loss when the registry
                       currency has a smaller scale than the incoming data.

  For scale-sensitive workflows prefer map representation."
  {:tag Money :added "2.1.0" :ex/strict true}
  (^Money [s]
   (json-string->money s nil))
  (^Money [s opts]
   (when (some? s)
     (when-not (string? s)
       (throw
        (ex-info
         "Money JSON representation must be a string."
         {:op    :bankster.serializers.json/json-string->money
          :value s
          :class (class s)})))
     (let [{:keys [currency amount]} (split-money-string s)
           ^Registry reg             (or (:registry opts) (registry/get))
           rm                        (:rounding-mode opts)
           rescale                   (:rescale opts)]
       (make-money currency (parse-bigdec amount) reg rm rescale)))))

;;
;; JSON text (parsing + decoding).
;;

(defn- parse-json-text
  {:private true :added "2.1.0"}
  [^String s opts]
  (if-let [parse-fn (:parse-fn opts)]
    (parse-fn s)
    (let [parse-string (try
                         (requiring-resolve 'cheshire.core/parse-string)
                         (catch Throwable _ nil))]
      (when-not parse-string
        (throw
         (ex-info
          "No JSON parser available."
          {:op   :bankster.serializers.json/parse-json-text
           :hint "Add cheshire/cheshire to deps.edn or pass :parse-fn in opts."})))
      (let [key-fn         (:key-fn opts)
            bigdec?        (if (contains? opts :bigdecimals?) (:bigdecimals? opts) true)
            use-bigdec-var (try
                             (requiring-resolve 'cheshire.parse/*use-bigdecimals?*)
                             (catch Throwable _ nil))
            parse-call     (fn []
                             (if (some? key-fn)
                               (parse-string s key-fn)
                               (parse-string s)))]
        (if (var? use-bigdec-var)
          (with-bindings {use-bigdec-var bigdec?}
            (parse-call))
          (parse-call))))))

(defn json-text->money
  "Parses JSON text and deserializes Money.

  Expects JSON object (map) or JSON string representation.

  Parser selection:
  - If `:parse-fn` is provided, it is used to parse the string into a Clojure value.
  - Otherwise, Cheshire is used when available.

  Options:
  - `:representation` - `:auto` (default), `:map`, or `:string`
  - `:parse-fn`       - custom parser fn (String -> Clojure value)
  - `:key-fn`         - Cheshire key-fn (e.g. `keyword`, `identity`, nil)
  - `:bigdecimals?`   - when using Cheshire, bind `cheshire.parse/*use-bigdecimals?*`
                        (default true)
  - `:registry`       - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode`  - rounding mode for rescaling
  - `:rescale`        - integer scale override"
  {:tag Money :added "2.1.0" :ex/strict true}
  (^Money [s]
   (json-text->money s nil))
  (^Money [s opts]
   (when (some? s)
     (when-not (string? s)
       (throw
        (ex-info
         "Money JSON text must be a string."
         {:op    :bankster.serializers.json/json-text->money
          :value s
          :class (class s)})))
     (let [rep (or (:representation opts) :auto)
           v   (parse-json-text s opts)]
       (case rep
         :auto   (cond
                   (map? v)    (json-map->money v opts)
                   (string? v) (json-string->money v opts)
                   :else
                   (throw
                    (ex-info
                     "Unsupported JSON value for Money."
                     {:op    :bankster.serializers.json/json-text->money
                      :value v
                      :class (class v)})))
         :map    (if (map? v)
                   (json-map->money v opts)
                   (throw
                    (ex-info
                     "JSON value is not a map."
                     {:op    :bankster.serializers.json/json-text->money
                      :value v
                      :class (class v)})))
         :string (if (string? v)
                   (json-string->money v opts)
                   (throw
                    (ex-info
                     "JSON value is not a string."
                     {:op    :bankster.serializers.json/json-text->money
                      :value v
                      :class (class v)})))
         (throw
          (ex-info
           "Invalid JSON representation; expected :auto, :map or :string."
           {:op             :bankster.serializers.json/json-text->money
            :representation rep})))))))

(defn json-text->currency
  "Parses JSON text and deserializes Currency.

  Expects JSON object (map) or JSON string representation.

  Options:
  - `:representation` - `:auto` (default), `:map`, or `:string`
  - `:parse-fn`       - custom parser fn (String -> Clojure value)
  - `:key-fn`         - Cheshire key-fn
  - `:bigdecimals?`   - when using Cheshire, bind `cheshire.parse/*use-bigdecimals?*`
                        (default true)
  - `:registry`       - registry to use for currency lookup (default: `registry/get`)"
  {:tag Currency :added "2.1.0"}
  (^Currency [s]
   (json-text->currency s nil))
  (^Currency [s opts]
   (when (some? s)
     (when-not (string? s)
       (throw
        (ex-info
         "Currency JSON text must be a string."
         {:op    :bankster.serializers.json/json-text->currency
          :value s
          :class (class s)})))
     (let [rep (or (:representation opts) :auto)
           v   (parse-json-text s opts)]
       (case rep
         :auto   (cond
                   (map? v)    (json-map->currency v opts)
                   (string? v) (json-string->currency v opts)
                   :else
                   (throw
                    (ex-info
                     "Unsupported JSON value for Currency."
                     {:op    :bankster.serializers.json/json-text->currency
                      :value v
                      :class (class v)})))
         :map    (if (map? v)
                   (json-map->currency v opts)
                   (throw
                    (ex-info
                     "JSON value is not a map."
                     {:op    :bankster.serializers.json/json-text->currency
                      :value v
                      :class (class v)})))
         :string (if (string? v)
                   (json-string->currency v opts)
                   (throw
                    (ex-info
                     "JSON value is not a string."
                     {:op    :bankster.serializers.json/json-text->currency
                      :value v
                      :class (class v)})))
         (throw
          (ex-info
           "Invalid JSON representation; expected :auto, :map or :string."
           {:op             :bankster.serializers.json/json-text->currency
            :representation rep})))))))

;;
;; Protocol implementations.
;;

(extend-protocol JsonSerializable

  Money

  (to-json-map
    (^clojure.lang.IPersistentMap [m]
     (money->json-map m nil))
    (^clojure.lang.IPersistentMap [m opts]
     (money->json-map m opts)))

  (to-json-full-map
    (^clojure.lang.IPersistentMap [m]
     (money->json-full-map m nil))
    (^clojure.lang.IPersistentMap [m opts]
     (money->json-full-map m opts)))

  (to-json-string
    (^String [m]
     (money->json-string m nil))
    (^String [m opts]
     (money->json-string m opts)))

  Currency

  (to-json-map
    (^clojure.lang.IPersistentMap [c]
     (currency->json-map c nil))
    (^clojure.lang.IPersistentMap [c opts]
     (currency->json-map c opts)))

  (to-json-full-map
    (^clojure.lang.IPersistentMap [c]
     (currency->json-full-map c nil))
    (^clojure.lang.IPersistentMap [c opts]
     (currency->json-full-map c opts)))

  (to-json-string
    (^String [c]
     (currency->json-string c nil))
    (^String [c opts]
     (currency->json-string c opts))))

(extend-protocol JsonDeserializable

  Class

  (from-json-map
    ([cls m]
     (from-json-map cls m nil))
    ([cls m opts]
     (cond
       (identical? cls Money)
       (json-map->money m opts)

       (identical? cls Currency)
       (json-map->currency m opts)

       :else
       (throw
        (ex-info
         "Unsupported type token for from-json-map."
         {:op    :bankster.serializers.json/from-json-map
          :type  cls
          :value m})))))

  (from-json-string
    ([cls s]
     (from-json-string cls s nil))
    ([cls s opts]
     (cond
       (identical? cls Money)
       (json-string->money s opts)

       (identical? cls Currency)
       (json-string->currency s opts)

       :else
       (throw
        (ex-info
         "Unsupported type token for from-json-string."
         {:op    :bankster.serializers.json/from-json-string
          :type  cls
          :value s}))))))

;;
;; Generic codec + integrations.
;;

(defn money-codec
  "Returns a representation-aware codec map:

  - `:encode`          - (Money -> json-value) where json-value is map or string
  - `:decode`          - (json-value -> Money)
  - `:representation`  - `:map` or `:string`

  Options:
  - `:representation` - `:map` (default) or `:string`
  - `:code-only?`     - passed to encoder
  - `:registry`       - passed to decoder
  - `:rounding-mode`  - passed to decoder"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  ([]
   (money-codec {}))
  ([{:keys [representation code-only? registry rounding-mode] :as opts}]
   (let [rep      (normalize-representation representation)
         enc-opts (when code-only? {:code-only? code-only?})
         dec-opts (cond-> nil
                    registry      (assoc :registry registry)
                    rounding-mode (assoc :rounding-mode rounding-mode))]
     {:representation rep
      :encode         (if (= rep :map)
                        (if enc-opts
                          #(money->json-map % enc-opts)
                          money->json-map)
                        (if enc-opts
                          #(money->json-string % enc-opts)
                          money->json-string))
      :decode         (if (= rep :map)
                        (if dec-opts
                          #(json-map->money % dec-opts)
                          json-map->money)
                        (if dec-opts
                          #(json-string->money % dec-opts)
                          json-string->money))})))

(defn register-cheshire-codecs!
  "Registers Cheshire encoders for `io.randomseed.bankster.Money`.

  Notes:
  - This function does not add any dependencies. If Cheshire is not on the classpath
    it throws.
  - Cheshire registration affects encoding; decoding remains explicit via
    `json-map->money` / `json-string->money`.
  - `:representation` selects whether Money is encoded as an object (`:map`) or a
    JSON string (`:string`).

  Options:
  - `:representation` - `:map` (default) or `:string`
  - `:code-only?`     - when truthy, namespace is omitted in currency identifiers
  - `:add-encoder`    - custom add-encoder fn (for testing)"
  {:added "2.1.0"}
  ([]
   (register-cheshire-codecs! {}))
  ([{:keys [representation code-only? add-encoder] :as opts}]
   (let [rep         (normalize-representation representation)
         resolved-add-var (try
                            (requiring-resolve 'cheshire.generate/add-encoder)
                            (catch Throwable _ nil))
         resolved-add-fn  (when resolved-add-var (var-get resolved-add-var))
         add-encoder      (or add-encoder resolved-add-fn)
         use-cheshire?    (and resolved-add-fn (identical? add-encoder resolved-add-fn))
         encode-map       (when use-cheshire?
                            (some-> (try
                                      (requiring-resolve 'cheshire.generate/encode-map)
                                      (catch Throwable _ nil))
                                    var-get))
         encode-str       (when use-cheshire?
                            (some-> (try
                                      (requiring-resolve 'cheshire.generate/encode-str)
                                      (catch Throwable _ nil))
                                    var-get))
         enc-opts    (when code-only? {:code-only? code-only?})]
     (when-not add-encoder
       (throw
        (ex-info
         "Cheshire is not available on the classpath."
         {:op   :bankster.serializers.json/register-cheshire-codecs!
          :opts opts
          :hint "Add cheshire/cheshire to deps.edn or use map/string helpers directly."})))
     (when (and use-cheshire? (not (and encode-map encode-str)))
       (throw
        (ex-info
         "Cheshire encoder helpers are not available on the classpath."
         {:op   :bankster.serializers.json/register-cheshire-codecs!
          :opts opts
          :hint "Add cheshire/cheshire to deps.edn or use map/string helpers directly."})))
     (add-encoder
      Money
      (if (= rep :map)
        (if use-cheshire?
          (fn [^Money m jg]
            (let [{:keys [currency amount]} (money->json-map m enc-opts)]
              (encode-map {"currency" currency "amount" amount} jg)))
          (fn [^Money m jg]
            (let [{:keys [currency amount]} (money->json-map m enc-opts)]
              (clojure.lang.Reflector/invokeInstanceMethod jg "writeStartObject" (object-array 0))
              (clojure.lang.Reflector/invokeInstanceMethod jg "writeStringField"
                                                           (object-array ["currency" currency]))
              (clojure.lang.Reflector/invokeInstanceMethod jg "writeStringField"
                                                           (object-array ["amount" amount]))
              (clojure.lang.Reflector/invokeInstanceMethod jg "writeEndObject" (object-array 0)))))
        (if use-cheshire?
          (fn [^Money m jg]
            (encode-str (money->json-string m enc-opts) jg))
          (fn [^Money m jg]
            (clojure.lang.Reflector/invokeInstanceMethod jg "writeString"
                                                         (object-array [(money->json-string m enc-opts)]))))))
     true)))
