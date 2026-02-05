;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Bankster library, EDN-oriented serialization helpers."
      :author "Paweł Wilk"
      :added  "2.1.0"}

    io.randomseed.bankster.serializers.edn

  (:require [clojure.edn                               :as      edn]
            [clojure.string                            :as      str]
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

(defprotocol ^{:added "2.1.0"} EdnSerializable
  "Protocol for EDN serialization of Bankster types.

  Options map supports:
  - `:code-only?` - when truthy, namespace is omitted: `:crypto/ETH` → `:ETH`
  - `:full?`      - when truthy, delegates to `to-edn-full-map` for full serialization
  - `:keys`       - vector of keys to include; supports nested opts via map elements
  - `:rescale`    - integer scale to rescale amounts to (requires `:rounding-mode` for downscaling)"

  (^{:tag clojure.lang.IPersistentMap :added "2.1.0" :ex/soft true}
   to-edn-map
   [this] [this opts]
   "Serializes the value to a minimal EDN-friendly map.
   For Money: {:currency :PLN, :amount 12.30M}
   For Currency: {:id :PLN}")

  (^{:tag clojure.lang.IPersistentMap :added "2.1.0" :ex/soft true}
   to-edn-full-map
   [this] [this opts]
   "Serializes the value to a full EDN-friendly map with all available fields.
   For Currency: {:id, :numeric, :scale, :kind, :domain}
   For Money: {:currency {...}, :amount} plus any extended fields.

   Options:
   - `:keys` - vector of keys to include; nested opts via map elements
              e.g. {:keys [:amount {:currency {:keys [:id :numeric]}}]}")

  (^{:tag String :added "2.1.0" :ex/soft true}
   to-edn-string
   [this] [this opts]
   "Serializes the value to an EDN string (tagged literal format)."))

(defprotocol ^{:added "2.1.0"} EdnDeserializable
  "Protocol for EDN deserialization of Bankster types.

  Options map supports:
  - `:registry`       - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode`  - rounding mode for rescaling (RoundingMode, keyword, or string)
  - `:rescale`        - integer scale to use instead of currency's nominal scale.
                        When provided, the Currency object is cloned with this scale
                        before creating Money. This prevents data loss when the registry
                        currency has a smaller scale than the incoming data."

  (^{:added "2.1.0" :ex/soft true}
   from-edn-map
   [type-token m] [type-token m opts]
   "Deserializes a value from an EDN map.")

  (^{:added "2.1.0" :ex/soft true}
   from-edn-string
   [type-token s] [type-token s opts]
   "Deserializes a value from an EDN string (tagged literal format)."))

;;
;; Aliases to common helpers.
;;

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
;; Shared helpers.
;;

(defn- mget
  "Fetches a value from map `m` by keyword `k`."
  {:private true :added "2.1.0"}
  [m k]
  (get m k))

(defn- parse-bigdec
  "Parses a value to BigDecimal. Delegates to `common/parse-bigdec`."
  {:private true :tag BigDecimal :added "2.1.0"}
  ^BigDecimal [x]
  (common/parse-bigdec x :bankster.serializers.edn/parse-bigdec))

(defn- edn-map->parts
  {:private true :added "2.1.0"}
  [m]
  (when (some? m)
    (let [cur (or (mget m :currency) (mget m :cur))
          amt (mget m :amount)
          rm  (or (mget m :rounding-mode) (mget m :rounding))]
      {:currency cur :amount amt :rounding-mode rm})))

(def ^{:private true :tag Money :added "2.1.0"} make-money
  "Creates Money from currency and amount. Delegates to `common/make-money`."
  common/make-money)

;;
;; Currency EDN serialization helpers.
;;

(defn- currency-id->edn-keyword
  "Converts currency ID keyword to EDN keyword representation.

  When `:code-only?` is truthy, namespace is omitted."
  {:private true :tag clojure.lang.Keyword :added "2.1.0"}
  ^clojure.lang.Keyword [^clojure.lang.Keyword id code-only?]
  (if code-only?
    (keyword (name id))
    id))

(defn currency->edn-keyword
  "Converts Currency to an EDN keyword identifier.

  Options:
  - `:code-only?` - when truthy, namespace is omitted: `:crypto/ETH` → `:ETH`"
  {:tag clojure.lang.Keyword :added "2.1.0"}
  (^clojure.lang.Keyword [^Currency c]
   (currency->edn-keyword c nil))
  (^clojure.lang.Keyword [^Currency c opts]
   (when (some? c)
     (currency-id->edn-keyword (.id ^Currency c) (:code-only? opts)))))

(defn currency->edn-full-map
  "Converts Currency to a full EDN-friendly map with all fields.

  Full shape:
  - `:id`      - currency identifier keyword (may be namespaced)
  - `:numeric` - ISO numeric code (or -1 if absent)
  - `:scale`   - decimal places (or -1 for auto-scaled)
  - `:domain`  - domain keyword (e.g. `:ISO-4217`, `:CRYPTO`)
  - `:kind`    - kind keyword (e.g. `:FIAT`, `:DECENTRALIZED`)

  Options:
  - `:code-only?` - when truthy, namespace is omitted in `:id`
  - `:keys`       - vector of keys to include (filters output)"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Currency c]
   (currency->edn-full-map c nil))
  (^clojure.lang.IPersistentMap [^Currency c opts]
   (when (some? c)
     (let [full-map  {:id      (currency-id->edn-keyword (.id ^Currency c) (:code-only? opts))
                      :numeric (.numeric ^Currency c)
                      :scale   (.scale ^Currency c)
                      :domain  (.domain ^Currency c)
                      :kind    (.kind ^Currency c)}
           keys-spec (extract-keys-spec (:keys opts))]
       (if keys-spec
         (filter-map-by-keys full-map keys-spec currency->edn-full-map)
         full-map)))))

(defn currency->edn-map
  "Converts Currency to a minimal EDN-friendly map.

  Minimal shape (by default):
  - `:id` - currency identifier keyword

  When `:full?` is truthy, delegates to `currency->edn-full-map`.

  Options:
  - `:code-only?` - when truthy, namespace is omitted in `:id`
  - `:full?`      - when truthy, returns full map via `currency->edn-full-map`
  - `:keys`       - (only with `:full?`) vector of keys to include"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Currency c]
   (currency->edn-map c nil))
  (^clojure.lang.IPersistentMap [^Currency c opts]
   (when (some? c)
     (if (:full? opts)
       (currency->edn-full-map c opts)
       {:id (currency-id->edn-keyword (.id ^Currency c) (:code-only? opts))}))))

(defn currency->edn-string
  "Converts Currency to an EDN tagged literal string.

  Format: `#currency <keyword>` (e.g. `#currency :PLN` or `#currency :crypto/ETH`)

  Options:
  - `:code-only?` - when truthy, namespace is omitted: `:crypto/ETH` → `:ETH`"
  {:tag String :added "2.1.0"}
  (^String [^Currency c]
   (currency->edn-string c nil))
  (^String [^Currency c opts]
   (when (some? c)
     (let [kw (currency-id->edn-keyword (.id ^Currency c) (:code-only? opts))]
       (str "#currency " (pr-str kw))))))

(defn edn-keyword->currency
  "Builds Currency from an EDN keyword identifier.

  Options:
  - `:registry` - registry to use for lookup (default: `registry/get`)"
  {:tag Currency :added "2.1.0" :ex/strict true}
  (^Currency [k]
   (edn-keyword->currency k nil))
  (^Currency [k opts]
   (when (some? k)
     (let [^Registry reg (or (:registry opts) (registry/get))]
       (currency/unit k reg)))))

(defn edn-map->currency
  "Builds Currency from an EDN map.

  Uses `:id` for lookup.

  Options:
  - `:registry` - registry to use for lookup (default: `registry/get`)"
  {:tag Currency :added "2.1.0" :ex/strict true}
  (^Currency [m]
   (edn-map->currency m nil))
  (^Currency [m opts]
   (when (some? m)
     (when-not (map? m)
       (throw
        (ex-info
         "Currency EDN representation must be a map."
         {:op    :bankster.serializers.edn/edn-map->currency
          :value m
          :class (class m)})))
     (let [id (mget m :id)]
       (when-not (some? id)
         (throw
          (ex-info
           "Currency EDN map is missing :id."
           {:op    :bankster.serializers.edn/edn-map->currency
            :value m})))
       (edn-keyword->currency id opts)))))

(defn edn-string->currency
  "Builds Currency from an EDN tagged literal string.

  Options:
  - `:registry` - registry to use for lookup (default: `registry/get`)"
  {:tag Currency :added "2.1.0" :ex/strict true}
  (^Currency [s]
   (edn-string->currency s nil))
  (^Currency [s opts]
   (when (some? s)
     (when-not (string? s)
       (throw
        (ex-info
         "Currency EDN representation must be a string."
         {:op    :bankster.serializers.edn/edn-string->currency
          :value s
          :class (class s)})))
     (let [^Registry reg (or (:registry opts) (registry/get))
           readers       {'currency (fn [k] (currency/unit k reg))}]
       (edn/read-string {:readers readers} s)))))

;;
;; Money <-> EDN map.
;;

(defn money->edn-full-map
  "Converts Money to a full EDN-friendly map with all fields.

  Full shape:
  - `:currency` - full currency map (via `currency->edn-full-map`)
  - `:amount`   - BigDecimal amount
  - Plus any extended fields from the Money record

  Options:
  - `:code-only?`    - when truthy, namespace is omitted in currency `:id`
  - `:keys`          - vector of keys to include; supports nested opts for `:currency`
                       e.g. {:keys [:amount {:currency {:keys [:id :numeric]}}]}
  - `:rescale`       - integer scale to rescale amounts to before serialization
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Money money]
   (money->edn-full-map money nil))
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
           base-map        {:currency (currency->edn-full-map cur cur-opts)
                            :amount   amt}
           ;; Add extended fields (from __extmap)
           ext-fields      (dissoc (into {} money) :currency :amount)
           full-map        (merge base-map ext-fields)]
       (if keys-spec
         ;; Filter by keys, with nested serialization for currency
         (reduce-kv
          (fn [acc k nested-opts]
            (if-some [v (get full-map k)]
              (if (and (some? nested-opts) (= k :currency))
                ;; For currency, re-serialize with nested opts
                (assoc acc k (currency->edn-full-map cur (merge cur-opts nested-opts)))
                (assoc acc k v))
              acc))
          {}
          keys-spec)
         full-map)))))

(defn money->edn-map
  "Converts Money to a minimal EDN-friendly map.

  Minimal shape:
  - `:currency` - currency identifier keyword (e.g. `:PLN` or `:crypto/USDT`)
  - `:amount`   - BigDecimal amount (scale preserved)

  When `:full?` is truthy, delegates to `money->edn-full-map`.

  Options:
  - `:code-only?`    - when truthy, namespace is omitted: `:crypto/ETH` → `:ETH`
  - `:full?`         - when truthy, returns full map via `money->edn-full-map`
  - `:keys`          - (only with `:full?`) vector of keys to include
  - `:rescale`       - integer scale to rescale amounts to before serialization
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [^Money money]
   (money->edn-map money nil))
  (^clojure.lang.IPersistentMap [^Money money opts]
   (when (some? money)
     (if (:full? opts)
       (money->edn-full-map money opts)
       (let [^BigDecimal amt (rescale-amount (.amount ^Money money)
                                             (:rescale opts)
                                             (:rounding-mode opts))]
         {:currency (currency-id->edn-keyword (.id ^Currency (.currency ^Money money))
                                              (:code-only? opts))
          :amount   amt})))))

(defn edn-map->money
  "Builds Money from an EDN map.

  Options:
  - `:registry`      - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)
  - `:rescale`       - integer scale to use instead of currency's nominal scale.
                       When provided, the Currency is cloned with this scale before
                       creating Money. This prevents data loss when the registry
                       currency has a smaller scale than the incoming data."
  {:tag Money :added "2.1.0" :ex/strict true}
  (^Money [m]
   (edn-map->money m nil))
  (^Money [m opts]
   (when (some? m)
     (when-not (map? m)
       (throw
        (ex-info
         "Money EDN representation must be a map."
         {:op    :bankster.serializers.edn/edn-map->money
          :value m
          :class (class m)})))
     (let [{:keys [currency amount rounding-mode]} (edn-map->parts m)
           ^Registry reg                           (or (:registry opts) (registry/get))
           rm                                      (or (:rounding-mode opts) rounding-mode)
           rescale                                 (:rescale opts)]
       (when-not (some? currency)
         (throw
          (ex-info
           "Money EDN map is missing :currency."
           {:op    :bankster.serializers.edn/edn-map->money
            :value m})))
       (when-not (some? amount)
         (throw
          (ex-info
           "Money EDN map is missing :amount."
           {:op    :bankster.serializers.edn/edn-map->money
            :value m})))
       (make-money currency (parse-bigdec amount) reg rm rescale)))))

;;
;; Money <-> EDN string.
;;

(defn money->edn-string
  "Converts Money to an EDN tagged literal string.

  Format:
  - ISO currencies: `#money[12.30M PLN]`
  - Namespaced currencies: `#money/crypto[1.5M ETH]`

  Options:
  - `:code-only?`    - when truthy, always uses `#money[...]` format without namespace
  - `:rescale`       - integer scale to rescale amounts to before serialization
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)"
  {:tag String :added "2.1.0"}
  (^String [^Money money]
   (money->edn-string money nil))
  (^String [^Money money opts]
   (when (some? money)
     (let [^Currency cur   (.currency ^Money money)
           ^BigDecimal amt (rescale-amount (.amount ^Money money)
                                           (:rescale opts)
                                           (:rounding-mode opts))
           ns              (namespace (.id cur))
           code            (name (.id cur))
           amt-str         (str (.toPlainString amt) "M")]
       (if (and ns (not (:code-only? opts)))
         (str "#money/" ns "[" amt-str " " code "]")
         (str "#money[" amt-str " " code "]"))))))

(defn edn-string->money
  "Builds Money from an EDN tagged literal string.

  Accepts:
  - `#money[12.30M PLN]`
  - `#money/crypto[1.5M ETH]`
  - `#money{:currency :PLN :amount 12.30M}`

  Options:
  - `:registry`      - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode` - rounding mode for rescaling (RoundingMode, keyword, or string)
  - `:rescale`       - integer scale to use instead of currency's nominal scale"
  {:tag Money :added "2.1.0" :ex/strict true}
  (^Money [s]
   (edn-string->money s nil))
  (^Money [s opts]
   (when (some? s)
     (when-not (string? s)
       (throw
        (ex-info
         "Money EDN representation must be a string."
         {:op    :bankster.serializers.edn/edn-string->money
          :value s
          :class (class s)})))
     (let [^Registry reg (or (:registry opts) (registry/get))
           rm            (:rounding-mode opts)
           rescale       (:rescale opts)
           ;; Create readers that produce Money directly
           money-reader  (fn [arg]
                           (cond
                             (vector? arg)
                             (let [[amt cur]     arg
                                   ^Currency c   (currency/unit cur reg)
                                   ^BigDecimal a (if (instance? BigDecimal amt)
                                                   amt
                                                   (parse-bigdec amt))]
                               (make-money c a reg rm rescale))

                             (map? arg)
                             (edn-map->money arg opts)

                             :else
                             (throw
                              (ex-info
                               "Invalid money literal argument."
                               {:op    :bankster.serializers.edn/edn-string->money
                                :value arg}))))
           ;; Build readers map with namespace variants
           readers (into {'money money-reader}
                         (for [ns-name ["crypto" "CRYPTO" "iso" "ISO" "fiat" "FIAT"
                                        "commodity" "COMMODITY" "experimental" "EXPERIMENTAL"]]
                           [(symbol "money" ns-name)
                            (fn [arg]
                              (if (vector? arg)
                                (let [[amt cur] arg
                                      cur-kw    (keyword ns-name (if (keyword? cur) (name cur) (str cur)))]
                                  (money-reader [amt cur-kw]))
                                (money-reader arg)))]))]
       (edn/read-string {:readers readers} s)))))

;;
;; Protocol implementations.
;;

(extend-protocol EdnSerializable

  Money

  (to-edn-map
    (^clojure.lang.IPersistentMap [m]
     (money->edn-map m nil))
    (^clojure.lang.IPersistentMap [m opts]
     (money->edn-map m opts)))

  (to-edn-full-map
    (^clojure.lang.IPersistentMap [m]
     (money->edn-full-map m nil))
    (^clojure.lang.IPersistentMap [m opts]
     (money->edn-full-map m opts)))

  (to-edn-string
    (^String [m]
     (money->edn-string m nil))
    (^String [m opts]
     (money->edn-string m opts)))

  Currency

  (to-edn-map
    (^clojure.lang.IPersistentMap [c]
     (currency->edn-map c nil))
    (^clojure.lang.IPersistentMap [c opts]
     (currency->edn-map c opts)))

  (to-edn-full-map
    (^clojure.lang.IPersistentMap [c]
     (currency->edn-full-map c nil))
    (^clojure.lang.IPersistentMap [c opts]
     (currency->edn-full-map c opts)))

  (to-edn-string
    (^String [c]
     (currency->edn-string c nil))
    (^String [c opts]
     (currency->edn-string c opts))))

(extend-protocol EdnDeserializable

  Class

  (from-edn-map
    ([cls m]
     (from-edn-map cls m nil))
    ([cls m opts]
     (cond
       (identical? cls Money)
       (edn-map->money m opts)

       (identical? cls Currency)
       (edn-map->currency m opts)

       :else
       (throw
        (ex-info
         "Unsupported type token for from-edn-map."
         {:op    :bankster.serializers.edn/from-edn-map
          :type  cls
          :value m})))))

  (from-edn-string
    ([cls s]
     (from-edn-string cls s nil))
    ([cls s opts]
     (cond
       (identical? cls Money)
       (edn-string->money s opts)

       (identical? cls Currency)
       (edn-string->currency s opts)

       :else
       (throw
        (ex-info
         "Unsupported type token for from-edn-string."
         {:op    :bankster.serializers.edn/from-edn-string
          :type  cls
          :value s}))))))

;;
;; Convenience codec.
;;

(defn money-codec
  "Returns a representation-aware codec map for EDN:

  - `:encode`          - (Money -> edn-value) where edn-value is map or string
  - `:decode`          - (edn-value -> Money)
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
   (let [rep      (or representation :map)
         _        (when-not (#{:map :string} rep)
                    (throw
                     (ex-info
                      "Invalid EDN representation; expected :map or :string."
                      {:op             :bankster.serializers.edn/money-codec
                       :representation rep})))
         enc-opts (when code-only? {:code-only? code-only?})
         dec-opts (cond-> nil
                    registry      (assoc :registry registry)
                    rounding-mode (assoc :rounding-mode rounding-mode))]
     {:representation rep
      :encode         (if (= rep :map)
                        (if enc-opts
                          #(money->edn-map % enc-opts)
                          money->edn-map)
                        (if enc-opts
                          #(money->edn-string % enc-opts)
                          money->edn-string))
      :decode         (if (= rep :map)
                        (if dec-opts
                          #(edn-map->money % dec-opts)
                          edn-map->money)
                        (if dec-opts
                          #(edn-string->money % dec-opts)
                          edn-string->money))})))
