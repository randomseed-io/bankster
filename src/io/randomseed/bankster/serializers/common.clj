(ns

    ^{:doc    "Bankster library, shared serialization helpers."
      :author "Paweł Wilk"
      :added  "2.1.0"}

    io.randomseed.bankster.serializers.common

  (:require [clojure.string                  :as      str]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.scale    :as    scale])

  (:import  (io.randomseed.bankster Currency
                                    Registry
                                    Money)
            (java.math              BigDecimal
                                    RoundingMode)))

;;
;; BigDecimal parsing.
;;

(defn parse-bigdec
  "Parses a value to BigDecimal.

  Accepts:
  - nil -> nil
  - BigDecimal -> as-is
  - integer -> BigDecimal/valueOf
  - number -> BigDecimal/valueOf (note: doubles lose precision)
  - string -> parsed via `bigdec` (trims whitespace, removes underscores)

  Throws ExceptionInfo on invalid input. The `op-kw` argument is used in error
  messages to identify the calling operation."
  {:tag BigDecimal :added "2.1.0"}
  ^BigDecimal [x op-kw]
  (cond
    (nil? x)
    nil

    (instance? BigDecimal x)
    ^BigDecimal x

    (integer? x)
    (BigDecimal/valueOf (long x))

    (number? x)
    ;; NOTE: doubles will lose precision; prefer strings for wire formats.
    (BigDecimal/valueOf (double x))

    (string? x)
    (let [^String s (str/trim ^String x)
          s         (str/replace s "_" "")]
      (when-not (seq s)
        (throw
         (ex-info
          "Empty amount string."
          {:op    op-kw
           :value x})))
      (try
        (bigdec s)
        (catch Throwable e
          (throw
           (ex-info
            "Invalid amount string."
            {:op    op-kw
             :value x}
            e)))))

    :else
    (throw
     (ex-info
      "Unsupported amount representation."
      {:op    op-kw
       :value x
       :class (class x)}))))

;;
;; Rescaling for serialization.
;;

(defn- normalize-rescale
  {:private true :tag Integer :added "2.1.0"}
  [rescale op-kw]
  (when (some? rescale)
    (when-not (integer? rescale)
      (throw
       (ex-info
        "Invalid rescale; expected a non-negative integer."
        {:op    op-kw
         :value rescale
         :class (class rescale)})))
    (let [sc (long rescale)]
      (when (neg? sc)
        (throw
         (ex-info
          "Invalid rescale; expected a non-negative integer."
          {:op    op-kw
           :value rescale})))
      (when (> sc Integer/MAX_VALUE)
        (throw
         (ex-info
          "Invalid rescale; value exceeds Integer/MAX_VALUE."
          {:op    op-kw
           :value rescale
           :max   Integer/MAX_VALUE})))
      (int sc))))

(defn- resolve-rounding-mode
  {:private true :tag RoundingMode :added "2.1.0"}
  [rounding-mode op-kw]
  (cond
    (nil? rounding-mode)
    nil

    (instance? RoundingMode rounding-mode)
    rounding-mode

    :else
    (let [rm (scale/post-parse-rounding rounding-mode)]
      (if (instance? RoundingMode rm)
        rm
        (throw
         (ex-info
          "Invalid rounding-mode."
          {:op    op-kw
           :value rounding-mode
           :class (class rounding-mode)}))))))

(defn rescale-amount
  "Rescales a BigDecimal amount to the given scale for serialization.

  When `rescale` is nil, returns the amount as-is.
  When `rescale` is provided, rescales using the given rounding-mode or
  falls back to `scale/*rounding-mode*` or UNNECESSARY.

  The rounding-mode can be a RoundingMode, keyword, or string."
  {:tag BigDecimal :added "2.1.0"}
  ^BigDecimal [^BigDecimal amount rescale rounding-mode]
  (if (nil? rescale)
    amount
    (let [target-scale  (normalize-rescale rescale :bankster.serializers.common/rescale-amount)
          current-scale (.scale amount)
          explicit-rm   (when (some? rounding-mode)
                          (resolve-rounding-mode rounding-mode
                                                 :bankster.serializers.common/rescale-amount))]
      (if (== current-scale target-scale)
        amount
        (let [^RoundingMode rm (or explicit-rm
                                   (scale/rounding-mode)
                                   scale/ROUND_UNNECESSARY)]
          (try
            (.setScale amount target-scale rm)
            (catch ArithmeticException e
              (throw
               (ex-info
                "ArithmeticException during rescaling."
                {:op                   :bankster.serializers.common/rescale-amount
                 :amount               amount
                 :scale                target-scale
                 :rounding-mode        rm
                 :arithmetic-exception true}
                e)))))))))

;;
;; Keys filtering for serialization.
;;

(defn extract-keys-spec
  "Extracts nested opts from :keys vector.

  Returns map of {field-keyword -> nested-opts-or-nil}.
  Returns empty map {} for empty vector (filters everything out).
  Returns nil for nil input (no filtering).

  E.g. [:amount {:currency {:keys [:id :numeric]}}]
       => {:amount nil, :currency {:keys [:id :numeric]}}"
  {:added "2.1.0"}
  [keys-vec]
  (when (some? keys-vec)
    (reduce
     (fn [acc item]
       (cond
         (keyword? item)
         (assoc acc item nil)

         (map? item)
         (reduce-kv
          (fn [a k v]
            (assoc a k v))
          acc
          item)

         :else acc))
     {}
     keys-vec)))

(defn filter-map-by-keys
  "Filters map `m` to only include keys from `keys-spec`.

  For keys with nested opts, applies those opts recursively via `serializer-fn`."
  {:added "2.1.0"}
  [m keys-spec serializer-fn]
  (if (nil? keys-spec)
    m
    (reduce-kv
     (fn [acc k nested-opts]
       (if-some [v (get m k)]
         (if (some? nested-opts)
           ;; Nested opts - apply serializer recursively
           (assoc acc k (serializer-fn v nested-opts))
           (assoc acc k v))
         acc))
     {}
     keys-spec)))

;;
;; Money construction for deserialization.
;;

(defn make-money
  "Creates Money from currency and amount for deserialization.

  When `rescale` is provided (non-nil integer), the Currency object is cloned with
  that scale instead of using its nominal scale. This allows deserializing data with
  a different precision than the registry currency defines.

  The rounding-mode can be a RoundingMode, keyword, or string - it will be parsed
  using `scale/post-parse-rounding`. If not provided, falls back to
  `scale/*rounding-mode*` or throws on precision loss.

  Arguments:
  - `currency`      - Currency object or identifier (keyword, string)
  - `amount`        - BigDecimal amount
  - `registry`      - Registry to use for currency lookup
  - `rounding-mode` - optional rounding mode (RoundingMode, keyword, or string)
  - `rescale`       - optional target scale (overrides currency's nominal scale)"
  {:tag Money :added "2.1.0"}
  (^Money [currency amount ^Registry registry]
   (let [^Currency c (if (instance? Currency currency)
                       currency
                       (currency/unit currency registry))
         sc          (long (.scale ^Currency c))]
     (try
       (let [^BigDecimal a (scale/monetary-scale amount sc)]
         (Money. c a))
       (catch ArithmeticException e
         (throw
          (ex-info
           "ArithmeticException during monetary scaling."
           {:op                   :bankster.serializers.common/make-money
            :amount               amount
            :scale                sc
            :rounding-mode        (scale/rounding-mode)
            :arithmetic-exception true}
           e))))))
  (^Money [currency amount ^Registry registry rounding-mode]
   (let [^Currency c      (if (instance? Currency currency)
                            currency
                            (currency/unit currency registry))
         sc               (long (.scale ^Currency c))
         ^RoundingMode rm (resolve-rounding-mode rounding-mode
                                                 :bankster.serializers.common/make-money)]
     (try
       (let [^BigDecimal a (if (some? rm)
                             (scale/monetary-scale amount sc rm)
                             (scale/monetary-scale amount sc))]
         (Money. c a))
       (catch ArithmeticException e
         (throw
          (ex-info
           "ArithmeticException during monetary scaling."
           {:op                   :bankster.serializers.common/make-money
            :amount               amount
            :scale                sc
            :rounding-mode        (or rm (scale/rounding-mode))
            :arithmetic-exception true}
           e))))))
  (^Money [currency amount ^Registry registry rounding-mode rescale]
   (if (nil? rescale)
     ;; No rescale - use standard path
     (if (some? rounding-mode)
       (make-money currency amount registry rounding-mode)
       (make-money currency amount registry))
     ;; Rescale: get currency from registry, then clone with new scale
     (let [^Currency base-cur (if (instance? Currency currency)
                                currency
                                (currency/unit currency registry))
           target-scale       (normalize-rescale rescale :bankster.serializers.common/make-money)
           ;; Clone currency with the target scale
           ^Currency c        (assoc base-cur :scale target-scale)
           ^RoundingMode rm   (resolve-rounding-mode rounding-mode
                                                     :bankster.serializers.common/make-money)]
       (try
         (let [^BigDecimal a (if (some? rm)
                               (scale/monetary-scale amount (long target-scale) rm)
                               (scale/monetary-scale amount (long target-scale)))]
           (Money. c a))
         (catch ArithmeticException e
           (throw
            (ex-info
             "ArithmeticException during monetary scaling."
             {:op                   :bankster.serializers.common/make-money
              :amount               amount
              :scale                target-scale
              :rounding-mode        (or rm (scale/rounding-mode))
              :arithmetic-exception true}
             e))))))))
