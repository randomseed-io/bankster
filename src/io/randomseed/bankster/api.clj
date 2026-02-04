(ns

    ^{:doc    "Bankster library, front API."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api

  (:require [io.randomseed.bankster.scale        :as        scale]
            [io.randomseed.bankster.money        :as        money]
            [io.randomseed.bankster.currency     :as     currency]
            [io.randomseed.bankster.registry     :as     registry]
            [io.randomseed.bankster.api.currency :as api-currency]
            [io.randomseed.bankster.api.money    :as    api-money])

  (:import  (io.randomseed.bankster Currency
                                    Registry
                                    Money)
            (java.math              BigDecimal)))

;;
;; Registry
;;

(defn default-registry
  "Returns the default registry (honors `registry/*default*`)."
  {:tag Registry :added "2.2.0"}
  []
  (registry/get))

(defn registry-or-default
  "Resolves `true` or `nil` into the current default registry, otherwise returns the
  given value."
  {:tag Registry :added "2.2.0"}
  [registry]
  (if (or (nil? registry) (true? registry)) (registry/get) registry))

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
  - For `Currency`: returns the nominal scale (or `-1` for auto-scaled).
  - For other values: calls `io.randomseed.bankster.scale/of` to return its scale."
  {:tag Long :added "2.2.0"}
  [x]
  (cond
    (instance? Money    x) (clojure.core/long (money/scale ^Money x))
    (instance? Currency x) (currency/scale ^Currency x)
    :else                  (scale/of x)))

(defn auto-scaled?
  "Returns `true` if a scalable's derived scale is auto-scaled.

  Delegates to `io.randomseed.bankster.scale/auto?`."
  {:tag Boolean :added "2.2.0"}
  ^Boolean [x]
  (scale/auto? x))

(defn iso-currency?
  "Returns `true` when the given currency is a kind of ISO currency."
  {:tag Boolean :added "2.2.0"}
  ([currency]
   (currency/iso? currency))
  ([currency registry]
   (currency/iso? currency (registry-or-default registry))))

;;
;; Core money helpers
;;

(def ^{:tag      Money
       :added    "2.2.0"
       :doc      (:doc (meta #'api-money/resolve))
       :arglists (:arglists (meta #'api-money/resolve))}
  money
  api-money/resolve)


(def ^{:tag      Money
       :added    "2.2.0"
       :doc      (:doc (meta #'api-money/resolve-try))
       :arglists (:arglists (meta #'api-money/resolve-try))}
  money-try
  api-money/resolve-try)

;;
;; Core currency helpers
;;

(def ^{:tag      Currency
       :added    "2.2.0"
       :doc      (:doc (meta #'api-currency/resolve))
       :arglists (:arglists (meta #'api-currency/resolve))}
  currency
  api-currency/resolve)

(def ^{:tag      Currency
       :added    "2.2.0"
       :doc      (:doc (meta #'api-currency/resolve-try))
       :arglists (:arglists (meta #'api-currency/resolve-try))}
  currency-try
  api-currency/resolve-try)

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

(doseq [[_ v] (ns-interns *ns*)]
  (alter-meta! v assoc :auto-alias true))
