(ns

    ^{:doc    "Bankster library, scalable protocol with implementation."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.scale

  (:refer-clojure :exclude [apply])

  (:require [clojure.string  :as str])

  (:import  [java.math MathContext RoundingMode BigDecimal]))

;;
;; Initial math context.
;;

(def ^{:tag MathContext :added "1.0.0"}
  unscaled-context
  "MathContext indicating auto-scaling and no rounding."
  (MathContext. 0 RoundingMode/UNNECESSARY))

;;
;; Default rounding mode.
;;

(def ^{:dynamic true :tag RoundingMode :added "1.0.0"}
  *rounding-mode*
  "Default rounding mode."
  nil)

(def ^{:tag ThreadLocal :no-doc true :added "2.0.0"}
  thread-rounding-mode
  "Thread-local cache used by `with-rounding`/`with-rescaling` to avoid dynamic Var
  lookups in hot paths. Falls back to `*rounding-mode*` when not set."
  (ThreadLocal.))

(defn rounding-mode
  "Returns a rounding mode using a thread-local fast path when available.

  Falls back to `*rounding-mode*`."
  {:tag RoundingMode :no-doc true :added "2.0.0"}
  (^RoundingMode []
   (or (.get ^ThreadLocal thread-rounding-mode)
       *rounding-mode*))
  (^RoundingMode [^RoundingMode default]
   (or (.get ^ThreadLocal thread-rounding-mode)
       *rounding-mode*
       default)))

;;
;; Re-scaling iterative operations.
;;

(def ^{:dynamic true :tag Boolean :added "1.0.0"}
  *each*
  "Enables re-scaling after each consecutive operation."
  false)

;;
;; Rounding modes.
;;

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_CEILING
  "Rounding mode to round towards positive infinity."
  RoundingMode/CEILING)

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_FLOOR
  "Rounding mode to round towards negative infinity."
  RoundingMode/FLOOR)

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_HALF_UP
  "Rounding mode to round towards nearest neighbor, unless neighbors are equidistant,
  in which case round up."
  RoundingMode/HALF_UP)

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_HALF_DOWN
  "Rounding mode to round towards nearest neighbor, unless neighbors are equidistant,
  in which case round down."
  RoundingMode/HALF_DOWN)

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_HALF_EVEN
  "Rounding mode to round towards nearest neighbor, unless neighbors are equidistant,
  in which case round towards the nearest one."
  RoundingMode/HALF_EVEN)

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_UP
  "Rounding mode to round away from zero."
  RoundingMode/UP)

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_DOWN
  "Rounding mode to round towards zero."
  RoundingMode/DOWN)

(def ^{:tag RoundingMode :added "1.0.0"}
  ROUND_UNNECESSARY
  "Indication that no rounding is necessary."
  RoundingMode/UNNECESSARY)

;;
;; Auto-scaled indicator.
;;

(def ^{:tag 'int :const true :added "2.1.0"}
  ^int auto-scaled
  "Represents the scale of a currency that is automatic and not limited to specific
  decimal places. Used to indicate that the scale should be determined by the actual
  precision of the amount, not by a fixed currency scale."
  (int -1))

(defn auto-scaled?
  "Returns `true` if the given scale equals the auto-scaled sentinel value (-1)."
  {:added "2.1.0" :tag Boolean}
  ^Boolean [^long scale]
  (== auto-scaled (int scale)))

(defmacro auto-scaled*?
  "Macro version of `auto-scaled?`. Returns `true` if the given scale equals the
  auto-scaled sentinel value (-1). Inlines the comparison for performance."
  {:added "2.1.0"}
  [scale]
  `(clojure.core/== auto-scaled (int ~scale)))

;;
;; Scale calculations.
;;

(defn div-max-precision
  "Returns the maximum possible precision for the operation of dividing two BigDecimal
  numbers."
  {:added "1.0.0" :auto-alias true}
  ^long [^BigDecimal a ^BigDecimal b]
  (long (min (+ (long (.precision ^BigDecimal a))
                (long (Math/ceil (/ (* 10.0 (int (.precision ^BigDecimal b))) 3.0))))
             (long Integer/MAX_VALUE))))

(defn div-math-context
  "Returns the MathContext set to handle the possible precision for the operation of
  dividing two BigDecimal numbers. Optional rounding mode may be provided."
  (^MathContext [^BigDecimal a ^BigDecimal b]
   (MathContext. (int (div-max-precision ^BigDecimal a ^BigDecimal b))
                 (or ^RoundingMode (rounding-mode) ^RoundingMode RoundingMode/UNNECESSARY)))
  (^MathContext [^BigDecimal a ^BigDecimal b ^RoundingMode rounding-mode]
   (MathContext. (int (div-max-precision ^BigDecimal a ^BigDecimal b))
                 ^RoundingMode rounding-mode)))

(defn- arithmetic-ex->ex-info
  "Wraps an ArithmeticException into ExceptionInfo with stable ex-data keys.

  Contract:
  - always sets `:op`, `:arithmetic-exception`, and `:arithmetic-exception/cause`
  - preserves all keys from the provided `data` map"
  {:tag clojure.lang.ExceptionInfo :private true :added "2.1.0"}
  [op data ^ArithmeticException e]
  (ex-info (or (.getMessage e) "Arithmetic error.")
           (assoc data
                  :op op
                  :arithmetic-exception true
                  :arithmetic-exception/cause e)
           e))

;;
;; Scalable protocol.
;;

(defprotocol ^{:added "1.0.0"} Scalable
  "The Scalable protocol describes values that can be scaled."

  (^{:added "1.0.0" :ex/strict true}
   of
   [num]
   "Returns a scale. If the given value is not of type that scales (or is used to
  produce scaled types) it will be converted to such.")

  (^{:added "1.0.0" :ex/strict true}
   apply
    [num] [num scale] [num scale rounding-mode]
    "Converts the given value to a scalable with or without changing its scale. For
  values that already are scalable it changes their scales if called with a second
  argument. The third argument, rounding-mode, must be present when downscaling and
  rounding is needed. For compound values (like monetary amounts) it will rescale the
  amount but will NOT update scale information of the unit (e.g. currency component).

  When operating on Money objects and called with a single argument, it reapplies
  the nominal currency scale.")

  (^{:tag BigDecimal :added "1.0.0" :ex/soft true}
   amount
   [num] [num scale] [num scale rounding-mode]
   "Returns the amount of a scalable as a BigDecimal number. Some scalables may not
    be purely numeric so this function is to get the actual, calculable value out of
    them.")

  (^{:tag Boolean :added "1.0.0" :ex/soft true}
   applied?
   [num]
   "Returns true if the value is of a type that contains scaling information.")

  (^{:tag Boolean :added "1.0.0" :ex/soft true}
   scalable?
   [num]
   "Returns true if the value can be converted to a scalable."))

(defn auto?
  "Returns `true` if the scale derived from `of` is auto-scaled.

  Uses `scalable?` as a soft guard (avoids strict resolution on unknown inputs)."
  {:tag Boolean :added "2.2.0"}
  ^Boolean [x]
  (when (scalable? x)
    (when-some [sc (of x)]
      (auto-scaled*? sc))))

(extend-protocol Scalable

  BigDecimal

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] true)

  (of ^long [num] (long (.scale ^BigDecimal num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    ^BigDecimal num)
   (^BigDecimal [num ^long scale]
    (let [sc (int scale)]
      (if (== (.scale ^BigDecimal num) sc)
        ^BigDecimal num
        (let [rm (rounding-mode)]
          (try
            (if (some? rm)
              (.setScale ^BigDecimal num sc ^RoundingMode rm)
              (.setScale ^BigDecimal num sc))
            (catch ArithmeticException e
              (throw
               (arithmetic-ex->ex-info :scale/apply
                                       {:value         num
                                        :scale         sc
                                        :rounding-mode rm}
                                       e))))))))
   (^BigDecimal [num ^long scale ^RoundingMode r]
    (let [sc (int scale)]
      (if (== (.scale ^BigDecimal num) sc)
        ^BigDecimal num
        (try
          (.setScale ^BigDecimal num sc ^RoundingMode r)
          (catch ArithmeticException e
            (throw
             (arithmetic-ex->ex-info :scale/apply
                                     {:value         num
                                      :scale         sc
                                      :rounding-mode r}
                                     e))))))))

  (^BigDecimal amount
   (^BigDecimal [num] num)
   (^BigDecimal [num ^long scale]   (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  clojure.lang.BigInt

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] false)

  (of ^long [num] (long (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (.toBigDecimal ^clojure.lang.BigInt num))
   (^BigDecimal [num ^long scale]
    (let [sc (int scale)
          rm (rounding-mode)
          bd (.toBigDecimal ^clojure.lang.BigInt num)]
      (if (some? rm)
        (.setScale ^BigDecimal bd sc ^RoundingMode rm)
        (.setScale ^BigDecimal bd sc))))
   (^BigDecimal [num ^long scale ^RoundingMode r]
    (let [sc (int scale)
          bd (.toBigDecimal ^clojure.lang.BigInt num)]
      (.setScale ^BigDecimal bd sc ^RoundingMode r))))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num ^long scale] (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  BigInteger

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] false)

  (of ^long [num] (long (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. ^BigInteger num))
   (^BigDecimal [num ^long scale]
    (let [sc (int scale)
          rm (rounding-mode)
          bd (BigDecimal. ^BigInteger num)]
      (if (some? rm)
        (.setScale ^BigDecimal bd sc ^RoundingMode rm)
        (.setScale ^BigDecimal bd sc))))
   (^BigDecimal [num ^long scale ^RoundingMode r]
    (let [sc (int scale)
          bd (BigDecimal. ^BigInteger num)]
      (.setScale ^BigDecimal bd sc ^RoundingMode r))))

  (^BigDecimal amount
   (^BigDecimal [num]               (apply num))
   (^BigDecimal [num ^long scale]   (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  Double

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] false)

  (of ^long [num] (long (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf num))
   (^BigDecimal [num ^long scale]
    (let [sc (int scale)
          rm (rounding-mode)
          bd (BigDecimal/valueOf num)]
      (try
        (if (some? rm)
          (.setScale ^BigDecimal bd sc ^RoundingMode rm)
          (.setScale ^BigDecimal bd sc))
        (catch ArithmeticException e
          (throw
           (arithmetic-ex->ex-info :scale/apply
                                   {:value         num
                                    :scale         sc
                                    :rounding-mode rm}
                                   e))))))
   (^BigDecimal [num ^long scale ^RoundingMode r]
    (let [sc (int scale)
          bd (BigDecimal/valueOf num)]
      (try
        (.setScale ^BigDecimal bd sc ^RoundingMode r)
        (catch ArithmeticException e
          (throw
           (arithmetic-ex->ex-info :scale/apply
                                   {:value         num
                                    :scale         sc
                                    :rounding-mode r}
                                   e)))))))

  (^BigDecimal amount
   (^BigDecimal [num]               (apply num))
   (^BigDecimal [num ^long scale]   (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  Float

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] false)

  (of ^long [num] (long (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf (double num)))
   (^BigDecimal [num ^long scale]
    (let [sc (int scale)
          rm (rounding-mode)
          bd (BigDecimal/valueOf (double num))]
      (try
        (if (some? rm)
          (.setScale ^BigDecimal bd sc ^RoundingMode rm)
          (.setScale ^BigDecimal bd sc))
        (catch ArithmeticException e
          (throw
           (arithmetic-ex->ex-info :scale/apply
                                   {:value         num
                                    :scale         sc
                                    :rounding-mode rm}
                                   e))))))
   (^BigDecimal [num ^long scale ^RoundingMode r]
    (let [sc (int scale)
          bd (BigDecimal/valueOf (double num))]
      (try
        (.setScale ^BigDecimal bd sc ^RoundingMode r)
        (catch ArithmeticException e
          (throw
           (arithmetic-ex->ex-info :scale/apply
                                   {:value         num
                                    :scale         sc
                                    :rounding-mode r}
                                   e)))))))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num ^long scale]   (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  clojure.lang.Ratio

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] false)

  (of ^long [num] (long (.scale ^BigDecimal (apply num))))

  (apply
    (^BigDecimal [num]
     (let [^BigDecimal a (apply (.numerator   ^clojure.lang.Ratio num))
           ^BigDecimal b (apply (.denominator ^clojure.lang.Ratio num))
           rm            (rounding-mode)]
       (try
         (if (some? rm)
           (.divide ^BigDecimal a ^BigDecimal b
                    ^MathContext (div-math-context ^BigDecimal a
                                                   ^BigDecimal b
                                                   ^RoundingMode rm))
           (.divide ^BigDecimal a ^BigDecimal b))
	 (catch ArithmeticException e
	   (throw
	    (arithmetic-ex->ex-info :scale/apply
	                            {:value         num
	                             :numerator     a
	                             :denominator   b
	                             :rounding-mode rm}
	                            e))))))
    (^BigDecimal [num ^long scale]
     (let [^BigDecimal a (apply (.numerator   ^clojure.lang.Ratio num))
           ^BigDecimal b (apply (.denominator ^clojure.lang.Ratio num))
           sc            (int scale)
           rm            (or (rounding-mode) ROUND_UNNECESSARY)]
       (try
         (.divide ^BigDecimal a ^BigDecimal b sc ^RoundingMode rm)
         (catch ArithmeticException e
           (throw
            (arithmetic-ex->ex-info :scale/apply
                                    {:value         num
                                     :numerator     a
                                     :denominator   b
                                     :scale         sc
                                     :rounding-mode rm}
                                    e))))))
    (^BigDecimal [num ^long scale ^RoundingMode r]
     (let [^BigDecimal a (apply (.numerator   ^clojure.lang.Ratio num))
           ^BigDecimal b (apply (.denominator ^clojure.lang.Ratio num))
           sc            (int scale)]
       (try
         (.divide ^BigDecimal a ^BigDecimal b sc ^RoundingMode r)
         (catch ArithmeticException e
           (throw
            (arithmetic-ex->ex-info :scale/apply
                                    {:value         num
                                     :numerator     a
                                     :denominator   b
                                     :scale         sc
                                     :rounding-mode r}
                                    e)))))))

  (^BigDecimal amount
   (^BigDecimal [num]               (apply num))
   (^BigDecimal [num ^long scale]   (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  Number

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] false)

  (of ^long [num] (long (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num] (BigDecimal/valueOf (long num)))
   (^BigDecimal [num ^long scale]
    (let [sc (int scale)
          rm (rounding-mode)
          bd (BigDecimal/valueOf (long num))]
      (if (some? rm)
        (.setScale ^BigDecimal bd sc ^RoundingMode rm)
        (.setScale ^BigDecimal bd sc))))
   (^BigDecimal [num ^long scale ^RoundingMode r]
    (let [sc (int scale)
          bd (BigDecimal/valueOf (long num))]
      (.setScale ^BigDecimal bd sc ^RoundingMode r))))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num ^long scale]   (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  String

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] false)

  (of ^long [num] (long (.scale (BigDecimal. ^String num ^MathContext unscaled-context))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. num ^MathContext unscaled-context))
   (^BigDecimal [num ^long scale]
    (let [sc (int scale)
          rm (rounding-mode)
          bd (BigDecimal. num ^MathContext unscaled-context)]
      (try
        (if (some? rm)
          (.setScale ^BigDecimal bd sc ^RoundingMode rm)
          (.setScale ^BigDecimal bd sc))
        (catch ArithmeticException e
          (throw
           (arithmetic-ex->ex-info :scale/apply
                                   {:value         num
                                    :scale         sc
                                    :rounding-mode rm}
                                   e))))))
   (^BigDecimal [num ^long scale ^RoundingMode r]
    (let [sc (int scale)
          bd (BigDecimal. num ^MathContext unscaled-context)]
      (try
        (.setScale ^BigDecimal bd sc ^RoundingMode r)
        (catch ArithmeticException e
          (throw
           (arithmetic-ex->ex-info :scale/apply
                                   {:value         num
                                    :scale         sc
                                    :rounding-mode r}
                                   e)))))))

  (^BigDecimal amount
   (^BigDecimal [num]               (apply num))
   (^BigDecimal [num ^long scale]   (apply num scale))
   (^BigDecimal [num ^long scale r] (apply num scale r)))

  nil

  (^Boolean scalable? [_] false)
  (^Boolean applied?  [_] false)

  (of [_] nil)

  (apply
    ([_]     nil)
    ([_ _]   nil)
    ([_ _ _] nil))

  (amount
    ([_] nil)
    ([_ _] nil)
    ([_ _ _] nil))

  Object

  (^Boolean scalable? [_] false)
  (^Boolean applied?  [_] false)

  (of [_] nil)

  (apply
    ([_]     nil)
    ([_ _]   nil)
    ([_ _ _] nil))

  (amount
    ([_] nil)
    ([_ _] nil)
    ([_ _ _] nil)))

;;
;; Monetary scaling.
;;

(defn monetary-scale
  "Applies monetary scaling rules to a BigDecimal-like value `n` and a currency scale.

  When the scale indicates auto-scaling (equals `auto-scaled`, i.e., -1), the value
  is returned as-is (converted to BigDecimal if necessary). Otherwise, the value is
  scaled to the given number of decimal places.

  This function is locale-independent and strict - it will throw ArithmeticException
  (wrapped in ExceptionInfo) if rounding is needed but no rounding mode is available.

  Arguments:
  - `n`  - a value convertible to BigDecimal (number, string, BigDecimal)
  - `sc` - target scale (long); -1 means auto-scaled (no rescaling)
  - `rm` - optional RoundingMode for downscaling

  Returns BigDecimal."
  {:tag BigDecimal :added "2.1.0"}
  (^BigDecimal [n ^long sc]
   (if (auto-scaled*? sc)
     (if (instance? BigDecimal n)
       ^BigDecimal n
       (apply n))
     (let [^BigDecimal bd   (if (instance? BigDecimal n) n (apply n))
           ^RoundingMode rm (or (rounding-mode) ROUND_UNNECESSARY)]
       (.setScale ^BigDecimal bd (int sc) rm))))
  (^BigDecimal [n ^long sc ^RoundingMode rm]
   (if (auto-scaled*? sc)
     (if (instance? BigDecimal n)
       ^BigDecimal n
       (apply n))
     (let [^BigDecimal bd    (if (instance? BigDecimal n) n (apply n))
           ^RoundingMode rm# (or rm ROUND_UNNECESSARY)]
       (.setScale ^BigDecimal bd (int sc) rm#)))))

;;
;; Getting integer and fractional parts.
;;

(defn integer
  "Returns the integer part of the given number, converted to decimal if
  required. Optional scale and rounding mode can be given. Makes use of
  *rounding-mode* if it's set."
  {:tag BigDecimal :added "1.0.0"}
  (^BigDecimal [n]
   (.setScale ^BigDecimal (amount n) 0 ROUND_DOWN))
  (^BigDecimal [n ^long scale]
   (.setScale ^BigDecimal (amount n scale) 0 ROUND_DOWN))
  (^BigDecimal [n ^long scale rounding-mode]
   (.setScale ^BigDecimal (amount n scale rounding-mode) 0 ROUND_DOWN)))

(defn fractional
  "Returns the fractional part of the given number, converted to decimal if
  required. Optional scale and rounding mode can be given. Makes use of
  *rounding-mode* if it's set."
  {:tag BigDecimal :added "1.0.0"}
  (^BigDecimal [n]
   (let [^BigDecimal n (amount n)]
     (.movePointRight ^BigDecimal (.remainder n BigDecimal/ONE)
                      (int (.scale n)))))
  (^BigDecimal [n ^long scale]
   (.movePointRight ^BigDecimal (.remainder ^BigDecimal (amount n scale)
                                            BigDecimal/ONE)
                    (int scale)))
  (^BigDecimal [n ^long scale rounding-mode]
   (.movePointRight ^BigDecimal (.remainder ^BigDecimal (amount n scale rounding-mode)
                                            BigDecimal/ONE)
                    (int scale))))

;;
;; Conversions to int and long.
;;

(def ^{:const true :tag 'int :added "1.1.0" :private true} float-precision   6)
(def ^{:const true :tag 'int :added "1.1.0" :private true} double-precision 15)

(defn ->int
  "Converts to an int (may cast to long on call boundaries) with optional rounding."
  {:tag 'int :added "1.0.0"}
  ([n]                             (int (.intValueExact ^BigDecimal (amount n 0))))
  ([n ^RoundingMode rounding-mode] (int (.intValueExact ^BigDecimal (amount n 0 rounding-mode)))))

(defn ->long
  "Converts to a long with optional rounding."
  {:added "1.0.0" :auto-alias true}
  (^long [n]                             (long (.longValueExact ^BigDecimal (amount n 0))))
  (^long [n ^RoundingMode rounding-mode] (long (.longValueExact ^BigDecimal (amount n 0 rounding-mode)))))

(defn ->float
  "Converts to a float with optional rescaling and rounding. If the precision of
  float is to small to express the value, rounding must be provided (either
  explicitly or using *rounding-mode*), otherwise an exception will be thrown."
  {:tag      'float :added "1.1.0"
   :arglists '([n]
               [n ^long scale]
               [n rounding-mode]
               [n ^long scale rounding-mode])}
  ([n]
   (.floatValue
    ^BigDecimal (.round ^BigDecimal (amount n)
                        (MathContext.
                         float-precision
                         (or ^RoundingMode (rounding-mode)
                             ^RoundingMode ROUND_UNNECESSARY)))))
  ([n scale-or-rounding]
   (if (instance? RoundingMode scale-or-rounding)
     (.floatValue
      ^BigDecimal (.round ^BigDecimal (amount n)
                          (MathContext.
                           float-precision
                           ^RoundingMode scale-or-rounding)))
     (let [^RoundingMode rm (or (rounding-mode) ROUND_UNNECESSARY)]
       (.floatValue
        ^BigDecimal (.round ^BigDecimal (amount n (long scale-or-rounding) rm)
                            (MathContext.
                             float-precision
                             ^RoundingMode rm))))))
  ([n ^long scale ^RoundingMode rounding-mode]
   (.floatValue
    ^BigDecimal (.round ^BigDecimal (amount n scale rounding-mode)
                        (MathContext.
                         float-precision
                         ^RoundingMode rounding-mode)))))

(defn ->double
  "Converts to a double with optional rescaling and rounding. If the precision of
  double is to small to express the value, rounding must be provided (either
  explicitly or using *rounding-mode*), otherwise an exception will be thrown."
  {:added    "1.1.0"
   :arglists '(^double [n]
               ^double [n ^long scale]
               ^double [n rounding-mode]
               ^double [n ^long scale rounding-mode])}
  (^double [n]
   (.doubleValue
    ^BigDecimal (.round ^BigDecimal (amount n)
                        (MathContext.
                         double-precision
                         (or ^RoundingMode (rounding-mode)
                             ^RoundingMode ROUND_UNNECESSARY)))))
  (^double [n scale-or-rounding]
   (if (instance? RoundingMode scale-or-rounding)
     (.doubleValue
      ^BigDecimal (.round ^BigDecimal (amount n)
                          (MathContext.
                           double-precision
                           ^RoundingMode scale-or-rounding)))
     (let [^RoundingMode rm (or (rounding-mode) ROUND_UNNECESSARY)]
       (.doubleValue
        ^BigDecimal (.round ^BigDecimal (amount n (long scale-or-rounding) rm)
                            (MathContext.
                             double-precision
                             ^RoundingMode rm))))))
  (^double [n ^long scale ^RoundingMode rounding-mode]
   (.doubleValue
    ^BigDecimal (.round ^BigDecimal (amount n scale rounding-mode)
                        (MathContext.
                         double-precision
                         ^RoundingMode rounding-mode)))))

;;
;; Aliases.
;;

(defn with
  "Alias for scale/apply."
  {:tag io.randomseed.bankster.scale.Scalable :added "1.0.0"}
  (^io.randomseed.bankster.scale.Scalable [num] (apply num))
  (^io.randomseed.bankster.scale.Scalable [num ^long scale] (apply num scale))
  (^io.randomseed.bankster.scale.Scalable [num ^long scale rounding-mode] (apply num scale rounding-mode)))

;;
;; Conversions to printable.
;;

(defn to-plain-string
  "Converts the amount to a plain string."
  {:tag String :added "1.0.7"}
  [n]
  (.toPlainString ^BigDecimal (amount n)))

(defn to-clojure-string
  "Converts the amount to a plain string, adding the M suffix when needed."
  {:tag String :added "1.1.0"}
  [n]
  (let [n (amount n)]
    (str ^String (.toPlainString ^BigDecimal n)
         (when (>= (.precision ^BigDecimal n) 16) "M"))))

(defn to-clojure-symbol
  "Converts the amount to a symbol, adding the M suffix when needed."
  {:tag clojure.lang.Symbol :added "1.1.0"}
  [n]
  (symbol (to-clojure-string (amount n))))

(defn to-symbol
  "Converts the amount to a symbol."
  {:tag clojure.lang.Symbol :added "1.1.0"}
  [n]
  (symbol (to-plain-string (amount n))))

;;
;; Rounding mode handling.
;;

(defn parse-rounding
  "Helper for parsing rounding modes in macros. Returns a symbol or the given value."
  {:tag RoundingMode :added "1.0.0" :no-doc true}
  [n]
  (let [rn (if (simple-ident? n) (name n) n)
        rn (if (and (string? rn) (str/starts-with? rn "ROUND_")) (subs rn 6) rn)]
    (or (when (string? rn)
          (try (when (some? (RoundingMode/valueOf ^String rn))
                 (symbol "java.math.RoundingMode" rn))
               (catch IllegalArgumentException _e nil)))
        n)))

(defn post-parse-rounding
  "Helper for parsing rounding modes in macros. Returns a symbol or the given value."
  {:tag RoundingMode :added "1.2.11" :no-doc true}
  [n]
  (if (instance? RoundingMode n) n
      (let [rn (if (simple-ident? n) (name n) n)
            rn (if (and (string? rn) (str/starts-with? rn "ROUND_")) (subs rn 6) rn)]
        (or (when (string? rn)
              (try (RoundingMode/valueOf ^String rn)
                   (catch IllegalArgumentException _e nil)))
            n))))

(defmacro with-rounding
  "Sets the rounding mode for operations on scaled values.

  The first argument should be a valid rounding (from `io.randomseed.bankster.scale`
  or `java.math.RoundingMode`) or one of the following:

  CEILING     - rounds towards positive infinity.
  DOWN        - rounds towards zero.
  FLOOR       - rounds towards negative infinity.
  HALF_DOWN   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  HALF_EVEN   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  HALF_UP     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  UP          – rounds away from zero
  UNNECESSARY - asserts that the requested operation has an exact result, hence no rounding is necessary."
  {:added "1.0.0"}
  [rounding-mode & body]
  (let [rms# (parse-rounding rounding-mode)]
    `(let [rm#   (post-parse-rounding ~rms#)
           prev# (.get ^ThreadLocal thread-rounding-mode)]
       (.set ^ThreadLocal thread-rounding-mode rm#)
       (try
         (binding [*rounding-mode* rm#]
           ~@body)
         (finally
           (if (nil? prev#)
             (.remove ^ThreadLocal thread-rounding-mode)
             (.set ^ThreadLocal thread-rounding-mode prev#)))))))

(defmacro with-rescaling
  "Enables re-scaling on some consecutive operations which support it and sets the
  rounding mode for operations on scaled values. Internally sets `*each*` to true and
  `*rounding-mode*` to the given value.

  The first argument should be a valid rounding (from `io.randomseed.bankster.scale`
  or `java.math.RoundingMode`) or one of the following:

  CEILING     - rounds towards positive infinity.
  DOWN        - rounds towards zero.
  FLOOR       - rounds towards negative infinity.
  HALF_DOWN   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  HALF_EVEN   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  HALF_UP     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  UP          – rounds away from zero
  UNNECESSARY - asserts that the requested operation has an exact result, hence no rounding is necessary."
  {:added "1.0.0"}
  ([rounding-mode & body]
   (let [rms# (parse-rounding rounding-mode)]
     `(let [rm#   (post-parse-rounding ~rms#)
            prev# (.get ^ThreadLocal thread-rounding-mode)]
        (.set ^ThreadLocal thread-rounding-mode rm#)
        (try
          (binding [*each*          true
                    *rounding-mode* rm#]
            ~@body)
          (finally
            (if (nil? prev#)
              (.remove ^ThreadLocal thread-rounding-mode)
              (.set ^ThreadLocal thread-rounding-mode prev#))))))))

(defmacro each
  "Enables re-scaling on some consecutive operations which support it and sets the rounding mode for
  operations on scaled values. Internally sets *rescale-each* to true and
  *rounding-mode* to the given value.

  Practically, in most cases it is better to use the with-rescaling macro which also
  sets rounding mode."
  {:added "1.0.0"}
  ([& body]
   `(binding [*each* true]
      ~@body)))
