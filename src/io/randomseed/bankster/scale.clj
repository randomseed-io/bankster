(ns io.randomseed.bankster.scale

  ^{:doc    "Bankster library, scalable protocol with implementation."
    :author "Paweł Wilk"
    :added  "1.0.0"}

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
;; Scale calculations.
;;

(defn div-max-precision
  "Returns the maximum possible precision for the operation of dividing two BigDecimal
  numbers."
  {:tag 'int :added "1.0.0"}
  [^BigDecimal a ^BigDecimal b]
  (int (min (+ (int  (.precision ^BigDecimal a))
               (long (Math/ceil (/ (* 10.0 (int (.precision ^BigDecimal b))) 3.0))))
            Integer/MAX_VALUE)))

(defn div-math-context
  "Returns the MathContext set to handle the possible precision for the operation of
  dividing two BigDecimal numbers. Optional rounding mode may be provided."
  ([^BigDecimal a ^BigDecimal b]
   (MathContext. (int (div-max-precision ^BigDecimal a ^BigDecimal b))
                 (or ^RoundingMode *rounding-mode* ^RoundingMode RoundingMode/UNNECESSARY)))
  ([^BigDecimal a ^BigDecimal b ^RoundingMode rounding-mode]
   (MathContext. (int (div-max-precision ^BigDecimal a ^BigDecimal b))
                 ^RoundingMode rounding-mode)))

;;
;; Scalable protocol.
;;

(defprotocol ^{:added "1.0.0"} Scalable
  "The Scalable protocol describes values that can be scaled."

  (^{:added "1.0.0"}
   ^int of
   [num]
   "Returns a scale. If the given value is not of type that scales (or is used to
  produce scaled types) it will be converted to such.")

  (apply
    [num] [num scale] [num scale rounding-mode]
    "Converts the given value to a scalable with or without changing its scale. For
  values that already are scalable it changes their scales if called with a second
  argument. The third argument, rounding-mode, must be present when downscaling and
  rounding is needed. For compound values (like monetary amounts) it will rescale the
  amount but will NOT update scale information of the unit (e.g. currency component).

  When operating on Money objects and called with a single argument, it reapplies
  the nominal currency scale.")

  (^{:tag BigDecimal :added "1.0.0"}
   amount
   [num] [num scale] [num scale rounding-mode]
   "Returns the amount of a scalable as a BigDecimal number. Some scalables may not
    be purely numeric so this function is to get the actual, calculable value out of
    them.")

  (^{:tag Boolean :added "1.0.0"}
   applied?
   [num]
   "Returns true if the value is of type which contains scaling information.")

  (^{:tag Boolean :added "1.0.0"}
   scalable?
   [num]
   "Returns true if the value can be converted to a scalable."))

(extend-protocol Scalable

  BigDecimal

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] true)

  (of [num] (int (.scale ^BigDecimal num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    ^BigDecimal num)
   (^BigDecimal [num scale]
    (if (= (.scale ^BigDecimal num) (int scale))
      ^BigDecimal num
      (if-some [rm *rounding-mode*]
        (.setScale ^BigDecimal num (int scale) ^RoundingMode rm)
        (.setScale ^BigDecimal num (int scale)))))
   (^BigDecimal [num scale ^RoundingMode r]
    (if (= (.scale ^BigDecimal num) (int scale))
      ^BigDecimal num
      (.setScale ^BigDecimal num (int scale) ^RoundingMode r))))

  (^BigDecimal amount
   (^BigDecimal [num] num)
   (^BigDecimal [num scale]   (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  clojure.lang.BigInt

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (int (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (.toBigDecimal ^clojure.lang.BigInt num))
   (^BigDecimal [num scale]
    (if-some [rm *rounding-mode*]
      (.setScale (.toBigDecimal ^clojure.lang.BigInt num) (int scale) ^RoundingMode rm)
      (.setScale (.toBigDecimal ^clojure.lang.BigInt num) (int scale))))
   (^BigDecimal [num scale ^RoundingMode r]
    (.setScale (.toBigDecimal ^clojure.lang.BigInt num) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  BigInteger

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (int (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. ^BigInteger num))
   (^BigDecimal [num scale]
    (if-some [rm *rounding-mode*]
      (.setScale (BigDecimal. ^BigInteger num) (int scale) ^RoundingMode rm)
      (.setScale (BigDecimal. ^BigInteger num) (int scale))))
   (^BigDecimal [num scale ^RoundingMode r]
    (.setScale (BigDecimal. ^BigInteger num) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  Double

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (int (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf num))
   (^BigDecimal [num scale]
    (if-some [rm *rounding-mode*]
      (.setScale (BigDecimal/valueOf num) (int scale) ^RoundingMode rm)
      (.setScale (BigDecimal/valueOf num) (int scale))))
   (^BigDecimal [num scale ^RoundingMode r]
    (.setScale (BigDecimal/valueOf num) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  Float

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (int (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf (double num)))
   (^BigDecimal [num scale]
    (if-some [rm *rounding-mode*]
      (.setScale (BigDecimal/valueOf (double num)) (int scale) ^RoundingMode rm)
      (.setScale (BigDecimal/valueOf (double num)) (int scale))))
   (^BigDecimal [num scale ^RoundingMode r]
    (.setScale (BigDecimal/valueOf (double num)) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  clojure.lang.Ratio

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (int (.scale ^BigDecimal (apply num))))

  (apply
    (^BigDecimal [num]
     (let [^BigDecimal a (apply (.numerator   ^clojure.lang.Ratio num))
           ^BigDecimal b (apply (.denominator ^clojure.lang.Ratio num))]
       (if-some [rm *rounding-mode*]
         (.divide ^BigDecimal a ^BigDecimal b
                  ^MathContext (div-math-context ^BigDecimal a
                                                 ^BigDecimal b
                                                 ^RoundingMode rm))
         (.divide ^BigDecimal a ^BigDecimal b))))
    (^BigDecimal [num scale]
     (if-some [rm *rounding-mode*]
       (.divide ^BigDecimal (apply (.numerator   ^clojure.lang.Ratio num))
                ^BigDecimal (apply (.denominator ^clojure.lang.Ratio num))
                (int scale)
                ^RoundingMode rm)
       (.divide ^BigDecimal (apply (.numerator   ^clojure.lang.Ratio num))
                ^BigDecimal (apply (.denominator ^clojure.lang.Ratio num))
                (int scale)
                ^RoundingMode ROUND_UNNECESSARY)))
    (^BigDecimal [num scale ^RoundingMode r]
     (.divide ^BigDecimal (apply (.numerator   ^clojure.lang.Ratio num))
              ^BigDecimal (apply (.denominator ^clojure.lang.Ratio num))
              (int scale)
              ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num]         (apply num))
   (^BigDecimal [num scale]   (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  Number

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (int (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [num] (BigDecimal/valueOf (long num)))
   (^BigDecimal [num scale]
    (if-some [rm *rounding-mode*]
      (.setScale (BigDecimal/valueOf (long num)) (int scale) ^RoundingMode rm)
      (.setScale (BigDecimal/valueOf (long num)) (int scale))))
   (^BigDecimal [num scale ^RoundingMode r]
    (.setScale (BigDecimal/valueOf (long num)) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  String

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (int (.scale (BigDecimal. ^String num ^MathContext unscaled-context))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. num ^MathContext unscaled-context))
   (^BigDecimal [num scale]
    (if-some [rm *rounding-mode*]
      (.setScale (BigDecimal. num ^MathContext unscaled-context) (int scale) ^RoundingMode rm)
      (.setScale (BigDecimal. num ^MathContext unscaled-context) (int scale))))
   (^BigDecimal [num scale ^RoundingMode r]
    (.setScale (BigDecimal. num ^MathContext unscaled-context) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  nil

  (^Boolean scalable? [num] false)
  (^Boolean applied?  [num] false)

  (of [num] nil)

  (apply
    ([num]         nil)
    ([num scale]   nil)
    ([num scale r] nil))

  (^BigDecimal amount
   (^BigDecimal [num] nil)
   (^BigDecimal [num scale] nil)
   (^BigDecimal [num scale r] nil))

  Object

  (scalable? [num] nil)
  (applied?  [num] nil)

  (of [num] (int (.scale ^BigDecimal (apply num))))

  (^BigDecimal apply
   (^BigDecimal [n]
    (BigDecimal. (long (num n)) ^MathContext unscaled-context))
   (^BigDecimal [n scale]
    (if-some [rm *rounding-mode*]
      (.setScale (BigDecimal. (long (num n)) ^MathContext unscaled-context) (int scale) ^RoundingMode rm)
      (.setScale (BigDecimal. (long (num n)) ^MathContext unscaled-context) (int scale))))
   (^BigDecimal [n scale ^RoundingMode r]
    (.setScale (BigDecimal. (long (num n)) ^MathContext unscaled-context) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r))))

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
  (^BigDecimal [n scale]
   (.setScale ^BigDecimal (amount n scale) 0 ROUND_DOWN))
  (^BigDecimal [n scale rounding-mode]
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
  (^BigDecimal [n scale]
   (.movePointRight ^BigDecimal (.remainder ^BigDecimal (amount n scale)
                                            BigDecimal/ONE)
                    (int scale)))
  (^BigDecimal [n scale rounding-mode]
   (.movePointRight ^BigDecimal (.remainder ^BigDecimal (amount n scale rounding-mode)
                                            BigDecimal/ONE)
                    (int scale))))

;;
;; Conversions to int and long.
;;

(def ^{:tag 'int :added "1.1.0" :private true} float-precision   6)
(def ^{:tag 'int :added "1.1.0" :private true} double-precision 15)

(defn ->int
  "Converts to an int with optional rounding."
  {:tag 'int :added "1.0.0"}
  ([n]                             (.intValueExact ^BigDecimal (amount n 0)))
  ([n ^RoundingMode rounding-mode] (.intValueExact ^BigDecimal (amount n 0 rounding-mode))))

(defn ->long
  "Converts to a long with optional rounding."
  {:tag 'long :added "1.0.0"}
  (^long [n]                             (.longValueExact ^BigDecimal (amount n 0)))
  (^long [n ^RoundingMode rounding-mode] (.longValueExact ^BigDecimal (amount n 0 rounding-mode))))

(defn ->float
  "Converts to a float with optional rescaling and rounding. If the precision of
  float is to small to express the value, rounding must be provided (either
  explicitly or using *rounding-mode*), otherwise an exception will be thrown."
  {:tag 'float :added "1.1.0"
   :arglists '([n]
               [n scale]
               [n rounding-mode]
               [n scale rounding-mode])}
  ([n]
   (.floatValue
    ^BigDecimal (.round ^BigDecimal (amount n)
                        (MathContext.
                         float-precision
                         (or ^RoundingMode *rounding-mode*
                             ^RoundingMode ROUND_UNNECESSARY)))))
  ([n scale-or-rounding]
   (if (instance? RoundingMode scale-or-rounding)
     (.floatValue
      ^BigDecimal (.round ^BigDecimal (amount n)
                          (MathContext.
                           float-precision
                           ^RoundingMode scale-or-rounding)))
     (let [^RoundingMode rm (or *rounding-mode* ROUND_UNNECESSARY)]
       (.floatValue
        ^BigDecimal (.round ^BigDecimal (amount n scale-or-rounding rm)
                            (MathContext.
                             float-precision
                             ^RoundingMode rm))))))
  ([n scale ^RoundingMode rounding-mode]
   (.floatValue
    ^BigDecimal (.round ^BigDecimal (amount n scale rounding-mode)
                        (MathContext.
                         float-precision
                         ^RoundingMode rounding-mode)))))

(defn ->double
  "Converts to a double with optional rescaling and rounding. If the precision of
  double is to small to express the value, rounding must be provided (either
  explicitly or using *rounding-mode*), otherwise an exception will be thrown."
  {:tag 'double :added "1.1.0"
   :arglists '(^double [n]
               ^double [n scale]
               ^double [n rounding-mode]
               ^double [n scale rounding-mode])}
  (^double [n]
   (.doubleValue
    ^BigDecimal (.round ^BigDecimal (amount n)
                        (MathContext.
                         double-precision
                         (or ^RoundingMode *rounding-mode*
                             ^RoundingMode ROUND_UNNECESSARY)))))
  (^double [n scale-or-rounding]
   (if (instance? RoundingMode scale-or-rounding)
     (.doubleValue
      ^BigDecimal (.round ^BigDecimal (amount n)
                          (MathContext.
                           double-precision
                           ^RoundingMode scale-or-rounding)))
     (let [^RoundingMode rm (or *rounding-mode* ROUND_UNNECESSARY)]
       (.doubleValue
        ^BigDecimal (.round ^BigDecimal (amount n scale-or-rounding rm)
                            (MathContext.
                             double-precision
                             ^RoundingMode rm))))))
  (^double [n scale ^RoundingMode rounding-mode]
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
  (^io.randomseed.bankster.scale.Scalable [num scale] (apply num scale))
  (^io.randomseed.bankster.scale.Scalable [num scale rounding-mode] (apply num scale rounding-mode)))

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
               (catch IllegalArgumentException e nil)))
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
                   (catch IllegalArgumentException e nil)))
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
    `(binding [*rounding-mode* (post-parse-rounding ~rms#)]
       ~@body)))

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
     `(binding [*each* true
                *rounding-mode* (post-parse-rounding ~rms#)]
        ~@body))))

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
