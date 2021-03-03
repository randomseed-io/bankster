(ns io.randomseed.bankster.scale

  ^{:doc    "Bankster library, scalable protocol with implementation."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [apply])

  (:require [clojure.string  :as str]
            [clojure.reflect :as  cr])

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
;; Scalable protocol.
;;

(defprotocol ^{:added "1.0.0"} Scalable
  "The Scalable protocol describes values that can be scaled."

  (of
    [num]
    "Returns a scale. If the given value is not of type that scales (or is used to
  produce scaled types) it will be converted to such.")

  (apply
    [num] [num scale] [num scale rounding-mode]
    "Converts the given value to a scalable with or without changing its scale (if
  any). For values that already are scalable changes their scale if called with a
  second argument. The third argument, rounding-mode, must be present when
  downscaling.")

  (amount
    [num] [num scale] [num scale rounding-mode]
    "Returns the amount of a scalable as BigDecimal number. Some scalables may not
    be numeric so this function is to get the actual number out of them.")

  (applied?
    [num]
    "Returns true if the value is of type which contains scaling information.")

  (scalable?
    [num]
    "Returns true if the value can be converted to a scalable."))

(extend-protocol Scalable

  BigDecimal

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] true)

  (of [num] (.scale ^BigDecimal num))

  (^BigDecimal apply
   (^BigDecimal [num]
    ^BigDecimal num)
   (^BigDecimal [num scale]
    (if (= (.scale ^BigDecimal num) (int scale))
      ^BigDecimal num
      (if *rounding-mode*
        (.setScale ^BigDecimal num (int scale) ^RoundingMode *rounding-mode*)
        (.setScale ^BigDecimal num (int scale)))))
   (^BigDecimal [num scale ^RoundingMode r]
    (if (= (.scale ^BigDecimal num) (int scale))
      ^BigDecimal num
      (.setScale ^BigDecimal num (int scale) ^RoundingMode r))))

  (^BigDecimal amount
   (^BigDecimal [num] num)
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  clojure.lang.BigInt

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (.toBigDecimal ^clojure.lang.BigInt num))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale (.toBigDecimal ^clojure.lang.BigInt num) (int scale) ^RoundingMode *rounding-mode*)
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

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. ^BigInteger num))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale (BigDecimal. ^BigInteger num) (int scale) ^RoundingMode *rounding-mode*)
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

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf num))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale (BigDecimal/valueOf num) (int scale) ^RoundingMode *rounding-mode*)
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

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf (double num)))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale (BigDecimal/valueOf (double num)) (int scale) ^RoundingMode *rounding-mode*)
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

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (/ (apply (.numerator ^clojure.lang.Ratio num))
       (apply (.denominator ^clojure.lang.Ratio num))))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale) ^RoundingMode *rounding-mode*)
         (apply (.denominator ^clojure.lang.Ratio num) (int scale) ^RoundingMode *rounding-mode*))
      (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale))
         (apply (.denominator ^clojure.lang.Ratio num) (int scale)))))
   (^BigDecimal [num scale ^RoundingMode r]
    (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale) ^RoundingMode r)
       (apply (.denominator ^clojure.lang.Ratio num) (int scale) ^RoundingMode r))))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r)))

  Number

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num] (BigDecimal/valueOf (long num)))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale (BigDecimal/valueOf (long num)) (int scale) ^RoundingMode *rounding-mode*)
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

  (of [num] (.scale (BigDecimal. ^String num ^MathContext unscaled-context)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. num ^MathContext unscaled-context))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale (BigDecimal. num ^MathContext unscaled-context) (int scale) ^RoundingMode *rounding-mode*)
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

  (^Boolean    scalable? [num] nil)
  (^Boolean    applied?  [num] nil)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [n]
    (BigDecimal. (long (num n)) ^MathContext unscaled-context))
   (^BigDecimal [n scale]
    (if *rounding-mode*
      (.setScale (BigDecimal. (long (num n)) ^MathContext unscaled-context) (int scale) ^RoundingMode *rounding-mode*)
      (.setScale (BigDecimal. (long (num n)) ^MathContext unscaled-context) (int scale))))
   (^BigDecimal [n scale ^RoundingMode r]
    (.setScale (BigDecimal. (long (num n)) ^MathContext unscaled-context) (int scale) ^RoundingMode r)))

  (^BigDecimal amount
   (^BigDecimal [num] (apply num))
   (^BigDecimal [num scale] (apply num scale))
   (^BigDecimal [num scale r] (apply num scale r))))


;;
;; Aliases.
;;

(defn ^io.randomseed.bankster.scale.Scalable with
  "Alias for scale/apply."
  {:added "1.0.0"}
  (^io.randomseed.bankster.scale.Scalable [num] (apply num))
  (^io.randomseed.bankster.scale.Scalable [num scale] (apply num scale))
  (^io.randomseed.bankster.scale.Scalable [num scale rounding-mode] (apply num scale rounding-mode)))

;;
;; Rounding mode handling.
;;

(defn parse-rounding
  "Helper for parsing rounding modes in macros. Accepted input:
  ROUND_mode, :ROUND_mode, BigDecimal/ROUND_mode, money/ROUND_mode, :mode, mode."
  {:tag RoundingMode :added "1.0.0"}
  [n]
  (if (ident? n)
    (let [sname (name n)
          ns-ok (if-some [ns (namespace n)]
                  (contains?
                   #{"BigDecimal"
                     "scale"
                     (cr/typename BigDecimal)} ns) true)]
      (if-not ns-ok n
              (if (str/starts-with? sname "ROUND_")
                (symbol "java.math.RoundingMode" (subs sname 6))
                (symbol "java.math.RoundingMode" sname))))
    n))

(defmacro with-rounding
  "Sets the rounding mode for operations on scaled values.

  The first argument should be one of the following:

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
    `(binding [*rounding-mode* ~rms#]
       ~@body)))

(defmacro with-rescaling
  "Enables re-scaling on some consecutive operations and sets the rounding mode for
  operations on scaled values. Internally sets *rescale-each* to true and
  *rounding-mode* to the given value.

  The first argument should be one of the following:

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
    `(binding [*each* true
               *rounding-mode* ~rms#]
       ~@body)))
