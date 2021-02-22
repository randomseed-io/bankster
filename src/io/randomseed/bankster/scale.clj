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

(def ^MathContext unscaled-context
  (MathContext. 0 RoundingMode/UNNECESSARY))

;;
;; Default rounding mode.
;;

(def ^:dynamic
  *rounding-mode*
  "Default rounding mode."
  nil)

;;
;; Constants describing rounding modes.
;;

(def ^{:const true :tag 'int} ROUND_CEILING     (int BigDecimal/ROUND_CEILING))
(def ^{:const true :tag 'int} ROUND_FLOOR       (int BigDecimal/ROUND_FLOOR))
(def ^{:const true :tag 'int} ROUND_HALF_UP     (int BigDecimal/ROUND_HALF_UP))
(def ^{:const true :tag 'int} ROUND_HALF_DOWN   (int BigDecimal/ROUND_HALF_DOWN))
(def ^{:const true :tag 'int} ROUND_HALF_EVEN   (int BigDecimal/ROUND_HALF_EVEN))
(def ^{:const true :tag 'int} ROUND_UP          (int BigDecimal/ROUND_UP))
(def ^{:const true :tag 'int} ROUND_DOWN        (int BigDecimal/ROUND_DOWN))
(def ^{:const true :tag 'int} ROUND_UNNECESSARY (int BigDecimal/ROUND_UNNECESSARY))

;;
;; Scalable protocol.
;;

(defprotocol Scalable
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
        (.setScale ^BigDecimal num (int scale) (int *rounding-mode*))
        (.setScale ^BigDecimal num (int scale)))))
   (^BigDecimal [num scale r]
    (if (= (.scale ^BigDecimal num) (int scale))
      ^BigDecimal num
      (.setScale ^BigDecimal num (int scale) (int r)))))

  clojure.lang.BigInt

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (.toBigDecimal ^clojure.lang.BigInt num))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) (int scale) (int *rounding-mode*))
      (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) (int scale))))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) (int scale) (int r))))

  BigInteger

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. ^BigInteger num))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) (int scale) (int *rounding-mode*))
      (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) (int scale))))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) (int scale) (int r))))

  Double

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf num))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale ^BigDecimal (BigDecimal/valueOf num) (int scale) (int *rounding-mode*))
      (.setScale ^BigDecimal (BigDecimal/valueOf num) (int scale))))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal/valueOf num) (int scale) (int r))))

  Float

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf (double num)))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) (int scale) (int *rounding-mode*))
      (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) (int scale))))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) (int scale) (int r))))

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
      (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale) (int *rounding-mode*))
         (apply (.denominator ^clojure.lang.Ratio num) (int scale) (int *rounding-mode*)))
      (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale))
         (apply (.denominator ^clojure.lang.Ratio num) (int scale)))))
   (^BigDecimal [num scale r]
    (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale) (int r))
       (apply (.denominator ^clojure.lang.Ratio num) (int scale) (int r)))))

  Number

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num] (BigDecimal/valueOf (long num)))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) (int scale) (int *rounding-mode*))
      (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) (int scale))))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) (int scale) (int r))))

  String

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. num ^MathContext unscaled-context))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale) (int *rounding-mode*))
      (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale))))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale) (int r))))

  nil

  (^Boolean scalable? [num] false)
  (^Boolean applied?  [num] false)

  (of [num] nil)

  (apply
    ([num]         nil)
    ([num scale]   nil)
    ([num scale r] nil))

  Object

  (^Boolean    scalable? [num] nil)
  (^Boolean    applied?  [num] nil)

  (of [num] (.scale ^BigDecimal (apply num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. num ^MathContext unscaled-context))
   (^BigDecimal [num scale]
    (if *rounding-mode*
      (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale) (int *rounding-mode*))
      (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale))))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale) (int r)))))

;;
;; Aliases.
;;

(defn ^io.randomseed.bankster.scale.Scalable with
  "Alias for scale/apply."
  (^io.randomseed.bankster.scale.Scalable [num]         (apply num))
  (^io.randomseed.bankster.scale.Scalable [num scale]   (apply num scale))
  (^io.randomseed.bankster.scale.Scalable [num scale rounding-mode] (apply num scale rounding-mode)))

;;
;; Rounding mode handling.
;;

(defn parse-rounding
  "Helper for parsing rounding modes in macros. Accepted input:
  ROUND_mode, :ROUND_mode, BigDecimal/ROUND_mode, money/ROUND_mode, :mode, mode."
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
                (symbol "java.math.BigDecimal" sname)
                (symbol "java.math.BigDecimal" (str "ROUND_" sname)))))
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
  [rounding-mode & body]
  (let [rms# (parse-rounding rounding-mode)]
    `(binding [*rounding-mode* ~rms#]
       ~@body)))
