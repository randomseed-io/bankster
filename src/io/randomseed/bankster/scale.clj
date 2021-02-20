(ns io.randomseed.bankster.scale

  ^{:doc    "Bankster library, scalable protocol with implementation."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [apply])

  (:import [java.math MathContext RoundingMode BigDecimal]))

;;
;; Initial math context.
;;

(def ^MathContext unscaled-context
  (MathContext. 0 RoundingMode/UNNECESSARY))

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
      (.setScale ^BigDecimal num (int scale))))
   (^BigDecimal [num scale r]
    (if (= (.scale ^BigDecimal num) (int scale))
      ^BigDecimal num
      (.setScale ^BigDecimal num (int scale) (int r)))))

  clojure.lang.BigInt

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (.toBigDecimal ^clojure.lang.BigInt num))
   (^BigDecimal [num scale]
    (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) (int scale)))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) (int scale) (int r))))

  BigInteger

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (BigDecimal. ^BigInteger num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. ^BigInteger num))
   (^BigDecimal [num scale]
    (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) (int scale)))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) (int scale) (int r))))

  Double

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (BigDecimal/valueOf num)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf num))
   (^BigDecimal [num scale]
    (.setScale ^BigDecimal (BigDecimal/valueOf num) (int scale)))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal/valueOf num) (int scale) (int r))))

  Float

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (BigDecimal/valueOf (double num))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal/valueOf (double num)))
   (^BigDecimal [num scale]
    (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) (int scale)))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) (int scale) (int r))))

  clojure.lang.Ratio

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (/ (of (.numerator ^clojure.lang.Ratio num))
                                   (of (.denominator ^clojure.lang.Ratio num)))))

  (^BigDecimal apply
   (^BigDecimal [num]
    (/ (apply (.numerator ^clojure.lang.Ratio num))
       (apply (.denominator ^clojure.lang.Ratio num))))
   (^BigDecimal [num scale]
    (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale))
       (apply (.denominator ^clojure.lang.Ratio num) (int scale))))
   (^BigDecimal [num scale r]
    (/ (apply (.numerator ^clojure.lang.Ratio num) (int scale) (int r))
       (apply (.denominator ^clojure.lang.Ratio num) (int scale) (int r)))))

  Number

  (^Boolean scalable? [num] true)
  (^Boolean applied?  [num] false)

  (of [num] (.scale ^BigDecimal (BigDecimal/valueOf (long num))))

  (^BigDecimal apply
   (^BigDecimal [num] (BigDecimal/valueOf (long num)))
   (^BigDecimal [num scale]
    (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) (int scale)))
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
    (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale)))
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

  (of [num] (.scale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context)))

  (^BigDecimal apply
   (^BigDecimal [num]
    (BigDecimal. num ^MathContext unscaled-context))
   (^BigDecimal [num scale]
    (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale)))
   (^BigDecimal [num scale r]
    (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) (int scale) (int r)))))
