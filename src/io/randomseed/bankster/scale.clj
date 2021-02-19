(ns io.randomseed.bankster.scale

  ^{:doc    "Bankster library, scalable protocol with implementation."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:import [java.math MathContext RoundingMode]))

;;
;; Initial math context.
;;

(def ^MathContext unscaled-context
  (MathContext. 0 RoundingMode/UNNECESSARY))

;;
;; Constants for denoting auto-scaling.
;;

(def ^:const  auto nil)
(def ^Boolean auto? nil?)

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
  "Scales the given number using the given scale and an optional rounding mode. For
  numbers and strings it returns BigDecimal instance. If the scale argument is
  missing, returns a scale. If there are 2 arguments and the scale argument is set to
  nil (or scale/auto), it just converts the number to a BigDecimal with a scale
  determined automatically."
  (of [num] [num scale] [num scale rounding-mode]))

(extend-protocol Scalable
  BigDecimal

  (of
    (^int [num]
     (.scale ^BigDecimal num))
    (^BigDecimal [num scale]
     (if (or (nil? scale) (= (.scale ^BigDecimal num) scale))
       ^BigDecimal num
       (.setScale ^BigDecimal num scale)))
    (^BigDecimal [num scale r]
     (if (= (.scale ^BigDecimal num) scale)
       ^BigDecimal num
       (.setScale ^BigDecimal num scale r))))

  clojure.lang.BigInt

  (of
    (^int [num]
     (.scale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num)))
    (^BigDecimal [num scale]
     (if (nil? scale)
       (.toBigDecimal ^clojure.lang.BigInt num)
       (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) scale)))
    (^BigDecimal [num scale r]
     (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) scale r)))

  BigInteger

  (of
    (^int [num]
     (.scale ^BigDecimal (BigDecimal. ^BigInteger num)))
    (^BigDecimal [num scale]
     (if (nil? scale)
       (BigDecimal. ^BigInteger num)
       (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) scale)))
    (^BigDecimal [num scale r]
     (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) scale r)))

  Double

  (of
    (^int [num]
     (.scale ^BigDecimal (BigDecimal/valueOf num)))
    (^BigDecimal [num scale]
     (if (nil? scale)
       (BigDecimal/valueOf num)
       (.setScale ^BigDecimal (BigDecimal/valueOf num) scale)))
    (^BigDecimal [num scale r]
     (.setScale ^BigDecimal (BigDecimal/valueOf num) scale r)))

  Float

  (of
    (^int [num]
     (.scale ^BigDecimal (BigDecimal/valueOf (double num))))
    (^BigDecimal [num scale]
     (if (nil? scale)
       (BigDecimal/valueOf (double num))
       (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) scale)))
    (^BigDecimal [num scale r] (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) scale r)))

  Number

  (of
    (^int [num]
     (.scale ^BigDecimal (BigDecimal/valueOf (long num))))
    (^BigDecimal [num scale]
     (if (nil? scale)
       (BigDecimal/valueOf (long num))
       (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) scale)))
    (^BigDecimal [num scale r]
     (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) scale r)))

  clojure.lang.Ratio

  (of
    (^int [num]
     (.scale ^BigDecimal (/ (of (.numerator ^clojure.lang.Ratio num))
                            (of (.denominator ^clojure.lang.Ratio num)))))
    (^BigDecimal [num scale]
     (if (nil? scale)
       (/ (of (.numerator ^clojure.lang.Ratio num))
          (of (.denominator ^clojure.lang.Ratio num)))
       (/ (of (.numerator ^clojure.lang.Ratio num) scale)
          (of (.denominator ^clojure.lang.Ratio num) scale))))
    (^BigDecimal [num scale r]
     (/ (of (.numerator ^clojure.lang.Ratio num) scale r)
        (of (.denominator ^clojure.lang.Ratio num) scale r))))

  Object

  (of
    (^int [num]
     (.scale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context)))
    (^BigDecimal [num scale]
     (if (nil? scale)
       (BigDecimal. num ^MathContext unscaled-context)
       (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) scale)))
    (^BigDecimal [num scale r]
     (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) scale r))))
