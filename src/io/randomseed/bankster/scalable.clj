(ns io.randomseed.bankster.scalable

  ^{:doc    "Bankster library, scalable protocol with implementation."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [clojure.string :as str])

  (:import [java.math MathContext RoundingMode]))

;;
;; Initial math context.
;;

(def ^MathContext unscaled-context
  (MathContext. 0 RoundingMode/UNNECESSARY))

;;
;; Scalable protocol.
;;

(defprotocol Scalable
  (of [num] [num scale] [num scale rounding-mode]))

(extend-protocol Scalable
  BigDecimal

  (of
    (^BigDecimal [num] num)
    (^BigDecimal [num scale]   (if (= (.scale ^BigDecimal num) scale) ^BigDecimal num (.setScale ^BigDecimal num scale)))
    (^BigDecimal [num scale r] (if (= (.scale ^BigDecimal num) scale) ^BigDecimal num (.setScale ^BigDecimal num scale r))))

  clojure.lang.BigInt

  (of
    (^BigDecimal [num]         (.toBigDecimal ^clojure.lang.BigInt num))
    (^BigDecimal [num scale]   (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) scale))
    (^BigDecimal [num scale r] (.setScale ^BigDecimal (.toBigDecimal ^clojure.lang.BigInt num) scale r)))

  BigInteger

  (of
    (^BigDecimal [num]         (BigDecimal. ^BigInteger num))
    (^BigDecimal [num scale]   (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) scale))
    (^BigDecimal [num scale r] (.setScale ^BigDecimal (BigDecimal. ^BigInteger num) scale r)))

  Double

  (of
    (^BigDecimal [num]         (BigDecimal/valueOf num))
    (^BigDecimal [num scale]   (.setScale ^BigDecimal (BigDecimal/valueOf num) scale))
    (^BigDecimal [num scale r] (.setScale ^BigDecimal (BigDecimal/valueOf num) scale r)))

  Float

  (of
    (^BigDecimal [num]         (BigDecimal/valueOf (double num)))
    (^BigDecimal [num scale]   (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) scale))
    (^BigDecimal [num scale r] (.setScale ^BigDecimal (BigDecimal/valueOf (double num)) scale r)))

  Number

  (of
    (^BigDecimal [num]         (BigDecimal/valueOf (long num)))
    (^BigDecimal [num scale]   (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) scale))
    (^BigDecimal [num scale r] (.setScale ^BigDecimal (BigDecimal/valueOf (long num)) scale r)))

  clojure.lang.Ratio

  (of
    (^BigDecimal [num]         (/ (of (.numerator ^clojure.lang.Ratio num))         (of (.denominator ^clojure.lang.Ratio num))))
    (^BigDecimal [num scale]   (/ (of (.numerator ^clojure.lang.Ratio num) scale)   (of (.denominator ^clojure.lang.Ratio num) scale)))
    (^BigDecimal [num scale r] (/ (of (.numerator ^clojure.lang.Ratio num) scale r) (of (.denominator ^clojure.lang.Ratio num) scale r))))

  Object

  (of
    (^BigDecimal [num]         (BigDecimal. num ^MathContext unscaled-context))
    (^BigDecimal [num scale]   (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) scale))
    (^BigDecimal [num scale r] (.setScale ^BigDecimal (BigDecimal. num ^MathContext unscaled-context) scale r))))
