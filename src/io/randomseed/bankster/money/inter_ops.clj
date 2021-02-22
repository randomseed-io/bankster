(ns io.randomseed.bankster.money.inter-ops

  ^{:doc    "Bankster library, transparent wrappers for money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [+ - * / =])

  (:require [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.scale    :as         scale]
            [io.randomseed.bankster.currency :as      currency]
            [io.randomseed.bankster.money    :as         money]
            [io.randomseed.bankster.money    :refer   [money?]]
            [io.randomseed.bankster.util     :refer       :all])

  (:import  [io.randomseed.bankster Currency Registry Money]
            [java.math MathContext RoundingMode]))

(defn +
  ([]            (money/add))
  ([a]           a)
  ([a b]         (if (money? a) (money/add a b) (clojure.core/+ a b)))
  ([a b  & more] (if (money? a)
                   (apply money/add a b more)
                   (apply clojure.core/+ a b more))))

(defn -
  ([a]           (if (money? a) (money/subtract a) (clojure.core/- a)))
  ([a b]         (if (money? a) (money/subtract a b) (clojure.core/- a b)))
  ([a b & more]  (if (money? a)
                   (apply money/subtract a b more)
                   (apply clojure.core/- a b more))))

(defn *
  ([]            (money/multiply))
  ([a]           (if (money? a)     (money/multiply a) (clojure.core/* a)))
  ([a b]         (if (or (money? a) (money? b)) (money/multiply a b) (clojure.core/* a b)))
  ([a b & more]  (if (or (money? a) (money? b) (some money? more))
                   (apply money/multiply a b more)
                   (apply clojure.core/* a b more))))

(defn /
  ([a]           (if (money? a) (money/divide a) (clojure.core// a)))
  ([a b]         (if (money? a) (money/divide a b) (clojure.core// a b)))
  ([a b & more]  (if (money? a)
                   (apply money/divide a b more)
                   (apply clojure.core// a b more))))

(defn =
  ([a]           true)
  ([a b]         (if (or (money? a) (money? b)) (money/equal? a b) (clojure.core/= a b)))
  ([a b & more]  (if (or (money? a) (money? b) (some money? more))
                   (apply money/equal? a b more)
                   (apply clojure.core/= a b more))))
