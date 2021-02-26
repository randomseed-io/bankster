(ns io.randomseed.bankster.money.inter-ops

  ^{:doc    "Bankster library, transparent wrappers for money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [> < >= <= + - * / = not= long compare])

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
  ([a b  & more] (if (money? a) (apply money/add a b more) (apply clojure.core/+ a b more))))

(defn -
  ([a]           (if (money? a) (money/sub a) (clojure.core/- a)))
  ([a b]         (if (money? a) (money/sub a b) (clojure.core/- a b)))
  ([a b & more]  (if (money? a) (apply money/sub a b more) (apply clojure.core/- a b more))))

(defn *
  ([]            (money/mul))
  ([a]           (if (money? a) (money/mul a) (num a)))
  ([a b]         (if (or (money? a) (money? b)) (money/mul a b) (clojure.core/* a b)))
  ([a b & more]  (apply money/mul a b more)))

(defn /
  ([a]           (if (money? a) (money/div a) (clojure.core// a)))
  ([a b]         (if (money? a) (money/div a b) (clojure.core// a b)))
  ([a b & more]  (if (money? a) (apply money/div a b more) (apply clojure.core// a b more))))

(defn =
  ([a]           true)
  ([a b]         (if (money? a) (if (money? b) (money/eq? a b) false) (if (money? b) false (clojure.core/= a b))))
  ([a b & more]  (if (= a b)
                   (if (next more)
                     (recur b (first more) (next more))
                     (= b (first more)))
                   false)))

(defn not=
  ([a]           false)
  ([a b]         (if (money? a) (if (money? b) (money/ne? a b) true) (if (money? b) true (clojure.core/not= a b))))
  ([a b & more]  (if (not= a b)
                   (if (next more)
                     (recur b (first more) (next more))
                     (not= b (first more)))
                   true)))

(defn >
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/gt? a b) false) (if (money? b) false (clojure.core/> a b))))
  (^Boolean [a b & more] (if (> a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (> b (first more)))
                           false)))

(defn >=
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/ge? a b) false) (if (money? b) false (clojure.core/>= a b))))
  (^Boolean [a b & more] (if (>= a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (>= b (first more)))
                           false)))

(defn <
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/lt? a b) false) (if (money? b) false (clojure.core/< a b))))
  (^Boolean [a b & more] (if (< a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (< b (first more)))
                           false)))

(defn <=
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/le? a b) false) (if (money? b) false (clojure.core/<= a b))))
  (^Boolean [a b & more] (if (<= a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (<= b (first more)))
                           false)))

(defn long
  {:added "1.0.0"}
  [a] (if (money? a) (money/major->long a) (clojure.core/long a)))
