(ns io.randomseed.bankster.money.inter-ops

  ^{:doc    "Bankster library, transparent wrappers for money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [> < >= <= + - * / = not= long int double float compare pos? neg?])

  (:require [io.randomseed.bankster]
            [io.randomseed.bankster.scale :as scale]
            [io.randomseed.bankster.money :as money :refer [money?]])

  (:import  (io.randomseed.bankster Currency
                                    Registry
                                    Money)
            (java.math              MathContext
                                    RoundingMode)))

(defn +
  "Calls `io.randomseed.bankster.money/add` for Money argument, `clojure.core/+` for other
  data types."
  {:added "1.0.0"}
  ([]            (money/add))
  ([a]           a)
  ([a b]         (if (money? a) (money/add a b) (clojure.core/+ a b)))
  ([a b  & more] (if (money? a) (apply money/add a b more) (apply clojure.core/+ a b more))))

(defn -
  "Calls `io.randomseed.bankster.money/sub` for Money argument, `clojure.core/-` for other
  data types."
  {:added "1.0.0"}
  ([a]           (if (money? a) (money/sub a) (clojure.core/- a)))
  ([a b]         (if (money? a) (money/sub a b) (clojure.core/- a b)))
  ([a b & more]  (if (money? a) (apply money/sub a b more) (apply clojure.core/- a b more))))

(defn *
  "Calls `io.randomseed.bankster.money/mul` for Money argument, `clojure.core/*` for other
  data types."
  {:added "1.0.0"}
  ([]            (money/mul))
  ([a]           (if (money? a) (money/mul a) (num a)))
  ([a b]         (if (or (money? a) (money? b)) (money/mul a b) (clojure.core/* a b)))
  ([a b & more]  (apply money/mul a b more)))

(defn /
  "Calls `io.randomseed.bankster.money/div` for Money argument, `clojure.core//` for other
  data types."
  {:added "1.0.0"}
  ([a]           (if (money? a) (money/div a) (clojure.core// a)))
  ([a b]         (if (money? a) (money/div a b) (clojure.core// a b)))
  ([a b & more]  (if (money? a) (apply money/div a b more) (apply clojure.core// a b more))))

(defn =
  "Calls `io.randomseed.bankster.money/eq?` for Money argument, `clojure.core/=` for other
  data types."
  {:tag Boolean :added "1.0.0"}
  ([a]           true)
  ([a b]         (if (money? a) (if (money? b) (money/eq? a b) false) (if (money? b) false (clojure.core/= a b))))
  ([a b & more]  (if (= a b)
                   (if (next more)
                     (recur b (first more) (next more))
                     (= b (first more)))
                   false)))

(defn not=
  "Calls `io.randomseed.bankster.money/ne?` for Money argument, `clojure.core/not=` for
  other data types."
  {:tag Boolean :added "1.0.0"}
  ([a]           false)
  ([a b]         (if (money? a) (if (money? b) (money/ne? a b) true) (if (money? b) true (clojure.core/not= a b))))
  ([a b & more]  (if (not= a b)
                   (if (next more)
                     (recur b (first more) (next more))
                     (not= b (first more)))
                   true)))

(defn >
  "Calls `io.randomseed.bankster.money/gt?` for Money argument, `clojure.core/>` for other
  data types."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/gt? a b) false) (if (money? b) false (clojure.core/> a b))))
  (^Boolean [a b & more] (if (> a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (> b (first more)))
                           false)))

(defn >=
  "Calls `io.randomseed.bankster.money/ge?` for Money argument, `clojure.core/>=` for other
  data types."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/ge? a b) false) (if (money? b) false (clojure.core/>= a b))))
  (^Boolean [a b & more] (if (>= a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (>= b (first more)))
                           false)))

(defn <
  "Calls `io.randomseed.bankster.money/lt?` for Money argument, `clojure.core/<` for other
  data types."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/lt? a b) false) (if (money? b) false (clojure.core/< a b))))
  (^Boolean [a b & more] (if (< a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (< b (first more)))
                           false)))

(defn <=
  "Calls `io.randomseed.bankster.money/le?` for Money argument, `clojure.core/<=` for other
  data types."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [a]          true)
  (^Boolean [a b]        (if (money? a) (if (money? b) (money/le? a b) false) (if (money? b) false (clojure.core/<= a b))))
  (^Boolean [a b & more] (if (<= a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (<= b (first more)))
                           false)))

(defn long
  "Calls io.randomseed.bankster.money/major->long for Money argument, clojure.core/long
  for other data types."
  {:tag 'long :added "1.0.0"}
  [a] (if (money? a) (money/major->long a) (clojure.core/long a)))

(defn int
  "Calls io.randomseed.bankster.money/major->int for Money argument, clojure.core/int
  for other data types."
  {:tag 'int :added "1.0.0"}
  [a] (if (money? a) (money/major->int a) (clojure.core/int a)))

(defn double
  "Calls io.randomseed.bankster.money/->double for Money argument, clojure.core/double
  for other data types."
  {:tag 'double :added "1.0.0"}
  [a] (if (money? a) (money/->double a) (clojure.core/double a)))

(defn float
  "Calls io.randomseed.bankster.money/->float for Money argument, clojure.core/float
  for other data types."
  {:tag 'float :added "1.0.0"}
  [a] (if (money? a) (money/->float a) (clojure.core/float a)))

(defn pos?
  "Calls `io.randomseed.bankster.money/is-pos?` for Money argument, `clojure.core/pos?`
  for other data types."
  {:tag Boolean :added "1.0.0"}
  [a] (if (money? a) (money/is-pos? a) (clojure.core/pos? a)))

(defn neg?
  "Calls `io.randomseed.bankster.money/is-neg?` for Money argument, `clojure.core/neg?`
  for other data types."
  {:tag Boolean :added "1.0.0"}
  [a] (if (money? a) (money/is-neg? a) (clojure.core/neg? a)))

(def ^{:tag BigDecimal
       :arglists '(^BigDecimal [^Money a]
                   ^BigDecimal [^Money a scale]
                   ^BigDecimal [^Money a scale ^RoundingMode rounding-mode])}
  integer
  "Alias for `io.randomseed.bankster.scale/integer`."
  scale/integer)

(def ^{:tag BigDecimal
       :arglists '(^BigDecimal [^Money a]
                   ^BigDecimal [^Money a scale]
                   ^BigDecimal [^Money a scale ^RoundingMode rounding-mode])}
  fractional
  "Alias for `io.randomseed.bankster.scale/fractional`."
  scale/fractional)

(def ^{:tag BigDecimal
       :arglists '(^BigDecimal [^Money a])}
  major
  "Alias for `io.randomseed.bankster.money/major`."
  money/major)

(def ^{:tag BigDecimal
       :arglists '(^BigDecimal [^Money a])}
  minor
  "Alias for `io.randomseed.bankster.money/minor`."
  money/minor)

(defn compare
  "Calls `io.randomseed.bankster.money/compare` for Money argument, `clojure.core/compare`
  for other data types."
  {:tag 'int :added "1.0.0"}
  [a b] (if (money? a) (money/compare a b) (clojure.core/compare a b)))
