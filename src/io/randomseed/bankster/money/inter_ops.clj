(ns

    ^{:doc    "Bankster library, transparent wrappers for money operations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.money.inter-ops

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
  "Calls `io.randomseed.bankster.money/add` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/+`."
  {:added "1.0.0"}
  ([]            0)
  ([a]           (if (money? a) a (clojure.core/+ 0 a)))
  ([a b]         (if (or (money? a) (money? b)) (money/add a b) (clojure.core/+ a b)))
  ([a b  & more] (if (or (money? a) (money? b) (some money? more))
                   (apply money/add a b more)
                   (apply clojure.core/+ a b more))))

(defn -
  "Calls `io.randomseed.bankster.money/sub` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/-`."
  {:added "1.0.0"}
  ([a]           (if (money? a) (money/sub a) (clojure.core/- a)))
  ([a b]         (if (or (money? a) (money? b)) (money/sub a b) (clojure.core/- a b)))
  ([a b & more]  (if (or (money? a) (money? b) (some money? more))
                   (apply money/sub a b more)
                   (apply clojure.core/- a b more))))

(defn *
  "Calls `io.randomseed.bankster.money/mul` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/*`."
  {:added "1.0.0"}
  ([]            1)
  ([a]           (if (money? a) a (clojure.core/* 1 a)))
  ([a b]         (if (or (money? a) (money? b)) (money/mul a b) (clojure.core/* a b)))
  ([a b & more]  (if (or (money? a) (money? b) (some money? more))
                   (apply money/mul a b more)
                   (apply clojure.core/* a b more))))

(defn /
  "Calls `io.randomseed.bankster.money/div` when any argument is a kind of Money,
  otherwise delegates to `clojure.core//`."
  {:added "1.0.0"}
  ([a]           (if (money? a) (money/div a) (clojure.core// a)))
  ([a b]         (if (or (money? a) (money? b)) (money/div a b) (clojure.core// a b)))
  ([a b & more]  (if (or (money? a) (money? b) (some money? more))
                   (apply money/div a b more)
                   (apply clojure.core// a b more))))

(defn =
  "Calls `io.randomseed.bankster.money/eq?` for Money argument, `clojure.core/=` for other
  data types."
  {:tag            Boolean
   :added          "1.0.0"
   :inline
   (fn [a b]
     `(let [a# ~a
            b# ~b]
        (if (io.randomseed.bankster.money/money? a#)
          (if (io.randomseed.bankster.money/money? b#)
            (io.randomseed.bankster.money/eq? a# b#)
            false)
          (if (io.randomseed.bankster.money/money? b#)
            false
            (clojure.core/= a# b#)))))
   :inline-arities #{2}}
  ([_]           true)
  ([a b]         (if (io.randomseed.bankster.money/money? a)
                   (if (io.randomseed.bankster.money/money? b)
                     (io.randomseed.bankster.money/eq? a b)
                     false)
                   (if (io.randomseed.bankster.money/money? b)
                     false
                     (clojure.core/= a b))))
  ([a b & more]  (if (= a b)
                   (if (next more)
                     (recur b (first more) (next more))
                     (= b (first more)))
                   false)))

(defn not=
  "Calls `io.randomseed.bankster.money/ne?` for Money argument, `clojure.core/not=` for
  other data types."
  {:tag            Boolean
   :added          "1.0.0"
   :inline
   (fn [a b]
     `(let [a# ~a
            b# ~b]
        (if (io.randomseed.bankster.money/money? a#)
          (if (io.randomseed.bankster.money/money? b#)
            (io.randomseed.bankster.money/ne? a# b#)
            true)
          (if (io.randomseed.bankster.money/money? b#)
            true
            (clojure.core/not= a# b#)))))
   :inline-arities #{2}}
  ([_]           false)
  ([a b]         (if (io.randomseed.bankster.money/money? a)
                   (if (io.randomseed.bankster.money/money? b)
                     (io.randomseed.bankster.money/ne? a b)
                     true)
                   (if (io.randomseed.bankster.money/money? b)
                     true
                     (clojure.core/not= a b))))
  ([a b & more]  (not (apply = a b more))))

(defn >
  "Calls `io.randomseed.bankster.money/gt?` for Money argument, `clojure.core/>` for other
  data types."
  {:tag            Boolean
   :added          "1.0.0"
   :inline
   (fn [a b]
     `(let [a# ~a
            b# ~b]
        (if (io.randomseed.bankster.money/money? a#)
          (if (io.randomseed.bankster.money/money? b#)
            (io.randomseed.bankster.money/gt? a# b#)
            false)
          (if (io.randomseed.bankster.money/money? b#)
            false
            (clojure.core/> a# b#)))))
   :inline-arities #{2}}
  (^Boolean [_]          true)
  (^Boolean [a b]        (if (io.randomseed.bankster.money/money? a)
                           (if (io.randomseed.bankster.money/money? b)
                             (io.randomseed.bankster.money/gt? a b)
                             false)
                           (if (io.randomseed.bankster.money/money? b)
                             false
                             (clojure.core/> a b))))
  (^Boolean [a b & more] (if (> a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (> b (first more)))
                           false)))

(defn >=
  "Calls `io.randomseed.bankster.money/ge?` for Money argument, `clojure.core/>=` for other
  data types."
  {:tag            Boolean
   :added          "1.0.0"
   :inline
   (fn [a b]
     `(let [a# ~a
            b# ~b]
        (if (io.randomseed.bankster.money/money? a#)
          (if (io.randomseed.bankster.money/money? b#)
            (io.randomseed.bankster.money/ge? a# b#)
            false)
          (if (io.randomseed.bankster.money/money? b#)
            false
            (clojure.core/>= a# b#)))))
   :inline-arities #{2}}
  (^Boolean [_]          true)
  (^Boolean [a b]        (if (io.randomseed.bankster.money/money? a)
                           (if (io.randomseed.bankster.money/money? b)
                             (io.randomseed.bankster.money/ge? a b)
                             false)
                           (if (io.randomseed.bankster.money/money? b)
                             false
                             (clojure.core/>= a b))))
  (^Boolean [a b & more] (if (>= a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (>= b (first more)))
                           false)))

(defn <
  "Calls `io.randomseed.bankster.money/lt?` for Money argument, `clojure.core/<` for other
  data types."
  {:tag            Boolean
   :added          "1.0.0"
   :inline
   (fn [a b]
     `(let [a# ~a
            b# ~b]
        (if (io.randomseed.bankster.money/money? a#)
          (if (io.randomseed.bankster.money/money? b#)
            (io.randomseed.bankster.money/lt? a# b#)
            false)
          (if (io.randomseed.bankster.money/money? b#)
            false
            (clojure.core/< a# b#)))))
   :inline-arities #{2}}
  (^Boolean [_]          true)
  (^Boolean [a b]        (if (io.randomseed.bankster.money/money? a)
                           (if (io.randomseed.bankster.money/money? b)
                             (io.randomseed.bankster.money/lt? a b)
                             false)
                           (if (io.randomseed.bankster.money/money? b)
                             false
                             (clojure.core/< a b))))
  (^Boolean [a b & more] (if (< a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (< b (first more)))
                           false)))

(defn <=
  "Calls `io.randomseed.bankster.money/le?` for Money argument, `clojure.core/<=` for other
  data types."
  {:tag            Boolean
   :added          "1.0.0"
   :inline
   (fn [a b]
     `(let [a# ~a
            b# ~b]
        (if (io.randomseed.bankster.money/money? a#)
          (if (io.randomseed.bankster.money/money? b#)
            (io.randomseed.bankster.money/le? a# b#)
            false)
          (if (io.randomseed.bankster.money/money? b#)
            false
            (clojure.core/<= a# b#)))))
   :inline-arities #{2}}
  (^Boolean [_]          true)
  (^Boolean [a b]        (if (io.randomseed.bankster.money/money? a)
                           (if (io.randomseed.bankster.money/money? b)
                             (io.randomseed.bankster.money/le? a b)
                             false)
                           (if (io.randomseed.bankster.money/money? b)
                             false
                             (clojure.core/<= a b))))
  (^Boolean [a b & more] (if (<= a b)
                           (if (next more)
                             (recur b (first more) (next more))
                             (<= b (first more)))
                           false)))

(defn long
  "Calls `io.randomseed.bankster.money/major->long` for Money argument, falls back to
  `clojure.core/long` for other data types."
  {:added          "1.0.0"
   :inline
   (fn [x]
     `(let [x# ~x]
        (if (instance? io.randomseed.bankster.Money x#)
          (clojure.core/long (io.randomseed.bankster.money/major->long x#))
          (clojure.core/long x#))))
   :inline-arities #{1}}
  ^long
  [x]
  (if (instance? io.randomseed.bankster.Money x)
    (clojure.core/long (io.randomseed.bankster.money/major->long x))
    (clojure.core/long x)))

(defn int
  "Calls `io.randomseed.bankster.money/major->int` for Money argument, falls back to
  `clojure.core/int` for other data types."
  {:added          "1.0.0"
   :inline
   (fn [x]
     `(let [x# ~x]
        (if (instance? io.randomseed.bankster.Money x#)
          (clojure.core/int (io.randomseed.bankster.money/major->int x#))
          (clojure.core/int x#))))
   :inline-arities #{1}}
  [x]
  (if (instance? io.randomseed.bankster.Money x)
    (clojure.core/int (io.randomseed.bankster.money/major->int x))
    (clojure.core/int x)))

(defn double
  "Calls `io.randomseed.bankster.money/->double` for Money argument, falls back to
  `clojure.core/double` for other data types."
  {:added          "1.0.0"
   :inline
   (fn [x]
     `(let [x# ~x]
        (if (instance? Money x#)
          (clojure.core/double (io.randomseed.bankster.money/->double x#))
          (clojure.core/double x#))))
   :inline-arities #{1}}
  ^double
  [x]
  (if (instance? io.randomseed.bankster.Money x)
    (clojure.core/double (io.randomseed.bankster.money/->double x))
    (clojure.core/double x)))

(defn float
  "Calls `io.randomseed.bankster.money/->float` for Money argument, falls back to
  `clojure.core/float` for other data types."
  {:added          "1.0.0"
   :inline
   (fn [x]
     `(let [x# ~x]
        (if (instance? io.randomseed.bankster.Money x#)
          (clojure.core/float (io.randomseed.bankster.money/->float x#))
          (clojure.core/float x#))))
   :inline-arities #{1}}
  [x]
  (if (instance? io.randomseed.bankster.Money x)
    (clojure.core/float (io.randomseed.bankster.money/->float x))
    (clojure.core/float x)))

(defn pos?
  "Calls `io.randomseed.bankster.money/is-pos?` for Money argument, `clojure.core/pos?`
  for other data types."
  {:added          "1.0.0"
   :inline
   (fn [a]
     `(let [a# ~a]
        (if (io.randomseed.bankster.money/money? a#)
          (io.randomseed.bankster.money/is-pos? a#)
          (clojure.core/pos? a#))))
   :inline-arities #{1}}
  [a]
  (if (io.randomseed.bankster.money/money? a)
    (io.randomseed.bankster.money/is-pos? a)
    (clojure.core/pos? a)))

(defn neg?
  "Calls `io.randomseed.bankster.money/is-neg?` for Money argument, `clojure.core/neg?`
  for other data types."
  {:added          "1.0.0"
   :inline
   (fn [a]
     `(let [a# ~a]
        (if (io.randomseed.bankster.money/money? a#)
          (io.randomseed.bankster.money/is-neg? a#)
          (clojure.core/neg? a#))))
   :inline-arities #{1}}
  [a]
  (if (io.randomseed.bankster.money/money? a)
    (io.randomseed.bankster.money/is-neg? a)
    (clojure.core/neg? a)))

(def ^{:tag      BigDecimal
       :arglists '(^BigDecimal [^Money a]
                   ^BigDecimal [^Money a scale]
                   ^BigDecimal [^Money a scale ^RoundingMode rounding-mode])}
  integer
  "Alias for `io.randomseed.bankster.scale/integer`."
  scale/integer)

(def ^{:tag      BigDecimal
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
  {:added          "1.0.0"
   :inline
   (fn [a b]
     `(let [a# ~a
            b# ~b]
        (if (io.randomseed.bankster.money/money? a#)
          (io.randomseed.bankster.money/compare a# b#)
          (clojure.core/compare a# b#))))
   :inline-arities #{2}}
  [a b]
  (if (io.randomseed.bankster.money/money? a)
    (io.randomseed.bankster.money/compare a b)
    (clojure.core/compare a b)))
