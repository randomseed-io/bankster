(ns io.randomseed.bankster.money.ops

  ^{:doc    "Bankster library, wrappers for money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [+ - * / =])

  (:require [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.scale    :as         scale]
            [io.randomseed.bankster.currency :as      currency]
            [io.randomseed.bankster.money    :as         money]
            [io.randomseed.bankster.util     :refer       :all])

  (:import  [io.randomseed.bankster Currency Registry Money]
            [java.math MathContext RoundingMode]))

(def ^{:tag Money
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  +
  "Alias for io.randomseed.bankster.money/add."
  money/add)

(def ^{:tag Money
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  -
  "Alias for io.randomseed.bankster.money/subtract."
  money/subtract)

(def ^{:tag Money
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [a b]
                   ^Money [a b & more])}
  *
  "Alias for io.randomseed.bankster.money/multiply."
  money/multiply)

(def ^{:tag Money
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  /
  "Alias for io.randomseed.bankster.money/divide."
  money/divide)

(def ^{:tag Boolean
       :arglists '(^Boolean []
                   ^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  =
  "Alias for io.randomseed.bankster.money/equal?."
  money/equal?)
