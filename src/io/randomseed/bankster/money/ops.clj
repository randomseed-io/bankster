(ns io.randomseed.bankster.money.ops

  ^{:doc    "Bankster library, wrappers for money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [+ - * / = not= long > < >= <= compare])

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
  money/sub)

(def ^{:tag Money
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [a b]
                   ^Money [a b & more])}
  *
  "Alias for io.randomseed.bankster.money/multiply."
  money/mul)

(def ^{:tag Money
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  /
  "Alias for io.randomseed.bankster.money/divide."
  money/div)

(def ^{:tag Boolean
       :arglists '(^Boolean []
                   ^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  =
  "Alias for io.randomseed.bankster.money/eq?."
  money/eq?)

(def ^{:tag Boolean
       :arglists '(^Boolean []
                   ^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  not=
  "Alias for io.randomseed.bankster.money/ne?."
  money/ne?)

(def ^{:tag Boolean
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  >
  "Alias for io.randomseed.bankster.money/gt?."
  money/gt?)

(def ^{:tag Boolean
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  >=
  "Alias for io.randomseed.bankster.money/ge?."
  money/ge?)

(def ^{:tag Boolean
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  <
  "Alias for io.randomseed.bankster.money/lt?."
  money/lt?)

(def ^{:tag Boolean
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  <=
  "Alias for io.randomseed.bankster.money/le?."
  money/le?)
