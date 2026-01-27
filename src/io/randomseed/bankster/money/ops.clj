(ns io.randomseed.bankster.money.ops

  ^{:doc    "Bankster library, wrappers for money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [+ - * / = not= int long double float > < >= <= compare pos? neg?])

  (:require [io.randomseed.bankster]
            [io.randomseed.bankster.scale :as scale]
            [io.randomseed.bankster.money :as money])

  (:import  (io.randomseed.bankster Money)
            (java.math MathContext RoundingMode)))

(def ^{:tag io.randomseed.bankster.Money :added "1.0.0"
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  +
  "Alias for `io.randomseed.bankster.money/add`."
  money/add)

(def ^{:tag io.randomseed.bankster.Money :added "1.0.0"
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  -
  "Alias for `io.randomseed.bankster.money/subtract`."
  money/sub)

(def ^{:tag io.randomseed.bankster.Money :added "1.0.0"
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [a b]
                   ^Money [a b & more])}
  *
  "Alias for `io.randomseed.bankster.money/multiply`."
  money/mul)

(def ^{:tag io.randomseed.bankster.Money :added "1.0.0"
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  /
  "Alias for `io.randomseed.bankster.money/divide`."
  money/div)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean []
                   ^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  =
  "Alias for `io.randomseed.bankster.money/eq?`."
  money/eq?)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean []
                   ^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  not=
  "Alias for `io.randomseed.bankster.money/ne?`."
  money/ne?)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  >
  "Alias for `io.randomseed.bankster.money/gt?`."
  money/gt?)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  >=
  "Alias for `io.randomseed.bankster.money/ge?`."
  money/ge?)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  <
  "Alias for `io.randomseed.bankster.money/lt?`."
  money/lt?)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  <=
  "Alias for `io.randomseed.bankster.money/le?`."
  money/le?)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean [^Money a])}
  pos?
  "Alias for `io.randomseed.bankster.money/is-pos?`."
  money/is-pos?)

(def ^{:tag Boolean :added "1.0.0"
       :arglists '(^Boolean [^Money a])}
  neg?
  "Alias for `io.randomseed.bankster.money/is-neg?`."
  money/is-neg?)

(def ^{:tag BigDecimal :added "1.0.0"
       :arglists '(^BigDecimal [^Money a]
                   ^BigDecimal [^Money a scale]
                   ^BigDecimal [^Money a scale ^RoundingMode rounding-mode])}
  integer
  "Alias for `io.randomseed.bankster.scale/integer`."
  scale/integer)

(def ^{:tag BigDecimal :added "1.0.0"
       :arglists '(^BigDecimal [^Money a]
                   ^BigDecimal [^Money a scale]
                   ^BigDecimal [^Money a scale ^RoundingMode rounding-mode])}
  fractional
  "Alias for `io.randomseed.bankster.scale/fractional`."
  scale/fractional)

(def ^{:tag BigDecimal :added "1.0.0"
       :arglists '(^BigDecimal [^Money a])}
  major
  "Alias for `io.randomseed.bankster.money/major`."
  money/major)

(def ^{:tag BigDecimal :added "1.0.0"
       :arglists '(^BigDecimal [^Money a])}
  minor
  "Alias for `io.randomseed.bankster.money/minor`."
  money/minor)

(def ^{:tag      'long :added "1.0.0"
       :arglists '(^long [^Money a ^Money b])}
  compare
  "Alias for `io.randomseed.bankster.money/compare`."
  money/compare)

(def ^{:tag      'long :added "1.0.0"
       :arglists '(^long [^Money a])}
  int
  "Alias for `io.randomseed.bankster.money/major->int`."
  money/major->int)

(def ^{:tag      'long :added "1.0.0"
       :arglists '(^long [^Money a])}
  long
  "Alias for io.randomseed.bankster.money/major->long."
  money/major->long)

(def ^{:tag 'double :added "1.0.0"
       :arglists '(^double [^Money a])}
  double
  "Alias for io.randomseed.bankster.money/->double."
  money/->double)

(def ^{:tag      'double :added "1.0.0"
       :arglists '(^double [^Money a])}
  float
  "Alias for `io.randomseed.bankster.money/->float`."
  money/->float)
