(ns

    ^{:doc    "Bankster library, front API money-aware operators."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.ops

  (:refer-clojure :exclude [> < >= <= + - * / = not= long int double float compare pos? neg?])

  (:require [io.randomseed.bankster.money.inter-ops :as money-ops]))

(def ^{:arglists (:arglists (meta #'money-ops/+))
       :added    "2.2.0"}
  +
  "Alias for `io.randomseed.bankster.money.inter-ops/+`.

  Calls `io.randomseed.bankster.money/add` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/+`."
  money-ops/+)

(def ^{:arglists (:arglists (meta #'money-ops/-))
       :added    "2.2.0"}
  -
  "Alias for `io.randomseed.bankster.money.inter-ops/-`.

  Calls `io.randomseed.bankster.money/sub` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/-`."
  money-ops/-)

(def ^{:arglists (:arglists (meta #'money-ops/*))
       :added    "2.2.0"}
  *
  "Alias for `io.randomseed.bankster.money.inter-ops/*`.

  Calls `io.randomseed.bankster.money/mul` when any argument is a kind of Money,
  otherwise delegates to `clojure.core/*`."
  money-ops/*)

(def ^{:arglists (:arglists (meta #'money-ops//))
       :added    "2.2.0"}
  /
  "Alias for `io.randomseed.bankster.money.inter-ops//`.

  Calls `io.randomseed.bankster.money/div` when any argument is a kind of Money,
  otherwise delegates to `clojure.core//`."
  money-ops//)

(def ^{:arglists (:arglists (meta #'money-ops/=))
       :added    "2.2.0"}
  =
  "Alias for `io.randomseed.bankster.money.inter-ops/=`.

  Calls `io.randomseed.bankster.money/eq?` for Money argument, `clojure.core/=` for
  other data types."
  money-ops/=)

(def ^{:arglists (:arglists (meta #'money-ops/not=))
       :added    "2.2.0"}
  not=
  "Alias for `io.randomseed.bankster.money.inter-ops/not=`.

  Calls `io.randomseed.bankster.money/ne?` for Money argument, `clojure.core/not=`
  for other data types."
  money-ops/not=)

(def ^{:arglists (:arglists (meta #'money-ops/>))
       :added    "2.2.0"}
  >
  "Alias for `io.randomseed.bankster.money.inter-ops/>`.

  Calls `io.randomseed.bankster.money/gt?` for Money argument, `clojure.core/>` for
  other data types."
  money-ops/>)

(def ^{:arglists (:arglists (meta #'money-ops/>=))
       :added    "2.2.0"}
  >=
  "Alias for `io.randomseed.bankster.money.inter-ops/>=`.

  Calls `io.randomseed.bankster.money/ge?` for Money argument, `clojure.core/>=` for
  other data types."
  money-ops/>=)

(def ^{:arglists (:arglists (meta #'money-ops/<))
       :added    "2.2.0"}
  <
  "Alias for `io.randomseed.bankster.money.inter-ops/<`.

  Calls `io.randomseed.bankster.money/lt?` for Money argument, `clojure.core/<` for
  other data types."
  money-ops/<)

(def ^{:arglists (:arglists (meta #'money-ops/<=))
       :added    "2.2.0"}
  <=
  "Alias for `io.randomseed.bankster.money.inter-ops/<=`.

  Calls `io.randomseed.bankster.money/le?` for Money argument, `clojure.core/<=` for
  other data types."
  money-ops/<=)

(def ^{:arglists (:arglists (meta #'money-ops/compare))
       :added    "2.2.0"}
  compare
  "Alias for `io.randomseed.bankster.money.inter-ops/compare`.

  Calls `io.randomseed.bankster.money/compare` for Money argument,
  `clojure.core/compare` for other data types."
  money-ops/compare)

(def ^{:arglists (:arglists (meta #'money-ops/pos?))
       :added    "2.2.0"}
  pos?
  "Alias for `io.randomseed.bankster.money.inter-ops/pos?`.

  Calls `io.randomseed.bankster.money/is-pos?` for Money argument,
  `clojure.core/pos?` for other data types."
  money-ops/pos?)

(def ^{:arglists (:arglists (meta #'money-ops/neg?))
       :added    "2.2.0"}
  neg?
  "Alias for `io.randomseed.bankster.money.inter-ops/neg?`.

  Calls `io.randomseed.bankster.money/is-neg?` for Money argument,
  `clojure.core/neg?` for other data types."
  money-ops/neg?)

(def ^{:arglists (:arglists (meta #'money-ops/long))
       :added    "2.2.0"}
  long
  "Alias for `io.randomseed.bankster.money.inter-ops/long`.

  Calls `io.randomseed.bankster.money/major->long` for Money argument, falls back to
  `clojure.core/long` for other data types."
  money-ops/long)

(def ^{:arglists (:arglists (meta #'money-ops/int))
       :added    "2.2.0"}
  int
  "Alias for `io.randomseed.bankster.money.inter-ops/int`.

  Calls `io.randomseed.bankster.money/major->int` for Money argument, falls back to
  `clojure.core/int` for other data types."
  money-ops/int)

(def ^{:arglists (:arglists (meta #'money-ops/double))
       :added    "2.2.0"}
  double
  "Alias for `io.randomseed.bankster.money.inter-ops/double`.

  Calls `io.randomseed.bankster.money/->double` for Money argument, falls back to
  `clojure.core/double` for other data types."
  money-ops/double)

(def ^{:arglists (:arglists (meta #'money-ops/float))
       :added    "2.2.0"}
  float
  "Alias for `io.randomseed.bankster.money.inter-ops/float`.

  Calls `io.randomseed.bankster.money/->float` for Money argument, falls back to
  `clojure.core/float` for other data types."
  money-ops/float)

(doseq [[_ v] (ns-interns *ns*)]
  (alter-meta! v assoc :auto-alias true))
