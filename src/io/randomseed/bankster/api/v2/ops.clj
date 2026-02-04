(ns

    ^{:doc    "Bankster library, front API version 2 money-aware operators."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.v2.ops

  (:refer-clojure :exclude [> < >= <= + - * / = not= long int double float compare pos? neg?])

  (:require [io.randomseed.bankster.util    :refer [auto-alias]]
            [io.randomseed.bankster.api.ops :as        api-ops]))

(def ^:private _auto-alias-loaded
  (do (auto-alias 'io.randomseed.bankster.api.ops) true))
