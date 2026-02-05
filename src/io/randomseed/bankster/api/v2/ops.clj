;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Bankster library, front API version 2 money-aware operators."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.v2.ops

  (:refer-clojure :exclude [> < >= <= + - * / = not= long int double float compare pos? neg?])

  (:require [io.randomseed.bankster.util    :refer [auto-alias]]
            [io.randomseed.bankster.api.ops :as        api-ops]))

(auto-alias 'io.randomseed.bankster.api.ops)
