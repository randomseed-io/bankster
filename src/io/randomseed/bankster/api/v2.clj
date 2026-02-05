;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Bankster library, front API version 2."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.v2

  (:require [io.randomseed.bankster.scale           :as        scale]
            [io.randomseed.bankster.money           :as        money]
            [io.randomseed.bankster.currency        :as     currency]
            [io.randomseed.bankster.registry        :as     registry]
            [io.randomseed.bankster.util            :as           bu])

  (:import  (io.randomseed.bankster Currency
                                    Registry
                                    Money)
            (java.math              BigDecimal)))

(def ^{:macro    true
       :auto-alias true
       :added    "2.2.0"
       :arglists (:arglists (meta #'io.randomseed.bankster.util/auto-alias))}
  auto-alias
  (deref #'io.randomseed.bankster.util/auto-alias))

(auto-alias 'io.randomseed.bankster.api)
