(ns

    ^{:doc    "Bankster library, front version 2 API money helpers."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.v2.money

  (:refer-clojure :exclude [* + - / < <= = == > >= abs apply max min not= rem
                            resolve cast format compare pos? neg? zero?])

  (:require [io.randomseed.bankster.util                :refer  [auto-alias]]
            [io.randomseed.bankster.api.v2.registry     :as     api-registry]
            [io.randomseed.bankster.api.v2.currency     :as     api-currency]
            [io.randomseed.bankster.currency            :as         currency]
            [io.randomseed.bankster.registry            :as         registry]
            [io.randomseed.bankster.money               :as            money]
            [io.randomseed.bankster.scale               :as            scale]
            [io.randomseed.bankster.serializers.edn     :as  serializers-edn]
            [io.randomseed.bankster.serializers.json    :as serializers-json])

  (:import  (io.randomseed.bankster Currency
                                    Registry
                                    Money)
            (java.math              BigDecimal)))

(auto-alias 'io.randomseed.bankster.api.money)
