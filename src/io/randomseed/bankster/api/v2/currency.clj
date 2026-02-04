(ns

    ^{:doc    "Bankster library, front API version 2 currency helpers."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.v2.currency

  (:refer-clojure :exclude [resolve name symbol ns update])

  (:require [io.randomseed.bankster.util                :refer  [auto-alias]]
            [io.randomseed.bankster.api.v2.registry     :as     api-registry]
            [io.randomseed.bankster.currency            :as         currency]
            [io.randomseed.bankster.registry            :as         registry]
            [io.randomseed.bankster.scale               :as            scale]
            [io.randomseed.bankster.serializers.edn     :as  serializers-edn]
            [io.randomseed.bankster.serializers.json    :as serializers-json])

  (:import  (io.randomseed.bankster Currency
                                    Registry)))

(def ^:private _auto-alias-loaded
  (do (auto-alias 'io.randomseed.bankster.api.currency) true))
