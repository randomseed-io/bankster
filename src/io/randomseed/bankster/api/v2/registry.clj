(ns

    ^{:doc    "Bankster library, front API version 2 registry helpers."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.v2.registry

  (:refer-clojure :exclude [update])

  (:require [io.randomseed.bankster.util     :refer [auto-alias]]
            [io.randomseed.bankster.registry :as        registry])

  (:import (io.randomseed.bankster Registry)))

(def ^:private _auto-alias-loaded
  (do (auto-alias 'io.randomseed.bankster.api.registry) true))
