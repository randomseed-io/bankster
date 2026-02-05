;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Bankster library, front API version 2 registry helpers."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.v2.registry

  (:refer-clojure :exclude [update])

  (:require [io.randomseed.bankster.util     :refer [auto-alias]]
            [io.randomseed.bankster.registry :as        registry])

  (:import (io.randomseed.bankster Registry)))

(auto-alias 'io.randomseed.bankster.api.registry)
