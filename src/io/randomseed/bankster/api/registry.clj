(ns

    ^{:doc    "Bankster library, front API registry helpers."
      :author "Paweł Wilk"
      :added  "2.2.0"}

    io.randomseed.bankster.api.registry

  (:refer-clojure :exclude [update])

  (:require [io.randomseed.bankster.util     :as       bu]
            [io.randomseed.bankster.registry :as registry])

  (:import (io.randomseed.bankster Registry)))

(defn default
  "Returns the default registry (honors `io.randomseed.bankster.registry/*default*`)."
  {:tag Registry :added "2.2.0"}
  []
  (registry/get))

(defn or-default
  "Resolves `true` or `nil` into the current default registry, otherwise returns the
  given value."
  {:tag Registry :added "2.2.0"}
  [registry]
  (if (or (nil? registry) (true? registry)) (registry/get) registry))

(defn hierarchy-derive
  "Alias for `io.randomseed.bankster.registry/hierarchy-derive`.

  Returns `registry` updated by deriving `tag` from `parent` inside a hierarchy
  identified by `hierarchy-name`."
  {:tag Registry :added "2.2.0"}
  [hierarchy-name tag parent registry]
  (if (true? registry)
    (registry/hierarchy-derive hierarchy-name tag parent (registry/get))
    (registry/hierarchy-derive hierarchy-name tag parent registry)))

(bu/defalias with              io.randomseed.bankster.registry/with)
(bu/defalias state             io.randomseed.bankster.registry/state)
(bu/defalias hierarchy-derive! io.randomseed.bankster.registry/hierarchy-derive!)

(bu/auto-alias 'io.randomseed.bankster.registry)

(doseq [[_ v] (ns-interns *ns*)]
  (alter-meta! v assoc :auto-alias true))
