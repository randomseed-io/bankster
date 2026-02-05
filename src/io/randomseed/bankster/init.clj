;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Bankster library, registry initialization helpers."
      :author "Paweł Wilk"
      :added  "2.0.0"}

    io.randomseed.bankster.init

  (:require [io.randomseed.bankster.config        :as     config]
            [io.randomseed.bankster.currency      :as   currency]
            [io.randomseed.bankster.registry      :as   registry]
            [io.randomseed.bankster.util.importer :as  importer]
            [io.randomseed.bankster.util.fs       :as        fs])

  (:import  (io.randomseed.bankster Registry)))

(defn- ensure-resource!
  "Throws when the given resource path cannot be found on classpath.

  When `optional?` is truthy, returns nil instead."
  {:tag java.net.URL :added "2.0.0" :private true}
  [^String resource-path optional?]
  (or (when (some? resource-path)
        (fs/paths->resource resource-path))
      (when-not optional?
        (throw
         (ex-info
          (str "Registry config resource not found: " resource-path)
          {:resource-path resource-path})))))

(defn load-registry
  "Loads a registry from an EDN config resource file (classpath path).

  By default it loads ONLY the provided `resource-path` (no overlay from Bankster's
  distribution config).

  Options map:

  - `:keep-dist?`         - when truthy, loads the distribution registry first and
                            overlays it with the user registry using
                            `io.randomseed.bankster.util.importer/merge-registry`.
                            Default: false.
  - `:dist-resource-path` - distribution resource path. Default:
                            `io.randomseed.bankster.config/default-resource-path`.
  - `:optional?`          - when truthy, missing `resource-path` is treated as
                            \"no overlay\" (returns dist registry when keep-dist?,
                            or an empty registry when not). Default: false.
  - `:merge-opts`         - a map of options for `importer/merge-registry`:
                            - `:verbose?` (boolean),
                            - `:preserve-fields` (seq),
                            - `:iso-like?` (boolean).

  Returns a `Registry`."
  {:tag Registry :added "2.0.0"}
  (^Registry [^String resource-path]
   (load-registry resource-path nil))
  (^Registry [^String resource-path opts]
   (let [opts              (or opts {})
         keep-dist?        (boolean (:keep-dist? opts))
         dist-resource-path (or (:dist-resource-path opts) config/default-resource-path)
         optional?         (boolean (:optional? opts))
         merge-opts        (or (:merge-opts opts) {})
         verbose?          (boolean (:verbose? merge-opts))
         preserve-fields   (:preserve-fields merge-opts)
         iso-like?         (boolean (:iso-like? merge-opts))
         _dist-url         (when keep-dist? (ensure-resource! dist-resource-path false))
         _user-url         (when (some? resource-path) (ensure-resource! resource-path optional?))
         dist-reg          (when keep-dist?
                             (currency/config->registry dist-resource-path (registry/new-registry)))
         user-reg          (when (and (some? resource-path)
                                      (fs/paths->resource resource-path))
                             (currency/config->registry resource-path (registry/new-registry)))]
     (cond
       keep-dist?
       (if (some? user-reg)
         (importer/merge-registry dist-reg user-reg verbose? preserve-fields iso-like?)
         dist-reg)

       (some? user-reg)
       user-reg

       :else
       (registry/new-registry)))))

(defn load-registry!
  "Like `load-registry`, but installs the result as the global registry."
  {:tag Registry :added "2.0.0"}
  (^Registry [^String resource-path]
   (load-registry! resource-path nil))
  (^Registry [^String resource-path opts]
   (registry/set! (load-registry resource-path opts))))

