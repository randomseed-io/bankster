(ns

    ^{:doc    "Bankster library, configuration handling."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.config

  (:refer-clojure :exclude [load])

  (:require [clojure.edn                    :as      edn]
            [io.randomseed.bankster.util.fs :as       fs]
            [io.randomseed.bankster])

  (:import  (io.randomseed.bankster Currency Registry)))

;;
;; Default configuration filename.
;;

(def ^{:tag String :const true}
  default-resource-path
  "Default registry resource file – relative path."
  "io/randomseed/bankster/config.edn")

(def ^{:tag String :const true}
  user-resource-path
  "Additional registry resource file – relative path."
  "META-INF/io/randomseed/bankster/currencies.edn")

;;
;; Config file reader.
;;

(declare add-currency-inline-data)

(defn load
  "Loads data structures from an EDN file. The given path should reside in one of the
  resource directories. If it is not given, the default-resource-path will be used."
  {:tag clojure.lang.PersistentHashMap :added "1.0.0"}
  (^clojure.lang.PersistentHashMap []
   (load default-resource-path))
  (^clojure.lang.PersistentHashMap [^String resource-path]
   (when-some [^java.net.URL r (fs/paths->resource resource-path)]
     (when-some [config (edn/read-string (slurp r))]
       (when (and (map? config) (pos? (count config)))
         (add-currency-inline-data config))))))

;;
;; Getters.
;;

(defn currencies
  "Returns currencies map of the given configuration map."
  {:tag clojure.lang.PersistentHashMap :added "1.0.0"}
  ^clojure.lang.PersistentHashMap [cfg] (get cfg :currencies {}))

(defn countries
  "Returns countries map of the given configuration map."
  {:tag clojure.lang.PersistentHashMap :added "1.0.0"}
  ^clojure.lang.PersistentHashMap [cfg] (get cfg :countries {}))

(defn localized
  "Returns localized properties map of the given configuration map."
  {:tag clojure.lang.PersistentHashMap :added "1.0.0"}
  ^clojure.lang.PersistentHashMap [cfg] (get cfg :localized {}))

(defn traits
  "Returns currency traits map of the given configuration map."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  ^clojure.lang.PersistentHashMap [cfg] (get cfg :traits {}))

(defn propagate-keys
  "Returns a collection of currency-map keys that should be propagated into Currency
  records as extension fields when loading the configuration."
  {:tag clojure.lang.IPersistentCollection :added "2.0.0"}
  [cfg] (get cfg :propagate-keys))

(defn version
  "Returns version string of the given configuration map."
  {:tag String :added "1.0.0"}
  ^String [cfg] (get cfg :version))

;;
;; Config normalization.
;;

(defn- seqable-coll
  "Returns a seq of `x` when it looks like a collection of values.

  Strings are treated as scalars, not as seqable collections."
  {:tag clojure.lang.ISeq :added "2.0.0" :private true}
  [x]
  (cond
    (nil? x) nil
    (string? x) (list x)
    (set? x) (seq (sort-by str x))
    (sequential? x) (seq x)
    (and (seqable? x) (not (string? x))) (seq x)
    :else (list x)))

(defn- merge-traits-values
  "Merges traits representations which may be given as a set, vector, list, etc.
  Returns a vector of distinct values (stable order: dst first, then src)."
  {:tag clojure.lang.IPersistentVector :added "2.0.0" :private true}
  [dst src]
  (let [dst (seqable-coll dst)
        src (seqable-coll src)
        ts  (remove nil? (concat dst src))
        ts  (seq (distinct ts))]
    (when ts (vec ts))))

(defn- merge-localized-entry
  "Deep-merges two localized property maps (locale -> properties map)."
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [dst src]
  (merge-with (fn [a b]
                (if (and (map? a) (map? b))
                  (merge a b)
                  b))
              (or dst {})
              (or src {})))

(defn- add-currency-inline-data
  "Takes a raw config map and expands currency-inline keys into top-level branches.

  Supported inline keys under each currency entry in `:currencies`:
  - `:countries`  seqable of country IDs (expanded into the top-level `:countries` map),
  - `:localized`  map of localized properties (deep-merged into top-level `:localized`),
  - `:traits`     seqable of traits (merged into top-level `:traits`).

  The currency entry itself is left intact (inline keys are not removed)."
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [cfg]
  (let [cur-id->attrs (clojure.core/get cfg :currencies)]
    (if-not (map? cur-id->attrs)
      cfg
      (let [cfg (update cfg :countries #(or % {}))
            cfg (update cfg :localized #(or % {}))
            cfg (update cfg :traits #(or % {}))]
        (reduce (fn [cfg [cid attrs]]
                  (let [cid       (keyword cid)
                        attrs     (or attrs {})
                        countries (when-some [xs (seqable-coll (clojure.core/get attrs :countries))]
                                    (seq (remove nil? xs)))
                        localized (clojure.core/get attrs :localized)
                        traits    (seqable-coll (clojure.core/get attrs :traits))]
                    (cond-> cfg
                      (seq countries)
                      (update :countries
                              (fn [ctr->cur]
                                (reduce (fn [m country-id]
                                          (assoc m (keyword country-id) cid))
                                        (or ctr->cur {})
                                        countries)))

                      (and (map? localized) (pos? (count localized)))
                      (update-in [:localized cid] merge-localized-entry localized)

                      (seq traits)
                      (update-in [:traits cid] merge-traits-values traits))))
                cfg
                cur-id->attrs)))))
