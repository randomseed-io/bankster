(ns io.randomseed.bankster.config

  ^{:doc    "Bankster library, configuration handling."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [load])

  (:require [clojure.edn                    :as      edn]
            [io.randomseed.bankster.util    :refer  :all]
            [io.randomseed.bankster.util.fs :as       fs]
            [io.randomseed.bankster])

  (:import  [io.randomseed.bankster Currency Registry]))

;;
;; Default configuration filename.
;;

(def ^String ^private ^const default-resource-path
  "Default registry resource file – relative path."
  "io/randomseed/bankster/config.edn")

(def ^String ^private ^const user-resource-path
  "Additional registry resource file – relative path."
  "META-INF/io/randomseed/bankster/currencies.edn")

;;
;; Config file reader.
;;

(defn ^clojure.lang.PersistentHashMap load
  "Loads data structures from an EDN file. The given path should reside in one of the
  resource directories. If it is not given the default-resource-path will be used."
  {:added "1.0.0"}
  ([]
   (load default-resource-path))
  ([^String resource-path]
   (when-some [^java.net.URL r (fs/paths->resource resource-path)]
     (when-some [config (edn/read-string (slurp r))]
       (when (and (map? config) (pos? (count config)))
         config)))))

;;
;; Getters.
;;

(defn currencies
  "Returns currencies map of the given configuration map."
  {:tag clojure.lang.PersistentHashMap :added "1.0.0"}
  ([cfg] (get cfg :currencies {})))

(defn countries
  "Returns countries map of the given configuration map."
  {:tag clojure.lang.PersistentHashMap :added "1.0.0"}
  ([cfg] (get cfg :countries {})))

(defn localized
  "Returns localized properties map of the given configuration map."
  {:tag clojure.lang.PersistentHashMap :added "1.0.0"}
  ([cfg] (get cfg :localized {})))

(defn version
  "Returns version string of the given configuration map."
  {:tag String :added "1.0.0"}
  ([cfg] (get cfg :version)))
