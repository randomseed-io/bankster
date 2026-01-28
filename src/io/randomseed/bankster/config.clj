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
         config)))))

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

(defn version
  "Returns version string of the given configuration map."
  {:tag String :added "1.0.0"}
  ^String [cfg] (get cfg :version))
