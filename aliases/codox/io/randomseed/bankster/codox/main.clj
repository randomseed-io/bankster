(ns io.randomseed.bankster.codox.main

  (:require [codox.main                      :as       c]
            [clojure.java.io                 :as      io]
            [clojure.edn                     :as     edn]
            [clojure.tools.deps              :as    deps]
            [clojure.string                  :as     str]))

(defn- read-edn-config
  [f]
  (when-let [url (io/resource (or (not-empty f) "codox.edn"))]
    (with-open [r (java.io.PushbackReader. (io/reader url))]
      (edn/read r))))

(defn- read-deps
  []
  (:project-edn (deps/find-edn-maps)))

(defn- get-options [deps config version src-dirs]
  (let [version  (or version "0.0.0")
        src-dirs (not-empty src-dirs)
        codox    config
        userdir  (System/getProperty "user.dir")]
    ;;(println (vec (or src-dirs (:paths deps) ["src"])))
    (merge {:source-paths (vec (or src-dirs (:paths deps) ["src"]))
            :root-path    (or (:root config) (:root deps) (:root-path config) (:root-path deps) userdir)
            :output-path  (str (io/file (:target deps userdir) "docs"))}
           codox
           {:name        (str/capitalize (or (:name codox) (:name deps)))
            :license     (:license     codox (:license     deps))
            :package     (:package     codox (:license     deps))
            :description (:description codox (:description deps))
            :version     (or version (:version codox) (:version deps))})))

(defn codox
  "Generate API documentation from source code."
  [version src-dirs]
  (let [version (when version (str version))
        deps    (read-deps)
        config  (read-edn-config "codox.edn")
        options (get-options deps config version src-dirs)]
    (codox.main/generate-docs options)
    (shutdown-agents)))

(defn -main
  [& {:keys [version src-dirs] :as _all}]
  (codox version src-dirs))
