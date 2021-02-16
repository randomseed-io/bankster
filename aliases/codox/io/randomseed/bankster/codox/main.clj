(ns io.randomseed.bankster.codox.main
  (:require [codox.main                      :as       c]
            [clojure.java.io                 :as      io]
            [clojure.edn                     :as     edn]
            [clojure.tools.deps.alpha        :as    deps]
            [clojure.string                  :as     str]))

(defn- read-deps
  []
  (:project-edn (deps/find-edn-maps)))

(defn- get-options [deps & more]
  (let [key     (or (first more) :codox)
        codox   (key deps)
        userdir (System/getProperty "user.dir")]
    (merge {:source-paths (:paths deps ["src"])
            :root-path    (:root-path deps userdir)
            :output-path  (str (io/file (:target deps userdir) "docs"))}
           codox
           {:name        (str/capitalize (:name codox (:name deps)))
            :license     (:license     codox (:license deps))
            :package     (:package     codox (:license deps))
            :version     (:version     codox (:version deps))
            :description (:description codox (:description deps))})))

(defn codox
  "Generate API documentation from source code."
  [key]
  (let [deps    (read-deps)
        options (get-options deps key)]
    (codox.main/generate-docs options)
    (shutdown-agents)
    (println "Generated HTML docs in" (:output-path options))))

(defn -main
  [& args]
  (codox :codox))
