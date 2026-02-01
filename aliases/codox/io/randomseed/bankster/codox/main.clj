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

(defn- non-nil-map
  "Returns a map containing only entries with non-nil values."
  [m]
  (into {} (filter (comp some? val)) m))

(defn- get-options
  [deps config {:keys [version src-dirs] :as overrides}]
  (let [version   (or (some-> version str) "0.0.0")
        src-dirs  (not-empty src-dirs)
        codox     config
        userdir   (System/getProperty "user.dir")
        overrides (-> overrides
                      (dissoc :src-dirs :config-file)
                      non-nil-map)]
    ;;(println (vec (or src-dirs (:paths deps) ["src"])))
    (merge {:source-paths (vec (or src-dirs (:paths deps) ["src"]))
            :root-path    (or (:root config) (:root deps) (:root-path config) (:root-path deps) userdir)
            :output-path  (str (io/file (:target deps userdir) "docs"))}
           codox
           {:name        (str/capitalize (or (:name codox) (:name deps)))
            :license     (:license     codox (:license     deps))
            :package     (:package     codox (:license     deps))
            :description (:description codox (:description deps))
            :version     (or version (:version codox) (:version deps))}
           overrides)))

(defn codox
  "Generate API documentation from source code."
  ([version src-dirs]
   (codox {:version version :src-dirs src-dirs}))
  ([{:keys [config-file] :as opts}]
   (let [deps    (read-deps)
         config  (read-edn-config (or (not-empty config-file) "codox.edn"))
         options (get-options deps config opts)]
     (codox.main/generate-docs options)
     (shutdown-agents))))

(defn -main
  [& args]
  (let [opts (cond
               (empty? args) {}
               (and (= 1 (count args)) (map? (first args))) (first args)
               (odd? (count args)) (throw (ex-info "Invalid exec args (expected map or even key/value list)."
                                                  {:args args}))
               :else (apply hash-map args))]
    (codox opts)))
