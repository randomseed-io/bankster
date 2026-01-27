(ns io.randomseed.bankster.build.main

  (:require [io.randomseed.bankster.build.pom-sync   :as pom-sync]
            [io.randomseed.bankster.build.maven-meta :as    mmeta]
            [clojure.java.io                         :as       io]
            [clojure.tools.build.api                 :as        b]
            [clojure.edn                             :as      edn]))

(defn kw->name
  [n]
  (not-empty
   (cond (ident?  n) (name n)
         (string? n) n
         (nil?    n) nil
         :else       (str n))))

(defn kw->symbol
  [n]
  (cond (nil?           n) nil
        (simple-symbol? n) n
        :else              (symbol (not-empty (kw->name n)))))

(defn description [opts] (kw->name   (:description opts)))
(defn app-version [opts] (kw->name   (:version     opts)))
(defn app-group   [opts] (kw->name   (:group       opts)))
(defn app-name    [opts] (kw->name   (:name        opts)))
(defn app-scm     [opts] (kw->name   (:scm         opts)))
(defn app-url     [opts] (kw->name   (:url         opts)))
(defn lib-name    [opts] (str (app-group opts) "/" (app-name    opts)))
(defn jar-name    [opts] (str (app-name  opts) "-" (app-version opts) ".jar"))
(defn jar-file    [opts] (str "target/" (jar-name opts)))
(defn aot-ns      [opts] (not-empty (mapv kw->symbol (:aot-ns opts))))

(defn- slurp-edn [^String path]
  (when (.exists (io/file path))
    (edn/read-string (slurp path))))

(defn- module-dir ^String [module]
  (if-some [m (kw->name module)]
    (str "modules/" m)
    "."))

(defn- file-exists? [^String path]
  (.exists (io/file path)))

(defn- ensure-module!
  "Ensures module exists on disk (`deps.edn` + `pom.xml`). Returns module files."
  [{:keys [module dir deps pom] :as mfiles}]
  (when-not (file-exists? deps)
    (throw (ex-info "Missing deps.edn"
                    {:module (or module ::unknown) :dir dir :missing deps})))
  (when-not (file-exists? pom)
    (throw (ex-info "Missing pom.xml"
                    {:module (or module ::unknown) :dir dir :missing pom})))
  mfiles)

(defn module-files
  "Returns key module paths (deps/pom/src/resources) for a module."
  ([opts]
   (let [module (kw->name (:module opts))
         dir    (or (kw->name (:dir opts))
                    (kw->name (:root-dir opts))
                    (kw->name (:root opts))
                    (module-dir module))]
     (ensure-module!
      {:module    (keyword module)
       :dir       dir
       :class-dir (or (kw->name (:class-dir opts)) (str dir "/target/classes"))
       :deps      (or (kw->name (:deps      opts)) (str dir "/deps.edn"))
       :pom       (or (kw->name (:pom       opts)) (str dir "/pom.xml"))
       :src       (or (kw->name (:src       opts)) (str dir "/src"))
       :res       (or (kw->name (:res       opts)) (str dir "/resources"))
       :jar       (or (kw->name (:jar       opts)) (str "target/" (jar-name opts)))}))))

(defn- read-deps
  "Reads `deps.edn`."
  [f]
  (slurp-edn f))

(defn- module-paths
  "List of relative directories to be packed taken from `deps.edn` keys `:paths`.
   Defaults to [\"src\" \"resources\"]."
  [{:keys [dir deps]}]
  (let [deps  (read-deps deps)
        paths (or (:paths deps) ["src" "resources"])]
    (->> paths
         (map #(str dir "/" %))
         (filter #(-> % io/file .isDirectory)))))

(defn- pom-stream
  [{:keys [pom]}]
  (io/input-stream (io/file pom)))

(defn- basis-for
  "Basis created from module's deps.edn."
  [{:keys [deps dir]} {:keys [aliases] :or {aliases []}}]
  (b/create-basis {:project deps :dir dir :aliases aliases}))

(defn- ensure-class-dir
  [{:keys [class-dir]}]
  (when (or (not class-dir) (< (count class-dir) 16))
    (throw (ex-info (str "Class directory is not long enough: " class-dir) {:data class-dir})))
  class-dir)

(defn sync-pom
  "Sync `deps.edn` -> `pom.xml` (dependency section).
   - clojure -T`:build` sync-pom
   - clojure -T`:build` sync-pom `:name`    \"utils-core\" \\
                               `:group`   '`io.randomseed` \\
                               `:version` \"1.0.0\"      \\
                               `:local-root-version` \"${`project.version`}\""
  [{:keys [local-root-version aliases] :as opts}]
  (let [{:keys [deps pom]} (module-files opts)]
    (pom-sync/sync-pom-deps! deps pom
                             {:name               (app-name    opts)
                              :group              (app-group   opts)
                              :lib-name           (lib-name    opts)
                              :version            (app-version opts)
                              :description        (description opts)
                              :url                (app-url     opts)
                              :scm                (app-scm     opts)
                              :aliases            aliases
                              :local-root-version (kw->name local-root-version)})))

(defn jar
  "Updates POM with dependencies and builds a single module JAR.
   - clojure -T`:build` jar `:module`  `:core`          \\
                          `:name`    \"utils-core\"  \\
                          `:group`   '`io.randomseed` \\
                          `:version` \"1.0.0\""
  [opts]
  (let [mfiles    (module-files     opts)
        aot-ns    (aot-ns           opts)
        class-dir (ensure-class-dir mfiles)
        src-dirs  (module-paths     mfiles)
        basis     (basis-for        mfiles opts)
        jar-path  (:jar             mfiles)]
    (io/make-parents               (io/file jar-path))
    (b/delete                      {:path class-dir})
    (b/copy-dir                    {:src-dirs src-dirs :target-dir class-dir})
    (when aot-ns (b/compile-clj    {:basis      basis
                                    :class-dir  class-dir
                                    :ns-compile aot-ns}))
    (mmeta/install-maven-metadata! {:artifact-id (app-name    opts)
                                    :group-id    (app-group   opts)
                                    :version     (app-version opts)
                                    :class-dir   class-dir
                                    :pom         (pom-stream mfiles)
                                    :aliases     (:aliases opts)})
    (b/jar                         {:class-dir class-dir
                                    :jar-file  jar-path
                                    :basis     basis})))

(defn -main [& opts]
  (jar opts))
