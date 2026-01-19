(ns io.randomseed.bankster.build.maven-meta

  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn- as-str [x]
  (cond
    (string? x) x
    (ident?  x) (name x)
    :else       (str x)))

(defn- ensure-trailing-newline
  ^String [^String s]
  (if (and s (not (str/ends-with? s "\n")))
    (str s "\n")
    s))

(defn- maven-meta-dir
  "Returns META-INF/maven/<groupId>/<artifactId> directory under class-dir."
  [class-dir group-id artifact-id]
  (io/file class-dir "META-INF" "maven" (as-str group-id) (as-str artifact-id)))

(defn install-maven-metadata!
  "Copies POM into jar's META-INF/maven/... and writes pom.properties.

  Args:
  - :class-dir   (required) path to module-specific class dir
  - :group-id    (required) e.g. \"io.randomseed\"
  - :artifact-id (required) e.g. \"utils-core\"
  - :version     (required) e.g. \"1.0.0\"
  - :pom         one of:
      * java.io.File / string path to pom.xml
      * java.io.InputStream (e.g. your (pom-stream m))
    (required)

  Returns class-dir."
  [{:keys [class-dir group-id artifact-id version pom] :as _opts}]
  (when-not (and class-dir group-id artifact-id version pom)
    (throw (ex-info "install-maven-metadata!: missing required keys"
                    {:required [:class-dir :group-id :artifact-id :version :pom]})))

  (let [meta-dir   (maven-meta-dir class-dir group-id artifact-id)
        pom-target (io/file meta-dir "pom.xml")
        props-file (io/file meta-dir "pom.properties")
        props      (-> (str "groupId="    (as-str group-id)    "\n"
                            "artifactId=" (as-str artifact-id) "\n"
                            "version="    (as-str version)     "\n")
                       ensure-trailing-newline)]
    (.mkdirs ^java.io.File meta-dir)
    (with-open [in  (cond
                      (instance? java.io.InputStream pom)
                      pom
                      :else
                      (io/input-stream (io/file (as-str pom))))
                out (io/output-stream pom-target)]
      (io/copy in out))

    ;; Write distilled properties.
    (spit props-file props :encoding "UTF-8")

    class-dir))
