(ns io.randomseed.bankster.build.pom-sync

  (:require [clojure.edn          :as  edn]
            [clojure.zip          :as  zip]
            [clojure.java.io      :as   io]
            [clojure.string       :as  str]
            [clojure.data.xml     :as  xml]))

(def scm-conn-prefix     "scm:git:git://")
(def scm-dev-conn-prefix "scm:git:ssh://git@")
(def scm-url-prefix      "https://")
(def scm-url-suffix      "/")
(def scm-conn-suffix     ".git")
(def scm-dev-conn-suffix ".git")

(def ^:private TOP-IND    "\n  ")
(def ^:private IND-DEPSEC "\n  ")
(def ^:private IND-DEP    "\n    ")
(def ^:private IND-FIELD  "\n      ")
(def ^:private NL         "\n")
(def ^:private IND2       "\n  ")

(defn- pom-ns
  [pom-root]
  (namespace (:tag pom-root)))

(defn- pom-tag
  [pom-ns local]
  (keyword pom-ns (name local)))

(defn- set-text!
  [loc s]
  (zip/edit loc assoc :content [(str s)]))

(defn parse-xml-fragment
  [^String s]
  (xml/parse
   (java.io.ByteArrayInputStream.
    (.getBytes s java.nio.charset.StandardCharsets/UTF_8))))

(defn- interleave-ws
  "Returns content: [ws child ws child ... ws-end]."
  [ws-between ws-end children]
  (-> []
      (into (mapcat (fn [ch] [ws-between ch]) children))
      (conj ws-end)))

(defn- ws?
  [x]
  (and (string? x) (re-matches #"\s*" x)))

(defn- trim-trailing-ws
  "Removes trailing whitespace-only text nodes from a content vector."
  [content]
  (loop [v (vec content)]
    (if (and (seq v) (ws? (peek v)))
      (recur (pop v))
      v)))

(defn- child-by-local
  "Returns first direct child element whose local name matches local-tag."
  [parent-loc local-tag]
  (let [want (name local-tag)]
    (loop [loc (zip/down parent-loc)]
      (when loc
        (let [n (zip/node loc)]
          (if (and (map? n) (= (name (:tag n)) want))
            loc
            (recur (zip/right loc))))))))

(defn- ensure-child!
  [parent-loc pom-ns local-tag]
  (or (child-by-local parent-loc local-tag)
      (let [t     (pom-tag pom-ns local-tag)
            child (xml/element t {} "")]
        ;; append-child returns *parent* loc; go down and jump to last child
        (-> parent-loc
            (zip/append-child child)
            zip/down
            zip/rightmost))))

(defn dependency-el
  [pom-ns {:keys [groupId artifactId version scope optional]}]
  (let [t (fn [local] (pom-tag pom-ns local))
        fields
        (cond-> [(hash-map :tag (t :groupId)    :attrs nil :content [(str groupId)])
                 (hash-map :tag (t :artifactId) :attrs nil :content [(str artifactId)])
                 (hash-map :tag (t :version)    :attrs nil :content [(str version)])]
          scope            (conj {:tag (t :scope) :attrs nil :content [(str scope)]})
          (some? optional) (conj {:tag (t :optional) :attrs nil :content [(str optional)]}))]
    {:tag     (t :dependency)
     :attrs   nil
     ;; 6 spacji przed każdym polem, a na końcu 4 spacje przed </dependency>
     :content (interleave-ws IND-FIELD IND-DEP fields)}))

(defn dependencies-el
  [pom-ns dep-specs]
  (let [t    (fn [local] (pom-tag pom-ns local))
        deps (mapv #(dependency-el pom-ns %) dep-specs)]
    {:tag     (t :dependencies)
     :attrs   nil
     ;; 4 spacje przed każdym <dependency>, a na końcu 2 spacje przed </dependencies>
     :content (interleave-ws IND-DEP IND-DEPSEC deps)}))

(defn set-project-dependencies
  "Ensures/updates <project><dependencies>...</dependencies>.
   - No blank line before <dependencies>
   - Ensures newline after </dependencies> so </project> is on its own line."
  [project-loc pom-ns dep-specs]
  (let [deps-node (dependencies-el pom-ns dep-specs)]
    (if-let [deps-loc (child-by-local project-loc :dependencies)]
      ;; replace in-place (usually formatting inside deps-node handles indentation)
      (-> deps-loc (zip/replace deps-node) zip/up)
      ;; append: normalize end of <project> first, then add IND2 + deps + NL
      (-> project-loc
          (zip/edit update :content trim-trailing-ws)
          (zip/append-child IND2)
          (zip/append-child deps-node)
          (zip/append-child NL)))))

(defn set-project-group-id [project-loc pom-ns group-id]
  (-> project-loc
      (ensure-child! pom-ns :groupId)
      (set-text! group-id)
      zip/up))

(defn set-project-artifact-id [project-loc pom-ns artifact-id]
  (-> project-loc
      (ensure-child! pom-ns :artifactId)
      (set-text! artifact-id)
      zip/up))

(defn set-project-name [project-loc pom-ns name]
  (-> project-loc
      (ensure-child! pom-ns :name)
      (set-text! name)
      zip/up))

(defn set-project-url [project-loc pom-ns url]
  (-> project-loc
      (ensure-child! pom-ns :url)
      (set-text! url)
      zip/up))

(defn set-project-description [project-loc pom-ns description]
  (-> project-loc
      (ensure-child! pom-ns :description)
      (set-text! description)
      zip/up))

(defn set-project-version [project-loc pom-ns ver]
  (-> project-loc
      (ensure-child! pom-ns :version)
      (set-text! ver)
      zip/up))

(defn- set-child-text-up
  "Ensure child under parent-loc, set its text, and return *parent-loc* (updated)."
  [parent-loc pom-ns local-tag s]
  (-> (ensure-child! parent-loc pom-ns local-tag) ; => child-loc
      (set-text! s)                               ; => edited child-loc
      zip/up))                                    ; => updated parent-loc

(defn set-project-scm-urls
  [project-loc pom-ns scm]
  (let [scm-url (str scm-url-prefix      scm scm-url-suffix)
        scm-cnn (str scm-conn-prefix     scm scm-conn-suffix)
        scm-dcn (str scm-dev-conn-prefix scm scm-dev-conn-suffix)
        scm-loc (ensure-child! project-loc pom-ns :scm)]
    (-> scm-loc
        (set-child-text-up pom-ns :url                 scm-url)
        (set-child-text-up pom-ns :connection          scm-cnn)
        (set-child-text-up pom-ns :developerConnection scm-dcn)
        zip/up)))

(defn set-project-scm-tag [project-loc pom-ns tag]
  (let [scm-loc (ensure-child! project-loc pom-ns :scm)
        tag-loc (ensure-child! scm-loc     pom-ns :tag)]
    (-> tag-loc (set-text! tag)
        zip/up     ; -> scm
        zip/up)))  ; -> project

(defn update-project-fields
  [pom-root {:keys [group name description version scm url]}]
  (let [ns (pom-ns pom-root)
        z0 (zip/xml-zip pom-root)
        z1 (cond-> z0
             group       (set-project-group-id    ns group)
             name        (set-project-artifact-id ns name)
             name        (set-project-name        ns name)
             description (set-project-description ns description)
             version     (set-project-version     ns version)
             version     (set-project-scm-tag     ns version)
             scm         (set-project-scm-urls    ns scm)
             url         (set-project-url         ns url))]
    (zip/root z1)))

(defn- read-edn-file
  [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (edn/read {:readers *data-readers*} r)))

(defn read-pom-xml [^java.io.File f]
  (with-open [in (java.io.FileInputStream. f)]
    (xml/parse in)))

(defn write-pom-xml! [^java.io.File f pom-root]
  (with-open [out (java.io.OutputStreamWriter.
                   (java.io.FileOutputStream. f)
                   java.nio.charset.StandardCharsets/UTF_8)]
    (xml/emit pom-root out)))

(defn update-pom-file!
  [pom-file opts]
  (let [pom  (read-pom-xml (java.io.File. pom-file))
        pom' (update-project-fields pom opts)]
    ;; (prn (map :tag (filter map? (:content pom'))))
    (write-pom-xml! (java.io.File. pom-file) pom')
    pom-file))

(defn- provided-scope?
  [{:keys [groupId artifactId]}]
  (and (= groupId "org.clojure")
       (= artifactId "clojure")))

(defn- lib->ga
  "`deps.edn` lib symbol (group/artifact) -> [groupId artifactId]."
  [lib]
  (let [s (str lib)
        i (.indexOf ^String s "/")]
    (if (neg? i)
      [s s]
      [(subs s 0 i) (subs s (inc i))])))

(defn- rel-local-root?
  [^String root]
  (or (str/starts-with? root "./")
      (str/starts-with? root "../")))

(defn- artifact-base
  "Given a lib symbol like `io.randomseed/utils-log` returns the artifact base,
   i.e. the part after '/' up to the first '-' (or whole artifact if no '-')."
  [lib]
  (let [s   (str lib)
        i   (.indexOf ^String s "/")
        art (if (neg? i) s (subs s (inc i)))
        j   (.indexOf ^String art "-")]
    (if (neg? j) art (subs art 0 j))))

(defn- coord->version
  "Returns Maven-representable version string, or nil (skip).

   Rules:
   - {`:mvn/version` \"x\"} -> \"x\"
   - {`:local/root` \"./...\" or \"../...\"} -> local-root-version/version ONLY when:
       * `:name` option was provided, AND
       * artifact-base(lib) == artifact-base(`:name`)
     Otherwise for relative local/root: nil (skipped).
   - other {`:local/root` ...} -> local-root-version (backward-compatible)
   - git deps etc -> nil"
  [lib coord {:keys [local-root-version version name]}]
  (cond
    (contains? coord :mvn/version)
    (str (:mvn/version coord))

    (contains? coord :local/root)
    (let [root (str (:local/root coord))
          rel? (rel-local-root? root)]
      (cond
        (and rel?
             (some? name)
             (= (artifact-base lib) (artifact-base name)))
        (str (or local-root-version version "${project.version}"))

        rel?
        nil

        :else
        (str (or local-root-version version "${project.version}"))))

    :else
    nil))

(defn- merge-alias-deps
  "Return an effective deps after merging with the given aliases.
   Minimal model:
   - start: (`:deps` m)
   - `:replace-deps` (if exists in alias) replaces all
   - `:extra-deps` adds or overrides (if exists)
   - `:override-deps` overrides or adds (if does not exist)"
  [deps-edn {:keys [aliases]}]
  (let [base-deps (or (:deps deps-edn) {})
        alias-m   (or (:aliases deps-edn) {})
        chosen    (keep alias-m aliases)]
    (reduce
     (fn [deps a]
       (cond
         (:replace-deps a)
         (merge (or (:replace-deps a) {}) (or (:override-deps a) {}) (or (:extra-deps a) {}))

         :else
         (-> deps
             (merge (or (:extra-deps a) {}))
             (merge (or (:override-deps a) {})))))
     base-deps
     chosen)))

(defn deps->maven-deps
  "Reads `deps.edn` and returns normalized Maven deps:
   [{`:groupId` .. `:artifactId` .. `:version` ..} ...] sorted deterministically.

   Options:
     `:version`            (version)
     `:local-root-version` (default version, then \"${`project.version`}\")
     `:name`               (used only for relative `:local/root` replacement rule)"
  ([deps-edn-path] (deps->maven-deps deps-edn-path nil))
  ([deps-edn-path opts]
   (let [m    (read-edn-file deps-edn-path)
         deps (merge-alias-deps m opts)]
     (->> deps
          (keep (fn [[lib coord]]
                  (when-let [v (coord->version lib coord (or opts {}))]
                    (let [[g a] (lib->ga lib)
                          dep   {:groupId g :artifactId a :version v}]
                      (cond-> dep
                        (provided-scope? dep) (assoc :scope "provided"))))))
          (sort-by (juxt :groupId :artifactId))
          vec))))

(defn update-pom-deps-xml
  [pom-root dep-specs]
  (let [ns (pom-ns pom-root)
        z0 (zip/xml-zip pom-root)
        z1 (set-project-dependencies z0 ns dep-specs)]
    (zip/root z1)))

(defn sync-pom-deps!
  "Reads `deps.edn` `:deps` and updates `pom.xml`:
   - updates <dependencies> via XML transform (no text splicing)
   - updates project fields via update-project-fields (your zipper pass)
   Returns {`:pom` ... `:deps` N}.

   Options:
     `:url`         (url)
     `:scm`         (scm)
     `:name`        (name)
     `:group`       (group)
     `:lib-name`    (group/name)
     `:version`     (version)
     `:description` (description)
     `:local-root-version` (fallback to version, then to \"${`project.version`}\")
     `:aliases`     (a list used to get extra dependencies from `deps.edn`)"
  ([deps-edn-path pom-path]
   (sync-pom-deps! deps-edn-path pom-path nil))
  ([deps-edn-path pom-path opts]
   (let [dep-specs (deps->maven-deps deps-edn-path opts)
         pom-file  (java.io.File. pom-path)
         pom0      (read-pom-xml pom-file) ;; 1) update top-level project fields
         pom1      (update-project-fields pom0 opts) ;; 2) update dependencies as XML tree
         pom2      (update-pom-deps-xml pom1 dep-specs)]
     (write-pom-xml! pom-file pom2)
     {:pom pom-path :deps (count dep-specs)})))
