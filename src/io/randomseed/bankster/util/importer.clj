(ns

    ^{:doc    "Bankster library, import-export operations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.util.importer

  (:require [clojure.java.io                 :as       io]
            [clojure.data.csv                :as      csv]
            [clojure.string                  :as      str]
            [trptr.java-wrapper.locale       :as        l]
            [puget.printer                   :as    puget]
            [io.randomseed.bankster          :as bankster]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.util.fs  :as       fs]
            [io.randomseed.bankster.util.map :as      map]
            [io.randomseed.bankster.util     :as       bu])

  (:import  (io.randomseed.bankster Currency Registry)
            (java.time              LocalDateTime)
            (java.time.format       DateTimeFormatter)))

;;
;; Pathnames and URIs.
;;

(def ^{:const true :tag String :added "1.0.0"}
  default-resource-name
  "Name of a default resource container."
  "io/randomseed/bankster")

(def ^{:const true :tag String :added "1.0.0"}
  default-resource-must-exist-file
  "Filename in a default resource container that must exist."
  "config.edn")

(def ^{:const true :tag String :added "2.0.0"}
  import-seed
  "Filename in a default resource container which is a static seed for data when
  importing."
  "seed.edn")

(def ^{:const true :tag String :added "1.0.0"}
  default-dump-filename
  "Default EDN dump file."
  "registry-dump.edn")

(def ^{:const true :tag String :added "1.0.0"}
  default-export-filename
  "Default EDN export file."
  "registry-export.edn")

(def ^{:const true :tag String :added "2.0.0"}
  default-export-currency-oriented-filename
  "Default EDN export file (currency-oriented)."
  "registry-export-currency-oriented.edn")

(def ^{:const true :tag String :added "1.0.0"}
  default-reader-filenames
  "Default data reader filenames (Clojure code)."
  ["data_readers.clj" "data_readers.cljc"])

(def ^{:const true :tag String :added "1.2.4"}
  default-data-reader-filename
  "Default data reader filename (pure data)."
  "data_readers_edn.clj")

(def ^{:const true :tag String :added "1.0.0"}
  default-handlers-pathname
  "Default pathname of a reader handlers file."
  "io/randomseed/bankster/money/reader_handlers.clj")

(def ^{:const true :tag String :added "1.0.0"}
  default-handlers-namespace
  "Default namespace of reader handlers."
  "io.randomseed.bankster.money")

(def ^{:const true :tag String :added "1.0.0"}
  default-countries-csv
  "Default CSV file with countries database."
  "org/joda/money/CountryData.csv")

(def ^{:const true :tag String :added "1.0.0"}
  default-currencies-csv
  "Default CSV file with currencies database."
  "org/joda/money/CurrencyData.csv")

(def ^{:tag 'long :const true :added "2.0.0"}
  default-legacy-weight
  "Default weight of legacy currencies."
  (long 10000))

;;
;; Transformation rules.
;;

(def ^{:private true :added "1.0.0"}
  special-kinds
  "ISO codes for special kinds of currencies."
  ;; These kinds are aligned with the default kind ontology shipped in seed/config.
  {:USN :iso.funds/settlement
   :XSU :iso.funds/international
   :CLF :iso.funds/market
   :XUA :iso.funds/institutional
   :XTS :iso/test
   :XPT :iso/metal
   :XPD :iso/metal
   :XAU :iso/metal
   :XAG :iso/metal
   :XDR :iso.funds/international
   :XBA :iso.funds/market
   :XBB :iso.funds/market
   :XBC :iso.funds/market
   :XBD :iso.funds/market
   :XXX :iso/null})

(defn make-currency
  "Shapes an ISO-standardized currency entry. Gets a sequence of linear collections
  describing currency and returns a currency object."
  {:private true :added "1.0.0"}
  [[id numeric scale comment]]
  (when (some? id)
    (let [comment (some-> comment str/triml)
          old?    (and (some? comment) (str/starts-with? comment "Old, now"))
          funds?  (and (some? comment) (str/starts-with? comment "FundsCode"))
          code    (keyword id)
          id      (if old? (keyword "iso-4217-legacy" id) (keyword id))
          numeric (or (bu/try-parse-long numeric) currency/no-numeric-id)
          numeric (if (< numeric 0) currency/no-numeric-id numeric)
          scale   (or (bu/try-parse-int scale) currency/auto-scaled)
          scale   (if (< scale 0) currency/auto-scaled scale)
          kind    (if funds? :iso/funds (get special-kinds code :iso/fiat))
          domain  (when-not old? :ISO-4217)
          weight  0]
      (currency/new-currency id (long numeric) (int scale) kind domain (int weight)))))

;;
;; Joda Money CSV importer.
;;

(defn countries-load
  "Reads CSV file in a format compliant with Joda Money and returns a map with currency
  to countries associations where countries are sets. The pathname should be
  relative to the resources directory."
  {:added "1.0.0"}
  [& [pathname]]
  (when-some [r (fs/paths->resource (or pathname default-countries-csv))]
    (->> r fs/read-csv
         (map (comp vec (partial map keyword)))
         (into {})
         (map/invert-in-sets))))

(defn currencies-load
  "Reads a CSV file compliant with Joda Money and returns a sequence of currencies.

  The pathname should be relative to the resources directory."
  {:added "1.0.0"}
  [& [pathname]]
  (when-some [f (fs/paths->resource (or pathname default-currencies-csv))]
    (let [split-comment
          (fn [row]
            (let [row (vec row)
                  l   (peek row)]
              (if (and (string? l) (str/includes? l "#"))
                (let [i (str/index-of l "#")
                      v (str/trimr (subs l 0 i))
                      c (str/triml (subs l (inc i)))
                      c (when-not (str/blank? c) c)]
                  (conj (pop row) v c))
                (conj row nil))))]
      (->> (fs/read-csv f true)
           (map split-comment)
           (map make-currency)))))

(defn joda-import
  "Reads CSV files with countries and currencies definitions (Joda Money format) and
  returns a registry."
  {:tag Registry :added "1.0.0"}
  ([]
   (joda-import nil nil))
  ([countries-pathname
    currencies-pathname]
   (let [^Registry                       registry   (registry/new-registry)
         ^clojure.lang.PersistentHashMap countries  (countries-load countries-pathname)
         ^clojure.lang.PersistentHashMap currencies (currencies-load currencies-pathname)]
     (reduce (fn ^Registry [^Registry r, ^Currency c]
               (currency/register r c (get countries (currency/id c))))
             registry currencies))))

(defn edn-import
  "Alias for `io.randomseed.bankster.registry/global`."
  {:tag clojure.lang.Atom :added "2.0.0"}
  []
  (registry/global))

;;
;; EDN dumper and exporter.
;;

(defn currency->map
  "Takes a currency and returns a map suitable for putting into a configuration
  file. Extension fields are ignored."
  {:added "1.0.0"}
  [{:keys [:numeric :scale :kind] :as c}]
  (as-> (sorted-map) m
    (if (and (number? numeric) (pos? numeric))      (assoc m :numeric numeric) m)
    (if-not (and (some? scale) (neg? scale))        (assoc m :scale   scale)   m)
    (if (some? kind)                                (assoc m :kind    kind)    m)))

(defn localized->map
  "Takes a localized map entry (1st level) and returns a map suitable for putting into
  a configuration file."
  {:added "1.0.0"}
  [m]
  (map/map-keys (comp keyword str l/locale) m))

(defn traits->map
  "Takes a traits set/vector and returns a representation suitable for putting into a
  configuration file."
  {:added "2.0.0"}
  [traits]
  (when (seq traits)
    (vec (sort-by str traits))))

(defn- sort-kw-vec
  "Returns a sorted vector of keyword-ish values."
  {:tag clojure.lang.IPersistentVector :added "2.0.0" :private true}
  [xs]
  (when (seq xs)
    (->> xs
         (remove nil?)
         (map keyword)
         (sort-by str)
         (vec))))

(defn map->currency-oriented
  "Takes a configuration map in a branch-oriented shape (as produced by `registry->map`)
  and returns a currency-oriented variant.

  Per-currency properties are embedded into each currency map under `:currencies`:
  - `:countries` (vector of country IDs),
  - `:localized` (localized properties map),
  - `:traits`    (vector of traits),
  - `:weight`    (currency weight).

  Top-level branches `:countries`, `:localized`, `:traits` and `:weights` are reduced
  to contain only orphaned entries (i.e. those not associated with any known currency
  IDs)."
  {:tag clojure.lang.IPersistentMap :added "2.0.0"}
  [m]
  (let [compare-by-str (fn [a b] (compare (str a) (str b)))
        cur-id->attrs (or (:currencies m) {})
        cur-ids       (set (keys cur-id->attrs))
        ctr->cur      (or (:countries m) {})
        cur->ctrs     (map/invert-in-sets ctr->cur)
        localized     (or (:localized m) {})
        traits        (or (:traits m) {})
        weights       (or (:weights m) {})
        currencies'   (reduce-kv (fn [out cid attrs]
                                   (let [ctrs (sort-kw-vec (get cur->ctrs cid))
                                         lcl  (get localized cid)
                                         trts (get traits cid)
                                         w?   (contains? weights cid)
                                         w    (get weights cid)]
                                     (assoc out cid
                                            (cond-> attrs
                                              (seq ctrs)                 (assoc :countries ctrs)
                                              (and (map? lcl) (seq lcl)) (assoc :localized lcl)
                                              (seq trts)                 (assoc :traits trts)
                                              w?                         (assoc :weight w)))))
                                 (sorted-map-by compare-by-str)
                                 cur-id->attrs)
        countries'    (into (sorted-map)
                            (remove (fn [[_ cid]] (contains? cur-ids cid)))
                            ctr->cur)
        localized'    (reduce dissoc localized cur-ids)
        traits'       (reduce dissoc traits cur-ids)
        weights'      (reduce dissoc weights cur-ids)]
    (assoc m
           :currencies currencies'
           :countries  countries'
           :localized  localized'
           :traits     traits'
           :weights    weights')))

(defn registry->map
  "Takes a registry and returns a map suitable for putting into a configuration
  file. Extension fields are ignored. When registry is not given it uses the global
  one."
  {:added "1.0.0"}
  ([]
   (registry->map (registry/state)))
  ([registry]
   (when (some? registry)
     (letfn [(hierarchy->parent-map [h]
               (let [rels (or (clojure.core/get h :parents) {})]
                 (into (sorted-map)
                       (map (fn [[child ps]]
                              (let [ps (cond
                                         (nil? ps)        nil
                                         (= 1 (count ps)) (first ps)
                                         :else            (vec (sort-by str ps)))]
                                (vector child ps))))
                       rels)))]
       (letfn [(compare-by-str [a b]
                 (compare (str a) (str b)))
               (edn-unreadable-currency-id? [cid]
                 ;; Clojure/EDN reader can't parse namespaced keywords with a name starting with a digit
                 ;; (e.g. :crypto/1INCH), but such keywords can exist at runtime.
                 (when (keyword? cid)
                   (let [n (name cid)]
                     (and (some? (namespace cid))
                          (pos? (count n))
                          (Character/isDigit ^Character (.charAt ^String n 0))))))
               (currency-id->edn [cid]
                 (if (edn-unreadable-currency-id? cid)
                   (str (namespace cid) "/" (name cid))
                   cid))]
       (let [pks (or (sort-kw-vec (get-in registry [:ext :propagate-keys])) [])]
         (sorted-map-by
          #(compare %2 %1)
          :version     (. (LocalDateTime/now) format (DateTimeFormatter/ofPattern "yyyyMMddHHmmssSS"))
          :propagate-keys pks
          :localized   (into (sorted-map-by compare-by-str)
                             (map (fn [[cid lp]]
                                    (vector (currency-id->edn cid) (localized->map lp))))
                             (or (:cur-id->localized registry) {}))
          :weights     (into (sorted-map-by compare-by-str)
                             (map (fn [[cid w]] (vector (currency-id->edn cid) w)))
                             (or (:cur-id->weight registry) {}))
          :traits      (into (sorted-map-by compare-by-str)
                             (keep (fn [[cid ts]]
                                     (when-some [ts (traits->map ts)]
                                       (vector (currency-id->edn cid) ts))))
                             (or (:cur-id->traits registry) {}))
          :currencies  (into (sorted-map-by compare-by-str)
                             (map (fn [[cid c]]
                                    (vector (currency-id->edn cid) (currency->map c))))
                             (or (:cur-id->cur registry) {}))
          :countries   (into (sorted-map)
                             (map (fn [[country-id ^Currency cur]]
                                    (vector country-id (currency-id->edn (.id cur)))))
                             (or (:ctr-id->cur registry) {}))
          :hierarchies (into (sorted-map) (map/map-vals hierarchy->parent-map (:hierarchies registry))))))))))

(defn registry->map-currency-oriented
  "Like `registry->map`, but produces a currency-oriented configuration map by
  embedding per-currency properties (countries, localized properties, traits) into
  the currency maps. Orphaned top-level entries are kept."
  {:tag clojure.lang.IPersistentMap :added "2.0.0"}
  ([]
   (registry->map-currency-oriented (registry/state)))
  ([registry]
   (map->currency-oriented (registry->map registry))))

(defn- default-registry
  "Returns the default registry as used by `io.randomseed.bankster.registry/get`,
  without relying on macroexpansion (helps code coverage and keeps behavior explicit)."
  {:tag Registry :added "2.0.0" :private true}
  []
  (or registry/*default* (registry/state)))

(defn dump
  "For the given filename (defaults to default-dump-filename) and a registry (defaults
  to a global registry) creates a dump in EDN format.

  Filename will be placed in the default directory of resources (the same directory
  as `config.edn`)."
  {:added "1.0.0"}
  [& args]
  (let [[filename registry] (case (count args)
                              0 [default-dump-filename (default-registry)]
                              1 [default-dump-filename (first args)]
                              2 [(first args) (second args)]
                              (throw (ex-info "Invalid arity." {:fn `dump :args args})))]
    (when-some [rdir (fs/resource-pathname default-resource-name
                                           default-resource-must-exist-file)]
      (let [pathname (io/file (.getParent ^java.io.File (io/file rdir)) filename)]
        (println "Dumping registry to" (str pathname))
        (spit pathname (puget/pprint-str registry))))))

(defn export
  "For the given filename (defaults to default-export-filename) and a registry (defaults
  to a global registry) creates a configuration file in EDN format.

  Filename will be placed in the default directory of resources (the same directory
  that holds `config.edn`)."
  {:added "1.0.0"}
  [& args]
  (let [[filename registry] (case (count args)
                              0 [default-export-filename (default-registry)]
                              1 [default-export-filename (first args)]
                              2 [(first args) (second args)]
                              (throw (ex-info "Invalid arity." {:fn `export :args args})))]
    (when-some [rdir (fs/resource-pathname default-resource-name
                                           default-resource-must-exist-file)]
      (let [pathname (io/file (.getParent ^java.io.File (io/file rdir)) filename)]
        ;; (println "Exporting configuration to" (str pathname))
        (spit pathname (puget/pprint-str (registry->map registry)))))))

(defn export-currency-oriented
  "For the given filename (defaults to default-export-currency-oriented-filename) and a
  registry (defaults to a global registry) creates a currency-oriented configuration
  file in EDN format.

  Currency-oriented export embeds per-currency properties (countries, localized
  properties and traits) into the maps under `:currencies`. Top-level branches keep
  only orphaned entries."
  {:added "2.0.0"}
  [& args]
  (let [[filename registry] (case (count args)
                              0 [default-export-currency-oriented-filename (default-registry)]
                              1 [default-export-currency-oriented-filename (first args)]
                              2 [(first args) (second args)]
                              (throw (ex-info "Invalid arity." {:fn `export-currency-oriented :args args})))]
    (when-some [rdir (fs/resource-pathname default-resource-name
                                           default-resource-must-exist-file)]
      (let [pathname (io/file (.getParent ^java.io.File (io/file rdir)) filename)]
        (spit pathname (puget/pprint-str (registry->map-currency-oriented registry)))))))

;;
;; Readers generator.
;;

(defn handler-preamble
  "Preamble generator for a handler file."
  {:no-doc true :added "1.0.0"}
  ([]
   (handler-preamble default-handlers-namespace))
  ([handlers-namespace]
   (let [nsp (symbol (str "'" handlers-namespace))]
     (list 'in-ns nsp))))

(defn handler-gen-for-prefix
  {:private true :added "1.0.0"}
  [prefix names]
  (map
   (fn [n]
     (list 'defn (symbol (str prefix "-" n))
           '{:no-doc true}
           '[arg] (list (symbol (str "ns-" prefix)) (str n) 'arg)))
   names))

(defn handler-gen
  "Generates handler functions for tagged literals for each namespaced currency. Each
  function will have a prefixed name."
  [names]
  (concat (handler-gen-for-prefix "code-literal" names)
          (handler-gen-for-prefix "data-literal" names)))

(defn readers-export
  "Creates clojure source code files with reader functions for tagged literals handling
  on a basis of registry information and data reader map files referring to the
  created handlers.

  The purpose of generation is primarily to create handlers for literals in forms of
  #money/NS[…], where NS is a namespace that corresponds to a namespace of a
  currency. Possible namespaces are taken from a registry (a map from its field
  .cur-id->cur).

  The function takes a registry (defaults to a global registry if not given), a
  sequence of reader filenames (defaults to default-reader-filenames), default
  handlers pathname (defaults to default-handlers-pathname) and default handlers
  namespace (defaults to default-handlers-namespace).

  Default namespace is a namespace in which money handlers will be defined. These
  handlers will be written to a file which pathname is constructed using the
  following tactic:

  1. Obtain the directory of the first filename from the given filenames list using
     Java's resource lookup. The assumption is it should be src directory of
     a project.

  2. Append the file path passed as the handlers-pathname.

  As for data reader map files, their directory name is also based on the lookup of
  the first filename. Each filename will be populated with the same content which is
  a map associating tagged literal with a function."
  {:added "1.0.0"}
  ([]
   (readers-export (registry/state)
                   default-reader-filenames
                   default-data-reader-filename
                   default-handlers-pathname
                   default-handlers-namespace))
  ([registry]
   (readers-export registry
                   default-reader-filenames
                   default-data-reader-filename
                   default-handlers-pathname
                   default-handlers-namespace))
  ([registry filenames]
   (readers-export registry
                   filenames
                   default-data-reader-filename
                   default-handlers-pathname
                   default-handlers-namespace))
  ([registry filenames data-filename]
   (readers-export registry
                   filenames
                   data-filename
                   default-handlers-pathname
                   default-handlers-namespace))
  ([registry filenames data-filename handlers-pathname handlers-namespace]
   (when-some [nsses (->> (.cur-id->cur ^Registry registry)
                          (map (comp namespace first))
                          (filter identity)
                          set seq)]
     (let [money-tag-nses ["money" "bankster.money"]
           m  (->> nsses
                   (mapcat (fn [ns]
                             (for [tag-ns money-tag-nses]
                               [(symbol tag-ns ns)
                                (symbol handlers-namespace (str "code-literal-" ns))])))
                   (into {'money            'io.randomseed.bankster.money/code-literal
                          'bankster.money   'io.randomseed.bankster.money/code-literal
                          'currency         'io.randomseed.bankster.currency/code-literal
                          'bankster.currency 'io.randomseed.bankster.currency/code-literal}))
           dm (->> nsses
                   (mapcat (fn [ns]
                             (for [tag-ns money-tag-nses]
                               [(symbol tag-ns ns)
                                (symbol handlers-namespace (str "data-literal-" ns))])))
                   (into {'money            'io.randomseed.bankster.money/data-literal
                          'bankster.money   'io.randomseed.bankster.money/data-literal
                          'currency         'io.randomseed.bankster.currency/data-literal
                          'bankster.currency 'io.randomseed.bankster.currency/data-literal}))]
       (when-some [fdir (io/resource (first filenames))]
         (when-some [pdir (.getParent (io/file fdir))]
           (when-some [hfile (io/file pdir handlers-pathname)]
             (println)
             (println "------------- data readers map (for handling Clojure code):")
             (println)
             (puget/cprint m)
             (println)
             (println "------------- data readers map (for handling EDN data):")
             (println)
             (puget/cprint dm)
             (println)
             (reduce (fn [_ f]
                       (let [fname (io/file pdir f)]
                         (println "Exporting to:" (str fname))
                         (spit fname (puget/pprint-str m))))
                     nil
                     filenames)
             (when (some? (seq data-filename))
               (when-some [fname (io/file pdir data-filename)]
                 (println "Exporting to:" (str fname))
                 (spit fname (puget/pprint-str dm))))
             (println)
             (println "Generating handlers code to:" (str hfile))
             (->> nsses
                  (handler-gen)
                  (cons (handler-preamble handlers-namespace))
                  (map puget/pprint-str)
                  (str/join (str \newline \newline))
                  (spit hfile)))))))))

;;
;; High-level operations.
;;

(def ^{:const true :tag String :private true :added "2.0.0"}
  default-seed-resource-path
  "Default seed data resource file – relative path."
  (str default-resource-name "/" import-seed))

(defn seed-import
  "Loads seed data from an EDN resource file (defaults to `seed.edn`) and returns a
  registry."
  {:tag Registry :added "2.0.0"}
  (^Registry []
   (seed-import nil))
  (^Registry [^String resource-path]
   (currency/config->registry (or resource-path default-seed-resource-path)
                              (registry/new-registry))))

(defn- localized->register-input
  "Converts internal registry representation of localized properties (Locale keys)
  into the form accepted by currency/register (keyword/string locale IDs)."
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [m]
  (when (and (map? m) (pos? (count m)))
    (map/map-keys (fn [k]
                    (if (identical? :* k)
                      :*
                      (keyword (str (l/locale k)))))
                  m)))

;; Weight is now a registry base (`:cur-id->weight`) and explicitness is represented
;; by key presence (including 0 values). There is no need to track explicitness via
;; Currency metadata for importer-level decisions.

(defn- hierarchy-map?
  "Returns `true` if `x` looks like a hierarchy map produced by `make-hierarchy`."
  {:tag Boolean :added "2.0.0" :private true}
  [x]
  (and (map? x)
       (contains? x :parents)
       (contains? x :ancestors)
       (contains? x :descendants)
       (map? (clojure.core/get x :parents))
       (map? (clojure.core/get x :ancestors))
       (map? (clojure.core/get x :descendants))))

(defn- parent-map->hierarchy
  "Builds a hierarchy map out of a \"parent map\" (child -> parent/parents).

  The map value may be:
  - a single parent (keyword/symbol/class), or
  - a set/vector/list/seq of parents (multiple inheritance)."
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [^clojure.lang.IPersistentMap rels]
  (reduce (fn [h [child parent]]
            (let [parents (cond
                            (set? parent)        (sort-by str parent)
                            (sequential? parent) parent
                            :else                (list parent))]
              (reduce (fn [h p] (derive h child p)) h parents)))
          (make-hierarchy)
          ;; Stable order makes failures deterministic (cycles, invalid derives, etc.).
          (sort-by (fn [[child parent]]
                     (let [parent (cond
                                    (set? parent)        (sort-by str parent)
                                    (sequential? parent) parent
                                    :else                parent)]
                       (str child "->" parent)))
                   rels)))

(defn- ->hierarchy
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [spec hierarchy-type]
  (cond
    (nil? spec)
    (make-hierarchy)

    (hierarchy-map? spec)
    spec

    (map? spec)
    (parent-map->hierarchy spec)

    :else
    (throw
     (ex-info
      "Invalid currency hierarchy specification."
      {:type  hierarchy-type
       :value spec}))))

(defn- merge-hierarchy
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [dst-h src-h hierarchy-type]
  (let [dst-h   (->hierarchy dst-h hierarchy-type)
        src-h   (->hierarchy src-h hierarchy-type)
        dst-rel (clojure.core/get dst-h :parents)
        src-rel (clojure.core/get src-h :parents)
        rels    (merge-with into dst-rel src-rel)]
    (parent-map->hierarchy rels)))

(defn- merge-hierarchies
  {:tag io.randomseed.bankster.CurrencyHierarchies :added "2.0.0" :private true}
  [dst-h src-h]
  (let [dst-h (or dst-h {})
        src-h (or src-h {})
        ks    (into (set (keys dst-h)) (keys src-h))]
    (bankster/map->CurrencyHierarchies
     (reduce (fn [m k]
               (assoc m k (merge-hierarchy (clojure.core/get dst-h k)
                                           (clojure.core/get src-h k)
                                           k)))
             {}
             ks))))

(defn merge-registry
  "Merges two registries by registering currencies from `src` into `dst`.

  Hierarchies (stored in `:hierarchies`) and extension data (stored in `:ext`) are
  merged as well.

  When `verbose?` is truthy it prints a message for each currency which is present in
  `src` but not in `dst`.

  When `preserve-fields` is given (a sequence of currency record keys, e.g.
  `[:domain :kind]`) and a currency is being replaced in `dst`, the values of these
  fields are preserved from the original currency in `dst`.

  Special sentinel keywords may be included in `preserve-fields`:

  - `::localized`  preserve localized properties from `dst`,
  - `::countries`  preserve assigned countries from `dst`.

  When `iso-like?` is truthy and the source currency is ISO-like (domain
  `:ISO-4217` or `:ISO-4217-LEGACY`) then the currency identity is treated as its
  ISO code (name part of the ID). If the source currency is a legacy ISO currency
  then its destination ID is normalized to `:iso-4217-legacy/CODE` and it replaces
  a previously existing `:CODE` entry (including migration of attached country
  mappings and localized properties).

  Note: in ISO-like mode `:domain` is never preserved from `dst` for ISO-like
  currencies (even if present in `preserve-fields`), to allow aligning ISO vs
  legacy ISO classification based on the source.

  Legacy currency weight: when a legacy currency has weight 0 and the weight was not
  explicitly set (presence in `:cur-id->weight`) in either source or destination,
  it is set to `default-legacy-weight`. Explicit weight 0 is preserved."
  {:tag Registry :added "2.0.0"}
  (^Registry [^Registry dst ^Registry src]
   (merge-registry dst src false nil false))
  (^Registry [^Registry dst ^Registry src verbose?]
   (merge-registry dst src verbose? nil false))
  (^Registry [^Registry dst ^Registry src verbose? preserve-fields]
   (merge-registry dst src verbose? preserve-fields false))
  (^Registry [^Registry dst ^Registry src verbose? preserve-fields iso-like?]
   (let [preserve-fields       (set preserve-fields)
         preserve-localized?   (contains? preserve-fields ::localized)
         preserve-countries?   (contains? preserve-fields ::countries)
         preserve-fields       (disj preserve-fields ::localized ::countries :id)
         ^Registry dst         (or dst (registry/new-registry))
         ^Registry src         (or src (registry/new-registry))
         merged-hierarchies    (merge-hierarchies (:hierarchies dst) (:hierarchies src))
         merged-ext            (merge (:ext dst) (:ext src))
         merged-weights        (merge (:cur-id->weight dst) (:cur-id->weight src))
         ^Registry dst         (assoc dst :hierarchies merged-hierarchies :ext merged-ext :cur-id->weight merged-weights)
         src-cur-id->cur       (:cur-id->cur src)
         src-cur-id->ctr-ids   (:cur-id->ctr-ids src)
         src-cur-id->localized (:cur-id->localized src)
         src-cur-id->traits    (:cur-id->traits src)]
     (reduce (fn ^Registry [^Registry r [_cid ^Currency c]]
               (let [d              (.domain ^Currency c)
                     iso-like?      (boolean
                                     (and iso-like?
                                          (or (identical? d :ISO-4217)
                                              (identical? d :ISO-4217-LEGACY))))
                     src-id         (.id ^Currency c)
                     iso-code-id    (when iso-like? (keyword (name src-id)))
                     iso-legacy-id  (when iso-like? (keyword "iso-4217-legacy" (name src-id)))
                     legacy?        (and iso-like? (identical? d :ISO-4217-LEGACY))
                     legacy-domain? (identical? d :ISO-4217-LEGACY)
                     dst-id         (if legacy? iso-legacy-id (or iso-code-id src-id))
                     alt-id         (when iso-like? (if legacy? iso-code-id iso-legacy-id))
                     ^Currency c    (if (and iso-like? (not= src-id dst-id)) (assoc c :id dst-id) c)
                     existing       (get (:cur-id->cur r) dst-id)
                     existing       (if (or existing (not iso-like?)) existing (get (:cur-id->cur r) alt-id))
                     existing-id    (when existing (.id ^Currency existing))
                     rename?        (and iso-like? (some? existing) (not= existing-id dst-id))
                     src-countries  (or (clojure.core/get src-cur-id->ctr-ids dst-id)
                                        (when iso-like? (clojure.core/get src-cur-id->ctr-ids iso-code-id)))
                     src-localized  (or (clojure.core/get src-cur-id->localized dst-id)
                                        (when iso-like? (clojure.core/get src-cur-id->localized iso-code-id)))
                     src-traits     (or (clojure.core/get src-cur-id->traits dst-id)
                                        (when iso-like? (clojure.core/get src-cur-id->traits iso-code-id)))]
                 (if existing
                   (let [existing-id        (or existing-id dst-id)
                         existing-countries (clojure.core/get (:cur-id->ctr-ids r) existing-id)
                         existing-localized (clojure.core/get (:cur-id->localized r) existing-id)
                         existing-traits    (clojure.core/get (:cur-id->traits r) existing-id)
                         ;; Weight base (source of truth): currency ID -> int weight.
                         src-id->weight      (or (:cur-id->weight src) {})
                         dst-id->weight      (or (:cur-id->weight r) {})
                         src-w?              (or (contains? src-id->weight dst-id)
                                                 (and iso-like? (contains? src-id->weight iso-code-id)))
                         src-w               (int (or (clojure.core/get src-id->weight dst-id)
                                                     (when iso-like? (clojure.core/get src-id->weight iso-code-id))
                                                     0))
                         ;; Transitional fallback: treat non-zero currency meta weight as explicit.
                         src-w-hint          (int (currency/weight c))
                         src-exp?            (or src-w? (not (zero? src-w-hint)))
                         src-wv              (if src-w? src-w src-w-hint)

                         dst-w?              (contains? dst-id->weight existing-id)
                         dst-w               (int (or (clojure.core/get dst-id->weight existing-id) 0))
                         dst-w-hint          (int (currency/weight existing))
                         dst-exp?            (or dst-w? (not (zero? dst-w-hint)))
                         dst-wv              (if dst-w? dst-w dst-w-hint)

                         base-w              (cond src-exp? src-wv
                                                   dst-exp? dst-wv
                                                   :else    0)
                         base-exp?           (or src-exp? dst-exp?)
                         final-w             (if (and legacy-domain? (zero? base-w) (not base-exp?))
                                               (int default-legacy-weight)
                                               (int base-w))
                         final-exp?          (or base-exp? (not (zero? final-w)))
                         weight-changed?     (or rename?
                                                 (not= final-w (int dst-wv))
                                                 (not= final-exp? (boolean dst-exp?)))
                         preserve-fields    (if iso-like? (disj preserve-fields :domain) preserve-fields)
                         preserve-fields    (filter #(contains? existing %) preserve-fields)
                         ^Currency c        (if (seq preserve-fields)
                                              (apply assoc c
                                                     (mapcat (fn [k] [k (clojure.core/get existing k)])
                                                             preserve-fields))
                                              c)
                         ^Currency c        (if final-exp? (currency/with-weight c final-w) c)
                         countries          (if rename?
                                              (when (or (seq existing-countries) (seq src-countries))
                                                (into (or existing-countries #{}) src-countries))
                                              (if preserve-countries?
                                                existing-countries
                                                src-countries))
                         localized          (if rename?
                                              (merge-with merge existing-localized src-localized)
                                              (if preserve-localized?
                                                existing-localized
                                                src-localized))
                         ;; Traits are merged additively (union), to avoid wiping dst metadata
                         ;; when src has no trait information (e.g. Joda importer).
                         traits             (let [ts (into (or existing-traits #{}) src-traits)]
                                              (cond-> ts legacy-domain? (conj :legacy)))
                         localized-input    (localized->register-input localized)
                         updated? (or rename?
                                      (not= c existing)
                                      weight-changed?
                                      (not= countries existing-countries)
                                      (not= localized existing-localized))
                         ^Registry r (if updated?
                                      (do (when verbose?
                                            (println "Updated currency:" (symbol (:id c))))
                                          (let [^Registry r (if rename?
                                                              (currency/unregister r existing)
                                                              r)]
                                            (currency/register r c countries localized-input (not rename?))))
                                      r)
                         ^Registry r (if rename?
                                      (clojure.core/update r :cur-id->traits dissoc existing-id)
                                      r)]
                     (if (seq traits)
                       (assoc-in r [:cur-id->traits dst-id] traits)
                       r))
                   (do (when (and verbose? (some? c))
                         (println "New currency:" (symbol (:id c))))
                      (let [src-id->weight (or (:cur-id->weight src) {})
                            src-w?         (or (contains? src-id->weight dst-id)
                                               (and iso-like? (contains? src-id->weight iso-code-id)))
                            src-w          (int (or (clojure.core/get src-id->weight dst-id)
                                                   (when iso-like? (clojure.core/get src-id->weight iso-code-id))
                                                   0))
                            src-w-hint     (int (currency/weight c))
                            src-exp?       (or src-w? (not (zero? src-w-hint)))
                            src-wv         (if src-w? src-w src-w-hint)
                            base-w         (if src-exp? src-wv 0)
                            base-exp?      (boolean src-exp?)
                            final-w        (if (and legacy-domain? (zero? base-w) (not base-exp?))
                                             (int default-legacy-weight)
                                             (int base-w))
                            final-exp?     (or base-exp? (not (zero? final-w)))
                            ^Currency c    (if final-exp? (currency/with-weight c final-w) c)
                            traits         (let [ts (into #{} src-traits)]
                                             (cond-> ts legacy-domain? (conj :legacy)))
                            ^Registry r    (currency/register r
                                                              c
                                                              src-countries
                                                              (localized->register-input src-localized)
                                                              false)]
                        (if (seq traits)
                          (assoc-in r [:cur-id->traits dst-id] traits)
                          r))))))
             dst
             src-cur-id->cur))))

(defn joda->bankster-dump
  "Reads Joda Money CSV files and creates a registry dump named
  resources/io/randomseed/bankster/registry-dump.edn."
  {:added "1.0.0"}
  []
  (let [dst (seed-import)
        jda (joda-import)]
    (dump (merge-registry dst jda true [:domain :kind ::localized] true))))

(defn joda->bankster-export
  "Reads Joda Money CSV files and creates a configuration file named
  resources/io/randomseed/bankster/registry-export.edn."
  {:added "1.0.0"}
  []
  (let [dst (seed-import)
        jda (joda-import)
        reg (merge-registry dst jda true [:domain :kind ::localized] true)]
    (export reg)
    (export-currency-oriented reg)))
