(ns io.randomseed.bankster.util.importer

  ^{:doc    "Bankster library, import-export operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [clojure.java.io                 :as       io]
            [clojure.data.csv                :as      csv]
            [clojure.string                  :as      str]
            [trptr.java-wrapper.locale       :as        l]
            [puget.printer                   :as    puget]
            [io.randomseed.bankster          :as bankster]
            [io.randomseed.bankster.scale    :as    scale]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.util.fs  :as       fs]
            [io.randomseed.bankster.util.map :as      map]
            [io.randomseed.bankster.util     :refer  :all])

  (:import  [io.randomseed.bankster Currency Registry]
            [java.time LocalDateTime format.DateTimeFormatter]))

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

(def ^{:const true :tag String :added "1.0.0"}
  default-dump-filename
  "Default EDN dump file."
  "registry-dump.edn")

(def ^{:const true :tag String :added "1.0.0"}
  default-export-filename
  "Default EDN export file."
  "registry-export.edn")

(def ^{:const true :tag String :added "1.0.0"}
  default-reader-filenames
  "Default data reader filenames."
  ["data_readers.clj" "data_readers.cljc"])

(def ^{:const true :tag String :added "1.0.0"}
  default-handlers-pathname
  "Default pathname of a reader handlers file."
  "io/randomseed/bankster/money/reader_handlers.clj")

(def ^{:const true :tag String :added "1.0.0"}
  default-handlers-namespace
  "Default namespace of a reader handlers."
  "io.randomseed.bankster.money")

(def ^{:const true :tag String :added "1.0.0"}
  default-countries-csv
  "Default CSV file with countries database."
  "org/joda/money/CountryData.csv")

(def ^{:const true :tag String :added "1.0.0"}
  default-currencies-csv
  "Default CSV file with currencies database."
  "org/joda/money/CurrencyData.csv")

;;
;; Transformation rules.
;;

(def ^{:private true :added "1.0.0"}
  special-kinds
  "ISO codes for special kinds of currencies."
  {:USN :FIDUCIARY
   :XSU :FIDUCIARY
   :CLF :FIDUCIARY
   :XUA :COMBANK
   :XTS :EXPERIMENTAL
   :XPT :COMMODITY
   :XPD :COMMODITY
   :XAU :COMMODITY
   :XAG :COMMODITY
   :XOF :FIAT
   :XPF :FIAT
   :XDR :FIDUCIARY
   :XBA :FIDUCIARY
   :XBB :FIDUCIARY
   :XBC :FIDUCIARY
   :XBD :FIDUCIARY
   :XXX nil})

(defn make-currency
  "Shapes an ISO-standardized currency entry. Gets a sequence of linear collections
  describing currency and returns a currency object."
  {:private true :added "1.0.0"}
  [[id numeric scale domain]]
  (when (some? id)
    (let [id      (keyword id)
          numeric (or (try-parse-long numeric) currency/no-numeric-id)
          numeric (if (< numeric 0) currency/no-numeric-id numeric)
          scale   (or (try-parse-int scale) currency/auto-scaled)
          scale   (if (< scale 0) currency/auto-scaled scale)
          kind    (get special-kinds id :FIAT)
          domain  :ISO-4217]
      (currency/new-currency id (long numeric) (int scale) kind domain))))

;;
;; Joda Money CSV importer.
;;

(defn ^clojure.lang.PersistentHashMap countries-load
  "Reads CSV file in a format compliant with Joda Money and returns a map with currency
  to countries associations where countries are as sets. The pathname should be
  relative to resources directory."
  {:added "1.0.0"}
  ([]
   (countries-load nil))
  ([^String pathname]
   (when-some [r (fs/paths->resource (or pathname default-countries-csv))]
     (->> r fs/read-csv
          (map (comp vec (partial map keyword)))
          (into {})
          (map/invert-in-sets)))))

(defn ^clojure.lang.PersistentHashMap currencies-load
  "Reads CSV file compliant with Joda Money and returns a map with currency
  ID (keyword) as a key and currency data as its value (vector). The pathname should
  be relative to resources directory."
  {:added "1.0.0"}
  ([]
   (currencies-load nil))
  ([^String pathname]
   (when-some [f (fs/paths->resource (or pathname default-currencies-csv))]
     (->> f fs/read-csv (map make-currency)))))

(defn joda-import
  "Reads CSV files with countries and currencies definitions (Joda Money format) and
  returns a registry."
  {:tag Registry :added "1.0.0"}
  ([]
   (joda-import nil nil))
  ([^String countries-pathname
    ^String currencies-pathname]
   (let [^Registry                       registry   (registry/new-registry)
         ^clojure.lang.PersistentHashMap countries  (countries-load countries-pathname)
         ^clojure.lang.PersistentHashMap currencies (currencies-load currencies-pathname)]
     (reduce (fn ^Registry [^Registry r, ^Currency c]
               (currency/register r c (get countries (currency/id c))))
             registry currencies))))

;;
;; EDN dumper and exporter.
;;

(defn currency->map
  "Takes a currency and returns a map suitable for putting into a configuration
  file. Extensions fields are ignored."
  {:added "1.0.0"}
  [{:keys [:numeric :scale :kind]}]
  (as-> (sorted-map) m
    (if (and (number? numeric) (pos? numeric)) (assoc m :numeric numeric) m)
    (if-not (and (some? scale) (neg? scale))   (assoc m :scale   scale)   m)
    (if (some? kind)                           (assoc m :kind    kind)    m)))

(defn localized->map
  "Takes a localized map entry (1st level) and returns a map suitable for putting into
  a configuration file."
  {:added "1.0.0"}
  [m]
  (map/map-keys (comp keyword str l/locale) m))

(defn registry->map
  "Takes a registry and returns a map suitable for putting into a configuration
  file. Extensions fields are ignored. When registry is not given it uses the global
  one. Extension fields are ignored."
  {:added "1.0.0"}
  ([]
   (registry->map (registry/state)))
  ([^Registry registry]
   (when (some? registry)
     (sorted-map-by
      #(compare %2 %1)
      :version    (. (LocalDateTime/now) format (DateTimeFormatter/ofPattern "YYYYMMddHHmmssSS"))
      :localized  (into (sorted-map) (map/map-vals localized->map (:cur-id->localized registry)))
      :currencies (into (sorted-map) (map/map-vals currency->map  (:cur-id->cur registry)))
      :countries  (into (sorted-map) (map/map-vals :id (:ctr-id->cur registry)))))))

(defn dump
  "For the given filename (defaults to default-dump-filename) and a registry (defaults
  to a global registry) creates a dump in EDN format.

  Filename will be placed in the default directory of resources (the same that which
  config.edn)."
  {:added "1.0.0"}
  ([]
   (dump default-dump-filename (registry/get)))
  ([^Registry registry]
   (dump default-dump-filename registry))
  ([^String   filename
    ^Registry registry]
   (when-some [rdir (fs/resource-pathname default-resource-name
                                          default-resource-must-exist-file)]
     (let [pathname (io/file (.getParent ^java.io.File (io/file rdir)) filename)]
       (println "Dumping registry to" (str pathname))
       (spit pathname (puget/pprint-str registry))))))

(defn export
  "For the given filename (defaults to default-dump-filename) and a registry (defaults
  to a global registry) creates a configuration file in EDN format.

  Filename will be placed in the default directory of resources (the same which holds
  config.edn)."
  {:added "1.0.0"}
  ([]
   (export default-export-filename (registry/get)))
  ([^Registry registry]
   (export default-export-filename registry))
  ([^String   filename
    ^Registry registry]
   (when-some [rdir (fs/resource-pathname default-resource-name
                                          default-resource-must-exist-file)]
     (let [pathname (io/file (.getParent ^java.io.File (io/file rdir)) filename)]
       (println "Exporting configuration to" (str pathname))
       (spit pathname (puget/pprint-str (registry->map registry)))))))

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

(defn handler-gen
  "Generates handler functions for tagged literals for each namespaced currency."
  {:no-doc true :added "1.0.0"}
  [names]
  (map
   (fn [n] (list 'defn (symbol (str "lit-" n))
                 '{:no-doc true}
                 '[arg] (list 'ns-lit (str n) 'arg)))
   names))

(defn readers-export
  "Creates clojure source code files with reader functions for tagged literals handling
  on a basis of registry information and data reader map files referring to the
  created handlers.

  The purpose of generation is primary to create handlers for literals in forms of
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
                   default-handlers-pathname
                   default-handlers-namespace))
  ([^Registry registry]
   (readers-export registry
                   default-reader-filenames
                   default-handlers-pathname
                   default-handlers-namespace))
  ([^Registry registry filenames]
   (readers-export registry
                   filenames
                   default-handlers-pathname
                   default-handlers-namespace))
  ([^Registry registry filenames handlers-pathname handlers-namespace]
   (when-some [nsses (->> (.cur-id->cur ^Registry registry)
                          (map (comp namespace first))
                          (filter identity)
                          set seq)]
     (let [m (->> nsses
                  (map #(vector (symbol "money" %) (symbol handlers-namespace (str "lit-" %))))
                  (into {'money    'io.randomseed.bankster.money/lit
                         'currency 'io.randomseed.bankster.currency/lit}))]
       (when-some [fdir (io/resource (first filenames))]
         (when-some [pdir (.getParent (io/file fdir))]
           (when-some [hfile (io/file pdir handlers-pathname)]
             (println)
             (println "------------------- readers map:")
             (println)
             (puget/cprint m)
             (println)
             (doseq [f filenames]
               (let [fname (io/file pdir f)]
                 (println "Exporting to:" (str fname))
                 (spit fname (puget/pprint-str m))))
             (println "Generating handlers code to:" (str hfile))
             (some->> nsses
                      handler-gen
                      (cons (handler-preamble handlers-namespace))
                      (map puget/pprint-str)
                      (str/join (str \newline \newline))
                      (spit hfile)))))))))

;;
;; High-level operations.
;;

(defn joda->bankster-dump
  "Reads Joda Money CSV files and creates a registry dump named
  resources/io/randomseed/bankster/registry-dump.edn."
  {:added "1.0.0"}
  []
  (println (time (dump (joda-import)))))

(defn joda->bankster-export
  "Reads Joda Money CSV files and creates a configuration file named
  resources/io/randomseed/bankster/registry-export.edn."
  {:added "1.0.0"}
  []
  (println (time (export (joda-import)))))
