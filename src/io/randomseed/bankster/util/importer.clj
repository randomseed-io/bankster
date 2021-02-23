(ns io.randomseed.bankster.util.importer

  ^{:doc    "Bankster library, import-export operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [clojure.java.io                 :as       io]
            [clojure.data.csv                :as      csv]
            [clojure.string                  :as      str]
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

(def ^String ^private ^const default-resource-name
  "Name of a default resource container."
  "io/randomseed/bankster")

(def ^String ^private ^const default-dump-filename
  "Default EDN dump file."
  "registry-dump.edn")

(def ^String ^private ^const default-export-filename
  "Default EDN export file."
  "registry-export.edn")

(def ^String ^private ^const default-reader-filenames
  "Default data reader filenames."
  ["data_readers.clj" "data_readers.cljc"])

(def ^String ^private ^const default-handlers-pathname
  "Default pathname of a reader handlers file."
  "io/randomseed/bankster/money/reader_handlers.clj")

(def ^String ^private ^const default-handlers-namespace
  "Default namespace of a reader handlers."
  "io.randomseed.bankster.money.reader-handlers")

(def ^String ^private ^const default-countries-csv
  "Default CSV file with country database."
  "org/joda/money/CountryData.csv")

(def ^String ^private ^const default-currencies-csv
  "Default CSV file with country database."
  "org/joda/money/CurrencyData.csv")

;;
;; Transformation rules.
;;

(def special-kinds
  "ISO codes for special currencies."
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
  "Shapes currency entry. Gets a sequence of linear collections describing currency and
  returns a currency object."
  [[id numeric scale]]
  (when (some? id)
    (let [id       (keyword id)
          numeric  (or (try-parse-long numeric) currency/no-numeric-id)
          numeric  (if (< numeric 0) currency/no-numeric-id numeric)
          scale    (or (try-parse-int scale) currency/auto-scaled)
          scale    (if (< scale 0) currency/auto-scaled scale)
          kind     (get special-kinds id :FIAT)]
      (currency/new-currency id (long numeric) (int scale) kind))))

;;
;; Joda Money CSV importer.
;;

(defn ^clojure.lang.PersistentHashMap countries-load
  "Reads CSV file in a format compliant with Joda Money and returns a map with currency
  to countries associations where countries are as sets. The pathname should be
  relative to resources directory."
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
  ([]
   (currencies-load nil))
  ([^String pathname]
   (when-some [f (fs/paths->resource (or pathname default-currencies-csv))]
     (->> f fs/read-csv (map make-currency)))))

(defn joda-import
  "Reads CSV files defining countries and currencies (Joda Money format) and returns a
  Bankster-suitable structure."
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
  [{:keys [:nr :sc :kind]}]
  (as-> (sorted-map) m
    (if (and (number? nr) (pos? nr))   (assoc m :numeric nr) m)
    (if-not (and (some? sc) (neg? sc)) (assoc m :scale   sc) m)
    (if (some? kind)                   (assoc m :kind  kind) m)))

(defn registry->map
  ([]
   (registry->map (registry/state)))
  ([^Registry registry]
   (when (some? registry)
     (sorted-map-by
      #(compare %2 %1)
      :version    (. (LocalDateTime/now) format (DateTimeFormatter/ofPattern "YYYYMMddHHmmssSS"))
      :currencies (into (sorted-map) (map/map-vals currency->map (:cur-id->cur registry)))
      :countries  (into (sorted-map) (map/map-vals :id (:ctr-id->cur registry)))))))

(defn dump
  ([^Registry registry]
   (dump default-dump-filename registry))
  ([^String   filename
    ^Registry registry]
   (when-some [rdir (fs/resource-pathname default-resource-name)]
     (spit (io/file rdir filename) (puget/pprint-str registry)))))

(defn export
  ([^Registry registry]
   (export default-export-filename registry))
  ([^String   filename
    ^Registry registry]
   (when-some [rdir (fs/resource-pathname default-resource-name)]
     (spit (io/file rdir filename) (puget/pprint-str (registry->map registry))))))

;;
;; Readers generator.
;;

(defn handler-preamble
  ([]
   (handler-preamble default-handlers-namespace))
  ([handlers-namespace]
   `(ns ~(symbol handlers-namespace))))

(defn handler-gen
  [names]
  (map
   (fn [n]
     (list 'defn (symbol (str "funds-" n))
           '[[a b]]
           (list 'let '[[c am] (if (number? a) [b a] [a b])]
                 (list 'io.randomseed.bankster.money/funds
                       (list 'keyword (str n) '(str (symbol c))) 'am))))
   names))

(defn readers-export
  ([]
   (readers-export (registry/state) default-reader-filenames default-handlers-pathname default-handlers-namespace))
  ([^Registry registry]
   (readers-export registry default-reader-filenames default-handlers-pathname default-handlers-namespace))
  ([^Registry registry filenames]
   (readers-export registry filenames default-handlers-pathname default-handlers-namespace))
  ([^Registry registry filenames handlers-pathname handlers-namespace]
   (when-some [nsses (->> (.cur-id->cur ^Registry registry)
                          (map (comp namespace first))
                          (filter identity)
                          set seq)]
     (let [m (->> nsses
                  (map #(vector (symbol "money" %) (symbol handlers-namespace (str "funds-" %))))
                  (into {'money    'io.randomseed.bankster.money/funds
                         'currency 'io.randomseed.bankster.currency/unit}))]
       (when-some [fdir (io/resource (first default-reader-filenames))]
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
  []
  (println (time (dump (joda-import)))))

(defn joda->bankster-export
  []
  (println (time (export (joda-import)))))
