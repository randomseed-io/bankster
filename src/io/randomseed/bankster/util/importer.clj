(ns io.randomseed.bankster.util.importer

  ^{:doc    "Bankster library, import-export operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [clojure.java.io                 :as       io]
            [clojure.data.csv                :as      csv]
            [clojure.edn                     :as      edn]
            [puget.printer                   :as    puget]
            [io.randomseed.bankster.scale    :as    scale]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.util.fs  :as       fs]
            [io.randomseed.bankster.util.map :as      map]
            [io.randomseed.bankster.util     :refer  :all])

  (:import  [io.randomseed.bankster Currency Registry]))

;;
;; Pathnames and URIs.
;;

(def ^String ^private ^const default-resource-name
  "Name of a default resource container."
  "io/randomseed/bankster/db")

(def ^String ^private ^const default-dump-filename
  "Default EDN dump file."
  "registry-dump.edn")

(def ^String ^private ^const default-export-filename
  "Default EDN export file."
  "registry-export.edn")

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
          numeric  (or (fs/try-parse-long numeric) currency/no-numeric-id)
          numeric  (if (< numeric 0) currency/no-numeric-id numeric)
          scale    (or (fs/try-parse-int scale) currency/auto-scaled)
          scale    (if (< scale 0) currency/auto-scaled scale)
          kind     (get special-kinds id :FIAT)]
      (apply currency/new-currency [id (long numeric) (int scale) kind]))))

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
     (spit (io/file rdir filename) (currency/export-registry registry)))))

;;
;; High-level operations.
;;

(defn joda->bankster-dump
  []
  (println (time (dump (joda-import)))))
