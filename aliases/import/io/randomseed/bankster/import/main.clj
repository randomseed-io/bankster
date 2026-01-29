(ns io.randomseed.bankster.import.main
  (:require [io.randomseed.bankster.util.importer :as imp]))

(defn import-export
  "Reads CSV files with currency and country data, and writes out EDN files."
  []
  (imp/joda->bankster-export))

(defn -main
  [& args]
  (import-export))
