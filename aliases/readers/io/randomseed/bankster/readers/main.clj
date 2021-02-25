(ns io.randomseed.bankster.readers.main
  (:require [io.randomseed.bankster.util.importer :as imp]))

(defn gen-readers
  "Generates tagged literal handler maps and handlers for namespaced currencies."
  []
  (imp/readers-export))

(defn -main
  [& args]
  (gen-readers))
