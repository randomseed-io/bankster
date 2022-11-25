(ns user

  (:require
   [clojure.spec.alpha                     :as                s]
   [orchestra.spec.test                    :as               st]
   [clojure.spec.test.alpha                :as              cst]
   [clojure.spec.gen.alpha                 :as              gen]
   [clojure.repl                           :refer          :all]
   [clojure.test                           :refer [run-tests
                                                   run-all-tests]]
   [clojure.tools.namespace.repl           :refer [refresh
                                                   refresh-all]]
   [expound.alpha                          :as          expound]

   [io.randomseed.bankster                 :as         bankster]
   [io.randomseed.bankster.scale           :as            scale]
   [io.randomseed.bankster.config          :as           config]
   [io.randomseed.bankster.registry        :as         registry]
   [io.randomseed.bankster.currency        :as         currency]
   [io.randomseed.bankster.money           :as            money]
   [io.randomseed.bankster.util            :as             util]
   [io.randomseed.bankster.util.fs         :as               fs]
   [io.randomseed.bankster.util.map        :as              map]
   [io.randomseed.bankster.util.importer   :as              imp]
   [io.randomseed.bankster.spec            :as             spec]

   [puget.printer                          :refer      [cprint]]
   [kaocha.repl                            :refer          :all]
   [infra])

  (:import [io.randomseed.bankster Registry Currency Money]))

(set! *warn-on-reflection* true)

(alter-var-root
 #'s/*explain-out*
 (constantly
  (expound/custom-printer {:show-valid-values? false
                           :print-specs?        true
                           :theme    :figwheel-theme})))

(when (System/getProperty "nrepl.load")
  (require 'nrepl))

(st/instrument)

(defn test-all []
  (refresh)
  (cst/with-instrument-disabled
    (run-all-tests)))

(alter-var-root #'*warn-on-reflection*
                (constantly true)
                (when (thread-bound? #'*warn-on-reflection*)
                  (set! *warn-on-reflection* true)))

(comment 
  (refresh-all)
  (cst/with-instrument-disabled (test-all))
  (cst/with-instrument-disabled (run-all))
  )
