(ns io.randomseed.bankster.test-main
  (:require [io.randomseed.bankster.cloverage-workaround :as cov-workaround]
            [kaocha.runner                               :as runner]))

(defn -main
  [& args]
  (cov-workaround/install!)
  (apply runner/-main args))
