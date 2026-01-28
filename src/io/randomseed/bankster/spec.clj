(ns

    ^{:doc    "Public specs of Bankster."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.spec

  (:require [io.randomseed.bankster       :as        bankster]
            [io.randomseed.bankster.util  :as            util]
            [clojure.spec.alpha           :as               s]
            [orchestra.spec.test          :as              st]
            [clojure.spec.gen.alpha       :as             gen]))

;;
;; Namespaces for easy use of keywords
;;

(alias 'arg   (create-ns 'bankster.arg))   ;; for internal arg specs
(alias 'args  (create-ns 'bankster.args))  ;; for internal args specs
(alias 'prop  (create-ns 'bankster.prop))  ;; for additional properties
