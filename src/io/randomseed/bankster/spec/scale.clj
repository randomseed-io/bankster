;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Scale specs for Bankster."
      :author "Paweł Wilk"
      :added  "2.2.3"
      :no-doc true}

    io.randomseed.bankster.spec.scale

  (:require [io.randomseed.bankster       :as        bankster]
            [io.randomseed.bankster.scale :as           scale]
            [clojure.spec.alpha           :as               s]
            [orchestra.spec.test          :as              st]
            [clojure.spec.gen.alpha       :as             gen]))

;;
;; Scale specs
;;

;; Automatic scale sentinel (-1)
(s/def ::auto-scaled
  (s/with-gen
    #(and (integer? %) (== -1 (int %)))
    (fn [] (gen/return -1))))
