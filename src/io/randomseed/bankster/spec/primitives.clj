;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Primitive specs for Bankster (sentinels, basic types)."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.spec.primitives

  (:require [io.randomseed.bankster            :as        bankster]
            [io.randomseed.bankster.spec.scale :as      spec.scale]
            [clojure.spec.alpha                :as               s]
            [clojure.spec.gen.alpha            :as             gen]))

;;
;; Namespaces for easy use of keywords
;;

(alias 'scale (create-ns 'io.randomseed.bankster.spec.scale))  ;; for scale-related specs

;;
;; Sentinels
;;
;; Note: ::scale/auto-scaled is defined in io.randomseed.bankster.spec.scale

;; No numeric ID sentinel (-1)
(s/def ::no-numeric-id
  (s/with-gen
    #(and (integer? %) (== -1 (long %)))
    (fn [] (gen/return -1))))

;;
;; Composite specs using sentinels
;;

;; Currency scale: either a non-negative integer or the auto-scaled sentinel (-1).
(s/def ::currency-scale
  (s/with-gen
    (s/or :auto-scaled ::scale/auto-scaled
          :fixed-scale (s/and integer? (complement neg?)))
    #(gen/one-of [(s/gen ::scale/auto-scaled)
                  (gen/large-integer* {:min 0 :max 20})])))

;; Currency numeric ID: either a positive long or the no-numeric-id sentinel (-1).
(s/def ::currency-numeric-id
  (s/with-gen
    (s/or :no-numeric ::no-numeric-id
          :numeric-id (s/and integer? pos?))
    #(gen/one-of [(s/gen ::no-numeric-id)
                  (gen/large-integer* {:min 1 :max 9999})])))
