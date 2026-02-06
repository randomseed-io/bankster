;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Public specs of Bankster."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.spec

  (:require [io.randomseed.bankster            :as        bankster]
            [io.randomseed.bankster.util       :as            util]
            [io.randomseed.bankster.spec.scale :as       spec.scale]
            [clojure.spec.alpha                :as               s]
            [orchestra.spec.test               :as              st]
            [clojure.spec.gen.alpha            :as             gen]))

;;
;; Namespaces for easy use of keywords
;;

(alias 'arg        (create-ns 'io.randomseed.bankster.arg))                   ;; for internal arg specs
(alias 'args       (create-ns 'io.randomseed.bankster.args))                  ;; for internal args specs
(alias 'prop       (create-ns 'io.randomseed.bankster.prop))                  ;; for additional properties
(alias 'scale      (create-ns 'io.randomseed.bankster.spec.scale))            ;; scale specs
(alias 'registry   (create-ns 'io.randomseed.bankster.Registry))              ;; Registry record
(alias 'currency-h (create-ns 'io.randomseed.bankster.CurrencyHierarchies))   ;; CurrencyHierarchies record
(alias 'currency   (create-ns 'io.randomseed.bankster.Currency))              ;; Currency record
(alias 'money      (create-ns 'io.randomseed.bankster.Money))                 ;; Money record

;;
;; Import core records
;;

(import '(io.randomseed.bankster Currency
                                 Money
                                 Registry
                                 CurrencyHierarchies))

;;
;; Currency scale specs
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
    (s/or :no-numeric #(and (integer? %) (== -1 (long %)))
          :numeric-id (s/and integer? pos?))
    #(gen/one-of [(gen/return -1)
                  (gen/large-integer* {:min 1 :max 9999})])))

;;
;; Currency record specs
;;

;; Currency identifier (qualified or unqualified keyword).
(s/def ::currency/id
  keyword?)

;; Currency numeric ID: either a positive long or the no-numeric-id sentinel (-1).
(s/def ::currency/numeric
  ::currency-numeric-id)

;; Currency scale: either a non-negative integer or the auto-scaled sentinel (-1).
(s/def ::currency/scale
  ::currency-scale)

;; Currency kind (namespaced keyword or nil).
(s/def ::currency/kind
  (s/nilable keyword?))

;; Currency domain (keyword or nil).
(s/def ::currency/domain
  (s/nilable keyword?))

;; Specification for the Currency record.
(s/def ::bankster/Currency
  (s/and #(instance? Currency %)
         (fn [c] (s/valid? ::currency/id (:id c)))
         (fn [c] (s/valid? ::currency/numeric (:numeric c)))
         (fn [c] (s/valid? ::currency/scale (:scale c)))
         (fn [c] (s/valid? ::currency/kind (:kind c)))
         (fn [c] (s/valid? ::currency/domain (:domain c)))))

;;
;; Money record specs
;;

;; Currency object in Money record.
(s/def ::money/currency
  #(instance? Currency %))

;; Money amount (BigDecimal).
(s/def ::money/amount
  #(instance? java.math.BigDecimal %))

;; Specification for the Money record.
(s/def ::bankster/Money
  (s/and #(instance? Money %)
         (fn [m] (s/valid? ::money/currency (:currency m)))
         (fn [m] (s/valid? ::money/amount (:amount m)))))

;;
;; CurrencyHierarchies record specs
;;

;; Domain hierarchy (associative structure).
(s/def ::currency-h/domain
  associative?)

;; Kind hierarchy (associative structure).
(s/def ::currency-h/kind
  associative?)

;; Traits hierarchy (associative structure).
(s/def ::currency-h/traits
  associative?)

;; Specification for the CurrencyHierarchies record.
(s/def ::bankster/CurrencyHierarchies
  (s/and #(instance? CurrencyHierarchies %)
         (fn [h] (s/valid? ::currency-h/domain (:domain h)))
         (fn [h] (s/valid? ::currency-h/kind (:kind h)))
         (fn [h] (s/valid? ::currency-h/traits (:traits h)))))

;;
;; Registry record specs
;;

;; Currency ID to Currency map.
(s/def ::registry/cur-id->cur
  (s/map-of keyword? ::bankster/Currency))

;; Currency numeric ID to Currency map.
(s/def ::registry/cur-nr->cur
  (s/map-of integer? ::bankster/Currency))

;; Country ID to Currency map.
(s/def ::registry/ctr-id->cur
  (s/map-of keyword? ::bankster/Currency))

;; Currency ID to set of country IDs.
(s/def ::registry/cur-id->ctr-ids
  (s/map-of keyword? (s/coll-of keyword? :kind set?)))

;; Currency ID to localized properties map.
(s/def ::registry/cur-id->localized
  (s/map-of keyword? map?))

;; Currency ID to traits (set or vector of keywords).
(s/def ::registry/cur-id->traits
  (s/map-of keyword? (s/coll-of keyword?)))

;; Currency ID to weight (integer).
(s/def ::registry/cur-id->weight
  (s/map-of keyword? integer?))

;; Currency code to set of currencies (weighted).
(s/def ::registry/cur-code->curs
  (s/map-of simple-keyword? (s/coll-of ::bankster/Currency :kind set?)))

;; Currency numeric ID to set of currencies (weighted).
(s/def ::registry/cur-nr->curs
  (s/map-of integer? (s/coll-of ::bankster/Currency :kind set?)))

;; Currency domain to set of currencies (weighted).
(s/def ::registry/cur-dom->curs
  (s/map-of keyword? (s/coll-of ::bankster/Currency :kind set?)))

;; Currency hierarchies object.
(s/def ::registry/hierarchies
  #(instance? CurrencyHierarchies %))

;; Registry version string.
(s/def ::registry/version
  string?)

;; Registry extension map.
(s/def ::registry/ext
  map?)

;; Specification for the Registry record.
(s/def ::bankster/Registry
  (s/and #(instance? Registry %)
         (fn [r] (s/valid? ::registry/cur-id->cur (:cur-id->cur r)))
         (fn [r] (s/valid? ::registry/cur-nr->cur (:cur-nr->cur r)))
         (fn [r] (s/valid? ::registry/ctr-id->cur (:ctr-id->cur r)))
         (fn [r] (s/valid? ::registry/cur-id->ctr-ids (:cur-id->ctr-ids r)))
         (fn [r] (s/valid? ::registry/cur-id->localized (:cur-id->localized r)))
         (fn [r] (s/valid? ::registry/cur-id->traits (:cur-id->traits r)))
         (fn [r] (s/valid? ::registry/cur-id->weight (:cur-id->weight r)))
         (fn [r] (s/valid? ::registry/cur-code->curs (:cur-code->curs r)))
         (fn [r] (s/valid? ::registry/cur-nr->curs (:cur-nr->curs r)))
         (fn [r] (s/valid? ::registry/cur-dom->curs (:cur-dom->curs r)))
         (fn [r] (s/valid? ::registry/hierarchies (:hierarchies r)))
         (fn [r] (s/valid? ::registry/version (:version r)))
         (fn [r] (s/valid? ::registry/ext (:ext r)))))
