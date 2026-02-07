;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Record specs for Bankster (Currency, Money, Registry, CurrencyHierarchies)."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.spec.records

  (:require [io.randomseed.bankster                  :as        bankster]
            [io.randomseed.bankster.spec.primitives  :as            prim]
            [clojure.spec.alpha                      :as               s]
            [clojure.spec.gen.alpha                  :as             gen])
  (:import [io.randomseed.bankster Currency Money Registry CurrencyHierarchies]))

;;
;; Namespaces for easy use of keywords (aliased to record types)
;;

(alias 'currency       (create-ns 'io.randomseed.bankster.Currency))              ;; Currency record
(alias 'currency-iso   (create-ns 'io.randomseed.bankster.Currency.ISO))         ;; ISO Currency variant
(alias 'currency-other (create-ns 'io.randomseed.bankster.Currency.Other))       ;; non-ISO Currency variant
(alias 'money          (create-ns 'io.randomseed.bankster.Money))                ;; Money record
(alias 'currency-h     (create-ns 'io.randomseed.bankster.CurrencyHierarchies))  ;; CurrencyHierarchies record
(alias 'registry       (create-ns 'io.randomseed.bankster.Registry))             ;; Registry record

;;
;; Currency record specs
;;

;; Currency identifier (qualified or unqualified keyword).
(s/def ::currency/id
  keyword?)

;; Currency numeric ID: either a positive long or the no-numeric-id sentinel (-1).
(s/def ::currency/numeric
  ::prim/currency-numeric-id)

;; Currency scale: either a non-negative integer or the auto-scaled sentinel (-1).
(s/def ::currency/scale
  ::prim/currency-scale)

;; Currency kind (namespaced keyword or nil).
(s/def ::currency/kind
  (s/nilable keyword?))

;; Currency domain.
(s/def ::currency/domain
  (s/or :iso-domain     ::prim/currency-iso-domain
        :regular-domain (s/nilable ::prim/currency-regular-domain)))

;;
;; ISO Currency variant field specs (keys resolve to record field names: :id, :numeric, :domain, etc.)
;; Note: specs used inside s/keys must be nonconforming when they use s/or,
;;       because s/keys calls conform and tries to assoc the result back into the record.
;;       Tagged tuples from s/or would break typed record fields (^long, ^int).
;;

(s/def ::currency-iso/id       simple-keyword?)
(s/def ::currency-iso/numeric  ::prim/currency-numeric-id-iso)
(s/def ::currency-iso/domain   ::prim/currency-iso-domain)
(s/def ::currency-iso/scale    ::prim/currency-scale)
(s/def ::currency-iso/kind     (s/nilable keyword?))

;;
;; Non-ISO Currency variant field specs
;;

(s/def ::currency-other/id      keyword?)
(s/def ::currency-other/numeric ::prim/currency-numeric-id)
(s/def ::currency-other/domain  (s/nilable ::prim/currency-regular-domain))
(s/def ::currency-other/scale   ::prim/currency-scale)
(s/def ::currency-other/kind    (s/nilable keyword?))

;; Specification for the ISO Currency record.
(s/def ::bankster/Currency-ISO
  (s/and
   #(instance? Currency %)
   (s/keys :req-un [::currency-iso/id
                    ::currency-iso/numeric
                    ::currency-iso/domain
                    ::currency-iso/scale
                    ::currency-iso/kind])))

;; Specification for the non-ISO Currency record.
(s/def ::bankster/Currency-Other
  (s/and
   #(instance? Currency %)
   (s/keys :req-un [::currency-other/id
                    ::currency-other/numeric
                    ::currency-other/domain
                    ::currency-other/scale
                    ::currency-other/kind])))

(s/def ::bankster/Currency
  (s/nonconforming
   (s/or :iso-currency     ::bankster/Currency-ISO
         :regular-currency ::bankster/Currency-Other)))

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
