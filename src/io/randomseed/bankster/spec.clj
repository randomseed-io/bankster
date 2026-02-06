;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Public specs of Bankster (aggregator - loads all spec modules)."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.spec

  (:require [io.randomseed.bankster                  :as        bankster]
            [io.randomseed.bankster.util             :as            util]
            [io.randomseed.bankster.spec.primitives  :as      primitives]
            [io.randomseed.bankster.spec.records     :as         records]
            [clojure.spec.alpha                      :as               s]
            [orchestra.spec.test                     :as              st]
            [clojure.spec.gen.alpha                  :as             gen]))

;;
;; Namespaces for easy use of keywords (for backward compatibility)
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
;; Specs are loaded and registered via requires above
;;
;; All specs are accessible via their qualified keywords:
;;
;; Main record specs are registered in their respective namespaces:
;; - ::bankster/Currency          (from spec.records)
;; - ::bankster/Money             (from spec.records)
;; - ::bankster/Registry          (from spec.records)
;; - ::bankster/CurrencyHierarchies (from spec.records)
;;
;; Field specs use qualified keywords matching their types:
;; - ::currency/id, ::currency/numeric, ::currency/scale, etc. (from spec.records)
;; - ::money/currency, ::money/amount                          (from spec.records)
;; - ::registry/cur-id->cur, etc.                              (from spec.records)
;; - ::currency-h/domain, ::currency-h/kind, ::currency-h/traits (from spec.records)
