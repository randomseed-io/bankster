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
;; Basic types
;;

(defn no-lowercase-unicode? [^String s]
  (not-any? #(Character/isLowerCase ^char %) s))

(defn starts-with-digit? [^String s]
  (and (pos? (.length s))
       (Character/isDigit (.charAt s 0))))

(s/def ::simple-keyword-no-lowercase
  (s/with-gen
    (s/and
     simple-keyword?
     #(let [n (name %)]
        (and (not (starts-with-digit? n))
             (no-lowercase-unicode? n))))
    (fn []
      (let [upper-letter (gen/elements (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
            digit        (gen/elements (vec "0123456789"))
            sym          (gen/elements [\_ \-])
            first-char   (gen/one-of [upper-letter sym])
            rest-char    (gen/one-of [upper-letter digit sym])]
        (gen/fmap
         (fn [[c cs]] (keyword (apply str c cs)))
         (gen/tuple
          first-char
          (gen/vector rest-char 0 32)))))))

;; Simple keyword identifier.
(s/def ::simple-id
  (s/with-gen
    simple-keyword?
    (fn [] (gen/fmap keyword (gen/such-that seq gen/string-alphanumeric)))))

;;
;; Composite specs using sentinels
;;

;; Currency scale: either a non-negative integer or the auto-scaled sentinel (-1).
(s/def ::currency-scale
  (s/with-gen
    (s/and integer? #(>= (long %) -1))
    #(gen/one-of [(gen/return -1)
                  (gen/large-integer* {:min 0 :max 20})])))

;; Currency numeric ID: either a positive long or the no-numeric-id sentinel (-1).
(s/def ::currency-numeric-id
  (s/with-gen
    (s/and integer? (some-fn #{-1} pos?))
    #(gen/one-of [(s/gen ::no-numeric-id)
                  (gen/large-integer* {:min 1 :max 9999})])))

;; Currency numeric ID: either a positive long or the no-numeric-id sentinel (-1).
(s/def ::currency-numeric-id-iso
  (s/with-gen
    (s/and integer? pos?)
    #(gen/large-integer* {:min 1 :max 9999})))

;; Simple currency identifier.
(s/def ::simple-currency-id ::simple-id)

;; Currency ISO domain.
(s/def ::currency-iso-domain
  (s/with-gen
    (s/and keyword? #{:ISO-4217})
    (fn [] (gen/return :ISO-4217))))

;; Currency non-ISO domain.
(s/def ::currency-regular-domain
  (s/with-gen
    (s/and ::simple-keyword-no-lowercase (complement #{:ISO-4217}))
    (fn []
      (gen/such-that
       (complement #{:ISO-4217})
       (s/gen ::simple-keyword-no-lowercase)
       100))))
