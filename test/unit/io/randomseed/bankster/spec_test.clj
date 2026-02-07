;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns io.randomseed.bankster.spec-test
  (:require [clojure.test                     :refer [deftest testing is are]]
            [clojure.spec.alpha               :as s]
            [clojure.spec.gen.alpha           :as gen]
            [clojure.test.check.properties    :as prop]
            [clojure.test.check.clojure-test  :refer [defspec]]
            [io.randomseed.bankster.spec]
            [io.randomseed.bankster           :as bankster]
            [io.randomseed.bankster.currency  :as currency]
            [io.randomseed.bankster.money     :as money]
            [io.randomseed.bankster.registry  :as registry])
  (:import [io.randomseed.bankster Currency Money Registry CurrencyHierarchies]))

;;
;; Currency specs tests
;;

(deftest currency-scale-spec-test
  (testing "Currency scale specification"
    (testing "auto-scaled sentinel"
      (is (s/valid? :io.randomseed.bankster.spec.primitives/currency-scale -1))
      (is (= -1 (s/conform :io.randomseed.bankster.spec.primitives/currency-scale -1))))

    (testing "fixed scales"
      (are [scale] (s/valid? :io.randomseed.bankster.spec.primitives/currency-scale scale)
        0
        2
        4
        8))

    (testing "invalid scales"
      (are [scale] (not (s/valid? :io.randomseed.bankster.spec.primitives/currency-scale scale))
        -2
        -10
        "2"))))

(deftest currency-numeric-id-spec-test
  (testing "Currency numeric ID specification"
    (testing "no-numeric-id sentinel"
      (is (s/valid? :io.randomseed.bankster.spec.primitives/currency-numeric-id -1))
      (is (= -1 (s/conform :io.randomseed.bankster.spec.primitives/currency-numeric-id -1))))

    (testing "valid numeric IDs"
      (are [nr] (s/valid? :io.randomseed.bankster.spec.primitives/currency-numeric-id nr)
        978   ;; EUR
        840   ;; USD
        1     ;; minimum valid
        999))

    (testing "invalid numeric IDs"
      (are [nr] (not (s/valid? :io.randomseed.bankster.spec.primitives/currency-numeric-id nr))
        0
        -2
        -100
        "978"))))

(deftest currency-field-specs-test
  (testing "Currency field specifications"
    (testing "id field"
      (are [id] (s/valid? :io.randomseed.bankster.Currency/id id)
        :EUR
        :USD
        :crypto/BTC
        :crypto/ETH)

      (is (not (s/valid? :io.randomseed.bankster.Currency/id "EUR")))
      (is (not (s/valid? :io.randomseed.bankster.Currency/id nil))))

    (testing "kind field"
      (are [kind] (s/valid? :io.randomseed.bankster.Currency/kind kind)
        :iso/fiat
        :virtual/token
        :iso/metal
        nil)

      (is (not (s/valid? :io.randomseed.bankster.Currency/kind "fiat"))))

    (testing "domain field"
      (are [domain] (s/valid? :io.randomseed.bankster.Currency/domain domain)
        :ISO-4217
        :CRYPTO
        :ISO-4217-LEGACY
        nil)

      (is (not (s/valid? :io.randomseed.bankster.Currency/domain "ISO-4217"))))))

(deftest currency-record-spec-test
  (testing "Currency record specification"
    (testing "valid currencies from registry"
      (let [eur (currency/unit :EUR)
            usd (currency/unit :USD)
            pln (currency/unit :PLN)]
        (is (s/valid? :io.randomseed.bankster/Currency eur))
        (is (s/valid? :io.randomseed.bankster/Currency usd))
        (is (s/valid? :io.randomseed.bankster/Currency pln))))

    (testing "crypto currencies"
      (when-let [btc (currency/resolve :crypto/BTC)]
        (is (s/valid? :io.randomseed.bankster/Currency btc))))

    (testing "auto-scaled currency"
      (let [auto-cur (currency/new :TEST/AUTO -1 -1 :virtual/test :TEST)]
        (is (s/valid? :io.randomseed.bankster/Currency auto-cur))
        (is (= -1 (:scale auto-cur)))))

    (testing "ISO-4217 domain requires simple keyword ID"
      (testing "valid - simple keyword with ISO-4217"
        (let [iso-cur (currency/new :EUR 978 2 :iso/fiat :ISO-4217)]
          (is (s/valid? :io.randomseed.bankster/Currency iso-cur))))

      (testing "invalid - namespaced keyword with ISO-4217"
        ;; Directly construct Currency record to test spec without currency/new validation
        (let [invalid-cur (Currency. :custom/EUR 978 2 :iso/fiat :ISO-4217)]
          (is (not (s/valid? :io.randomseed.bankster/Currency invalid-cur)))))

      (testing "valid - namespaced keyword with non-ISO domain"
        (let [crypto-cur (currency/new :crypto/BTC -1 8 :virtual/crypto :CRYPTO)]
          (is (s/valid? :io.randomseed.bankster/Currency crypto-cur)))))))

;;
;; Money specs tests
;;

(deftest money-field-specs-test
  (testing "Money field specifications"
    (testing "currency field"
      (let [eur (currency/unit :EUR)]
        (is (s/valid? :io.randomseed.bankster.Money/currency eur))
        (is (not (s/valid? :io.randomseed.bankster.Money/currency :EUR)))
        (is (not (s/valid? :io.randomseed.bankster.Money/currency nil)))))

    (testing "amount field"
      (is (s/valid? :io.randomseed.bankster.Money/amount 100M))
      (is (s/valid? :io.randomseed.bankster.Money/amount 0.01M))
      (is (s/valid? :io.randomseed.bankster.Money/amount -50M))

      (is (not (s/valid? :io.randomseed.bankster.Money/amount 100)))
      (is (not (s/valid? :io.randomseed.bankster.Money/amount 100.0)))
      (is (not (s/valid? :io.randomseed.bankster.Money/amount "100"))))))

(deftest money-record-spec-test
  (testing "Money record specification"
    (testing "valid money objects"
      (let [m1 (money/of :EUR 100)
            m2 (money/of :USD 50.25)
            m3 (money/of :PLN 0)]
        (is (s/valid? :io.randomseed.bankster/Money m1))
        (is (s/valid? :io.randomseed.bankster/Money m2))
        (is (s/valid? :io.randomseed.bankster/Money m3))))

    (testing "negative amounts"
      (let [m (money/of :EUR -100)]
        (is (s/valid? :io.randomseed.bankster/Money m))))

    (testing "large amounts"
      (let [m (money/of :EUR 1000000000)]
        (is (s/valid? :io.randomseed.bankster/Money m))))))

;;
;; CurrencyHierarchies specs tests
;;

(deftest currency-hierarchies-spec-test
  (testing "CurrencyHierarchies record specification"
    (let [reg (registry/get)
          hierarchies (:hierarchies reg)]
      (is (instance? CurrencyHierarchies hierarchies))
      (is (s/valid? :io.randomseed.bankster/CurrencyHierarchies hierarchies))

      (testing "hierarchy fields"
        (is (s/valid? :io.randomseed.bankster.CurrencyHierarchies/domain
                      (:domain hierarchies)))
        (is (s/valid? :io.randomseed.bankster.CurrencyHierarchies/kind
                      (:kind hierarchies)))
        (is (s/valid? :io.randomseed.bankster.CurrencyHierarchies/traits
                      (:traits hierarchies)))))))

;;
;; Registry specs tests
;;

(deftest registry-field-specs-test
  (testing "Registry field specifications"
    (let [reg (registry/get)]
      (testing "cur-id->cur map"
        (is (s/valid? :io.randomseed.bankster.Registry/cur-id->cur
                      (:cur-id->cur reg))))

      (testing "cur-nr->cur map"
        (is (s/valid? :io.randomseed.bankster.Registry/cur-nr->cur
                      (:cur-nr->cur reg))))

      (testing "cur-code->curs map"
        (is (s/valid? :io.randomseed.bankster.Registry/cur-code->curs
                      (:cur-code->curs reg))))

      (testing "hierarchies"
        (is (s/valid? :io.randomseed.bankster.Registry/hierarchies
                      (:hierarchies reg))))

      (testing "version"
        (is (s/valid? :io.randomseed.bankster.Registry/version
                      (:version reg))))

      (testing "ext"
        (is (s/valid? :io.randomseed.bankster.Registry/ext
                      (:ext reg)))))))

(deftest registry-record-spec-test
  (testing "Registry record specification"
    (let [reg (registry/get)]
      (is (instance? Registry reg))
      (is (s/valid? :io.randomseed.bankster/Registry reg)))))

;;
;; Integration tests
;;

(deftest spec-integration-test
  (testing "Specs work together in realistic scenarios"
    (testing "Create and validate money with different currencies"
      (let [eur-money (money/of :EUR 100.50)
            usd-money (money/of :USD 75.25)
            pln-money (money/of :PLN 450)]
        (is (s/valid? :io.randomseed.bankster/Money eur-money))
        (is (s/valid? :io.randomseed.bankster/Money usd-money))
        (is (s/valid? :io.randomseed.bankster/Money pln-money))

        (is (s/valid? :io.randomseed.bankster/Currency (:currency eur-money)))
        (is (s/valid? :io.randomseed.bankster/Currency (:currency usd-money)))
        (is (s/valid? :io.randomseed.bankster/Currency (:currency pln-money)))))

    (testing "Registry contains valid currencies"
      (let [reg (registry/get)
            currencies (vals (:cur-id->cur reg))]
        (is (every? #(s/valid? :io.randomseed.bankster/Currency %) currencies))))))

;;
;; Error explanation tests
;;

(deftest spec-explain-test
  (testing "Spec explain provides useful error messages"
    (testing "invalid currency scale"
      (let [result (s/explain-data :io.randomseed.bankster.spec.primitives/currency-scale -5)]
        (is (some? result))
        (is (contains? result ::s/problems))))

    (testing "invalid money amount"
      (let [result (s/explain-data :io.randomseed.bankster.Money/amount 100)]
        (is (some? result))
        (is (contains? result ::s/problems))))))

;;
;; Generative tests (property-based testing)
;;

(deftest currency-scale-generative-test
  (testing "Generated currency scales are valid"
    (let [samples (s/exercise :io.randomseed.bankster.spec.primitives/currency-scale 20)]
      (doseq [[value conformed] samples]
        (is (s/valid? :io.randomseed.bankster.spec.primitives/currency-scale value))
        (is (= value conformed))))))

(deftest currency-numeric-id-generative-test
  (testing "Generated currency numeric IDs are valid"
    (let [samples (s/exercise :io.randomseed.bankster.spec.primitives/currency-numeric-id 20)]
      (doseq [[value conformed] samples]
        (is (s/valid? :io.randomseed.bankster.spec.primitives/currency-numeric-id value))
        (is (= value conformed))))))

(deftest currency-fields-sample-test
  (testing "Sampled currency field values"
    (testing "currency IDs"
      (let [samples (gen/sample (s/gen :io.randomseed.bankster.Currency/id) 10)]
        (is (every? keyword? samples))))

    (testing "currency scales"
      (let [samples (gen/sample (s/gen :io.randomseed.bankster.Currency/scale) 20)]
        (is (every? #(s/valid? :io.randomseed.bankster.Currency/scale %) samples))))

    (testing "currency numeric IDs"
      (let [samples (gen/sample (s/gen :io.randomseed.bankster.Currency/numeric) 20)]
        (is (every? #(s/valid? :io.randomseed.bankster.Currency/numeric %) samples))))))

;;
;; Property-based tests using test.check
;;

(defspec currency-scale-property-test 100
  (prop/for-all [scale (s/gen :io.randomseed.bankster.spec.primitives/currency-scale)]
    (s/valid? :io.randomseed.bankster.spec.primitives/currency-scale scale)))

(defspec currency-numeric-id-property-test 100
  (prop/for-all [numeric-id (s/gen :io.randomseed.bankster.spec.primitives/currency-numeric-id)]
    (s/valid? :io.randomseed.bankster.spec.primitives/currency-numeric-id numeric-id)))

(defspec currency-id-property-test 50
  (prop/for-all [id (s/gen :io.randomseed.bankster.Currency/id)]
    (and (keyword? id)
         (s/valid? :io.randomseed.bankster.Currency/id id))))

(defspec currency-kind-property-test 50
  (prop/for-all [kind (s/gen :io.randomseed.bankster.Currency/kind)]
    (s/valid? :io.randomseed.bankster.Currency/kind kind)))

(defspec currency-domain-property-test 50
  (prop/for-all [domain (s/gen :io.randomseed.bankster.Currency/domain)]
    (s/valid? :io.randomseed.bankster.Currency/domain domain)))

;;
;; Generative roundtrip tests
;;

(deftest currency-scale-conform-roundtrip-test
  (testing "Currency scale conform/unform roundtrip"
    (let [samples (s/exercise :io.randomseed.bankster.spec.primitives/currency-scale 20)]
      (doseq [[original conformed] samples]
        (let [unformed (s/unform :io.randomseed.bankster.spec.primitives/currency-scale conformed)]
          (is (= original unformed)))))))

(deftest auto-scaled-sentinel-spec-test
  (testing "auto-scaled sentinel spec from spec.scale"
    (is (s/valid? :io.randomseed.bankster.spec.scale/auto-scaled -1))
    (is (not (s/valid? :io.randomseed.bankster.spec.scale/auto-scaled 0)))
    (is (not (s/valid? :io.randomseed.bankster.spec.scale/auto-scaled 1)))
    (is (not (s/valid? :io.randomseed.bankster.spec.scale/auto-scaled "x"))))
  (testing "auto-scaled generator produces valid values"
    (let [samples (gen/sample (s/gen :io.randomseed.bankster.spec.scale/auto-scaled) 10)]
      (is (every? #{-1} samples)))))

(deftest currency-numeric-id-iso-generative-test
  (testing "Generated ISO numeric IDs are valid positive integers"
    (let [samples (gen/sample (s/gen :io.randomseed.bankster.spec.primitives/currency-numeric-id-iso) 20)]
      (is (every? #(and (integer? %) (pos? %)) samples)))))
