(ns

    ^{:doc    "bankster library, importer tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.util.importer-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster          :as bankster]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.util.importer :as importer]))

(deftest make-currency-legacy-domain-from-comment
  (testing "comment \"Old, now\" causes currency ID to get iso-4217-legacy namespace"
    (let [c (#'io.randomseed.bankster.util.importer/make-currency
             ["ADP" "20" "0" "Old, now EUR"])]
      (is (= :iso-4217-legacy/ADP (:id c)))
      (is (= :ISO-4217-LEGACY (:domain c))))))

(deftest merge-registry-merges-hierarchies-and-ext
  (testing "merges :hierarchies and :ext while keeping existing currency data"
    (let [dst (-> (registry/new-registry)
                  (assoc :hierarchies (bankster/->CurrencyHierarchies
                                       (derive (make-hierarchy) :ISO-4217-LEGACY :ISO-4217)
                                       (derive (make-hierarchy) :COMBANK :FIAT)))
                  (assoc :ext {:dst 1 :shared :dst}))
          src (-> (registry/new-registry)
                  (assoc :hierarchies (bankster/->CurrencyHierarchies
                                       (derive (make-hierarchy) :ISO-4217-SUPER :ISO-4217)
                                       (derive (make-hierarchy) :CENTRALBANK :FIAT)))
                  (assoc :ext {:src 2 :shared :src}))
          merged (importer/merge-registry dst src)
          hs     (:hierarchies merged)]
      (is (= {:dst 1 :src 2 :shared :src} (:ext merged)))
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:domain hs) :ISO-4217-SUPER :ISO-4217))
      (is (isa? (:kind hs) :COMBANK :FIAT))
      (is (isa? (:kind hs) :CENTRALBANK :FIAT)))))

(deftest merge-registry-verbose-reports-new-currencies
  (testing "when verbose? is truthy it reports currencies present in src but not in dst"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217))
                  (currency/register (currency/new :ABC 999 2 :FIAT :ISO-4217)))
          out (with-out-str (importer/merge-registry dst src true))]
      (is (re-find #"New currency: ABC" out))
      (is (not (re-find #"New currency: EUR" out))))))

(deftest merge-registry-verbose-reports-updated-currencies
  (testing "when verbose? is truthy it reports only real updates (post-preserve-fields)"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 3 :FIAT :ISO-4217)))
          out (with-out-str (importer/merge-registry dst src true))]
      (is (re-find #"Updated currency: EUR" out))
      (is (not (re-find #"New currency: EUR" out))))))

(deftest merge-registry-preserves-currency-fields-when-replacing
  (testing "preserve-fields keeps selected fields from dst when replacing currency in dst"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)))
          src (-> (registry/new-registry)
                  ;; Same ID, but different domain/kind/scale. We'll preserve :domain and :kind from dst.
                  (currency/register (currency/new :EUR 978 9 :DECENTRALIZED :CRYPTO)))
          merged (importer/merge-registry dst src false [:domain :kind])
          eur    (get (:cur-id->cur merged) :EUR)]
      (is (= :ISO-4217 (:domain eur)))
      (is (= :FIAT (:kind eur)))
      (is (= 9 (:scale eur))))))

(deftest merge-registry-skips-update-when-equal-after-preserve
  (testing "does not update currency if it becomes equal after preserve-fields (prevents data loss)"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)
                                     nil
                                     {:en {:name "Euro"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :DECENTRALIZED :CRYPTO)))
          preserve [:domain :kind ::importer/localized]
          out (with-out-str (importer/merge-registry dst src true preserve))
          merged (importer/merge-registry dst src false preserve)]
      (is (= "Euro" (currency/localized-property :name :EUR :en merged)))
      (is (not (re-find #"Updated currency: EUR" out))))))

(deftest merge-registry-preserves-localized-and-countries
  (testing "preserve-fields can preserve localized properties and countries with sentinels"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)
                                     [:PL]
                                     {:en {:name "Euro"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 3 :FIAT :ISO-4217)
                                     [:DE]
                                     {:en {:name "Eur0"}}))
          merged (importer/merge-registry dst src false [::importer/localized ::importer/countries])
          eur    (get (:cur-id->cur merged) :EUR)]
      (is (= 3 (:scale eur)))
      (is (= #{:PL} (currency/countries :EUR merged)))
      (is (= "Euro" (currency/localized-property :name :EUR :en merged))))))

(deftest merge-registry-iso-like-renames-iso-to-legacy
  (testing "iso-like? promotes legacy ISO currencies to :iso-4217-legacy/* IDs and migrates dst data"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :ADP 20 0 :FIAT :ISO-4217)
                                     [:AD]
                                     {:en {:name "Andorran peseta"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :iso-4217-legacy/ADP 20 0 :FIAT)))
          preserve [:domain :kind ::importer/localized ::importer/countries]
          merged (importer/merge-registry dst src false preserve true)
          adp    (get (:cur-id->cur merged) :iso-4217-legacy/ADP)]
      (is (nil? (get (:cur-id->cur merged) :ADP)))
      (is (some? adp))
      (is (= :ISO-4217-LEGACY (:domain adp)))
      (is (= (int importer/default-legacy-weight) (:weight adp)))
      (is (= #{:AD} (currency/countries :iso-4217-legacy/ADP merged)))
      (is (= "Andorran peseta"
             (currency/localized-property :name :iso-4217-legacy/ADP :en merged))))))

(deftest merge-registry-legacy-weight-preserves-nonzero-existing
  (testing "legacy currencies default to a high weight unless dst already had a non-zero weight"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :ADP 20 0 :FIAT :ISO-4217 7)
                                     [:AD]
                                     {:en {:name "Andorran peseta"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :iso-4217-legacy/ADP 20 0 :FIAT)))
          preserve [:domain :kind ::importer/localized ::importer/countries]
          merged (importer/merge-registry dst src false preserve true)
          adp    (get (:cur-id->cur merged) :iso-4217-legacy/ADP)]
      (is (some? adp))
      (is (= 7 (:weight adp))))))

(deftest merge-registry-legacy-weight-defaults-for-new-currency
  (testing "a newly merged legacy currency gets the default high weight when its weight is 0"
    (let [dst    (registry/new-registry)
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :iso-4217-legacy/AAA 999 2 :FIAT)))
          merged (importer/merge-registry dst src)
          aaa    (get (:cur-id->cur merged) :iso-4217-legacy/AAA)]
      (is (some? aaa))
      (is (= :ISO-4217-LEGACY (:domain aaa)))
      (is (= (int importer/default-legacy-weight) (:weight aaa))))))

(deftest merge-registry-merges-extra-hierarchy-keys
  (testing "merges hierarchy values for any keys present in :hierarchies (record may grow)"
    (let [dst (-> (registry/new-registry)
                  (assoc :hierarchies (-> (bankster/->CurrencyHierarchies (make-hierarchy) (make-hierarchy))
                                          (assoc :foo (derive (make-hierarchy) :X :Y))))
                  (assoc :ext {}))
          src (-> (registry/new-registry)
                  (assoc :hierarchies (-> (bankster/->CurrencyHierarchies (make-hierarchy) (make-hierarchy))
                                          (assoc :foo (derive (make-hierarchy) :X :Z)
                                                :bar (derive (make-hierarchy) :A :B))))
                  (assoc :ext {}))
          merged (importer/merge-registry dst src)
          hs     (:hierarchies merged)
          foo-h  (get hs :foo)
          bar-h  (get hs :bar)]
      (is (isa? foo-h :X :Y))
      (is (isa? foo-h :X :Z))
      (is (isa? bar-h :A :B)))))
