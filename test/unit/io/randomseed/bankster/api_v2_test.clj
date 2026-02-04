(ns

    ^{:doc    "bankster library, api v2 tests."
      :author "Paweł Wilk"
      :added  "2.2.0"
      :no-doc true}

    io.randomseed.bankster.api-v2-test

  (:require [clojure.set                            :as set]
            [clojure.test                           :refer [deftest testing is]]
            [io.randomseed.bankster.api             :as api]
            [io.randomseed.bankster.api.currency    :as api-currency]
            [io.randomseed.bankster.api.money       :as api-money]
            [io.randomseed.bankster.api.ops         :as api-ops]
            [io.randomseed.bankster.api.registry    :as api-registry]
            [io.randomseed.bankster.api.v2          :as api-v2]
            [io.randomseed.bankster.api.v2.currency :as api-v2-currency]
            [io.randomseed.bankster.api.v2.money    :as api-v2-money]
            [io.randomseed.bankster.api.v2.ops      :as api-v2-ops]
            [io.randomseed.bankster.api.v2.registry :as api-v2-registry]
            [io.randomseed.bankster.currency        :as currency]
            [io.randomseed.bankster.registry        :as registry])

  (:import (io.randomseed.bankster Currency Money Registry)))

(defn- mk-test-registry
  []
  (let [r0  (registry/new)
        eur (currency/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
    (currency/register r0 eur)))

(defn- public-names
  [ns-sym]
  (set (keys (ns-publics ns-sym))))

(defn- assert-v2-subset
  [v2-ns api-ns & [ignored]]
  (let [v2-names  (set/difference (public-names v2-ns) (set ignored))
        api-names (public-names api-ns)
        missing  (sort (set/difference v2-names api-names))]
    (is (empty? missing)
        (str "Missing api counterparts for " v2-ns ": " missing))))

(deftest v2-auto-aliases
  (let [r (mk-test-registry)]
    (registry/with r
      (testing "api.v2 top-level"
        (let [m (api-v2/money 1 :EUR)]
          (is (instance? Money m))
          (is (= :EUR (.id ^Currency (.currency ^Money m))))))
      (testing "api.v2 currency"
        (let [c (api-v2-currency/resolve :EUR)]
          (is (instance? Currency c))
          (is (= :EUR (.id ^Currency c)))))
      (testing "api.v2 money"
        (is (instance? Money (api-v2-money/resolve 1 :EUR))))
      (testing "api.v2 ops"
        (is (= 3 (api-v2-ops/+ 1 2))))
      (testing "api.v2 registry"
        (is (instance? Registry (api-v2-registry/state)))))))

(deftest v2-subset-of-api
  (testing "api.v2 top-level matches api"
    ;; `auto-alias` is a bootstrap helper kept only in v2.
    (assert-v2-subset 'io.randomseed.bankster.api.v2
                      'io.randomseed.bankster.api
                      ['auto-alias]))
  (testing "api.v2.currency matches api.currency"
    (assert-v2-subset 'io.randomseed.bankster.api.v2.currency
                      'io.randomseed.bankster.api.currency))
  (testing "api.v2.money matches api.money"
    (assert-v2-subset 'io.randomseed.bankster.api.v2.money
                      'io.randomseed.bankster.api.money))
  (testing "api.v2.ops matches api.ops"
    (assert-v2-subset 'io.randomseed.bankster.api.v2.ops
                      'io.randomseed.bankster.api.ops))
  (testing "api.v2.registry matches api.registry"
    (assert-v2-subset 'io.randomseed.bankster.api.v2.registry
                      'io.randomseed.bankster.api.registry)))
