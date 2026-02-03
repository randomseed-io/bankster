(ns

    ^{:doc    "bankster library, api tests."
      :author "Paweł Wilk"
      :added  "2.2.0"
      :no-doc true}

    io.randomseed.bankster.api-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster.api      :as api]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.registry :as registry])

  (:import (io.randomseed.bankster Currency Money Registry)))

(defn- mk-test-registry
  []
  (let [r0  (registry/new)
        eur (currency/new :EUR 978 2 :iso/fiat :ISO-4217 0)
        pln (currency/new :PLN 985 2 :iso/fiat :ISO-4217 0)]
    (-> r0
        (currency/register eur)
        (currency/register pln))))

(defn- with-default-currency
  [^Registry r f]
  (registry/with r
    (try
      (currency/set-default! :EUR)
      (f)
      (finally
        (currency/unset-default!)))))

(deftest currency-api
  (let [r (mk-test-registry)]
    (with-default-currency
      r
      (fn []
        (testing "currency (strict) arities and registry modes"
          (is (= :EUR (.id ^Currency (api/currency))))
          (is (= :EUR (.id ^Currency (api/currency :EUR))))
          (is (= :EUR (.id ^Currency (api/currency :EUR nil))))
          (is (= :EUR (.id ^Currency (api/currency :EUR true))))
          (is (= :EUR (.id ^Currency (api/currency :EUR r))))
          (let [cur (api/currency :EUR r)]
            (is (identical? cur (api/currency cur)))
            (is (identical? cur (api/currency cur nil)))
            (is (= :EUR (.id ^Currency (api/currency cur r)))))
          (is (thrown? clojure.lang.ExceptionInfo (api/currency :NOPE r))))

        (testing "currency-try (soft) arities and registry modes"
          (is (= :EUR (.id ^Currency (api/currency-try))))
          (is (= :EUR (.id ^Currency (api/currency-try :EUR))))
          (is (= :EUR (.id ^Currency (api/currency-try :EUR nil))))
          (is (= :EUR (.id ^Currency (api/currency-try :EUR true))))
          (is (= :EUR (.id ^Currency (api/currency-try :EUR r))))
          (let [cur (api/currency :EUR r)]
            (is (identical? cur (api/currency-try cur)))
            (is (identical? cur (api/currency-try cur nil)))
            (is (= :EUR (.id ^Currency (api/currency-try cur r)))))
          (is (nil? (api/currency-try :NOPE r))))))))

(deftest money-api
  (let [r (mk-test-registry)]
    (with-default-currency
      r
      (fn []
        (testing "money arities and branches"
          (is (instance? Money (api/money)))
          (is (instance? Money (api/money 1)))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR r)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR :HALF_UP)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR nil)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR :HALF_UP r)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR nil r))))))

        (testing "try-money arities and branches"
          (is (instance? Money (api/try-money)))
          (is (instance? Money (api/try-money 1)))
          (is (instance? Money (api/try-money 1 :EUR)))
          (is (instance? Money (api/try-money 1 :EUR r)))
          (is (instance? Money (api/try-money 1 :EUR :HALF_UP)))
          (is (instance? Money (api/try-money 1 :EUR nil)))
          (is (instance? Money (api/try-money 1 :EUR :HALF_UP r)))
          (is (instance? Money (api/try-money 1 :EUR nil r)))
          (is (nil? (api/try-money 1 :NOPE r))))))))
