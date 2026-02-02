(ns

    ^{:doc    "Bankster library, core records tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.bankster-test

  (:require [clojure.test :refer [deftest testing is]]
            [io.randomseed.bankster :as bankster]
            [io.randomseed.bankster.money :as money]))

(deftest record-toString-contracts
  (testing "Currency toString returns id name"
    (let [c (bankster/->Currency :PLN 985 2 :iso/fiat :ISO-4217)]
      (is (= "PLN" (.toString c)))))
  (testing "Registry toString returns pr-str form"
    (let [r (bankster/->Registry {} {} {} {} {} {} {} {} {} nil "v" {})]
      (is (string? (.toString r)))
      (is (re-find #"^#Registry" (.toString r)))))
  (testing "Money toString uses amount and currency"
    (let [m (money/of :PLN 12.30M)]
      (is (= "12.30 PLN" (.toString m))))))
