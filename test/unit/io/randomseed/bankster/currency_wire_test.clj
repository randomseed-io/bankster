(ns

    ^{:doc    "bankster library, currency wire/canonicalization tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.currency-wire-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster.currency :as c]))

(deftest currency-id-str-canonicalization
  (testing "to-id-str upper-cases only the name part and preserves namespace"
    (is (= "PLN" (c/to-id-str :pln)))
    (is (= "crypto/USDT" (c/to-id-str :crypto/usdt)))
    (is (= "CrYpTo/USDT" (c/to-id-str :CrYpTo/usdt))))
  (testing "map representations are supported"
    (is (= "PLN" (c/to-id-str {:id :pln})))
    (is (= "crypto/USDT" (c/to-id-str {:id :crypto/usdt})))))

(deftest currency-code-str-canonicalization
  (testing "to-code-str returns canonical code string without namespace"
    (is (= "PLN" (c/to-code-str :pln)))
    (is (= "USDT" (c/to-code-str :crypto/usdt)))
    (is (= "USDT" (c/to-code-str {:id :crypto/usdt})))))

