(ns

    ^{:doc    "bankster library, registry accessors tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.registry-access-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster.currency :as c]
            [io.randomseed.bankster.registry :as registry])

  (:import (io.randomseed.bankster Registry)))

(deftest registry-accessors-basic
  (testing "currency-id->currency/currency-code->currencies use explicit registry argument"
    (let [^Registry r0 (registry/new-registry)
          ^Registry r1 (-> r0
                           (c/register (c/new :AAA 1 2 :iso/fiat :ISO-4217))
                           (c/register (c/new :crypto/AAA 1 2 :virtual/native :CRYPTO))
                           (c/set-weight :AAA 10)
                           (c/set-weight :crypto/AAA 5))]
      (is (= :AAA (c/id (registry/currency-id->currency :AAA r1))))
      (is (= :crypto/AAA (c/id (registry/currency-id->currency :crypto/AAA r1))))
      ;; Same code, different IDs; ensure bucket returns both.
      (is (= #{:AAA :crypto/AAA}
             (set (map c/id (registry/currency-code->currencies :AAA r1)))))
      (is (= #{:AAA}
             (set (map c/id (registry/currency-domain->currencies :ISO-4217 r1)))))
      (is (= #{:crypto/AAA}
             (set (map c/id (registry/currency-domain->currencies :CRYPTO r1)))))
      ;; Lowest weight should be the first currency when sorting by weight.
      (is (= [:crypto/AAA :AAA]
             (->> (registry/currency-code->currencies :AAA r1)
                  (sort-by (fn [cur] (c/weight cur r1)))
                  (mapv c/id)))))))
