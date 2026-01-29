(ns

    ^{:doc    "bankster library, registry tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.registry-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster          :as bankster]))

(deftest new-registry-hierarchies
  (testing "default registry has CurrencyHierarchies initialized"
    (let [r  (registry/new)
          hs (:hierarchies r)]
      (is (instance? io.randomseed.bankster.CurrencyHierarchies hs))
      (is (map? (:domain hs)))
      (is (map? (:kind hs)))))

  (testing "hierarchies can be passed as a parent-map in a map spec"
    (let [r (registry/new {} {} {} {} {} {} {}
                          {:domain {:ISO-4217-LEGACY :ISO-4217}
                           :kind   {:COMBANK :FIAT}}
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:kind hs)   :COMBANK         :FIAT))))

  (testing "parent-map supports multiple parents via set/vector values"
    (let [r (registry/new {} {} {} {} {} {} {}
                          {:domain {:ISO-4217-LEGACY #{:ISO-4217 :MONEY}
                                    :ISO-4217-LEGACY2 [:ISO-4217 :MONEY]}
                           :kind   {:COMBANK #{:FIAT :FUNDS}}}
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:domain hs) :ISO-4217-LEGACY :MONEY))
      (is (isa? (:domain hs) :ISO-4217-LEGACY2 :ISO-4217))
      (is (isa? (:domain hs) :ISO-4217-LEGACY2 :MONEY))
      (is (isa? (:kind hs)   :COMBANK :FIAT))
      (is (isa? (:kind hs)   :COMBANK :FUNDS))))

  (testing "hierarchies can be passed as a record with parent-maps"
    (let [r (registry/new {} {} {} {} {} {} {}
                          (bankster/->CurrencyHierarchies {:ISO-4217-LEGACY :ISO-4217} nil)
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (map? (:kind hs)))))

  (testing "hierarchies can be passed as already constructed hierarchy maps"
    (let [dom (derive (make-hierarchy) :ISO-4217-LEGACY :ISO-4217)
          r   (registry/new {} {} {} {} {} {} {}
                            {:domain dom}
                            "test")
          hs  (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (map? (:kind hs))))))
