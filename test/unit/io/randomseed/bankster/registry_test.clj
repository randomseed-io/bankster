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
    (let [r (registry/new {} {} {} {} {} {} {} {}
                          {}
                          {:domain {:ISO-4217-LEGACY :ISO-4217}
                           :kind   {:COMBANK :FIAT}}
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:kind hs)   :COMBANK         :FIAT))))

  (testing "hierarchies can include custom axes (additional keys)"
    (let [r (registry/new {} {} {} {} {} {} {} {}
                          {}
                          {:domain {:ISO-4217-LEGACY :ISO-4217}
                           :kind   {:COMBANK :FIAT}
                           :traits {:stablecoin :stable
                                    :fiat-backed :stable}}
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:traits hs) :stablecoin :stable))
      (is (isa? (:traits hs) :fiat-backed :stable))))

  (testing "parent-map supports multiple parents via set/vector values"
    (let [r (registry/new {} {} {} {} {} {} {} {}
                          {}
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
    (let [r (registry/new {} {} {} {} {} {} {} {}
                          {}
                          (bankster/->CurrencyHierarchies {:ISO-4217-LEGACY :ISO-4217} nil nil)
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (map? (:kind hs)))))

  (testing "hierarchies can be passed as already constructed hierarchy maps"
    (let [dom (derive (make-hierarchy) :ISO-4217-LEGACY :ISO-4217)
          r   (registry/new {} {} {} {} {} {} {} {}
                            {}
                            {:domain dom}
                            "test")
          hs  (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (map? (:kind hs))))))

(deftest registry-get-and-hierarchy-nil-safe
  (testing "registry/get treats literal true as a sentinel meaning: use the default registry"
    (let [r (registry/get)]
      (is (= r (registry/get true)))))

  (testing "hierarchies/hierarchy accept nil registry and fall back to the default registry"
    (let [hs (registry/hierarchies)]
      (is (= hs (registry/hierarchies nil)))
      (is (= (:kind hs) (registry/hierarchy :kind nil)))
      (is (= (:traits hs) (registry/hierarchy :traits nil))))))

(deftest hierarchy-derive-updates-selected-hierarchy
  (testing "hierarchy-derive returns a new registry with derived relationship"
    (let [r0 (registry/new)
          r1 (registry/hierarchy-derive :kind :iso/funds :iso/money r0)
          h0 (registry/hierarchy :kind r0)
          h1 (registry/hierarchy :kind r1)]
      (is (not (isa? h0 :iso/funds :iso/money)))
      (is (isa? h1 :iso/funds :iso/money))))

  (testing "hierarchy-derive! updates the global registry"
    (let [orig (registry/state)]
      (try
        (registry/set! (registry/new))
        (registry/hierarchy-derive! :domain :ISO-4217-LEGACY :ISO-4217)
        (let [h (registry/hierarchy :domain (registry/state))]
          (is (isa? h :ISO-4217-LEGACY :ISO-4217)))
        (finally
          (registry/set! orig))))))
