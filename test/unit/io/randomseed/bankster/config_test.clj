(ns

    ^{:doc    "bankster library, config tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.config-test

  (:require [clojure.test               :refer [deftest testing is]]
            [io.randomseed.bankster.config :as config]))

(deftest kwish-and-normalization
  (let [kwish #'config/kwish]
    (testing "kwish normalizes symbols, strings, and fallbacks"
      (is (= :a (kwish 'a)))
      (is (= :ns/a (kwish 'ns/a)))
      (is (= :PLN (kwish ":PLN")))
      (is (= :PLN (kwish "PLN")))
      (is (= :123 (kwish 123)))))
  (testing "normalize-config returns non-maps as-is"
    (is (= 1 (#'config/normalize-config 1)))))

(deftest seqable-and-localized-merges
  (testing "seqable-coll falls back to scalar list for non-seqables"
    (is (= '(42) (#'config/seqable-coll 42))))
  (testing "merge-localized-entry merges nested property maps"
    (is (= {:en {:name "Zloty" :symbol "PLN"}}
           (#'config/merge-localized-entry {:en {:name "Zloty"}}
                                           {:en {:symbol "PLN"}}))))
  (testing "merge-localized-entry tolerates nil branches"
    (is (= {:en {:name "Zloty"}}
           (#'config/merge-localized-entry nil {:en {:name "Zloty"}}))))
  )

(deftest add-inline-currency-data
  (testing "inline currency data is expanded into top-level branches"
    (let [cfg {:currencies {"PLN" {:countries [:PL nil]
                                   :localized {:en {:name "Zloty"}}
                                   :traits    [:fiat :cash]
                                   :weight    7}}}
          res (#'config/add-currency-inline-data cfg)]
      (is (= :PLN (get-in res [:countries :PL])))
      (is (= {:name "Zloty"} (get-in res [:localized :PLN :en])))
      (is (= [:fiat :cash] (get-in res [:traits :PLN])))
      (is (= 7 (get-in res [:weights :PLN])))))
  (testing "invalid currency IDs are skipped (no changes)"
    (let [cfg {:currencies {nil {:countries [:PL]}}}
          res (#'config/add-currency-inline-data cfg)]
      (is (= {:currencies {nil {:countries [:PL]}}
              :countries  {}
              :localized  {}
              :traits     {}
              :weights    {}}
             res)))))

(deftest load-default-config
  (testing "load uses default resource path"
    (let [cfg (config/load)]
      (is (map? cfg))
      (is (contains? cfg :currencies)))))
