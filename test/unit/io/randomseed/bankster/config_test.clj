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
      (is (nil? (kwish nil)))
      (is (nil? (kwish "   ")))
      (is (= :a (kwish 'a)))
      (is (= :ns/a (kwish 'ns/a)))
      (is (= :USD (kwish "  :USD ")))
      (is (= :PLN (kwish ":PLN")))
      (is (= :PLN (kwish "PLN")))
      (is (= :123 (kwish 123)))))
  (testing "normalize-config returns non-maps as-is"
    (is (= 1 (#'config/normalize-config 1)))))

(deftest normalize-id-maps
  (testing "normalize-currency-id-map converts keys to keywords"
    (let [norm #'config/normalize-currency-id-map
          res  (norm {"PLN" {:scale 2} :USD {:scale 2} nil {:scale 1}})]
      (is (= {:PLN {:scale 2} :USD {:scale 2}} res))))
  (testing "normalize-countries-map converts both keys and values"
    (let [norm #'config/normalize-countries-map
          res  (norm {"PL" "PLN" :US :USD nil "EUR"})]
      (is (= {:PL :PLN :US :USD} res)))))

(deftest seqable-and-localized-merges
  (testing "seqable-coll handles common input shapes"
    (is (nil? (#'config/seqable-coll nil)))
    (is (= '("x") (#'config/seqable-coll "x")))
    (is (= [:a :b] (vec (#'config/seqable-coll #{:b :a}))))
    (is (= '(1 2) (#'config/seqable-coll [1 2])))
    (is (= '([:a 1]) (#'config/seqable-coll {:a 1})))
    (is (= '(42) (#'config/seqable-coll 42))))
  (testing "merge-localized-entry merges nested property maps"
    (is (= {:en {:name "Zloty" :symbol "PLN"}}
           (#'config/merge-localized-entry {:en {:name "Zloty"}}
                                           {:en {:symbol "PLN"}}))))
  (testing "merge-localized-entry tolerates nil branches"
    (is (= {:en {:name "Zloty"}}
           (#'config/merge-localized-entry nil {:en {:name "Zloty"}}))))
  (testing "merge-localized-entry replaces non-map values"
    (is (= {:en {:name "Zloty"}}
           (#'config/merge-localized-entry {:en "Z"} {:en {:name "Zloty"}})))))

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
             res))))
  (testing "inline data uses :we fallback and skips invalid country ids"
    (let [cfg {:currencies {"X" {:countries [:PL "" nil]
                                 :traits    #{:b :a}
                                 :we        9}}}
          res (#'config/add-currency-inline-data cfg)]
      (is (= :X (get-in res [:countries :PL])))
      (is (= [:a :b] (get-in res [:traits :X])))
      (is (= 9 (get-in res [:weights :X])))))
  (testing "add-currency-inline-data leaves config unchanged when currencies not a map"
    (let [cfg {:currencies []}]
      (is (= cfg (#'config/add-currency-inline-data cfg))))))

(deftest load-default-config
  (testing "load uses default resource path"
    (let [cfg (config/load)]
      (is (map? cfg))
      (is (contains? cfg :currencies)))))

(deftest load-normalizes-config
  (testing "load normalizes string IDs to keywords"
    (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [_] (java.net.URL. "file:/tmp/config.edn"))
                  clojure.core/slurp (fn [_] "{:currencies {\"PLN\" {:scale 2}}}")]
      (let [cfg (config/load "ignored.edn")]
        (is (= {:scale 2} (get-in cfg [:currencies :PLN])))))))

(deftest normalize-config-branches
  (let [norm #'config/normalize-config
        cfg  {:currencies {"PLN" {:scale 2}}
              :traits     {"PLN" [:fiat]}
              :weights    {"PLN" 7}
              :localized  {"PLN" {:en {:name "Zloty"}}}
              :countries  {"PL" "PLN"}}]
    (let [res (norm cfg)]
      (is (= {:scale 2} (get-in res [:currencies :PLN])))
      (is (= [:fiat] (get-in res [:traits :PLN])))
      (is (= 7 (get-in res [:weights :PLN])))
      (is (= {:name "Zloty"} (get-in res [:localized :PLN :en])))
      (is (= :PLN (get-in res [:countries :PL]))))))

(deftest load-empty-or-nonmap-config
  (testing "load returns nil for empty or non-map configs"
    (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [_] (java.net.URL. "file:/tmp/empty.edn"))
                  clojure.core/slurp (fn [_] "{}")]
      (is (nil? (config/load "ignored.edn"))))
    (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [_] (java.net.URL. "file:/tmp/empty.edn"))
                  clojure.core/slurp (fn [_] "[]")]
      (is (nil? (config/load "ignored.edn"))))))
