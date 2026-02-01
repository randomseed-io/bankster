(ns

    ^{:doc    "bankster library, util.map tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.util.map-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [clojure.string                  :as str]
            [io.randomseed.bankster.util.map :as um]))

(deftest lazy-get-does-not-evaluate-default
  (testing "lazy-get does not evaluate default expression when key exists"
    (let [hits (atom 0)
          dflt (fn [] (swap! hits inc) :d)]
      (is (= :x (um/lazy-get {:a :x} :a (dflt))))
      (is (= 0 @hits))
      (is (= :d (um/lazy-get {:a :x} :b (dflt))))
      (is (= 1 @hits)))))

(deftest update-existing-and-missing
  (testing "update-existing updates only when key exists"
    (is (= {:a 2} (um/update-existing {:a 1} :a inc)))
    (is (= {:a 1} (um/update-existing {:a 1} :b inc)))
    (is (= {:a 0} (um/update-existing {:a 1} :a 0))))
  (testing "update-missing updates only when key is missing"
    (is (= {:a 1} (um/update-missing {} :a (fn [x] x) 1)))
    (is (= {:a 2} (um/update-missing {} :a + 1 1)))
    (is (= {:a 1} (um/update-missing {:a 1} :a inc)))))

(deftest assoc-if-family
  (testing "assoc-if variants"
    (is (= {:a 1} (um/assoc-if {} true :a 1)))
    (is (= {}     (um/assoc-if {} false :a 1)))
    (is (= {}     (um/assoc-if-not {} true :a 1)))
    (is (= {:a 1} (um/assoc-if-not {} false :a 1)))
    (is (= {:a 2} (um/assoc-if-key {:a 1} :a odd? 2)))
    (is (= {:a 1} (um/assoc-if-key {:a 1} :a even? 2)))
    (is (= {:a 1} (um/assoc-if-not-key {:a 1} :a odd? 2)))
    (is (= {:a 2} (um/assoc-if-not-key {:a 1} :a even? 2)))))

(deftest removals-and-mappers
  (testing "remove-empty-values removes nil/empty and keeps non-empty"
    (is (= {:c 1} (um/remove-empty-values {:a nil :b [] :c 1 :d ""})))
    (is (= {:a nil :b [] :c 1 :d ""}
           (um/remove-empty-values {:a nil :b [] :c 1 :d ""} #{:x}))))
  (testing "map-vals/map-keys/map-keys-and-vals"
    (is (= {:a 2 :b 3} (um/map-vals inc {:a 1 :b 2})))
    (is (= {:A 1 :B 2} (um/map-keys (comp keyword str/upper-case name) {:a 1 :b 2})))
    (is (= {:a "1" :b "2"}
           (um/map-keys-and-vals (fn [k v] [k (str v)]) {:a 1 :b 2})))))

(deftest invert-and-recursive-updates
  (testing "invert-in-sets preserves all keys in sets"
    (is (= {:x #{:a :b} :y #{:c}}
           (um/invert-in-sets {:a :x :b :x :c :y} #{}))))
  (testing "map-of-vectors-invert-flatten flattens vectors into key->value"
    (is (= {:x :a :y :a :z :b}
           (um/map-of-vectors-invert-flatten {:a [:x :y] :b [:z]}))))
  (testing "update-values respects create-keys? flag"
    (is (= {:a 2} (um/update-values {:a 1} {:a inc :b inc} false)))
    (is (= {:a 2 :b 1} (um/update-values {:a 1} {:a inc :b (constantly 1)} true))))
  (testing "update-values-recur visits nested maps and vectors"
    (let [m {:a 1 :b {:c 2} :d [{:e 3}]}]
      (is (= {:a 1 :b {:c 20} :d [{:e 30}]}
             (um/update-values-recur m {:c #(* 10 %) :e #(* 10 %)} false))))))

(deftest dissoc-in-and-remove-keys-ns
  (testing "dissoc-in removes nested keys but keeps empty maps"
    (is (= {:a {}} (um/dissoc-in {:a {:b 1}} [:a :b])))
    (is (= {:a {:b 1}} (um/dissoc-in {:a {:b 1}} [:a :c]))))
  (testing "remove-keys-ns strips namespaces from qualified keys"
    (is (= {:a 1 :b 2}
           (um/remove-keys-ns {:ns/a 1 :b 2})))))
