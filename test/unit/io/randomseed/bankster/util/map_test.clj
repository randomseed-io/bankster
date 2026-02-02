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
  (testing "remove-if-value removes entries matching predicate"
    (is (= {:b 2} (um/remove-if-value {:a 1 :b 2} odd?)))
    (is (= {:a 1 :b 2} (um/remove-if-value {:a 1 :b 2} nil?)))
    (is (= {:b 2} (um/remove-if-value {:a nil :b 2} nil?))))
  (testing "remove-empty-values removes nil/empty and keeps non-empty"
    (is (= {:c 1} (um/remove-empty-values {:a nil :b [] :c 1 :d ""})))
    (is (= {:a nil :b [] :c 1 :d ""}
           (um/remove-empty-values {:a nil :b [] :c 1 :d ""} #{:x}))))
  (testing "remove-empty-values handles seqable non-counted values"
    (let [lazy-empty (lazy-seq nil)]
      (is (= {:b 1} (um/remove-empty-values {:a lazy-empty :b 1})))
      (is (= {:b 1} (um/remove-empty-values {:a lazy-empty :b 1} #{:a})))))
  (testing "remove-empty-values second arity removes only selected empty keys"
    (is (= {:c 1 :d "x"}
           (um/remove-empty-values {:a nil :b [] :c 1 :d "x"} #{:a :b}))))
  (testing "remove-by-if-value-in removes only keys from the set"
    (is (= {:a 1} (um/remove-by-if-value-in {:a 1 :b nil} nil? #{:b})))
    (is (= {:b 2} (um/remove-by-if-value-in {:a 1 :b 2} odd? #{:a})))
    (is (= {:a 2 :b 2} (um/remove-by-if-value-in {:a 2 :b 2} odd? #{:a}))))
  (testing "remove-if-value-in / remove-if-value-not-in"
    (is (= {:a 1} (um/remove-if-value-in {:a 1 :b 2} [2])))
    (is (= {:b 2} (um/remove-if-value-not-in {:a 1 :b 2} [2]))))
  (testing "remove-except keeps only selected keys"
    (is (= {:a 1} (um/remove-except {:a 1 :b 2} [:a]))))
  (testing "map-vals/map-keys/map-keys-and-vals"
    (is (= {:a 2 :b 3} (um/map-vals inc {:a 1 :b 2})))
    (is (= {:A 1 :B 2} (um/map-keys (comp keyword str/upper-case name) {:a 1 :b 2})))
    (is (= {:a "1" :b "2"}
           (um/map-keys-and-vals (fn [k v] [k (str v)]) {:a 1 :b 2})))))

(deftest map-vals-by-k-and-kv
  (testing "map-vals-by-k uses default dst map"
    (is (= {:a :A :b :B}
           (um/map-vals-by-k (comp keyword str/upper-case name) {:a 1 :b 2}))))
  (testing "map-vals-by-k supports destination map"
    (is (= {:a :A :b :B}
           (um/map-vals-by-k (comp keyword str/upper-case name) {:a 1 :b 2} {}))))
  (testing "map-vals-by-kv uses default dst map"
    (is (= {:a 2 :b 4}
           (um/map-vals-by-kv (fn [_k v] (* 2 v)) {:a 1 :b 2}))))
  (testing "map-vals-by-kv supports destination map"
    (is (= {:a 2 :b 4}
           (um/map-vals-by-kv (fn [_k v] (* 2 v)) {:a 1 :b 2} {})))))

(deftest map-keys-by-v-tests
  (testing "map-keys-by-v uses default dst map"
    (is (= {2 1 3 2} (um/map-keys-by-v inc {:a 1 :b 2}))))
  (testing "map-keys-by-v supports destination map"
    (is (= {:x 0 2 1 3 2} (um/map-keys-by-v inc {:a 1 :b 2} {:x 0})))))

(deftest invert-and-recursive-updates
  (testing "invert-in-sets preserves all keys in sets"
    (is (= {:x #{:a :b} :y #{:c}}
           (um/invert-in-sets {:a :x :b :x :c :y} #{}))))
  (testing "map-of-sets-invert preserves all values"
    (is (= {:x #{:a :b} :y #{:a}}
           (um/map-of-sets-invert {:a #{:x :y} :b #{:x}}))))
  (testing "map-of-vectors-invert-flatten flattens vectors into key->value"
    (is (= {:x :a :y :a :z :b}
           (um/map-of-vectors-invert-flatten {:a [:x :y] :b [:z]}))))
  (testing "update-values respects create-keys? flag"
    (is (= {:a 2 :b 2} (um/update-values {:a 1 :b 2} {:a inc})))
    (is (= {:a 2} (um/update-values {:a 1} {:a inc :b inc} false)))
    (is (= {:a 2 :b 1} (um/update-values {:a 1} {:a inc :b (constantly 1)} true))))
  (testing "update-values-recur visits nested maps and vectors"
    (let [m {:a 1 :b {:c 2} :d [{:e 3}]}]
      (is (= {:a 1 :b {:c 20} :d [{:e 30}]}
             (um/update-values-recur m {:c #(* 10 %) :e #(* 10 %)}))))
    (let [m {:a 1 :b {:c 2} :d [{:e 3}]}]
      (is (= {:a 1 :b {:c 20} :d [{:e 30}]}
             (um/update-values-recur m {:c #(* 10 %) :e #(* 10 %)} false)))))
  (testing "map-values recursively updates nested maps"
    (is (= {:a 2 :b {:c 3}}
           (um/map-values inc {:a 1 :b {:c 2}})))))

(deftest dissoc-in-and-remove-keys-ns
  (testing "dissoc-in removes nested keys but keeps empty maps"
    (is (= {:a {}} (um/dissoc-in {:a {:b 1}} [:a :b])))
    (is (= {:a 1} (um/dissoc-in {:a 1} [:b :c])))
    (is (= {:a {:b 1}} (um/dissoc-in {:a {:b 1}} [:a :c]))))
  (testing "remove-keys-ns strips namespaces from qualified keys"
    (is (= {:a 1 :b 2}
           (um/remove-keys-ns {:ns/a 1 :b 2})))))

(deftest remove-keys-ns-non-ident-branch
  (testing "remove-keys-ns uses name for non-ident when qualified-ident? is forced"
    (with-redefs [clojure.core/qualified-ident? (fn [_] true)]
      (is (= {"ns/name" 1}
             (um/remove-keys-ns {"ns/name" 1}))))))
