(ns

    ^{:doc    "bankster library, util tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.util-test

  (:require [clojure.test                 :refer [deftest testing is]]
            [io.randomseed.bankster.util  :as u])

  (:import (java.util ArrayList)))

(deftest keyword-coercions
  (testing "ensure-keyword preserves and converts identifiers"
    (is (= :a (u/ensure-keyword :a)))
    (is (= :a (u/ensure-keyword 'a)))
    (is (= :ns/a (u/ensure-keyword 'ns/a)))
    (is (= :123 (u/ensure-keyword 123))))
  (testing "must-have-ns adds namespace when missing"
    (is (= :x/a (u/must-have-ns :a "x")))
    (is (= 'x/a (u/must-have-ns 'a "x")))
    (is (= :x/a (u/must-have-ns :x/a "x")))))

(deftest slash-splitting
  (testing "split-on-first-slash"
    (is (= [nil nil] (u/split-on-first-slash nil)))
    (is (= [nil nil] (u/split-on-first-slash "")))
    (is (= ["abc" nil] (u/split-on-first-slash "abc")))
    (is (= ["a" "b/c"] (u/split-on-first-slash "a/b/c")))
    (is (= [nil "a"] (u/split-on-first-slash "/a")))
    (is (= ["a" nil] (u/split-on-first-slash "a/")))))

(deftest inferred-key-access
  (testing "inferred-contains?/get check both qualified and simple key variants"
    (let [m {:a 1 :ns/a 2}]
      (is (true? (u/inferred-contains? m :a)))
      (is (true? (u/inferred-contains? m :ns/a)))
      (is (= 2 (u/inferred-get m :ns/a)))
      (is (= 1 (u/inferred-get {:a 1} :ns/a)))
      (is (= :nope (u/inferred-get {} :ns/a :nope))))))

(deftest collection-helpers
  (testing "with-not-empty returns nil for empty and identity for non-empty"
    (is (= nil (u/with-not-empty [])))
    (is (= [1] (u/with-not-empty [1]))))
  (testing "remove-from-set-where and keep-in-set-where return nil when empty"
    (is (= nil (u/remove-from-set-where even? #{2})))
    (is (= nil (u/keep-in-set-where even? #{1})))
    (is (= #{1 3} (u/remove-from-set-where even? #{1 2 3})))
    (is (= #{2} (u/keep-in-set-where even? #{1 2 3})))))

(deftest macros-try-null-and-is
  (testing "try-null catches NPE"
    (is (= nil (u/try-null (.toString nil)))))
  (testing "is / is-not return original value when predicate fails/succeeds"
    (is (= -10 (u/is pos? -10 123)))
    (is (= 123 (u/is pos? 10 123)))
    (is (= 10 (u/is-not pos? 10 123)))
    (is (= 123 (u/is-not pos? -10 123)))))

(deftest numeric-utils
  (testing "count-digits"
    (is (= 1 (u/count-digits 0)))
    (is (= 1 (u/count-digits 9)))
    (is (= 2 (u/count-digits 10)))
    (is (= 3 (u/count-digits 999))))
  (testing "try-parse-int/long"
    (is (= 12 (u/try-parse-int "12")))
    (is (= nil (u/try-parse-int "nope")))
    (is (= 12 (u/try-parse-int 12.0)))
    (is (= 12 (u/try-parse-long "12")))
    (is (= nil (u/try-parse-long "")))))

(deftest bytes-and-text
  (testing "bytes roundtrip is stable for ASCII"
    (let [b (u/to-bytes "abc")]
      (is (= "abc" (String. b "UTF-8")))
      (is (= "abc" (u/bytes-to-string b)))))
  (testing "bytes-concat skips empty arrays and returns nil for empty input"
    (is (= nil (u/bytes-concat)))
    (let [a (u/to-bytes "a")
          b (byte-array 0)
          c (u/to-bytes "c")]
      (is (= "ac" (u/bytes-to-string (u/bytes-concat a b c))))))
  (testing "to-long uses default on parse failure"
    (is (= 7 (u/to-long nil 7)))
    (is (= 12 (u/to-long "12" 7)))
    (is (= 7 (u/to-long "nope" 7)))))

(deftest lazy-iterator-seq-test
  (testing "lazy-iterator-seq exposes Java Iterable as a seq"
    (let [al (doto (ArrayList.) (.add 1) (.add 2) (.add 3))]
      (is (= [1 2 3] (vec (u/lazy-iterator-seq al)))))))

(deftest char-ranges
  (testing "char-ranges->set expands inclusive ranges"
    (let [s (u/char-ranges->set [\A \C] [\0 \2])]
      (is (contains? s \A))
      (is (contains? s \C))
      (is (contains? s \2))
      (is (not (contains? s \D))))))
