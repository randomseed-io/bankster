(ns

    ^{:doc    "bankster library, util tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.util-test

  (:require [clojure.test                 :refer [deftest testing is]]
            [clojure.spec.alpha           :as s]
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
    (is (= :x/a (u/must-have-ns :x/a "x"))))
  (testing "ensure-keyword-having-ns forces namespace"
    (is (= :x/a (u/ensure-keyword-having-ns :a "x")))
    (is (= :x/a (u/ensure-keyword-having-ns 'a "x")))))

(deftest spec-asserts-coverage
  (let [prev (s/check-asserts true)]
    (try
      (is (= :x/a (u/must-have-ns :a "x")))
      (is (= #{:b} (u/replace-in-set #{:a} :a :b)))
      (let [b (byte-array [1 2 3])]
        (is (identical? b (u/to-bytes b))))
      (let [b (u/to-bytes "x")]
        (is (= "x" (u/bytes-to-string b))))
      (is (= 7 (u/to-long "7" 0)))
      (finally
        (s/check-asserts prev)))))

(deftest spec-not-conflicting-ns
  (testing "not-conflicting-ns spec predicate"
    (is (true? (s/valid? ::u/not-conflicting-ns ["x" :a])))
    (is (true? (s/valid? ::u/not-conflicting-ns ["x" :x/a])))
    (is (false? (s/valid? ::u/not-conflicting-ns ["x" :y/a])))))

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
      (is (true? (u/inferred-contains? {:a 1} :ns/a)))
      (is (= 2 (u/inferred-get m :ns/a)))
      (is (= 1 (u/inferred-get {:a 1} :a)))
      (is (= 1 (u/inferred-get {:a 1} :ns/a)))
      (is (= :nope (u/inferred-get {} :ns/a :nope)))
      (is (= false (u/inferred-contains? {} :a))))))

(deftest collection-helpers
  (testing "with-not-empty returns nil for empty and identity for non-empty"
    (is (= nil (u/with-not-empty [])))
    (is (= [1] (u/with-not-empty [1]))))
  (testing "remove-from-set-where and keep-in-set-where return nil when empty"
    (is (= nil (u/remove-from-set-where even? #{2})))
    (is (= nil (u/keep-in-set-where even? #{1})))
    (is (= #{1 3} (u/remove-from-set-where even? #{1 2 3})))
    (is (= #{2} (u/keep-in-set-where even? #{1 2 3}))))
  (testing "remove-from-set-where supports non-transient sets"
    (is (= #{1 3} (u/remove-from-set-where even? (sorted-set 1 2 3)))))
  (testing "keep-in-set-where supports non-transient sets"
    (is (= #{2} (u/keep-in-set-where even? (sorted-set 1 2 3)))))
  (testing "replace-in-set swaps values"
    (is (= #{:b} (u/replace-in-set #{:a} :a :b)))))

(deftest macros-try-null-and-is
  (testing "try-null catches NPE"
    (is (= nil (u/try-null (.toString nil)))))
  (testing "is / is-not return original value when predicate fails/succeeds"
    (is (= -10 (u/is pos? -10 123)))
    (is (= 123 (u/is pos? 10 123)))
    (is (= 10 (u/is-not pos? 10 123)))
    (is (= 123 (u/is-not pos? -10 123)))))

(defn- auto-alias-in-temp-ns
  [form]
  (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.auto-alias-" (gensym)))
        ns-obj (create-ns ns-sym)]
    (binding [*ns* ns-obj]
      (eval '(clojure.core/require 'io.randomseed.bankster.util))
      (eval '(clojure.core/require 'io.randomseed.bankster.api))
      (eval form))
    (ns-resolve ns-obj 'money)))

(deftest auto-alias-macro-branches
  (testing "auto-alias accepts symbol, quoted symbol and keyword"
    (is (auto-alias-in-temp-ns '(io.randomseed.bankster.util/auto-alias io.randomseed.bankster.api)))
    (is (auto-alias-in-temp-ns '(io.randomseed.bankster.util/auto-alias 'io.randomseed.bankster.api)))
    (is (auto-alias-in-temp-ns '(io.randomseed.bankster.util/auto-alias :io.randomseed.bankster.api)))))

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
    (is (= 12 (u/try-parse-long 12.0)))
    (is (= nil (u/try-parse-long "")))
    (is (= nil (u/try-parse-long "nope")))))

(deftest ns-infer-and-thread-utils
  (testing "ns-infer respects existing namespace and opt flag"
    (is (= :x/a (u/ns-infer "x" :a)))
    (is (= :ns/a (u/ns-infer "x" :ns/a)))
    (is (= :a (u/ns-infer "x" :a false))))
  (testing "current-thread helpers return sensible values"
    (is (integer? (u/current-thread-id)))
    (is (string? (u/current-thread-name)))
    (is (instance? Thread (u/current-thread))))
  (testing "current-thread-id falls back when threadId method is unavailable"
    (let [orig-make-array clojure.core/make-array
          calls (atom 0)]
      (with-redefs [clojure.core/make-array (fn [c n]
                                              (swap! calls inc)
                                              (if (= 1 @calls)
                                                (into-array Class [String])
                                                (orig-make-array c n)))]
        (is (integer? (u/current-thread-id)))))))

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
  (testing "bytes-concat accepts single array"
    (let [a (u/to-bytes "a")]
      (is (= "a" (u/bytes-to-string (u/bytes-concat a))))))
  (testing "text-to-bytes handles bytes and nil"
    (let [a (u/to-bytes "x")]
      (is (= a (u/text-to-bytes a)))
      (let [b (u/text-to-bytes nil)]
        (is (bytes? b))
        (is (zero? (alength b))))
      (is (= "x" (u/bytes-to-string (u/text-to-bytes "x"))))))
  (testing "to-long uses default on parse failure"
    (is (= 7 (u/to-long nil 7)))
    (is (= 12 (u/to-long "12" 7)))
    (is (= 7 (u/to-long "nope" 7)))))

(deftest random-and-seq-helpers
  (testing "get-rand-int uses optional RNG"
    (let [rng (java.util.Random. 0)]
      (is (<= 0 (u/get-rand-int 10) 9))
      (is (<= 0 (u/get-rand-int 10 rng) 9))
      (is (<= 0 (u/get-rand-int 10 nil) 9))
      (is (= 0 (u/get-rand-int 0 rng)))))
  (testing "random-digits-len handles shrink and RNG"
    (let [rng (java.util.Random. 0)]
      (is (= 0 (u/random-digits-len 0 1 true)))
      (is (pos? (u/random-digits-len 5 0 true)))
      (is (pos? (u/random-digits-len 5 5 true)))
      (is (pos? (u/random-digits-len 5 1 true nil)))
      (is (pos? (u/random-digits-len 5 0 true rng)))
      (is (= 0 (u/random-digits-len 0 1 true rng)))
      (is (pos? (u/random-digits-len 5 5 true rng)))
      (is (= 5 (u/random-digits-len 5 3 false)))
      (is (= 5 (u/random-digits-len 5 3 false rng)))))
  (testing "gen-digits supports RNG"
    (let [rng (java.util.Random. 0)]
      (is (string? (u/gen-digits 4)))
      (is (string? (u/gen-digits 4 rng)))
      (is (string? (u/gen-digits 4 nil)))))
  (testing "get-rand-nth supports RNG and empty input"
    (let [rng (java.util.Random. 0)]
      (is (nil? (u/get-rand-nth [])))
      (is (#{:a :b} (u/get-rand-nth [:a :b] rng)))
      (is (#{:a :b} (u/get-rand-nth [:a :b] nil)))
      (is (#{:a :b} (u/get-rand-nth [:a :b])))))
  (testing "juxt-seq returns a lazy seq of results"
    (let [f (u/juxt-seq inc dec)]
      (is (= [2 0] (vec (f 1))))))
  (testing "uuid and try-upper-case helpers"
    (is (= (java.util.UUID/fromString "00000000-0000-0000-0000-000000000000")
           (u/uuid "00000000-0000-0000-0000-000000000000")))
    (is (= "AB" (u/try-upper-case "ab")))
    (is (nil? (u/try-upper-case nil)))))

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

(deftest some-string-helpers
  (testing "some-string returns nil for non-strings and empty strings"
    (is (nil? (u/some-string nil)))
    (is (nil? (u/some-string "")))
    (is (nil? (u/some-string 123))))
  (testing "some-string returns value for non-empty strings"
    (is (= "x" (u/some-string "x")))))
