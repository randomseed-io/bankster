(ns

    ^{:doc    "bankster library, transparent wrappers tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.inter-ops-test

  (:require [clojure.test :refer [deftest testing is]]
            ;; Force side-effectful init of readers/registry used by constructors.
            [io.randomseed.bankster]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.money.inter-ops :as i]
            [io.randomseed.bankster.money]
            [io.randomseed.bankster.scale :as scale]))

(defn- m
  "Helper to construct Money quickly."
  [amount]
  (io.randomseed.bankster.money/of :PLN amount))

(defn- eur
  "Helper to construct Money quickly (EUR)."
  [amount]
  (io.randomseed.bankster.money/of :EUR amount))

(defn- assert-money
  [mv expected-currency expected-amount]
  (is (true? (io.randomseed.bankster.money/money? mv)))
  (is (= expected-currency (currency/id mv)))
  (is (= expected-amount (scale/amount mv))))

(deftest equality-operators
  (testing "= on non-Money"
    (is (true?  (i/= 1)))
    (is (true?  (i/= 1 1)))
    (is (false? (i/= 1 2)))
    (is (true?  (i/= 1 1 1)))
    (is (false? (i/= 1 1 2))))
  (testing "= on Money"
    (let [a (m 10M)
          b (m 10M)
          c (m 11M)]
      (is (true?  (i/= a b)))
      (is (false? (i/= a c)))
      (is (true?  (i/= a b a)))
      (is (false? (i/= a b c)))
      (is (true?  (i/= (m 0M) (m 0M))))
      (is (true?  (i/= (m -10M) (m -10M))))
      (is (false? (i/= (m -10M) (m 10M))))
      (is (false? (i/= a 10M)))
      (is (false? (i/= 10M a)))))
  (testing "not= on non-Money"
    (is (false? (i/not= 1)))
    (is (false? (i/not= 1 1)))
    (is (true?  (i/not= 1 2)))
    (is (true?  (i/not= 1 2 3)))
    ;; same as (not (= ...))
    (is (true?  (i/not= 1 1 2)))
    (is (false? (i/not= 1 1 1))))
  (testing "not= on Money"
    (let [a (m 10M)
          b (m 10M)
          c (m 11M)]
      (is (false? (i/not= a b)))
      (is (true?  (i/not= a c)))
      (is (false? (i/not= a b a)))
      (is (true?  (i/not= a b c)))
      (is (true?  (i/not= (m 0M) (m 1M))))
      (is (false? (i/not= (m -1M) (m -1M))))
      (is (true?  (i/not= a 10M)))
      (is (true?  (i/not= 10M a))))))

(deftest ordering-operators
  (testing "ordering on non-Money"
    (is (true?  (i/> 3 2 1)))
    (is (false? (i/> 3 2 2)))
    (is (true?  (i/>= 3 3 2 2)))
    (is (false? (i/>= 3 2 3)))
    (is (true?  (i/< 1 2 3)))
    (is (false? (i/< 1 2 2)))
    (is (true?  (i/<= 1 1 2 2)))
    (is (false? (i/<= 2 1 2))))
  (testing "ordering on Money"
    (let [a (m 1M)
          b (m 2M)
          c (m 3M)]
      (is (true?  (i/< a b c)))
      (is (true?  (i/<= a a b b)))
      (is (true?  (i/> c b a)))
      (is (true?  (i/>= c c b b)))
      (is (true?  (i/> (m 0M) (m -1M))))
      (is (true?  (i/< (m -2M) (m -1M) (m 0M))))
      (is (false? (i/> a b)))
      (is (false? (i/< c b)))))
  (testing "ordering with mixed Money/non-Money returns false"
    (let [a (m 1M)]
      (is (false? (i/> a 0M)))
      (is (false? (i/> 0M a)))
      (is (false? (i/< a 0M)))
      (is (false? (i/< 0M a)))
      (is (false? (i/>= a 0M)))
      (is (false? (i/>= 0M a)))
      (is (false? (i/<= a 0M)))
      (is (false? (i/<= 0M a))))))

(deftest arithmetic-operators
  (testing "core-like behavior on non-Money values"
    (is (= (+) (i/+)))
    (is (= (*) (i/*)))
    (is (= (+ 1) (i/+ 1)))
    (is (= (* 2) (i/* 2)))
    (is (= (+ 1 2 3) (i/+ 1 2 3)))
    (is (= (* 2 3 4) (i/* 2 3 4)))
    (is (instance? Long (i/+)))
    (is (instance? Long (i/*))))
  (testing "unary + rejects non-numeric values like clojure.core/+"
    (is (thrown? ClassCastException (i/+ :foo))))
  (testing "+ on Money (same currency)"
    (assert-money (i/+ (m 1M) (m 2M)) :PLN 3M)
    (assert-money (i/+ (m 1.10M) (m 2.20M)) :PLN 3.30M)
    (assert-money (i/+ (m -1M) (m 2M)) :PLN 1M)
    (assert-money (i/+ (m 1M) (m 2M) (m 3M)) :PLN 6M))
  (testing "- on Money (same currency)"
    (assert-money (i/- (m 5M)) :PLN -5M)
    (assert-money (i/- (m 5M) (m 2M)) :PLN 3M)
    (assert-money (i/- (m 10M) (m 2M) (m 3M)) :PLN 5M))
  (testing "* on Money"
    (assert-money (i/* (m 10M) 2) :PLN 20M)
    (assert-money (i/* 2 (m 10M)) :PLN 20M)
    (assert-money (i/* (m 10M) 2 3) :PLN 60M))
  (testing "/ on Money"
    (assert-money (i// (m 10M) 2) :PLN 5M)
    (is (thrown? clojure.lang.ExceptionInfo (i// (m 1M))))
    (assert-money (i// (m 1M) 4) :PLN 0.25M)))

(deftest compare-and-sign
  (testing "compare on non-Money"
    (is (neg? (i/compare 1 2)))
    (is (zero? (i/compare 2 2)))
    (is (pos? (i/compare 3 2))))
  (testing "compare on Money"
    (is (neg? (i/compare (m 1M) (m 2M))))
    (is (zero? (i/compare (m 2M) (m 2M))))
    (is (pos? (i/compare (m 3M) (m 2M))))
    (is (zero? (i/compare (m 0M) (m 0M))))
    (is (neg? (i/compare (m -1M) (m 0M))))
    (is (pos? (i/compare (m 0M) (m -1M)))))
  (testing "pos?/neg? on non-Money"
    (is (true?  (i/pos? 1)))
    (is (false? (i/pos? 0)))
    (is (false? (i/pos? -1)))
    (is (true?  (i/neg? -1)))
    (is (false? (i/neg? 0)))
    (is (false? (i/neg? 1))))
  (testing "pos?/neg? on Money"
    (is (true?  (i/pos? (m 1M))))
    (is (false? (i/pos? (m 0M))))
    (is (false? (i/pos? (m -1M))))
    (is (true?  (i/neg? (m -1M))))
    (is (false? (i/neg? (m 0M))))
    (is (false? (i/neg? (m 1M))))
    (is (true?  (i/pos? (m 0.01M))))
    (is (true?  (i/neg? (m -0.01M))))))

(deftest numeric-casts
  (testing "casts on non-Money"
    (is (= 12 (i/long 12.9)))
    (is (= 12 (i/int 12.9)))
    (is (= 12.0 (i/double 12)))
    (is (= (float 12.0) (i/float 12))))
  (testing "casts on Money (major component)"
    (let [x (m 12M)]
      (is (= 12 (i/long x)))
      (is (= 12 (i/int x)))
      (is (= 12.0 (i/double x)))
      (is (= (float 12.0) (i/float x)))))
  (testing "casts on Money (negative/zero)"
    (let [z (m 0M)
          n (m -12M)]
      (is (= 0  (i/long z)))
      (is (= 0  (i/int z)))
      (is (= 0.0 (i/double z)))
      (is (= (float 0.0) (i/float z)))
      (is (= -12 (i/long n)))
      (is (= -12 (i/int n)))
      (is (= -12.0 (i/double n)))
      (is (= (float -12.0) (i/float n))))))

(deftest cross-currency-safety
  (testing "operators that require same currency should reject mismatched Money"
    (let [a (m 1M)
          b (eur 1M)]
      (is (thrown? Throwable (i/+ a b)))
      (is (thrown? Throwable (i/- a b)))
      (is (thrown? Throwable (i/compare a b)))
      (is (thrown? Throwable (i/> a b)))
      (is (thrown? Throwable (i/>= a b)))
      (is (thrown? Throwable (i/< a b)))
      (is (thrown? Throwable (i/<= a b)))
      ;; equality is defined, but should be false across currencies
      (is (false? (i/= a b)))
      (is (true?  (i/not= a b))))))
