(ns

    ^{:doc    "bankster library, tests for money.inter-ops (transparent numeric wrappers)."
      :author "Paweł Wilk"
      :added  "2.2.0"
      :no-doc true}

    io.randomseed.bankster.money.inter-ops-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster.money    :as m]
            [io.randomseed.bankster.money.inter-ops :as iop])

  (:import (java.math BigDecimal)))

(deftest arithmetic-wrappers
  (let [calls (atom [])]
    (with-redefs [m/add (fn [& xs] (swap! calls conj [:add xs]) ::add)
                  m/sub (fn [& xs] (swap! calls conj [:sub xs]) ::sub)
                  m/mul (fn [& xs] (swap! calls conj [:mul xs]) ::mul)
                  m/div (fn [& xs] (swap! calls conj [:div xs]) ::div)]
      (let [m1 (m/of :PLN 1M)
            m2 (m/of :PLN 2M)]
        (testing "clojure.core fallbacks"
          (is (= 0 (iop/+)))
          (is (= 1 (iop/*)))
          (is (= 3 (iop/+ 1 2)))
          (is (= 3 (iop/+ 3)))
          (is (= -3 (iop/- 3)))
          (is (= 2 (iop/- 5 3)))
          (is (= 6 (iop/* 2 3)))
          (is (= 3 (iop/* 3)))
          (is (= (clojure.core// 6 2) (iop// 6 2)))
          (is (= (clojure.core// 6) (iop// 6))))
        (testing "Money dispatch for unary arities"
          (is (identical? m1 (iop/+ m1)))
          (is (= ::sub (iop/- m1)))
          (is (identical? m1 (iop/* m1)))
          (is (= ::div (iop// m1))))
        (testing "Money dispatch for binary arities"
          (is (= ::add (iop/+ m1 m2)))
          (is (= ::sub (iop/- m1 m2)))
          (is (= ::mul (iop/* m1 m2)))
          (is (= ::div (iop// m1 m2))))
        (testing "Money dispatch for variadic arities (money appears in :more)"
          (is (= ::add (iop/+ 1 2 m1)))
          (is (= ::add (iop/+ 1 m1 2)))
          (is (= ::add (iop/+ m1 2 3)))
          (is (= ::sub (iop/- 5 3 m1)))
          (is (= ::sub (iop/- 1 m1 2)))
          (is (= ::sub (iop/- m1 2 3)))
          (is (= ::mul (iop/* 2 3 m1)))
          (is (= ::mul (iop/* 2 m1 3)))
          (is (= ::mul (iop/* m1 2 3)))
          (is (= ::div (iop// 6 2 m1)))
          (is (= ::div (iop// 6 m1 2)))
          (is (= ::div (iop// m1 2 5))))
        (testing "variadic core fallbacks (no Money anywhere)"
          (is (= (clojure.core/- 10 3 2) (iop/- 10 3 2)))
          (is (= (clojure.core/+ 1 2 3) (iop/+ 1 2 3)))
          (is (= (clojure.core/* 2 3 4) (iop/* 2 3 4)))
          (is (= (clojure.core// 100 2 5) (iop// 100 2 5))))
        (is (some #(= [:add [1 2 m1]] %) @calls))
        (is (some #(= [:sub [5 3 m1]] %) @calls))
        (is (some #(= [:mul [2 3 m1]] %) @calls))
        (is (some #(= [:div [6 2 m1]] %) @calls))))))

(deftest comparison-wrappers
  (let [calls (atom [])]
    (with-redefs [m/eq? (fn [a b] (swap! calls conj [:eq [a b]]) true)
                  m/ne? (fn [a b] (swap! calls conj [:ne [a b]]) true)
                  m/gt? (fn [a b] (swap! calls conj [:gt [a b]]) true)
                  m/ge? (fn [a b] (swap! calls conj [:ge [a b]]) true)
                  m/lt? (fn [a b] (swap! calls conj [:lt [a b]]) true)
                  m/le? (fn [a b] (swap! calls conj [:le [a b]]) true)]
      (let [m1 (m/of :PLN 1M)
            m2 (m/of :PLN 2M)
            =  (var-get #'iop/=)
            not= (var-get #'iop/not=)
            >  (var-get #'iop/>)
            >= (var-get #'iop/>=)
            <  (var-get #'iop/<)
            <= (var-get #'iop/<=)]
        (testing "= and not="
          (is (true? (= 1)))
          (is (false? (not= 1)))
          (is (true? (= 1 1)))
          (is (false? (= 1 2)))
          (is (true? (not= 1 2)))
          (is (false? (not= 1 1)))
          (is (true? (= 1 1 1)))
          (is (true? (= 1 1 1 1))) ; recur branch
          (is (false? (= 1 1 2)))
          (is (false? (= 1 2 2)))
          (is (false? (not= 1 1 1)))
          (is (true? (not= 1 1 2))))
        (testing "Money vs non-money behavior"
          (is (true? (= m1 m2)))
          (is (true? (not= m1 m2)))
          (is (false? (= m1 1)))
          (is (false? (= 1 m1)))
          (is (true? (not= m1 1)))
          (is (true? (not= 1 m1))))
        (testing "ordering comparisons"
          (is (true? (> 1)))
          (is (true? (> 3 2)))
          (is (false? (> 2 3)))
          (is (true? (> 3 2 1)))
          (is (true? (> 4 3 2 1))) ; recur branch
          (is (false? (> 1 2 3)))
          (is (false? (> 3 2 2)))
          (is (true? (> m2 m1)))
          (is (false? (> m2 1)))
          (is (false? (> 1 m2)))
          (is (true? (>= 1)))
          (is (true? (>= 2 2)))
          (is (false? (>= 1 2)))
          (is (true? (>= 3 2 2)))
          (is (true? (>= 3 3 2 2))) ; recur branch
          (is (false? (>= 3 2 3)))
          (is (false? (>= 1 2 3)))
          (is (true? (>= m2 m1)))
          (is (false? (>= m2 1)))
          (is (false? (>= 1 m2)))
          (is (true? (< 1)))
          (is (true? (< 1 2)))
          (is (false? (< 2 1)))
          (is (true? (< 1 2 3)))
          (is (true? (< 1 2 3 4))) ; recur branch
          (is (false? (< 1 2 2)))
          (is (false? (< 2 1 0)))
          (is (true? (< m1 m2)))
          (is (false? (< m1 1)))
          (is (false? (< 1 m1)))
          (is (true? (<= 1)))
          (is (true? (<= 2 2)))
          (is (false? (<= 2 1)))
          (is (true? (<= 1 2 2)))
          (is (true? (<= 1 1 2 2))) ; recur branch
          (is (false? (<= 1 2 1)))
          (is (false? (<= 2 1 0)))
          (is (true? (<= m1 m2)))
          (is (false? (<= m1 1)))
          (is (false? (<= 1 m1)))))
      (is (some #(= :eq (first %)) @calls))
      (is (some #(= :ne (first %)) @calls))
      (is (some #(= :gt (first %)) @calls))
      (is (some #(= :ge (first %)) @calls))
      (is (some #(= :lt (first %)) @calls))
      (is (some #(= :le (first %)) @calls)))))

(deftest coercions-and-predicates
  (let [m-pos (m/of :PLN 1.25M)
        m-neg (m/of :PLN -1.25M)
        long  (var-get #'iop/long)
        int   (var-get #'iop/int)
        double (var-get #'iop/double)
        float  (var-get #'iop/float)
        pos?  (var-get #'iop/pos?)
        neg?  (var-get #'iop/neg?)]
    (testing "numeric fallbacks"
      (is (= 12 (long 12)))
      (is (= 12 (int 12)))
      (is (= 1.5 (double 1.5)))
      (is (= 1.5 (float 1.5)))
      (is (true? (pos? 1)))
      (is (true? (neg? -1))))
    (testing "Money paths"
      (is (= (clojure.core/long (m/major->long m-pos)) (long m-pos)))
      (is (= (clojure.core/int  (m/major->int  m-pos)) (int  m-pos)))
      (is (= (clojure.core/double (m/->double m-pos)) (double m-pos)))
      (is (= (clojure.core/float  (m/->float  m-pos)) (float  m-pos)))
      (is (true?  (pos? m-pos)))
      (is (false? (neg? m-pos)))
      (is (false? (pos? m-neg)))
      (is (true?  (neg? m-neg))))))

(deftest aliases-and-compare
  (let [x 12.34M
        m1 (m/of :PLN 12.34M)
        m2 (m/of :PLN 12.34M)]
    (testing "scale aliases are callable"
      (is (= 12M (iop/integer x)))
      (is (= 34M (iop/fractional x))))
    (testing "money aliases are callable"
      (is (instance? BigDecimal (iop/major m1)))
      (is (instance? BigDecimal (iop/minor m1))))
    (testing "compare dispatch"
      (let [compare (var-get #'iop/compare)]
        (is (= (m/compare m1 m2) (compare m1 m2)))
        (is (= 0 (compare 1 1)))))))
