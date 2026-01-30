(ns

    ^{:doc    "bankster library, JSR-354 compat tests."
      :author "Paweł Wilk"
      :added  "3.0.0"
      :no-doc true}

    io.randomseed.bankster.jsr-354-test

  (:require [clojure.test                :refer [deftest testing is]]
            [io.randomseed.bankster.jsr-354 :as jsr]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.money    :as    money]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.scale    :as    scale])

  (:import (io.randomseed.bankster Currency Money Registry)
           (io.randomseed.bankster.jsr_354 MonetaryContext)
           (java.math BigDecimal)))

(defn- amount=
  [^Money money ^BigDecimal expected]
  (.equals ^BigDecimal (money/amount money) ^BigDecimal expected))

(deftest monetary-context-builder
  (testing "defaults"
    (let [ctx (jsr/context)]
      (is (instance? MonetaryContext ctx))
      (is (identical? Money       (:amount-type ctx)))
      (is (identical? BigDecimal  (:number-type ctx)))
      (is (nil? (:precision ctx)))
      (is (nil? (:max-scale ctx)))
      (is (nil? (:fixed-scale? ctx)))
      (testing "standardized attrs"
        (is (= Money      (get (:attrs ctx) jsr/attr-amount-type)))
        (is (= BigDecimal (get (:attrs ctx) jsr/attr-number-type))))
      (testing "bankster extras"
        (is (= :weight (get-in ctx [:bankster :currency-resolution])))
        (is (nil? (get-in ctx [:bankster :registry])))
        (is (nil? (get-in ctx [:bankster :registry-version]))))))

  (testing "attrs keys are normalized to strings"
    (let [ctx   (jsr/context {:attrs {:foo 1 :ns/bar 2 "baz" 3 'qux 4 'ns/sym 5 42 6}})
          attrs (:attrs ctx)]
      (is (= 1 (get attrs "foo")))
      (is (= 2 (get attrs "ns/bar")))
      (is (= 3 (get attrs "baz")))
      (is (= 4 (get attrs "qux")))
      (is (= 5 (get attrs "ns/sym")))
      (is (= 6 (get attrs "42")))))

  (testing "precision/max-scale/fixed-scale? are reflected both in fields and attrs"
    (let [ctx   (jsr/context {:precision 12 :max-scale 4 :fixed-scale? true})
          attrs (:attrs ctx)]
      (is (= 12 (:precision ctx)))
      (is (= 4  (:max-scale ctx)))
      (is (true? (:fixed-scale? ctx)))
      (is (= 12 (get attrs jsr/attr-precision)))
      (is (= 4  (get attrs jsr/attr-max-scale)))
      (is (= true (get attrs jsr/attr-fixed-scale)))))

  (testing "registry and registry-version can be derived"
    (let [r   (registry/new)
          ctx (jsr/context {:registry r})
          v   (get-in ctx [:bankster :registry-version])]
      (is (instance? Registry (get-in ctx [:bankster :registry])))
      (is (string? v))
      (is (re-matches #"\d{16}" v))))

  (testing "bankster extras may be passed explicitly"
    (let [r   (registry/new)
          ctx (jsr/context {:bankster {:registry r}
                            :rounding-mode :HALF_EVEN
                            :rescale-each? true})]
      (is (instance? Registry (get-in ctx [:bankster :registry])))
      (is (re-matches #"\d{16}" (get-in ctx [:bankster :registry-version])))
      (is (identical? scale/ROUND_HALF_EVEN (get-in ctx [:bankster :rounding-mode])))
      (is (true? (get-in ctx [:bankster :rescale-each?])))))

  (testing "invalid inputs are rejected"
    (is (thrown? clojure.lang.ExceptionInfo (jsr/context {:attrs 1})))
    (is (thrown? clojure.lang.ExceptionInfo (jsr/context {:registry {}})))
    (is (thrown? clojure.lang.ExceptionInfo (jsr/context {:bankster {:registry {}}})))))

(deftest with-context-binds-bankster-runtime
  (testing "registry binding influences currency resolution (weight wins)"
    (let [c-a  (currency/new :AAA)
          c-ca (currency/new :crypto/AAA)
          r    (-> (registry/new)
                   (currency/register c-a)
                   (currency/register c-ca)
                   (currency/set-weight :AAA 10)
                   (currency/set-weight :crypto/AAA 0))
          ctx  (jsr/context {:registry r
                             :rounding-mode :HALF_EVEN
                             :rescale-each? true})]
      (jsr/with-context ctx
        (is (identical? r registry/*default*))
        (is (identical? scale/ROUND_HALF_EVEN scale/*rounding-mode*))
        (is (true? scale/*each*))
        (is (= :crypto/AAA (currency/id (jsr/get-currency :AAA)))))))

  (testing "rounding-mode binding enables divisions that would otherwise fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (jsr/divide (jsr/create :EUR 1) 3)))
    (let [ctx (jsr/context {:rounding-mode :HALF_EVEN})]
      (jsr/with-context ctx
        (is (= #money[0.33 EUR] (jsr/divide (jsr/create :EUR 1) 3))))))

  (testing "camelCase macro alias works"
    (let [ctx (jsr/context {:rounding-mode :HALF_EVEN})]
      (jsr/withContext ctx
        (is (identical? scale/ROUND_HALF_EVEN scale/*rounding-mode*))))))

(deftest jsr-354-currencyunit-api
  (testing "currency resolution and list"
    (is (instance? Currency (jsr/get-currency :EUR)))
    (is (instance? Currency (jsr/getCurrency :EUR)))
    (is (contains? (set (map currency/id (jsr/get-currencies))) :EUR))
    (is (contains? (set (map currency/id (jsr/getCurrencies))) :EUR)))

  (testing "CurrencyUnit-like properties"
    (is (= "EUR" (jsr/get-currency-code :EUR)))
    (is (= "EUR" (jsr/getCurrencyCode #money[1 EUR])))
    (is (= 978 (jsr/get-numeric-code :EUR)))
    (is (= 978 (jsr/getNumericCode #money[1 EUR])))
    (is (= 2 (jsr/get-default-fraction-digits :EUR)))
    (is (= 2 (jsr/getDefaultFractionDigits #money[1 EUR])))))

(deftest jsr-354-monetary-api
  (testing "default rounding operator rescales to currency nominal scale"
    (let [eur (jsr/get-currency :EUR)
          m   (Money. ^Currency eur 10.125M)]
      (is (= 3 (money/scale m)))
      (is (amount= ((jsr/get-default-rounding) m) 10.12M))
      (is (amount= ((jsr/getDefaultRounding) m) 10.12M))))

  (testing "default rounding operator respects a bound rounding-mode"
    (binding [scale/*rounding-mode* scale/ROUND_HALF_UP]
      (let [eur (jsr/get-currency :EUR)
            m   (Money. ^Currency eur 10.125M)]
        (is (amount= ((jsr/get-default-rounding) m) 10.13M))))))

(deftest jsr-354-monetaryamount-api
  (testing "construction and number accessors"
    (let [m (jsr/create :EUR 10)]
      (is (instance? Money m))
      (is (= "EUR" (jsr/getCurrencyCode m)))
      (is (amount= m 10.00M))
      (is (.equals 10.00M (jsr/get-number m)))
      (is (.equals (.stripTrailingZeros 10.00M) (jsr/get-number-stripped m)))
      (is (.equals (.stripTrailingZeros 10.00M) (jsr/getNumberStripped m)))))

  (testing "arithmetic"
    (let [a #money[10 EUR]
          b #money[5 EUR]]
      (is (= #money[15 EUR] (jsr/add a b)))
      (is (= #money[5 EUR]  (jsr/subtract a b)))
      (is (= #money[20 EUR] (jsr/multiply a 2)))
      (is (= #money[5 EUR]  (jsr/divide a 2)))
      (is (= #money[3.33 EUR] (jsr/divide #money[10 EUR] 3 scale/ROUND_HALF_EVEN)))
      (is (= #money[2 EUR]  (jsr/remainder a 4)))
      (is (= #money[2 EUR]  (jsr/remainder a 4 scale/ROUND_HALF_EVEN)))))

  (testing "numeric-argument guards (JSR compat expects numbers, not Money)"
    (is (thrown? clojure.lang.ExceptionInfo
                 (jsr/multiply #money[1 EUR] #money[2 EUR])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (jsr/divide #money[1 EUR] #money[2 EUR])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (jsr/remainder #money[1 EUR] #money[2 EUR]))))

  (testing "sign and magnitude"
    (is (= 1  (jsr/signum #money[1 EUR])))
    (is (= 0  (jsr/signum #money[0 EUR])))
    (is (= -1 (jsr/signum (jsr/negate #money[1 EUR]))))
    (is (= #money[1 EUR] (jsr/abs (jsr/negate #money[1 EUR]))))
    (is (= #money[-1 EUR] (jsr/negate #money[1 EUR])))
    (is (= #money[1 EUR] (jsr/plus #money[1 EUR])))
    (let [m (jsr/strip-trailing-zeros #money[1 EUR])]
      (is (amount= m 1M))
      (is (= 0 (.scale ^BigDecimal (money/amount m))))
      (is (amount= (jsr/stripTrailingZeros #money[1 EUR]) 1M))))

  (testing "predicates"
    (is (true?  (jsr/is-zero #money[0 EUR])))
    (is (true?  (jsr/isZero #money[0 EUR])))
    (is (true?  (jsr/is-positive #money[1 EUR])))
    (is (true?  (jsr/isPositive #money[1 EUR])))
    (is (true?  (jsr/is-negative (jsr/negate #money[1 EUR]))))
    (is (true?  (jsr/isNegative (jsr/negate #money[1 EUR]))))
    (is (true?  (jsr/is-positive-or-zero #money[0 EUR])))
    (is (true?  (jsr/isPositiveOrZero #money[0 EUR])))
    (is (true?  (jsr/is-negative-or-zero #money[0 EUR])))
    (is (true?  (jsr/isNegativeOrZero #money[0 EUR]))))

  (testing "comparison"
    (let [a #money[1 EUR]
          b (money/scale a 3)]
      (is (= 0 (jsr/compare-to a b)))
      (is (= 0 (jsr/compareTo a b)))
      (is (true? (jsr/is-equal-to a b)))
      (is (true? (jsr/isEqualTo a b)))
      (is (true? (jsr/is-greater-than #money[2 EUR] #money[1 EUR])))
      (is (true? (jsr/isGreaterThan #money[2 EUR] #money[1 EUR])))
      (is (true? (jsr/is-greater-than-or-equal-to #money[2 EUR] #money[2 EUR])))
      (is (true? (jsr/isGreaterThanOrEqualTo #money[2 EUR] #money[2 EUR])))
      (is (true? (jsr/is-less-than #money[1 EUR] #money[2 EUR])))
      (is (true? (jsr/isLessThan #money[1 EUR] #money[2 EUR])))
      (is (true? (jsr/is-less-than-or-equal-to #money[2 EUR] #money[2 EUR])))
      (is (true? (jsr/isLessThanOrEqualTo #money[2 EUR] #money[2 EUR])))))

  (testing "operator/query helpers"
    (let [m  #money[1 EUR]
          op (fn [x] (jsr/add x #money[2 EUR]))
          q  (fn [x] (jsr/getCurrencyCode x))]
      (is (= #money[3 EUR] (jsr/with m op)))
      (is (= "EUR" (jsr/query m q))))))
