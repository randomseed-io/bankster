(ns

    ^{:doc    "Additional coverage tests for money namespace."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.money-coverage-test

  (:require [clojure.test                     :refer [deftest testing is]]
            [clojure.string                   :as str]
            [io.randomseed.bankster.money     :as m]
            [io.randomseed.bankster.currency  :as c]
            [io.randomseed.bankster.registry  :as registry]
            [io.randomseed.bankster.scale     :as scale]
            [io.randomseed.bankster.serializers.json :as sj]
            [io.randomseed.bankster.util.fs   :as fs])

  (:import (io.randomseed.bankster Money Currency Registry)
           (java.io StringReader)
           (java.math BigDecimal BigInteger RoundingMode)
           (java.text DecimalFormat)))

(defn ^:private m-eval [form]
  (binding [*ns* (the-ns 'io.randomseed.bankster.money)]
    (clojure.core/eval form)))

(deftest monetary-scale-and-parsing-helpers
  (testing "monetary-scale private helpers"
    (let [bd (BigDecimal. "1.23")]
      (is (identical? bd (#'m/monetary-scale bd)))
      (is (= (BigDecimal. "1.23") (#'m/monetary-scale 1.23)))
      (is (identical? bd (#'m/monetary-scale bd c/auto-scaled)))
      (is (identical? bd (#'m/monetary-scale bd c/auto-scaled RoundingMode/HALF_UP)))
      (is (instance? BigDecimal (#'m/monetary-scale 1 c/auto-scaled)))
      (is (instance? BigDecimal (#'m/monetary-scale 1 c/auto-scaled RoundingMode/HALF_UP)))
      (is (= "1.2" (.toPlainString ^BigDecimal (#'m/monetary-scale (BigDecimal. "1.23") 1 RoundingMode/DOWN))))))
  (testing "currency+amount and parsing tokens"
    (let [ca #'m/currency+amount]
      (is (= [nil nil] (ca "")))
      (is (= [nil nil] (ca "   ")))
      (is (= ["EUR" "100"] (ca "EUR100")))
      (is (= ["EUR" "100"] (ca "EUR 100")))
      (is (= ["PLN" "-100"] (ca "-100 PLN")))
      (is (= ["PLN" "1000.00"] (ca "1_000.00 PLN")))
      (is (= ["crypto/ETH" "1.0"] (ca "crypto/ETH1.0")))))
  (testing "split-currency-first and remainder->token branches"
    (let [rt  #'m/remainder->token
          scf #'m/split-currency-first]
      (is (= "PLN" (rt " P L N" 0)))
      (is (= ["PLN" "-12.30"] (scf "PLN -12.30")))
      (is (= ["PLN-1" "2.30"] (scf "PLN-12.30")))))
  (testing "mk-bigdec handles numbers and non-numbers"
    (is (= 1M (#'m/mk-bigdec 1)))
    (is (= "x" (#'m/mk-bigdec "x")))))

(deftest parse-int-branches
  (testing "parse-int falls back to currency+amount when unit cannot resolve"
    (with-redefs [c/unit (fn [_] nil)]
      (binding [c/*default* nil]
        (is (thrown? clojure.lang.ExceptionInfo (#'m/parse-int identity "12.30")))
        (is (thrown? clojure.lang.ExceptionInfo (#'m/parse-int identity "PLN 12.30")))))))

(deftest parse-and-of-macros
  (binding [c/*default* (c/of-id :PLN)]
    (testing "parse arities and error paths"
      (is (instance? Money (m/parse :PLN 12.30M)))
      (is (instance? Money (m/parse :PLN 12.30M RoundingMode/HALF_UP)))
      (is (instance? Money (m/parse :PLN)))
      (is (instance? Money (m/parse (m/of :PLN 1M))))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Cannot create money amount without a valid currency"
           (m/parse nil 1M))))
    (testing "parse when no default currency is available"
      (binding [c/*default* nil]
        (is (thrown? clojure.lang.ExceptionInfo (m/parse "12.30")))))
    (testing "parse-major/parse-minor"
      (is (instance? Money (m/parse-major :PLN 12.34M)))
      (is (instance? Money (m/parse-minor :PLN 12.34M))))
    (testing "of/of-major/of-minor macros cover common forms"
      (is (instance? Money (m/of)))
      (is (instance? Money (m/of 12.30M)))
      (is (instance? Money (m/of :PLN 12.30M)))
      (is (instance? Money (m/of 12.30M :PLN)))
      (is (instance? Money (m/of "PLN" 12.30M)))
      (is (instance? Money (m/of "PLN")))
      (is (instance? Money (m/of "12.30 PLN")))
      (is (instance? Money (m/of "PLN 12.30")))
      (is (instance? Money (m/of "12.30 PLN" :HALF_UP)))
      (is (instance? Money (m/of "12.30 PLN" RoundingMode/HALF_UP)))
      (is (instance? Money (m/of :PLN 12.30M :HALF_UP)))
      (is (instance? Money (m/of-major :PLN 12.30M)))
      (is (instance? Money (m/of-major)))
      (is (instance? Money (m/of-major :PLN)))
      (is (instance? Money (m/of-minor :PLN 1230)))
      (is (instance? Money (m/of-minor)))
      (is (instance? Money (m/of-minor :PLN)))
      (is (instance? Money (m/of {:id :PLN :scale 2})))
      (is (instance? Money (m/of (m/of :PLN 1M))))
      (is (instance? Money (m/of (m/of :PLN 1M) 2)))
      (let [money (m/of :PLN 1M)]
        (is (instance? Money (m/of money)))))))

(deftest accountable-protocol-coverage
  (let [money (m/of :PLN 12.30M)
        cur   (c/of-id :PLN)]
    (testing "value on Money/Currency/Symbol/Keyword/String/Number"
      (is (instance? Money (m/value money)))
      (is (instance? Money (m/value money 10M)))
      (is (instance? Money (m/value money 10M RoundingMode/HALF_UP)))
      (is (instance? Money (m/value cur)))
      (is (instance? Money (m/value cur 1M)))
      (is (instance? Money (m/value cur 1M RoundingMode/HALF_UP)))
      (is (instance? Money (m/value 'PLN)))
      (is (instance? Money (m/value :PLN)))
      (is (instance? Money (m/value "PLN")))
      (is (instance? Money (m/value 'PLN 1M)))
      (is (instance? Money (m/value :PLN 1M)))
      (is (instance? Money (m/value "PLN" 1M)))
      (binding [c/*default* (c/of-id :PLN)]
        (is (instance? Money (m/value 1M))))
      (is (instance? Money (m/value 1M :PLN)))
      (is (nil? (m/value nil)))
      (is (instance? Money (m/value nil (m/of :PLN 1M))))
      (is (nil? (m/value nil :PLN RoundingMode/HALF_UP)))
      (is (instance? Money (m/value [ :PLN 1M ])))
      (is (instance? Money (m/value [ :PLN 1M ] RoundingMode/HALF_UP)))
      (is (instance? Money (m/value [ :PLN ] 1M RoundingMode/HALF_UP))))
    (testing "cast on Money/Currency/Symbol/Keyword/String/Number"
      (is (instance? Money (m/cast money)))
      (is (instance? Money (m/cast money :PLN)))
      (is (instance? Money (m/cast money :PLN RoundingMode/HALF_UP)))
      (is (instance? Money (m/cast cur money)))
      (is (instance? Money (m/cast cur money RoundingMode/HALF_UP)))
      (is (instance? Money (m/cast 'PLN)))
      (is (instance? Money (m/cast :PLN)))
      (is (instance? Money (m/cast "PLN")))
      (is (instance? Money (m/cast 'PLN money)))
      (is (instance? Money (m/cast :PLN money)))
      (is (instance? Money (m/cast "PLN" money)))
      (is (instance? Money (m/cast 985 money)))
      (is (nil? (m/cast nil)))
      (is (nil? (m/cast nil nil)))
      (is (nil? (m/cast nil nil nil)))
      (is (instance? Money (m/cast [ :PLN money ])))
      (is (instance? Money (m/cast [ :PLN money ] RoundingMode/HALF_UP)))
      (is (instance? Money (m/cast [ :PLN ] money RoundingMode/HALF_UP))))
    (testing "object fallback for value/cast"
      (let [dummy (Object.)]
        (with-redefs [m/parse (fn [& _] (m/of :PLN 1M))]
          (is (instance? Money (m/value dummy :PLN)))
          (is (instance? Money (m/value dummy :PLN RoundingMode/HALF_UP)))
          (is (instance? Money (m/cast dummy)))
          (is (instance? Money (m/cast dummy :PLN)))
          (is (instance? Money (m/cast dummy :PLN RoundingMode/HALF_UP))))))))

(deftest monetary-protocol-coverage
  (let [money (m/of :PLN 12.30M)
        reg   (registry/state)]
    (is (= :PLN (c/to-id money)))
    (is (= :PLN (c/to-code money)))
    (is (= "PLN" (c/to-id-str money)))
    (is (= "PLN" (c/to-code-str money)))
    (is (integer? (c/to-numeric-id money)))
    (is (instance? Currency (c/to-currency money)))
    (is (= {:currency :PLN :amount 12.30M} (c/to-map money)))
    (is (true? (c/definitive? money)))
    (is (instance? Currency (c/resolve money)))
    (is (instance? Currency (c/resolve money reg)))
    (is (set? (c/resolve-all money)))
    (is (set? (c/resolve-all money reg)))
    (is (instance? Currency (c/of-id money)))
    (is (instance? Currency (c/of-id money reg)))
    (is (instance? Currency (c/unit money)))
    (is (instance? Currency (c/unit money reg)))
    (is (= :PLN (c/id money)))
    (is (= :PLN (c/id money reg)))
    (is (boolean? (c/defined? money)))
    (is (boolean? (c/defined? money reg)))
    (is (boolean? (c/present? money)))
    (is (boolean? (c/present? money reg)))))

(deftest major-minor-and-registry
  (let [money (m/of :PLN 12.34M)]
    (testing "major/minor helpers"
      (is (instance? Money (m/major-value money)))
      (is (instance? Money (m/minor-value money)))
      (is (instance? Money (m/major-value :PLN 12.34M)))
      (is (instance? Money (m/minor-value :PLN 12.34M)))
      (is (instance? Money (m/major-value :PLN 12.34M RoundingMode/DOWN)))
      (is (instance? Money (m/minor-value :PLN 12.34M RoundingMode/DOWN))))
    (testing "of-registry arities"
      (let [reg (registry/state)]
        (is (instance? Money (m/of-registry money)))
        (is (instance? Money (m/of-registry reg money)))
        (is (instance? Money (m/of-registry reg money RoundingMode/HALF_UP)))))))

(deftest amount-operations
  (let [money (m/of :PLN 12.30M)]
    (testing "on-amount arities and alias apply"
      (is (instance? Money (m/on-amount money inc)))
      (is (instance? Money (m/on-amount money + 1M)))
      (is (instance? Money (m/on-amount money + 1M 1M)))
      (is (instance? Money (m/on-amount money + 1M 1M 1M)))
      (is (instance? Money (m/apply money + 1M 1M))))
    (testing "set-amount and strip"
      (is (instance? Money (m/set-amount money 10.01M)))
      (is (instance? Money (m/set-amount money 10.01M RoundingMode/HALF_UP)))
      (is (instance? Money (m/strip money))))
    (testing "amount/currency/stripped-amount/unparse"
      (is (instance? BigDecimal (m/amount money)))
      (is (instance? BigDecimal (m/amount :PLN 12.30M)))
      (is (instance? BigDecimal (m/amount :PLN 12.30M RoundingMode/HALF_UP)))
      (is (= :PLN (c/id (m/currency money))))
      (is (instance? Currency (m/currency money 1M)))
      (is (instance? Currency (m/currency money 1M RoundingMode/HALF_UP)))
      (is (instance? BigDecimal (m/stripped-amount money)))
      (is (instance? BigDecimal (m/stripped-amount money 1M)))
      (is (instance? BigDecimal (m/stripped-amount money 1M RoundingMode/HALF_UP)))
      (is (vector? (m/unparse money))))))

(deftest unparse-arities
  (let [money (m/of :PLN 1.20M)]
    (is (vector? (m/unparse money 1M)))
    (is (vector? (m/unparse money 1M RoundingMode/HALF_UP)))))

(deftest comparisons-and-predicates
  (let [a (m/of :PLN 10M)
        b (m/of :PLN 12M)
        c (m/of :PLN 10.00M)
        d (m/of :PLN 9M)]
    (testing "compare and compare-amounts with nil"
      (is (= 0 (m/compare-amounts nil)))
      (is (= -1 (m/compare-amounts nil a)))
      (is (= 1 (m/compare-amounts a nil))))
    (testing "eq/ne and amount-based variants"
      (is (true? (m/eq? a)))
      (is (false? (m/eq? a b)))
      (is (true? (m/eq? a c)))
      (is (true? (m/eq? a c a)))
      (is (true? (m/eq-am? a c)))
      (is (false? (m/ne? a c)))
      (is (true? (m/ne-am? a b))))
    (testing "ordering predicates"
      (is (true? (m/gt? b a)))
      (is (true? (m/ge? b a)))
      (is (true? (m/lt? a b)))
      (is (true? (m/le? a b)))
      (is (true? (m/gt? b a d))))
    (testing "zero/neg/pos predicates"
      (is (true? (m/is-zero? (m/of :PLN 0M))))
      (is (true? (m/is-neg? (m/of :PLN -1M))))
      (is (true? (m/is-pos? (m/of :PLN 1M))))
      (is (true? (m/is-neg-or-zero? (m/of :PLN -1M))))
      (is (true? (m/is-pos-or-zero? (m/of :PLN 0M)))))))

(deftest compare-and-eq-am-branches
  (let [a    (m/of :PLN 10.00M)
        b    (m/of :PLN 10.000M)
        usd  (m/of :USD 10M)]
    (testing "compare error branches"
      (is (= 0 (m/compare nil nil)))
      (is (= -1 (m/compare nil a)))
      (is (thrown? clojure.lang.ExceptionInfo (m/compare a usd)))
      (is (= 0 (m/compare a b))))
    (testing "same-currency-ids? and eq-am? scale branches"
      (is (true? (m/same-currency-ids? a b)))
      (is (false? (m/eq-am? a usd)))
      (is (true? (m/eq-am? a b))))))

(deftest scale-round-convert-branches
  (let [a        (m/of :PLN 12.30M)
        usd      (m/of :USD 1M)
        auto-cur (c/new-currency :AUTO nil c/auto-scaled)]
    (testing "scale arities"
      (is (= 2 (m/scale a)))
      (is (instance? Money (m/scale a 2)))
      (is (instance? Money (m/scale a 2 RoundingMode/HALF_UP))))
    (testing "round branches"
      (is (identical? a (m/round a 5)))
      (is (instance? Money (m/round a 1 RoundingMode/HALF_UP))))
    (testing "round-to branches"
      (is (identical? a (m/round-to a nil)))
      (is (identical? a (m/round-to a 0)))
      (is (thrown? NumberFormatException (m/round-to a "x")))
      (is (thrown? clojure.lang.ExceptionInfo (m/round-to a usd)))
      (is (instance? Money (m/round-to a (m/of :PLN 0.05M) RoundingMode/HALF_UP))))
    (testing "convert branches"
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/convert a (m/of :PLN 1.00M) RoundingMode/HALF_UP)))
        (is (instance? Money (m/convert a :PLN 1.00M)))
        (is (instance? Money (m/convert a :PLN 1.00M RoundingMode/HALF_UP))))
      (binding [scale/*rounding-mode* nil]
        (is (instance? Money (m/convert a :PLN 1.00M)))
        (is (instance? Money (m/convert a auto-cur 2M)))))))

(deftest round-non-auto-branch
  (let [cur      (c/new-currency :TST nil 2)
        money    (Money. cur (BigDecimal. "12.30"))
        money-hi (Money. cur (BigDecimal. "12.345"))]
    (is (instance? Money (m/round money 1 RoundingMode/HALF_UP)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/round money-hi 2 scale/ROUND_UNNECESSARY)))))

(deftest scaling-and-arithmetic
  (let [a (m/of :PLN 10.00M)
        b (m/of :PLN 2.50M)]
    (testing "scale/rescale"
      (is (instance? Money (m/scale a 2)))
      (is (instance? Money (m/rescale a 3 RoundingMode/HALF_UP))))
    (testing "add/sub/mul/div basics"
      (is (instance? Money (m/add a b)))
      (is (instance? Money (m/sub a b)))
      (is (instance? Money (m/mul a 2)))
      (is (instance? Money (m/div a 2))))
    (testing "min/max/convert"
      (is (instance? Money (m/min-amount a b)))
      (is (instance? Money (m/max-amount a b)))
      (is (thrown? clojure.lang.ExceptionInfo (m/min-amount a (m/of :USD 1M))))
      (is (thrown? clojure.lang.ExceptionInfo (m/max-amount a (m/of :USD 1M))))
      (is (instance? Money (m/convert a (m/of :PLN 1M))))
      (is (instance? Money (m/convert a :PLN 1M))))
    (testing "remainders and sign helpers"
      (is (instance? BigDecimal (m/rem a b)))
      (is (instance? Money (m/rem a 2)))
      (is (instance? Money (m/neg a)))
      (is (instance? Money (m/pos a)))
      (is (instance? Money (m/abs (m/of :PLN -1M)))))
    (testing "rounding helpers"
      (is (instance? Money (m/round a 1)))
      (is (instance? Money (m/round-to a 1))))))

(deftest add-sub-branch-coverage
  (let [a   (m/of :PLN 10M)
        b   (m/of :PLN 2M)
        usd (m/of :USD 1M)]
    (is (= 0M (m/add)))
    (is (instance? Money (m/sub a)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add 1)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add a 1)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add a usd)))
    (is (instance? Money (m/add a b b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add a b 1)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add a b usd)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub 1)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub a 1)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub a usd)))
    (is (instance? Money (m/sub a b b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub a b 1)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub a b usd)))))

(deftest mul-div-rem-branch-coverage
  (let [auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (m/value auto-cur 1.234M)
        a          (m/of :PLN 10.00M)
        b          (m/of :PLN 2.00M)
        usd        (m/of :USD 1.00M)]
    (testing "mul branches"
      (is (= 1M (m/mul)))
      (is (= 2M (m/mul 2M)))
      (is (instance? BigDecimal (m/mul 2 3)))
      (is (instance? Money (m/mul a 2)))
      (is (instance? Money (m/mul 2 a)))
      (is (instance? Money (m/mul auto-money 2)))
      (is (instance? Money (m/mul 2 auto-money)))
      (is (thrown? clojure.lang.ExceptionInfo (m/mul a b)))
      (is (thrown? clojure.lang.ExceptionInfo (m/mul 1M a b)))
      (binding [scale/*each* true]
        (is (instance? Money (m/mul a 2 3))))
      (is (thrown? clojure.lang.ExceptionInfo (m/mul-scaled a b)))
      (is (instance? BigDecimal (m/mul-scaled 2 a 3)))
      (is (instance? BigDecimal (m/mul-scaled 2 auto-money 3))))
    (testing "div branches"
      (is (instance? BigDecimal (m/div a b)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div a usd)))
      (is (instance? Money (m/div a 2)))
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/div a 2.5M))))
      (is (thrown? clojure.lang.ExceptionInfo (m/div 2 a)))
      (is (instance? BigDecimal (m/div 10 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div a)))
      (is (instance? BigDecimal (m/div 2)))
      (binding [scale/*each* true]
        (is (instance? Money (m/div a 2 2))))
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? BigDecimal (m/div-scaled 10 3 2))))
      (binding [scale/*rounding-mode* nil]
        (is (instance? BigDecimal (m/div-scaled 10 2 5))))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 10 a)))
      (is (instance? Money (m/div-scaled a 2 2)))
      (is (instance? BigDecimal (m/div-scaled a b 2)))
      (is (instance? BigDecimal (m/div-scaled a usd 2))))
    (testing "div-core-fn error branches"
      (is (thrown? clojure.lang.ExceptionInfo (#'m/div-core-fn 1M (m/of :PLN 1M))))
      (is (thrown? clojure.lang.ExceptionInfo (#'m/div-core-fn 1M 3))))
    (testing "rem branches"
      (is (instance? BigDecimal (m/rem a b)))
      (is (instance? Money (m/rem a 2)))
      (is (instance? Money (m/rem auto-money 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/rem a usd)))
      (is (thrown? clojure.lang.ExceptionInfo (m/rem 2 a)))
      (is (instance? BigDecimal (m/rem 10 3 nil)))))) 

(deftest rem-core-macro-branches
  (testing "rem-core macro expansion paths"
    (let [exp2 (@#'m/rem-core nil nil 1M 2M)
          exp3 (@#'m/rem-core nil nil 1M 2M RoundingMode/HALF_UP)
          exp4 (@#'m/rem-core nil nil 1M 2M 2 RoundingMode/HALF_UP)
          exp5 (@#'m/rem-core nil nil 1.234M 1M 1 'java.math.RoundingMode/HALF_UP)]
      (is (seq? exp2))
      (is (seq? exp3))
      (is (seq? exp4))
      (is (instance? BigDecimal (m-eval exp5))))))

(deftest major-minor-conversions
  (let [a (m/of :PLN 12.34M)]
    (testing "major/minor numeric conversions"
      (is (instance? BigDecimal (m/major a)))
      (is (instance? BigDecimal (m/minor a)))
      (is (integer? (m/major->long a)))
      (is (integer? (m/major->int a)))
      (is (integer? (m/minor->long a)))
      (is (integer? (m/minor->int a)))
      (is (vector? (m/major-minor a)))
      (is (every? integer? (m/major-minor->int a)))
      (is (every? integer? (m/major-minor->long a))))
    (testing "symbol and numeric conversions"
      (is (symbol? (m/->symbol a)))
      (is (symbol? (m/->clojure-symbol a)))
      (is (float? (#'m/->float a)))
      (is (double? (#'m/->double a)))
      (is (double? (#'m/->double a 2)))
      (is (double? (#'m/->double a 1 RoundingMode/HALF_UP)))
      (is (number? (#'m/->float a 2)))
      (is (number? (#'m/->float a 1 RoundingMode/HALF_UP))))))

(deftest major-minor-adjustments
  (let [a (m/of :PLN 12.34M)]
    (is (instance? Money (m/add-major a 1)))
    (is (instance? Money (m/sub-major a 1)))
    (is (instance? Money (m/inc-major a)))
    (is (instance? Money (m/dec-major a)))
    (is (instance? Money (m/add-minor a 1)))
    (is (instance? Money (m/sub-minor a 1)))
    (is (instance? Money (m/inc-minor a)))
    (is (instance? Money (m/dec-minor a)))))

(deftest allocation-and-distribution
  (let [a (m/of :PLN 10M)]
    (is (vector? (m/allocate a [1 1 1])))
    (is (vector? (m/allocate a [1])))
    (is (thrown? clojure.lang.ExceptionInfo (m/allocate a [0])))
    (is (thrown? clojure.lang.ExceptionInfo (m/allocate a [1.5])))
    (is (vector? (m/distribute a 3)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Ratio must be integer-like"
         (m/distribute a [1 2 3])))
    (is (thrown? clojure.lang.ExceptionInfo (m/distribute a 0)))
    (is (thrown? clojure.lang.ExceptionInfo (m/distribute a 1.5)))))

(deftest macros-and-readers
  (testing "with-rounding/with-rescaling/with-currency"
    (m/with-rounding :HALF_UP
      (is (instance? Money (m/of :PLN 1.234M))))
    (m/with-rescaling :HALF_UP
      (is (instance? Money (m/of :PLN 1.2345M))))
    (m/with-currency :PLN
      (is (instance? Money (m/of 1.23M)))))
  (testing "literals and readers"
    (is (seq? (#'m/lit-parse :PLN nil)))
    (is (seq? (#'m/lit-parse :PLN 1M nil)))
    (is (seq? (#'m/lit-parse :PLN 1M :HALF_UP)))
    (is (seq? (m/code-literal nil)))
    (is (seq? (m/code-literal :PLN)))
    (is (seq? (m/code-literal {:currency :PLN :amount 1M :rounding :HALF_UP})))
    (is (seq? (#'m/lit-parse :PLN 1M)))
    (is (seq? (m/code-literal '[1M PLN])))
    (is (instance? Money (m/data-literal [1M 'PLN])))
    (is (instance? Money (m/data-literal {:currency :PLN :amount 1M})))
    (is (map? (m/readers)))
    (is (string? (m/format (m/of :PLN 1M) java.util.Locale/US)))
    (let [fmt (DecimalFormat.)]
      (is (string? (m/format-with fmt (m/of :PLN 1M))))))
  (testing "defliteral and namespace literal helpers"
    (let [orig @#'clojure.core/*data-readers*]
      (is (some? (m/defliteral :PLN)))
      (alter-var-root #'clojure.core/*data-readers* (constantly orig)))
    (with-redefs [c/unit (fn [_] nil)]
      (is (nil? (m/defliteral :PLN))))
    (is (seq? (#'m/ns-code-literal "crypto" :ETH)))
    (is (instance? Money (#'m/ns-data-literal "crypto" [:ETH 1M])))
    (with-redefs [m/data-literal (fn [arg] arg)]
      (is (= [:crypto/ETH 1M nil] (#'m/ns-data-literal "crypto" [1M :ETH]))))))

(deftest map-and-json-helpers
  (let [money (m/of :PLN 12.30M)]
    (testing "to-map/of-map roundtrip"
      (is (= :PLN (:currency (m/to-map money))))
      (is (instance? Money (m/of-map {:currency :PLN :amount "12.30"})))
      (is (instance? Money (m/of-map {:cur :PLN :amount 12.30M})))
      (is (instance? Money (m/of-map {"currency" "PLN" "amount" "12.30"})))
      (is (instance? Money (m/of-map {:currency :PLN :amount 12.30M :rounding-mode :HALF_UP})))
      (is (nil? (m/of-map nil)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (m/of-map {:currency :PLN :amount 1M :rounding-mode "BOGUS"})))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"must be a map"
           (m/of-map 12)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"missing :currency"
           (m/of-map {:amount 1M})))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"missing :amount"
           (m/of-map {:currency :PLN}))))
    (testing "to-json/from-json helpers"
      (is (map? (m/to-json-map money)))
      (is (string? (m/to-json-string money)))
      (is (nil? (m/to-json-map nil)))
      (is (nil? (m/to-json-string nil)))
      (is (map? (m/to-json-map money {:code-only? true})))
      (is (string? (m/to-json-string money {:code-only? true})))
      (is (instance? Money (m/from-json-map {:currency "PLN" :amount "12.30"})))
      (is (instance? Money (m/from-json-map {:currency "PLN" :amount "12.30"}
                                            {:registry (registry/get)
                                             :rounding-mode "HALF_UP"})))
      (is (instance? Money (m/from-json-string "12.30 PLN")))
      (is (instance? Money (m/from-json-string "12.30 PLN"
                                               {:registry (registry/get)
                                                :rounding-mode :HALF_UP}))))))

(deftest format-and-printing-branches
  (let [money   (m/of :PLN 12.30M)
        ns-cur  (c/new-currency :crypto/ETH nil 8)
        ns-money (Money. ns-cur (BigDecimal. "1.0"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (string? (m/format money java.util.Locale/US))))
    (binding [scale/*rounding-mode* scale/ROUND_UNNECESSARY]
      (is (string? (m/format money java.util.Locale/US))))
    (binding [scale/*rounding-mode* nil]
      (is (string? (m/format money java.util.Locale/US))))
    (binding [scale/*rounding-mode* nil]
      (is (string? (m/format money java.util.Locale/US {:grouping true}))))
    (is (string? (m/format money java.util.Locale/US {:rounding-mode RoundingMode/HALF_UP
                                                      :grouping true})))
    (is (string? (m/format money java.util.Locale/US {:rounding-mode scale/ROUND_UNNECESSARY})))
    (is (str/includes? (pr-str ns-money) "#money/crypto"))
    (is (str/includes? (pr-str money) "#money["))))

(deftest parse-rounding-and-of-major-minor-coverage
  (binding [c/*default* (c/of-id :PLN)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/parse nil 1M RoundingMode/HALF_UP)))
    (is (instance? Money (m/parse-major 12.34M)))
    (is (instance? Money (m/parse-major :PLN 12.34M RoundingMode/HALF_UP)))
    (is (instance? Money (m/parse-minor 12.34M)))
    (is (instance? Money (m/parse-minor :PLN 12.34M RoundingMode/HALF_UP)))
    (is (instance? Money (m/of-major :PLN 12.34M RoundingMode/HALF_UP)))
    (is (instance? Money (m/of-minor :PLN 1234 RoundingMode/HALF_UP)))))

(deftest of-gen-branch-coverage
  (is (seq? (m/of-gen 'parse "PLN")))
  (is (seq? (m/of-gen 'parse "PLN 1.00" (Object.))))
  (is (seq? (m/of-gen 'parse (Object.) "PLN" :HALF_UP))))

(deftest accountable-missing-arities-coverage
  (let [money (m/of :PLN 1M)
        cur   (c/of-id :PLN)
        dummy (Object.)]
    (is (instance? Money (m/cast cur)))
    (is (instance? Money (m/cast 'PLN money RoundingMode/HALF_UP)))
    (is (instance? Money (m/cast :PLN money RoundingMode/HALF_UP)))
    (is (instance? Money (m/cast "PLN" money RoundingMode/HALF_UP)))
    (is (instance? Money (m/value 'PLN 1M RoundingMode/HALF_UP)))
    (is (instance? Money (m/value "PLN" 1M RoundingMode/HALF_UP)))
    (binding [c/*default* (c/of-id :PLN)]
      (is (instance? Money (m/cast 1M))))
    (is (instance? Money (m/value 1M :PLN RoundingMode/HALF_UP)))
    (is (instance? Money (m/cast 985 money RoundingMode/HALF_UP)))
    (with-redefs [m/parse (fn [& _] (m/of :PLN 1M))]
      (is (instance? Money (m/value dummy))))))

(deftest scalable-protocol-branch-coverage
  (let [cur      (c/new-currency :TST nil 2)
        money    (Money. cur (BigDecimal. "1.23"))
        money-hi (Money. cur (BigDecimal. "1.234"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (scale/apply money-hi)))
      (is (instance? Money (scale/apply money 3)))
      (is (instance? Money (scale/apply money 3 RoundingMode/HALF_UP)))
      (is (instance? BigDecimal (scale/amount money 3)))
      (is (instance? BigDecimal (scale/amount money 3 RoundingMode/HALF_UP))))
    (binding [scale/*rounding-mode* nil]
      (is (thrown? clojure.lang.ExceptionInfo (scale/apply money-hi))))
    (binding [scale/*rounding-mode* scale/ROUND_UNNECESSARY]
      (is (thrown? clojure.lang.ExceptionInfo (scale/apply money-hi 2)))
      (is (thrown? clojure.lang.ExceptionInfo (scale/amount money-hi 2))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (scale/apply money-hi 2 scale/ROUND_UNNECESSARY)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (scale/amount money-hi 2 scale/ROUND_UNNECESSARY)))))

(deftest compare-additional-branches
  (let [cur (c/of-id :PLN)
        a   (Money. cur (BigDecimal. "10.00"))
        b   (Money. cur (BigDecimal. "10.000"))]
    (is (= 0 (m/compare-amounts nil nil)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/compare-amounts (m/of :PLN 1M) (m/of :USD 1M))))
    (is (= 0 (m/compare a)))
    (is (= 1 (m/compare a nil)))
    (is (true? (m/same-currencies? a b)))
    (is (true? (m/money? a)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare a b)))))

(deftest eq-am-and-recursion-branches
  (let [cur (c/of-id :PLN)
        m1  (Money. cur (BigDecimal. "1.0"))
        m2  (Money. cur (BigDecimal. "1.00"))
        m3  (Money. cur (BigDecimal. "1.000"))]
    (is (true? (m/eq-am? m1 m2)))
    (is (true? (m/eq-am? m2 m1)))
    (is (true? (m/eq-am? m1 m2 m3 m1)))
    (is (true? (m/eq? m2 m2 m2 m2)))))

(deftest predicate-recursion-branches
  (let [a (m/of :PLN 4M)
        b (m/of :PLN 3M)
        c (m/of :PLN 2M)
        d (m/of :PLN 1M)]
    (is (true? (m/gt? a b c d)))
    (is (true? (m/ge? a a b b)))
    (is (true? (m/lt? d c b a)))
    (is (true? (m/le? d d c c)))))

(deftest unary-and-varargs-predicate-branches
  (let [a (m/of :PLN 1M)
        b (m/of :PLN 2M)
        c (m/of :PLN 3M)]
    (is (true? (m/eq-am? a)))
    (is (false? (m/ne? a)))
    (is (false? (m/ne-am? a)))
    (is (true? (m/gt? b)))
    (is (true? (m/ge? b)))
    (is (true? (m/lt? b)))
    (is (true? (m/le? b)))
    (is (true? (m/ne? a b c)))
    (is (true? (m/ne-am? a b c)))))

(deftest rescale-and-add-sub-varargs-branches
  (let [a (m/of :PLN 10M)]
    (is (instance? Money (m/rescale a)))
    (is (instance? Money (m/rescale a 3)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add 1 2 3)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub 1 2 3)))))

(deftest mul-and-mul-scaled-additional-branches
  (let [cur        (c/new-currency :TST nil 2)
        a          (Money. cur (BigDecimal. "2.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (m/value auto-cur 1.2M)]
    (is (instance? Money (m/mul a 2 3)))
    (is (instance? Money (m/mul auto-money 2 3)))
    (is (instance? BigDecimal (m/mul 2 3 4)))
    (is (instance? Money (m/mul-scaled auto-money 2 3)))
    (is (instance? Money (m/mul-scaled 2 3 auto-money)))
    (binding [scale/*rounding-mode* nil]
      (is (thrown? clojure.lang.ExceptionInfo (m/mul a 0.333M 2))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/mul-scaled (Money. cur (BigDecimal. "1.00"))
                               2
                               (Money. cur (BigDecimal. "1.00")))))))

(deftest div-scaled-additional-branches
  (let [auto-cur    (c/new-currency :AUTO nil c/auto-scaled)
        auto-money  (m/value auto-cur 10M)
        auto-money2 (m/value auto-cur 2M)
        cur         (c/new-currency :TST nil 2)
        m1          (Money. cur (BigDecimal. "10.00"))
        m2          (Money. cur (BigDecimal. "2.00"))
        m3          (Money. cur (BigDecimal. "1.00"))
        usd         (m/of :USD 2M)]
    (is (instance? BigDecimal (m/div-scaled 2)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/div-scaled 10 (m/of :PLN 1M) 2)))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (thrown? clojure.lang.ExceptionInfo
                   (m/div-scaled 10 2 (m/of :PLN 1M)))))
    (is (instance? BigDecimal (m/div-scaled auto-money 2 auto-money2 2)))
    (is (instance? Money (m/div-scaled auto-money 2 2)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/div-scaled auto-money 2 auto-money2 (m/value auto-cur 1M))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/div-scaled auto-money 2 (m/of :USD 1M))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/div-scaled m1 2 usd)))
    (is (instance? BigDecimal (m/div-scaled m1 2 m2 2)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/div-scaled m1 2 m2 m3)))))

(deftest div-additional-branches
  (let [auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (m/value auto-cur 10M)
        cur        (c/new-currency :TST nil 2)
        m1         (Money. cur (BigDecimal. "10.00"))
        m2         (Money. cur (BigDecimal. "2.00"))
        m3         (Money. cur (BigDecimal. "1.00"))
        usd        (m/of :USD 1M)]
    (is (instance? Money (m/div auto-money 2)))
    (with-redefs [scale/ROUND_UNNECESSARY nil]
      (binding [scale/*rounding-mode* nil]
        (is (thrown? clojure.lang.ExceptionInfo (m/div 1 3)))))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 3)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 m2 m3)))
    (is (instance? BigDecimal (m/div m1 2 m2)))
    (is (instance? BigDecimal (m/div m1 2 m2 2)))
    (is (instance? Money (m/div m1 2 2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 2 m2 m3)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 2 usd)))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? BigDecimal (m/div 10 2 2))))
    (binding [scale/*rounding-mode* nil]
      (is (instance? BigDecimal (m/div 8 2 2))))
    (is (instance? Money (m/div auto-money 2 2)))
    (is (instance? Money (m/min-amount m1)))
    (is (instance? Money (m/max-amount m1)))))

(deftest convert-and-rem-rounding-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "1.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "1.23"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (m/convert money cur 0.333333M))))
    (binding [scale/*rounding-mode* nil]
      (is (thrown? clojure.lang.ExceptionInfo
                   (m/convert money cur 0.333333M))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/convert money cur 0.333333M scale/ROUND_UNNECESSARY)))
    (is (instance? Money (m/convert auto-money auto-cur 2M RoundingMode/HALF_UP)))
    (is (instance? Money (m/convert money cur 0.333333M RoundingMode/HALF_UP)))
    (is (instance? Money (m/rem (Money. cur (BigDecimal. "1.23")) 2)))
    (is (instance? Money (m/rem (Money. cur (BigDecimal. "1.23"))
                                0.123M
                                RoundingMode/HALF_UP)))
    (is (instance? BigDecimal (m/rem 10 3 RoundingMode/HALF_UP)))))

(deftest round-to-and-allocate-branches
  (let [money (m/of :PLN 10M)]
    (is (instance? Money (m/round-to money)))
    (is (thrown? clojure.lang.ExceptionInfo (m/round-to money (Object.))))
    (is (vector? (m/allocate money [1 0 1])))))

(deftest allocation-edge-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "10.00"))]
    (is (thrown? clojure.lang.ExceptionInfo (m/allocate money [])))
    (is (thrown? clojure.lang.ExceptionInfo (m/allocate money [0 0])))
    (is (vector? (m/allocate (Money. cur (BigDecimal. "-10.00")) [1 1])))
    (let [auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
          auto-money (Money. auto-cur (BigDecimal. "1.234"))]
      (is (vector? (m/allocate auto-money [1 1]))))))

(deftest data-literal-non-seq-coverage
  (with-redefs [m/code-literal (fn [_] 42)]
    (is (= 42 (m/data-literal :whatever)))))

(deftest format-default-arity-coverage
  (is (string? (m/format (m/of :PLN 1M)))))

(deftest bd-set-scale-and-monetary-scale-branches
  (let [bd (BigDecimal. "1.23")]
    (is (= bd (m-eval `(let [bd# ~bd] (m/bd-set-scale bd# 2 nil :scale/apply {})))))
    (is (= (BigDecimal. "1.2")
           (m-eval `(m/bd-set-scale (BigDecimal. "1.23") 1 RoundingMode/DOWN :scale/apply {}))))
    (is (= (BigDecimal. "1.2300")
           (m-eval `(m/bd-set-scale (BigDecimal. "1.23") 4 nil :scale/apply {}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m-eval `(m/bd-set-scale (BigDecimal. "1.23") 1 nil :scale/apply {}))))
    (is (identical? bd (#'m/monetary-scale bd c/auto-scaled)))
    (is (instance? BigDecimal (#'m/monetary-scale "1.23" 2)))
    (is (instance? BigDecimal (#'m/monetary-scale "1.23" 2 RoundingMode/HALF_UP)))
    (is (thrown? clojure.lang.ExceptionInfo (#'m/monetary-scale "1.23" 1 nil)))))

(deftest parsing-helper-extra-branches
  (let [num-start #'m/numeric-start?
        parse-num #'m/parse-number
        scf       #'m/split-currency-first]
    (is (false? (num-start "abc" 3)))
    (is (false? (num-start "+" 0)))
    (is (true? (num-start "+ 1" 0)))
    (is (false? (num-start "+ x" 0)))
    (is (= ["+1" 2] (parse-num "+1" 0)))
    (is (= ["1" 1] (parse-num "1+2" 0)))
    (is (= ["12" 3] (parse-num "1_2" 0)))
    (is (= ["1" 1] (parse-num "1_" 0)))
    (is (= ["PLN" "+12.30"] (scf "PLN +12.30")))
    (is (= ["PLN" "12.30"] (scf "PLN12.30")))
    (is (= ["PLN+ABC" nil] (scf "PLN +ABC")))))

(deftest currency-unit-and-parse-int-branches
  (is (nil? (m-eval `(m/currency-unit-strict 123))))
  (is (instance? Currency (m-eval `(m/currency-unit-strict :PLN))))
  (is (instance? Currency (m-eval `(m/currency-unit-strict {:id :PLN :scale 2}))))
  (binding [c/*default* (c/of-id :PLN)]
    (is (instance? Money (#'m/parse-int identity 12.34M)))
    (is (nil? (#'m/parse-int identity nil))))
  (with-redefs [c/unit (fn [_] nil)]
    (is (thrown? clojure.lang.ExceptionInfo (#'m/parse-int identity :PLN 1M)))
    (is (thrown? clojure.lang.ExceptionInfo (#'m/parse-int identity :PLN 1M RoundingMode/HALF_UP)))))

(deftest nil-branch-coverage
  (is (nil? (m/major-value nil)))
  (is (nil? (m/minor-value nil)))
  (is (nil? (m/on-amount nil inc)))
  (is (nil? (m/set-amount nil 1M)))
  (is (nil? (m/set-amount nil 1M RoundingMode/HALF_UP)))
  (is (nil? (m/amount nil)))
  (is (nil? (m/amount nil nil)))
  (is (nil? (m/amount nil nil nil)))
  (is (nil? (m/currency nil)))
  (is (nil? (m/currency nil nil)))
  (is (nil? (m/currency nil nil nil)))
  (is (nil? (m/stripped-amount nil)))
  (is (nil? (m/stripped-amount nil nil)))
  (is (nil? (m/stripped-amount nil nil nil)))
  (is (nil? (m/unparse nil)))
  (is (nil? (m/unparse nil nil)))
  (is (nil? (m/unparse nil nil nil))))

(deftest rescaled-and-predicate-false-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "1.00"))
        money-hi   (Money. cur (BigDecimal. "1.000"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "1.23"))
        a          (m/of :PLN 1M)
        b          (m/of :PLN 2M)
        c1         (m/of :PLN 3M)
        usd        (m/of :USD 1M)]
    (is (true? (m/rescaled? money-hi)))
    (is (false? (m/rescaled? money)))
    (is (nil? (m/rescaled? auto-money)))
    (is (false? (m/eq? money usd)))
    (is (false? (m/eq-am? money usd)))
    (is (false? (m/gt? a b c1)))
    (is (false? (m/ge? a b c1)))
    (is (false? (m/lt? c1 b a)))
    (is (false? (m/le? c1 b a)))))

(deftest same-currency-structural-branches
  (let [cur1 (c/new-currency :TST nil 2)
        cur2 (c/new-currency :TST nil 2)
        m1   (Money. cur1 (BigDecimal. "1.00"))
        m2   (Money. cur2 (BigDecimal. "2.00"))]
    (is (true? (m/same-currencies? m1 m2)))
    (is (instance? Money (m/add m1 m2)))
    (is (instance? Money (m/sub m1 m2)))
    (is (instance? Money (m/add m1 m2 m1)))
    (is (instance? Money (m/sub m1 m2 m1)))))

(deftest mul-branch-coverage-extra
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "2.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "2.00"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (m/mul money 2.5M)))
      (is (instance? Money (m/mul 2.5M money)))
      (is (instance? Money (m/mul money 2 3)))
      (is (instance? Money (m/mul-scaled money 2 3)))
      (is (instance? BigDecimal (m/mul-scaled 2 money 3)))
      (is (instance? Money (m/mul-scaled 2 3 money)))
      (is (instance? Money (m/mul-scaled auto-money 2 3))))
    (binding [scale/*rounding-mode* nil]
      (is (thrown? clojure.lang.ExceptionInfo (m/mul money 0.333M))))
    (is (instance? Money (m/mul auto-money 2.5M)))))

(deftest div-varargs-and-scaled-extra-branches
  (let [cur        (c/new-currency :TST nil 2)
        m1         (Money. cur (BigDecimal. "10.00"))
        m2         (Money. cur (BigDecimal. "2.00"))
        m3         (Money. cur (BigDecimal. "1.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        am1        (Money. auto-cur (BigDecimal. "10.00"))
        am2        (Money. auto-cur (BigDecimal. "2.00"))
        usd        (m/of :USD 1M)]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (m/div m1 2.5M)))
      (is (instance? BigDecimal (m/div 10 2 2))))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 m2 m3)))
    (is (instance? BigDecimal (m/div m1 2 m2 2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 2 usd)))
    (is (instance? Money (m/div am1 2 2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div am1 am2 usd)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 m2 m3 m2)))
    (is (instance? BigDecimal (m/div-scaled am1 am2 2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 am2 am2 am2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 am2 (m/of :USD 1M))))
    (is (instance? BigDecimal (m/div-scaled m1 m2 2)))
    (is (instance? BigDecimal (m/div-scaled m1 m2 m3 2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled m1 m2 m3 m2)))))

(deftest div-branch-matrix-extra
  (let [cur      (c/new-currency :TST nil 2)
        m1       (Money. cur (BigDecimal. "10.00"))
        m2       (Money. cur (BigDecimal. "2.00"))
        m3       (Money. cur (BigDecimal. "1.00"))
        auto-cur (c/new-currency :AUTO nil c/auto-scaled)
        am1      (Money. auto-cur (BigDecimal. "10.00"))
        am2      (Money. auto-cur (BigDecimal. "2.00"))
        am3      (Money. auto-cur (BigDecimal. "1.00"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? BigDecimal (m/div 10 4 2)))
      (is (instance? BigDecimal (m/div-scaled 10 4 2))))
    (binding [scale/*rounding-mode* nil]
      (is (instance? BigDecimal (m/div 8 2 2)))
      (is (instance? BigDecimal (m/div-scaled 8 2 2))))
    (is (instance? BigDecimal (m/div m1 m2)))
    (is (instance? Money (m/div m1 2)))
    (is (instance? Money (m/div m1 2M)))
    (is (instance? Money (m/div am1 2)))
    (is (instance? BigDecimal (m/div m1 2 m2 2)))
    (is (instance? Money (m/div m1 2 2)))
    (is (instance? BigDecimal (m/div-scaled am1 am2 am3 2)))
    (is (instance? Money (m/div-scaled am1 2 2)))
    (is (instance? BigDecimal (m/div-scaled m1 m2 m3 2)))))

(deftest rem-round-to-and-convert-extra-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "1.23"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "1.23"))]
    (is (instance? Money (m/rem auto-money 2)))
    (is (instance? Money (m/rem money 2)))
    (is (instance? BigDecimal (m/rem 10 3 nil)))
    (is (instance? Money (m/round-to money (m/value cur 0.05M))))
    (binding [scale/*rounding-mode* nil]
      (is (instance? Money (m/convert money auto-cur 2M))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/convert money cur 0.333333M scale/ROUND_UNNECESSARY)))))

(deftest bigint-and-readers-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "10.00"))]
    (is (vector? (m/allocate money [(BigInteger/valueOf 1) (BigInteger/valueOf 2)])))
    (is (vector? (m/allocate money [1N 2])))
    (is (vector? (m/allocate money [(BigInteger/valueOf 2)])))
    (is (vector? (m/allocate money [1N])))
    (is (instance? BigInteger (#'m/to-bigint 1N)))
    (is (thrown? clojure.lang.ExceptionInfo (m/allocate money [-1N])))
    (let [reg (c/register (registry/new-registry)
                          (c/new-currency :crypto/ETH nil 8))]
      (let [r (m/readers reg)
            h (get r 'money/crypto)]
        (is (map? r))
        (is (instance? Money (h [1M 'ETH])))))
    (is (map? (m/readers (registry/new-registry))))))

(deftest arith-ex-and-macro-expansion-branches
  (is (thrown? clojure.lang.ExceptionInfo
               (m-eval `(m/arith-ex :x {} (throw (ArithmeticException.))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (m-eval `(m/arith-ex :x {} (throw (ArithmeticException. "boom"))))))
  (is (= 1 (m-eval `(m/arith-ex :x {} 1))))
  (let [rm   'java.math.RoundingMode/HALF_UP
        exp1 (@#'m/mul-core nil nil 1M 2M 2 rm)
        exp2 (@#'m/mul-core nil nil 1M 2M 2 rm nil)
        exp3 (@#'m/mul-core nil nil 1M 2M)
        exp4 (@#'m/mul-core nil nil 1M 2M nil)
        exp5 (@#'m/mul-core nil nil 1M 1.234M 2 'java.math.RoundingMode/UNNECESSARY)]
    (is (seq? exp1))
    (is (instance? BigDecimal (m-eval exp1)))
    (is (instance? BigDecimal (m-eval exp2)))
    (is (instance? BigDecimal (m-eval exp3)))
    (is (instance? BigDecimal (m-eval exp4)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval exp5))))
  (let [rm   'java.math.RoundingMode/HALF_UP
        exp1 (@#'m/div-core nil nil 1M 2M rm)
        exp2 (@#'m/div-core nil nil 1M 2M 2 rm)
        exp3 (@#'m/div-core nil nil 1M 2M)
        exp4 (@#'m/div-core nil nil 1M 3M 2 'java.math.RoundingMode/UNNECESSARY)
        exp5 (@#'m/div-core nil nil 1M (m/of :PLN 1M))]
    (is (seq? exp1))
    (is (instance? BigDecimal (m-eval exp1)))
    (is (instance? BigDecimal (m-eval exp2)))
    (is (instance? BigDecimal (m-eval exp3)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval exp4)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval exp5)))))

(deftest parse-int-and-of-gen-extra-branches
  (binding [c/*default* (c/of-id :PLN)]
    (is (instance? Money (#'m/parse-int identity (m/of :PLN 1M))))
    (is (instance? Money (#'m/parse-int identity (c/of-id :PLN))))
    (is (instance? Money (#'m/parse-int identity "PLN"))))
  (is (instance? Money (#'m/parse-int identity :PLN 1.23M)))
  (is (instance? Money (#'m/parse-int identity :PLN 1.23M RoundingMode/HALF_UP)))
  (is (seq? (m/of-gen 'parse "PLN 1.00" :HALF_UP)))
  (is (seq? (m/of-gen 'parse "PLN 1.00" (Object.))))
  (is (seq? (m/of-gen 'parse (Object.) (Object.))))
  (is (seq? (m/of-gen 'parse (Object.) "PLN" :HALF_UP))))

(deftest on-amount-nil-arities
  (is (nil? (m/on-amount nil + 1M)))
  (is (nil? (m/on-amount nil + 1M 2M)))
  (is (nil? (m/on-amount nil + 1M 2M 3M)))
  (is (nil? (m/on-amount nil + 1M 2M 3M 4M))))

(deftest scalable-and-compare-extra-branches
  (let [auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "1.23"))
        a          (m/of :PLN 1M)
        b          (m/of :PLN 1M)
        usd        (m/of :USD 1M)]
    (is (true? (scale/scalable? auto-money)))
    (is (true? (scale/applied? auto-money)))
    (is (identical? auto-money (scale/apply auto-money)))
    (is (instance? BigDecimal (scale/amount auto-money)))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (scale/apply auto-money 3)))
      (is (instance? BigDecimal (scale/amount auto-money 3))))
    (is (true? (m/same-currencies? a b)))
    (is (false? (m/same-currencies? a usd)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare-amounts a usd)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare a usd)))))

(deftest map-json-nil-branches
  (is (nil? (m/to-map nil)))
  (is (nil? (m/to-json-map nil)))
  (is (nil? (m/to-json-map nil {})))
  (is (nil? (m/to-json-string nil)))
  (is (nil? (m/to-json-string nil {})))
  (is (instance? Money (m/of-map {"currency" "PLN" "amount" "1.00" "rounding" "HALF_UP"})))
  (is (instance? Money (m/of-map {"currency" "PLN" "amount" "1.00" "rounding-mode" "HALF_UP"})))
  (is (instance? Money (m/of-map {:currency :PLN :amount 1M :rounding :HALF_UP}))))

(deftest literals-and-readers-extra-branches
  (let [orig @#'clojure.core/*data-readers*]
    (m/defliteral :PLN)
    (let [v (resolve 'io.randomseed.bankster.money/of-PLN)]
      (is (= '(quote nil) (v nil)))
      (is (instance? Money (v 1M))))
    (alter-var-root #'clojure.core/*data-readers* (constantly orig)))
  (is (seq? (m/code-literal {:currency :PLN :amount 1M :rounding-mode :HALF_UP})))
  (is (instance? Money (#'m/ns-data-literal "crypto" [1M :ETH])))
  (is (seq? (#'m/ns-code-literal "crypto" [1M :ETH])))
  (is (seq? (#'m/ns-code-literal "crypto" [1M :ETH :HALF_UP]))))

(deftest load-readers-branches
  (with-redefs [fs/get-resource (fn [_] nil)]
    (is (nil? (#'m/load-readers "x"))))
  (with-redefs [fs/get-resource (fn [_] (StringReader. "nil"))
                clojure.edn/read-string (fn [_] nil)]
    (is (nil? (#'m/load-readers "x"))))
  (with-redefs [fs/get-resource (fn [_] (StringReader. "[]"))
                clojure.edn/read-string (fn [_] [])]
    (is (nil? (#'m/load-readers "x"))))
  (with-redefs [fs/get-resource (fn [_] (StringReader. "{:a io.randomseed.bankster.money/of}"))
                clojure.edn/read-string (fn [_] {:a 'io.randomseed.bankster.money/of})]
    (is (map? (#'m/load-readers "x")))))

(deftest bd-set-scale-branches
  (let [bd (BigDecimal. "1.23")]
    (is (= bd (m-eval `(m/bd-set-scale ~bd 2 java.math.RoundingMode/DOWN :x {}))))
    (is (= "1.230" (.toPlainString ^BigDecimal (m-eval `(m/bd-set-scale ~bd 3 nil :x {})))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m-eval `(m/bd-set-scale ~bd 1 nil :x {}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m-eval `(m/bd-set-scale ~bd 1 java.math.RoundingMode/UNNECESSARY :x {}))))
    (is (= "1.2" (.toPlainString ^BigDecimal (m-eval `(m/bd-set-scale ~bd 1 java.math.RoundingMode/DOWN :x {})))))))

(deftest round-to-non-money-first-arg
  (testing "round-to throws when first argument is not Money and interval is positive"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"First argument must be a kind of Money"
         (m/round-to "not-money" 0.05M)))))

(deftest parse-number-and-split-branches
  (let [parse-number #'m/parse-number
        split-first  #'m/split-currency-first]
    (is (= "+1" (first (parse-number "+1" 0))))
    (is (= "1" (first (parse-number "1" 0))))
    (is (= ["USD-1" "0"] (split-first "USD-10")))
    (is (= ["USD" "10"] (split-first "USD10")))
    (is (vector? (split-first "USD -")))
    (is (= ["USD" "-10"] (split-first "USD -10")))))

(deftest currency-unit-strict-and-parse-int-branches
  (is (instance? Currency (m-eval `(m/currency-unit-strict ~{:id :TST :sc 2}))))
  (is (nil? (m-eval `(m/currency-unit-strict 1))))
  (is (instance? Currency (m-eval `(m/currency-unit-strict :PLN))))
  (binding [c/*default* (c/of-id :PLN)]
    (is (instance? Money (#'m/parse-int identity 1.00M))))
  (binding [c/*default* (c/of-id :PLN)]
    (with-redefs [c/unit (fn [_] nil)]
      (is (instance? Money (#'m/parse-int identity " ")))))
  (is (thrown? clojure.lang.ExceptionInfo (#'m/parse-int identity 1 1M)))
  (is (thrown? clojure.lang.ExceptionInfo (#'m/parse-int identity 1 1M RoundingMode/HALF_UP))))

(deftest of-gen-more-branches
  (is (seq? (m/of-gen 'parse 1)))
  (is (seq? (m/of-gen 'parse "PLN")))
  (is (seq? (m/of-gen 'parse :PLN)))
  (is (seq? (m/of-gen 'parse "PLN 1.00" :HALF_UP)))
  (is (seq? (m/of-gen 'parse "PLN 1.00" RoundingMode/HALF_UP)))
  (is (seq? (m/of-gen 'parse "PLN 1.00" 10)))
  (is (seq? (m/of-gen 'parse "PLN 1.00" "HALF_UP")))
  (is (seq? (m/of-gen 'parse "PLN" 1 :HALF_UP)))
  (is (seq? (m/of-gen 'parse 1 :PLN :HALF_UP)))
  (is (seq? (m/of-gen 'parse (m/of :PLN 1M) :PLN :HALF_UP)))
  (is (seq? (m/of-gen 'parse (m/of :PLN 1M) 2 :HALF_UP))))

(deftest rounding-context-and-define-branches
  (is (contains? @#'m/rounding->context RoundingMode/HALF_EVEN))
  (let [mny (m/of :PLN 1M)
        reg (c/register (registry/new-registry) (c/new-currency :X nil 2))
        cur (c/of-id :X reg)
        missing (Money. (c/new-currency :ZZZ nil 2) (BigDecimal. "1.00"))]
    (is (true? (c/defined? mny)))
    (is (true? (c/defined? (Money. cur (BigDecimal. "1.00")) reg)))
    (is (false? (c/defined? missing)))))

(deftest scalable-apply-amount-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "1.234"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "1.23"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (scale/apply money)))
      (is (instance? Money (scale/apply money 3)))
      (is (instance? Money (scale/apply money 3 RoundingMode/HALF_UP)))
      (is (instance? BigDecimal (scale/amount money 3)))
      (is (instance? BigDecimal (scale/amount money 3 RoundingMode/HALF_UP))))
    (binding [scale/*rounding-mode* nil]
      (is (instance? Money (scale/apply money 3)))
      (is (instance? BigDecimal (scale/amount money 3))))
    (is (identical? auto-money (scale/apply auto-money)))))

(deftest compare-eq-and-same-branches
  (let [a  (m/of :PLN 1M)
        a2 (m/scale (m/of :PLN 1M) 3 RoundingMode/HALF_UP)
        b  (m/of :PLN 2M)
        c  (m/of :USD 1M)]
    (is (= 0 (m/compare-amounts nil nil)))
    (is (= -1 (m/compare-amounts nil a)))
    (is (= 1 (m/compare-amounts a nil)))
    (is (= -1 (m/compare-amounts a b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare-amounts a c)))
    (is (= 0 (m/compare nil nil)))
    (is (= -1 (m/compare nil a)))
    (is (= 1 (m/compare a nil)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare a a2)))
    (is (true? (m/eq? a a)))
    (is (false? (m/eq? a b)))
    (is (true? (m/eq-am? a a2)))
    (is (false? (m/eq-am? a c)))))

(deftest add-and-sub-branches
  (let [a (m/of :PLN 1M)
        b (m/of :PLN 2M)
        c (m/of :USD 1M)]
    (is (thrown? clojure.lang.ExceptionInfo (m/add 1 (m/of :PLN 1M))))
    (is (thrown? clojure.lang.ExceptionInfo (m/add (m/of :PLN 1M) 1)))
    (is (instance? Money (m/add a b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add a c)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add a b 1M)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add a b c)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub 1 (m/of :PLN 1M))))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub (m/of :PLN 1M) 1)))
    (is (instance? Money (m/sub a b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub a c)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub a b 1M)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub a b c)))))

(deftest mul-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "1.23"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "1.23"))]
    (is (thrown? clojure.lang.ExceptionInfo (m/mul money (m/of :PLN 1M))))
    (is (instance? Money (m/mul money 2)))
    (is (instance? Money (m/mul 2 money)))
    (is (instance? Money (m/mul auto-money 2)))
    (is (instance? Money (m/mul 2 auto-money)))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (m/mul money 2 3)))
      (binding [scale/*each* true]
        (is (instance? Money (m/mul money 2 3)))
        (is (instance? Money (m/mul 2 3 money)))))))

(deftest div-scaled-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "10.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "10.00"))
        other      (Money. (c/new-currency :USD nil 2) (BigDecimal. "2.00"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? BigDecimal (m/div-scaled 10 3 2)))
      (is (instance? BigDecimal (m/div-scaled auto-money 2 auto-money 2)))
      (is (instance? Money (m/div-scaled auto-money 2 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled auto-money 2 other)))
      (is (instance? BigDecimal (m/div-scaled money 2 money 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled money 2 other))))
    (binding [scale/*rounding-mode* nil]
      (is (instance? BigDecimal (m/div-scaled 10 2 2))))
    (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 10 money)))
    (is (thrown? clojure.lang.ExceptionInfo (#'m/div-core-fn (BigDecimal. "1") (BigDecimal. "3"))))))

(deftest div-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "10.00"))
        money2     (Money. cur (BigDecimal. "2.00"))
        other      (Money. (c/new-currency :USD nil 2) (BigDecimal. "2.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "10.00"))]
    (is (instance? BigDecimal (m/div money money2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div money other)))
    (is (instance? Money (m/div auto-money 2)))
    (is (instance? Money (m/div money 2)))
    (is (instance? Money (m/div money 2.5M)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div money 0)))
    (is (instance? BigDecimal (m/div 10 2)))
    (with-redefs [scale/ROUND_UNNECESSARY nil]
      (binding [scale/*rounding-mode* nil]
        (is (thrown? clojure.lang.ExceptionInfo (m/div 1M 3M)))))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? BigDecimal (m/div 100 2 2))))
    (binding [scale/*rounding-mode* nil]
      (is (instance? BigDecimal (m/div 100 2 2))))
    (is (instance? BigDecimal (m/div money money2 2)))
    (is (instance? Money (m/div money 2 2)))
    (is (instance? BigDecimal (m/div money 2 money2 2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div money 2 other)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div money money2 other)))))

(deftest rem-round-and-round-to-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "10.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "10.00"))
        other      (Money. (c/new-currency :USD nil 2) (BigDecimal. "2.00"))]
    (is (instance? Money (m/rem money 3 RoundingMode/HALF_UP)))
    (is (instance? Money (m/rem auto-money 3 RoundingMode/HALF_UP)))
    (is (instance? BigDecimal (m/rem money money RoundingMode/HALF_UP)))
    (is (thrown? clojure.lang.ExceptionInfo (m/rem money other RoundingMode/HALF_UP)))
    (is (instance? BigDecimal (m/rem 10 3)))
    (is (instance? BigDecimal (m/rem 10 3 nil)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval `(m/rem-core 1M ~(m/of :PLN 1M))))))
  (let [cur     (c/new-currency :TST nil 2)
        money   (Money. cur (BigDecimal. "1.23"))
        other   (Money. (c/new-currency :USD nil 2) (BigDecimal. "0.05"))
        interval (Money. cur (BigDecimal. "0.05"))]
    (is (identical? money (m/round money 2)))
    (is (instance? Money (m/round money 1 RoundingMode/DOWN)))
    (is (thrown? clojure.lang.ExceptionInfo (m/round-to money (Object.))))
    (is (thrown? clojure.lang.ExceptionInfo (m/round-to money other)))
    (binding [scale/*rounding-mode* nil]
      (is (instance? Money (m/round-to money interval))))))

(deftest bigint-ns-literals-and-readers-branches
  (is (instance? BigInteger (#'m/to-bigint 1)))
  (is (instance? Money (#'m/ns-data-literal "crypto" [1M :ETH])))
  (is (instance? Money (#'m/ns-data-literal "crypto" [:ETH 1M])))
  (is (seq? (#'m/ns-code-literal "crypto" [1M :ETH])))
  (is (seq? (#'m/ns-code-literal "crypto" [:ETH 1M])))
  (let [reg (-> (registry/new-registry)
                (c/register (c/new-currency :crypto/ETH nil 8))
                (c/register (c/new-currency :metal/AU nil 3))
                (c/register (c/new-currency :iso/TEST 978 2)))]
    (registry/with reg
      (is (seq? (#'m/ns-code-literal "iso" [1 978]))))
    (is (map? (m/readers)))
    (let [r (m/readers reg)]
      (is (contains? r 'money/crypto))
      (is (contains? r 'money/metal))
      (is (instance? Money ((get r 'money/crypto) [1M 'ETH]))))))

(deftest of-registry-branch
  (is (instance? Money (m/of-registry (m/of :PLN 1M)))))

(deftest compare-and-eq-variadic-branches
  (let [a  (m/of :PLN 1M)
        b  (m/of :PLN 2M)
        c  (m/of :USD 1M)
        a3 (m/scale a 3 RoundingMode/HALF_UP)]
    (is (= 0 (m/compare-amounts nil nil)))
    (is (= -1 (m/compare-amounts nil a)))
    (is (= 1 (m/compare-amounts a nil)))
    (is (= -1 (m/compare-amounts a b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare-amounts a c)))
    (is (= -1 (m/compare a b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare a a3)))
    (is (true? (m/eq? a a a)))
    (is (false? (m/eq? a b a)))
    (is (true? (m/eq-am? a a a)))
    (is (false? (m/eq-am? a b a)))))

(deftest min-max-branches
  (let [a (m/of :PLN 1M)
        b (m/of :PLN 2M)
        c (m/of :USD 1M)]
    (is (instance? Money (m/min-amount a b)))
    (is (instance? Money (m/max-amount a b)))
    (is (thrown? clojure.lang.ExceptionInfo (m/min-amount a c)))
    (is (thrown? clojure.lang.ExceptionInfo (m/max-amount a c)))))

(deftest convert-and-rem-extra-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "1.23"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (m/convert money cur 2M))))
    (is (instance? Money (m/convert money cur 2M RoundingMode/DOWN)))))

(deftest div-number-and-money-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "10.00"))
        money2     (Money. cur (BigDecimal. "2.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "10.00"))]
    (is (instance? Money (m/div money (Integer. 2))))
    (is (instance? Money (m/div money (BigDecimal. "2"))))
    (is (instance? Money (m/div auto-money 2)))
    (with-redefs [scale/ROUND_UNNECESSARY nil]
      (binding [scale/*rounding-mode* nil]
        (is (instance? BigDecimal (m/div 1M 2M)))))))

(deftest div-scaled-exception-branches
  (let [cur        (c/new-currency :TST nil 2)
        money      (Money. cur (BigDecimal. "10.00"))
        money2     (Money. cur (BigDecimal. "2.00"))
        auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
        auto-money (Money. auto-cur (BigDecimal. "10.00"))
        auto2      (Money. auto-cur (BigDecimal. "2.00"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 10 2 money2)))
      (is (instance? BigDecimal (m/div-scaled auto-money auto2 2)))
      (is (instance? BigDecimal (m/div-scaled money money2 2))))
    (binding [scale/*rounding-mode* nil]
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 10 2 money2))))))

(deftest bigint-and-readers-extra-branches
  (is (instance? BigInteger (#'m/to-bigint (Integer. 1))))
  (let [reg (c/register (registry/new-registry) (c/new-currency :USD nil 2))]
    (is (map? (m/readers reg)))
    (is (map? (m/readers (registry/new-registry))))))

(deftest ns-data-literal-numeric-branch
  (let [reg (c/register (registry/new-registry) (c/new-currency :iso/TEST 978 2))]
    (registry/with reg
      (is (thrown? clojure.lang.ExceptionInfo (#'m/ns-data-literal "iso" [1 978]))))))

(deftest structural-currency-callsite-branches
  (let [cur1 (c/new-currency :TST nil 2)
        cur2 (c/new-currency :TST nil 2)
        m1   (Money. cur1 (BigDecimal. "1.00"))
        m2   (Money. cur2 (BigDecimal. "1.00"))
        m3   (Money. cur2 (BigDecimal. "2.00"))
        i2   (Money. cur2 (BigDecimal. "0.05"))]
    (is (= 0 (m/compare-amounts m1 m2)))
    (is (= 0 (m/compare m1 m2)))
    (is (true? (m/eq? m1 m2)))
    (is (true? (m/eq-am? m1 m2)))
    (is (instance? Money (m/min-amount m1 m3)))
    (is (instance? Money (m/max-amount m1 m3)))
    (is (instance? BigDecimal (m/div m1 m2)))
    (is (instance? BigDecimal (m/rem m1 m2 RoundingMode/HALF_UP)))
    (is (instance? Money (m/round-to m1 i2)))))

(deftest div-core-macro-extra-branches
  (let [rm 'java.math.RoundingMode/HALF_UP
        exp1 (@#'m/div-core nil nil 1M 2M rm)
        exp2 (@#'m/div-core nil nil 1M 0M rm)
        exp3 (@#'m/div-core nil nil 1M 2M 2 rm)
        exp4 (@#'m/div-core nil nil 1M 0M 2 rm)
        exp5 (@#'m/div-core nil nil 1M 2M)
        exp6 (@#'m/div-core nil nil 1M 3M)
        exp7 (@#'m/div-core nil nil 1M (m/of :PLN 1M))]
    (is (instance? BigDecimal (m-eval exp1)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval exp2)))
    (is (instance? BigDecimal (m-eval exp3)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval exp4)))
    (is (instance? BigDecimal (m-eval exp5)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval exp6)))
    (is (thrown? clojure.lang.ExceptionInfo (m-eval exp7)))))

(deftest div-callsite-structural-and-exception-branches
  (let [cur1 (c/new-currency :TST nil 2)
        cur2 (c/new-currency :TST nil 2)
        m1   (Money. cur1 (BigDecimal. "10.00"))
        m2   (Money. cur2 (BigDecimal. "2.00"))
        ac1  (c/new-currency :AUTO nil c/auto-scaled)
        ac2  (c/new-currency :AUTO nil c/auto-scaled)
        am1  (Money. ac1 (BigDecimal. "10.00"))
        am2  (Money. ac2 (BigDecimal. "2.00"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? BigDecimal (m/div m1 m2)))
      (is (instance? BigDecimal (m/div m1 2 m2 2)))
      (is (instance? BigDecimal (m/div-scaled m1 2 m2 2)))
      (is (instance? BigDecimal (m/div-scaled am1 2 am2 2))))
    (binding [scale/*rounding-mode* RoundingMode/UNNECESSARY]
      (is (thrown? clojure.lang.ExceptionInfo (m/div 1 3 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 1 3 2))))))

(deftest div-error-callsite-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "10.00"))
        zero  (Money. cur BigDecimal/ZERO)
        auto  (Money. (c/new-currency :AUTO nil c/auto-scaled) (BigDecimal. "10.00"))
        auto0 (Money. (c/new-currency :AUTO nil c/auto-scaled) BigDecimal/ZERO)]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 1 0 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 1 2 0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled money 0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled money 2 zero 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled auto 0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled auto 2 auto0 2))))
    (binding [scale/*rounding-mode* RoundingMode/UNNECESSARY]
      (is (thrown? clojure.lang.ExceptionInfo (m/div 1 0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div money zero)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div money 0.0M)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div money 2 zero 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div money 0 2))))))

(deftest div-scaled-bm-true-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "10.00"))
        money2 (Money. (c/new-currency :TST nil 2) (BigDecimal. "2.00"))
        auto  (Money. (c/new-currency :AUTO nil c/auto-scaled) (BigDecimal. "10.00"))
        auto2 (Money. (c/new-currency :AUTO nil c/auto-scaled) (BigDecimal. "2.00"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? BigDecimal (m/div-scaled money money2 2)))
      (is (instance? BigDecimal (m/div-scaled auto auto2 2))))))

(deftest rem-core-error-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "10.00"))]
    (is (thrown? ArithmeticException (m/rem money 0 RoundingMode/UNNECESSARY)))
    (is (thrown? NullPointerException (m/rem money 2 nil)))))

(deftest rem-rounding-mode-dynamic-branches
  (let [cur   (c/new-currency :TST nil 2)
        money (Money. cur (BigDecimal. "10.00"))]
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (m/rem money 3))))))

(deftest monetary-scale-error-branches
  (let [bd (BigDecimal. "1.23")]
    (binding [scale/*rounding-mode* RoundingMode/UNNECESSARY]
      (is (thrown? clojure.lang.ExceptionInfo (#'m/monetary-scale bd 1))))
    (binding [scale/*rounding-mode* RoundingMode/DOWN]
      (is (instance? BigDecimal (#'m/monetary-scale bd 1))))
    (binding [scale/*rounding-mode* nil]
      (is (thrown? clojure.lang.ExceptionInfo (#'m/monetary-scale bd 1))))
    (is (instance? BigDecimal (#'m/monetary-scale 1 c/auto-scaled)))
    (is (instance? BigDecimal (#'m/monetary-scale 1 2)))
    (is (instance? BigDecimal (#'m/monetary-scale 1 2 RoundingMode/DOWN)))))

(deftest mul-error-branches
  (let [cur   (c/new-currency :TST nil 0)
        money (Money. cur (BigDecimal. "1"))]
    (binding [scale/*rounding-mode* RoundingMode/UNNECESSARY]
      (is (thrown? clojure.lang.ExceptionInfo (m/mul 1.5M money))))))

(deftest div-varargs-error-branches
  (binding [scale/*rounding-mode* RoundingMode/UNNECESSARY]
    (is (thrown? clojure.lang.ExceptionInfo (m/div 1 2 0))))
  (binding [scale/*rounding-mode* nil]
    (is (thrown? clojure.lang.ExceptionInfo (m/div 1 2 0)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div 1 0 2)))))

(deftest readers-no-namespace-branches
  (let [reg (c/register (registry/new-registry) (c/new-currency :USD nil 2))]
    (is (map? (m/readers reg)))
    (is (map? (m/readers nil)))))

(deftest readers-namespace-handler-branches
  (let [reg (-> (registry/new-registry)
                (c/register (c/new-currency :crypto/ETH nil 8)))
        r   (m/readers reg)]
    (is (instance? Money ((get r 'bankster.money/crypto) [1M 'ETH])))))

(deftest div-scaled-and-div-exception-branches
  (let [auto-cur (c/new-currency :AUTO nil c/auto-scaled)
        am1      (Money. auto-cur (BigDecimal. "10.00"))
        am2      (Money. auto-cur (BigDecimal. "2.00"))
        am0      (Money. auto-cur (BigDecimal. "0.00"))
        cur      (c/new-currency :TST nil 2)
        m1       (Money. cur (BigDecimal. "10.00"))
        m2       (Money. cur (BigDecimal. "2.00"))]
    (binding [scale/*rounding-mode* nil]
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 1 3)))
      (is (instance? Money (m/div-scaled am1 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled m1 3)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div 1 3 2))))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 2 am2 0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 2 am0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled m1 2 m2 0)))
      (is (thrown? clojure.lang.ExceptionInfo (m/div m1 2 m2 0))))))

(deftest rem-rounding-scale-branches
  (let [cur0   (c/new-currency :T0 nil 0)
        money0 (Money. cur0 (BigDecimal. "1"))]
    (is (instance? Money (m/rem money0 0.3M RoundingMode/HALF_UP)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/rem money0 0.3M RoundingMode/UNNECESSARY)))))

(deftest coverage-gap-branches
  (testing "parsing helpers"
    (let [parse-num #'m/parse-number
          split-first #'m/split-currency-first]
      (is (= "-" (first (parse-num "-" 0))))
      (is (= "+1" (first (parse-num "+1" 0))))
      (is (= "1" (first (parse-num "1" 0))))
      (is (= ["USD" "12"] (split-first "USD12")))
      (is (vector? (split-first "USD-12")))
      (is (vector? (split-first "USD +12")))
      (is (vector? (split-first "USD +ABC")))))
  (testing "currency-unit-strict and parse-int"
    (is (nil? (m-eval `(m/currency-unit-strict 1.0))))
    (is (instance? Currency (m-eval `(m/currency-unit-strict :PLN))))
    (is (instance? Currency (m-eval `(m/currency-unit-strict {:id :PLN :scale 2}))))
    (with-redefs [c/unit (fn [_] nil)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (m/parse :PLN 1M RoundingMode/HALF_UP)))))
  (testing "monetary-scale and scalable apply/amount"
    (let [cur       (c/new-currency :TST nil 2)
          money     (Money. cur (BigDecimal. "1.234"))
          money2    (Money. cur (BigDecimal. "1.20"))
          auto-cur  (c/new-currency :AUTO nil c/auto-scaled)
          auto-money (Money. auto-cur (BigDecimal. "1.234"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? BigDecimal (#'m/monetary-scale 1.23M 2)))
        (is (instance? Money (scale/apply money)))
        (is (instance? Money (scale/apply money 2)))
        (is (instance? Money (scale/apply money 2 RoundingMode/HALF_UP)))
        (is (instance? BigDecimal (scale/amount money 2)))
        (is (instance? BigDecimal (scale/amount money 2 RoundingMode/HALF_UP))))
      (binding [scale/*rounding-mode* nil]
        (is (instance? Money (scale/apply money2)))
        (is (instance? BigDecimal (scale/amount money2 2)))
        (is (thrown? clojure.lang.ExceptionInfo (scale/apply money))))
      (is (identical? auto-money (scale/apply auto-money)))
      (is (instance? BigDecimal (scale/amount auto-money 3)))))
  (testing "registry and defined?"
    (let [cur     (c/new-currency :TST nil 2)
          reg     (c/register (registry/new-registry) cur)
          money   (Money. cur (BigDecimal. "1.00"))
          missing (Money. (c/new-currency :ZZ nil 2) (BigDecimal. "1.00"))]
      (registry/with reg
        (is (instance? Money (m/of-registry money))))
      (is (instance? Money (m/of-registry reg money RoundingMode/DOWN)))
      (is (true? (c/defined? money reg)))
      (is (false? (c/defined? missing)))))
  (testing "compare/eq branches"
    (let [cur1 (c/new-currency :TST nil 2)
          cur2 (c/new-currency :TST nil 2)
          m1   (Money. cur1 (BigDecimal. "1.0"))
          m2   (Money. cur1 (BigDecimal. "2.00"))
          m3   (Money. cur2 (BigDecimal. "1.00"))
          usd  (m/of :USD 1M)]
      (is (true? (m/same-currencies? m1 m3)))
      (is (false? (m/same-currencies? m1 usd)))
      (is (true? (m/eq? m1 m1)))
      (is (false? (m/eq? m1 m2)))
      (is (false? (m/eq? m1 usd)))
      (is (true? (m/eq-am? m1 m3)))
      (is (true? (m/eq-am? m3 m1)))
      (is (false? (m/eq-am? m1 usd)))
      (is (= 0 (m/compare-amounts nil nil)))
      (is (= -1 (m/compare-amounts nil m1)))
      (is (= 1 (m/compare-amounts m1 nil)))
      (is (thrown? clojure.lang.ExceptionInfo (m/compare-amounts m1 usd)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (m/compare m1 (m/scale m1 3 RoundingMode/HALF_UP))))))
  (testing "add/sub varargs with structural currencies"
    (let [cur1 (c/new-currency :TST nil 2)
          cur2 (c/new-currency :TST nil 2)
          m1   (Money. cur1 (BigDecimal. "1.00"))
          m2   (Money. cur1 (BigDecimal. "2.00"))
          m3   (Money. cur2 (BigDecimal. "3.00"))
          usd  (m/of :USD 1M)]
      (is (instance? Money (m/add m1 m2)))
      (is (instance? Money (m/add m1 m3 m2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/add m1 m2 usd)))
      (is (instance? Money (m/sub m2 m1)))
      (is (instance? Money (m/sub m2 m3 m1)))
      (is (thrown? clojure.lang.ExceptionInfo (m/sub m2 m1 usd)))))
  (testing "mul branches"
    (let [cur        (c/new-currency :TST nil 2)
          money      (Money. cur (BigDecimal. "1.20"))
          money-hi   (Money. cur (BigDecimal. "1.234"))
          auto-cur   (c/new-currency :AUTO nil c/auto-scaled)
          auto-money (Money. auto-cur (BigDecimal. "1.23"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/mul-scaled money 2 3)))
        (is (instance? BigDecimal (m/mul-scaled 2 money 3)))
        (is (instance? Money (m/mul money 2)))
        (is (instance? Money (m/mul 2 money)))
        (is (instance? Money (m/mul money 2 3)))
        (is (instance? Money (m/mul auto-money 2 3))))
      (binding [scale/*rounding-mode* nil]
        (is (thrown? clojure.lang.ExceptionInfo (m/mul money-hi 0.333M))))))
  (testing "div scaled and div branches"
    (let [cur       (c/new-currency :TST nil 2)
          cur2      (c/new-currency :TST2 nil 2)
          m1        (Money. cur (BigDecimal. "10.00"))
          m2        (Money. cur (BigDecimal. "2.00"))
          m3        (Money. cur2 (BigDecimal. "2.00"))
          auto-cur  (c/new-currency :AUTO nil c/auto-scaled)
          auto-cur2 (c/new-currency :AUTO2 nil c/auto-scaled)
          am1       (Money. auto-cur (BigDecimal. "10.00"))
          am2       (Money. auto-cur (BigDecimal. "2.00"))
          am0       (Money. auto-cur (BigDecimal. "0.00"))
          am3       (Money. auto-cur2 (BigDecimal. "2.00"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? BigDecimal (m/div-scaled 10 2 2)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 10 0 2)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 10 2 0)))
        (is (instance? BigDecimal (m/div m1 m2)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div m1 m3)))
        (is (instance? BigDecimal (m/div 10 2 2)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div 10 2 0)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div 10 0 2)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div m1 2 m2 0)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div m1 2 m3 2)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div m1 0)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 2 am2 0)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 2 am0)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 am3))))
      (binding [scale/*rounding-mode* nil]
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled 1 3)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div m1 3)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div 1 3 2))))))
  (testing "min/max, convert, rem, round, round-to"
    (let [cur      (c/new-currency :TST nil 2)
          m1       (Money. cur (BigDecimal. "1.00"))
          m2       (Money. cur (BigDecimal. "2.00"))
          other    (Money. (c/new-currency :USD nil 2) (BigDecimal. "1.00"))
          cur0     (c/new-currency :T0 nil 0)
          m0       (Money. cur0 (BigDecimal. "1"))
          interval (Money. cur (BigDecimal. "0.05"))]
      (is (instance? Money (m/min-amount m1 m2)))
      (is (instance? Money (m/max-amount m2 m1)))
      (is (thrown? clojure.lang.ExceptionInfo (m/min-amount m1 other)))
      (is (thrown? clojure.lang.ExceptionInfo (m/max-amount m1 other)))
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/convert m1 cur 0.333333M)))
        (is (instance? Money (m/convert m1 cur 0.333333M RoundingMode/HALF_UP)))
        (is (instance? Money (m/round m1 1 RoundingMode/HALF_UP)))
        (is (instance? Money (m/round-to m1 interval RoundingMode/HALF_UP))))
      (binding [scale/*rounding-mode* nil]
        (is (thrown? clojure.lang.ExceptionInfo (m/convert m1 cur 0.333333M)))
        (is (instance? Money (m/round-to m1 interval))))
      (is (thrown? clojure.lang.ExceptionInfo (m/convert m1 cur 0.333333M RoundingMode/UNNECESSARY)))
      (is (instance? Money (m/rem m1 2 RoundingMode/HALF_UP)))
      (is (thrown? clojure.lang.ExceptionInfo (m/rem m1 other RoundingMode/HALF_UP)))
      (is (instance? Money (m/rem m0 0.3M RoundingMode/HALF_UP)))
      (is (thrown? clojure.lang.ExceptionInfo (m/rem m0 0.3M RoundingMode/UNNECESSARY)))
      (is (instance? Money (m/round m1 2)))
      (is (thrown? clojure.lang.ExceptionInfo (m/round-to m1 other)))))
  (testing "to-bigint and ns-data/readers"
    (is (instance? BigInteger (#'m/to-bigint 1)))
    (is (instance? BigInteger (#'m/to-bigint (Integer. 1))))
    (is (instance? BigInteger (#'m/to-bigint (BigInteger/valueOf 1))))
    (is (instance? BigInteger (#'m/to-bigint 1N)))
    (is (thrown? clojure.lang.ExceptionInfo (#'m/to-bigint 1.5)))
    (with-redefs [m/data-literal identity]
      (is (= [:crypto/ETH nil nil] (#'m/ns-data-literal "crypto" 'ETH)))
      (is (= [1 nil nil] (#'m/ns-data-literal "crypto" 1))))
    (is (map? (m/readers)))
    (is (map? (m/readers (registry/new-registry))))))

(deftest arith-message-and-div-core-num-branches
  (is (= "Arithmetic error." (#'m/arith-message (ArithmeticException.))))
  (is (= "boom" (#'m/arith-message (ArithmeticException. "boom"))))
  (is (instance? BigDecimal (#'m/div-core-num-fn 10M 2)))
  (is (thrown? clojure.lang.ExceptionInfo (#'m/div-core-num-fn 1M 3))))

(deftest parsing-forced-branches
  (let [parse-num #'m/parse-number
        split-first #'m/split-currency-first]
    (with-redefs [clojure.core/zero? (fn [_] false)]
      (is (vector? (parse-num "+1" 0))))
    (with-redefs [clojure.core/pos? (fn [_] false)]
      (is (vector? (split-first "USD +12"))))))

(deftest currency-equivalence-callsite-branches
  (let [cur1     (c/new-currency :EQ nil 2)
        cur2     (c/new-currency :EQ nil 2)
        m1       (Money. cur1 (BigDecimal. "1.00"))
        m2       (Money. cur2 (BigDecimal. "2.00"))
        m1b      (Money. cur2 (BigDecimal. "1.00"))
        interval (Money. cur2 (BigDecimal. "0.05"))]
    (is (true? (m/same-currencies? m1 m1)))
    (is (true? (m/same-currencies? m1 m2)))
    (is (true? (m/eq? m1 m1b)))
    (is (true? (m/eq-am? m1 m1b)))
    (is (instance? Money (m/add m1 m2)))
    (is (instance? Money (m/sub m2 m1)))
    (is (number? (m/compare-amounts m1 m2)))
    (is (number? (m/compare m1 m2)))
    (is (instance? Money (m/min-amount m1 m2)))
    (is (instance? Money (m/max-amount m1 m2)))
    (is (instance? BigDecimal (m/rem m2 m1 RoundingMode/HALF_UP)))
    (is (instance? Money (m/round-to m1 interval)))))

(deftest scalable-bd-set-scale-callsite-branches
  (let [cur     (c/new-currency :TST nil 2)
        money   (Money. cur (BigDecimal. "1.20"))
        money-hi (Money. cur (BigDecimal. "1.234"))]
    (binding [scale/*rounding-mode* nil]
      (is (instance? Money (scale/apply money)))
      (is (thrown? clojure.lang.ExceptionInfo (scale/apply money-hi)))
      (is (instance? Money (scale/apply money 2)))
      (is (thrown? clojure.lang.ExceptionInfo (scale/apply money-hi 2)))
      (is (instance? BigDecimal (scale/amount money 2)))
      (is (thrown? clojure.lang.ExceptionInfo (scale/amount money-hi 2))))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (scale/apply money-hi)))
      (is (instance? Money (scale/apply money-hi 2)))
      (is (instance? Money (scale/apply money-hi 2 RoundingMode/HALF_UP)))
      (is (instance? BigDecimal (scale/amount money-hi 2 RoundingMode/HALF_UP))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (scale/apply money-hi 2 RoundingMode/UNNECESSARY)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (scale/amount money-hi 2 RoundingMode/UNNECESSARY)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (scale/apply money-hi 2 nil)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (scale/amount money-hi 2 nil)))))

(deftest currency-kind-mismatch-branches
  (let [cur1 (c/new-currency :EQ nil 2 :K1 :DOM nil)
        cur2 (c/new-currency :EQ nil 2 :K2 :DOM nil)
        m1   (Money. cur1 (BigDecimal. "1.00"))
        m2   (Money. cur2 (BigDecimal. "2.00"))
        interval (Money. cur2 (BigDecimal. "0.05"))]
    (is (false? (m/same-currencies? m1 m2)))
    (is (false? (m/eq? m1 m2)))
    (is (false? (m/eq-am? m1 m2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/add m1 m2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/sub m1 m2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div m1 m2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled m1 2 m2 2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/min-amount m1 m2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/max-amount m1 m2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/rem m1 m2 RoundingMode/HALF_UP)))
    (is (thrown? clojure.lang.ExceptionInfo (m/round-to m1 interval)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare-amounts m1 m2)))
    (is (thrown? clojure.lang.ExceptionInfo (m/compare m1 m2)))))

(deftest defined-and-parse-int-default-branches
  (let [money   (m/of :PLN 1M)
        missing (Money. (c/new-currency :ZZ nil 2) (BigDecimal. "1.00"))]
    (is (true? (c/defined? money)))
    (is (false? (c/defined? missing))))
  (is (thrown? clojure.lang.ExceptionInfo (m/parse 1 1M RoundingMode/HALF_UP))))

(deftest convert-round-and-rem-extra-branches
  (let [cur   (c/new-currency :TST nil 2)
        cur0  (c/new-currency :T0 nil 0)
        money (Money. cur (BigDecimal. "1.23"))
        money0 (Money. cur0 (BigDecimal. "1"))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/convert money cur 0.333333M nil)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/round money 1 RoundingMode/UNNECESSARY)))
    (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
      (is (instance? Money (m/round-to money (Money. cur (BigDecimal. "0.05")) nil))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/rem money0 0.3M RoundingMode/UNNECESSARY)))))


(deftest coverage-missing-forms-extra
  (testing "bd-set-scale and round paths"
    (let [bd    (BigDecimal. "1.23")
          cur   (c/new-currency :TST nil 2)
          money (Money. cur (BigDecimal. "1.23"))]
      (is (= "1.2"
             (.toPlainString ^BigDecimal
                             (m-eval `(m/bd-set-scale ~bd 1 java.math.RoundingMode/DOWN :x {})))))
      (is (instance? Money (m/round money 1 RoundingMode/DOWN)))
      (is (thrown? clojure.lang.ExceptionInfo (m/round money 1 nil)))))
  (testing "parse-number and split-currency-first extra paths"
    (let [parse-num   #'m/parse-number
          split-first #'m/split-currency-first]
      (let [[num idx] (parse-num "A" 0)]
        (is (nil? num))
        (is (= 0 idx)))
      (is (= ["USD-1" "0"] (split-first "USD-10")))
      (is (= ["USD" "-10"] (split-first "USD -10")))
      (is (= ["USD+ABC" nil] (split-first "USD +ABC")))))
  (testing "rounding->context half-even entry"
    (let [ctx (get @#'m/rounding->context RoundingMode/HALF_EVEN)]
      (is (instance? java.math.MathContext ctx))
      (is (= RoundingMode/HALF_EVEN (.getRoundingMode ^java.math.MathContext ctx)))))
  (testing "mul varargs rescale path"
    (let [cur   (c/new-currency :TST nil 2)
          money (Money. cur (BigDecimal. "1.23"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/mul money 1.234M 2))))))
  (testing "div-scaled auto currency initial div-core"
    (let [auto-cur (c/new-currency :AUTO nil c/auto-scaled)
          am1      (Money. auto-cur (BigDecimal. "10.00"))
          am2      (Money. auto-cur (BigDecimal. "2.00"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/div-scaled am1 2 2)))
        (is (instance? BigDecimal (m/div-scaled am1 am2 2))))))
  (testing "rem non-auto number path"
    (let [cur   (c/new-currency :TST nil 2)
          money (Money. cur (BigDecimal. "10.00"))]
      (is (instance? Money (m/rem money (BigInteger/valueOf 3) RoundingMode/HALF_UP)))))
  (testing "round-to with explicit rounding"
    (let [cur      (c/new-currency :TST nil 2)
          money    (Money. cur (BigDecimal. "10.00"))
          interval (Money. cur (BigDecimal. "0.05"))]
      (binding [scale/*rounding-mode* nil]
        (is (instance? Money (m/round-to money interval RoundingMode/HALF_EVEN))))))
  (testing "to-bigint integer? branch"
    (is (instance? BigInteger (#'m/to-bigint (long 1)))))
  (testing "readers registry/get arities"
    (with-redefs [registry/get (fn
                                 ([] (registry/new-registry))
                                 ([reg] reg))]
      (is (map? (m/readers)))
      (is (map? (m/readers (registry/new-registry))))))
  (testing "defined? default registry lookup"
    (let [cur   (c/new-currency :DEF nil 2)
          reg   (c/register (registry/new-registry) cur)
          money (Money. cur (BigDecimal. "1.00"))]
      (registry/with reg
        (is (true? (c/defined? money)))))))

(deftest coverage-missing-forms-extra-2
  (testing "parse-number and split-currency-first forced branches"
    (let [parse-num   #'m/parse-number
          split-first #'m/split-currency-first]
      (with-redefs [io.randomseed.bankster.money/sign-char? (fn [_] false)]
        (is (vector? (parse-num "+1" 0))))
      (is (= ["12" 2] (parse-num "12" 0)))
      (is (vector? (split-first "1USD")))
      (is (vector? (split-first "-USD")))
      (with-redefs [clojure.core/pos? (fn [_] false)]
        (is (vector? (split-first "USD10"))))
      (with-redefs [io.randomseed.bankster.money/sep-char? (fn [_] true)]
        (is (vector? (split-first "USD-10"))))
      (with-redefs [io.randomseed.bankster.money/sep-char? (fn [_] false)]
        (is (vector? (split-first "USD +10"))))
      (with-redefs [io.randomseed.bankster.money/sign-char? (fn [_] false)]
        (is (vector? (split-first "USD +10"))))
      (with-redefs [io.randomseed.bankster.money/numeric-start? (fn [_ _] false)]
        (is (vector? (split-first "USD +10"))))))
  (testing "div-core macro extra arities and exceptions"
    (let [rm       'java.math.RoundingMode/HALF_UP
          exp-ok3  (@#'m/div-core nil nil 10M 2M rm)
          exp-bad3 (@#'m/div-core nil nil 1M 3M 'java.math.RoundingMode/UNNECESSARY)
          exp-ok2  (@#'m/div-core nil nil 10M 2M)
          exp-bad2 (@#'m/div-core nil nil 1M 3M)]
      (is (instance? BigDecimal (m-eval exp-ok3)))
      (is (thrown? clojure.lang.ExceptionInfo (m-eval exp-bad3)))
      (is (instance? BigDecimal (m-eval exp-ok2)))
      (is (thrown? clojure.lang.ExceptionInfo (m-eval exp-bad2)))))
  (testing "div-scaled loop init paths with success and failure"
    (let [auto-cur (c/new-currency :AUTO2 nil c/auto-scaled)
          am1      (Money. auto-cur (BigDecimal. "10.00"))
          am2      (Money. auto-cur (BigDecimal. "2.00"))
          cur      (c/new-currency :TST2 nil 2)
          m1       (Money. cur (BigDecimal. "10.00"))
          m2       (Money. cur (BigDecimal. "2.00"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/div-scaled am1 2 2)))
        (is (instance? BigDecimal (m/div-scaled am1 am2 2)))
        (is (instance? Money (m/div-scaled m1 2 2)))
        (is (instance? BigDecimal (m/div-scaled m1 m2 2))))
      (binding [scale/*rounding-mode* RoundingMode/UNNECESSARY]
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled am1 3 2)))
        (is (thrown? clojure.lang.ExceptionInfo (m/div-scaled m1 3 2))))))
  (testing "to-bigint integer? branch via int? override"
    (with-redefs [clojure.core/int? (fn [_] false)]
      (is (instance? BigInteger (#'m/to-bigint 1)))))
  (testing "round-to uses scale rounding-mode"
    (let [cur      (c/new-currency :TST3 nil 2)
          money    (Money. cur (BigDecimal. "10.00"))
          interval (Money. cur (BigDecimal. "0.05"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/round-to money interval nil))))
      (binding [scale/*rounding-mode* nil]
        (is (instance? Money (m/round-to money interval nil))))
      (scale/with-rounding RoundingMode/HALF_UP
        (is (instance? Money (m/round-to money interval RoundingMode/DOWN)))))))

(deftest coverage-missing-forms-extra-3
  (testing "readers with default registry sentinel"
    (let [reg (c/register (registry/new-registry)
                          (c/new-currency :DEF2 nil 2))]
      (binding [registry/*default* reg]
        (is (map? (m/readers)))
        (is (map? (m/readers reg))))
      (binding [registry/*default* reg]
        (is (map? (m/readers nil))))
      (binding [registry/*default* nil]
        (is (map? (m/readers nil))))))
  (testing "rem non-auto rounding scale path"
    (let [cur   (c/new-currency :TST4 nil 2)
          money (Money. cur (BigDecimal. "1.23"))]
      (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
        (is (instance? Money (m/rem money 1/3 RoundingMode/HALF_UP)))
        (is (instance? Money (m/rem money 0.333333M RoundingMode/HALF_UP)))))))
