(ns

    ^{:doc    "Bankster library, common serializers helpers tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.serializers.common-test

  (:require [clojure.test                               :refer [deftest testing is]]
            [io.randomseed.bankster                     :as bankster]
            [io.randomseed.bankster.registry            :as registry]
            [io.randomseed.bankster.scale               :as scale]
            [io.randomseed.bankster.serializers.common  :as sc])

  (:import (io.randomseed.bankster Currency Money)
           (java.math BigDecimal RoundingMode)))

(deftest parse-bigdec-contract
  (testing "parse-bigdec accepts supported inputs"
    (is (nil? (sc/parse-bigdec nil :op)))
    (let [bd (BigDecimal. "1.23")]
      (is (identical? bd (sc/parse-bigdec bd :op))))
    (is (= (BigDecimal/valueOf 12) (sc/parse-bigdec 12 :op)))
    (is (instance? BigDecimal (sc/parse-bigdec 12.0 :op)))
    (is (= (BigDecimal. "1000.50") (sc/parse-bigdec "1_000.50" :op))))
  (testing "parse-bigdec throws on invalid input"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Empty amount"
                          (sc/parse-bigdec "" :op)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid amount"
                          (sc/parse-bigdec "nope" :op)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                          (sc/parse-bigdec {:x 1} :op)))))

(deftest rescale-and-keys-filtering
  (testing "rescale-amount handles nil and same-scale"
    (let [bd (BigDecimal. "1.23")]
      (is (identical? bd (sc/rescale-amount bd nil nil)))
      (is (identical? bd (sc/rescale-amount bd 2 nil)))))
  (testing "rescale-amount with rounding-mode keyword"
    (let [bd (BigDecimal. "1.234")]
      (is (= "1.23" (.toPlainString ^BigDecimal (sc/rescale-amount bd 2 :HALF_UP))))))
  (testing "rescale-amount falls back to scale/*rounding-mode*"
    (let [bd (BigDecimal. "1.234")]
      (binding [scale/*rounding-mode* RoundingMode/FLOOR]
        (is (= "1.23" (.toPlainString ^BigDecimal (sc/rescale-amount bd 2 nil)))))))
  (testing "rescale-amount falls back to ROUND_UNNECESSARY and throws"
    (let [bd (BigDecimal. "1.234")
          ex (try
               (binding [scale/*rounding-mode* nil]
                 (sc/rescale-amount bd 2 nil))
               (catch clojure.lang.ExceptionInfo e e))]
      (is (= :bankster.serializers.common/rescale-amount (:op (ex-data ex))))
      (is (= scale/ROUND_UNNECESSARY (:rounding-mode (ex-data ex))))))
  (testing "normalize-rescale rejects too large values"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Integer/MAX_VALUE"
                          (sc/rescale-amount (BigDecimal. "1.00")
                                             (inc Integer/MAX_VALUE)
                                             nil))))
  (testing "extract-keys-spec and filter-map-by-keys"
    (is (nil? (sc/extract-keys-spec nil)))
    (is (= {} (sc/extract-keys-spec [])))
    (let [keys-spec (sc/extract-keys-spec [:amount 42 {:currency {:keys [:id]}}])
          m         {:amount  (BigDecimal. "1.00")
                     :currency {:id :PLN :numeric 985}}]
      (is (= m (sc/filter-map-by-keys m nil (fn [x _] x))))
      (is (= {:amount (BigDecimal. "1.00")
              :currency {:id :PLN}}
             (sc/filter-map-by-keys
              m keys-spec
              (fn [cur opts]
                (sc/filter-map-by-keys cur (sc/extract-keys-spec (:keys opts)) (fn [x _] x))))))
      (is (= {:amount (BigDecimal. "1.00")}
             (sc/filter-map-by-keys m {:amount nil :missing nil} (fn [x _] x)))))))

(deftest make-money-error-and-rescale
  (let [cur0 (bankster/->Currency :X 0 0 :iso/fiat :ISO-4217)
        cur2 (bankster/->Currency :Y 0 2 :iso/fiat :ISO-4217)
        reg  (registry/new-registry)]
    (testing "make-money wraps ArithmeticException on downscale without rounding"
      (binding [scale/*rounding-mode* nil]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"ArithmeticException"
             (sc/make-money cur0 (BigDecimal. "1.1") reg)))))
    (testing "make-money 4-arity uses fallback rounding-mode and throws on loss"
      (binding [scale/*rounding-mode* nil]
        (let [ex (try
                   (sc/make-money cur0 (BigDecimal. "1.1") reg nil)
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (= :bankster.serializers.common/make-money (:op (ex-data ex))))
          (is (= nil (:rounding-mode (ex-data ex)))))))
    (testing "make-money 4-arity reports explicit rounding-mode on error"
      (let [ex (try
                 (sc/make-money cur0 (BigDecimal. "1.1") reg RoundingMode/UNNECESSARY)
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (= :bankster.serializers.common/make-money (:op (ex-data ex))))
        (is (= RoundingMode/UNNECESSARY (:rounding-mode (ex-data ex))))))
    (testing "make-money supports explicit rounding-mode"
      (let [m (sc/make-money cur2 (BigDecimal. "1.239") reg
                             RoundingMode/HALF_UP)]
        (is (instance? Money m))
        (is (= "1.24" (.toPlainString ^BigDecimal (.amount ^Money m))))))
    (testing "make-money supports rescale (overrides currency scale)"
      (let [m (sc/make-money cur2 (BigDecimal. "1.2300") reg
                             nil 4)]
        (is (= 4 (.scale ^BigDecimal (.amount ^Money m))))))
    (testing "make-money rescale wraps ArithmeticException on loss"
      (let [ex (try
                 (sc/make-money cur0 (BigDecimal. "1.1") reg nil 0)
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (= :bankster.serializers.common/make-money (:op (ex-data ex))))
        (is (= 0 (:scale (ex-data ex))))
        (is (= true (:arithmetic-exception (ex-data ex))))))))
