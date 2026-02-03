(ns

    ^{:doc    "bankster library, scale tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.scale-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [clojure.test.check.generators   :as tgen]
            [clojure.test.check.properties   :as prop]
            [clojure.test.check.clojure-test :as ctest]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.scale    :as scale])

  (:import (java.math BigDecimal BigInteger RoundingMode)))

(defn- bd=
  "BigDecimal equality ignoring scale (value equality)."
  [^BigDecimal a ^BigDecimal b]
  (zero? (.compareTo a b)))

(defn- ex-data-of
  "Executes `f` and returns ex-data if it throws ExceptionInfo, otherwise nil."
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- with-clean-thread-rounding-mode
  "Runs `f` with thread-local rounding-mode cleared, then restores it."
  [f]
  (let [^ThreadLocal tl @#'scale/thread-rounding-mode
        prev          (.get tl)]
    (try
      (when (some? prev) (.remove tl))
      (f)
      (finally
        (if (nil? prev) (.remove tl) (.set tl prev))))))

(deftest bigdecimal-apply-downscale-requires-rounding
  (testing "downscaling without rounding throws ExceptionInfo with :arithmetic-exception"
    (let [x 1.23M]
      (with-clean-thread-rounding-mode
        (fn []
          (binding [scale/*rounding-mode* nil]
            (try
              (scale/apply x 0)
              (is false "expected an exception")
              (catch clojure.lang.ExceptionInfo e
                (let [d (ex-data e)]
                  (is (= :scale/apply (:op d)))
                  (is (= 0 (:scale d)))
                  (is (= x (:value d)))
                  (is (contains? d :rounding-mode))
                  (is (nil? (:rounding-mode d)))
                  (is (true? (:arithmetic-exception d)))))))))))
  (testing "downscaling with ROUND_UNNECESSARY throws ExceptionInfo too (explicit rounding-mode path)"
    (let [x 1.23M]
      (try
        (scale/apply x 0 scale/ROUND_UNNECESSARY)
        (is false "expected an exception")
        (catch clojure.lang.ExceptionInfo e
          (let [d (ex-data e)]
            (is (= :scale/apply (:op d)))
            (is (= 0 (:scale d)))
            (is (= x (:value d)))
            (is (= RoundingMode/UNNECESSARY (:rounding-mode d)))
            (is (true? (:arithmetic-exception d))))))))
  (testing "downscaling with explicit rounding mode works"
    (is (= 2M (scale/apply 1.5M 0 scale/ROUND_HALF_UP)))
    (is (= 1M (scale/apply 1.4M 0 scale/ROUND_HALF_UP)))))

(deftest bigdecimal-apply-upscale-and-exact-downscale-without-rounding
  (testing "upscaling does not require rounding"
    (is (= 1.00M (scale/apply 1M 2))))
  (testing "exact downscale succeeds without rounding mode"
    (binding [scale/*rounding-mode* nil]
      (is (= 1.2M (scale/apply 1.20M 1))))))

(deftest bigdecimal-amount-arities
  (testing "amount supports arities 2/3 for BigDecimal"
    (is (= 1.20M (scale/amount 1.2M 2)))
    (is (= 2M (scale/amount 1.5M 0 scale/ROUND_HALF_UP)))))

(deftest bigdecimal-apply-same-scale-is-identity
  (testing "apply returns the same BigDecimal instance when the scale is unchanged"
    (let [x 1.23M]
      (is (identical? x (scale/apply x 2)))
      (is (identical? x (scale/apply x 2 scale/ROUND_HALF_UP))))))

(deftest ratio-apply-nonterminating-expansion
  (testing "ratio without rounding throws (non-terminating decimal expansion)"
    (try
      (scale/apply 1/3)
      (is false "expected an exception")
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e)]
          (is (= :scale/apply (:op d)))
          (is (true? (:arithmetic-exception d)))
          (is (instance? BigDecimal (:numerator d)))
          (is (instance? BigDecimal (:denominator d)))))))
  (testing "ratio with scale and rounding returns a scaled BigDecimal"
    (is (= 0.33M (scale/apply 1/3 2 scale/ROUND_HALF_UP)))
    (is (= 0.67M (scale/apply 2/3 2 scale/ROUND_HALF_UP)))))

(deftest ratio-regressions
  (testing "selected ratios without rounding must throw"
    (doseq [x [1/3 2/7 10/6]]
      (try
        (scale/apply x)
        (is false (str "expected ex-info for " x))
        (catch clojure.lang.ExceptionInfo e
          (is (= :scale/apply (:op (ex-data e))))
          (is (true? (:arithmetic-exception (ex-data e)))))))))

(deftest rounding-mode-contract
  (testing "when thread-rounding-mode is empty and *rounding-mode* is nil, returns nil"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* nil]
          (is (nil? (scale/rounding-mode)))))))
  (testing "when *rounding-mode* is set, it is returned (thread-local empty)"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* RoundingMode/DOWN]
          (is (= RoundingMode/DOWN (scale/rounding-mode)))))))
  (testing "rounding-mode arity-1 returns *rounding-mode* when thread-local empty"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* RoundingMode/DOWN]
          (is (= RoundingMode/DOWN (scale/rounding-mode RoundingMode/HALF_UP)))))))
  (testing "thread-local wins over *rounding-mode*"
    (let [^ThreadLocal tl @#'scale/thread-rounding-mode
          prev          (.get tl)]
      (.set tl RoundingMode/UP)
      (try
        (binding [scale/*rounding-mode* RoundingMode/DOWN]
          (is (= RoundingMode/UP (scale/rounding-mode)))
          (is (= RoundingMode/UP (scale/rounding-mode RoundingMode/HALF_UP))))
        (finally
          (if (nil? prev) (.remove tl) (.set tl prev))))))
  (testing "rounding-mode arity-1 returns default when both sources are empty"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* nil]
          (is (= RoundingMode/HALF_UP (scale/rounding-mode RoundingMode/HALF_UP))))))))

(deftest div-math-context-contract
  (testing "arity-2 uses ROUND_UNNECESSARY when no rounding mode is available"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* nil]
          (let [^java.math.MathContext mc (scale/div-math-context 1M 2M)]
            (is (= RoundingMode/UNNECESSARY (.getRoundingMode mc))))))))
  (testing "arity-2 uses thread-local/dynamic rounding mode when present"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
          (let [^java.math.MathContext mc (scale/div-math-context 1M 3M)]
            (is (= RoundingMode/HALF_UP (.getRoundingMode mc))))))))
  (testing "arity-3 uses the given rounding mode"
    (let [^java.math.MathContext mc (scale/div-math-context 1M 3M RoundingMode/DOWN)]
      (is (= RoundingMode/DOWN (.getRoundingMode mc))))))

(deftest arithmetic-ex-wrapping-contract
  (let [arithmetic-ex->ex-info (var-get #'scale/arithmetic-ex->ex-info)]
    (testing "uses original exception message when present"
      (let [e (ArithmeticException. "boom")
            x (arithmetic-ex->ex-info :scale/apply {:value 1} e)]
        (is (= "boom" (ex-message x)))
        (is (= true (:arithmetic-exception (ex-data x))))))
    (testing "falls back to a stable message when exception message is nil"
      (let [e (ArithmeticException.)
            x (arithmetic-ex->ex-info :scale/apply {:value 1} e)]
        (is (= "Arithmetic error." (ex-message x)))
        (is (= :scale/apply (:op (ex-data x))))
        (is (= true (:arithmetic-exception (ex-data x))))))))

(deftest parse-rounding-and-post-parse-rounding
  (testing "parse-rounding accepts ROUND_ prefix and returns a symbol"
    (is (= 'java.math.RoundingMode/HALF_UP (scale/parse-rounding :ROUND_HALF_UP)))
    (is (= 'java.math.RoundingMode/HALF_UP (scale/parse-rounding "ROUND_HALF_UP"))))
  (testing "parse-rounding accepts HALF_UP as keyword/symbol/string"
    (is (= 'java.math.RoundingMode/HALF_UP (scale/parse-rounding :HALF_UP)))
    (is (= 'java.math.RoundingMode/HALF_UP (scale/parse-rounding 'HALF_UP)))
    (is (= 'java.math.RoundingMode/HALF_UP (scale/parse-rounding "HALF_UP"))))
  (testing "parse-rounding passes through RoundingMode instances"
    (is (= RoundingMode/HALF_UP (scale/parse-rounding RoundingMode/HALF_UP))))
  (testing "post-parse-rounding keeps a RoundingMode instance"
    (is (= RoundingMode/HALF_UP (scale/post-parse-rounding RoundingMode/HALF_UP))))
  (testing "post-parse-rounding passes through non-ident, non-string values"
    (is (= 123 (scale/post-parse-rounding 123))))
  (testing "post-parse-rounding accepts ROUND_ prefix too"
    (is (= RoundingMode/HALF_UP (scale/post-parse-rounding :ROUND_HALF_UP))))
  (testing "unknown strings are returned as-is"
    (is (= "NOPE" (scale/parse-rounding "NOPE")))
    (is (= "NOPE" (scale/post-parse-rounding "NOPE"))))
  (testing "parse-rounding + post-parse-rounding work together (symbol resolves at runtime)"
    (let [sym (scale/parse-rounding :HALF_UP)]
      (is (= RoundingMode/HALF_UP (scale/post-parse-rounding (eval sym)))))))

(deftest with-rounding-parses-rounding-tokens
  (testing "macro path: parse-rounding -> post-parse-rounding"
    (is (= RoundingMode/HALF_UP (scale/with-rounding :ROUND_HALF_UP (scale/rounding-mode))))
    (is (= RoundingMode/HALF_UP (scale/with-rounding "ROUND_HALF_UP" (scale/rounding-mode))))
    (is (= RoundingMode/HALF_UP (scale/with-rounding :HALF_UP (scale/rounding-mode))))
    (is (= RoundingMode/HALF_UP (scale/with-rounding 'HALF_UP (scale/rounding-mode))))
    (is (= RoundingMode/HALF_UP (scale/with-rounding "HALF_UP" (scale/rounding-mode))))))

(deftest with-rounding-uses-threadlocal-fast-path
  (testing "scale/rounding-mode reflects nested bindings and restores correctly"
    (binding [scale/*rounding-mode* nil]
      (is (nil? (scale/rounding-mode)))
      (is (nil? scale/*rounding-mode*))
      (scale/with-rounding HALF_UP
        (is (= RoundingMode/HALF_UP (scale/rounding-mode)))
        (is (= RoundingMode/HALF_UP scale/*rounding-mode*))
        (scale/with-rounding DOWN
          (is (= RoundingMode/DOWN (scale/rounding-mode)))
          (is (= RoundingMode/DOWN scale/*rounding-mode*)))
        (is (= RoundingMode/HALF_UP (scale/rounding-mode)))
        (is (= RoundingMode/HALF_UP scale/*rounding-mode*)))
      (is (nil? (scale/rounding-mode)))
      (is (nil? scale/*rounding-mode*)))))

(deftest with-rounding-restores-on-exception
  (testing "thread-local and *rounding-mode* are restored even when body throws"
    (let [^ThreadLocal tl @#'scale/thread-rounding-mode
          prev          (.get tl)]
      (.set tl RoundingMode/UP)
      (try
        (binding [scale/*rounding-mode* RoundingMode/DOWN]
          (is (= RoundingMode/UP (scale/rounding-mode)))
          (is (= RoundingMode/DOWN scale/*rounding-mode*))
          (try
            (scale/with-rounding HALF_UP
              (is (= RoundingMode/HALF_UP (scale/rounding-mode)))
              (is (= RoundingMode/HALF_UP scale/*rounding-mode*))
              (throw (ex-info "boom" {:boom true})))
            (is false "expected exception")
            (catch clojure.lang.ExceptionInfo e
              (is (= true (:boom (ex-data e))))))
          (is (= RoundingMode/UP (scale/rounding-mode)))
          (is (= RoundingMode/DOWN scale/*rounding-mode*)))
        (finally
          (if (nil? prev) (.remove tl) (.set tl prev)))))))

(deftest with-rescaling-enables-each
  (testing "with-rescaling binds *each* and rounding mode"
    (is (false? scale/*each*))
    (scale/with-rescaling HALF_UP
      (is (true? scale/*each*))
      (is (= RoundingMode/HALF_UP (scale/rounding-mode))))
    (is (false? scale/*each*))))

(deftest each-macro-contract
  (testing "each binds *each* to true only within the body"
    (binding [scale/*each* false]
      (is (false? scale/*each*))
      (scale/each
        (is (true? scale/*each*)))
      (is (false? scale/*each*)))))

(deftest clojure-number-rendering
  (testing "to-clojure-string adds M suffix when precision >= 16"
    (is (= "999999999999999"   (scale/to-clojure-string 999999999999999M)))
    (is (= "1000000000000000M" (scale/to-clojure-string 1000000000000000M)))))

(deftest scalable-semantics
  (testing "scalable?/applied? are falsey for non-scalables"
    (is (false? (scale/scalable? (Object.))))
    (is (false? (scale/applied? (Object.))))
    (is (nil? (scale/of (Object.)))))
  (testing "applied? for BigDecimal is true; for String/Number is falsey"
    (is (true? (scale/applied? (bigdec "1.0"))))
    (is (false? (scale/applied? "1.23")))
    (is (false? (boolean (scale/applied? 1)))))
  (testing "amount always returns BigDecimal for supported values"
    (is (instance? BigDecimal (scale/amount 1)))
    (is (instance? BigDecimal (scale/amount "1.23"))))
  (testing "of returns the BigDecimal scale as long"
    (is (= 1 (scale/of (bigdec "1.0"))))
    (is (= 2 (scale/of (bigdec "1.00"))))))

(deftest scalable-predicates-all-implementations
  (testing "scalable?/applied? cover all built-in Scalable implementations"
    (is (true?  (scale/scalable? 1.0M)))
    (is (true?  (scale/scalable? (bigint 1))))
    (is (true?  (scale/scalable? (BigInteger/valueOf 1))))
    (is (true?  (scale/scalable? 1.5)))
    (is (true?  (scale/scalable? (float 1.5))))
    (is (true?  (scale/scalable? 1/2)))
    (is (true?  (scale/scalable? 42)))
    (is (true?  (scale/scalable? "1.23")))
    (is (false? (scale/scalable? nil)))
    (is (false? (scale/scalable? (Object.))))
    (is (true?  (scale/applied? 1.0M)))
    (is (false? (scale/applied? (bigint 1))))
    (is (false? (scale/applied? (BigInteger/valueOf 1))))
    (is (false? (scale/applied? 1.5)))
    (is (false? (scale/applied? (float 1.5))))
    (is (false? (scale/applied? 1/2)))
    (is (false? (scale/applied? 42)))
    (is (false? (scale/applied? "1.23")))
    (is (false? (scale/applied? nil)))
    (is (false? (scale/applied? (Object.))))))

(deftest scalable-nil-and-object-arities
  (testing "nil/Object implementations are total (all arities) and return nil/falsey"
    (let [o (Object.)]
      (is (false? (scale/scalable? nil)))
      (is (false? (scale/applied? nil)))
      (is (nil? (scale/of nil)))
      (is (nil? (scale/apply nil)))
      (is (nil? (scale/apply nil 2)))
      (is (nil? (scale/apply nil 2 scale/ROUND_HALF_UP)))
      (is (nil? (scale/amount nil)))
      (is (nil? (scale/amount nil 2)))
      (is (nil? (scale/amount nil 2 scale/ROUND_HALF_UP)))
      (is (false? (scale/scalable? o)))
      (is (false? (scale/applied? o)))
      (is (nil? (scale/of o)))
      (is (nil? (scale/apply o)))
      (is (nil? (scale/apply o 2)))
      (is (nil? (scale/apply o 2 scale/ROUND_HALF_UP)))
      (is (nil? (scale/amount o)))
      (is (nil? (scale/amount o 2)))
      (is (nil? (scale/amount o 2 scale/ROUND_HALF_UP))))))

(deftest scalable-protocol-arities-exhaustive
  (testing "exercise all Scalable arities across built-in implementations"
    (let [cases [{:v 1.23M                     :scale2 2 :rm scale/ROUND_HALF_UP :pred #(instance? BigDecimal %)}
                 {:v (BigInteger/valueOf 123)  :scale2 2 :rm scale/ROUND_HALF_UP :pred #(instance? BigDecimal %)}
                 {:v 1.5                       :scale2 2 :rm scale/ROUND_HALF_UP :pred #(instance? BigDecimal %)}
                 {:v (float 1.5)               :scale2 2 :rm scale/ROUND_HALF_UP :pred #(instance? BigDecimal %)}
                 {:v 1/2                       :scale2 2 :rm scale/ROUND_HALF_UP :pred #(instance? BigDecimal %)}
                 {:v 42                        :scale2 2 :rm scale/ROUND_HALF_UP :pred #(instance? BigDecimal %)}
                 {:v "12.3"                    :scale2 2 :rm scale/ROUND_HALF_UP :pred #(instance? BigDecimal %)}
                 {:v nil                       :scale2 2 :rm scale/ROUND_HALF_UP :pred nil?}
                 {:v (Object.)                 :scale2 2 :rm scale/ROUND_HALF_UP :pred nil?}]]
      (doseq [{:keys [v scale2 rm pred]} cases]
        (is (boolean? (boolean (scale/scalable? v))))
        (is (boolean? (boolean (scale/applied? v))))
        (scale/of v)
        (is (pred (scale/apply v)))
        (is (pred (scale/apply v scale2)))
        (is (pred (scale/apply v scale2 rm)))
        (is (pred (scale/amount v)))
        (is (pred (scale/amount v scale2)))
        (is (pred (scale/amount v scale2 rm)))))))

(deftest integer-and-fractional-invariants
  (testing "integer + fractional/10^scale reconstructs the original value (BigDecimal)"
    (let [n (bigdec "-12.3400")]
      (is (bd=
           (scale/amount n)
           (.add ^BigDecimal (scale/integer n)
                 (.movePointLeft ^BigDecimal (scale/fractional n) (scale/of n)))))))
  (testing "fractional is within [0, 10^scale) for non-negative numbers"
    (let [n 12.34M
          sc (scale/of n)
          frac (scale/fractional n)
          ^BigDecimal lim (.pow BigDecimal/TEN (int sc))]
      (is (not (neg? (.signum ^BigDecimal frac))))
      (is (neg? (.compareTo ^BigDecimal frac lim))))))

(deftest float-double-conversions
  (testing "->float/->double without rounding throw when precision cannot be represented"
    (binding [scale/*rounding-mode* nil]
      ;; NOTE: call via var-get to stay compatible with cloverage instrumentation
      ;; (instrumented Vars may not implement primitive IFn interfaces).
      (let [->float  (var-get #'scale/->float)
            ->double (var-get #'scale/->double)]
        (is (thrown? ArithmeticException (->float 1234567M)))
        (is (thrown? ArithmeticException (->double 1234567890123456M))))))
  (testing "->float/->double with rounding work"
    (let [->float  (var-get #'scale/->float)
          ->double (var-get #'scale/->double)]
      (is (float?  (->float  1234567M scale/ROUND_HALF_UP)))
      (is (double? (->double 1234567890123456M scale/ROUND_HALF_UP))))))

(deftest float-double-default-rounding-mode
  (testing "->float/->double (arity-1) use *rounding-mode* when present"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
          (let [->float  (var-get #'scale/->float)
                ->double (var-get #'scale/->double)]
            (is (float?  (->float  1234567M)))
            (is (double? (->double 1234567890123456M))))))))
  (testing "->float/->double (arity-2) accept scale and use rounding-mode source"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
          (let [->float  (var-get #'scale/->float)
                ->double (var-get #'scale/->double)]
            (is (float?  (->float  12.34M 1)))
            (is (double? (->double 12.34M 1))))))))
  (testing "->float/->double (arity-2) fall back to ROUND_UNNECESSARY when no rounding mode is present"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* nil]
          (let [->float  (var-get #'scale/->float)
                ->double (var-get #'scale/->double)]
            (is (float?  (->float  12.30M 1)))
            (is (double? (->double 12.30M 1))))))))
  (testing "->float/->double (arity-3) accept scale + explicit rounding-mode"
    (let [->float  (var-get #'scale/->float)
          ->double (var-get #'scale/->double)]
      (is (float?  (->float  12.34M 1 scale/ROUND_HALF_UP)))
      (is (double? (->double 12.34M 1 scale/ROUND_HALF_UP))))))

(deftest int-long-and-printable-conversions
  (testing "->int/->long exact conversions"
    (is (= 12 (scale/->int 12M)))
    (is (= 12 (scale/->long 12M))))
  (testing "->int/->long with rounding"
    (is (= 1 (scale/->int 1.9M scale/ROUND_DOWN)))
    (is (= 1 (scale/->long 1.9M scale/ROUND_DOWN))))
  (testing "to-plain-string and symbol conversions"
    (is (= "12.3400" (scale/to-plain-string (bigdec "12.3400"))))
    (is (= (symbol "12.3400") (scale/to-symbol (bigdec "12.3400"))))
    (is (= (symbol "1000000000000000M") (scale/to-clojure-symbol 1000000000000000M)))))

(deftest with-alias-contract
  (testing "scale/with is an alias for scale/apply"
    (is (= (scale/apply 1.23M)
           (scale/with  1.23M)))
    (is (= (scale/apply 1.20M 1)
           (scale/with  1.20M 1)))
    (is (= (scale/apply 1.23M 0 scale/ROUND_HALF_UP)
           (scale/with  1.23M 0 scale/ROUND_HALF_UP)))))

(deftest integer-fractional-arities
  (testing "integer/fractional support arities 1/2/3"
    (is (= 12M (scale/integer 12.34M)))
    (is (= 12M (scale/integer 12.34M 2)))
    (is (= 12M (scale/integer 12.34M 2 scale/ROUND_HALF_UP)))
    (is (= 34M (scale/fractional 12.34M)))
    (is (= 34M (scale/fractional 12.34M 2)))
    (is (= 34M (scale/fractional 12.34M 2 scale/ROUND_HALF_UP)))))

(deftest scalable-implementations-smoke
  (testing "covers non-BigDecimal implementations and their branches"
    (with-clean-thread-rounding-mode
      (fn []
        (binding [scale/*rounding-mode* nil]
          (is (= 123.00M (scale/apply (bigint 123) 2)))
          (is (= 0 (scale/of (bigint 123))))
          (is (= 123M (scale/amount (bigint 123))))
          (is (= 123.00M (scale/apply (bigint 123) 2 scale/ROUND_HALF_UP)))
          (is (= 123.00M (scale/amount (bigint 123) 2)))
          (is (= 123.00M (scale/amount (bigint 123) 2 scale/ROUND_HALF_UP)))
          (is (= 123.00M (scale/apply (BigInteger/valueOf 123) 2)))
          (is (= 0 (scale/of (BigInteger/valueOf 123))))
          (is (= 123M (scale/amount (BigInteger/valueOf 123))))
          (is (= 123.00M (scale/apply (BigInteger/valueOf 123) 2 scale/ROUND_HALF_UP)))
          (is (= 123.00M (scale/amount (BigInteger/valueOf 123) 2)))
          (is (= 123.00M (scale/amount (BigInteger/valueOf 123) 2 scale/ROUND_HALF_UP)))
          (is (= 1 (scale/of 1.5)))
          (is (= 1.5M (scale/amount 1.5)))
          (is (= 1.5M (scale/amount 1.5 1)))
          (is (= 2M (scale/amount 1.5 0 scale/ROUND_HALF_UP)))
          (let [d (ex-data-of #(scale/apply 1.5 0))]
            (is (= :scale/apply (:op d)))
            (is (= 0 (:scale d)))
            (is (= 1.5 (:value d)))
            (is (= true (:arithmetic-exception d))))
          (let [d (ex-data-of #(scale/apply 1.5 0 scale/ROUND_UNNECESSARY))]
            (is (= :scale/apply (:op d)))
            (is (= 0 (:scale d)))
            (is (= 1.5 (:value d)))
            (is (= true (:arithmetic-exception d))))
          (is (= 1 (scale/of (float 1.5))))
          (is (= 1.5M (scale/amount (float 1.5))))
          (is (= 1.5M (scale/amount (float 1.5) 1)))
          (is (= 2M (scale/amount (float 1.5) 0 scale/ROUND_HALF_UP)))
          (let [d (ex-data-of #(scale/apply (float 1.5) 0))]
            (is (= :scale/apply (:op d)))
            (is (= 0 (:scale d)))
            (is (= (float 1.5) (:value d)))
            (is (= true (:arithmetic-exception d))))
          (let [d (ex-data-of #(scale/apply (float 1.5) 0 scale/ROUND_UNNECESSARY))]
            (is (= :scale/apply (:op d)))
            (is (= 0 (:scale d)))
            (is (= (float 1.5) (:value d)))
            (is (= true (:arithmetic-exception d))))
          (is (= 0 (scale/of 42)))
          (is (= 42.00M (scale/apply 42 2)))
          (is (= 2 (scale/of 42.00M)))
          (is (= 42M (scale/amount 42)))
          (is (= 42.00M (scale/amount 42 2)))
          (is (= 42.00M (scale/amount 42 2 scale/ROUND_UNNECESSARY)))
          (is (= 1 (scale/of 1/2)))
          (is (= 0.50M (scale/apply 1/2 2)))
          (is (instance? BigDecimal (scale/amount 1/2)))
          (is (= 0.50M (scale/amount 1/2 2)))
          (is (= 0.50M (scale/amount 1/2 2 scale/ROUND_HALF_UP)))
          (let [d (ex-data-of #(scale/apply 1/3 2))]
            (is (= :scale/apply (:op d)))
            (is (= 2 (:scale d)))
            (is (= true (:arithmetic-exception d)))
            (is (= RoundingMode/UNNECESSARY (:rounding-mode d)))
            (is (instance? BigDecimal (:numerator d)))
            (is (instance? BigDecimal (:denominator d))))
          (let [d (ex-data-of #(scale/apply 1/3 2 scale/ROUND_UNNECESSARY))]
            (is (= :scale/apply (:op d)))
            (is (= 2 (:scale d)))
            (is (= true (:arithmetic-exception d)))
            (is (= RoundingMode/UNNECESSARY (:rounding-mode d)))
            (is (instance? BigDecimal (:numerator d)))
            (is (instance? BigDecimal (:denominator d))))
          (is (= 1 (scale/of "12.3")))
          (is (= 12.30M (scale/apply "12.3" 2)))
          (is (= 12.30M (scale/apply "12.3" 2 scale/ROUND_HALF_UP)))
          (is (= 12.30M (scale/amount "12.3" 2)))
          (is (= 12.30M (scale/amount "12.3" 2 scale/ROUND_HALF_UP)))
          (let [d (ex-data-of #(scale/apply "12.3" 0))]
            (is (= :scale/apply (:op d)))
            (is (= "12.3" (:value d)))
            (is (= 0 (:scale d)))
            (is (= true (:arithmetic-exception d))))
          (let [d (ex-data-of #(scale/apply "12.3" 0 scale/ROUND_UNNECESSARY))]
            (is (= :scale/apply (:op d)))
            (is (= "12.3" (:value d)))
            (is (= 0 (:scale d)))
            (is (= true (:arithmetic-exception d)))))
        (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
          (is (= 123.00M (scale/apply (bigint 123) 2)))
          (is (= 123.00M (scale/apply (BigInteger/valueOf 123) 2)))
          (is (= 42.00M (scale/apply 42 2)))
          (is (= 12.30M (scale/apply "12.3" 2)))
          (is (= 2M (scale/apply 1.5 0)))
          (is (= 2M (scale/apply (float 1.5) 0)))
          (is (instance? BigDecimal (scale/apply 1/3)))
          (is (= 0.33M (scale/apply 1/3 2)))
          (is (= 0.33M (scale/apply 1/3 2 scale/ROUND_HALF_UP))))))))

(ctest/defspec apply-idempotent-on-bigdecimals
  200
  (prop/for-all [n   (tgen/large-integer* {:min -1000000 :max 1000000})
                 sc  (tgen/choose 0 8)
                 rm  (tgen/elements [scale/ROUND_HALF_UP
                                     scale/ROUND_HALF_DOWN
                                     scale/ROUND_HALF_EVEN
                                     scale/ROUND_DOWN
                                     scale/ROUND_UP])]
    (let [^BigDecimal bd1 (scale/apply n sc ^RoundingMode rm)
          ^BigDecimal bd2 (scale/apply bd1 sc ^RoundingMode rm)]
      (and (== sc (.scale bd1))
           (identical? bd1 bd2)))))

(ctest/defspec apply-upscale-preserves-value
  200
  (prop/for-all [n  (tgen/large-integer* {:min -1000000 :max 1000000})
                 s1 (tgen/choose 0 6)
                 s2 (tgen/choose 0 6)]
    (let [s-low  (min s1 s2)
          s-high (max s1 s2)
          ^BigDecimal a (scale/apply n s-low  scale/ROUND_UNNECESSARY)
          ^BigDecimal b (scale/apply n s-high scale/ROUND_UNNECESSARY)]
      (bd= a b))))

(ctest/defspec integer-fractional-recompose
  200
  (prop/for-all [unscaled (tgen/large-integer* {:min -1000000 :max 1000000})
                 sc       (tgen/choose 0 8)]
    (let [^BigDecimal n (.movePointLeft (BigDecimal/valueOf (long unscaled)) (int sc))
          ^BigDecimal i (scale/integer n)
          ^BigDecimal f (scale/fractional n)
          ^BigDecimal rr (.add i (.movePointLeft f (int sc)))]
      (bd= (scale/amount n) rr))))

(ctest/defspec auto?-false-for-bigdecimals
  200
  (prop/for-all [n  (tgen/large-integer* {:min -1000000 :max 1000000})
                 sc (tgen/choose 0 8)]
    (let [^BigDecimal bd (.setScale (BigDecimal. (str n)) (int sc))]
      (false? (scale/auto? bd)))))

(ctest/defspec auto?-matches-currency-scale
  200
  (prop/for-all [sc (tgen/one-of [(tgen/return scale/auto-scaled)
                                  (tgen/choose 0 8)])]
    (let [cur (currency/new :TST 999 sc :iso/fiat :ISO-4217 0)]
      (= (scale/auto-scaled? sc) (scale/auto? cur)))))

(deftest monetary-scale-auto-scaled-non-bigdecimal
  (testing "monetary-scale applies conversion when auto-scaled and input is not BigDecimal"
    (let [sc (long scale/auto-scaled)]
      (is (= 12M (scale/monetary-scale "12" sc)))
      (is (= 12M (scale/monetary-scale 12 sc)))
      (is (= 12M (scale/monetary-scale "12" sc java.math.RoundingMode/HALF_UP)))
      (let [^java.math.BigDecimal bd (bigdec "12.00")]
        (is (identical? bd (scale/monetary-scale bd sc)))))))

(deftest auto?-uses-scale-of
  (testing "auto? derives scale via of"
    (let [auto-cur (currency/new :XAU 959 scale/auto-scaled :iso/metal :ISO-4217 0)
          nom-cur  (currency/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
      (is (true? (scale/auto? auto-cur)))
      (is (false? (scale/auto? nom-cur))))
    (is (false? (scale/auto? 12.3M)))
    (is (false? (scale/auto? 12)))
    (is (false? (scale/auto? "12.3")))
    (is (nil? (scale/auto? nil)))))
