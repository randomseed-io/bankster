(ns

    ^{:doc    "bankster library, JSON serializers tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.serializers.json-test

  (:require [clojure.test                       :refer [deftest testing is]]
            [clojure.java.io                   :as io]
            [io.randomseed.bankster.currency    :as c]
            [io.randomseed.bankster.money       :as m]
            [io.randomseed.bankster.serializers.json :as sj])

  (:import (java.math BigDecimal)))

(defonce ^:private dummy-json-generator-class* (atom nil))

(defn- ensure-dummy-json-generator-class! ^Class []
  (or @dummy-json-generator-class*
      (let [name   "io.randomseed.bankster.test_support.DummyJsonGenerator"
            pkgdir "io/randomseed/bankster/test_support"
            outdir (io/file "target" "tmp" "dummy-json-generator-classes")
            srcdir (io/file "target" "tmp" "dummy-json-generator-src" pkgdir)
            _      (.mkdirs srcdir)
            _      (.mkdirs outdir)
            java   (io/file srcdir "DummyJsonGenerator.java")
            code   (str
                    "package io.randomseed.bankster.test_support;\n"
                    "import java.util.ArrayList;\n"
                    "import java.util.List;\n"
                    "public final class DummyJsonGenerator {\n"
                    "  private final ArrayList<String> calls = new ArrayList<>();\n"
                    "  public void writeStartObject() { calls.add(\"writeStartObject\"); }\n"
                    "  public void writeStringField(String k, String v) { calls.add(\"writeStringField:\" + k + \"=\" + v); }\n"
                    "  public void writeEndObject() { calls.add(\"writeEndObject\"); }\n"
                    "  public void writeString(String s) { calls.add(\"writeString:\" + s); }\n"
                    "  public List<String> calls() { return calls; }\n"
                    "}\n")]
        (spit java code)
        (let [^javax.tools.JavaCompiler compiler (javax.tools.ToolProvider/getSystemJavaCompiler)]
          (when-not compiler
            (throw
             (ex-info
              "JDK Java compiler is not available (ToolProvider/getSystemJavaCompiler returned nil)."
              {:op :bankster.test/ensure-dummy-json-generator-class})))
          (let [fm    (.getStandardFileManager compiler nil nil nil)
                units (.getJavaFileObjectsFromFiles fm [java])
                opts  ["-d" (.getPath outdir)]
                task  (.getTask compiler nil fm nil opts nil units)
                ok?   (.call task)]
            (.close fm)
            (when-not ok?
              (throw
               (ex-info
                "Failed to compile DummyJsonGenerator."
                {:op :bankster.test/ensure-dummy-json-generator-class
                 :out (.getPath outdir)
                 :src (.getPath java)}))))
          (let [cl (java.net.URLClassLoader.
                    (into-array java.net.URL [(.toURL (.toURI outdir))])
                    (.getContextClassLoader (Thread/currentThread)))
                cls (Class/forName name true cl)]
            (reset! dummy-json-generator-class* cls)
            cls)))))

(deftest money-edn-map
  (testing "money/to-map emits an EDN-friendly shape"
    (let [x (m/of :PLN 12.30M)]
      (is (= (m/to-map x)
             {:currency :PLN
              :amount   12.30M}))
      (is (= (c/to-map x)
             {:currency :PLN
              :amount   12.30M})))))

(deftest money-json-map-roundtrip
  (testing "money <-> JSON map roundtrip preserves scale"
    (let [x (m/of :PLN 12.30M)
          j (m/to-json-map x)]
      (is (= j {:currency "PLN" :amount "12.30"}))
      (let [y (m/from-json-map j)]
        (is (= (c/id y) :PLN))
        (is (instance? BigDecimal (m/amount y)))
        (is (= (.toPlainString ^BigDecimal (m/amount y)) "12.30"))))))

(deftest money-json-string-roundtrip
  (testing "money <-> JSON string roundtrip"
    (let [x (m/of :PLN 12.30M)
          s (m/to-json-string x)]
      (is (= s "12.30 PLN"))
      (let [y (m/from-json-string s)]
        (is (= (c/id y) :PLN))
        (is (= (.toPlainString ^BigDecimal (m/amount y)) "12.30")))))
  (testing "string decoding accepts currency-first and no-whitespace forms"
    (is (= (.toPlainString ^BigDecimal (m/amount (m/from-json-string "PLN 12.30"))) "12.30"))
    (is (= (.toPlainString ^BigDecimal (m/amount (m/from-json-string "PLN12.30"))) "12.30"))
    (is (= (.toPlainString ^BigDecimal (m/amount (m/from-json-string "12.30PLN"))) "12.30"))
    (is (= (.toPlainString ^BigDecimal (m/amount (m/from-json-string "PLN.30"))) "0.30"))
    (is (= (.toPlainString ^BigDecimal (m/amount (m/from-json-string ".30PLN"))) "0.30"))))

(deftest cheshire-integration-is-optional
  (testing "register-cheshire-codecs! throws when Cheshire is absent"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cheshire is not available"
         (sj/register-cheshire-codecs!)))))

(deftest json-errors-and-edge-cases
  (testing "money-codec rejects invalid representation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid JSON representation"
         (sj/money-codec {:representation :nope}))))
  (testing "json-map->money validates input"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a map"
         (sj/json-map->money 123)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing :currency"
         (sj/json-map->money {:amount "1.00"})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing :amount"
         (sj/json-map->money {:currency "PLN"})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"RoundingMode"
         (sj/json-map->money {:currency "PLN" :amount "1.00" :rounding-mode "HALF_UP"}))))
  (testing "amount parsing rejects blank/invalid strings and unsupported representations"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Empty amount string"
         (sj/json-map->money {:currency "PLN" :amount ""})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid amount string"
         (sj/json-map->money {:currency "PLN" :amount "nope"})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported amount representation"
         (sj/json-map->money {:currency "PLN" :amount (Object.)}))))
  (testing "json-string->money validates input and rejects malformed strings"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a string"
         (sj/json-string->money 12)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Empty money string"
         (sj/json-string->money "   ")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"exactly two tokens"
         (sj/json-string->money "1 2 3")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"exactly one numeric token"
         (sj/json-string->money "ABC DEF")))))

(deftest json-splitting-and-rounding
  (testing "json-string->money supports amount-first single-token form"
    (is (= "12.30"
           (.toPlainString ^BigDecimal (m/amount (sj/json-string->money "12.30PLN")))))
    (is (= "-12.30"
           (.toPlainString ^BigDecimal (m/amount (sj/json-string->money "-12.30PLN"))))))
  (testing "json-string->money rejects single token without digits"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must contain digits"
         (sj/json-string->money "PLNABC"))))
  (testing "json-map->money accepts numeric amounts (int/double) and underscores"
    (let [cur (c/of {:id :KIKI :scale 2})]
      (is (= "12.00"
             (.toPlainString
              ^BigDecimal
              (m/amount (sj/json-map->money {:currency cur :amount 12}))))))
    (is (= "12.30"
           (.toPlainString
            ^BigDecimal
            (m/amount (sj/json-map->money {:currency :PLN :amount "12.30"})))))
    (is (= "1234.00"
           (.toPlainString
            ^BigDecimal
            (m/amount (sj/json-map->money {:currency :PLN :amount "1_234.00"})))))))

(deftest cheshire-encoder-via-injection
  (testing "register-cheshire-codecs! can be exercised without Cheshire via :add-encoder injection"
    (let [cls         (ensure-dummy-json-generator-class!)
          encoders    (atom {})
          add-encoder (fn [cls f] (swap! encoders assoc cls f))
          money       (m/of :PLN 12.30M)
          jg          (.newInstance ^Class cls)]
      (is (= true (sj/register-cheshire-codecs! {:representation :map :add-encoder add-encoder})))
      (is (contains? @encoders io.randomseed.bankster.Money))
      ((get @encoders io.randomseed.bankster.Money) money jg)
      (is (= ["writeStartObject"
              "writeStringField:currency=PLN"
              "writeStringField:amount=12.30"
              "writeEndObject"]
             (vec (.calls jg))))))
  (testing "string representation encoder uses writeString"
    (let [cls (ensure-dummy-json-generator-class!)
          encoders (atom {})
          add-encoder (fn [cls f] (swap! encoders assoc cls f))
          money (m/of :PLN 12.30M)
          jg (.newInstance ^Class cls)]
      (is (= true (sj/register-cheshire-codecs! {:representation :string :add-encoder add-encoder})))
      ((get @encoders io.randomseed.bankster.Money) money jg)
      (is (= ["writeString:12.30 PLN"]
             (vec (.calls jg)))))))

(deftest money-codec-covers-both-representations
  (testing "money-codec encodes/decodes as map"
    (let [{:keys [encode decode representation]} (sj/money-codec {:representation :map})
          m0 (m/of :PLN 1.23M)]
      (is (= :map representation))
      (is (= {:currency "PLN" :amount "1.23"} (encode m0)))
      (let [m1 (decode {:currency "PLN" :amount "1.23"})]
        (is (= :PLN (c/id m1)))
        (is (= "1.23" (.toPlainString ^BigDecimal (m/amount m1)))))))
  (testing "money-codec encodes/decodes as string"
    (let [{:keys [encode decode representation]} (sj/money-codec {:representation :string})
          m0 (m/of :PLN 1.23M)]
      (is (= :string representation))
      (is (= "1.23 PLN" (encode m0)))
      (let [m1 (decode "1.23 PLN")]
        (is (= :PLN (c/id m1)))
        (is (= "1.23" (.toPlainString ^BigDecimal (m/amount m1))))))))

(deftest serializers-json-extra-branches
  (testing "money-codec default representation is :map"
    (is (= :map (:representation (sj/money-codec)))))
  (testing "json-map->money accepts explicit java.math.RoundingMode"
    (let [cur (c/of {:id :KIKI :scale 2})
          m1  (sj/json-map->money {:currency cur
                                  :amount "1.005"
                                  :rounding-mode java.math.RoundingMode/HALF_UP})]
      (is (= :KIKI (c/id m1)))
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m1))))))
  (testing "parse-bigdec covers BigDecimal and number branches"
    (let [cur (c/of {:id :KIKI :scale 2})]
      (is (= "1.20"
             (.toPlainString ^BigDecimal
                             (m/amount (sj/json-map->money {:currency cur :amount 1.2M})))))
      (is (= "1.20"
             (.toPlainString ^BigDecimal
                             (m/amount (sj/json-map->money {:currency cur :amount 1.2})))))))
  (testing "private monetary-scale covers auto-scaled and non-BigDecimal branches"
    (let [monetary-scale (var-get #'sj/monetary-scale)]
      (is (= 12M (monetary-scale 12 -1)))
      (is (= 12.00M (monetary-scale 12 2)))
      (is (= 1.23M (monetary-scale 1.234 2 java.math.RoundingMode/DOWN)))
      (is (= 1M (monetary-scale 1M -1 java.math.RoundingMode/HALF_UP)))
      (is (= 1M (monetary-scale 1 -1 java.math.RoundingMode/HALF_UP)))))
  (testing "private make-money can fall back to currency/unit when to-currency is nil"
    (let [make-money (var-get #'sj/make-money)]
      (with-redefs [io.randomseed.bankster.currency/to-currency (fn [_] nil)]
        (let [m1 (make-money :PLN 12)]
          (is (= :PLN (c/id m1)))
          (is (= "12.00" (.toPlainString ^BigDecimal (m/amount m1)))))
        (let [m2 (make-money :PLN 12 java.math.RoundingMode/HALF_UP)]
          (is (= :PLN (c/id m2)))
          (is (= "12.00" (.toPlainString ^BigDecimal (m/amount m2))))))))
  (testing "json-string->money covers split errors"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cannot split money string into currency and amount"
         (sj/json-string->money "12.30")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Money string without whitespace must contain digits"
         (sj/json-string->money "PLN")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cannot split money string into currency and amount"
         (sj/json-string->money "PLN1A")))))

(deftest serializers-json-private-helper-edge-cases
  (testing "parse-bigdec nil branch is explicit"
    (let [parse-bigdec (var-get #'sj/parse-bigdec)]
      (is (nil? (parse-bigdec nil)))))
  (testing "monetary-scale uses dynamic rounding-mode when present"
    (let [monetary-scale (var-get #'sj/monetary-scale)]
      (binding [io.randomseed.bankster.scale/*rounding-mode* java.math.RoundingMode/HALF_UP]
        (is (= 1.24M (monetary-scale 1.235 2))))))
  (testing "monetary-scale arity-3 falls back to UNNECESSARY when rm is nil"
    (let [monetary-scale (var-get #'sj/monetary-scale)]
      (is (thrown? ArithmeticException (monetary-scale 1.235 2 nil)))))
  (testing "amount-first split rejects invalid numeric token even when currency exists"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cannot split money string into currency and amount"
         (sj/json-string->money "+.PLN")))
    (is (= "12.30"
           (.toPlainString ^BigDecimal (m/amount (sj/json-string->money "+12.30PLN")))))))
