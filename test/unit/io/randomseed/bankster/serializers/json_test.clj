(ns

    ^{:doc    "bankster library, JSON serializers tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.serializers.json-test

  (:require [clojure.test                            :refer [deftest testing is]]
            [clojure.java.io                        :as io]
            [io.randomseed.bankster                 :as bankster]
            [io.randomseed.bankster.currency        :as c]
            [io.randomseed.bankster.money           :as m]
            [io.randomseed.bankster.registry        :as registry]
            [io.randomseed.bankster.scale           :as scale]
            [io.randomseed.bankster.serializers.json :as sj])

  (:import (io.randomseed.bankster Currency Money)
           (java.math BigDecimal RoundingMode)))

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

(deftest cheshire-integration-available
  (testing "register-cheshire-codecs! succeeds when Cheshire is on classpath"
    (is (= true (sj/register-cheshire-codecs!)))))

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
    ;; String rounding modes are now accepted via scale/post-parse-rounding
    (let [m (sj/json-map->money {:currency "PLN" :amount "1.005" :rounding-mode "HALF_UP"})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
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
  (testing "scale/monetary-scale covers auto-scaled and non-BigDecimal branches"
    ;; monetary-scale now lives in scale namespace (extracted from serializers)
    (is (= 12M (scale/monetary-scale 12 -1)))
    (is (= 12.00M (scale/monetary-scale 12 2)))
    (is (= 1.23M (scale/monetary-scale 1.234 2 java.math.RoundingMode/DOWN)))
    (is (= 1M (scale/monetary-scale 1M -1 java.math.RoundingMode/HALF_UP)))
    (is (= 1M (scale/monetary-scale 1 -1 java.math.RoundingMode/HALF_UP))))
  (testing "private make-money uses currency/unit for lookup"
    (let [make-money (var-get #'sj/make-money)
          reg        (registry/get)]
      ;; make-money now requires registry as 3rd arg (and optional 4th for rounding-mode)
      (let [m1 (make-money :PLN 12 reg)]
        (is (= :PLN (c/id m1)))
        (is (= "12.00" (.toPlainString ^BigDecimal (m/amount m1)))))
      (let [m2 (make-money :PLN 12 reg java.math.RoundingMode/HALF_UP)]
        (is (= :PLN (c/id m2)))
        (is (= "12.00" (.toPlainString ^BigDecimal (m/amount m2)))))))
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
  (testing "scale/monetary-scale uses dynamic rounding-mode when present"
    ;; monetary-scale now lives in scale namespace
    (binding [io.randomseed.bankster.scale/*rounding-mode* java.math.RoundingMode/HALF_UP]
      (is (= 1.24M (scale/monetary-scale 1.235 2)))))
  (testing "scale/monetary-scale arity-3 falls back to UNNECESSARY when rm is nil"
    ;; monetary-scale now lives in scale namespace
    (is (thrown? ArithmeticException (scale/monetary-scale 1.235 2 nil))))
  (testing "amount-first split rejects invalid numeric token even when currency exists"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cannot split money string into currency and amount"
         (sj/json-string->money "+.PLN")))
    (is (= "12.30"
           (.toPlainString ^BigDecimal (m/amount (sj/json-string->money "+12.30PLN")))))))

;;
;; Protocol tests.
;;

(deftest json-serializable-protocol-money
  (testing "Money implements JsonSerializable via to-json-map"
    (let [m (m/of :PLN 12.30M)]
      (is (= {:currency "PLN" :amount "12.30"} (sj/to-json-map m)))
      (is (= {:currency "PLN" :amount "12.30"} (sj/to-json-map m nil)))
      (is (= {:currency "PLN" :amount "12.30"} (sj/to-json-map m {})))))
  (testing "Money implements JsonSerializable via to-json-full-map with opts"
    (let [m (m/of :PLN 12.30M)]
      (is (map? (sj/to-json-full-map m {})))))
  (testing "Money implements JsonSerializable via to-json-string"
    (let [m (m/of :PLN 12.30M)]
      (is (= "12.30 PLN" (sj/to-json-string m)))
      (is (= "12.30 PLN" (sj/to-json-string m nil)))
      (is (= "12.30 PLN" (sj/to-json-string m {}))))))

(deftest json-serializable-protocol-currency
  (testing "Currency implements JsonSerializable via to-json-map (minimal)"
    (let [c (c/of :PLN)]
      (is (map? (sj/to-json-map c)))
      (is (= "PLN" (:id (sj/to-json-map c))))
      ;; Minimal map has only :id
      (is (nil? (:numeric (sj/to-json-map c))))
      (is (nil? (:scale (sj/to-json-map c))))))
  (testing "Currency implements JsonSerializable via to-json-full-map"
    (let [c (c/of :PLN)]
      (is (map? (sj/to-json-full-map c)))
      (is (map? (sj/to-json-full-map c {})))
      (is (= "PLN" (:id (sj/to-json-full-map c))))
      (is (= 985 (:numeric (sj/to-json-full-map c))))
      (is (= 2 (:scale (sj/to-json-full-map c))))
      (is (= "iso/fiat" (:kind (sj/to-json-full-map c))))
      (is (= "ISO-4217" (:domain (sj/to-json-full-map c))))))
  (testing "Currency implements JsonSerializable via to-json-string"
    (let [c (c/of :PLN)]
      (is (= "PLN" (sj/to-json-string c)))
      (is (= "PLN" (sj/to-json-string c {}))))))

(deftest json-deserializable-protocol
  (testing "Class implements JsonDeserializable via from-json-map for Money"
    (let [m (sj/from-json-map Money {:currency "PLN" :amount "12.30"})]
      (is (instance? Money m))
      (is (= :PLN (c/id m)))
      (is (= "12.30" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "Class implements JsonDeserializable via from-json-map for Currency"
    (let [c (sj/from-json-map Currency {:id "PLN"})]
      (is (instance? Currency c))
      (is (= :PLN (.id ^Currency c)))))
  (testing "Class implements JsonDeserializable via from-json-string for Money"
    (let [m (sj/from-json-string Money "12.30 PLN")]
      (is (instance? Money m))
      (is (= :PLN (c/id m)))))
  (testing "Class implements JsonDeserializable via from-json-string for Currency"
    (let [c (sj/from-json-string Currency "PLN")]
      (is (instance? Currency c))
      (is (= :PLN (.id ^Currency c)))))
  (testing "from-json-map throws for unsupported type"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported type token"
         (sj/from-json-map String {:currency "PLN" :amount "1.00"}))))
  (testing "from-json-string throws for unsupported type"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported type token"
         (sj/from-json-string String "12.30 PLN")))))

;;
;; :code-only? option tests.
;;

(deftest code-only-option-money
  (testing "money->json-map with :code-only? omits namespace"
    (let [m (m/of :crypto/ETH 1.5M)]
      (is (= "crypto/ETH" (:currency (sj/money->json-map m))))
      (is (= "ETH" (:currency (sj/money->json-map m {:code-only? true}))))))
  (testing "money->json-string with :code-only? omits namespace"
    ;; ETH has scale 18, so 1.5M becomes 1.500000000000000000
    (let [m (m/of :crypto/ETH 1.5M)]
      (is (= "1.500000000000000000 crypto/ETH" (sj/money->json-string m)))
      (is (= "1.500000000000000000 ETH" (sj/money->json-string m {:code-only? true}))))))

(deftest code-only-option-currency
  (testing "currency->json-map with :code-only? omits namespace"
    (let [c (c/of :crypto/ETH)]
      (is (= "crypto/ETH" (:id (sj/currency->json-map c))))
      (is (= "ETH" (:id (sj/currency->json-map c {:code-only? true}))))))
  (testing "currency->json-string with :code-only? omits namespace"
    (let [c (c/of :crypto/ETH)]
      (is (= "crypto/ETH" (sj/currency->json-string c)))
      (is (= "ETH" (sj/currency->json-string c {:code-only? true}))))))

;;
;; :registry option tests.
;;

(deftest registry-option-deserialization
  (testing "json-map->money with :registry option uses custom registry"
    (let [custom-reg (registry/new-registry)
          custom-reg (c/register custom-reg (c/of {:id :MYCUR :scale 4}))
          m          (sj/json-map->money {:currency "MYCUR" :amount "1.2345"}
                                         {:registry custom-reg})]
      (is (= :MYCUR (c/id m)))
      (is (= "1.2345" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "json-string->money with :registry option uses custom registry"
    (let [custom-reg (registry/new-registry)
          custom-reg (c/register custom-reg (c/of {:id :MYCUR :scale 4}))
          m          (sj/json-string->money "1.2345 MYCUR" {:registry custom-reg})]
      (is (= :MYCUR (c/id m))))))

;;
;; :rounding-mode option tests.
;;

(deftest rounding-mode-option-deserialization
  (testing "json-map->money with :rounding-mode in opts overrides map's rounding-mode"
    (let [m (sj/json-map->money {:currency "PLN" :amount "1.005"}
                                {:rounding-mode RoundingMode/HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "json-string->money with :rounding-mode rounds appropriately"
    (let [m (sj/json-string->money "1.005 PLN" {:rounding-mode RoundingMode/HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m)))))))

;;
;; Currency serialization tests.
;;

(deftest currency-json-map-roundtrip
  (testing "currency <-> JSON map roundtrip (minimal)"
    (let [c  (c/of :PLN)
          jm (sj/currency->json-map c)]
      (is (= "PLN" (:id jm)))
      ;; Minimal map has only :id
      (is (nil? (:numeric jm)))
      (is (nil? (:scale jm)))
      (let [c2 (sj/json-map->currency jm)]
        (is (= :PLN (.id ^Currency c2))))))
  (testing "currency <-> JSON full-map roundtrip"
    (let [c  (c/of :PLN)
          jm (sj/currency->json-full-map c)]
      (is (= "PLN" (:id jm)))
      (is (= 985 (:numeric jm)))
      (is (= 2 (:scale jm)))
      (is (= "iso/fiat" (:kind jm)))
      (is (= "ISO-4217" (:domain jm)))
      (let [c2 (sj/json-map->currency jm)]
        (is (= :PLN (.id ^Currency c2)))))))

(deftest currency-json-string-roundtrip
  (testing "currency <-> JSON string roundtrip"
    (let [c  (c/of :PLN)
          js (sj/currency->json-string c)]
      (is (= "PLN" js))
      (let [c2 (sj/json-string->currency js)]
        (is (= :PLN (.id ^Currency c2)))))))

(deftest currency-json-errors
  (testing "json-map->currency validates input"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a map"
         (sj/json-map->currency "PLN")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing :id"
         (sj/json-map->currency {:numeric 985})))))

;;
;; money-codec with new options.
;;

(deftest money-codec-new-options
  (testing "money-codec with :code-only? passes to encoder"
    (let [{:keys [encode]} (sj/money-codec {:code-only? true})
          m (m/of :crypto/ETH 1.5M)]
      (is (= "ETH" (:currency (encode m))))))
  (testing "money-codec with :registry and :rounding-mode passes to decoder"
    (let [custom-reg (registry/new-registry)
          custom-reg (c/register custom-reg (c/of {:id :MYCUR :scale 2}))
          {:keys [decode]} (sj/money-codec {:registry custom-reg
                                            :rounding-mode RoundingMode/HALF_UP})
          m (decode {:currency "MYCUR" :amount "1.005"})]
      (is (= :MYCUR (c/id m)))
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m)))))))

(deftest money-codec-string-with-opts-coverage
  (testing "money-codec :string uses enc/dec opts when provided"
    (let [custom-reg (registry/new-registry)
          custom-reg (c/register custom-reg (c/of {:id :MYCUR :scale 2}))
          {:keys [encode decode representation]} (sj/money-codec {:representation :string
                                                                  :code-only? true
                                                                  :registry custom-reg
                                                                  :rounding-mode RoundingMode/HALF_UP})
          money (m/of :crypto/ETH 1.5M)]
      (is (= :string representation))
      (is (= "1.500000000000000000 ETH" (encode money)))
      (let [m1 (decode "1.005 MYCUR")]
        (is (= :MYCUR (c/id m1)))
        (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m1))))))))

;;
;; Cheshire integration with :code-only?.
;;

(deftest cheshire-encoder-with-code-only
  (testing "register-cheshire-codecs! with :code-only? affects output"
    ;; ETH has scale 18
    (let [cls         (ensure-dummy-json-generator-class!)
          encoders    (atom {})
          add-encoder (fn [cls f] (swap! encoders assoc cls f))
          money       (m/of :crypto/ETH 1.5M)
          jg          (.newInstance ^Class cls)]
      (is (= true (sj/register-cheshire-codecs! {:representation :map
                                                 :code-only? true
                                                 :add-encoder add-encoder})))
      ((get @encoders io.randomseed.bankster.Money) money jg)
      (is (= ["writeStartObject"
              "writeStringField:currency=ETH"
              "writeStringField:amount=1.500000000000000000"
              "writeEndObject"]
             (vec (.calls jg)))))))

;;
;; Full map serialization tests.
;;

(deftest money-json-full-map
  (testing "money->json-full-map returns currency as nested map"
    (let [money (m/of :PLN 12.30M)
          jm (sj/money->json-full-map money)]
      (is (map? (:currency jm)))
      (is (= "PLN" (get-in jm [:currency :id])))
      (is (= 985 (get-in jm [:currency :numeric])))
      (is (= 2 (get-in jm [:currency :scale])))
      (is (= "12.30" (:amount jm)))))
  (testing "money->json-full-map with :code-only? affects currency id"
    (let [money (m/of :crypto/ETH 1.5M)
          jm (sj/money->json-full-map money {:code-only? true})]
      (is (= "ETH" (get-in jm [:currency :id])))))
  (testing "money->json-full-map via protocol"
    (let [money (m/of :PLN 12.30M)
          jm (sj/to-json-full-map money)]
      (is (map? (:currency jm)))
      (is (= "PLN" (get-in jm [:currency :id]))))))

(deftest full-option-delegates-to-full-map
  (testing "currency->json-map with :full? true returns full map"
    (let [c (c/of :PLN)
          jm (sj/currency->json-map c {:full? true})]
      (is (= "PLN" (:id jm)))
      (is (= 985 (:numeric jm)))
      (is (= 2 (:scale jm)))))
  (testing "money->json-map with :full? true returns full map"
    (let [money (m/of :PLN 12.30M)
          jm (sj/money->json-map money {:full? true})]
      (is (map? (:currency jm)))
      (is (= "PLN" (get-in jm [:currency :id])))))
  (testing "to-json-map with :full? true via protocol"
    (let [c (c/of :PLN)
          jm (sj/to-json-map c {:full? true})]
      (is (= 985 (:numeric jm))))))

(deftest keys-filtering-currency
  (testing "currency->json-full-map with :keys filters output"
    (let [c (c/of :PLN)
          jm (sj/currency->json-full-map c {:keys [:id :numeric]})]
      (is (= "PLN" (:id jm)))
      (is (= 985 (:numeric jm)))
      (is (nil? (:scale jm)))
      (is (nil? (:domain jm)))))
  (testing "currency->json-full-map with empty :keys returns empty map"
    (let [c (c/of :PLN)
          jm (sj/currency->json-full-map c {:keys []})]
      (is (= {} jm)))))

(deftest currency-json-full-map-kind-without-namespace
  (testing "currency->json-full-map renders non-namespaced kind with name"
    (let [c  (c/of {:id :ZZZ :numeric 999 :scale 2 :kind :fiat :domain :ISO-4217})
          jm (sj/currency->json-full-map c)]
      (is (= "fiat" (:kind jm))))))

(deftest keys-filtering-money
  (testing "money->json-full-map with :keys filters output"
    (let [money (m/of :PLN 12.30M)
          jm (sj/money->json-full-map money {:keys [:amount]})]
      (is (= "12.30" (:amount jm)))
      (is (nil? (:currency jm)))))
  (testing "money->json-full-map ignores missing keys"
    (let [money (m/of :PLN 12.30M)
          jm (sj/money->json-full-map money {:keys [:amount :missing]})]
      (is (= "12.30" (:amount jm)))
      (is (nil? (:missing jm)))))
  (testing "money->json-full-map with empty :keys returns empty map"
    (let [money (m/of :PLN 12.30M)
          jm (sj/money->json-full-map money {:keys []})]
      (is (= {} jm))))
  (testing "money->json-full-map with nested :keys for currency"
    (let [money (m/of :PLN 12.30M)
          jm (sj/money->json-full-map money {:keys [:amount {:currency {:keys [:id :numeric]}}]})]
      (is (= "12.30" (:amount jm)))
      (is (map? (:currency jm)))
      (is (= "PLN" (get-in jm [:currency :id])))
      (is (= 985 (get-in jm [:currency :numeric])))
      (is (nil? (get-in jm [:currency :scale]))))))

;;
;; Real Cheshire integration tests.
;;

(deftest cheshire-real-encode-money
  (testing "Money encodes to JSON via Cheshire with map representation"
    (require 'cheshire.core)
    (let [generate-string (resolve 'cheshire.core/generate-string)]
      ;; Register encoder with map representation
      (sj/register-cheshire-codecs! {:representation :map})
      (let [money (m/of :PLN 12.30M)
            json  (generate-string money)]
        (is (string? json))
        (is (re-find #"\"currency\"" json))
        (is (re-find #"\"PLN\"" json))
        (is (re-find #"\"amount\"" json))
        (is (re-find #"\"12\.30\"" json)))))
  (testing "Money encodes to JSON via Cheshire with string representation"
    (require 'cheshire.core)
    (let [generate-string (resolve 'cheshire.core/generate-string)]
      ;; Register encoder with string representation
      (sj/register-cheshire-codecs! {:representation :string})
      (let [money (m/of :EUR 99.99M)
            json  (generate-string money)]
        (is (string? json))
        (is (= "\"99.99 EUR\"" json)))))
  (testing "Money encodes with :code-only? option"
    (require 'cheshire.core)
    (let [generate-string (resolve 'cheshire.core/generate-string)]
      ;; Register encoder with code-only
      (sj/register-cheshire-codecs! {:representation :map :code-only? true})
      (let [money (m/of :crypto/ETH 1.5M)
            json  (generate-string money)]
        (is (string? json))
        (is (re-find #"\"ETH\"" json))
        (is (not (re-find #"\"crypto/ETH\"" json)))))))

(deftest cheshire-real-decode-money
  (testing "JSON decodes to Money via json-map->money"
    (require 'cheshire.core)
    (let [parse-string (resolve 'cheshire.core/parse-string)]
      (let [json  "{\"currency\":\"PLN\",\"amount\":\"12.30\"}"
            m     (parse-string json true)
            money (sj/json-map->money m)]
        (is (instance? Money money))
        (is (= :PLN (c/id money)))
        (is (= "12.30" (.toPlainString ^BigDecimal (m/amount money)))))))
  (testing "JSON decodes to Money via json-string->money"
    (require 'cheshire.core)
    (let [parse-string (resolve 'cheshire.core/parse-string)]
      ;; String representation
      (let [json  "\"12.30 PLN\""
            s     (parse-string json)
            money (sj/json-string->money s)]
        (is (instance? Money money))
        (is (= :PLN (c/id money)))))))

(deftest json-text-helpers
  (testing "json-text->money parses JSON object (map representation)"
    (require 'cheshire.core)
    (let [json  "{\"currency\":\"PLN\",\"amount\":12.30}"
          money (sj/json-text->money json)]
      (is (instance? Money money))
      (is (= :PLN (c/id money)))
      (is (= "12.30" (.toPlainString ^BigDecimal (m/amount money))))))
  (testing "json-text->money parses JSON string representation"
    (require 'cheshire.core)
    (let [json  "\"12.30 PLN\""
          money (sj/json-text->money json {:representation :string})]
      (is (instance? Money money))
      (is (= :PLN (c/id money)))))
  (testing "json-text->money with :map uses parsed map value"
    (let [money (sj/json-text->money "{}" {:representation :map
                                           :parse-fn (fn [_] {:currency "PLN" :amount "12.30"})})]
      (is (= :PLN (c/id money)))))
  (testing "json-text->currency parses JSON object (map representation)"
    (require 'cheshire.core)
    (let [json     "{\"id\":\"PLN\"}"
          currency (sj/json-text->currency json)]
      (is (instance? Currency currency))
      (is (= :PLN (c/id currency)))))
  (testing "json-text->currency with :map/:string uses parsed value"
    (let [cur1 (sj/json-text->currency "{}" {:representation :map
                                             :parse-fn (fn [_] {:id "PLN"})})
          cur2 (sj/json-text->currency "\"PLN\"" {:representation :string
                                                  :parse-fn (fn [_] "PLN")})]
      (is (= :PLN (c/id cur1)))
      (is (= :PLN (c/id cur2)))))
  (testing "json-text->money representation mismatch throws"
    (require 'cheshire.core)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"JSON value is not a map"
         (sj/json-text->money "\"12.30 PLN\"" {:representation :map}))))
  (testing "json-text->currency representation mismatch throws"
    (require 'cheshire.core)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"JSON value is not a string"
         (sj/json-text->currency "{\"id\":\"PLN\"}" {:representation :string})))))

(deftest cheshire-real-roundtrip
  (testing "Money roundtrip: encode with Cheshire, decode with json-map->money"
    (require 'cheshire.core)
    (let [generate-string (resolve 'cheshire.core/generate-string)
          parse-string    (resolve 'cheshire.core/parse-string)]
      ;; Reset to map representation
      (sj/register-cheshire-codecs! {:representation :map})
      (let [money1 (m/of :USD 123.45M)
            json   (generate-string money1)
            m      (parse-string json true)
            money2 (sj/json-map->money m)]
        (is (= :USD (c/id money2)))
        (is (= "123.45" (.toPlainString ^BigDecimal (m/amount money2)))))))
  (testing "Crypto money roundtrip preserves scale"
    (require 'cheshire.core)
    (let [generate-string (resolve 'cheshire.core/generate-string)
          parse-string    (resolve 'cheshire.core/parse-string)]
      (sj/register-cheshire-codecs! {:representation :map})
      ;; ETH has scale 18
      (let [money1 (m/of :crypto/ETH 1.5M)
            json   (generate-string money1)
            m      (parse-string json true)
            money2 (sj/json-map->money m)]
        (is (= :crypto/ETH (c/id money2)))
        (is (= "1.500000000000000000" (.toPlainString ^BigDecimal (m/amount money2)))))))
  (testing "Multiple money values in a collection"
    (require 'cheshire.core)
    (let [generate-string (resolve 'cheshire.core/generate-string)
          parse-string    (resolve 'cheshire.core/parse-string)]
      (sj/register-cheshire-codecs! {:representation :map})
      (let [moneys [(m/of :PLN 10) (m/of :EUR 20) (m/of :USD 30)]
            json   (generate-string moneys)
            parsed (parse-string json true)]
        (is (sequential? parsed))
        (is (= 3 (count parsed)))
        (let [m1 (sj/json-map->money (nth parsed 0))
              m2 (sj/json-map->money (nth parsed 1))
              m3 (sj/json-map->money (nth parsed 2))]
          (is (= :PLN (c/id m1)))
          (is (= :EUR (c/id m2)))
          (is (= :USD (c/id m3))))))))

;;
;; Rescale option tests.
;;

(deftest rescale-serialization
  (testing "money->json-map with :rescale upscales amount"
    (let [money (m/of :PLN 12.30M)
          jm    (sj/money->json-map money {:rescale 4})]
      (is (= "12.3000" (:amount jm)))))
  (testing "money->json-map with :rescale downscales with rounding"
    ;; Use crypto/ETH which has scale 18 to create Money with more precision
    (let [money (m/of :crypto/ETH 12.345M)
          ;; Without rounding mode, scale down to 2 throws
          _ (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"ArithmeticException during rescaling"
                 (sj/money->json-map money {:rescale 2})))
          ;; With rounding mode, works
          jm (sj/money->json-map money {:rescale 2 :rounding-mode :HALF_UP})]
      (is (= "12.35" (:amount jm)))))
  (testing "money->json-string with :rescale"
    (let [money (m/of :PLN 12.30M)
          s     (sj/money->json-string money {:rescale 4})]
      (is (= "12.3000 PLN" s))))
  (testing "money->json-full-map with :rescale"
    (let [money (m/of :PLN 12.30M)
          jm    (sj/money->json-full-map money {:rescale 4})]
      (is (= "12.3000" (:amount jm)))))
  (testing ":rescale with keyword rounding mode"
    (let [money (m/of :crypto/ETH 12.345M)
          jm    (sj/money->json-map money {:rescale 2 :rounding-mode :HALF_DOWN})]
      (is (= "12.34" (:amount jm)))))
  (testing ":rescale with string rounding mode"
    (let [money (m/of :crypto/ETH 12.345M)
          jm    (sj/money->json-map money {:rescale 2 :rounding-mode "HALF_UP"})]
      (is (= "12.35" (:amount jm))))))

(deftest rescale-deserialization
  (testing "json-map->money with :rescale creates Money with custom scale"
    (let [m (sj/json-map->money {:currency "PLN" :amount "12.3456"}
                                {:rescale 4})]
      (is (= :PLN (c/id m)))
      (is (= "12.3456" (.toPlainString ^BigDecimal (m/amount m))))
      ;; Currency should have the custom scale
      (is (= 4 (.scale ^io.randomseed.bankster.Currency (c/of m))))))
  (testing "json-map->money with :rescale prevents data loss"
    ;; PLN has scale 2, but we want to preserve 4 decimal places
    (let [m (sj/json-map->money {:currency "PLN" :amount "12.3456"}
                                {:rescale 4})]
      (is (= "12.3456" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "json-map->money with :rescale allows downscaling with rounding"
    (let [m (sj/json-map->money {:currency "PLN" :amount "12.345"}
                                {:rescale 2 :rounding-mode :HALF_UP})]
      (is (= "12.35" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "json-string->money with :rescale"
    (let [m (sj/json-string->money "12.3456 PLN" {:rescale 4})]
      (is (= "12.3456" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing ":rescale with keyword rounding mode in deserialization"
    (let [m (sj/json-map->money {:currency "PLN" :amount "12.345"}
                                {:rescale 2 :rounding-mode :HALF_DOWN})]
      (is (= "12.34" (.toPlainString ^BigDecimal (m/amount m)))))))

(deftest rounding-mode-parsing
  (testing "rounding-mode accepts RoundingMode object"
    (let [m (sj/json-map->money {:currency "PLN" :amount "1.005"}
                                {:rounding-mode RoundingMode/HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "rounding-mode accepts keyword"
    (let [m (sj/json-map->money {:currency "PLN" :amount "1.005"}
                                {:rounding-mode :HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "rounding-mode accepts string"
    (let [m (sj/json-map->money {:currency "PLN" :amount "1.005"}
                                {:rounding-mode "HALF_UP"})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "rounding-mode accepts ROUND_ prefixed string"
    (let [m (sj/json-map->money {:currency "PLN" :amount "1.005"}
                                {:rounding-mode "ROUND_HALF_UP"})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m)))))))

(deftest invalid-rounding-and-rescale-json
  (testing "invalid rounding-mode in opts throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid rounding-mode"
         (sj/json-map->money {:currency "PLN" :amount "1.005"}
                             {:rounding-mode "NOPE"}))))
  (testing "invalid rounding-mode in map throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid rounding-mode"
         (sj/json-map->money {:currency "PLN" :amount "1.005" :rounding-mode "NOPE"}))))
  (testing "invalid rescale value throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid rescale"
         (sj/json-map->money {:currency "PLN" :amount "1.00"}
                             {:rescale -1}))))
  (testing "invalid rescale type throws"
    (let [money (m/of :PLN 1.00M)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid rescale"
           (sj/money->json-map money {:rescale "2"})))))
  (testing "invalid rounding-mode on serialization throws"
    (let [money (m/of :PLN 1.00M)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid rounding-mode"
           (sj/money->json-map money {:rescale 2 :rounding-mode "NOPE"}))))))

(deftest json-text-parsing-branches
  (testing "parse-json-text uses parse-fn when provided"
    (is (= {:ok true} (#'sj/parse-json-text "{}" {:parse-fn (fn [_] {:ok true})}))))
  (testing "parse-json-text throws when no parser is available"
    (with-redefs [clojure.core/requiring-resolve (fn [_] nil)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"No JSON parser available"
           (#'sj/parse-json-text "{}" {})))))
  (testing "parse-json-text uses parse-string and key-fn branches"
    (let [calls    (atom [])
          parse-fn (fn
                     ([s] (swap! calls conj [:parse s]) {:ok true})
                     ([s key-fn] (swap! calls conj [:parse s key-fn]) {:ok true}))]
      (with-redefs [clojure.core/requiring-resolve (fn [sym]
                                                     (cond
                                                       (= sym 'cheshire.core/parse-string) parse-fn
                                                       (= sym 'cheshire.parse/*use-bigdecimals?*) #'clojure.core/*print-length*
                                                       :else nil))]
        (#'sj/parse-json-text "{}" {})
        (#'sj/parse-json-text "{}" {:key-fn keyword})
        (is (= [[:parse "{}"] [:parse "{}" keyword]] @calls))))))
  (testing "parse-json-text uses parse-string without bigdec var"
    (let [calls    (atom [])
          parse-fn (fn
                     ([s] (swap! calls conj [:parse s]) {:ok true})
                     ([s key-fn] (swap! calls conj [:parse s key-fn]) {:ok true}))]
      (with-redefs [clojure.core/requiring-resolve (fn [sym]
                                                     (cond
                                                       (= sym 'cheshire.core/parse-string) parse-fn
                                                       (= sym 'cheshire.parse/*use-bigdecimals?*) nil
                                                       :else nil))]
        (#'sj/parse-json-text "{}" {})
        (is (= [[:parse "{}"]] @calls)))))

(deftest json-text-representation-errors
  (testing "json-text->money requires string input"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a string"
         (sj/json-text->money 12))))
  (testing "json-text->money rejects unsupported JSON value in :auto"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported JSON value"
         (sj/json-text->money "x" {:parse-fn (fn [_] [:bad])}))))
  (testing "json-text->money rejects mismatched representation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"not a map"
         (sj/json-text->money "x" {:representation :map :parse-fn (fn [_] "PLN 1.00")})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"not a string"
         (sj/json-text->money "x" {:representation :string :parse-fn (fn [_] {:currency "PLN" :amount "1.00"})}))))
  (testing "json-text->money rejects invalid representation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid JSON representation"
         (sj/json-text->money "x" {:representation :nope :parse-fn (fn [_] {:currency "PLN" :amount "1.00"})}))))
  (testing "json-text->currency requires string input"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a string"
         (sj/json-text->currency 12))))
  (testing "json-text->currency rejects unsupported JSON value in :auto"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported JSON value"
         (sj/json-text->currency "x" {:parse-fn (fn [_] [:bad])}))))
  (testing "json-text->currency rejects mismatched representation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"not a map"
         (sj/json-text->currency "x" {:representation :map :parse-fn (fn [_] "PLN")})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"not a string"
         (sj/json-text->currency "x" {:representation :string :parse-fn (fn [_] {:id "PLN"})}))))
  (testing "json-text->currency rejects invalid representation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid JSON representation"
         (sj/json-text->currency "x" {:representation :nope :parse-fn (fn [_] {:id "PLN"})})))))

(deftest money-json-full-map-extensions-and-keys
  (let [money (-> (m/of :PLN 12.30M)
                  (assoc :rate 1.23M :tag :alpha :meta {:a 1}))
        jm    (sj/money->json-full-map money)
        jm2   (sj/money->json-full-map money {:keys [:amount {:currency {:keys [:id]}} :rate :tag :meta]})]
    (is (= "1.23" (:rate jm)))
    (is (= "alpha" (:tag jm)))
    (is (= "{:a 1}" (:meta jm)))
    (is (= "PLN" (get-in jm2 [:currency :id])))
    (is (= "12.30" (:amount jm2)))))

(deftest currency-json-full-map-domain-branch
  (let [cur (c/of :crypto/USDT)]
    (is (= "CRYPTO" (:domain (sj/currency->json-full-map cur))))))

(deftest money-codec-encode-decode-branches
  (let [{:keys [encode decode]} (sj/money-codec {:representation :string})
        m0 (m/of :PLN 1.00M)]
    (is (= "1.00 PLN" (encode m0)))
    (is (= :PLN (c/id (decode "1.00 PLN"))))))

(deftest cheshire-codecs-missing
  (with-redefs [clojure.core/requiring-resolve (fn [_] nil)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cheshire is not available"
         (sj/register-cheshire-codecs! {:representation :map})))))
