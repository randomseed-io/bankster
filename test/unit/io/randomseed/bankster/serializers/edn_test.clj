(ns

    ^{:doc    "Bankster library, EDN serializers tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.serializers.edn-test

  (:require [clojure.test                           :refer [deftest testing is]]
            [clojure.edn                            :as edn]
            [io.randomseed.bankster                 :as bankster]
            [io.randomseed.bankster.currency        :as c]
            [io.randomseed.bankster.money           :as m]
            [io.randomseed.bankster.registry        :as registry]
            [io.randomseed.bankster.serializers.edn :as se])

  (:import (io.randomseed.bankster Currency Money)
           (java.math BigDecimal RoundingMode)))

;;
;; Money EDN map serialization.
;;

(deftest money-edn-map-serialization
  (testing "money->edn-map produces EDN-friendly map with keyword currency"
    (let [m (m/of :PLN 12.30M)
          em (se/money->edn-map m)]
      (is (= :PLN (:currency em)))
      (is (instance? BigDecimal (:amount em)))
      (is (= "12.30" (.toPlainString ^BigDecimal (:amount em))))))
  (testing "money->edn-map with namespaced currency"
    (let [m (m/of :crypto/ETH 1.5M)
          em (se/money->edn-map m)]
      (is (= :crypto/ETH (:currency em)))))
  (testing "money->edn-map with :code-only? omits namespace"
    (let [m (m/of :crypto/ETH 1.5M)
          em (se/money->edn-map m {:code-only? true})]
      (is (= :ETH (:currency em))))))

(deftest money-edn-map-deserialization
  (testing "edn-map->money creates Money from EDN map"
    (let [m (se/edn-map->money {:currency :PLN :amount 12.30M})]
      (is (instance? Money m))
      (is (= :PLN (c/id m)))
      (is (= "12.30" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-map->money accepts string amounts"
    (let [m (se/edn-map->money {:currency :PLN :amount "12.30"})]
      (is (= "12.30" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-map->money validates input"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a map"
         (se/edn-map->money "PLN 12.30")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing :currency"
         (se/edn-map->money {:amount 12.30M})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing :amount"
         (se/edn-map->money {:currency :PLN})))))

(deftest money-edn-map-roundtrip
  (testing "money <-> EDN map roundtrip preserves value"
    (let [m1 (m/of :PLN 12.30M)
          em (se/money->edn-map m1)
          m2 (se/edn-map->money em)]
      (is (= :PLN (c/id m2)))
      (is (= "12.30" (.toPlainString ^BigDecimal (m/amount m2)))))))

;;
;; Money EDN string serialization.
;;

(deftest money-edn-string-serialization
  (testing "money->edn-string produces tagged literal for ISO currency"
    (let [m (m/of :PLN 12.30M)
          es (se/money->edn-string m)]
      (is (string? es))
      (is (= "#money[12.30M PLN]" es))))
  (testing "money->edn-string produces namespaced tagged literal for crypto"
    ;; ETH has scale 18
    (let [m (m/of :crypto/ETH 1.5M)
          es (se/money->edn-string m)]
      (is (= "#money/crypto[1.500000000000000000M ETH]" es))))
  (testing "money->edn-string with :code-only? uses simple tag"
    ;; ETH has scale 18
    (let [m (m/of :crypto/ETH 1.5M)
          es (se/money->edn-string m {:code-only? true})]
      (is (= "#money[1.500000000000000000M ETH]" es)))))

(deftest money-edn-string-deserialization
  (testing "edn-string->money parses vector-form tagged literal"
    (let [m (se/edn-string->money "#money[12.30M PLN]")]
      (is (instance? Money m))
      (is (= :PLN (c/id m)))
      (is (= "12.30" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-string->money parses namespaced tagged literal"
    (let [m (se/edn-string->money "#money/crypto[1.5M ETH]")]
      (is (instance? Money m))
      (is (= :crypto/ETH (c/id m)))))
  (testing "edn-string->money parses map-form tagged literal"
    (let [m (se/edn-string->money "#money{:currency :PLN :amount 12.30M}")]
      (is (instance? Money m))
      (is (= :PLN (c/id m)))))
  (testing "edn-string->money validates input type"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a string"
         (se/edn-string->money 123)))))

(deftest money-edn-string-roundtrip
  (testing "money <-> EDN string roundtrip for ISO currency"
    (let [m1 (m/of :PLN 12.30M)
          es (se/money->edn-string m1)
          m2 (se/edn-string->money es)]
      (is (= :PLN (c/id m2)))
      (is (= "12.30" (.toPlainString ^BigDecimal (m/amount m2))))))
  (testing "money <-> EDN string roundtrip for crypto currency"
    ;; ETH has scale 18
    (let [m1 (m/of :crypto/ETH 1.5M)
          es (se/money->edn-string m1)
          m2 (se/edn-string->money es)]
      (is (= :crypto/ETH (c/id m2)))
      (is (= "1.500000000000000000" (.toPlainString ^BigDecimal (m/amount m2)))))))

;;
;; Currency EDN serialization.
;;

(deftest currency-edn-map-serialization
  (testing "currency->edn-map produces minimal currency info (only :id)"
    (let [c (c/of :PLN)
          em (se/currency->edn-map c)]
      (is (= :PLN (:id em)))
      ;; Minimal map has only :id
      (is (nil? (:numeric em)))
      (is (nil? (:scale em)))
      (is (nil? (:domain em)))))
  (testing "currency->edn-full-map produces full currency info"
    (let [c (c/of :PLN)
          em (se/currency->edn-full-map c)]
      (is (= :PLN (:id em)))
      (is (= 985 (:numeric em)))
      (is (= 2 (:scale em)))
      (is (= :ISO-4217 (:domain em)))
      (is (= :iso/fiat (:kind em)))))
  (testing "currency->edn-map with :code-only? omits namespace"
    (let [c (c/of :crypto/ETH)
          em (se/currency->edn-map c {:code-only? true})]
      (is (= :ETH (:id em)))))
  (testing "currency->edn-full-map with :code-only? omits namespace"
    (let [c (c/of :crypto/ETH)
          em (se/currency->edn-full-map c {:code-only? true})]
      (is (= :ETH (:id em)))
      (is (= 18 (:scale em))))))

(deftest currency-edn-keyword-serialization
  (testing "currency->edn-keyword returns currency ID"
    (let [c (c/of :PLN)]
      (is (= :PLN (se/currency->edn-keyword c)))))
  (testing "currency->edn-keyword with :code-only? omits namespace"
    (let [c (c/of :crypto/ETH)]
      (is (= :crypto/ETH (se/currency->edn-keyword c)))
      (is (= :ETH (se/currency->edn-keyword c {:code-only? true}))))))

(deftest currency-edn-string-serialization
  (testing "currency->edn-string produces tagged literal"
    (let [c (c/of :PLN)
          es (se/currency->edn-string c)]
      (is (= "#currency :PLN" es))))
  (testing "currency->edn-string with namespaced currency"
    (let [c (c/of :crypto/ETH)
          es (se/currency->edn-string c)]
      (is (= "#currency :crypto/ETH" es))))
  (testing "currency->edn-string with :code-only?"
    (let [c (c/of :crypto/ETH)
          es (se/currency->edn-string c {:code-only? true})]
      (is (= "#currency :ETH" es)))))

(deftest currency-edn-map-deserialization
  (testing "edn-map->currency creates Currency from EDN map"
    (let [c (se/edn-map->currency {:id :PLN})]
      (is (instance? Currency c))
      (is (= :PLN (.id ^Currency c)))))
  (testing "edn-map->currency validates input"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a map"
         (se/edn-map->currency :PLN)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing :id"
         (se/edn-map->currency {:numeric 985})))))

(deftest currency-edn-keyword-deserialization
  (testing "edn-keyword->currency creates Currency from keyword"
    (let [c (se/edn-keyword->currency :PLN)]
      (is (instance? Currency c))
      (is (= :PLN (.id ^Currency c))))))

(deftest currency-edn-string-deserialization
  (testing "edn-string->currency parses tagged literal"
    (let [c (se/edn-string->currency "#currency :PLN")]
      (is (instance? Currency c))
      (is (= :PLN (.id ^Currency c)))))
  (testing "edn-string->currency validates input type"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a string"
         (se/edn-string->currency :PLN)))))

(deftest currency-edn-roundtrip
  (testing "currency <-> EDN map roundtrip"
    (let [c1 (c/of :PLN)
          em (se/currency->edn-map c1)
          c2 (se/edn-map->currency em)]
      (is (= :PLN (.id ^Currency c2)))))
  (testing "currency <-> EDN string roundtrip"
    (let [c1 (c/of :PLN)
          es (se/currency->edn-string c1)
          c2 (se/edn-string->currency es)]
      (is (= :PLN (.id ^Currency c2))))))

;;
;; Protocol tests.
;;

(deftest edn-serializable-protocol-money
  (testing "Money implements EdnSerializable via to-edn-map"
    (let [m (m/of :PLN 12.30M)]
      (is (= :PLN (:currency (se/to-edn-map m))))
      (is (instance? BigDecimal (:amount (se/to-edn-map m))))))
  (testing "Money implements EdnSerializable via to-edn-string"
    (let [m (m/of :PLN 12.30M)]
      (is (= "#money[12.30M PLN]" (se/to-edn-string m))))))

(deftest edn-serializable-protocol-currency
  (testing "Currency implements EdnSerializable via to-edn-map"
    (let [c (c/of :PLN)]
      (is (= :PLN (:id (se/to-edn-map c))))))
  (testing "Currency implements EdnSerializable via to-edn-string"
    (let [c (c/of :PLN)]
      (is (= "#currency :PLN" (se/to-edn-string c))))))

(deftest edn-deserializable-protocol
  (testing "Class implements EdnDeserializable via from-edn-map for Money"
    (let [m (se/from-edn-map Money {:currency :PLN :amount 12.30M})]
      (is (instance? Money m))
      (is (= :PLN (c/id m)))))
  (testing "Class implements EdnDeserializable via from-edn-map for Currency"
    (let [c (se/from-edn-map Currency {:id :PLN})]
      (is (instance? Currency c))
      (is (= :PLN (.id ^Currency c)))))
  (testing "Class implements EdnDeserializable via from-edn-string for Money"
    (let [m (se/from-edn-string Money "#money[12.30M PLN]")]
      (is (instance? Money m))
      (is (= :PLN (c/id m)))))
  (testing "Class implements EdnDeserializable via from-edn-string for Currency"
    (let [c (se/from-edn-string Currency "#currency :PLN")]
      (is (instance? Currency c))
      (is (= :PLN (.id ^Currency c)))))
  (testing "from-edn-map throws for unsupported type"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported type token"
         (se/from-edn-map String {:currency :PLN :amount 1.00M}))))
  (testing "from-edn-string throws for unsupported type"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported type token"
         (se/from-edn-string String "#money[12.30M PLN]")))))

;;
;; :registry option tests.
;;

(deftest registry-option-edn-deserialization
  (testing "edn-map->money with :registry option uses custom registry"
    (let [custom-reg (registry/new-registry)
          custom-reg (c/register custom-reg (c/of {:id :MYCUR :scale 4}))
          m          (se/edn-map->money {:currency :MYCUR :amount 1.2345M}
                                        {:registry custom-reg})]
      (is (= :MYCUR (c/id m)))
      (is (= "1.2345" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-string->money with :registry option uses custom registry"
    (let [custom-reg (registry/new-registry)
          custom-reg (c/register custom-reg (c/of {:id :MYCUR :scale 4}))
          m          (se/edn-string->money "#money[1.2345M MYCUR]" {:registry custom-reg})]
      (is (= :MYCUR (c/id m))))))

;;
;; :rounding-mode option tests.
;;

(deftest rounding-mode-option-edn-deserialization
  (testing "edn-map->money with :rounding-mode rounds appropriately"
    (let [m (se/edn-map->money {:currency :PLN :amount 1.005M}
                               {:rounding-mode RoundingMode/HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-map->money accepts keyword rounding-mode in map"
    (let [m (se/edn-map->money {:currency :PLN :amount 1.005M :rounding-mode :HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-string->money with :rounding-mode rounds appropriately"
    (let [m (se/edn-string->money "#money[1.005M PLN]" {:rounding-mode RoundingMode/HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m)))))))

;;
;; money-codec tests.
;;

(deftest edn-money-codec
  (testing "money-codec default representation is :map"
    (is (= :map (:representation (se/money-codec)))))
  (testing "money-codec encodes/decodes as map"
    (let [{:keys [encode decode representation]} (se/money-codec {:representation :map})
          m0 (m/of :PLN 1.23M)]
      (is (= :map representation))
      (is (= :PLN (:currency (encode m0))))
      (is (instance? BigDecimal (:amount (encode m0))))
      (let [m1 (decode {:currency :PLN :amount 1.23M})]
        (is (= :PLN (c/id m1))))))
  (testing "money-codec encodes/decodes as string"
    (let [{:keys [encode decode representation]} (se/money-codec {:representation :string})
          m0 (m/of :PLN 1.23M)]
      (is (= :string representation))
      (is (= "#money[1.23M PLN]" (encode m0)))
      (let [m1 (decode "#money[1.23M PLN]")]
        (is (= :PLN (c/id m1))))))
  (testing "money-codec rejects invalid representation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid EDN representation"
         (se/money-codec {:representation :nope}))))
  (testing "money-codec with :code-only? affects encoder"
    (let [{:keys [encode]} (se/money-codec {:code-only? true})
          m (m/of :crypto/ETH 1.5M)]
      (is (= :ETH (:currency (encode m))))))
  (testing "money-codec with :registry and :rounding-mode affects decoder"
    (let [custom-reg (registry/new-registry)
          custom-reg (c/register custom-reg (c/of {:id :MYCUR :scale 2}))
          {:keys [decode]} (se/money-codec {:registry custom-reg
                                            :rounding-mode RoundingMode/HALF_UP})
          m (decode {:currency :MYCUR :amount 1.005M})]
      (is (= :MYCUR (c/id m)))
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m)))))))

;;
;; Edge cases and error handling.
;;

(deftest edn-parse-bigdec-edge-cases
  (testing "private parse-bigdec handles nil"
    (let [parse-bigdec (var-get #'se/parse-bigdec)]
      (is (nil? (parse-bigdec nil)))))
  (testing "private parse-bigdec handles BigDecimal"
    (let [parse-bigdec (var-get #'se/parse-bigdec)]
      (is (= 12.30M (parse-bigdec 12.30M)))))
  (testing "private parse-bigdec handles integer"
    (let [parse-bigdec (var-get #'se/parse-bigdec)]
      (is (= 12M (parse-bigdec 12)))))
  (testing "private parse-bigdec handles double"
    (let [parse-bigdec (var-get #'se/parse-bigdec)]
      (is (instance? BigDecimal (parse-bigdec 12.3)))))
  (testing "private parse-bigdec rejects empty string"
    (let [parse-bigdec (var-get #'se/parse-bigdec)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Empty amount string"
           (parse-bigdec "  ")))))
  (testing "private parse-bigdec rejects invalid string"
    (let [parse-bigdec (var-get #'se/parse-bigdec)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid amount string"
           (parse-bigdec "abc")))))
  (testing "private parse-bigdec rejects unsupported type"
    (let [parse-bigdec (var-get #'se/parse-bigdec)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Unsupported amount representation"
           (parse-bigdec (Object.)))))))

(deftest edn-nil-handling
  (testing "money->edn-map returns nil for nil input"
    (is (nil? (se/money->edn-map nil))))
  (testing "money->edn-string returns nil for nil input"
    (is (nil? (se/money->edn-string nil))))
  (testing "edn-map->money returns nil for nil input"
    (is (nil? (se/edn-map->money nil))))
  (testing "edn-string->money returns nil for nil input"
    (is (nil? (se/edn-string->money nil))))
  (testing "currency->edn-map returns nil for nil input"
    (is (nil? (se/currency->edn-map nil))))
  (testing "currency->edn-string returns nil for nil input"
    (is (nil? (se/currency->edn-string nil))))
  (testing "edn-map->currency returns nil for nil input"
    (is (nil? (se/edn-map->currency nil))))
  (testing "edn-string->currency returns nil for nil input"
    (is (nil? (se/edn-string->currency nil))))
  (testing "edn-keyword->currency returns nil for nil input"
    (is (nil? (se/edn-keyword->currency nil)))))

;;
;; Full map serialization tests.
;;

(deftest money-edn-full-map
  (testing "money->edn-full-map returns currency as nested map"
    (let [money (m/of :PLN 12.30M)
          em (se/money->edn-full-map money)]
      (is (map? (:currency em)))
      (is (= :PLN (get-in em [:currency :id])))
      (is (= 985 (get-in em [:currency :numeric])))
      (is (= 2 (get-in em [:currency :scale])))
      (is (= 12.30M (:amount em)))))
  (testing "money->edn-full-map with :code-only? affects currency id"
    (let [money (m/of :crypto/ETH 1.5M)
          em (se/money->edn-full-map money {:code-only? true})]
      (is (= :ETH (get-in em [:currency :id])))))
  (testing "money->edn-full-map via protocol"
    (let [money (m/of :PLN 12.30M)
          em (se/to-edn-full-map money)]
      (is (map? (:currency em)))
      (is (= :PLN (get-in em [:currency :id]))))))

(deftest full-option-delegates-to-full-map-edn
  (testing "currency->edn-map with :full? true returns full map"
    (let [c (c/of :PLN)
          em (se/currency->edn-map c {:full? true})]
      (is (= :PLN (:id em)))
      (is (= 985 (:numeric em)))
      (is (= 2 (:scale em)))))
  (testing "money->edn-map with :full? true returns full map"
    (let [money (m/of :PLN 12.30M)
          em (se/money->edn-map money {:full? true})]
      (is (map? (:currency em)))
      (is (= :PLN (get-in em [:currency :id])))))
  (testing "to-edn-map with :full? true via protocol"
    (let [c (c/of :PLN)
          em (se/to-edn-map c {:full? true})]
      (is (= 985 (:numeric em))))))

(deftest keys-filtering-currency-edn
  (testing "currency->edn-full-map with :keys filters output"
    (let [c (c/of :PLN)
          em (se/currency->edn-full-map c {:keys [:id :numeric]})]
      (is (= :PLN (:id em)))
      (is (= 985 (:numeric em)))
      (is (nil? (:scale em)))
      (is (nil? (:domain em)))))
  (testing "currency->edn-full-map with empty :keys returns empty map"
    (let [c (c/of :PLN)
          em (se/currency->edn-full-map c {:keys []})]
      (is (= {} em)))))

(deftest keys-filtering-money-edn
  (testing "money->edn-full-map with :keys filters output"
    (let [money (m/of :PLN 12.30M)
          em (se/money->edn-full-map money {:keys [:amount]})]
      (is (= 12.30M (:amount em)))
      (is (nil? (:currency em)))))
  (testing "money->edn-full-map with nested :keys for currency"
    (let [money (m/of :PLN 12.30M)
          em (se/money->edn-full-map money {:keys [:amount {:currency {:keys [:id :numeric]}}]})]
      (is (= 12.30M (:amount em)))
      (is (map? (:currency em)))
      (is (= :PLN (get-in em [:currency :id])))
      (is (= 985 (get-in em [:currency :numeric])))
      (is (nil? (get-in em [:currency :scale]))))))

;;
;; Rescale option tests.
;;

(deftest rescale-serialization-edn
  (testing "money->edn-map with :rescale upscales amount"
    (let [money (m/of :PLN 12.30M)
          em    (se/money->edn-map money {:rescale 4})]
      (is (= "12.3000" (.toPlainString ^BigDecimal (:amount em))))))
  (testing "money->edn-map with :rescale downscales with rounding"
    ;; Use crypto/ETH which has scale 18 to create Money with more precision
    (let [money (m/of :crypto/ETH 12.345M)
          ;; Without rounding mode, scale down to 2 throws
          _ (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"ArithmeticException during rescaling"
                 (se/money->edn-map money {:rescale 2})))
          ;; With rounding mode, works
          em (se/money->edn-map money {:rescale 2 :rounding-mode :HALF_UP})]
      (is (= "12.35" (.toPlainString ^BigDecimal (:amount em))))))
  (testing "money->edn-string with :rescale"
    (let [money (m/of :PLN 12.30M)
          s     (se/money->edn-string money {:rescale 4})]
      (is (re-find #"12\.3000M" s))))
  (testing "money->edn-full-map with :rescale"
    (let [money (m/of :PLN 12.30M)
          em    (se/money->edn-full-map money {:rescale 4})]
      (is (= "12.3000" (.toPlainString ^BigDecimal (:amount em))))))
  (testing ":rescale with keyword rounding mode"
    (let [money (m/of :crypto/ETH 12.345M)
          em    (se/money->edn-map money {:rescale 2 :rounding-mode :HALF_DOWN})]
      (is (= "12.34" (.toPlainString ^BigDecimal (:amount em)))))))

(deftest rescale-deserialization-edn
  (testing "edn-map->money with :rescale creates Money with custom scale"
    (let [m (se/edn-map->money {:currency :PLN :amount 12.3456M}
                               {:rescale 4})]
      (is (= :PLN (c/id m)))
      (is (= "12.3456" (.toPlainString ^BigDecimal (m/amount m))))
      ;; Currency should have the custom scale
      (is (= 4 (.scale ^io.randomseed.bankster.Currency (c/of m))))))
  (testing "edn-map->money with :rescale prevents data loss"
    ;; PLN has scale 2, but we want to preserve 4 decimal places
    (let [m (se/edn-map->money {:currency :PLN :amount 12.3456M}
                               {:rescale 4})]
      (is (= "12.3456" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-map->money with :rescale allows downscaling with rounding"
    (let [m (se/edn-map->money {:currency :PLN :amount 12.345M}
                               {:rescale 2 :rounding-mode :HALF_UP})]
      (is (= "12.35" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "edn-string->money with :rescale"
    (let [m (se/edn-string->money "#money[12.3456M PLN]" {:rescale 4})]
      (is (= "12.3456" (.toPlainString ^BigDecimal (m/amount m)))))))

(deftest rounding-mode-parsing-edn
  (testing "rounding-mode accepts RoundingMode object"
    (let [m (se/edn-map->money {:currency :PLN :amount 1.005M}
                               {:rounding-mode RoundingMode/HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "rounding-mode accepts keyword"
    (let [m (se/edn-map->money {:currency :PLN :amount 1.005M}
                               {:rounding-mode :HALF_UP})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m))))))
  (testing "rounding-mode accepts string"
    (let [m (se/edn-map->money {:currency :PLN :amount 1.005M}
                               {:rounding-mode "HALF_UP"})]
      (is (= "1.01" (.toPlainString ^BigDecimal (m/amount m)))))))

(deftest invalid-rounding-and-rescale-edn
  (testing "invalid rounding-mode in opts throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid rounding-mode"
         (se/edn-map->money {:currency :PLN :amount 1.005M}
                            {:rounding-mode "NOPE"}))))
  (testing "invalid rounding-mode in map throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid rounding-mode"
         (se/edn-map->money {:currency :PLN :amount 1.005M :rounding-mode "NOPE"}))))
  (testing "invalid rescale value throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid rescale"
         (se/edn-map->money {:currency :PLN :amount 1.00M}
                            {:rescale -1}))))
  (testing "invalid rescale type throws"
    (let [money (m/of :PLN 1.00M)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid rescale"
           (se/money->edn-map money {:rescale "2"})))))
  (testing "invalid rounding-mode on serialization throws"
    (let [money (m/of :PLN 1.00M)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid rounding-mode"
           (se/money->edn-map money {:rescale 2 :rounding-mode "NOPE"}))))))
