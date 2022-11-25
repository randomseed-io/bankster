(ns

    ^{:doc    "bankster library, money tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.money-test

  (:require [clojure.test                    :refer [deftest
                                                     testing
                                                     is are
                                                     use-fixtures]]
            [clojure.spec.alpha              :as                 s]
            [clojure.spec.gen.alpha          :as               gen]
            [orchestra.spec.test             :as                st]
            [io.randomseed.bankster          :as          bankster]
            [io.randomseed.bankster.spec     :as              spec]
            [expound.alpha                   :as           expound]
            [io.randomseed.bankster.currency :as                 c]
            [io.randomseed.bankster.registry :as          registry]
            [io.randomseed.bankster.money    :as                 m]
            [io.randomseed.bankster.scale    :as             scale])

  (:import [io.randomseed.bankster Currency Money]))

(s/check-asserts true)

(defmacro map=
  [a b]
  `(= (into {} ~a) ~b))

(deftest of-macro
  (testing "when it returns nil for nil as an amount"
    (is (= (m/of  nil nil)  nil))
    (is (= (m/of  PLN nil)  nil))
    (is (= (m/of :PLN nil)  nil))
    (is (= (m/of nil :PLN)  nil)))
  (testing "when it returns a money object"
    (is (map= (m/of PLN 10.11 scale/ROUND_DOWN) {:amount 10.11M :currency #currency PLN}))
    (is (map= (m/of PLN 10.111111 scale/ROUND_DOWN) {:amount 10.11M :currency #currency PLN}))
    (is (map= (m/of PLN 10.111111 scale/ROUND_UP) {:amount 10.12M :currency #currency PLN}))
    (is (map= (m/of EUR 12.12) {:amount 12.12M :currency #currency EUR}))
    (is (map= (m/of :EUR 12.12) {:amount 12.12M :currency #currency EUR}))
    (is (map= (m/of crypto/ETH 1.00001) {:amount 1.00001M :currency #currency :crypto/ETH}))
    (is (map= (m/of 1.00001 crypto/ETH) {:amount 1.00001M :currency #currency :crypto/ETH}))
    (is (map= (m/of :12PLN)   {:amount 12M :currency #currency PLN}))
    (is (map= (m/of :PLN12)   {:amount 12M :currency #currency PLN}))
    (is (map= (m/of :12_PLN)  {:amount 12M :currency #currency PLN}))
    (is (map= (m/of :PLN_12)  {:amount 12M :currency #currency PLN}))
    (is (map= (m/of PLN12)    {:amount 12M :currency #currency PLN}))
    (is (map= (m/of 12 PLN)   {:amount 12M :currency #currency PLN}))
    (is (map= (m/of "12 PLN") {:amount 12M :currency #currency PLN}))
    (is (map= (m/of "PLN 12") {:amount 12M :currency #currency PLN}))
    (is (map= (m/of :12.00001PLN DOWN) {:amount 12M :currency #currency PLN}))
    (is (map= (m/of :12.00001PLN ROUND_DOWN) {:amount 12M :currency #currency PLN}))
    (is (map= (m/of :PLN12.111 UP)     {:amount 12.12M :currency #currency PLN}))
    (is (map= (m/of PLN12.111 UP)      {:amount 12.12M :currency #currency PLN}))
    (is (map= (m/of PLN_12.111 UP)     {:amount 12.12M :currency #currency PLN}))
    (is (map= (m/of :PLN 12.111 UP)    {:amount 12.12M :currency #currency PLN}))
    (is (map= (m/of "12.111 PLN" UP)   {:amount 12.12M :currency #currency PLN}))
    (is (map= (m/of PLN) {:amount 0M :currency #currency PLN}))
    (m/with-currency EUR (is (map= (m/of 1000) {:amount 1000M :currency #currency EUR})))
    (is (map= (m/of crypto/ETH) {:amount 0M :currency #currency crypto/ETH}))
    (let [mv #money[10 PLN]] (is (map= (m/of mv) {:amount 10M :currency #currency PLN})))
    (is (map= (m/of #currency{:id :KIKI :scale 1} 5) {:amount 5M :currency #currency{:id KIKI :scale 1 :weight 0}}))
    (is (map= (m/of #currency{:id :KIKI :scale 1})   {:amount 0M :currency #currency{:id KIKI :scale 1 :weight 0}}))
    (let [mv (m/of #currency{:id :KIKI :scale 1} 123)]
      (is (map= (m/of mv 10) {:amount 10M :currency #currency{:id :KIKI :scale 1 :weight 0}}))
      (is (map= (m/of mv)    {:amount 123M :currency #currency{:id :KIKI :scale 1 :weight 0}})))))

(deftest money-tagged-literal
  (testing "when it returns a money object"
    (is (map= #money PLN {:amount 0M :currency #currency PLN}))
    (is (map= #money crypto/ETH {:amount 0M :currency #currency crypto/ETH}))
    (is (= #money[19 EUR] (m/of 19 :EUR)))
    (is (map= #money[19 {:id :EUR :domain :ISO-4217}] {:amount 19M :currency (c/of {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0})}))
    (is (map= #money[19 EUR]    {:amount 19M :currency #currency EUR}))
    (is (map= #money[19 EUR]    {:amount 19M :currency #currency {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}}))
    (is (map= #money :19EUR     {:amount 19M :currency #currency EUR}))
    (is (map= #money EUR_19.1   {:amount 19.1M :currency #currency EUR}))
    (is (map= #money "19.1 EUR" {:amount 19.1M :currency #currency EUR}))
    (is (map= #money/crypto ETH1.00000001   {:amount 1.00000001M :currency #currency crypto/ETH}))
    (is (map= #money/crypto[ETH 1.00000001] {:amount 1.00000001M :currency #currency crypto/ETH}))
    (is (map= #money/crypto[1.00000001 ETH] {:amount 1.00000001M :currency #currency crypto/ETH}))
    (is (map= #money crypto/ETH1.00000001   {:amount 1.00000001M :currency #currency crypto/ETH}))))

(deftest monetary-protocol
  (testing "when it can get ID of a currency"
    (is (= (c/id #money[12.12 PLN]) :PLN))
    (is (= (c/id #money/crypto[12.12 ETH]) :crypto/ETH)))
  (testing "when it gets a currency unit from a registry or returns it when given directly"
    (is (map= (c/unit #money[10 EUR]) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0})))
  (testing "when it checks if a currency is defined"
    (is (= (c/defined? #money[5 EUR]) true))
    (is (= (c/defined? (m/of #currency{:id :KIKI :scale 1} 5)) false))
    (is (= (c/defined? (m/of #currency{:id :KIKI :scale 1})) false)))
  (testing "when it checks whether two currencies have the same IDs"
    (is (= (c/same-ids? #money[10 PLN] :PLN) true))
    (is (= (c/same-ids? :PLN #money[10 PLN]) true))
    (is (= (c/same-ids? #money[10 PLN] #money[10 PLN]) true))
    (is (= (c/same-ids? #currency crypto/USDT #money[20 EUR]) false))
    (is (= (c/same-ids? #currency crypto/USDT #money/crypto[20 USDT]) true))
    (is (= (c/same-ids? #money[10 PLN] :PLN) true))
    (is (= (c/same-ids? :PLN #money[10 PLN]) true))
    (is (= (c/same-ids? #money[10 PLN] #money[10 PLN]) true)))
  (testing "when the of-registry function rescales amount and sets the scale of a currency"
    (let [r          (registry/get)
          r          (c/update r #currency{:id :crypto/USDT, :domain :CRYPTO, :kind :COMBANK, :scale 6, :weight 4})
          orig-money #money[15.22 USDT]
          new-money  (m/of-registry r orig-money)]
      (is (= (c/scale orig-money) 8))
      (is (= (m/scale orig-money) 8))
      (is (= (c/scale new-money)  6))
      (is (= (m/scale new-money)  6))
      (is (= (scale/of (m/amount orig-money)) 8))
      (is (= (scale/of (m/amount new-money))  6)))))

(deftest currency-properties
  (testing "when it's possible to get the numeric value of an ISO currency"
    (is (= (c/nr #money[12.32 EUR]) 978))
    (is (= (c/nr #money[1 crypto/ETH]) nil))
    (is (= (c/nr (m/of 10 #currency{:id :PLN :scale 1})) nil)))
  (testing "when it's possible to get scale of a monetary amount"
    (is (= (c/sc #money[1 EUR]) 2))
    (is (= (c/sc #money[1 crypto/ETH]) 18))
    (is (= (c/sc #money[1 XXX]) nil))
    (is (= (c/sc (m/of 10 #currency{:id :PLN :scale 1})) 1)))
  (testing "when it's possible to get the domain of a currency"
    (is (= (c/domain #money[1 EUR]) :ISO-4217))
    (is (= (c/domain #money[1 crypto/ETH]) :CRYPTO))
    (is (= (c/domain (m/of 10 #currency{:id :PLN :scale 1})) nil)))
  (testing "when it's possible to get the kind of a currency"
    (is (= (c/kind #money[1 EUR]) :FIAT))
    (is (= (c/kind #money[1 crypto/ETH]) :DECENTRALIZED))
    (is (= (c/kind (m/of 10 #currency{:id :PLN :scale 1})) nil)))
  (testing "when it's possible to get the namespaced code of a currency"
    (is (= (c/ns-code #money[1 EUR]) "EUR"))
    (is (= (c/ns-code #money[1 crypto/ETH]) "crypto/ETH"))
    (is (= (c/ns-code (m/of 10 #currency{:id :PLN :scale 1})) "PLN")))
  (testing "when it's possible to get the code of a currency"
    (is (= (c/code #money[1 EUR]) "EUR"))
    (is (= (c/code #money[1 crypto/ETH]) "ETH"))
    (is (= (c/code (m/of 10 #currency{:id :PLN :scale 1})) "PLN")))
  (testing "when it's possible to get the ID of a currency"
    (is (= (c/id #money[1 EUR]) :EUR))
    (is (= (c/id #money[1 crypto/ETH]) :crypto/ETH))
    (is (= (c/id (m/of 10 #currency{:id :PLN :scale 1})) :PLN)))
  (testing "when it's possible to get countries associated with a currency"
    (is (= (c/countries #money[1 USD]) #{:TL :IO :PR :BQ :EC :VG :US :GU :AS :PW :TC :MP :VI :FM :MH :UM}))
    (is (= (c/countries #money[1 crypto/ETH]) nil))
    (is (= (c/countries (m/of 10 #currency{:id :PLN :scale 1})) #{:PL})))
  (testing "when it's possible to distinguish money from the currency"
    (is (= (c/currency? #money[1 EUR]) false)))
  (testing "when it's possible to validate money as a currency representation"
    (is (= (c/possible? #money[1 EUR]) true)))
  (testing "when it's possible to check if a currency has numeric ID"
    (is (= (c/has-numeric-id? #money[1 EUR]) true))
    (is (= (c/has-numeric-id? #money[1 crypto/ETH]) false))
    (is (= (c/has-numeric-id? (m/of 10 #currency{:id :PLN :scale 1})) false)))
  (testing "when it's possible to check if a currency has any kind"
    (is (= (c/has-kind? #money[1 EUR]) true))
    (is (= (c/has-kind? #money[1 crypto/ETH]) true))
    (is (= (c/has-kind? (m/of 1 #currency{:id :PLN :scale 1})) false)))
  (testing "when it's possible to check if a currency has particular kind"
    (is (= (c/kind-of? #money[1 EUR] :FIAT) true))
    (is (= (c/kind-of? #money[1 crypto/ETH] :DECENTRALIZED) true))
    (is (= (c/kind-of? (m/of 1 #currency{:id :PLN :scale 1}) :FIAT) false)))
  (testing "when it's possible to check if a currency has assigned some country"
    (is (= (c/has-country? #money[1 EUR]) true))
    (is (= (c/has-country? #money[1 crypto/ETH]) false))
    (is (= (c/has-country? (m/of #currency{:id :PLN :scale 1} 1)) true)))
  (testing "when it's possible to check if a currency has assigned the given domain"
    (is (= (c/in-domain? :ISO-4217 #money[1 EUR]) true))
    (is (= (c/in-domain? nil #money[1 crypto/ETH]) false))
    (is (= (c/in-domain? nil (m/of 1 #currency{:id :PLN :scale 1})) true)))
  (testing "when it's possible to check if a currency is auto-scaled"
    (is (= (c/big? #money[1 EUR]) false))
    (is (= (c/big? #money/crypto[1 ETH]) false))
    (is (= (c/big? (m/of 1 #currency{:id :PLN :scale 1})) false))
    (is (= (c/big? (m/of 1 #currency{:id :XXX :scale 1})) false))
    (is (= (c/big? (m/of 1 #currency{:id :PLN})) true))
    (is (= (c/big? (m/of 1 #currency{:id :XXX})) true))
    (is (= (c/big? (m/of 1 #currency :XXX)) true)))
  (testing "when it's possible to check if a currency is cryptocurrency"
    (is (= (c/crypto? #money[1 EUR]) false))
    (is (= (c/crypto? #money/crypto[1 ETH]) true))
    (is (= (c/crypto? (m/of 1 #currency{:id :PLN :scale 1})) false))
    (is (= (c/crypto? (m/of #currency{:id :crypto/PLN :scale 1} 1)) true)))
  (testing "when it's possible to check if a currency is an ISO currency"
    (is (= (c/iso? #money[1 EUR]) true))
    (is (= (c/iso? #money/crypto[1 ETH]) false))
    (is (= (c/iso? (m/of 1 #currency{:id :PLN :scale 1})) false))
    (is (= (c/iso? (m/of #currency{:id :crypto/PLN :scale 1} 1)) false))
    (is (= (c/iso? (m/of 1 #currency{:id :ETH :scale 18 :domain :ISO-4217})) true))
    (is (= (c/iso? (m/of 1 #currency{:id :PLN :scale 1 :domain :ISO-4217})) true)))
  (testing "when it's possible to get a localized property"
    (is (= (c/localized-property :name #money[1 :crypto/ETH] :pl) "Ether"))
    (is (= (c/name #money[2 PLN] :pl) "złoty polski"))
    (is (= (c/name #money[1 :crypto/ETH] :pl) "Ether"))
    (is (= (c/name #money[0.1 :crypto/ETH] :pl_PL) "Ether"))
    (is (= (c/name #money[1.1 crypto/ETH] :en) "Ether"))
    (is (#{"EUR" "€"} (c/symbol #money[2.22 EUR] :en_US)))
    (is (#{"EUR" "€"} (c/symbol #money EUR :en)))
    (is (= (c/symbol #money PLN :pl) "zł"))))

(deftest monetary-value-properties
  (testing "when it's possible to get the amount"
    (is (= (m/amount #money[123.4 EUR]) 123.4M))
    (is (= (m/amount #money[123.4 crypto/ETH]) 123.4M))
    (is (= (m/stripped-amount #money[123.4 EUR]) 123.4M))
    (is (= (m/stripped-amount #money[123.4 crypto/ETH]) 123.4M)))
  (testing "when it's possible to get the currency"
    (is (= (m/currency #money[123.4 EUR]) #currency EUR))
    (is (= (m/currency #money[123.4 crypto/ETH]) #currency crypto/ETH)))
  (testing "when it's possible to get the scale"
    (is (= (m/scale #money[123.4 EUR]) 2))
    (is (= (m/scale #money[123.4 crypto/ETH]) 18))))

(deftest accountable-protocol
  (testing "when it can create a monetary value based on a currency and an amount"
    (is (= (m/value :EUR) #money[0 EUR]))
    (is (= (m/value :EUR 12.34) #money[12.34 EUR]))
    (is (= (m/value :EUR 12.345 scale/ROUND_UP) #money[12.35 EUR]))
    (is (= (m/value 'EUR) #money[0 EUR]))
    (is (= (m/value 'EUR 12.34) #money[12.34 EUR]))
    (is (= (m/value 'EUR 12.345 scale/ROUND_UP) #money[12.35 EUR]))
    (is (= (m/value #currency :EUR) #money[0 EUR]))
    (is (= (m/value #currency :EUR 12.34) #money[12.34 EUR]))
    (is (= (m/value 12.34 :EUR) #money[12.34 EUR]))
    (is (= (m/value 12.34 #currency :EUR) #money[12.34 EUR]))
    (is (= (m/value 12.345 #currency :EUR scale/ROUND_UP) #money[12.35 EUR]))
    (m/with-currency EUR
      (is (= (m/value 12.34) #money[12.34 EUR]))
      (is (= (m/value 12.345 nil scale/ROUND_UP) #money[12.35 EUR])))
    (is (= (m/value #currency :EUR) #money[0 EUR]))
    (testing "when it can create a monetary value based on other monetary value"
      (is (= (m/value #money[5 EUR]) #money[5 EUR]))
      (is (= (m/value #money[5 EUR] 12.34) #money[12.34 EUR]))
      (is (= (m/value #money[10 EUR] 12.345 scale/ROUND_UP) #money[12.35 EUR])))))

(deftest monetary-values-calculations
  (testing "when it's possible to divide monetary values"
    (is (= (m/div 2) 0.5M))
    (is (= (m/div #money[1 PLN] #money[4 PLN]) 0.25M))
    (is (= (m/div #money[1 PLN] 4M) #money[0.25 PLN]))
    (is (= (m/div #money[10 PLN] 2 2) #money[2.50 PLN]))
    (is (= (m/div #money[10 PLN] #money[2 PLN] 1) 5M))
    (is (= (m/div #money[1 PLN] 8 0.125) #money[1 PLN]))
    (is (= (m/div #money[1 PLN] 8 0.125 #money[1 PLN]) 1M))
    (is (= (m/div #money[1 EUR] 8 0.125) #money[1 EUR]))
    (is (= (m/div 1 8 0.125)       1M))
    (is (= (m/div 1 8)             0.125M))
    (is (= (m/div 1 8 1)           0.125M))
    (is (thrown? ArithmeticException (m/div 1 3 0.333334)))
    (is (thrown? ArithmeticException (m/div #money[1 PLN] 8)))
    (is (thrown? ArithmeticException (m/div 1 3)))
    (is (thrown? ArithmeticException (m/div 1 3 1)))
    (is (thrown? ArithmeticException (m/div #money[1 PLN] 3)))
    (scale/with-rounding UP
      (is (= (m/div 1 8 0.125)         1M))
      (is (= (m/div 1 3 0.333334)      1.000017999964000071999857M))
      (is (= (m/div 1 8)               0.125M))
      (is (= (m/div 1 3)               0.33334M))
      (is (= (m/div 1 8 1)             0.125M))
      (is (= (m/div 1 3 1)             0.33334M))
      (is (= (m/div #money[1 PLN] 8)   #money[0.13 PLN]))
      (is (= (m/div #money[1 PLN] 3)   #money[0.34 PLN]))
      (is (= (m/div #money[1 PLN] 8 1) #money[0.13 PLN]))
      (is (= (m/div #money[1 PLN] 3 1) #money[0.34 PLN])))
    (let [m (m/div (m/scale #money[1 EUR] 10) 8)
          n (m/div 1M 8)]
      (is (= (m/div #money[1 EUR] n 8M) #money[1 EUR]))
      (is (= (m/div n 0.125) 1M))
      (is (= (m/div n 1 0.125 1) 1M))
      (is (= (m/div #money[1 EUR] m 8M) 1M))
      (is (= (m/div m 0.125) #money[1 EUR]))
      (is (= (m/div m 1 0.125 1) #money[1 EUR])))
    (is (= (m/div-scaled 2) 0.5M))
    (is (= (m/div-scaled #money[1 PLN] #money[4 PLN]) 0.25M))
    (is (= (m/div-scaled #money[1 PLN] 4M) #money[0.25 PLN]))
    (is (= (m/div-scaled #money[10 PLN] 2 2) #money[2.50 PLN]))
    (is (= (m/div-scaled #money[10 PLN] #money[2 PLN] 1) 5M))
    (is (thrown? ArithmeticException (m/div-scaled (m/value :EUR 1) 8 0.125)))
    (scale/with-rounding UP
      (is (= (m/div-scaled 1 1 4 2 0.125) 1M))
      (is (= (m/div-scaled #money[1 PLN] 1 1 #money[3 PLN]) 0.34M))
      (is (= (m/div-scaled #money[1 PLN] 1 1 3) #money[0.34 PLN]))
      (is (= (m/div-scaled #money[1 PLN] 1 8 #money[0.125 PLN]) 1M))
      (is (= (m/div-scaled #money[1 PLN] 1 #money[8 PLN] 0.125) 1.04M))
      (is (= (m/div-scaled #money[1 PLN] #money[1 PLN] 8 0.125) 1.04M))
      (is (= (m/div-scaled #money[1 PLN] 1 8 0.125) #money[1.04 PLN]))
      (is (= (m/div-scaled 1 1 8 0.125) 1M))))
  (let [m (m/div (m/scale #money[1 EUR] 10) 8)
        n (m/div 1M 8)]
    (is (= (m/div-scaled #money[1 EUR] n 8M) #money[1 EUR]))
    (is (= (m/div-scaled n 0.125) 1M))
    (is (= (m/div-scaled n 1 0.125 1) 1M))
    (is (= (m/div-scaled #money[1 EUR] m 8M) 1M))
    (is (= (m/div-scaled m 0.125) #money[1 EUR]))
    (is (= (m/div-scaled m 1 0.125 1) #money[1 EUR])))
  (testing "when it's possible to multiply monetary values"
    (is (== (m/mul) 1M))
    (is (== (m/mul 2) 2M))
    (is (= (m/mul #money[2 PLN]) #money[2 PLN]))
    (is (= (m/mul #money[1.10 PLN] 4.1) #money[4.51 PLN]))
    (is (= (m/mul #money[1 PLN] 4.1 2) #money[8.20 PLN]))
    (is (= (m/mul #money[10 PLN] 0.5) #money[5 PLN]))
    (is (= (m/mul 2 #money[10 PLN]) #money[20 PLN]))
    (is (= (m/mul 2 0.5 #money[10 PLN]) #money[10 PLN]))
    (is (= (m/mul 2 0.5 2 #money[10 PLN]) #money[20 PLN]))
    (is (= (m/mul 2 0.5 2 #money[10 PLN] 2.5) #money[50 PLN]))
    (is (= (m/mul #money[1 PLN] 0.5 0.5 0.5 2) #money[0.25 PLN]))
    (is (== (m/mul-scaled) 1M))
    (is (== (m/mul-scaled 2) 2M))
    (is (= (m/mul-scaled #money[2 PLN]) #money[2 PLN]))
    (is (= (m/mul-scaled #money[1.10 PLN] 4.1) #money[4.51 PLN]))
    (is (= (m/mul-scaled #money[1 PLN] 4.1 2) #money[8.20 PLN]))
    (is (= (m/mul-scaled #money[10 PLN] 0.5) #money[5 PLN]))
    (is (= (m/mul-scaled 2 #money[10 PLN]) #money[20 PLN]))
    (is (= (m/mul-scaled 2 0.5 #money[10 PLN]) #money[10 PLN]))
    (is (= (m/mul-scaled 2 0.5 2 #money[10 PLN]) #money[20 PLN]))
    (is (= (m/mul-scaled 2 0.5 2 #money[10 PLN] 2.5) #money[50 PLN]))
    (is (thrown? ArithmeticException (m/mul-scaled 1 0.5 0.5 0.5 #money[1 PLN] 2)))
    (is (thrown? ArithmeticException (m/mul-scaled 1 0.5 0.5 0.5 #money[1 PLN])))
    (is (thrown? ArithmeticException (m/mul-scaled #money[1 PLN] 0.5 0.5 0.5 2))))
  (testing "when it's possible to add monetary values"
    (is (= (m/add) 0M))
    (is (= (m/add #money[1.25 PLN]) #money[1.25 PLN]))
    (is (= (m/add #money[1.25 PLN] #money[1 PLN]) #money[2.25 PLN]))
    (is (= (m/add #money[10 PLN] #money[1.25 PLN] #money[1 PLN]) #money[12.25 PLN]))
    (let [m (m/div (scale/apply #money[1  EUR] 10) 8)
          n (m/div (scale/apply #money[-1 EUR] 10) 8)]
      (is (= (m/add #money[10 EUR] m n) #money[10 EUR]))
      (is (= (m/add m n) #money[0 EUR]))))
  (testing "when it's possible to subtract monetary values"
    (is (= (m/sub #money[1.25 PLN]) #money[-1.25 PLN]))
    (is (= (m/sub #money[1.25 PLN] #money[1 PLN]) #money[0.25 PLN]))
    (is (= (m/sub #money[10 PLN] #money[1.25 PLN] #money[1 PLN]) #money[7.75 PLN]))
    (let [m (m/div (scale/apply #money[1  EUR] 10) 8)
          n (m/div (scale/apply #money[-1 EUR] 10) 8)]
      (is (= (m/sub #money[10 EUR] m n) #money[10 EUR]))
      (is (= (m/sub m n) #money[0.25 EUR]))))
  (testing "when it's possible to increase and decrease minor and major components"
    (is (= (m/inc-major #money[10.01 PLN]) #money[11.01 PLN]))
    (is (= (m/inc-minor #money[10.01 PLN]) #money[10.02 PLN]))
    (is (= (m/dec-major #money[10.01 PLN]) #money[9.01 PLN]))
    (is (= (m/dec-minor #money[10.01 PLN]) #money[10.00 PLN])))
  (testing "when it's possible to add and subtract minor and major components"
    (is (= (m/add-major #money[10.01 PLN] 1) #money[11.01 PLN]))
    (is (= (m/add-minor #money[10.01 PLN] 1) #money[10.02 PLN]))
    (is (= (m/sub-major #money[10.01 PLN] 1) #money[9.01 PLN]))
    (is (= (m/sub-minor #money[10.01 PLN] 1) #money[10.00 PLN])))
  (testing "when it's possible to calculate min and max amounts"
    (is (= (m/min-amount #money[10.01 PLN] #money[10.02 PLN]) #money[10.01 PLN]))
    (is (= (m/min-amount #money[10.01 PLN] #money[10.02 PLN] #money[0 PLN]) #money[0 PLN]))
    (is (= (m/max-amount #money[10.01 PLN] #money[10.02 PLN]) #money[10.02 PLN]))
    (is (= (m/max-amount #money[10.01 PLN] #money[10.02 PLN] #money[0 PLN]) #money[10.02 PLN])))
  (testing "when it's possible to convert amount of one currency to another"
    (scale/with-rounding UP (is (= (m/convert #money[10.01 EUR] :PLN 4.55) #money[45.55 PLN])))
    (is (= (m/convert #money[10.01 EUR] :PLN 4.55 scale/ROUND_UP) #money[45.55 PLN]))
    (is (= (m/convert #money[10 EUR] :PLN 4.55) #money[45.50 PLN])))
  (testing "when it's possible to negate the amount of a monetary value"
    (is (= (m/neg #money[10 EUR])  #money[-10 EUR])))
  (testing "when it's possible to get the absolute monetary value"
    (is (= (m/abs #money[-10 EUR]) #money[10 EUR]))
    (is (= (m/abs #money[10 EUR])  #money[10 EUR])))
  (testing "when it's possible to round the amount of a monetary value"
    (is (= (m/round #money[10.12 EUR] 1 scale/ROUND_UP)   #money[10.20 EUR]))
    (is (= (m/round #money[10.12 EUR] 1 scale/ROUND_DOWN) #money[10.10 EUR]))
    (scale/with-rounding DOWN (is (= (m/round #money[10.12 EUR] 1) #money[10.10 EUR]))))
  (testing "when it's possible to get the major part of a monetary value"
    (is (= (m/major #money[10.12 EUR])  10M))
    (is (= (m/major #money[-10.12 EUR]) -10M)))
  (testing "when it's possible to get the minor part of a monetary value"
    (is (= (m/minor #money[10.12 EUR])  12M))
    (is (= (m/minor #money[-10.12 EUR]) -12M)))
  (testing "when it's possible to get major and minor parts of a monetary value"
    (is (= (m/major-minor #money[10.12 EUR])  [10M 12M]))
    (is (= (m/major-minor #money[-10.12 EUR]) [-10M -12M])))
  (testing "when it's possible to compare amounts of monetary values"
    (is (= (m/compare-amounts #money[10.12 EUR] #money[10.12 EUR]) 0))
    (is (= (m/compare-amounts #money[10.12 EUR] #money[10.13 EUR]) -1))
    (is (= (m/compare-amounts #money[10.12 EUR] #money[10.11 EUR]) 1))
    (is (= (m/compare-amounts #money[10.12 EUR] (scale/apply #money[10.12 EUR] 10)) 0))
    (is (= (m/compare-amounts #money[10.12 EUR] (scale/apply #money[10.13 EUR] 10)) -1))
    (is (= (m/compare-amounts #money[10.12 EUR] (scale/apply #money[10.11 EUR] 10)) 1)))
  (testing "when it's possible to compare monetary values"
    (is (= (m/compare #money[10.12 EUR] #money[10.12 EUR]) 0))
    (is (= (m/compare #money[10.12 EUR] #money[10.13 EUR]) -1))
    (is (= (m/compare #money[10.12 EUR] #money[10.11 EUR]) 1)))
  (testing "when it's possible to read scales and rescale monetary values"
    (is (= (m/scale #money[10 PLN]) 2))
    (is (= (m/scale #money[10 PLN] 4) #money[10 PLN]))
    (is (= (m/scale (m/scale #money[10 PLN] 4)) 4))
    (is (= (m/eq? #money[10 PLN] (m/scale #money[10 PLN] 4)) false))
    (is (= (m/eq-am? #money[10 PLN] (m/scale #money[10 PLN] 4)) true))
    (is (= (m/major-minor (m/add-minor (m/scale #money[10 PLN] 4) 1234)) [10M 1234M]))
    (is (= (m/strip #money/crypto[12.2345 ETH]) #money/crypto[12.2345 ETH]))
    (is (= (scale/of (m/strip #money/crypto[12.2345 ETH])) 4))
    (is (= (scale/of (m/currency (m/strip #money/crypto[12.2345 ETH]))) 18))
    (is (= (scale/of (m/amount (m/strip #money/crypto[12.2345 ETH]))) 4))
    (is (= (m/rescaled? (m/strip #money/crypto[12.2345 ETH])) true))))

(deftest monetary-values-logical-operations
  (testing "when it's possible to check if a value is Money"
    (is (= (m/money? #money[10.12 EUR]) true))
    (is (= (m/money? #currency EUR) false))
    (is (= (m/money? :EUR) false))
    (is (= (m/money? :10_EUR) false))
    (is (= (m/money? "10 EUR") false))
    (is (= (m/money? 'EUR) false))
    (is (= (m/money? 'EUR10) false))
    (is (= (m/money? 123) false))
    (is (= (m/money? nil) false)))
  (testing "when it's possible to check if monetary values are equal"
    (is (= (m/eq? #money[10 EUR]) true))
    (is (= (m/eq? #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/eq? #money[10 EUR] #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/eq? #money[10 EUR] #money[20 EUR]) false))
    (is (= (m/eq? #money[10 EUR] #money[20 EUR] #money[10 EUR]) false))
    (is (= (m/eq? #money[10 EUR] #money[10 PLN] #money[10 EUR]) false))
    (is (= (m/eq? #money[10 EUR] #money[10 EUR] (scale/apply #money[10 EUR] 3)) false)))
  (testing "when it's possible to check if monetary values are different"
    (is (= (m/ne? #money[10 EUR]) false))
    (is (= (m/ne? #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/ne? #money[10 EUR] #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/ne? #money[10 EUR] #money[20 EUR]) true))
    (is (= (m/ne? #money[10 EUR] #money[20 EUR] #money[10 EUR]) true))
    (is (= (m/ne? #money[10 EUR] #money[10 PLN] #money[10 EUR]) true))
    (is (= (m/ne? #money[10 EUR] #money[10 EUR] (scale/apply #money[10 EUR] 3)) true)))
  (testing "when it's possible to check if monetary values are equal (regardless of scales)"
    (is (= (m/eq-am? #money[10 EUR]) true))
    (is (= (m/eq-am? #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/eq-am? #money[10 EUR] #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/eq-am? #money[10 EUR] #money[20 EUR]) false))
    (is (= (m/eq-am? #money[10 EUR] #money[20 EUR] #money[10 EUR]) false))
    (is (= (m/eq-am? #money[10 EUR] #money[10 PLN] #money[10 EUR]) false))
    (is (= (m/eq-am? #money[10 EUR] #money[10 EUR] (scale/apply #money[10 EUR] 3)) true)))
  (testing "when it's possible to check if monetary values are different (regardless of scales)"
    (is (= (m/ne-am? #money[10 EUR]) false))
    (is (= (m/ne-am? #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/ne-am? #money[10 EUR] #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/ne-am? #money[10 EUR] #money[20 EUR]) true))
    (is (= (m/ne-am? #money[10 EUR] #money[20 EUR] #money[10 EUR]) true))
    (is (= (m/ne-am? #money[10 EUR] #money[10 PLN] #money[10 EUR]) true))
    (is (= (m/ne-am? #money[10 EUR] #money[10 EUR] (scale/apply #money[10 EUR] 3)) false)))
  (testing "when it's possible to check if monetary values are in monotonically decreasing order"
    (is (= (m/gt? #money[10 EUR]) true))
    (is (= (m/gt? #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/gt? #money[10 EUR] #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/gt? #money[20 EUR] #money[10 EUR]) true))
    (is (= (m/gt? #money[20 EUR] #money[10 EUR] #money[8 EUR]) true)))
  (testing "when it's possible to check if monetary values are in monotonically non-increasing order"
    (is (= (m/ge? #money[10 EUR]) true))
    (is (= (m/ge? #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/ge? #money[10 EUR] #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/ge? #money[20 EUR] #money[10 EUR]) true))
    (is (= (m/ge? #money[20 EUR] #money[10 EUR] #money[8 EUR]) true))
    (is (= (m/ge? #money[20 EUR] #money[10 EUR] #money[11 EUR]) false)))
  (testing "when it's possible to check if monetary values are in monotonically increasing order"
    (is (= (m/lt? #money[10 EUR]) true))
    (is (= (m/lt? #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/lt? #money[10 EUR] #money[10 EUR] #money[10 EUR]) false))
    (is (= (m/lt? #money[20 EUR] #money[10 EUR]) false))
    (is (= (m/lt? #money[20 EUR] #money[10 EUR] #money[8 EUR]) false))
    (is (= (m/lt? #money[10 EUR] #money[20 EUR] #money[30 EUR]) true)))
  (testing "when it's possible to check if monetary values are in monotonically non-decreasing order"
    (is (= (m/le? #money[10 EUR]) true))
    (is (= (m/le? #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/le? #money[10 EUR] #money[10 EUR] #money[10 EUR]) true))
    (is (= (m/le? #money[20 EUR] #money[10 EUR]) false))
    (is (= (m/le? #money[20 EUR] #money[10 EUR] #money[8 EUR]) false))
    (is (= (m/le? #money[10 EUR] #money[20 EUR] #money[30 EUR]) true)))
  (testing "when it's possible to check if a monetary value is negative"
    (is (= (m/is-neg? #money[10 EUR]) false))
    (is (= (m/is-neg? #money[-10 EUR]) true)))
  (testing "when it's possible to check if a monetary value is positive"
    (is (= (m/is-pos? #money[10 EUR]) true))
    (is (= (m/is-pos? #money[-10 EUR]) false)))
  (testing "when it's possible to check if a monetary value is zero"
    (is (= (m/is-zero? #money[10 EUR]) false))
    (is (= (m/is-zero? #money[-10 EUR]) false))
    (is (= (m/is-zero? #money[0 PLN]) true)))
  (testing "when it's possible to check if a monetary value is negative or zero"
    (is (= (m/is-pos-or-zero? #money[10 EUR]) true))
    (is (= (m/is-pos-or-zero? #money[-10 EUR]) false))
    (is (= (m/is-pos-or-zero? #money[0 PLN]) true)))
  (testing "when it's possible to check if a monetary value is positive or zero"
    (is (= (m/is-neg-or-zero? #money[10 EUR]) false))
    (is (= (m/is-neg-or-zero? #money[-10 EUR]) true))
    (is (= (m/is-neg-or-zero? #money[0 PLN]) true))))
