(ns

    ^{:doc    "bankster library, currency tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.currency-test

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
            [io.randomseed.bankster.registry :as          registry]
            [io.randomseed.bankster.currency :as                 c]))

(s/check-asserts true)

(defmacro map=
  [a b]
  `(= (into {} ~a) ~b))

(deftest new-currency
  (testing "when it returns nil for nil or empty map"
    (is (= (c/new nil) nil))
    (is (= (c/new {})  nil))
    (is (= (c/map->new {}) nil)))
  (testing "when it returns a currency object"
    (is (map= (c/new :crypto/EUR) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 2 :FIAT :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id c/auto-scaled :FIAT :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 c/auto-scaled :FIAT :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id 2 :FIAT :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :domain :ISO-4217}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :crypto/EUR :domain :CRYPTO}) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :domain :ISO-4217}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale c/auto-scaled}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale 2 :kind :FIAT}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled :kind :FIAT}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :domain :ISO-4217 :kind :FIAT}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2 :kind :FIAT}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale 2 :kind :FIAT}) {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new :EUR) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :crypto/EUR) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id c/auto-scaled) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id 2) {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new :EUR 1000 c/auto-scaled) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 2 :FIAT) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id c/auto-scaled :FIAT) {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 c/auto-scaled :FIAT) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id 2 :FIAT) {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR}) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :crypto/EUR}) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id}) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled}) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale 2}) {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale 2 :kind :FIAT}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled :kind :FIAT}) {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :kind :FIAT}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale 2 :kind :FIAT}) {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale 2 :weight 0}))))

(deftest currency-tagged-literal
  (testing "when it returns nil for nil or empty map"
    (is (= #currency nil nil))
    (is (= #currency {} nil)))
  (testing "when it returns a currency object"
    (is (map= #currency {:id :EUR :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :crypto/EUR} {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000} {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2} {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :scale 2 :kind :FIAT} {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :kind :FIAT} {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :kind :FIAT} {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2 :kind :FIAT :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2 :weight 0})))
  (testing "when it can use different registries"
    (let [r (c/update (registry/get) #currency{:id PLN :scale 10})]
      (is (map= (c/with-registry r #currency PLN) {:id :PLN :scale 10 :numeric -1 :weight 0 :kind nil :domain nil})))))

(deftest currency-registering
  (testing "when it returns nil for nil or empty map"
    (is (= #currency nil nil))
    (is (= #currency {} nil)))
  (testing "when it gets currency objects from a global registry"
    (is (map= #currency EUR {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= #currency :EUR {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= #currency "EUR" {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0})))
  (testing "when it creates an ad-hoc currency object"
    (is (map= #currency {:id :EUR} {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :crypto/EUR} {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 978 :domain nil} {:id :EUR :domain nil :kind nil :numeric 978 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2 :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :scale 2 :kind :FIAT :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :kind :FIAT :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :kind :FIAT :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2 :kind :FIAT :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2 :weight 0}))))

(deftest monetary-protocol
  (testing "when it can get ID of a currency"
    (is (= (c/id :PLN) :PLN))
    (is (= (c/id :crypto/ETH) :crypto/ETH))
    (is (= (c/id 'crypto/BTC) :crypto/BTC))
    (is (= (c/id "PLN") :PLN))
    (is (= (c/id #currency PLN) :PLN))
    (is (= (c/id #currency crypto/ETH) :crypto/ETH)))
  (testing "when it can resolve currency from a map"
    (is (map= (c/resolve {:id :EUR}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:code "EUR"}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:numeric "978"}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:nr 978 :sc "2"}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (= (c/resolve {:numeric "ABC"}) nil))
    (is (= (c/resolve {:id :EUR :domain nil}) nil))
    (is (map= (c/resolve {:id :EUR :weight 0}) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (= (c/resolve {:id :EUR :weight 1}) nil))
    (is (= (c/resolve-all {:id :EUR}) #{#currency EUR}))
    (is (= (c/resolve-all {:code "EUR"}) #{#currency EUR}))
    (is (= (c/resolve-all {:numeric "978"}) #{#currency EUR}))
    (is (= (c/resolve-all {:nr 978 :sc "2"}) #{#currency EUR}))
    (is (= (c/resolve-all {:numeric "ABC"}) nil))
    (is (= (c/resolve-all {:id :EUR :domain nil}) nil))
    (is (= (c/resolve-all {:id :EUR :weight 0}) #{#currency EUR}))
    (is (= (c/resolve-all {:id :EUR :weight 1}) nil)))
  (testing "when it gets a currency unit from a registry or returns it when given directly"
    (is (map= (c/unit #currency EUR) {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit :EUR)          {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit "EUR")         {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit 978)           {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit {:id :EUR})    {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0})))
  (testing "when it checks if a currency is defined"
    (is (= (c/defined? :PLN) true))
    (is (= (c/defined? :PPPP) false))
    (is (= (c/defined? "EUR") true))
    (is (= (c/defined? #currency crypto/ETH) true))
    (is (= (c/defined? 978) true))
    (is (= (c/defined? 10101010) false)))
  (testing "when it checks whether two currencies have the same IDs"
    (is (= (c/same-ids? :PLN :PLN) true))
    (is (= (c/same-ids? :PLN "PLN") true))
    (is (= (c/same-ids? 'PLN :PLN) true))
    (is (= (c/same-ids? 'PLN 'PLN) true))
    (is (= (c/same-ids? :PLN :EUR) false))
    (is (= (c/same-ids? :PLN :crypto/PLN) false))
    (is (= (c/same-ids? :crypto/USDT :crypto/USDT) true))
    (is (= (c/same-ids? :crypto/USDT #currency crypto/USDT) true))
    (is (= (c/same-ids? :USDT #currency crypto/USDT) false))
    (is (= (c/same-ids? #currency crypto/USDT :PLN) false))))

(deftest currency-properties
  (testing "when it's possible to get the numeric value of an ISO currency"
    (is (= (c/nr #currency EUR) 978))
    (is (= (c/nr #currency crypto/ETH) nil))
    (is (= (c/nr #currency{:id :PLN :scale 1}) nil)))
  (testing "when it's possible to get scale of a monetary amount"
    (is (= (c/sc #currency EUR) 2))
    (is (= (c/sc #currency crypto/ETH) 18))
    (is (= (c/sc #currency XXX) nil))
    (is (= (c/sc #currency{:id :PLN :scale 1}) 1)))
  (testing "when it's possible to get the domain of a currency"
    (is (= (c/domain #currency EUR) :ISO-4217))
    (is (= (c/domain #currency crypto/ETH) :CRYPTO))
    (is (= (c/domain #currency{:id :PLN :scale 1}) nil)))
  (testing "when it's possible to get the kind of a currency"
    (is (= (c/kind #currency EUR) :FIAT))
    (is (= (c/kind #currency crypto/ETH) :DECENTRALIZED))
    (is (= (c/kind #currency{:id :PLN :scale 1}) nil)))
  (testing "when it's possible to get the namespaced code of a currency"
    (is (= (c/ns-code #currency EUR) "EUR"))
    (is (= (c/ns-code #currency crypto/ETH) "crypto/ETH"))
    (is (= (c/ns-code #currency{:id :PLN :scale 1}) "PLN")))
  (testing "when it's possible to get the code of a currency"
    (is (= (c/code #currency EUR) "EUR"))
    (is (= (c/code #currency crypto/ETH) "ETH"))
    (is (= (c/code #currency{:id :PLN :scale 1}) "PLN")))
  (testing "when it's possible to get the ID of a currency"
    (is (= (c/id #currency EUR) :EUR))
    (is (= (c/id #currency crypto/ETH) :crypto/ETH))
    (is (= (c/id #currency{:id :PLN :scale 1}) :PLN)))
  (testing "when it's possible to get countries associated with a currency"
    (is (= (c/countries #currency USD) #{:TL :IO :PR :BQ :EC :VG :US :GU :AS :PW :TC :MP :VI :FM :MH :UM}))
    (is (= (c/countries #currency crypto/ETH) nil))
    (is (= (c/countries #currency{:id :PLN :scale 1}) #{:PL})))
  (testing "when it's possible to distinguish other type of values from the currency"
    (is (= (c/currency? #currency EUR) true))
    (is (= (c/currency? #currency :EUR) true))
    (is (= (c/currency? #currency{:id :AKAKAK}) true))
    (is (= (c/currency? nil) false))
    (is (= (c/currency? 123123) false))
    (is (= (c/currency? 978) false))
    (is (= (c/currency? :PLN) false)))
  (testing "when it's possible to validate common currency representations as possible"
    (is (= (c/possible? #currency EUR) true))
    (is (= (c/possible? :EUR) true))
    (is (= (c/possible? :crypto/ETH) true))
    (is (= (c/possible? 978) true))
    (is (= (c/possible? :LALALA) true))
    (is (= (c/possible? 123123) false))
    (is (= (c/possible? nil) false)))
	  (testing "when it's possible to validate definitive currency representations"
	    (is (= (c/definitive? #currency EUR) true))
	    (is (= (c/definitive? (java.util.Currency/getInstance "EUR")) true))
	    (is (= (c/definitive? :EUR) false))
	    (is (= (c/definitive? "EUR") false))
	    (is (= (c/definitive? 978) false))
	    (is (= (c/definitive? {:id :EUR}) false))
	    (is (= (c/definitive? {:id :EUR :domain :ISO-4217 :numeric 978 :scale 2}) true))
	    (is (= (c/definitive? {:id :EUR :domain :ISO-4217 :numeric 978 :scale 2 :kind :FIAT}) true))
	    (is (= (c/definitive? {:id :EUR :do :ISO-4217 :nr 978 :sc 2 :ki :FIAT}) true))
	    (is (= (c/definitive? {:id :EUR :domain nil :numeric nil :scale "2" :kind nil}) true))
	    (is (= (c/definitive? {:id :EUR :domain nil :numeric nil :scale nil :kind nil}) true))
	    (is (= (c/definitive? {:id :EUR :scale 2}) false))
	    (is (= (c/definitive? {:id :PLN :domain nil :numeric nil :scale c/auto-scaled :kind nil}) true)))
  (testing "when it's possible to check if a currency has numeric ID"
    (is (= (c/has-numeric-id? #currency EUR) true))
    (is (= (c/has-numeric-id? #currency crypto/ETH) false))
    (is (= (c/has-numeric-id? #currency{:id :PLN :scale 1}) false)))
  (testing "when it's possible to check if a currency has particular kind"
    (is (= (c/kind-of? :FIAT #currency EUR) true))
    (is (= (c/kind-of? :DECENTRALIZED #currency crypto/ETH) true))
    (is (= (c/kind-of? :FIAT #currency{:id :PLN :scale 1}) false)))
  (testing "when it's possible to check if a currency has any kind"
    (is (= (c/has-kind? #currency EUR) true))
    (is (= (c/has-kind? #currency crypto/ETH) true))
    (is (= (c/has-kind? #currency{:id :PLN :scale 1}) false)))
  (testing "when it's possible to check if a currency has assigned some country"
    (is (= (c/has-country? #currency EUR) true))
    (is (= (c/has-country? #currency crypto/ETH) false))
    (is (= (c/has-country? #currency{:id :PLN :scale 1}) true)))
  (testing "when it's possible to check if a currency has assigned the given domain"
    (is (= (c/in-domain? :ISO-4217 #currency EUR) true))
    (is (= (c/in-domain? nil #currency crypto/ETH) false))
    (is (= (c/in-domain? nil #currency{:id :PLN :scale 1}) true)))
  (testing "when it's possible to check if a currency is auto-scaled"
    (is (= (c/big? #currency EUR) false))
    (is (= (c/big? #currency crypto/ETH) false))
    (is (= (c/big? #currency{:id :PLN :scale 1}) false))
    (is (= (c/big? #currency{:id :XXX :scale 1}) false))
    (is (= (c/big? #currency{:id :PLN}) true))
    (is (= (c/big? #currency{:id :XXX}) true))
    (is (= (c/big? #currency :XXX) true)))
  (testing "when it's possible to check if a currency is cryptocurrency"
    (is (= (c/crypto? #currency EUR) false))
    (is (= (c/crypto? #currency crypto/ETH) true))
    (is (= (c/crypto? #currency{:id :PLN :scale 1}) false))
    (is (= (c/crypto? #currency{:id :crypto/PLN :scale 1}) true)))
  (testing "when it's possible to check if a currency is an ISO currency"
    (is (= (c/iso? #currency EUR) true))
    (is (= (c/iso? #currency crypto/ETH) false))
    (is (= (c/iso? #currency{:id :PLN :scale 1}) false))
    (is (= (c/iso? #currency{:id :crypto/PLN :scale 1}) false))
    (is (= (c/iso? #currency{:id :ETH :scale 18 :domain :ISO-4217}) true))
    (is (= (c/iso? #currency{:id :PLN :scale 1 :domain :ISO-4217}) true))))

(deftest currency-properties-localized
  (testing "when it's possible to get a localized property"
    (is (= (c/localized-property :name :crypto/ETH :pl) "Ether"))
    (is (= (c/name :PLN :pl) "złoty polski"))
    (is (= (c/name :crypto/ETH :pl) "Ether"))
    (is (= (c/name :crypto/ETH :pl_PL) "Ether"))
    (is (= (c/name :crypto/ETH :en) "Ether"))
    (is (#{"EUR" "€"} (c/symbol :EUR :en_US)))
    (is (#{"EUR" "€"} (c/symbol :EUR :en)))
    (is (= (c/symbol :PLN :pl) "zł"))))
