(ns

    ^{:doc    "bankster library, currency tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.currency-test

  (:require [clojure.spec.alpha              :as               s]
            [midje.sweet                     :refer         :all]
            [midje.experimental              :refer    [for-all]]
            [clojure.spec.gen.alpha          :as             gen]
            [orchestra.spec.test             :as              st]
            [io.randomseed.bankster          :as        bankster]
            [io.randomseed.bankster.spec     :as            spec]
            [expound.alpha                   :as         expound]
            [io.randomseed.bankster.currency :as               c]))

(s/check-asserts true)

(facts "about `new-currency`"
       (fact "when it returns nil for nil or empty map"
             (c/new nil) => nil
             (c/new {})  => nil
             (c/map->new {}) => nil)
       (fact "when it returns a currency object"
             (c/new :crypto/EUR) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new :EUR 1000 2 :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}
             (c/new :EUR c/no-numeric-id c/auto-scaled :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1 :weight 0}
             (c/new :EUR 1000 c/auto-scaled :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}
             (c/new :EUR c/no-numeric-id 2 :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2 :weight 0}
             (c/new {:id :EUR :domain :ISO-4217}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :crypto/EUR :domain :CRYPTO}) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric 1000 :domain :ISO-4217}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2 :weight 0}
             (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale c/auto-scaled}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale 2 :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :domain :ISO-4217 :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2 :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2 :weight 0}

             (c/new :EUR) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new :crypto/EUR) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new :EUR 1000) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1 :weight 0}
             (c/new :EUR c/no-numeric-id) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new :EUR c/no-numeric-id c/auto-scaled) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new :EUR c/no-numeric-id 2) => {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}
             (c/new :EUR 1000 c/auto-scaled) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1 :weight 0}
             (c/new :EUR 1000 2 :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale 2 :weight 0}
             (c/new :EUR c/no-numeric-id c/auto-scaled :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1 :weight 0}
             (c/new :EUR 1000 c/auto-scaled :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale -1 :weight 0}
             (c/new :EUR c/no-numeric-id 2 :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale 2 :weight 0}
             (c/new {:id :EUR}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :crypto/EUR}) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric 1000}) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale 2}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}
             (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled}) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric 1000 :scale 2 :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale 2 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale -1 :weight 0}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale 2 :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale 2 :weight 0}))

(facts "about currency tagged literal"
       (fact "when it returns nil for nil or empty map"
             #currency nil => nil
             #currency {} => nil)
       (fact "when it returns a currency object"
             #currency {:id :EUR :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}
             #currency {:id :crypto/EUR} => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}
             #currency {:id :EUR :numeric 1000} => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1 :weight 0}
             #currency {:id :EUR :scale 2} => {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}
             #currency {:id :EUR :numeric 1000 :scale 2 :kind :FIAT} => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale 2 :weight 0}
             #currency {:id :EUR :kind :FIAT} => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1 :weight 0}
             #currency {:id :EUR :numeric 1000 :kind :FIAT} => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale -1 :weight 0}
             #currency {:id :EUR :scale 2 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2 :weight 0}))

(facts "about currency registering"
       (fact "when it returns nil for nil or empty map"
             #currency nil => nil
             #currency {} => nil)
       (fact "when it gets currency objects from a global registry"
             #currency EUR => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}
             #currency :EUR => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}
             #currency "EUR" => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0})
       (fact "when it creates an ad-hoc currency object"
             #currency {:id :EUR} => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}
             #currency {:id :crypto/EUR} => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}
             #currency {:id :EUR :numeric 1000 :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}
             #currency {:id :EUR :scale 2 :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2  :weight 0}
             #currency {:id :EUR :numeric 1000 :scale 2 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2  :weight 0}
             #currency {:id :EUR :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1  :weight 0}
             #currency {:id :EUR :numeric 1000 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1  :weight 0}
             #currency {:id :EUR :scale 2 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2  :weight 0}))

(facts "about Monetary protocol"
       (fact "when it can get ID of a currency"
             (c/id :PLN) => :PLN
             (c/id :crypto/ETH) => :crypto/ETH
             (c/id 'crypto/BTC) => :crypto/BTC
             (c/id "PLN") => :PLN
             (c/id #currency PLN) => :PLN
             (c/id #currency crypto/ETH) => :crypto/ETH)
       (fact "when it gets a currency unit from a registry or returns it when given directly"
             (c/unit #currency EUR)  => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}
             (c/unit :EUR)  => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}
             (c/unit "EUR") => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}
             (c/unit 978)   => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2 :weight 0}
             (c/unit {:id :EUR}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0})
       (fact "when it checks if a currency is defined"
             (c/defined? :PLN) => true
             (c/defined? :PPPP) => false
             (c/defined? "EUR") => true
             (c/defined? #currency crypto/ETH) => true
             (c/defined? 978) => true
             (c/defined? 10101010) => false)
       (fact "when it checks whether two currencies have the same IDs"
             (c/same-ids? :PLN :PLN) => true
             (c/same-ids? :PLN "PLN") => true
             (c/same-ids? 'PLN :PLN) => true
             (c/same-ids? 'PLN 'PLN) => true
             (c/same-ids? :PLN :EUR) => false
             (c/same-ids? :PLN :crypto/PLN) => false
             (c/same-ids? :crypto/USDT :crypto/USDT) => true
             (c/same-ids? :crypto/USDT #currency crypto/USDT) => true
             (c/same-ids? :USDT #currency crypto/USDT) => false
             (c/same-ids? #currency crypto/USDT :PLN) => false))

(facts "about currency properties"
       (fact "when it's possible to get the numeric value of an ISO currency"
             (c/nr #currency EUR) => 978
             (c/nr #currency crypto/ETH) => nil
             (c/nr #currency{:id :PLN :scale 1}) => nil)
       (fact "when it's possible to get scale of a monetary amount"
             (c/sc #currency EUR) => 2
             (c/sc #currency crypto/ETH) => 18
             (c/sc #currency XXX) => nil
             (c/sc #currency{:id :PLN :scale 1}) => 1)
       (fact "when it's possible to get the domain of a currency"
             (c/domain #currency EUR) => :ISO-4217
             (c/domain #currency crypto/ETH) => :CRYPTO
             (c/domain #currency{:id :PLN :scale 1}) => nil)
       (fact "when it's possible to get the kind of a currency"
             (c/kind #currency EUR) => :FIAT
             (c/kind #currency crypto/ETH) => :DECENTRALIZED
             (c/kind #currency{:id :PLN :scale 1}) => nil)
       (fact "when it's possible to get the namespaced code of a currency"
             (c/ns-code #currency EUR) => "EUR"
             (c/ns-code #currency crypto/ETH) => "crypto/ETH"
             (c/ns-code #currency{:id :PLN :scale 1}) => "PLN")
       (fact "when it's possible to get the code of a currency"
             (c/code #currency EUR) => "EUR"
             (c/code #currency crypto/ETH) => "ETH"
             (c/code #currency{:id :PLN :scale 1}) => "PLN")
       (fact "when it's possible to get the ID of a currency"
             (c/id #currency EUR) => :EUR
             (c/id #currency crypto/ETH) => :crypto/ETH
             (c/id #currency{:id :PLN :scale 1}) => :PLN)
       (fact "when it's possible to get countries associated with a currency"
             (c/countries #currency USD) => #{:TL :IO :PR :BQ :EC :VG :US :GU :AS :PW :TC :MP :VI :FM :MH :UM}
             (c/countries #currency crypto/ETH) => nil
             (c/countries #currency{:id :PLN :scale 1}) => #{:PL})
       (fact "when it's possible to distinguish other type of values from the currency"
             (c/currency? #currency EUR) => true
             (c/currency? #currency :EUR) => true
             (c/currency? #currency{:id :AKAKAK}) => true
             (c/currency? nil) => false
             (c/currency? 123123) => false
             (c/currency? 978) => false
             (c/currency? :PLN) => false)
       (fact "when it's possible to validate common currency representations as possible"
             (c/possible? #currency EUR) => true
             (c/possible? :EUR) => true
             (c/possible? :crypto/ETH) => true
             (c/possible? 978) => true
             (c/possible? :LALALA) => false
             (c/possible? 123123) => false
             (c/possible? nil) => false)
       (fact "when it's possible to check if a currency has numeric ID"
             (c/has-numeric-id? #currency EUR) => true
             (c/has-numeric-id? #currency crypto/ETH) => false
             (c/has-numeric-id? #currency{:id :PLN :scale 1}) => false)
       (fact "when it's possible to check if a currency has particular kind"
             (c/kind-of? #currency EUR :FIAT) => true
             (c/kind-of? #currency crypto/ETH :DECENTRALIZED) => true
             (c/kind-of? #currency{:id :PLN :scale 1} :FIAT) => false)
       (fact "when it's possible to check if a currency has any kind"
             (c/has-kind? #currency EUR) => true
             (c/has-kind? #currency crypto/ETH) => true
             (c/has-kind? #currency{:id :PLN :scale 1}) => false)
       (fact "when it's possible to check if a currency has assigned some country"
             (c/has-country? #currency EUR) => true
             (c/has-country? #currency crypto/ETH) => false
             (c/has-country? #currency{:id :PLN :scale 1}) => true)
       (fact "when it's possible to check if a currency has assigned the given domain"
             (c/in-domain? :ISO-4217 #currency EUR) => true
             (c/in-domain? nil #currency crypto/ETH) => false
             (c/in-domain? nil #currency{:id :PLN :scale 1}) => true)
       (fact "when it's possible to check if a currency is auto-scaled"
             (c/big? #currency EUR) => false
             (c/big? #currency crypto/ETH) => false
             (c/big? #currency{:id :PLN :scale 1}) => false
             (c/big? #currency{:id :XXX :scale 1}) => false
             (c/big? #currency{:id :PLN}) => true
             (c/big? #currency{:id :XXX}) => true
             (c/big? #currency :XXX) => true)
       (fact "when it's possible to check if a currency is cryptocurrency"
             (c/crypto? #currency EUR) => false
             (c/crypto? #currency crypto/ETH) => true
             (c/crypto? #currency{:id :PLN :scale 1}) => false
             (c/crypto? #currency{:id :crypto/PLN :scale 1}) => true)
       (fact "when it's possible to check if a currency is an ISO currency"
             (c/iso? #currency EUR) => true
             (c/iso? #currency crypto/ETH) => false
             (c/iso? #currency{:id :PLN :scale 1}) => false
             (c/iso? #currency{:id :crypto/PLN :scale 1}) => false
             (c/iso? #currency{:id :ETH :scale 18 :domain :ISO-4217}) => true
             (c/iso? #currency{:id :PLN :scale 1 :domain :ISO-4217}) => true))

(facts "about currency localized properties"
       (fact "when it's possible to get a localized property"
             (c/localized-property :name :crypto/ETH :pl) => "Ether"
             (c/name :PLN :pl) => "złoty polski"
             (c/name :crypto/ETH :pl) => "Ether"
             (c/name :crypto/ETH :pl_PL) => "Ether"
             (c/name :crypto/ETH :en) => "Ether"
             (#{"EUR" "€"} (c/symbol :EUR :en_US)) => truthy
             (#{"EUR" "€"} (c/symbol :EUR :en)) => truthy
             (c/symbol :PLN :pl) => "zł"))
