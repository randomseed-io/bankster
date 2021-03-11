(ns

    ^{:doc    "bankster library, money tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.bankster.money-test

  (:require [clojure.spec.alpha              :as               s]
            [midje.sweet                     :refer         :all]
            [midje.experimental              :refer    [for-all]]
            [clojure.spec.gen.alpha          :as             gen]
            [orchestra.spec.test             :as              st]
            [io.randomseed.bankster          :as        bankster]
            [io.randomseed.bankster.spec     :as            spec]
            [expound.alpha                   :as         expound]
            [io.randomseed.bankster.currency :as               c]
            [io.randomseed.bankster.money    :as               m]
            [io.randomseed.bankster.scale    :as           scale])

  (:import [io.randomseed.bankster Currency Money]))

(s/check-asserts true)

(facts "about `of macro`"
       (fact "when it returns nil for nil as an amount"
             (m/of  nil nil) => nil
             (m/of  PLN nil)  => nil
             (m/of :PLN nil)  => nil
             (m/of nil :PLN)  => nil)
       (fact "when it returns a money object"
             (m/of PLN 10.11 scale/ROUND_DOWN) =>  {:amount 10.11M :currency #currency PLN}
             (m/of PLN 10.111111 scale/ROUND_DOWN) =>  {:amount 10.11M :currency #currency PLN}
             (m/of PLN 10.111111 scale/ROUND_UP) =>  {:amount 10.12M :currency #currency PLN}
             (m/of EUR 12.12) => {:amount 12.12M :currency #currency EUR}
             (m/of :EUR 12.12) => {:amount 12.12M :currency #currency EUR}
             (m/of crypto/ETH 1.00001) => {:amount 1.00001M :currency #currency :crypto/ETH}
             (m/of 1.00001 crypto/ETH) => {:amount 1.00001M :currency #currency :crypto/ETH}
             (m/of :12PLN)   => {:amount 12M :currency #currency PLN}
             (m/of :PLN12)   => {:amount 12M :currency #currency PLN}
             (m/of :12_PLN)  => {:amount 12M :currency #currency PLN}
             (m/of :PLN_12)  => {:amount 12M :currency #currency PLN}
             (m/of PLN12)    => {:amount 12M :currency #currency PLN}
             (m/of 12 PLN)   => {:amount 12M :currency #currency PLN}
             (m/of "12 PLN") => {:amount 12M :currency #currency PLN}
             (m/of "PLN 12") => {:amount 12M :currency #currency PLN}
             (m/of :12.00001PLN DOWN) => {:amount 12M :currency #currency PLN}
             (m/of :PLN12.111 UP)     => {:amount 12.12M :currency #currency PLN}
             (m/of PLN12.111 UP)      => {:amount 12.12M :currency #currency PLN}
             (m/of PLN_12.111 UP)     => {:amount 12.12M :currency #currency PLN}
             (m/of :PLN 12.111 UP)    => {:amount 12.12M :currency #currency PLN}
             (m/of "12.111 PLN" UP)   => {:amount 12.12M :currency #currency PLN}
             (m/of PLN) => {:amount 0M :currency #currency PLN}
             (m/with-currency EUR (m/of 1000)) => {:amount 1000M :currency #currency EUR}
             (m/of crypto/ETH) => {:amount 0M :currency #currency crypto/ETH}
             (let [mv #money[10 PLN]] (m/of mv))    => {:amount 10M :currency #currency PLN}
             (m/of #currency{:id :KIKI :scale 1} 5) => {:amount 5M :currency #currency{:id KIKI :scale 1}}
             (m/of #currency{:id :KIKI :scale 1})   => {:amount 0M :currency #currency{:id KIKI :scale 1}}
             (let [mv (m/of #currency{:id :KIKI :scale 1} 123)]
               (m/of mv 10) => {:amount 10M  :currency #currency{:id :KIKI :scale 1}}
               (m/of mv)    => {:amount 123M :currency #currency{:id :KIKI :scale 1}})))

(facts "about money tagged literal"
       (fact "when it returns nil for nil or empty map"
             #money nil => nil
             #money[nil nil] => nil
             #money[nil] => nil)
       (fact "when it returns a money object"
             #money PLN => {:amount 0M :currency #currency PLN}
             #money crypto/ETH => {:amount 0M :currency #currency crypto/ETH}
             #money[19 EUR] => (m/of 19 :EUR)
             #money[19 {:id :EUR :domain :ISO-4217}] => {:amount 19M :currency (c/of {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1})}
             #money[19 EUR]    => {:amount 19M :currency #currency EUR}
             #money[19 EUR]    => {:amount 19M :currency #currency {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2}}
             #money :19EUR     => {:amount 19M :currency #currency EUR}
             #money EUR_19.1   => {:amount 19.1M :currency #currency EUR}
             #money "19.1 EUR" => {:amount 19.1M :currency #currency EUR}
             #money/crypto ETH1.00000001   => {:amount 1.00000001M :currency #currency crypto/ETH}
             #money/crypto[ETH 1.00000001] => {:amount 1.00000001M :currency #currency crypto/ETH}
             #money/crypto[1.00000001 ETH] => {:amount 1.00000001M :currency #currency crypto/ETH}
             #money crypto/ETH1.00000001   => {:amount 1.00000001M :currency #currency crypto/ETH}))

(facts "about Monetary protocol"
       (fact "when it can get ID of a currency"
             (c/id #money[12.12 PLN]) => :PLN
             (c/id #money/crypto[12.12 ETH]) => :crypto/ETH)
       (fact "when it gets a currency unit from a registry or returns it when given directly"
             (c/unit #money[10 EUR]) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2})
       (fact "when it checks if a currency is defined"
             (c/defined? #money[5 EUR]) => true
             (c/defined? (m/of #currency{:id :KIKI :scale 1} 5)) => false
             (c/defined? (m/of #currency{:id :KIKI :scale 1})) => false)
       (fact "when it checks whether two currencies have the same IDs"
             (c/same-ids? #money[10 PLN] :PLN) => true
             (c/same-ids? :PLN #money[10 PLN]) => true
             (c/same-ids? #money[10 PLN] #money[10 PLN]) => true
             (c/same-ids? #currency crypto/USDT #money[20 EUR]) => false
             (c/same-ids? #currency crypto/USDT #money/crypto[20 USDT]) => true
             (c/same-ids? #money[10 PLN] :PLN) => true
             (c/same-ids? :PLN #money[10 PLN]) => true
             (c/same-ids? #money[10 PLN] #money[10 PLN]) => true))

(facts "about currency properties"
       (fact "when it's possible to get the numeric value of an ISO currency"
             (c/nr #money[12.32 EUR]) => 978
             (c/nr #money[1 crypto/ETH]) => nil
             (c/nr (m/of 10 #currency{:id :PLN :scale 1})) => nil)
       (fact "when it's possible to get scale of a monetary amount"
             (c/sc #money[1 EUR]) => 2
             (c/sc #money[1 crypto/ETH]) => 18
             (c/sc #money[1 XXX]) => nil
             (c/sc (m/of 10 #currency{:id :PLN :scale 1})) => 1)
       (fact "when it's possible to get the domain of a currency"
             (c/domain #money[1 EUR]) => :ISO-4217
             (c/domain #money[1 crypto/ETH]) => :CRYPTO
             (c/domain (m/of 10 #currency{:id :PLN :scale 1})) => nil)
       (fact "when it's possible to get the kind of a currency"
             (c/kind #money[1 EUR]) => :FIAT
             (c/kind #money[1 crypto/ETH]) => :DECENTRALIZED
             (c/kind (m/of 10 #currency{:id :PLN :scale 1})) => nil)
       (fact "when it's possible to get the code of a currency"
             (c/code #money[1 EUR]) => "EUR"
             (c/code #money[1 crypto/ETH]) => "crypto/ETH"
             (c/code (m/of 10 #currency{:id :PLN :scale 1})) => "PLN")
       (fact "when it's possible to get the short code of a currency"
             (c/short-code #money[1 EUR]) => "EUR"
             (c/short-code #money[1 crypto/ETH]) => "ETH"
             (c/short-code (m/of 10 #currency{:id :PLN :scale 1})) => "PLN")
       (fact "when it's possible to get the ID of a currency"
             (c/id #money[1 EUR]) => :EUR
             (c/id #money[1 crypto/ETH]) => :crypto/ETH
             (c/id (m/of 10 #currency{:id :PLN :scale 1})) => :PLN)
       (fact "when it's possible to get countries associated with a currency"
             (c/countries #money[1 USD]) => #{:TL :IO :PR :BQ :EC :VG :US :GU :AS :PW :TC :MP :VI :FM :MH :UM}
             (c/countries #money[1 crypto/ETH]) => nil
             (c/countries (m/of 10 #currency{:id :PLN :scale 1})) => #{:PL})
       (fact "when it's possible to distinguish money from the currency"
             (c/currency? #money[1 EUR]) => false)
       (fact "when it's possible to validate money as a currency representation"
             (c/possible? #money[1 EUR]) => true)
       (fact "when it's possible to check if a currency has numeric ID"
             (c/has-numeric-id? #money[1 EUR]) => true
             (c/has-numeric-id? #money[1 crypto/ETH]) => false
             (c/has-numeric-id? (m/of 10 #currency{:id :PLN :scale 1})) => false)
       (fact "when it's possible to check if a currency has any kind"
             (c/has-kind? #money[1 EUR]) => true
             (c/has-kind? #money[1 crypto/ETH]) => true
             (c/has-kind? (m/of 1 #currency{:id :PLN :scale 1})) => false)
       (fact "when it's possible to check if a currency has particular kind"
             (c/kind-of? #money[1 EUR] :FIAT) => true
             (c/kind-of? #money[1 crypto/ETH] :DECENTRALIZED) => true
             (c/kind-of? (m/of 1 #currency{:id :PLN :scale 1}) :FIAT) => false)
       (fact "when it's possible to check if a currency has assigned some country"
             (c/has-country? #money[1 EUR]) => true
             (c/has-country? #money[1 crypto/ETH]) => false
             (c/has-country? (m/of #currency{:id :PLN :scale 1} 1)) => true)
       (fact "when it's possible to check if a currency has assigned the given domain"
             (c/in-domain? :ISO-4217 #money[1 EUR]) => true
             (c/in-domain? nil #money[1 crypto/ETH]) => false
             (c/in-domain? nil (m/of 1 #currency{:id :PLN :scale 1})) => true)
       (fact "when it's possible to check if a currency is auto-scaled"
             (c/big? #money[1 EUR]) => false
             (c/big? #money/crypto[1 ETH]) => false
             (c/big? (m/of 1 #currency{:id :PLN :scale 1})) => false
             (c/big? (m/of 1 #currency{:id :XXX :scale 1})) => false
             (c/big? (m/of 1 #currency{:id :PLN})) => true
             (c/big? (m/of 1 #currency{:id :XXX})) => true
             (c/big? (m/of 1 #currency :XXX)) => true)
       (fact "when it's possible to check if a currency is cryptocurrency"
             (c/crypto? #money[1 EUR]) => false
             (c/crypto? #money/crypto[1 ETH]) => true
             (c/crypto? (m/of 1 #currency{:id :PLN :scale 1})) => false
             (c/crypto? (m/of #currency{:id :crypto/PLN :scale 1} 1)) => true)
       (fact "when it's possible to check if a currency is an ISO currency"
             (c/iso? #money[1 EUR]) => true
             (c/iso? #money/crypto[1 ETH]) => false
             (c/iso? (m/of 1 #currency{:id :PLN :scale 1})) => false
             (c/iso? (m/of #currency{:id :crypto/PLN :scale 1} 1)) => false
             (c/iso? (m/of 1 #currency{:id :ETH :scale 18 :domain :ISO-4217})) => true
             (c/iso? (m/of 1 #currency{:id :PLN :scale 1 :domain :ISO-4217})) => true)
       (fact "when it's possible to get a localized property"
             (c/localized-property :name #money[1 :crypto/ETH] :pl) => "Ether"
             (c/name #money[2 PLN] :pl) => "złoty polski"
             (c/name #money[1 :crypto/ETH] :pl) => "Ether"
             (c/name #money[0.1 :crypto/ETH] :pl_PL) => "Ether"
             (c/name #money[1.1 crypto/ETH] :en) => "Ether"
             (#{"EUR" "€"} (c/symbol #money[2.22 EUR] :en_US)) => truthy
             (#{"EUR" "€"} (c/symbol #money EUR :en)) => truthy
             (c/symbol #money PLN :pl) => "zł"))

(facts "about Accountable protocol"
       (fact "when it can create a monetary value based on a currency and an amount"
             (m/value :EUR) => #money[0 EUR]
             (m/value :EUR 12.34) => #money[12.34 EUR]
             (m/value :EUR 12.345 scale/ROUND_UP) => #money[12.35 EUR]
             (m/value 'EUR) => #money[0 EUR]
             (m/value 'EUR 12.34) => #money[12.34 EUR]
             (m/value 'EUR 12.345 scale/ROUND_UP) => #money[12.35 EUR]
             (m/value #currency :EUR) => #money[0 EUR]
             (m/value #currency :EUR 12.34) => #money[12.34 EUR]
             (m/value 12.34 :EUR) => #money[12.34 EUR]
             (m/value 12.34 #currency :EUR) => #money[12.34 EUR]
             (m/value 12.345 #currency :EUR scale/ROUND_UP) => #money[12.35 EUR]
             (m/with-currency EUR
               (m/value 12.34) => #money[12.34 EUR]
               (m/value 12.345 nil scale/ROUND_UP) => #money[12.35 EUR]))
       (m/value #currency :EUR) => #money[0 EUR]
       (fact "when it can create a monetary value based on other monetary value"
             (m/value #money[5 EUR]) => #money[5 EUR]
             (m/value #money[5 EUR] 12.34) => #money[12.34 EUR]
             (m/value #money[10 EUR] 12.345 scale/ROUND_UP) => #money[12.35 EUR]))

(facts "about calculations on monetary values"
       (fact "when it's possible to add monetary values"
             (m/add) => 0M
             (m/add #money[1.25 PLN]) => #money[1.25 PLN]
             (m/add #money[1.25 PLN] #money[1 PLN]) => #money[2.25 PLN]
             (m/add #money[10 PLN] #money[1.25 PLN] #money[1 PLN]) => #money[12.25 PLN])
       (fact "when it's possible to subtract monetary values"
             (m/sub #money[1.25 PLN]) => #money[-1.25 PLN]
             (m/sub #money[1.25 PLN] #money[1 PLN]) => #money[0.25 PLN]
             (m/sub #money[10 PLN] #money[1.25 PLN] #money[1 PLN]) => #money[7.75 PLN])
       (fact "when it's possible to divide monetary values"
             (m/div 2) => 0.5M
             (m/div #money[1 PLN] #money[4 PLN]) => 0.25M
             (m/div #money[1 PLN] 4M) => #money[0.25 PLN]
             (m/div #money[10 PLN] 2 2) => #money[2.50 PLN]
             (m/div #money[10 PLN] #money[2 PLN] 1) => 5M
             (m/div-scaled 2) => 0.5M
             (m/div-scaled #money[1 PLN] #money[4 PLN]) => 0.25M
             (m/div-scaled #money[1 PLN] 4M) => #money[0.25 PLN]
             (m/div-scaled #money[10 PLN] 2 2) => #money[2.50 PLN]
             (m/div-scaled #money[10 PLN] #money[2 PLN] 1) => 5M)
       (fact "when it's possible to multiply monetary values"
             (m/mul) => 1M
             (m/mul 2) => 2M
             (m/mul #money[2 PLN]) => #money[2 PLN]
             (m/mul #money[1.10 PLN] 4.1) => #money[4.51 PLN]
             (m/mul #money[1 PLN] 4.1 2) => #money[8.20 PLN]
             (m/mul #money[10 PLN] 0.5) => #money[5 PLN]
             (m/mul 2 #money[10 PLN]) => #money[20 PLN]
             (m/mul 2 0.5 #money[10 PLN]) => #money[10 PLN]
             (m/mul 2 0.5 2 #money[10 PLN]) => #money[20 PLN]
             (m/mul 2 0.5 2 #money[10 PLN] 2.5) => #money[50 PLN]
             (m/mul-scaled) => 1M
             (m/mul-scaled 2) => 2M
             (m/mul-scaled #money[2 PLN]) => #money[2 PLN]
             (m/mul-scaled #money[1.10 PLN] 4.1) => #money[4.51 PLN]
             (m/mul-scaled #money[1 PLN] 4.1 2) => #money[8.20 PLN]
             (m/mul-scaled #money[10 PLN] 0.5) => #money[5 PLN]
             (m/mul-scaled 2 #money[10 PLN]) => #money[20 PLN]
             (m/mul-scaled 2 0.5 #money[10 PLN]) => #money[10 PLN]
             (m/mul-scaled 2 0.5 2 #money[10 PLN]) => #money[20 PLN]
             (m/mul-scaled 2 0.5 2 #money[10 PLN] 2.5) => #money[50 PLN]))

(fact "about logical operations on monetary values"
      
      )
