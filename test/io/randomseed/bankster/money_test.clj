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
             (m/of :PLN12.111 UP) => {:amount 12.12M :currency #currency PLN}
             (m/of PLN12.111 UP) => {:amount 12.12M :currency #currency PLN}
             (m/of PLN_12.111 UP) => {:amount 12.12M :currency #currency PLN}
             (m/of :PLN 12.111 UP) => {:amount 12.12M :currency #currency PLN}
             (m/of "12.111 PLN" UP) => {:amount 12.12M :currency #currency PLN}
             (m/of PLN) => {:amount 0M :currency #currency PLN}
             (m/with-currency EUR (m/of 1000)) => {:amount 1000M :currency #currency EUR}
             (m/of crypto/ETH) => {:amount 0M :currency #currency crypto/ETH}
             (let [mv #money[10 PLN]] (m/of mv)) => {:amount 10M :currency #currency PLN}
             (m/of #currency{:id :KIKI :scale 1} 5) => {:amount 5M :currency #currency{:id KIKI :scale 1}}
             (m/of #currency{:id :KIKI :scale 1}) => {:amount 0M :currency #currency{:id KIKI :scale 1}}
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
             #money[19 EUR] => {:amount 19M :currency #currency EUR}
             #money[19 EUR] => {:amount 19M :currency #currency {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2}}
             #money :19EUR => {:amount 19M :currency #currency EUR}
             #money EUR_19.1 => {:amount 19.1M :currency #currency EUR}
             #money "19.1 EUR" => {:amount 19.1M :currency #currency EUR}
             #money/crypto ETH1.00000001 => {:amount 1.00000001M :currency #currency crypto/ETH}
             #money/crypto[ETH 1.00000001] => {:amount 1.00000001M :currency #currency crypto/ETH}
             #money/crypto[1.00000001 ETH] => {:amount 1.00000001M :currency #currency crypto/ETH}
             #money crypto/ETH1.00000001 => {:amount 1.00000001M :currency #currency crypto/ETH}))

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
