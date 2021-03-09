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
             (c/new :crypto/EUR) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1}
             (c/new :EUR 1000 2 :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2}
             (c/new :EUR c/no-numeric-id c/auto-scaled :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1}
             (c/new :EUR 1000 c/auto-scaled :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1}
             (c/new :EUR c/no-numeric-id 2 :FIAT :ISO-4217) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2}
             (c/new {:id :EUR :domain :ISO-4217}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1}
             (c/new {:id :crypto/EUR :domain :CRYPTO}) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric 1000 :domain :ISO-4217}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2}
             (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale c/auto-scaled}) => {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1}
             (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale 2 :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :domain :ISO-4217 :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2 :kind :FIAT}) => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2}

             (c/new :EUR) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1}
             (c/new :crypto/EUR) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1}
             (c/new :EUR 1000) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1}
             (c/new :EUR c/no-numeric-id) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1}
             (c/new :EUR c/no-numeric-id c/auto-scaled) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1}
             (c/new :EUR c/no-numeric-id 2) => {:id :EUR :domain nil :kind nil :numeric -1 :scale 2}
             (c/new :EUR 1000 c/auto-scaled) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1}
             (c/new :EUR 1000 2 :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale 2}
             (c/new :EUR c/no-numeric-id c/auto-scaled :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1}
             (c/new :EUR 1000 c/auto-scaled :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale -1}
             (c/new :EUR c/no-numeric-id 2 :FIAT) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale 2}
             (c/new {:id :EUR}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1}
             (c/new {:id :crypto/EUR}) => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric 1000}) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale 2}) => {:id :EUR :domain nil :kind nil :numeric -1 :scale 2}
             (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled}) => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1}
             (c/new {:id :EUR :numeric 1000 :scale 2 :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale 2}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1}
             (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale -1}
             (c/new {:id :EUR :numeric c/no-numeric-id :scale 2 :kind :FIAT}) => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale 2}))

(facts "about currency tagged literal"
       (fact "when it returns nil for nil or empty map"
             #currency nil => nil
             #currency {} => nil)
       (fact "when it returns a currency object"
             #currency {:id :EUR :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1}
             #currency {:id :crypto/EUR} => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1}
             #currency {:id :EUR :numeric 1000} => {:id :EUR :domain nil :kind nil :numeric 1000 :scale -1}
             #currency {:id :EUR :scale 2} => {:id :EUR :domain nil :kind nil :numeric -1 :scale 2}
             #currency {:id :EUR :numeric 1000 :scale 2 :kind :FIAT} => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale 2}
             #currency {:id :EUR :kind :FIAT} => {:id :EUR :domain nil :kind :FIAT :numeric -1 :scale -1}
             #currency {:id :EUR :numeric 1000 :kind :FIAT} => {:id :EUR :domain nil :kind :FIAT :numeric 1000 :scale -1}
             #currency {:id :EUR :scale 2 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2}))

(facts "about currency registering"
       (fact "when it returns nil for nil or empty map"
             #currency nil => nil
             #currency {} => nil)
       (fact "when it gets currency objects from a global registry"
             #currency EUR => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2}
             #currency :EUR => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2}
             #currency "EUR" => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 978 :scale 2})
       (fact "when it creates an ad-hoc currency object"
             #currency {:id :EUR} => {:id :EUR :domain nil :kind nil :numeric -1 :scale -1}
             #currency {:id :crypto/EUR} => {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1}
             #currency {:id :EUR :numeric 1000 :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1}
             #currency {:id :EUR :scale 2 :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2}
             #currency {:id :EUR :numeric 1000 :scale 2 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale 2}
             #currency {:id :EUR :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale -1}
             #currency {:id :EUR :numeric 1000 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric 1000 :scale -1}
             #currency {:id :EUR :scale 2 :kind :FIAT :domain :ISO-4217} => {:id :EUR :domain :ISO-4217 :kind :FIAT :numeric -1 :scale 2}))
