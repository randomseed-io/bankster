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
             (c/new :EUR) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc -1}
             (c/new :crypto/EUR) => {:id :crypto/EUR :do :CRYPTO :kind nil :nr -1 :sc -1}
             (c/new :EUR 1000) => {:id :EUR :do :ISO-4217 :kind nil :nr 1000 :sc -1}
             (c/new :EUR c/no-numeric-id) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc -1}
             (c/new :EUR c/no-numeric-id c/auto-scaled) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc -1}
             (c/new :EUR c/no-numeric-id 2) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc 2}
             (c/new :EUR 1000 c/auto-scaled) => {:id :EUR :do :ISO-4217 :kind nil :nr 1000 :sc -1}
             (c/new :EUR 1000 2 :FIAT) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr 1000 :sc 2}
             (c/new :EUR c/no-numeric-id c/auto-scaled :FIAT) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr -1 :sc -1}
             (c/new :EUR 1000 c/auto-scaled :FIAT) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr 1000 :sc -1}
             (c/new :EUR c/no-numeric-id 2 :FIAT) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr -1 :sc 2}
             (c/new {:id :EUR}) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc -1}
             (c/new {:id :crypto/EUR}) => {:id :crypto/EUR :do :CRYPTO :kind nil :nr -1 :sc -1}
             (c/new {:id :EUR :nr 1000}) => {:id :EUR :do :ISO-4217 :kind nil :nr 1000 :sc -1}
             (c/new {:id :EUR :nr c/no-numeric-id}) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc -1}
             (c/new {:id :EUR :nr c/no-numeric-id :sc c/auto-scaled}) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc -1}
             (c/new {:id :EUR :nr c/no-numeric-id :sc 2}) => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc 2}
             (c/new {:id :EUR :nr 1000 :sc c/auto-scaled}) => {:id :EUR :do :ISO-4217 :kind nil :nr 1000 :sc -1}
             (c/new {:id :EUR :nr 1000 :sc 2 :kind :FIAT}) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr 1000 :sc 2}
             (c/new {:id :EUR :nr c/no-numeric-id :sc c/auto-scaled :kind :FIAT}) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr -1 :sc -1}
             (c/new {:id :EUR :nr 1000 :sc c/auto-scaled :kind :FIAT}) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr 1000 :sc -1}
             (c/new {:id :EUR :nr c/no-numeric-id :sc 2 :kind :FIAT}) => {:id :EUR :do :ISO-4217 :kind :FIAT :nr -1 :sc 2}))

(facts "about currency tagged literal"
       (fact "when it returns nil for nil or empty map"
             #currency nil => nil
             #currency {} => nil)
       (fact "when it returns a currency object"
             #currency {:id :EUR} => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc -1}
             #currency {:id :crypto/EUR} => {:id :crypto/EUR :do :CRYPTO :kind nil :nr -1 :sc -1}
             #currency {:id :EUR :nr 1000} => {:id :EUR :do :ISO-4217 :kind nil :nr 1000 :sc -1}
             #currency {:id :EUR :sc 2} => {:id :EUR :do :ISO-4217 :kind nil :nr -1 :sc 2}
             #currency {:id :EUR :nr 1000 :sc 2 :kind :FIAT} => {:id :EUR :do :ISO-4217 :kind :FIAT :nr 1000 :sc 2}
             #currency {:id :EUR :kind :FIAT} => {:id :EUR :do :ISO-4217 :kind :FIAT :nr -1 :sc -1}
             #currency {:id :EUR :nr 1000 :kind :FIAT} => {:id :EUR :do :ISO-4217 :kind :FIAT :nr 1000 :sc -1}
             #currency {:id :EUR :sc 2 :kind :FIAT} => {:id :EUR :do :ISO-4217 :kind :FIAT :nr -1 :sc 2}))
