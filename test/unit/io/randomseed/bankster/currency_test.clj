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
            [io.randomseed.bankster.money    :as                 m]
            [io.randomseed.bankster.currency :as                 c])

  (:import (io.randomseed.bankster Registry)))

(s/check-asserts true)

(defmacro map=
  [a b]
  `(let [v# ~a
         m# (into {} v#)
         m# (if (instance? io.randomseed.bankster.Currency v#)
              (assoc m# :weight (c/weight v#))
              m#)]
     (= m# ~b)))

(deftest new-currency
  (testing "when it returns nil for nil or empty map"
    (is (= (c/new nil) nil))
    (is (= (c/new {})  nil))
    (is (= (c/map->new {}) nil)))
  (testing "when it ignores registry-backed inline keys on ad-hoc map inputs"
    (let [cur (c/new {:id            :QQQ
                      :numeric        1
                      :scale          2
                      :domain         :ISO-4217
                      :countries      [:PL]
                      :localized      {:* {:name "Q"}}
                      :traits         [:token/erc20]
                      :propagate-keys [:foo]
                      :foo            "bar"})]
      (is (= :QQQ (:id cur)))
      (is (= "bar" (:foo cur)))
      (is (= false (contains? cur :countries)))
      (is (= false (contains? cur :localized)))
      (is (= false (contains? cur :traits)))
      (is (= false (contains? cur :propagate-keys)))))
  (testing "when it returns a currency object"
    (is (map= (c/new :crypto/EUR) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 2 :iso/fiat :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id c/auto-scaled :iso/fiat :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 c/auto-scaled :iso/fiat :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id 2 :iso/fiat :ISO-4217) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :domain :ISO-4217}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :crypto/EUR :domain :CRYPTO}) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :domain :ISO-4217}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2}) {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale c/auto-scaled}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :domain :ISO-4217 :scale 2 :kind :iso/fiat}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale c/auto-scaled :kind :iso/fiat}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :domain :ISO-4217 :kind :iso/fiat}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :domain :ISO-4217 :scale 2 :kind :iso/fiat}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale 2 :kind :iso/fiat}) {:id :EUR :domain nil :kind :iso/fiat :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new :EUR) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :crypto/EUR) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id c/auto-scaled) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id 2) {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new :EUR 1000 c/auto-scaled) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 2 :iso/fiat) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id c/auto-scaled :iso/fiat) {:id :EUR :domain nil :kind :iso/fiat :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new :EUR 1000 c/auto-scaled :iso/fiat) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new :EUR c/no-numeric-id 2 :iso/fiat) {:id :EUR :domain nil :kind :iso/fiat :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR}) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :crypto/EUR}) {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id}) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled}) {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale 2}) {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled}) {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale 2 :kind :iso/fiat}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale 2 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale c/auto-scaled :kind :iso/fiat}) {:id :EUR :domain nil :kind :iso/fiat :numeric -1 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric 1000 :scale c/auto-scaled :kind :iso/fiat}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale -1 :weight 0}))
    (is (map= (c/new {:id :EUR :numeric c/no-numeric-id :scale 2 :kind :iso/fiat}) {:id :EUR :domain nil :kind :iso/fiat :numeric -1 :scale 2 :weight 0}))))

(deftest currency-equality-ignores-weight
  (testing "core = / hash do not include weight"
    (let [a (c/new :X 10 2 :iso/fiat :ISO-4217 0)
          b (c/new :X 10 2 :iso/fiat :ISO-4217 7)]
      (is (= 0 (c/weight a)))
      (is (= 7 (c/weight b)))
      (is (= a b))
      (is (= (hash a) (hash b))))))

(deftest currency-tagged-literal
  (testing "when it returns nil for nil or empty map"
    (is (= #currency nil nil))
    (is (= #currency {} nil)))
  (testing "when it returns a currency object"
    (is (map= #currency [EUR] {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :crypto/EUR} {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000} {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2} {:id :EUR :domain nil :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :scale 2 :kind :iso/fiat} {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :kind :iso/fiat} {:id :EUR :domain nil :kind :iso/fiat :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :kind :iso/fiat} {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2 :kind :iso/fiat :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric -1 :scale 2 :weight 0})))
  (testing "when it can use different registries"
    (let [r (c/update (registry/get) #currency{:id PLN :scale 10})]
      (is (map= (c/with-registry r #currency PLN) {:id :PLN :scale 10 :numeric -1 :weight 0 :kind nil :domain nil})))))

(deftest currency-update-preserves-registry-traits
  (testing "updating a currency does not drop registry-level traits"
    (let [r0 (registry/new-registry)
          r1 (c/register r0 (c/new :AAA 1 2 :iso/fiat :ISO-4217))
          r1 (c/set-traits r1 :AAA #{:control/decentralized})
          r2 (c/update r1 (c/new :AAA 1 3 :iso/fiat :ISO-4217))]
      (is (= #{:control/decentralized}
             (registry/currency-id->traits* :AAA r2)))
      (is (true? (c/decentralized? :AAA r2))))))

(deftest currency-id-is-soft-with-dynamic-registry
  (testing "id does not throw for unknown currencies even when registry/*default* is bound"
    (let [r0 (registry/new-registry)
          r1 (c/register r0 (c/new :crypto/BTC))]
      (binding [registry/*default* r1]
        (is (= :crypto/BTC (c/id :BTC)))
        (is (= :BLABLA (c/id :BLABLA)))))))

(deftest currency-properties-are-soft-for-unknown-identifiers
  (testing "nr/sc/domain/kind/weight return nil (soft) when currency cannot be resolved"
    (binding [registry/*default* (registry/new-registry)]
      (is (= nil (c/nr :NOPE)))
      (is (= nil (c/sc :NOPE)))
      (is (= nil (c/domain :NOPE)))
      (is (= nil (c/kind :NOPE)))
      (is (= nil (c/weight :NOPE))))))

(deftest currency-registering
  (testing "when it returns nil for nil or empty map"
    (is (= #currency nil nil))
    (is (= #currency {} nil)))
  (testing "when it gets currency objects from a global registry"
    (is (map= #currency EUR {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= #currency :EUR {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= #currency "EUR" {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0})))
  (testing "when it creates an ad-hoc currency object"
    (is (map= #currency {:id :EUR} {:id :EUR :domain nil :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :crypto/EUR} {:id :crypto/EUR :domain :CRYPTO :kind nil :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 978 :domain nil} {:id :EUR :domain nil :kind nil :numeric 978 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind nil :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2 :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind nil :numeric -1 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :scale 2 :kind :iso/fiat :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale 2 :weight 0}))
    (is (map= #currency {:id :EUR :kind :iso/fiat :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric -1 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :numeric 1000 :kind :iso/fiat :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 1000 :scale -1 :weight 0}))
    (is (map= #currency {:id :EUR :scale 2 :kind :iso/fiat :domain :ISO-4217} {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric -1 :scale 2 :weight 0}))))

(deftest monetary-protocol
  (testing "when it can get ID of a currency"
    (is (= (c/id :PLN) :PLN))
    (is (= (c/id :crypto/ETH) :crypto/ETH))
    (is (= (c/id 'crypto/BTC) :crypto/BTC))
    (is (= (c/id "PLN") :PLN))
    (is (= (c/id #currency PLN) :PLN))
    (is (= (c/id #currency crypto/ETH) :crypto/ETH)))
  (testing "when it can resolve currency from a map"
    (is (map= (c/resolve {:id :eur}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:id :EUR}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:code :eur}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:code "EUR"}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:numeric "978"}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/resolve {:nr 978 :sc "2"}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (= (c/resolve {:numeric "ABC"}) nil))
    (is (= (c/resolve {:id :EUR :domain nil}) nil))
    (is (map= (c/resolve {:id :EUR :weight 0}) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
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
    (is (map= (c/unit #currency EUR) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit :EUR)          {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit "EUR")         {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit 978)           {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit {:id :eur})    {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
    (is (map= (c/unit {:id :EUR})    {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0})))
  (testing "unit-try is soft but preserves unit semantics for Currency inputs"
    (let [cur    (c/new :EUR 978 2 :iso/fiat :ISO-4217)
          cur'   (c/new :EUR 978 3 :iso/fiat :ISO-4217)
          reg    (registry/get)]
      (is (map= (c/unit-try :EUR) {:id :EUR :domain :ISO-4217 :kind :iso/fiat :numeric 978 :scale 2 :weight 0}))
      (is (nil? (c/unit-try :NOPE)))
      (is (identical? cur (c/unit-try cur nil)))
      (is (nil? (c/unit-try cur' reg))))))
  (testing "when it checks if a currency is defined"
    (is (= (c/defined? :PLN) true))
    (is (= (c/defined? :PPPP) false))
    (is (= (c/defined? "EUR") true))
    (is (= (c/defined? #currency crypto/ETH) true))
    (is (= (c/defined? 978) true))
    (is (= (c/defined? 10101010) false)))
  (testing "when it checks if a currency is present"
    (is (= (c/present? :PLN) true))
    (is (= (c/present? :PPPP) false))
    (is (= (c/present? #currency PLN) true))
    (is (= (c/present? #currency{:id :PLN :scale 1}) false))
    (is (= (c/present? (m/of :EUR 1)) true))
    (is (= (c/present? (m/of #currency{:id :PLN :scale 1} 1)) false))
    (let [jc (java.util.Currency/getInstance "EUR")]
      (is (= (c/defined? jc) true))
      (is (= (c/present? jc) true))
      (is (= (c/id jc) :EUR))))
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
    (is (= (c/same-ids? #currency crypto/USDT :PLN) false)))

(deftest currency-java
  (testing "when it converts currency to java.util.Currency"
    (let [^java.util.Currency eur (c/java :EUR)]
      (is (instance? java.util.Currency eur))
      (is (= "EUR" (.getCurrencyCode eur)))
      (is (= 978   (.getNumericCode eur)))
      (is (= 2     (.getDefaultFractionDigits eur))))
    (is (= (c/java (c/new :EUR 978 1 nil :ISO-4217)) nil))
    (is (= (c/java (c/new :EUR 999 2 nil :ISO-4217)) nil))
    (is (= (c/java (c/new :EUR 978 2 nil :CRYPTO)) nil))
    (is (= (c/java (c/new :EUR c/no-numeric-id 2 nil :ISO-4217)) nil))
    (is (= (c/java (c/new :EUR 978 c/auto-scaled nil :ISO-4217)) nil))
    (is (= (c/java (c/new :PPPP 123 2 nil :ISO-4217)) nil))))

(deftest currency-properties
  (testing "when it's possible to get the numeric value of an ISO currency"
    (is (= (c/nr #currency EUR) 978))
    (is (= (c/nr #currency crypto/ETH) nil))
    (is (= (c/nr #currency{:id :PLN :scale 1}) nil)))
  (testing "when it's possible to get scale of a monetary amount"
    (is (= (c/sc #currency EUR) 2))
    (is (= (c/sc #currency crypto/ETH) 18))
    (is (= (c/sc #currency XXX) c/auto-scaled))
    (is (= (c/sc #currency{:id :PLN :scale 1}) 1)))
  (testing "when it's possible to get the domain of a currency"
    (is (= (c/domain #currency EUR) :ISO-4217))
    (is (= (c/domain #currency crypto/ETH) :CRYPTO))
    (is (= (c/domain #currency{:id :PLN :scale 1}) nil)))
  (testing "when it's possible to get the kind of a currency"
    (is (= (c/kind #currency EUR) :iso/fiat))
    (is (= (c/kind #currency crypto/ETH) :virtual/native))
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
    (is (= (c/countries #currency{:id :PLN :scale 1}) #{:PL}))
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/countries #currency{:id :PPPP}))))
  (testing "when it gets localized properties"
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/localized-properties #currency{:id :PPPP}))))
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
    (is (= (c/definitive? {:id :EUR :domain :ISO-4217 :numeric 978 :scale 2 :kind :iso/fiat}) true))
    (is (= (c/definitive? {:id :EUR :do :ISO-4217 :nr 978 :sc 2 :ki :iso/fiat}) true))
    (is (= (c/definitive? {:id :EUR :domain nil :numeric nil :scale "2" :kind nil}) true))
    (is (= (c/definitive? {:id :EUR :domain nil :numeric nil :scale nil :kind nil}) true))
    (is (= (c/definitive? {:id :EUR :scale 2}) false))
    (is (= (c/definitive? {:id :PLN :domain nil :numeric nil :scale c/auto-scaled :kind nil}) true)))
  (testing "when it's possible to check if a currency has numeric ID"
    (is (= (c/has-numeric-id? #currency EUR) true))
    (is (= (c/has-numeric-id? #currency crypto/ETH) false))
    (is (= (c/has-numeric-id? #currency{:id :PLN :scale 1}) false)))
  (testing "when it's possible to check if a currency has particular kind"
    (is (= (c/of-kind? :iso/fiat #currency EUR) true))
    (is (= (c/of-kind? :iso #currency EUR) true))
    (is (= (c/of-kind? :virtual/native #currency crypto/ETH) true))
    (is (= (c/of-kind? :virtual #currency crypto/ETH) true))
    (is (= (c/of-kind? :iso #currency crypto/ETH) false))
    (is (= (c/of-kind? :iso/fiat #currency{:id :PLN :scale 1}) false)))
  (testing "when it's possible to check if a currency has any kind"
    (is (= (c/has-kind? #currency EUR) true))
    (is (= (c/has-kind? #currency crypto/ETH) true))
    (is (= (c/has-kind? #currency{:id :PLN :scale 1}) false)))
  (testing "when it's possible to check if a currency has a particular kind (exact match)"
    (is (= (c/has-kind? #currency EUR :iso/fiat) true))
    (is (= (c/has-kind? #currency EUR :iso) false))
    (is (= (c/has-kind? #currency{:id :PLN :scale 1} :iso/fiat) false)))
  (testing "when it's possible to check if a currency has assigned some country"
    (is (= (c/has-country? #currency EUR) true))
    (is (= (c/has-country? #currency crypto/ETH) false))
    (is (= (c/has-country? #currency{:id :PLN :scale 1}) true)))
  (testing "when it's possible to check if a currency has any domain"
    (is (= (c/has-domain? #currency EUR) true))
    (is (= (c/has-domain? #currency crypto/ETH) true))
    (is (= (c/has-domain? #currency{:id :PLN :scale 1}) false))
    ;; A nil second arg is treated as "use default registry" (presence check).
    (is (= (c/has-domain? #currency{:id :PLN :scale 1} nil) false)))
  (testing "when it's possible to check if a currency has a particular domain (exact match)"
    (is (= (c/has-domain? #currency EUR :ISO-4217) true))
    (is (= (c/has-domain? #currency EUR :CRYPTO) false))
    (is (= (c/has-domain? #currency crypto/ETH :CRYPTO) true)))
  (testing "when it's possible to check if a currency has assigned the given domain"
    (is (= (c/in-domain? :ISO-4217 #currency EUR) true))
    (is (= (c/in-domain? nil #currency crypto/ETH) false))
    (is (= (c/in-domain? nil #currency{:id :PLN :scale 1}) true))
    (is (= (c/of-domain? :ISO-4217 #currency EUR) true))
    (is (= (c/of-domain? nil #currency crypto/ETH) false))
    (is (= (c/of-domain? nil #currency{:id :PLN :scale 1}) true))
    (let [dom (derive (make-hierarchy) :ISO-4217-LEGACY :ISO-4217)
          r   (assoc (registry/new)
                     :hierarchies
                     (bankster/->CurrencyHierarchies dom (make-hierarchy) (make-hierarchy)))]
      (is (= (c/of-domain? :ISO-4217 {:id :OLD :domain :ISO-4217-LEGACY :numeric 1 :scale 2} r) true))
      (is (= (c/of-domain? :ISO-4217 {:id :CRY :domain :CRYPTO :numeric 1 :scale 2} r) false))))
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
    ;; iso? is based on :kind hierarchy (not only on :domain).
    (is (= (c/iso? #currency{:id :PLN :scale 1 :kind :iso/fiat}) true))
    (is (= (c/iso? #currency{:id :PLN :scale 1 :domain :ISO-4217}) false))))

(deftest currency-traits
  (testing "traits: presence, exact membership and hierarchy-aware checks"
    (is (= (c/has-trait? :crypto/USDT) true))
    ;; In the default config, most crypto assets have at least a native blockchain trait.
    (is (= (c/has-trait? :crypto/ETH) true))
    (is (= (c/has-trait? :PLN) false))
    ;; Exact membership only:
    (is (= (c/has-trait? :crypto/USDT :token/erc20) true))
    (is (= (c/has-trait? :crypto/USDT :token) false))
    ;; Hierarchy-aware predicate:
    (is (= (c/of-trait? :token :crypto/USDT) true))
    (is (= (c/of-trait? :privacy :crypto/XMR) true))
    (is (= (c/of-trait? :privacy :crypto/USDT) false))))

(deftest currency-decentralized-predicate
  (testing "decentralized? checks :control/decentralized trait"
    (let [r0 (registry/new)
          r1 (c/register r0 (c/new :AAA 1 2 :iso/fiat :ISO-4217))
          r2 (assoc-in r1 [:cur-id->traits :AAA] #{:control/decentralized})]
      (is (= true (c/decentralized?  :AAA r2)))
      (is (= true (c/decentralised? :AAA r2)))
      (is (= false (c/decentralized? :BBB r2))))))

(deftest currency-properties-localized
  (testing "when it's possible to get a localized property"
    (is (= (c/localized-property :name :crypto/ETH :pl) "Ether"))
    (is (= (c/name :PLN :pl) "złoty polski"))
    (is (= (c/name :crypto/ETH :pl) "Ether"))
    (is (= (c/name :crypto/ETH :pl_PL) "Ether"))
    (is (= (c/name :crypto/ETH :en) "Ether"))
    (is (#{"EUR" "€"} (c/symbol :EUR :en_US)))
    (is (#{"EUR" "€"} (c/symbol :EUR :en)))
    (is (= (c/symbol :PLN :pl) "zł"))
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/localized-property :name #currency{:id :PPPP} :pl)))))

(deftest unregister-removes-code-mapping
  (testing "unregister removes entries from :cur-code->curs for namespaced currency IDs"
    (let [r0  (registry/new)
          cur (c/new :crypto/AAA c/no-numeric-id 2 nil :CRYPTO 0)
      r1  (c/register r0 cur)
      r2  (c/unregister r1 cur)]
      ;; A plain code lookup should work after register...
      (is (= :crypto/AAA (.id (c/resolve :AAA r1))))
      ;; ...and must not work after unregister (regression for stale :cur-code->curs entry).
      (is (= nil (c/resolve :AAA r2)))
      (is (= nil (get (registry/currency-code->currencies* r2) :AAA))))))

(deftest currency-info
  (testing "info returns currency fields and registry-associated properties"
    (let [cur (c/new :AAA 123 2 :iso/fiat :ISO-4217 0)
          r0  (registry/new)
      r1  (-> r0
                  (c/register cur [:PL] {:* {:name "A"}})
                  (assoc :cur-id->traits {:AAA #{:token/erc20}}))]
      (is (= (c/info :AAA r1)
             {:id :AAA
              :numeric 123
              :scale 2
              :domain :ISO-4217
              :kind :iso/fiat
              :weight 0
              :countries #{:PL}
              :localized {:* {:name "A"}}
              :traits #{:token/erc20}}))
      ;; Missing registry-backed currency reference -> nil (soft behavior).
      (is (= nil (c/info :ZZZ r1)))
      ;; Ad-hoc currency object is still informative even if it's not in the registry.
      (let [m (c/info (c/new :QQQ 1 2 :iso/test :ISO-4217 0) r1)]
        (is (= :QQQ (:id m)))
        (is (= false (contains? m :countries)))
        (is (= false (contains? m :localized)))
        (is (= false (contains? m :traits)))))))

(deftest formatter-extended-max-fraction-digits
  (testing "formatter-extended sets max-fraction-digits without throwing"
    (let [^java.text.DecimalFormat f (c/formatter-extended :EUR :en_US {:max-fraction-digits 1})]
      (is (instance? java.text.DecimalFormat f))
      (is (= 1 (.getMaximumFractionDigits f)))
      ;; Use BigDecimal to avoid binary floating-point surprises with UNNECESSARY rounding.
      (is (string? (.format f 12.3M))))))

(deftest currency-to-map-parses-string-hints
  (testing "to-map parses :numeric/:scale/:weight from strings (regression)"
    (is (= (c/to-map {:id     :EUR
                      :numeric "978"
                      :scale   "2"
                      :weight  "5"
                      :domain  "iso-4217"
                      :kind    "fiat"})
           {:id :EUR
            :nr 978
            :sc 2
            :ki :fiat
            :do :ISO-4217
            :we 5}))
    (is (= (c/to-map {:id :EUR :scale "not-a-number"})
           {:id :EUR}))))

(deftest unit-map-kind-is-case-sensitive-and-may-be-namespaced
  (testing "unit resolves currencies using case-sensitive, namespaced :kind hints"
    (let [r (-> (registry/new)
                (c/register (c/new :XUA 965 0 :iso/funds :ISO-4217)))]
      (is (= :iso/funds (:kind (c/unit {:id :XUA :kind :iso/funds} r))))
      (is (= :iso/funds (:kind (c/unit {:id :XUA :kind "iso/funds"} r))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (c/unit {:id :XUA :kind "ISO/FUNDS"} r))))))

(deftest kind-of-uses-registry-hierarchy
  (testing "of-kind? consults registry :kind hierarchy (derived kinds)"
    (let [k  (derive (make-hierarchy) :iso/funds :funds)
          hs (bankster/->CurrencyHierarchies (make-hierarchy) k (make-hierarchy))
          r  (-> (registry/new)
                 (assoc :hierarchies hs)
                 (c/register (c/new :XUA 965 0 :iso/funds :ISO-4217)))]
      (is (= true (c/of-kind? :funds :XUA r)))
      (is (= true (c/funds? :XUA r))))))

(deftest register-allows-shared-numeric-id
  (testing "register allows multiple currencies for the same numeric ID; resolve picks the lowest weight"
    (let [r0 (registry/new)
          a  (c/new :AAA 999 2 nil :ISO-4217 10)
          b  (c/new :BBB 999 2 nil :ISO-4217 0)
          r1 (-> r0 (c/register a) (c/register b))]
      (is (= :BBB (.id (c/resolve 999 r1))))
      (is (= #{:AAA :BBB} (set (map c/id (c/resolve-all 999 r1))))))))

(deftest unregister-updates-canonical-shared-numeric-id
  (testing "unregister updates canonical mapping for shared numeric IDs"
    (let [r0 (registry/new)
          a  (c/new :AAA 999 2 nil :ISO-4217 10)
          b  (c/new :BBB 999 2 nil :ISO-4217 0)
          r1 (-> r0 (c/register a) (c/register b))
          r2 (c/unregister r1 b)]
      (is (= :AAA (.id (c/resolve 999 r2))))
      (is (= #{:AAA} (set (map c/id (c/resolve-all 999 r2)))))
      (let [r3 (c/unregister r2 a)]
        (is (= nil (c/resolve 999 r3)))
        (is (= nil (get (registry/currency-nr->currencies* r3) 999)))
        (is (= nil (get (registry/currency-nr->currency* r3) 999)))))))

(deftest unregister-updates-canonical-shared-code
  (testing "unregister updates canonical resolution for shared currency codes"
    (let [r0 (registry/new)
          a  (c/new :AAA        c/no-numeric-id 2 nil :ISO-4217 10)
          b  (c/new :crypto/AAA c/no-numeric-id 2 nil :CRYPTO   0)
          r1 (-> r0 (c/register a) (c/register b))]
      ;; When multiple currencies share the same code, `resolve` should pick the
      ;; lowest-weight one.
      (is (= :crypto/AAA (.id (c/resolve :AAA r1))))
      (is (= #{:AAA :crypto/AAA}
             (set (map c/id (get (registry/currency-code->currencies* r1) :AAA)))))

      ;; After unregistering the canonical one, the next one must be promoted.
      (let [r2 (c/unregister r1 b)]
        (is (= :AAA (.id (c/resolve :AAA r2))))
        (is (= #{:AAA}
               (set (map c/id (get (registry/currency-code->currencies* r2) :AAA)))))

        ;; After unregistering the last one, the code bucket key must disappear.
        (let [r3 (c/unregister r2 a)]
          (is (= nil (c/resolve :AAA r3)))
          (is (= nil (get (registry/currency-code->currencies* r3) :AAA))))))))

(deftest registry-weight-bases-are-synchronized
  (testing "weight base map and weighted buckets stay in sync when updating weights"
    (let [r0     (registry/new)
          a      (c/new :AAA        999 2 nil :ISO-4217 10)
          b      (c/new :crypto/AAA 999 2 nil :CRYPTO   0)
          r1     (-> r0
                     (c/register a [:PL])
                     (c/register b [:US]))
          id->w1 (registry/currency-id->weight* r1)]
      (is (= 10 (get id->w1 :AAA)))
      (is (false? (contains? id->w1 :crypto/AAA)))

      ;; Buckets must reflect base weights (missing entry == 0).
      (doseq [bucket [(get (registry/currency-code->currencies* r1) :AAA)
                      (get (registry/currency-nr->currencies*   r1) 999)]
              cur    bucket]
        (let [cid  (c/id cur)
              base (long (get id->w1 cid 0))]
          (is (= base (long (c/weight cur))))))

      (let [code-bucket (get (registry/currency-code->currencies* r1) :AAA)]
        (is (= :crypto/AAA (c/id (first code-bucket)))))
      (is (= :crypto/AAA (c/id (c/resolve 999 r1))))

      (let [r2     (c/set-weight r1 :AAA 0)
            r3     (c/set-weight r2 :crypto/AAA 20)
            id->w3 (registry/currency-id->weight* r3)]
        (is (contains? id->w3 :AAA))
        (is (= 0  (get id->w3 :AAA)))
        (is (= 20 (get id->w3 :crypto/AAA)))

        (let [code-bucket (get (registry/currency-code->currencies* r3) :AAA)]
          (is (= :AAA (c/id (first code-bucket)))))
        (is (= :AAA (c/id (c/resolve 999 r3))))

        ;; Country -> currency mapping should point to the same instance as cur-id->cur.
        (let [cur-aaa (registry/currency-id->currency* :AAA r3)]
          (is (identical? cur-aaa (registry/country-id->currency* :PL r3))))

        ;; Clearing explicit weight drops the base map entry (default still 0).
        (let [r4     (c/clear-weight r3 :AAA)
              id->w4 (registry/currency-id->weight* r4)]
          (is (false? (contains? id->w4 :AAA)))
          (let [code-bucket (get (registry/currency-code->currencies* r4) :AAA)]
            (is (= :AAA (c/id (first code-bucket))))))))))

(deftest weight-prefers-registry-base-when-provided
  (testing "currency/weight with a registry consults the registry weight base (source of truth)"
    (let [r0    (registry/new)
          r1    (c/register r0 (c/new :AAA 999 2 :iso/fiat :ISO-4217))
          r2    (c/set-weight r1 :AAA 10)
          ad-hoc (c/new :AAA 999 2 :iso/fiat :ISO-4217 5)]
      (is (= 5  (c/weight ad-hoc)))
      (is (= 10 (c/weight ad-hoc r2))))))

(deftest currency-auto-initialization-can-be-disabled
  (testing "binding bankster/*initialize-registry* to false prevents side-effectful registry init"
    (let [orig  (registry/state)
          empty (registry/new)]
      (try
        (registry/set! empty)
        (binding [bankster/*initialize-registry* false]
          (#'io.randomseed.bankster.currency/initialize-registry-if-enabled!))
        (is (identical? empty (registry/state)))
        (finally
          (registry/set! orig))))))
