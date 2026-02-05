(ns

    ^{:doc    "bankster library, api tests."
      :author "Paweł Wilk"
      :added  "2.2.0"
      :no-doc true}

    io.randomseed.bankster.api-test

  (:require [clojure.test                        :refer [deftest testing is]]
            [io.randomseed.bankster.api          :as api]
            [io.randomseed.bankster.api.currency :as api-currency]
            [io.randomseed.bankster.api.money    :as api-money]
            [io.randomseed.bankster.api.ops      :as api-ops]
            [io.randomseed.bankster.api.registry :as api-registry]
            [io.randomseed.bankster.currency     :as currency]
            [io.randomseed.bankster.registry     :as registry]
            [io.randomseed.bankster.scale        :as scale])

  (:import (io.randomseed.bankster Currency Money Registry)))

(defn- mk-test-registry
  []
  (let [r0  (registry/new)
        eur (currency/new :EUR 978 2 :iso/fiat :ISO-4217 0)
        pln (currency/new :PLN 985 2 :iso/fiat :ISO-4217 0)
        xau (currency/new :XAU 959 -1 :iso/metal :ISO-4217 0)]
    (-> r0
        (currency/register eur)
        (currency/register pln)
        (currency/register xau))))

(defn- with-default-currency
  [^Registry r f]
  (registry/with r
    (try
      (currency/set-default! :EUR)
      (f)
      (finally
        (currency/unset-default!)))))

(deftest currency-api
  (let [r (mk-test-registry)]
    (with-default-currency
      r
      (fn []
        (testing "scale"
          (let [m    (api/money 1.23M :EUR r)
                cur  (api/currency :EUR r)
                auto (api/currency :XAU r)]
            (is (instance? java.math.BigDecimal (api/amount 1.2M)))
            (is (= 2 (.scale ^java.math.BigDecimal (api/amount 1.2M 2))))
            (is (= 2 (.scale ^java.math.BigDecimal
                             (api/amount 1.2M 2 java.math.RoundingMode/HALF_UP))))
            (is (= 2 (api/scale m)))
            (is (= 2 (api/scale cur)))
            (is (= currency/auto-scaled (api/scale auto)))
            (is (= 0 (api/scale 1))))
          (is (nil? (api/scale nil)))
          (is (= java.math.RoundingMode/HALF_UP
                 (api/with-rescaling :HALF_UP (scale/rounding-mode))))
          (is (true? (api/with-rescaling :HALF_UP scale/*each*)))
          (is (false? (api/auto-scaled? 1.2M)))
          (is (false? (api/auto-scaled? :EUR)))
          (is (true? (api/auto-scaled? :XAU)))
          (is (nil? (api/auto-scaled? nil))))

        (testing "currency (strict) arities and registry modes"
          (is (= :EUR (.id ^Currency (api/currency))))
          (is (= :EUR (.id ^Currency (api/currency :EUR))))
          (is (= :EUR (.id ^Currency (api/currency :EUR nil))))
          (is (= :EUR (.id ^Currency (api/currency :EUR true))))
          (is (= :EUR (.id ^Currency (api/currency :EUR r))))
          (is (= :EUR (.id ^Currency (api-currency/resolve))))
          (is (= :EUR (.id ^Currency (api-currency/resolve :EUR))))
          (is (= :EUR (.id ^Currency (api-currency/resolve :EUR nil))))
          (is (= :EUR (.id ^Currency (api-currency/resolve :EUR true))))
          (is (= :EUR (.id ^Currency (api-currency/resolve :EUR r))))
          (let [cur (api/currency :EUR r)]
            (is (identical? cur (api/currency cur)))
            (is (identical? cur (api/currency cur nil)))
            (is (= :EUR (.id ^Currency (api/currency cur r)))))
          (is (thrown? clojure.lang.ExceptionInfo (api/currency :NOPE r))))

        (testing "resolve-try (soft) arities and registry modes"
          (is (= :EUR (.id ^Currency (api-currency/resolve-try))))
          (is (= :EUR (.id ^Currency (api-currency/resolve-try :EUR))))
          (is (= :EUR (.id ^Currency (api-currency/resolve-try :EUR nil))))
          (is (= :EUR (.id ^Currency (api-currency/resolve-try :EUR true))))
          (is (= :EUR (.id ^Currency (api-currency/resolve-try :EUR r))))
          (api-registry/with r
            (is (= :EUR (.id ^Currency (api-currency/resolve-try :EUR true)))))
          (let [cur (api/currency :EUR r)]
          (is (identical? cur (api-currency/resolve-try cur)))
          (is (identical? cur (api-currency/resolve-try cur nil)))
          (is (= :EUR (.id ^Currency (api-currency/resolve-try cur r)))))
          (is (nil? (api-currency/resolve-try :NOPE r))))

        (testing "currency resolve-all"
          (let [matches (api-currency/resolve-all :EUR)]
            (is (set? matches))
            (is (= #{:EUR} (set (map currency/id matches)))))
          (let [matches (api-currency/resolve-all :EUR nil)]
            (is (set? matches))
            (is (= #{:EUR} (set (map currency/id matches)))))
          (let [matches (api-currency/resolve-all :EUR true)]
            (is (set? matches))
            (is (= #{:EUR} (set (map currency/id matches)))))
          (let [matches (api-currency/resolve-all :EUR r)]
            (is (set? matches))
            (is (= #{:EUR} (set (map currency/id matches)))))
          (is (nil? (api-currency/resolve-all :NOPE r))))

        (testing "currency selectors"
          (is (= 3 (count (api-currency/all))))
          (is (= 3 (count (api-currency/all nil))))
          (is (= 3 (count (api-currency/all true))))
          (is (= 3 (count (api-currency/all r))))
          (let [ids (set (map currency/id (api-currency/of-domain :ISO-4217)))]
            (is (= #{:EUR :PLN :XAU} ids)))
          (let [ids (set (map currency/id (api-currency/of-domain :ISO-4217 nil)))]
            (is (= #{:EUR :PLN :XAU} ids)))
          (let [ids (set (map currency/id (api-currency/of-domain :ISO-4217 true)))]
            (is (= #{:EUR :PLN :XAU} ids)))
          (let [ids (set (map currency/id (api-currency/of-domain :ISO-4217 r)))]
            (is (= #{:EUR :PLN :XAU} ids)))
          (let [ids (set (map currency/id (api-currency/of-kind :iso/fiat)))]
            (is (= #{:EUR :PLN} ids)))
          (let [ids (set (map currency/id (api-currency/of-kind :iso/fiat nil)))]
            (is (= #{:EUR :PLN} ids)))
          (let [ids (set (map currency/id (api-currency/of-kind :iso/fiat true)))]
            (is (= #{:EUR :PLN} ids)))
          (let [ids (set (map currency/id (api-currency/of-kind :iso/fiat r)))]
            (is (= #{:EUR :PLN} ids)))
          (is (nil? (api-currency/of-kind :nope r)))
          (is (nil? (api-currency/of-kind :nope))))

        (testing "currency selectors with default registry binding"
          (api-registry/with r
            (is (= 3 (count (api-currency/all true))))
            (let [matches (api-currency/resolve-all :EUR true)]
              (is (set? matches))
              (is (= #{:EUR} (set (map currency/id matches)))))
            (let [ids (set (map currency/id (api-currency/of-domain :ISO-4217 true)))]
              (is (= #{:EUR :PLN :XAU} ids)))
            (let [ids (set (map currency/id (api-currency/of-kind :iso/fiat true)))]
              (is (= #{:EUR :PLN} ids)))))

        (testing "currency selectors for empty registry"
          (api-registry/with (registry/new)
            (is (nil? (api-currency/of-kind :iso/fiat)))
            (is (nil? (api-currency/of-kind :iso/fiat true)))))

        (testing "currency macro alias"
          (is (= :EUR (.id ^Currency (api-currency/of :EUR))))
          (is (= :EUR (.id ^Currency (api-currency/of {:id :EUR})))))

        (testing "currency normalization"
          (is (= :EUR (api-currency/normalize :EUR)))
          (is (= :EUR (api-currency/normalize [:EUR])))
          (is (= {:id :EUR} (api-currency/normalize {:id "EUR"}))))

        (testing "currency context"
          (api-money/with-currency :PLN
            (is (= :PLN (api-currency/id (api/currency))))))

        (testing "currency context via api-currency/with"
          (api-currency/with :EUR
            (is (= :EUR (api-currency/id (api/currency))))))

        (testing "currency default context via api-currency/with-default"
          (api-currency/with-default :PLN
            (is (= :PLN (api-currency/id (api/currency))))))

        (testing "currency accessors and predicates"
          (is (registry/registry? (api-currency/default-registry)))
          (is (registry/registry? (api-currency/registry-or-default nil)))
          (is (registry/registry? (api-currency/registry-or-default true)))
          (is (identical? r (api-currency/registry-or-default r)))
          (is (= :EUR (api-currency/id :EUR)))
          (is (= :EUR (api-currency/id :EUR nil)))
          (is (= :EUR (api-currency/id :EUR true)))
          (is (= :EUR (api-currency/id :EUR r)))
          (is (= "EUR" (api-currency/id-str :EUR)))
          (is (= "ETH" (api-currency/code-str :crypto/eth)))
          (is (thrown? clojure.lang.ExceptionInfo (api-currency/id :NOPE r)))
          (is (= "EUR" (api-currency/code :EUR)))
          (is (= "EUR" (api-currency/code :EUR r)))
          (is (= 978 (api-currency/nr :EUR)))
          (is (= 978 (api-currency/nr :EUR r)))
          (is (= 2 (api-currency/scale :EUR)))
          (is (= 2 (api-currency/scale :EUR r)))
          (is (= :ISO-4217 (api-currency/domain :EUR)))
          (is (= :ISO-4217 (api-currency/domain :EUR r)))
          (is (= :iso/fiat (api-currency/kind :EUR)))
          (is (= :iso/fiat (api-currency/kind :EUR r)))
          (let [sym (api-currency/symbol :EUR)]
            (is (string? sym))
            (is (seq sym)))
          (let [sym (api-currency/symbol :EUR java.util.Locale/US)]
            (is (string? sym))
            (is (seq sym)))
          (let [sym (api-currency/symbol :EUR java.util.Locale/US r)]
            (is (string? sym))
            (is (seq sym)))
          (let [nm (api-currency/name :EUR)]
            (is (string? nm))
            (is (seq nm)))
          (let [nm (api-currency/name :EUR java.util.Locale/US)]
            (is (string? nm))
            (is (seq nm)))
          (let [nm (api-currency/name :EUR java.util.Locale/US r)]
            (is (string? nm))
            (is (seq nm)))
          (is (true? (api-currency/defined? :EUR)))
          (is (true? (api-currency/defined? :EUR r)))
          (is (false? (api-currency/defined? :NOPE r)))
          (is (true? (api-currency/present? :EUR)))
          (is (true? (api-currency/present? :EUR r)))
          (is (false? (api-currency/present? :NOPE r)))
          (is (true? (api-currency/possible? :EUR)))
          (is (true? (api-currency/possible? :EUR r)))
          (is (false? (api-currency/possible? nil)))
          (is (= 2 (api-currency/scale :EUR)))
          (is (= 2 (api-currency/scale :EUR r)))
          (is (= currency/auto-scaled (api-currency/scale :XAU r)))
          (is (false? (api-currency/auto-scaled? :EUR)))
          (is (true? (api-currency/currency? (api/currency :EUR r))))
          (is (false? (api-currency/currency? :EUR)))
          (is (true? (api-currency/auto-scaled? :XAU)))
          (is (true? (api-currency/auto-scaled? :XAU r)))
          (let [iso-cur (currency/new :ISO-TEST 999 2 :iso :ISO-4217 0)]
            (is (true? (api/iso-currency? iso-cur)))
            (is (true? (api/iso-currency? iso-cur r))))
          (is (true? (api-currency/definitive?
                       {:id :EUR :domain :ISO-4217 :numeric 978 :scale 2})))
          (is (false? (api-currency/definitive? :EUR))))

        (testing "currency trait predicates"
          (let [crypto (currency/new :crypto/TEST 0 8 :stable :CRYPTO 0)
                r'     (api-currency/add-traits r :EUR #{:control/decentralized})]
            (is (true? (api-currency/crypto? crypto)))
            (is (true? (api-currency/crypto? crypto r)))
            (is (true? (api-currency/stable? crypto)))
            (is (true? (api-currency/stable? crypto r)))
            (is (false? (api-currency/decentralized? :EUR)))
            (is (true? (api-currency/decentralized? :EUR r')))
            (is (false? (api-currency/decentralized? :PLN r')))
            (is (true? (api-currency/has-trait? :EUR :control/decentralized r')))
            (is (nil? (api-currency/has-trait? :PLN :control/decentralized r')))
            (is (true? (api-currency/of-trait? :control/decentralized :EUR r')))))

        (testing "currency registry operations"
          (let [r0  (registry/new)
                cur (currency/new :TEST/COIN 999 2 :test/coin :TEST 0)
                r1  (api-currency/register r0 cur)
                r2  (api-currency/unregister r1 cur)
                r3  (api-currency/set-traits r1 :TEST/COIN #{:trait/a})
                r4  (api-currency/add-traits r3 :TEST/COIN #{:trait/b})
                r5  (api-currency/remove-traits r4 :TEST/COIN #{:trait/a})]
            (is (true? (api-currency/present? :TEST/COIN r1)))
            (is (false? (api-currency/present? :TEST/COIN r2)))
            (is (true? (api-currency/has-trait? :TEST/COIN :trait/a r3)))
            (is (true? (api-currency/has-trait? :TEST/COIN :trait/b r4)))
            (is (false? (api-currency/has-trait? :TEST/COIN :trait/a r5)))))

        (testing "currency registry mutations"
          (let [orig (registry/state)
                cur  (currency/new :TEST/GLOBAL 998 2 :test/global :TEST 0)]
            (try
              (api-currency/register! cur)
              (is (true? (api-currency/present? :TEST/GLOBAL (registry/state))))
              (api-currency/set-traits! :TEST/GLOBAL #{:trait/x})
              (is (true? (api-currency/has-trait? :TEST/GLOBAL :trait/x (registry/state))))
              (api-currency/add-traits! :TEST/GLOBAL #{:trait/y})
              (is (true? (api-currency/has-trait? :TEST/GLOBAL :trait/y (registry/state))))
              (api-currency/remove-traits! :TEST/GLOBAL #{:trait/x})
              (is (false? (api-currency/has-trait? :TEST/GLOBAL :trait/x (registry/state))))
              (api-currency/unregister! cur)
              (is (false? (api-currency/present? :TEST/GLOBAL (registry/state))))
              (finally
                (registry/set! orig)))))

        (testing "currency info"
          (let [info (api/info :EUR)]
            (is (= :EUR (:id info))))
          (let [info (api/info :EUR r)]
            (is (= :EUR (:id info))))
          (let [info (api/info :EUR :pl r)]
            (is (= :EUR (:id info))))
          (let [info (api-currency/info :EUR)]
            (is (= :EUR (:id info))))
          (let [info (api-currency/info :EUR r)]
            (is (= :EUR (:id info))))
          (let [info (api-currency/info :EUR :pl r)]
            (is (= :EUR (:id info)))))

        (testing "currency serialization and parsing"
          (let [cur           (api/currency :EUR r)
                cur-map       (api-currency/->map cur)
                edn-str       (api-currency/->edn cur)
                edn-str'      (api-currency/->edn cur {:code-only? true})
                json-str      (api-currency/->json cur)
                json-str'     (api-currency/->json cur {:code-only? true})
                json-text-id  (str "\"" json-str "\"")
                json-text-map (str "{\"id\":\"" json-str "\"}")]
            (is (map? cur-map))
            (is (string? edn-str))
            (is (string? edn-str'))
            (is (string? json-str))
            (is (string? json-str'))
            (is (nil? (api-money/->edn nil)))
            (is (nil? (api-money/->edn nil {})))
            (api-registry/with r
              (is (= :EUR (api-currency/id (api-currency/from-edn cur-map))))
              (is (= :EUR (api-currency/id (api-currency/from-edn edn-str))))
              (is (= :EUR (api-currency/id (api-currency/from-edn-text edn-str))))
              (is (= :EUR (api-currency/id (api-currency/from-edn :EUR))))
              (is (= :EUR (api-currency/id (api-currency/from-json json-str))))
              (is (= :EUR (api-currency/id (api-currency/from-json {:id :EUR}))))
              (is (= :EUR (api-currency/id (api-currency/from-json-text json-text-id))))
              (is (= :EUR (api-currency/id (api-currency/from-json-text json-text-map))))
              (is (nil? (api-currency/from-edn nil)))
              (is (nil? (api-currency/from-edn-text nil)))
              (is (nil? (api-currency/from-json nil)))
              (is (nil? (api-currency/from-json-text nil))))
            (is (= :EUR (api-currency/id (api-currency/from-edn edn-str {:registry r}))))
            (is (= :EUR (api-currency/id (api-currency/from-edn-text edn-str {:registry r}))))
            (is (= :EUR (api-currency/id (api-currency/from-json json-str {:registry r}))))
            (is (thrown? clojure.lang.ExceptionInfo (api-currency/from-edn 42)))
            (is (thrown? clojure.lang.ExceptionInfo (api-currency/from-edn-text 42)))
            (is (thrown? clojure.lang.ExceptionInfo (api-currency/from-json 42)))
            (is (thrown? clojure.lang.ExceptionInfo (api-currency/from-json-text 42)))))))) 

    (testing "currency registry true without default binding"
      (binding [registry/*default* nil]
        (is (instance? Currency (api-currency/resolve-try :EUR true)))
        (is (contains? (set (map currency/id (api-currency/resolve-all :EUR true))) :EUR))
        (is (contains? (set (map currency/id (api-currency/all true))) :EUR))
        (is (contains? (set (map currency/id (api-currency/of-domain :ISO-4217 true))) :EUR))
        (is (contains? (set (map currency/id (api-currency/of-kind :iso/fiat true))) :EUR))
        (is (= :EUR (api-currency/id :EUR true))))))

(deftest money-api
  (let [r (mk-test-registry)]
    (with-default-currency
      r
      (fn []
        (testing "money arities and branches"
          (is (instance? Money (api/money)))
          (is (instance? Money (api/money 1)))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR r)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR :HALF_UP)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR nil)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR :HALF_UP r)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api/money 1 :EUR nil r)))))
          (is (instance? Money (api-money/resolve)))
          (is (instance? Money (api-money/resolve 1)))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR r)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR :HALF_UP)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR nil)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR nil nil)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR nil true)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR :HALF_UP r)))))
          (is (= :EUR (.id ^Currency (.currency ^Money (api-money/resolve 1 :EUR nil r))))))

        (testing "money macro alias"
          (let [m (api-money/of 1 :EUR)]
            (is (= :EUR (.id ^Currency (.currency ^Money m))))))
        (testing "money major/minor macro aliases"
          (let [m1 (api-money/major-of 1 :EUR)
                m2 (api-money/minor-of 1 :EUR)]
            (is (= :EUR (.id ^Currency (.currency ^Money m1))))
            (is (= :EUR (.id ^Currency (.currency ^Money m2))))))

        (testing "try-money arities and branches"
          (is (instance? Money (api/money-try)))
          (is (instance? Money (api/money-try 1)))
          (is (nil? (api/money-try 1 :NOPE)))
          (is (instance? Money (api/money-try 1 :EUR :HALF_UP)))
          (is (instance? Money (api/money-try 1 :EUR :HALF_UP r)))
          (is (instance? Money (api/money-try 1 :EUR)))
          (is (nil? (api/money-try 1 nil)))
          (is (nil? (api/money-try 1 :NOPE)))
          (is (instance? Money (api/money-try 1 :EUR r)))
          (is (instance? Money (api/money-try 1 :EUR :HALF_UP)))
          (is (nil? (api/money-try 1 :NOPE :HALF_UP)))
          (is (instance? Money (api/money-try 1 :EUR nil)))
          (is (nil? (api/money-try 1 nil nil)))
          (is (nil? (api/money-try 1 :NOPE nil)))
          (is (instance? Money (api/money-try 1 :EUR :HALF_UP r)))
          (is (instance? Money (api/money-try 1 :EUR nil r)))
          (is (instance? Money (api/money-try 1 :EUR nil nil)))
          (is (instance? Money (api/money-try 1 :EUR nil true)))
          (is (nil? (api/money-try 1 :NOPE nil r)))
          (is (nil? (api/money-try 1 :NOPE r))))

        (testing "money internal helpers"
          (is (instance? Currency (api/currency-try :EUR nil)))
          (is (instance? Currency (api/currency-try :EUR true))))

        (testing "money of-registry and cast"
          (let [m  (api/money 1.23M :EUR r)
                m' (api-money/of-registry r m)]
            (is (instance? Money m'))
            (is (identical? (api/currency :EUR r)
                            (.currency ^Money m'))))
          (let [m (api/money 1.23M :EUR r)]
            (is (instance? Money (api-money/of-registry m)))
            (is (instance? Money (api-money/of-registry r m :HALF_UP)))
            (is (instance? Money (api-money/of-registry r m nil))))
          (let [m  (api/money 1 :EUR r)
                m2 (api-money/cast m :PLN)]
            (is (= :PLN (.id ^Currency (.currency ^Money m2)))))
          (let [m (api/money 1 :EUR r)]
            (is (instance? Money (api-money/cast m)))
            (is (instance? Money (api-money/cast m :PLN :HALF_UP)))
            (is (instance? Money (api-money/cast m :PLN nil)))
            (is (instance? Money (api-money/cast-try m)))
            (is (instance? Money (api-money/cast-try m :PLN :HALF_UP)))
            (is (instance? Money (api-money/cast-try m :PLN nil))))
          (is (nil? (api-money/cast-try (api/money 1 :EUR r) :NOPE)))
          (is (nil? (api-money/cast-try (api/money 1 :EUR r) :NOPE :HALF_UP))))

        (testing "money major/minor constructors"
          (is (instance? Money (api-money/major 1)))
          (is (instance? Money (api-money/major 1 :EUR)))
          (is (instance? Money (api-money/major 1 :EUR :HALF_UP)))
          (is (instance? Money (api-money/major 1 :EUR nil)))
          (is (instance? Money (api-money/major 1 :EUR r)))
          (is (instance? Money (api-money/major 1 :EUR :HALF_UP r)))
          (is (instance? Money (api-money/major 1 :EUR nil r)))
          (is (instance? Money (api-money/minor 1)))
          (is (instance? Money (api-money/minor 1 :EUR)))
          (is (instance? Money (api-money/minor 1 :EUR :HALF_UP)))
          (is (instance? Money (api-money/minor 1 :EUR nil)))
          (is (instance? Money (api-money/minor 1 :EUR r)))
          (is (instance? Money (api-money/minor 1 :EUR :HALF_UP r)))
          (is (instance? Money (api-money/minor 1 :EUR nil r))))

        (testing "money accessors"
          (let [m (api/money 1.2300M :EUR r)]
            (is (true? (api-money/money? m)))
            (is (instance? java.math.BigDecimal (api-money/amount m)))
            (is (instance? java.math.BigDecimal (api-money/amount 1 :EUR)))
            (is (instance? java.math.BigDecimal
                           (api-money/amount 1 :EUR java.math.RoundingMode/HALF_UP)))
            (is (instance? Currency (api-money/currency m)))
            (is (instance? Currency (api-money/currency 1 :EUR)))
            (is (instance? Currency
                           (api-money/currency 1 :EUR java.math.RoundingMode/HALF_UP)))
            (is (= 2 (api-money/scale m)))
            (is (instance? Money (api-money/scale m 3)))
            (is (instance? Money (api-money/scale m 3 java.math.RoundingMode/HALF_UP)))
            (is (false? (api-money/auto-scaled? m)))
            (is (instance? Money (api-money/strip m)))
            (is (nil? (api-money/strip nil)))
            (is (string? (api-money/format m java.util.Locale/US)))
            (is (string? (api-money/format m)))
            (is (string? (api-money/format m java.util.Locale/US {})))
            (let [info (api/info m)]
              (is (= :EUR (.id ^Currency (:currency info))))
              (is (= (.amount ^Money m) (:amount info))))
            (let [info (api/info m r)]
              (is (= :EUR (.id ^Currency (:currency info)))))
            (let [info (api/info m :pl r)]
              (is (= :EUR (.id ^Currency (:currency info)))))
            (let [info (api-money/info m)]
              (is (= :EUR (.id ^Currency (:currency info))))
              (is (= (.amount ^Money m) (:amount info))))))

        (testing "money normalization"
          (let [m (api-money/normalize {:currency :EUR :amount "12.30"})]
            (is (instance? Money m))
            (is (= :EUR (.id ^Currency (.currency ^Money m)))))
          (let [m (api-money/normalize 12.30M)]
            (is (instance? Money m))
            (is (= :EUR (.id ^Currency (.currency ^Money m)))))
          (is (instance? Money (api-money/normalize 1 :EUR)))
          (is (instance? Money (api-money/normalize 1 :EUR :HALF_UP)))
          (is (nil? (api-money/normalize nil)))
          (is (thrown? clojure.lang.ArityException
                       (api-money/normalize 1 :EUR :HALF_UP :EXTRA))))

        (testing "money parsing helpers"
          (let [m (api-money/parse 12.30M)]
            (is (instance? Money m))
            (is (= :EUR (.id ^Currency (.currency ^Money m)))))
          (let [m (api-money/parse-major 12)]
            (is (instance? Money m)))
          (let [m (api-money/parse-minor 123)]
            (is (instance? Money m)))
          (let [m (api-money/parse :EUR "12.30")]
            (is (instance? Money m)))
          (let [m (api-money/parse :EUR "12.345" :HALF_UP)]
            (is (instance? Money m)))
          (let [m (api-money/parse :EUR "12.30" nil)]
            (is (instance? Money m)))
          (let [m (api-money/parse-major :EUR "12")]
            (is (instance? Money m)))
          (let [m (api-money/parse-major :EUR "12" :HALF_UP)]
            (is (instance? Money m)))
          (let [m (api-money/parse-major :EUR "12" nil)]
            (is (instance? Money m)))
          (let [m (api-money/parse-minor :EUR "1234")]
            (is (instance? Money m)))
          (let [m (api-money/parse-minor :EUR "1234" :HALF_UP)]
            (is (instance? Money m)))
          (let [m (api-money/parse-minor :EUR "1234" nil)]
            (is (instance? Money m))))

        (testing "money rounding and allocation"
          (scale/with-rounding java.math.RoundingMode/HALF_UP
            (is (= java.math.RoundingMode/HALF_UP (api-money/rounding-mode))))
          (is (= java.math.RoundingMode/HALF_UP
                 (api-money/rounding-mode java.math.RoundingMode/HALF_UP)))
          (is (registry/registry? (api-money/default-registry)))
          (is (registry/registry? (api-money/registry-or-default nil)))
          (is (registry/registry? (api-money/registry-or-default true)))
          (is (identical? r (api-money/registry-or-default r)))
          (is (instance? java.math.BigDecimal (api-money/scale-apply 1.23M)))
          (is (= 2 (.scale ^java.math.BigDecimal (api-money/scale-apply 1.23M 2))))
          (is (= 2 (.scale ^java.math.BigDecimal
                           (api-money/scale-apply 1.23M 2 java.math.RoundingMode/HALF_UP))))
          (let [m (api/money 1.23M :EUR r)]
            (is (instance? Money (api-money/round-to m)))
            (is (instance? Money (api-money/round-to m 0.05M)))
            (is (instance? Money (api-money/round-to m 0.05M :HALF_UP))))
          (let [m     (api/money 10.00M :EUR r)
                parts (api-money/allocate m [1 1 1])]
            (is (= 3 (count parts)))
            (is (every? api-money/money? parts)))
          (let [m     (api/money 10.00M :EUR r)
                parts (api-money/distribute m 3)]
            (is (= 3 (count parts)))
            (is (every? api-money/money? parts))))

        (testing "money arithmetic and comparisons"
          (let [m0   (api/money 0 :EUR r)
                m1   (api/money 1 :EUR r)
                m2   (api/money 2 :EUR r)
                mneg (api/money -1 :EUR r)]
            (is (= 0M (api-money/add)))
            (is (instance? Money (api-money/add m1 m2)))
            (is (instance? Money (api-money/sub m2 m1)))
            (is (instance? Money (api-money/mul m1 2)))
            (is (instance? Money (api-money/div m2 2)))
            (is (true? (api-money/eq? m1 (api/money 1 :EUR r))))
            (is (true? (api-money/ne? m1 m2)))
            (is (true? (api-money/gt? m2 m1)))
            (is (true? (api-money/ge? m2 m2)))
            (is (true? (api-money/lt? m1 m2)))
            (is (true? (api-money/le? m1 m1)))
            (is (zero? (api-money/compare m1 m1)))
            (is (true? (api-money/pos? m2)))
            (is (true? (api-money/neg? mneg)))
            (is (true? (api-money/zero? m0)))))

        (testing "money predicates"
          (let [m0 (api/money 0 :EUR r)
                m1 (api/money 1 :EUR r)
                m2 (api/money 2 :EUR r)
                m3 (api/money 1 :PLN r)]
            (is (true? (api-money/zero? m0)))
            (is (false? (api-money/zero? m1)))
            (is (true? (api-money/same-currencies? m1 m2)))
            (is (false? (api-money/same-currencies? m1 m3)))))

        (testing "money serialization and parsing"
          (let [m             (api/money 12.30M :EUR r)
                m-map         (api-money/->map m)
                edn-str       (api-money/->edn m)
                edn-str'      (api-money/->edn m {:code-only? true})
                json-str      (api-money/->json m)
                json-str'     (api-money/->json m {:code-only? true})
                json-text-id  (str "\"" json-str "\"")
                json-text-map (str "{\"currency\":\"EUR\",\"amount\":\""
                                   (.toPlainString ^java.math.BigDecimal (.amount ^Money m))
                                   "\"}")]
            (is (map? m-map))
            (is (string? edn-str))
            (is (string? edn-str'))
            (is (string? json-str))
            (is (string? json-str'))
            (api-registry/with r
              (is (instance? Money (api-money/from-edn m-map)))
              (is (instance? Money (api-money/from-edn edn-str)))
              (is (instance? Money (api-money/from-edn-text edn-str)))
              (is (instance? Money (api-money/from-json json-str)))
              (is (instance? Money (api-money/from-json m-map)))
              (is (instance? Money (api-money/from-json-text json-text-id)))
              (is (instance? Money (api-money/from-json-text json-text-map)))
              (is (nil? (api-money/from-edn nil)))
              (is (nil? (api-money/from-edn-text nil)))
              (is (nil? (api-money/from-json nil)))
              (is (nil? (api-money/from-json-text nil))))
            (is (instance? Money (api-money/from-edn edn-str {:registry r})))
            (is (instance? Money (api-money/from-edn-text edn-str {:registry r})))
            (is (instance? Money (api-money/from-json json-str {:registry r})))
            (is (thrown? clojure.lang.ExceptionInfo (api-money/from-edn 42)))
            (is (thrown? clojure.lang.ExceptionInfo (api-money/from-edn-text 42)))
            (is (thrown? clojure.lang.ExceptionInfo (api-money/from-json 42)))
            (is (thrown? clojure.lang.ExceptionInfo (api-money/from-json-text 42))))))))

    (testing "money registry true without default binding"
      (binding [registry/*default* nil]
        (is (registry/registry? (api-money/registry-or-default true)))
        (is (instance? Money (api-money/resolve 1 :EUR true)))
        (is (instance? Money (api-money/resolve-try 1 :EUR true))))))

(deftest try-money-without-default
  (currency/unset-default!)
  (is (nil? (api/money-try)))
  (is (nil? (api/money-try 1)))
  (is (= 0 (api/scale 1))))

(deftest registry-helpers
  (let [r    (mk-test-registry)
        orig (registry/state)]
    (try
      (registry/set! r)
      (is (identical? r (api/default-registry)))
      (is (identical? r (api/registry-or-default true)))
      (is (identical? r (api/registry-or-default nil)))
      (is (identical? r (api/registry-or-default r)))
      (is (identical? r (api-registry/default)))
      (is (identical? r (api-registry/or-default true)))
      (is (identical? r (api-registry/or-default nil)))
      (is (identical? r (api-currency/default-registry)))
      (is (identical? r (api-money/default-registry)))
      (is (= :EUR (.id ^Currency (api/currency :EUR true))))
      (finally
        (registry/set! orig)))
    (api-registry/with r
      (is (identical? r (api/default-registry)))
      (is (identical? r (api/registry-or-default true)))
      (is (identical? r (api/registry-or-default nil)))
      (is (identical? r (api-registry/default)))
      (is (identical? r (api-registry/or-default true)))
      (is (identical? r (api-registry/or-default nil)))
      (is (identical? r (api-registry/or-default r)))
      (is (identical? r (api-currency/default-registry)))
      (is (identical? r (api-money/default-registry)))
      (is (= :EUR (.id ^Currency (api/currency :EUR)))))
    (api-money/with-registry r
      (is (identical? r (api/default-registry)))
      (is (= :EUR (.id ^Currency (api/currency :EUR)))))
    (api-currency/with-registry r
      (is (identical? r (api/default-registry)))
      (is (= :EUR (.id ^Currency (api/currency :EUR)))))))

(deftest registry-true-uses-global
  (let [r    (mk-test-registry)
        orig (registry/state)
        ids  (fn [coll] (set (map currency/id coll)))]
    (try
      (registry/set! r)
      (binding [registry/*default* nil]
        (testing "currency registry true uses global"
          (is (= (ids (api-currency/resolve-all :EUR true))
                 (ids (api-currency/resolve-all :EUR r))))
          (is (= (ids (api-currency/all true))
                 (ids (api-currency/all r))))
          (is (= (ids (api-currency/of-domain :ISO-4217 true))
                 (ids (api-currency/of-domain :ISO-4217 r))))
          (is (= (ids (api-currency/of-kind :iso/fiat true))
                 (ids (api-currency/of-kind :iso/fiat r))))
          (is (= (api-currency/code :EUR true)
                 (api-currency/code :EUR r)))
          (is (= (api-currency/nr :EUR true)
                 (api-currency/nr :EUR r)))
          (is (= (api-currency/auto-scaled? :XAU true)
                 (api-currency/auto-scaled? :XAU r)))
          (is (= (api-currency/info :EUR true)
                 (api-currency/info :EUR r)))
          (is (= (api-currency/domain :EUR true)
                 (api-currency/domain :EUR r)))
          (is (= (api-currency/kind :EUR true)
                 (api-currency/kind :EUR r)))
          (is (= (api-currency/symbol :EUR java.util.Locale/US true)
                 (api-currency/symbol :EUR java.util.Locale/US r)))
          (is (= (api-currency/name :EUR java.util.Locale/US true)
                 (api-currency/name :EUR java.util.Locale/US r)))
          (is (= (api-currency/defined? :EUR true)
                 (api-currency/defined? :EUR r)))
          (is (= (api-currency/present? :EUR true)
                 (api-currency/present? :EUR r)))
          (is (= (api-currency/possible? :EUR true)
                 (api-currency/possible? :EUR r)))
          (is (= (api-currency/crypto? :EUR true)
                 (api-currency/crypto? :EUR r)))
          (is (= (api-currency/stable? :EUR true)
                 (api-currency/stable? :EUR r)))
          (is (= (api-currency/decentralized? :EUR true)
                 (api-currency/decentralized? :EUR r))))

        (testing "money registry true uses global"
          (is (= :EUR (.id ^Currency
                           (.currency ^Money (api-money/resolve 1 :EUR true)))))
          (is (= :EUR (.id ^Currency
                           (.currency ^Money (api-money/resolve-try 1 :EUR true)))))
          (let [m (api/money 1 :EUR r)]
            (is (= (api-money/of-registry true m)
                   (api-money/of-registry r m))))
          (is (= :EUR (.id ^Currency
                           (.currency ^Money (api-money/major 1 :EUR true)))))
          (is (= :EUR (.id ^Currency
                           (.currency ^Money (api-money/minor 1 :EUR true)))))))
      (finally
        (registry/set! orig)))))

(deftest registry-api
  (let [orig (registry/state)
        r    (mk-test-registry)]
    (try
      (is (identical? (api-registry/state) (registry/state)))
      (let [r1 (api-registry/hierarchy-derive :traits :test/a :test/b r)]
        (is (isa? (registry/hierarchy :traits r1) :test/a :test/b)))
      (let [r2 (api-registry/hierarchy-derive :traits :test/c :test/d true)]
        (is (isa? (registry/hierarchy :traits r2) :test/c :test/d)))
      (api-registry/with r
        (let [r3 (api-registry/hierarchy-derive :traits :test/e :test/f true)]
          (is (isa? (registry/hierarchy :traits r3) :test/e :test/f))))
      (api-registry/hierarchy-derive! :traits :test/x :test/y)
      (is (isa? (registry/hierarchy :traits (registry/state)) :test/x :test/y))
      (finally
        (registry/set! orig)))))

(deftest rounding-alias
  (is (nil? (api/rounding-mode)))
  (is (nil? (api-money/rounding-mode)))
  (is (= java.math.RoundingMode/UP
         (api/rounding-mode java.math.RoundingMode/UP)))
  (is (= java.math.RoundingMode/UP
         (api-money/rounding-mode java.math.RoundingMode/UP)))
  (is (= java.math.RoundingMode/HALF_UP
         (api/with-rounding :HALF_UP (scale/rounding-mode))))
  (is (= java.math.RoundingMode/HALF_UP
         (api-money/with-rounding :HALF_UP (scale/rounding-mode))))
  (is (= java.math.RoundingMode/HALF_UP
         (api-money/with-rescaling :HALF_UP (scale/rounding-mode))))
  (is (true? (api-money/with-rescaling :HALF_UP scale/*each*)))
  (let [v0 (api/scale-apply 1.20M)
        v0m (api-money/scale-apply 1.20M)
        v1 (api/scale-apply 1M 2)
        v2 (api/scale-apply 1.234M 2 java.math.RoundingMode/HALF_UP)
        v3 (api-money/scale-apply 1M 2)
        v4 (api-money/scale-apply 1.234M 2 java.math.RoundingMode/HALF_UP)]
    (is (= 2 (.scale ^BigDecimal v0)))
    (is (= 2 (.scale ^BigDecimal v0m)))
    (is (= 2 (.scale ^BigDecimal v1)))
    (is (= 2 (.scale ^BigDecimal v2)))
    (is (= 2 (.scale ^BigDecimal v3)))
    (is (= 2 (.scale ^BigDecimal v4)))
    (is (= 1.23M v2))
    (is (= 1.23M v4))))

(deftest inter-ops
  (let [r (mk-test-registry)]
    (with-default-currency
      r
      (fn []
        (let [m1   (api/money 1 :EUR r)
              m2   (api/money 2 :EUR r)
              mneg (api/money -1 :EUR r)]
          (is (instance? Money (api-ops/+ m1 m2)))
          (is (= 3 (api-ops/+ 1 2)))
          (is (= 1 (api-ops/- 3 2)))
          (is (= 6 (api-ops/* 2 3)))
          (is (= 2 (api-ops// 4 2)))
          (is (true? (api-ops/= m1 (api/money 1 :EUR r))))
          (is (true? (api-ops/not= 1 2)))
          (is (true? (api-ops/> m2 m1)))
          (is (true? (api-ops/>= 2 2)))
          (is (true? (api-ops/< 1 2)))
          (is (true? (api-ops/<= 2 2)))
          (is (neg? (api-ops/compare m1 m2)))
          (is (true? (api-ops/pos? m1)))
          (is (true? (api-ops/neg? mneg)))
          (is (== 1 (api-ops/long m1)))
          (is (== 1 (api-ops/int m1)))
          (is (== 1.0 (api-ops/double m1)))
          (is (== 1.0 (double (api-ops/float m1)))))))))

(deftest api-ops
  (let [r (mk-test-registry)]
    (with-default-currency
      r
      (fn []
        (let [m1 (api/money 1 :EUR r)
              m2 (api/money 2 :EUR r)]
          (is (instance? Money (api-ops/+ m1 m2)))
          (is (= 3 (api-ops/+ 1 2)))
          (is (true? (api-ops/> m2 m1)))
          (is (neg? (api-ops/compare m1 m2)))
          (is (== 1 (api-ops/long m1))))))))
