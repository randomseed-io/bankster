(ns io.randomseed.bankster.examples.tagged-literals
  "Tagged literal examples for readable money and currency expressions.

   Bankster provides tagged literals that make code more readable
   and enable data-as-code for financial values."
  (:require [io.randomseed.bankster.api          :as       api]
            [io.randomseed.bankster.api.money    :as api-money]
            [io.randomseed.bankster.api.ops      :as   api-ops]
            [io.randomseed.bankster.api.currency :as api-currency]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Basic money literals
;;; ---------------------------------------------------------------------------

(comment
  ;; Standard format: #money[amount CURRENCY]
  #money[100 PLN]
  ;; => Money record with 100 PLN

  #money[99.99 EUR]
  ;; => Money record with 99.99 EUR

  #money[1234.56 USD]
  ;; => Money record with 1234.56 USD

  ;; With explicit decimal suffix
  #money[100.00M PLN]
  ;; => Same as above, M suffix for BigDecimal literal
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Crypto currency literals
;;; ---------------------------------------------------------------------------

(comment
  ;; Crypto uses namespaced tag
  #money/crypto[1.5 ETH]
  ;; => Money record with 1.5 ETH (18 decimal precision)

  #money/crypto[0.00123456 BTC]
  ;; => Money record with BTC (8 decimal precision)

  #money/crypto[1000 USDT]
  ;; => Money record with USDT stablecoin

  ;; High precision amounts
  #money/crypto[0.000000000000000001 ETH]
  ;; => 1 wei
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Currency literals
;;; ---------------------------------------------------------------------------

(comment
  ;; Simple currency lookup
  #currency PLN
  ;; => Currency record for Polish Zloty

  #currency EUR
  ;; => Currency record for Euro

  ;; Namespaced currencies
  #currency crypto/ETH
  ;; => Currency record for Ethereum

  #currency crypto/BTC
  ;; => Currency record for Bitcoin
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Using literals in data structures
;;; ---------------------------------------------------------------------------

(def price-list
  "Product price list using tagged literals."
  {:widget-a #money[29.99 PLN]
   :widget-b #money[49.99 PLN]
   :widget-c #money[99.99 PLN]
   :premium  #money[199.99 PLN]})

(def exchange-rates
  "Exchange rates as money literals."
  {:EUR->PLN #money[4.35 PLN]
   :USD->PLN #money[4.05 PLN]
   :GBP->PLN #money[5.10 PLN]})

(def crypto-portfolio
  "Crypto portfolio using tagged literals."
  {:eth  #money/crypto[2.5 ETH]
   :btc  #money/crypto[0.15 BTC]
   :usdt #money/crypto[5000 USDT]})

(comment
  ;; Access values naturally
  (:widget-a price-list)
  ;; => #money[29.99 PLN]

  ;; Operations work directly
  (api-ops/+ (:widget-a price-list) (:widget-b price-list))
  ;; => #money[79.98 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Literals in function definitions
;;; ---------------------------------------------------------------------------

(def minimum-order-value
  "Minimum order value for free shipping."
  #money[100 PLN])

(def shipping-cost
  "Standard shipping cost."
  #money[14.99 PLN])

(defn calculate-total
  "Calculates order total with shipping."
  [subtotal]
  (if (api-ops/>= subtotal minimum-order-value)
    subtotal
    (api-ops/+ subtotal shipping-cost)))

(comment
  (calculate-total #money[50 PLN])
  ;; => #money[64.99 PLN]

  (calculate-total #money[150 PLN])
  ;; => #money[150.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Literals in test assertions
;;; ---------------------------------------------------------------------------

(comment
  ;; In test files, literals make assertions readable
  (require '[clojure.test :refer [deftest is]])

  (deftest allocation-test
    (is (= [#money[33.34 PLN] #money[33.33 PLN] #money[33.33 PLN]]
           (api-money/allocate #money[100 PLN] [1 1 1]))))

  (deftest arithmetic-test
    (is (api-ops/= #money[150 PLN]
                   (api-ops/+ #money[100 PLN] #money[50 PLN]))))
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: EDN configuration files
;;; ---------------------------------------------------------------------------

;; In config.edn file:
;; {:pricing
;;  {:base-price #money[99.99 PLN]
;;   :vat-rate 0.23
;;   :discounts {:student #money[20 PLN]
;;               :senior  #money[15 PLN]}}
;;  :limits
;;  {:min-order #money[50 PLN]
;;   :max-order #money[10000 PLN]}}

(defn load-pricing-config
  "Loads pricing configuration with money literals."
  [config-path]
  ;; EDN reader automatically handles #money literals
  (clojure.edn/read-string
   {:readers {'money    io.randomseed.bankster.api.money/data-literal
              'currency io.randomseed.bankster.api.currency/data-literal}}
   (slurp config-path)))

(comment
  ;; Reading EDN with literals
  (clojure.edn/read-string
   {:readers {'money io.randomseed.bankster.api.money/data-literal}}
   "#money[100 PLN]")
  ;; => #money[100.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Print representation
;;; ---------------------------------------------------------------------------

(comment
  ;; Money prints as tagged literal (roundtrip-safe)
  (pr-str #money[100.50 PLN])
  ;; => "#money[100.50M PLN]"

  (pr-str #money/crypto[1.5 ETH])
  ;; => "#money/crypto[1.5M ETH]"

  ;; Can be read back
  (read-string (pr-str #money[100.50 PLN]))
  ;; => #money[100.50 PLN]

  ;; Currency print representation
  (pr-str #currency EUR)
  ;; => "#currency EUR"
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Comparison with alternative creation methods
;;; ---------------------------------------------------------------------------

(comment
  ;; Tagged literal (most readable)
  #money[100 PLN]

  ;; Macro (programmatic, allows variables)
  (api-money/of :PLN 100)

  ;; Function (fully dynamic)
  (api/money :PLN 100)

  ;; All produce equivalent results
  (api-ops/= #money[100 PLN]
             (api-money/of :PLN 100))
  ;; => true

  ;; Tagged literals are compile-time - currency must be literal
  ;; For runtime currency, use of/value:
  (let [currency :PLN
        amount 100]
    (api-money/of currency amount))
  )

;;; ---------------------------------------------------------------------------
;;; Example 10: Domain-specific languages with literals
;;; ---------------------------------------------------------------------------

(def invoice-template
  "Invoice template using tagged literals."
  {:header {:company "ACME Corp"
            :currency #currency PLN}
   :items [{:description "Consulting (8h)"
            :unit-price #money[150 PLN]
            :quantity 8}
           {:description "Travel expenses"
            :unit-price #money[250 PLN]
            :quantity 1}]
   :payment-terms {:due-days 14
                   :late-fee #money[50 PLN]}})

(defn calculate-invoice
  "Calculates invoice totals from template."
  [template]
  (let [items (:items template)
        line-totals (map (fn [{:keys [unit-price quantity]}]
                           (api-ops/* unit-price quantity))
                         items)
        subtotal (apply api-ops/+ line-totals)]
    {:items items
     :subtotal subtotal
     :vat (api-ops/* subtotal 0.23M)
     :total (api-ops/* subtotal 1.23M)}))

(comment
  (calculate-invoice invoice-template)
  ;; => {:items [...]
  ;;     :subtotal #money[1450.00 PLN]
  ;;     :vat #money[333.50 PLN]
  ;;     :total #money[1783.50 PLN]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 11: Working with both ISO and crypto literals
;;; ---------------------------------------------------------------------------

(def multi-currency-portfolio
  "Portfolio spanning fiat and crypto."
  {:fiat {:pln #money[10000 PLN]
          :eur #money[2500 EUR]
          :usd #money[1500 USD]}
   :crypto {:btc #money/crypto[0.5 BTC]
            :eth #money/crypto[5.0 ETH]
            :usdt #money/crypto[10000 USDT]}})

(defn portfolio-summary
  "Generates summary of multi-currency portfolio."
  [portfolio]
  {:fiat-currencies (keys (:fiat portfolio))
   :crypto-currencies (keys (:crypto portfolio))
   :fiat-count (count (:fiat portfolio))
   :crypto-count (count (:crypto portfolio))})

(comment
  (portfolio-summary multi-currency-portfolio)
  ;; => {:fiat-currencies (:pln :eur :usd)
  ;;     :crypto-currencies (:btc :eth :usdt)
  ;;     :fiat-count 3
  ;;     :crypto-count 3}
  )
