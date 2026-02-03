(ns io.randomseed.bankster.examples.crypto
  "Cryptocurrency handling in Bankster.

   Bankster has built-in support for popular cryptocurrencies with
   appropriate precision (e.g., 18 decimal places for ETH, 8 for BTC)."
  (:require [io.randomseed.bankster.money    :as money]
            [io.randomseed.bankster.api      :as api]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.scale    :as scale]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Basic cryptocurrency operations
;;; ---------------------------------------------------------------------------

(comment
  ;; Creating crypto amounts (tagged literal with namespace)
  #money/crypto[1.5 ETH]
  #money/crypto[0.00123456 BTC]

  ;; Or via function
  (api/money-of :crypto/ETH 2.5M)
  (api/money-of :crypto/BTC 0.001M)

  ;; Check if currency is crypto
  (api/currency-crypto? (api/currency-of :crypto/ETH))
  ;; => true

  (api/currency-crypto? (api/currency-of :PLN))
  ;; => false
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Minimal units (wei, satoshi)
;;; ---------------------------------------------------------------------------

(def one-wei
  "Smallest ETH unit (10^-18)."
  #money/crypto[0.000000000000000001 ETH])

(def one-satoshi
  "Smallest BTC unit (10^-8)."
  #money/crypto[0.00000001 BTC])

(comment
  ;; Arithmetic on minimal units
  (api/* one-wei 1000000000000000000M)
  ;; => #money/crypto[1 ETH]

  (api/* one-satoshi 100000000M)
  ;; => #money/crypto[1 BTC]

  ;; Convert satoshi -> BTC
  (defn satoshi->btc [satoshi]
    (api/money-of :crypto/BTC (/ satoshi 100000000M)))

  (satoshi->btc 50000000)
  ;; => #money/crypto[0.5 BTC]
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Crypto portfolio
;;; ---------------------------------------------------------------------------

(defn portfolio
  "Creates a cryptocurrency portfolio."
  [& balances]
  (into {} (map (fn [m] [(api/currency-id (api/money-currency m)) m]) balances)))

(defn portfolio-value
  "Calculates portfolio value in target currency."
  [portfolio rates target-currency]
  (let [values (for [[currency balance] portfolio
                     :let [rate (get rates currency 0M)]]
                 (api/* (api/money-of target-currency rate)
                            (api/money-amount balance)))]
    (apply api/+ values)))

(comment
  (def my-portfolio
    (portfolio #money/crypto[2.5 ETH]
               #money/crypto[0.15 BTC]
               #money/crypto[1000 USDT]))

  ;; Rates in PLN
  (def rates-pln
    {:crypto/ETH  8500M
     :crypto/BTC  175000M
     :crypto/USDT 4.05M})

  (portfolio-value my-portfolio rates-pln :PLN)
  ;; => #money[51550.00 PLN]  ; approximate value
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Transaction fee calculation (gas)
;;; ---------------------------------------------------------------------------

(defn calculate-gas-fee
  "Calculates ETH transaction fee.
   gas-limit - maximum gas units
   gas-price - price per gas unit in gwei (1 gwei = 10^-9 ETH)"
  [gas-limit gas-price-gwei]
  (let [gwei-to-eth 0.000000001M
        gas-price-eth (* gas-price-gwei gwei-to-eth)]
    (api/money-of :crypto/ETH (* gas-limit gas-price-eth))))

(comment
  ;; Typical ETH transfer: 21000 gas × 30 gwei
  (calculate-gas-fee 21000 30)
  ;; => #money/crypto[0.00063 ETH]

  ;; Smart contract interaction: 150000 gas × 50 gwei
  (calculate-gas-fee 150000 50)
  ;; => #money/crypto[0.0075 ETH]
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: DCA (Dollar Cost Averaging) calculator
;;; ---------------------------------------------------------------------------

(defn simulate-dca
  "Simulates DCA strategy - regular purchases for fixed amount."
  [monthly-amount historical-rates]
  (let [purchases (for [rate historical-rates]
                    {:rate   rate
                     :amount monthly-amount
                     :btc    (api/money-of :crypto/BTC
                                       (with-precision 8
                                         (/ (api/money-amount monthly-amount) rate)))})]
    {:purchases      purchases
     :total-spent    (api/* monthly-amount (count historical-rates))
     :total-btc      (apply api/+ (map :btc purchases))
     :average-rate   (/ (reduce + historical-rates)
                        (count historical-rates))}))

(comment
  ;; Simulate 6 months of DCA at 500 PLN/month
  (def btc-pln-rates [160000M 170000M 150000M 180000M 165000M 175000M])

  (simulate-dca #money[500 PLN] btc-pln-rates)
  ;; => {:total-spent #money[3000 PLN]
  ;;     :total-btc #money/crypto[0.01831... BTC]
  ;;     :average-rate 166666.67M}
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Arbitrage - price comparison across exchanges
;;; ---------------------------------------------------------------------------

(defn find-arbitrage
  "Finds arbitrage opportunities between exchanges."
  [exchange-prices]
  (let [sorted     (sort-by :price exchange-prices)
        cheapest   (first sorted)
        expensive  (last sorted)
        spread     (api/- (:price expensive) (:price cheapest))
        percent    (* 100M (/ (api/money-amount spread)
                              (api/money-amount (:price cheapest))))]
    {:buy-on       (:exchange cheapest)
     :buy-price    (:price cheapest)
     :sell-on      (:exchange expensive)
     :sell-price   (:price expensive)
     :gross-profit spread
     :profit-pct   (format "%.2f%%" percent)}))

(comment
  (def btc-prices
    [{:exchange :binance  :price #money[174500 PLN]}
     {:exchange :kraken   :price #money[175200 PLN]}
     {:exchange :coinbase :price #money[175800 PLN]}
     {:exchange :zonda    :price #money[174200 PLN]}])

  (find-arbitrage btc-prices)
  ;; => {:buy-on :zonda
  ;;     :buy-price #money[174200 PLN]
  ;;     :sell-on :coinbase
  ;;     :sell-price #money[175800 PLN]
  ;;     :gross-profit #money[1600 PLN]
  ;;     :profit-pct "0.92%"}
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Staking rewards calculator
;;; ---------------------------------------------------------------------------

(defn calculate-staking-rewards
  "Calculates staking rewards.
   apy - Annual Percentage Yield as decimal (e.g., 0.05 = 5%)"
  [staked-amount apy days]
  (let [daily-rate (/ apy 365M)
        multiplier (Math/pow (+ 1 (double daily-rate)) days)
        final-val  (api/* staked-amount (bigdec multiplier))]
    {:staked      staked-amount
     :apy         (str (* 100 apy) "%")
     :days        days
     :final-value final-val
     :reward      (api/- final-val staked-amount)}))

(comment
  ;; 32 ETH staked for 365 days at 4.5% APY
  (calculate-staking-rewards #money/crypto[32 ETH] 0.045M 365)
  ;; => {:staked #money/crypto[32 ETH]
  ;;     :apy "4.5%"
  ;;     :days 365
  ;;     :final-value #money/crypto[33.472... ETH]
  ;;     :reward #money/crypto[1.472... ETH]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Listing available cryptocurrencies in registry
;;; ---------------------------------------------------------------------------

(comment
  ;; List all cryptocurrencies in default registry
  (api/currency-of-domain :CRYPTO)

  ;; Or via predicate
  (->> (api/currency-all)
       (filter api/currency-crypto?)
       (map api/currency-id))
  ;; => (:crypto/ETH :crypto/BTC :crypto/USDT :crypto/USDC ...)

  ;; Currency details
  (api/currency-of :crypto/ETH)
  ;; => #currency{:id :crypto/ETH, :numeric nil, :scale 18,
  ;;              :kind :crypto/coin, :domain :CRYPTO}
  )
