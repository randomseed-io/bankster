(ns io.randomseed.bankster.examples.crypto-traits
  "Cryptocurrency traits, kinds, and classification examples.

   Bankster provides:
   1. KIND predicates - based on currency kind hierarchy (stable?, fiat?, wrapped?, etc.)
   2. TRAIT predicates - based on registry traits (decentralized?, has-trait?, of-trait?)
   3. DOMAIN predicates - based on currency domain (crypto?, iso-strict?, iso-legacy?)

   This allows rich classification and filtering of currencies."
  (:require [io.randomseed.bankster.api          :as          api]
            [io.randomseed.bankster.api.currency :as api-currency]
            [io.randomseed.bankster.api.money    :as    api-money]
            [io.randomseed.bankster.api.ops      :as      api-ops]
            [io.randomseed.bankster.api.registry :as api-registry]
            [io.randomseed.bankster.registry     :as     registry]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Built-in KIND predicates (based on kind hierarchy)
;;; ---------------------------------------------------------------------------

(comment
  ;; Bankster provides many built-in predicates for currency kinds:

  ;; Stability and value types
  (api-currency/stable?     (api-currency/of :crypto/USDT))  ;; => true (stablecoin)
  (api-currency/asset?      (api-currency/of :crypto/BTC))   ;; => true (value-bearing)
  (api-currency/peg?        (api-currency/of :crypto/USDT))  ;; => true (pegged to fiat)
  (api-currency/commodity?  (api-currency/of :XAU))          ;; => true (gold)
  (api-currency/metal?      (api-currency/of :XAU))          ;; => true (precious metal)

  ;; Token classifications
  (api-currency/wrapped?    (api-currency/of :crypto/WBTC))  ;; => true (wrapped Bitcoin)
  (api-currency/staked?     (api-currency/of :crypto/stETH)) ;; => true (staked ETH)
  (api-currency/referenced? (api-currency/of :crypto/USDT))  ;; => true (references fiat)

  ;; Fiat and traditional currencies
  (api-currency/fiat?       (api-currency/of :USD))          ;; => true
  (api-currency/fiduciary?  (api-currency/of :EUR))          ;; => true
  (api/iso-currency?        (api-currency/of :PLN))          ;; => true (ISO-4217)
  (api-currency/funds?      (api-currency/of :XDR))          ;; => true (SDR - settlement)

  ;; Virtual and experimental
  (api-currency/virtual?      (api-currency/of :crypto/ETH))   ;; => true
  (api-currency/experimental? (api-currency/of :XTS))          ;; => true (test currency)
  (api-currency/test?         (api-currency/of :XTS))          ;; => true
  (api-currency/special?      (api-currency/of :XXX))          ;; => true (no currency)
  (api-currency/null?         (api-currency/of :XXX))          ;; => true
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Built-in DOMAIN predicates
;;; ---------------------------------------------------------------------------

(comment
  ;; Domain predicates check the currency's domain classification

  ;; Cryptocurrency domain
  (api-currency/crypto? (api-currency/of :crypto/BTC))
  ;; => true

  (api-currency/crypto? (api-currency/of :USD))
  ;; => false

  ;; ISO domain variants
  (api-currency/iso-strict? (api-currency/of :EUR))
  ;; => true (current, official ISO currency)

  (api-currency/iso-legacy? (api-currency/of :DEM))
  ;; => true (former German Mark - legacy ISO)

  ;; General domain check
  (api-currency/of-domain? :CRYPTO (api-currency/of :crypto/ETH))
  ;; => true

  (api-currency/of-domain? :ISO-4217 (api-currency/of :PLN))
  ;; => true
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Built-in TRAIT predicate - decentralized?
;;; ---------------------------------------------------------------------------

(comment
  ;; decentralized? checks for :control/decentralized trait
  (api-currency/decentralized? (api-currency/of :crypto/BTC))
  ;; => true (Bitcoin is decentralized)

  (api-currency/decentralized? (api-currency/of :crypto/ETH))
  ;; => true (Ethereum is decentralized)

  ;; Centralized stablecoins
  (api-currency/decentralized? (api-currency/of :crypto/USDT))
  ;; => false (Tether is centralized)

  (api-currency/decentralized? (api-currency/of :crypto/USDC))
  ;; => false (USD Coin is centralized)

  ;; Fiat currencies are not decentralized
  (api-currency/decentralized? (api-currency/of :USD))
  ;; => false
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Combining predicates for classification
;;; ---------------------------------------------------------------------------

(defn classify-currency
  "Classifies a currency using built-in predicates."
  [currency-id]
  (let [curr (api-currency/of currency-id)]
    {:id             currency-id
     ;; Domain
     :crypto?        (api-currency/crypto? curr)
     :iso?           (api/iso-currency? curr)
     ;; Kind
     :stable?        (api-currency/stable? curr)
     :fiat?          (api-currency/fiat? curr)
     :wrapped?       (api-currency/wrapped? curr)
     :commodity?     (api-currency/commodity? curr)
     :virtual?       (api-currency/virtual? curr)
     ;; Trait
     :decentralized? (api-currency/decentralized? curr)}))

(comment
  (classify-currency :crypto/BTC)
  ;; => {:id :crypto/BTC
  ;;     :crypto? true, :iso? false
  ;;     :stable? false, :fiat? false, :wrapped? false
  ;;     :commodity? false, :virtual? true
  ;;     :decentralized? true}

  (classify-currency :crypto/USDT)
  ;; => {:id :crypto/USDT
  ;;     :crypto? true, :iso? false
  ;;     :stable? true, :fiat? false, :wrapped? false
  ;;     :commodity? false, :virtual? true
  ;;     :decentralized? false}

  (classify-currency :USD)
  ;; => {:id :USD
  ;;     :crypto? false, :iso? true
  ;;     :stable? false, :fiat? true, :wrapped? false
  ;;     :commodity? false, :virtual? false
  ;;     :decentralized? false}

  (classify-currency :XAU)
  ;; => {:id :XAU
  ;;     :crypto? false, :iso? true
  ;;     :stable? false, :fiat? false, :wrapped? false
  ;;     :commodity? true, :virtual? false
  ;;     :decentralized? false}
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Querying traits directly with has-trait? and of-trait?
;;; ---------------------------------------------------------------------------

(comment
  ;; has-trait? - exact trait match
  (api-currency/has-trait? (api-currency/of :crypto/BTC) :control/decentralized)
  ;; => true

  (api-currency/has-trait? (api-currency/of :crypto/USDT) :control/centralized)
  ;; => true

  ;; Check for blockchain network trait
  (api-currency/has-trait? (api-currency/of :crypto/ETH) :blockchain/ethereum)
  ;; => true

  (api-currency/has-trait? (api-currency/of :crypto/BTC) :blockchain/bitcoin)
  ;; => true

  ;; Token standards
  (api-currency/has-trait? (api-currency/of :crypto/USDT) :token/erc20)
  ;; => true (for Ethereum-based USDT)

  ;; Privacy coins
  (api-currency/has-trait? (api-currency/of :crypto/XMR) :privacy/coin)
  ;; => true (Monero is a privacy coin)

  ;; DeFi traits
  (api-currency/has-trait? (api-currency/of :crypto/UNI) :defi/governance)
  ;; => true (if registered with trait)
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Hierarchical trait queries (of-trait?)
;;; ---------------------------------------------------------------------------

(comment
  ;; of-trait? respects trait hierarchy (uses isa?)
  ;; :stable/coin derives from [:stable :token/fungible]

  (api-currency/of-trait? :stable (api-currency/of :crypto/USDT))
  ;; => true (USDT has :stable/coin which derives from :stable)

  (api-currency/of-trait? :token/fungible (api-currency/of :crypto/USDT))
  ;; => true (derives from :token/fungible)

  (api-currency/of-trait? :token (api-currency/of :crypto/USDT))
  ;; => true (:token/fungible derives from :token)

  ;; :control hierarchy
  (api-currency/of-trait? :control (api-currency/of :crypto/BTC))
  ;; => true (has :control/decentralized which derives from :control)

  ;; :blockchain hierarchy - query parent
  (api-currency/of-trait? :blockchain (api-currency/of :crypto/ETH))
  ;; => true (has :blockchain/ethereum which derives from :blockchain)

  ;; Trait hierarchy in config.edn:
  ;; :control/decentralized -> :control -> :all
  ;; :stable/coin -> [:stable :token/fungible] -> :all
  ;; :blockchain/ethereum -> :blockchain -> :all
  ;; :peg/fiat -> [:peg :stable/coin] -> ...
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Filtering currencies by traits
;;; ---------------------------------------------------------------------------

(defn currencies-with-trait
  "Returns all currencies that have a given trait."
  [trait]
  (->> (api-currency/all)
       (filter #(api-currency/has-trait? % trait))
       (map api-currency/id)))

(defn decentralized-currencies
  "Returns all decentralized currencies."
  []
  (->> (api-currency/all)
       (filter api-currency/decentralized?)
       (map api-currency/id)))

(defn stablecoins
  "Returns all stablecoins."
  []
  (->> (api-currency/all)
       (filter api-currency/stable?)
       (map api-currency/id)))

(comment
  ;; List all decentralized currencies
  (decentralized-currencies)
  ;; => (:crypto/BTC :crypto/ETH :crypto/LTC ...)

  ;; List all stablecoins
  (stablecoins)
  ;; => (:crypto/USDT :crypto/USDC :crypto/DAI ...)

  ;; List all privacy coins
  (currencies-with-trait :privacy/coin)
  ;; => (:crypto/XMR :crypto/ZEC ...)

  ;; List Ethereum-based tokens
  (currencies-with-trait :blockchain/ethereum)
  ;; => (:crypto/ETH :crypto/USDT :crypto/USDC ...)
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Portfolio analysis by traits
;;; ---------------------------------------------------------------------------

(defn analyze-portfolio-traits
  "Analyzes a portfolio by currency traits."
  [holdings]
  (let [currencies (map (comp api-currency/of first) holdings)]
    {:total-positions   (count holdings)
     :decentralized     (count (filter api-currency/decentralized? currencies))
     :centralized       (count (filter #(api-currency/has-trait? % :control/centralized) currencies))
     :stablecoins       (count (filter api-currency/stable? currencies))
     :privacy-coins     (count (filter #(api-currency/has-trait? % :privacy/coin) currencies))
     :defi-tokens       (count (filter #(api-currency/of-trait? :defi currencies) currencies))}))

(defn portfolio-decentralization-ratio
  "Calculates what percentage of portfolio (by value) is decentralized."
  [holdings rates-in-usd]
  (let [total-value (reduce + (map (fn [[curr amount]]
                                     (* amount (get rates-in-usd curr 0M)))
                                   holdings))
        decentralized-value (reduce + (map (fn [[curr amount]]
                                             (if (api-currency/decentralized? (api-currency/of curr))
                                               (* amount (get rates-in-usd curr 0M))
                                               0M))
                                           holdings))]
    (if (pos? total-value)
      {:total-usd          total-value
       :decentralized-usd  decentralized-value
       :decentralized-pct  (* 100M (/ decentralized-value total-value))}
      {:total-usd 0M :decentralized-usd 0M :decentralized-pct 0M})))

(comment
  (def my-holdings
    [[:crypto/BTC  0.5M]
     [:crypto/ETH  5.0M]
     [:crypto/USDT 10000M]
     [:crypto/USDC 5000M]])

  (analyze-portfolio-traits my-holdings)
  ;; => {:total-positions 4
  ;;     :decentralized 2
  ;;     :centralized 2
  ;;     :stablecoins 2
  ;;     :privacy-coins 0
  ;;     :defi-tokens 0}

  (def usd-rates
    {:crypto/BTC  45000M
     :crypto/ETH  3000M
     :crypto/USDT 1M
     :crypto/USDC 1M})

  (portfolio-decentralization-ratio my-holdings usd-rates)
  ;; => {:total-usd 52500M
  ;;     :decentralized-usd 37500M  ; BTC + ETH
  ;;     :decentralized-pct 71.43M}
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Risk assessment based on predicates
;;; ---------------------------------------------------------------------------

(defn assess-currency-risk
  "Assesses risk factors for a cryptocurrency based on its traits."
  [currency-id]
  (let [curr (api-currency/of currency-id)]
    {:currency         currency-id
     :centralization-risk
     (cond
       (api-currency/decentralized? curr) :low
       (api-currency/has-trait? curr :control/federated) :medium
       (api-currency/has-trait? curr :control/centralized) :high
       :else :unknown)

     :stability-risk
     (cond
       (api-currency/stable? curr) :low
       (api-currency/has-trait? curr :peg/fiat) :low
       :else :high)

     :regulatory-risk
     (cond
       (api-currency/has-trait? curr :privacy/coin) :high
       (api-currency/has-trait? curr :control/centralized) :medium
       :else :medium)

     :smart-contract-risk
     (cond
       (api-currency/has-trait? curr :token/erc20) :medium
       (api-currency/has-trait? curr :token/bep20) :medium
       (api-currency/has-trait? curr :blockchain/bitcoin) :low
       :else :low)}))

(comment
  (assess-currency-risk :crypto/BTC)
  ;; => {:currency :crypto/BTC
  ;;     :centralization-risk :low
  ;;     :stability-risk :high
  ;;     :regulatory-risk :medium
  ;;     :smart-contract-risk :low}

  (assess-currency-risk :crypto/USDT)
  ;; => {:currency :crypto/USDT
  ;;     :centralization-risk :high
  ;;     :stability-risk :low
  ;;     :regulatory-risk :medium
  ;;     :smart-contract-risk :medium}

  (assess-currency-risk :crypto/XMR)
  ;; => {:currency :crypto/XMR
  ;;     :centralization-risk :low
  ;;     :stability-risk :high
  ;;     :regulatory-risk :high  ; privacy coin
  ;;     :smart-contract-risk :low}
  )

;;; ---------------------------------------------------------------------------
;;; Example 10: Adding custom traits to currencies
;;; ---------------------------------------------------------------------------

(comment
  ;; Add traits to a currency in a registry
  (def custom-registry
    (-> (api-registry/state)
        (api-currency/add-traits :crypto/ETH #{:defi/base-layer :network/layer1})
        (api-currency/add-traits :crypto/MATIC #{:network/layer2 :defi/scaling})))

  ;; Query custom traits
  (api-registry/with custom-registry
    (api-currency/has-trait? (api-currency/of :crypto/ETH) :defi/base-layer))
  ;; => true

  ;; Set traits (replaces existing)
  (def registry-with-new-traits
    (api-currency/set-traits (api-registry/state)
                         :crypto/BTC
                         #{:control/decentralized
                           :blockchain/bitcoin
                           :store-of-value
                           :digital-gold}))

  ;; Remove traits
  (def registry-without-trait
    (api-currency/remove-traits (api-registry/state)
                            :crypto/USDT
                            #{:some-trait-to-remove}))
  )

;;; ---------------------------------------------------------------------------
;;; Example 11: Blockchain network filtering
;;; ---------------------------------------------------------------------------

(defn currencies-on-blockchain
  "Returns currencies that operate on a specific blockchain."
  [blockchain-trait]
  (->> (api-currency/all)
       (filter #(api-currency/has-trait? % blockchain-trait))
       (map api-currency/id)))

(defn multi-chain-currencies
  "Returns currencies that exist on multiple blockchains."
  [currency-id blockchains]
  (let [curr (api-currency/of currency-id)]
    {:currency   currency-id
     :chains     (filter #(api-currency/has-trait? curr %) blockchains)
     :multi-chain? (> (count (filter #(api-currency/has-trait? curr %) blockchains)) 1)}))

(comment
  ;; All Ethereum-based tokens
  (currencies-on-blockchain :blockchain/ethereum)
  ;; => (:crypto/ETH :crypto/USDT :crypto/USDC :crypto/UNI ...)

  ;; All Solana-based tokens
  (currencies-on-blockchain :blockchain/solana)
  ;; => (:crypto/SOL ...)

  ;; Check if USDT is multi-chain
  (multi-chain-currencies :crypto/USDT
                          [:blockchain/ethereum
                           :blockchain/tron
                           :blockchain/solana
                           :blockchain/avalanche])
  ;; => {:currency :crypto/USDT
  ;;     :chains [:blockchain/ethereum :blockchain/tron ...]
  ;;     :multi-chain? true}
  )

;;; ---------------------------------------------------------------------------
;;; Example 12: Arithmetic with trait-aware validation
;;; ---------------------------------------------------------------------------

(defn add-if-same-stability
  "Adds two amounts only if they have the same stability profile."
  [a b]
  (let [curr-a (api-money/currency a)
        curr-b (api-money/currency b)
        stable-a? (api-currency/stable? curr-a)
        stable-b? (api-currency/stable? curr-b)]
    (cond
      (not (api-money/same-currencies? a b))
      {:error "Different currencies"}

      (not= stable-a? stable-b?)
      {:error "Cannot mix stable and volatile currencies in this context"
       :a-stable? stable-a?
       :b-stable? stable-b?}

      :else
      {:result (api-ops/+ a b)})))

(defn calculate-with-stablecoin-fee
  "Applies different fee rates for stablecoins vs volatile crypto."
  [amount fee-rates]
  (let [curr (api-money/currency amount)
        fee-rate (if (api-currency/stable? curr)
                   (:stablecoin fee-rates)
                   (:volatile fee-rates))
        fee (api-ops/* amount fee-rate)]
    {:amount     amount
     :fee-rate   fee-rate
     :fee        fee
     :net-amount (api-ops/- amount fee)
     :is-stable? (api-currency/stable? curr)}))

(comment
  ;; Same-stability addition
  (add-if-same-stability #money/crypto[100 USDT] #money/crypto[50 USDT])
  ;; => {:result #money/crypto[150 USDT]}

  ;; Fee calculation based on stability
  (def fee-rates {:stablecoin 0.001M  ; 0.1% for stablecoins
                  :volatile   0.003M}) ; 0.3% for volatile

  (calculate-with-stablecoin-fee #money/crypto[1000 USDT] fee-rates)
  ;; => {:amount #money/crypto[1000 USDT]
  ;;     :fee-rate 0.001M
  ;;     :fee #money/crypto[1 USDT]
  ;;     :net-amount #money/crypto[999 USDT]
  ;;     :is-stable? true}

  (calculate-with-stablecoin-fee #money/crypto[1 ETH] fee-rates)
  ;; => {:amount #money/crypto[1 ETH]
  ;;     :fee-rate 0.003M
  ;;     :fee #money/crypto[0.003 ETH]
  ;;     :net-amount #money/crypto[0.997 ETH]
  ;;     :is-stable? false}
  )

;;; ---------------------------------------------------------------------------
;;; Example 13: DeFi protocol simulation
;;; ---------------------------------------------------------------------------

(defn validate-collateral
  "Validates if a currency can be used as collateral in DeFi."
  [currency-id allowed-traits]
  (let [curr (api-currency/of currency-id)]
    {:currency     currency-id
     :valid?       (some #(api-currency/of-trait? % curr) allowed-traits)
     :traits-found (filter #(api-currency/of-trait? % curr) allowed-traits)
     :decentralized? (api-currency/decentralized? curr)}))

(defn calculate-ltv
  "Calculates loan-to-value ratio based on collateral traits."
  [collateral-currency-id base-ltv]
  (let [curr (api-currency/of collateral-currency-id)
        ;; Adjust LTV based on traits
        ltv-adjustment (cond
                         (api-currency/stable? curr) 0.10M      ; +10% for stablecoins
                         (api-currency/decentralized? curr) 0M  ; no adjustment
                         :else -0.10M)]                     ; -10% for centralized
    {:currency   collateral-currency-id
     :base-ltv   base-ltv
     :adjustment ltv-adjustment
     :final-ltv  (+ base-ltv ltv-adjustment)}))

(comment
  ;; Validate collateral for a lending protocol
  (validate-collateral :crypto/ETH [:control/decentralized :blockchain/ethereum])
  ;; => {:currency :crypto/ETH
  ;;     :valid? true
  ;;     :traits-found [:control/decentralized :blockchain/ethereum]
  ;;     :decentralized? true}

  (validate-collateral :crypto/USDT [:control/decentralized])
  ;; => {:currency :crypto/USDT
  ;;     :valid? false
  ;;     :traits-found []
  ;;     :decentralized? false}

  ;; LTV calculation
  (calculate-ltv :crypto/ETH 0.75M)
  ;; => {:currency :crypto/ETH, :base-ltv 0.75M, :adjustment 0M, :final-ltv 0.75M}

  (calculate-ltv :crypto/USDT 0.75M)
  ;; => {:currency :crypto/USDT, :base-ltv 0.75M, :adjustment 0.10M, :final-ltv 0.85M}
  )

;;; ---------------------------------------------------------------------------
;;; Example 14: Viewing trait and kind hierarchies
;;; ---------------------------------------------------------------------------

(comment
  ;; Get the traits hierarchy from registry
  (-> (api-registry/state)
      .hierarchies
      :traits)
  ;; => {:control/decentralized :control
  ;;     :control/centralized :control
  ;;     :stable/coin [:stable :token/fungible]
  ;;     :blockchain/ethereum :blockchain
  ;;     ...}

  ;; Check hierarchy relationships
  (isa? (-> (api-registry/state) .hierarchies :traits)
        :stable/coin
        :stable)
  ;; => true

  (isa? (-> (api-registry/state) .hierarchies :traits)
        :stable/coin
        :token/fungible)
  ;; => true

  ;; All traits that derive from :control
  (descendants (-> (api-registry/state) .hierarchies :traits) :control)
  ;; => #{:control/decentralized :control/centralized :control/federated}

  ;; All traits that derive from :blockchain
  (descendants (-> (api-registry/state) .hierarchies :traits) :blockchain)
  ;; => #{:blockchain/ethereum :blockchain/bitcoin :blockchain/solana ...}
  )

;;; ---------------------------------------------------------------------------
;;; Example 15: Creating custom trait hierarchy (:trust example)
;;; ---------------------------------------------------------------------------

;; This example shows how to:
;; 1. Create a new trait hierarchy (e.g., :trust with levels)
;; 2. Add traits to currencies
;; 3. Make existing traits inherit from your custom hierarchy
;;    (so currencies automatically gain trust levels)

(defn build-trust-hierarchy
  "Builds a registry with custom :trust trait hierarchy.

   Hierarchy:
     :trust (root)
       ├── :trust/full   (highest trust - e.g., BTC, ETH)
       ├── :trust/normal (standard trust - e.g., fiat-pegged stables)
       ├── :trust/low    (lower trust - e.g., algorithmic stables)
       └── :trust/none   (no trust - e.g., test currencies)"
  []
  (-> (api-registry/state)
      ;; Step 1: Create :trust hierarchy
      ;; Each trust level derives from :trust parent
      (api-registry/hierarchy-derive :traits :trust/full   :trust)
      (api-registry/hierarchy-derive :traits :trust/normal :trust)
      (api-registry/hierarchy-derive :traits :trust/low    :trust)
      (api-registry/hierarchy-derive :traits :trust/none   :trust)

      ;; Step 2: Assign trust traits to specific currencies
      (api-currency/add-traits :crypto/BTC  #{:trust/full})
      (api-currency/add-traits :crypto/ETH  #{:trust/full})
      (api-currency/add-traits :crypto/USDT #{:trust/normal})
      (api-currency/add-traits :crypto/USDC #{:trust/normal})
      (api-currency/add-traits :XXX         #{:trust/none})))

(defn build-trust-hierarchy-with-inheritance
  "Builds trust hierarchy AND makes existing traits inherit trust levels.

   This is powerful: by deriving :peg/fiat from :trust/normal,
   ALL currencies that have :peg/fiat trait automatically become :trust/normal!

   Similarly, :metal (precious metals like XAU, XAG) can inherit :trust/full."
  []
  (-> (api-registry/state)
      ;; Step 1: Create base :trust hierarchy
      (api-registry/hierarchy-derive :traits :trust/full   :trust)
      (api-registry/hierarchy-derive :traits :trust/normal :trust)
      (api-registry/hierarchy-derive :traits :trust/low    :trust)
      (api-registry/hierarchy-derive :traits :trust/none   :trust)

      ;; Step 2: Make existing trait categories inherit trust levels
      ;; :peg/fiat -> :trust/normal (fiat-pegged stablecoins are normally trusted)
      (api-registry/hierarchy-derive :traits :peg/fiat :trust/normal)

      ;; :collateral/crypto -> :trust/low (crypto-collateralized = lower trust)
      (api-registry/hierarchy-derive :traits :collateral/crypto :trust/low)

      ;; Step 3: Explicitly assign trust to major cryptocurrencies
      (api-currency/add-traits :crypto/BTC #{:trust/full})
      (api-currency/add-traits :crypto/ETH #{:trust/full})
      (api-currency/add-traits :XXX        #{:trust/none})))

(defn trust-level
  "Returns the trust level of a currency (using of-trait? for hierarchy check)."
  [currency-id registry]
  (let [curr (api-currency/of currency-id registry)]
    (cond
      (api-currency/of-trait? :trust/full   curr registry) :trust/full
      (api-currency/of-trait? :trust/normal curr registry) :trust/normal
      (api-currency/of-trait? :trust/low    curr registry) :trust/low
      (api-currency/of-trait? :trust/none   curr registry) :trust/none
      :else :trust/unknown)))

(defn currencies-by-trust
  "Groups currencies by their trust level."
  [currency-ids registry]
  (group-by #(trust-level % registry) currency-ids))

(comment
  ;; === Basic trust hierarchy ===
  (def reg-basic (build-trust-hierarchy))

  ;; Check trust levels
  (api-registry/with reg-basic
    {:btc-trust  (trust-level :crypto/BTC reg-basic)
     :usdt-trust (trust-level :crypto/USDT reg-basic)
     :xxx-trust  (trust-level :XXX reg-basic)})
  ;; => {:btc-trust :trust/full
  ;;     :usdt-trust :trust/normal
  ;;     :xxx-trust :trust/none}

  ;; Query using of-trait? (respects hierarchy)
  (api-registry/with reg-basic
    (api-currency/of-trait? :trust (api-currency/of :crypto/BTC)))
  ;; => true (BTC has :trust/full which derives from :trust)

  ;; === Advanced: inheritance from existing traits ===
  (def reg-inherited (build-trust-hierarchy-with-inheritance))

  ;; Now ANY currency with :peg/fiat trait automatically has :trust/normal!
  ;; USDT has :peg/fiat, so it inherits :trust/normal through hierarchy
  (api-registry/with reg-inherited
    (let [usdt (api-currency/of :crypto/USDT)]
      {:has-peg-fiat?     (api-currency/of-trait? :peg/fiat usdt)
       :has-trust-normal? (api-currency/of-trait? :trust/normal usdt)
       :has-trust?        (api-currency/of-trait? :trust usdt)}))
  ;; => {:has-peg-fiat? true
  ;;     :has-trust-normal? true   ; <-- inherited via :peg/fiat -> :trust/normal
  ;;     :has-trust? true}         ; <-- and :trust/normal -> :trust

  ;; Group currencies by trust
  (api-registry/with reg-inherited
    (currencies-by-trust [:crypto/BTC :crypto/ETH :crypto/USDT :crypto/USDC :XXX]
                         reg-inherited))
  ;; => {:trust/full   [:crypto/BTC :crypto/ETH]
  ;;     :trust/normal [:crypto/USDT :crypto/USDC]  ; via :peg/fiat inheritance
  ;;     :trust/none   [:XXX]}

  ;; === Inspect the hierarchy ===
  (def traits-h (registry/hierarchy :traits reg-inherited))

  ;; Verify :peg/fiat now derives from :trust/normal
  (isa? traits-h :peg/fiat :trust/normal)
  ;; => true

  (isa? traits-h :peg/fiat :trust)
  ;; => true (transitive: :peg/fiat -> :trust/normal -> :trust)

  ;; All descendants of :trust
  (descendants traits-h :trust)
  ;; => #{:trust/full :trust/normal :trust/low :trust/none :peg/fiat :collateral/crypto}
  )

;;; ---------------------------------------------------------------------------
;;; Example 16: Practical trust-based filtering and validation
;;; ---------------------------------------------------------------------------

(defn acceptable-for-settlement?
  "Checks if currency meets minimum trust requirements for settlement."
  [currency-id min-trust-level registry]
  (let [curr (api-currency/of currency-id registry)
        trust-order {:trust/full 4, :trust/normal 3, :trust/low 2, :trust/none 1}
        curr-level  (trust-level currency-id registry)
        curr-score  (get trust-order curr-level 0)
        min-score   (get trust-order min-trust-level 0)]
    {:currency    currency-id
     :trust-level curr-level
     :acceptable? (>= curr-score min-score)}))

(defn filter-by-min-trust
  "Filters currencies that meet minimum trust level."
  [currency-ids min-trust registry]
  (filter #(:acceptable? (acceptable-for-settlement? % min-trust registry))
          currency-ids))

(comment
  (def reg (build-trust-hierarchy-with-inheritance))

  ;; Check settlement acceptability
  (acceptable-for-settlement? :crypto/BTC :trust/normal reg)
  ;; => {:currency :crypto/BTC, :trust-level :trust/full, :acceptable? true}

  (acceptable-for-settlement? :XXX :trust/normal reg)
  ;; => {:currency :XXX, :trust-level :trust/none, :acceptable? false}

  ;; Filter for high-trust settlement
  (filter-by-min-trust [:crypto/BTC :crypto/ETH :crypto/USDT :XXX] :trust/normal reg)
  ;; => (:crypto/BTC :crypto/ETH :crypto/USDT)
  )
