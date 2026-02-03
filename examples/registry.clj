(ns io.randomseed.bankster.examples.registry
  "Currency registry examples for multi-tenant and custom currency scenarios.

   Bankster uses a registry system to store currency definitions,
   supporting custom currencies, dynamic scoping, and hierarchical classification."
  (:require [io.randomseed.bankster.money    :as    money]
            [io.randomseed.bankster.api      :as api]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.init     :as init]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Inspecting the default registry
;;; ---------------------------------------------------------------------------

(comment
  ;; Get the global registry
  (api/registry-state)
  ;; => #Registry{...}

  ;; List all currencies
  (count (api/currency-all))
  ;; => ~200+ currencies (ISO + crypto)

  ;; List ISO currencies only
  (count (api/currency-of-domain :ISO-4217))

  ;; List crypto currencies
  (count (api/currency-of-domain :CRYPTO))

  ;; Get specific currency
  (api/currency-of :PLN)
  ;; => #currency{:id :PLN, :numeric 985, :scale 2,
  ;;              :kind :iso/fiat, :domain :ISO-4217}

  (api/currency-of :crypto/ETH)
  ;; => #currency{:id :crypto/ETH, :numeric nil, :scale 18,
  ;;              :kind :crypto/coin, :domain :CRYPTO}
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Creating custom currencies
;;; ---------------------------------------------------------------------------

(defn create-loyalty-points
  "Creates a loyalty points currency."
  [company-code]
  (currency/new-currency
   (keyword (str company-code "/POINTS"))
   0                                    ; no decimal places
   (keyword (str "loyalty/" company-code)))) ; kind

(defn create-virtual-currency
  "Creates a virtual currency for games/apps."
  [name scale]
  (currency/new-currency
   (keyword (str "virtual/" name))
   scale
   :virtual/token))

(comment
  (create-loyalty-points "ACME")
  ;; => #currency{:id :ACME/POINTS, :scale 0, :kind :loyalty/ACME}

  (create-virtual-currency "GOLD" 2)
  ;; => #currency{:id :virtual/GOLD, :scale 2, :kind :virtual/token}
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Registering custom currencies
;;; ---------------------------------------------------------------------------

(def custom-registry
  "Registry with custom currencies added."
  (-> (api/registry-state)
      (api/currency-register (currency/new-currency :ACME/POINTS 0 :loyalty/points))
      (api/currency-register (currency/new-currency :GAME/GOLD 2 :virtual/token))
      (api/currency-register (currency/new-currency :GAME/GEMS 0 :virtual/token))))

(comment
  ;; Use custom registry in scope
  (api/with-registry custom-registry
    (api/money-of :ACME/POINTS 1500))
  ;; => #money[1500 ACME/POINTS]

  (api/with-registry custom-registry
    (api/money-of :GAME/GOLD 99.50M))
  ;; => #money[99.50 GAME/GOLD]

  ;; Query custom currencies
  (api/with-registry custom-registry
    (api/currency-of-kind :virtual/token))
  ;; => [#currency GAME/GOLD, #currency GAME/GEMS]
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Dynamic registry scoping
;;; ---------------------------------------------------------------------------

(defn process-payment
  "Processes payment using current registry."
  [amount-str]
  (let [amount (api/money-parse amount-str)]
    {:parsed   amount
     :currency (api/currency-id (api/money-currency amount))
     :valid?   (api/currency-definitive? (api/money-currency amount))}))

(comment
  ;; With default registry
  (process-payment "100 PLN")
  ;; => {:parsed #money[100.00 PLN], :currency :PLN, :valid? true}

  ;; With custom registry
  (api/with-registry custom-registry
    (process-payment "500 ACME/POINTS"))
  ;; => {:parsed #money[500 ACME/POINTS], :currency :ACME/POINTS, :valid? true}
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Multi-tenant currency support
;;; ---------------------------------------------------------------------------

(defn create-tenant-registry
  "Creates a registry for a specific tenant with their custom currencies."
  [tenant-id custom-currencies]
  (reduce
   (fn [reg {:keys [id scale kind]}]
     (api/currency-register reg (currency/new-currency id scale kind)))
   (api/registry-state)
   custom-currencies))

(def tenant-registries
  "Registries for different tenants."
  {:tenant-a (create-tenant-registry
              :tenant-a
              [{:id :TENANT-A/CREDITS :scale 2 :kind :credits/internal}])
   :tenant-b (create-tenant-registry
              :tenant-b
              [{:id :TENANT-B/TOKENS :scale 0 :kind :tokens/internal}
               {:id :TENANT-B/BONUS  :scale 2 :kind :bonus/internal}])})

(defn with-tenant
  "Executes operation in tenant's currency context."
  [tenant-id f]
  (if-let [reg (get tenant-registries tenant-id)]
    (api/with-registry reg (f))
    (throw (ex-info "Unknown tenant" {:tenant-id tenant-id}))))

(comment
  ;; Tenant A operations
  (with-tenant :tenant-a
    #(api/money-of :TENANT-A/CREDITS 100.50M))
  ;; => #money[100.50 TENANT-A/CREDITS]

  ;; Tenant B operations
  (with-tenant :tenant-b
    #(api/+ (api/money-of :TENANT-B/TOKENS 100)
                (api/money-of :TENANT-B/TOKENS 50)))
  ;; => #money[150 TENANT-B/TOKENS]
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Currency lookup methods
;;; ---------------------------------------------------------------------------

(comment
  ;; By ID (keyword)
  (api/currency-of :EUR)
  ;; => #currency EUR

  ;; By numeric code
  (api/currency-of 978)  ; EUR numeric code
  ;; => #currency EUR

  ;; By country
  (currency/of-country :PL)
  ;; => #currency PLN

  (currency/of-country :US)
  ;; => #currency USD

  ;; By domain
  (api/currency-of-domain :ISO-4217)
  ;; => [#currency AED, #currency AFN, ...]

  (api/currency-of-domain :CRYPTO)
  ;; => [#currency crypto/BTC, #currency crypto/ETH, ...]

  ;; By kind
  (api/currency-of-kind :iso/fiat)
  ;; => all fiat currencies

  (api/currency-of-kind :crypto/coin)
  ;; => all crypto coins
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Currency properties and traits
;;; ---------------------------------------------------------------------------

(comment
  ;; Get currency properties
  (let [eur (api/currency-of :EUR)]
    {:id      (api/currency-id eur)
     :numeric (api/currency-nr eur)
     :scale   (api/currency-scale eur)
     :domain  (api/currency-domain eur)
     :kind    (api/currency-kind eur)})
  ;; => {:id :EUR, :numeric 978, :scale 2,
  ;;     :domain :ISO-4217, :kind :iso/fiat}

  ;; Check currency characteristics
  (api/currency-crypto? (api/currency-of :crypto/BTC))
  ;; => true

  (currency/iso-strict? (api/currency-of :EUR))
  ;; => true

  ;; Domain hierarchy
  (currency/in-domain? (api/currency-of :EUR) :ISO-4217)
  ;; => true
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Modifying registry (mutable operations)
;;; ---------------------------------------------------------------------------

(comment
  ;; WARNING: These modify the global registry!
  ;; Use only for initialization or testing.

  ;; Register new currency globally
  (api/currency-register! (currency/new-currency :TEST/COIN 8 :test/currency))

  ;; Now available globally
  (api/money-of :TEST/COIN 1.23456789M)
  ;; => #money[1.23456789 TEST/COIN]

  ;; Unregister
  (api/currency-unregister! :TEST/COIN)

  ;; Safer approach: use with-registry for scoped changes
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Currency weights (conflict resolution)
;;; ---------------------------------------------------------------------------

;; When multiple currencies have the same code, weight determines priority

(comment
  ;; Get currency weight
  (currency/weight (api/currency-of :USD))
  ;; => default weight

  ;; Create registry with weighted currencies
  (def weighted-registry
    (-> (api/registry-state)
        (api/currency-register
         (currency/with-weight
           (currency/new-currency :USD 2 :custom/usd)
           100))))  ; higher weight = higher priority

  ;; In lookup, higher weight wins when codes conflict
  )

;;; ---------------------------------------------------------------------------
;;; Example 10: Building a complete custom registry
;;; ---------------------------------------------------------------------------

(defn build-gaming-registry
  "Builds a complete registry for a gaming application."
  []
  (-> (registry/new-registry)
      ;; Add real currencies for purchases
      (api/currency-register (api/currency-of :USD))
      (api/currency-register (api/currency-of :EUR))
      (api/currency-register (api/currency-of :PLN))
      ;; Add game currencies
      (api/currency-register (currency/new-currency :GAME/GOLD 2 :game/soft))
      (api/currency-register (currency/new-currency :GAME/GEMS 0 :game/hard))
      (api/currency-register (currency/new-currency :GAME/XP 0 :game/experience))
      ;; Add seasonal currencies
      (api/currency-register (currency/new-currency :GAME/SNOW 0 :game/seasonal))
      (api/currency-register (currency/new-currency :GAME/HEARTS 0 :game/seasonal))))

(comment
  (def gaming-registry (build-gaming-registry))

  (api/with-registry gaming-registry
    (let [purchase-price #money[4.99 USD]
          gems-granted   (api/money-of :GAME/GEMS 500)
          bonus-gold     (api/money-of :GAME/GOLD 1000.00M)]
      {:purchase purchase-price
       :rewards  {:gems gems-granted
                  :gold bonus-gold}}))
  ;; => {:purchase #money[4.99 USD]
  ;;     :rewards {:gems #money[500 GAME/GEMS]
  ;;               :gold #money[1000.00 GAME/GOLD]}}
  )

;;; ---------------------------------------------------------------------------
;;; Example 11: Registry initialization control
;;; ---------------------------------------------------------------------------

(comment
  ;; Disable auto-initialization when loading namespace
  (binding [io.randomseed.bankster/*initialize-registry* false]
    (require '[io.randomseed.bankster.currency :as c] :reload))

  ;; Now initialize manually with custom config (with optional overlay on dist config)
  (init/load-registry! "path/to/custom-config.edn"
                       {:keep-dist? true
                        :merge-opts {:preserve-fields [:domain :kind]
                                     :iso-like? true}})
  )
