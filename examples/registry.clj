(ns io.randomseed.bankster.examples.registry
  "Currency registry examples for multi-tenant and custom currency scenarios.

   Bankster uses a registry system to store currency definitions,
   supporting custom currencies, dynamic scoping, and hierarchical classification."
  (:require [io.randomseed.bankster.money    :as money]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.registry :as registry]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Inspecting the default registry
;;; ---------------------------------------------------------------------------

(comment
  ;; Get the global registry
  (registry/state)
  ;; => #Registry{...}

  ;; List all currencies
  (count (currency/all))
  ;; => ~200+ currencies (ISO + crypto)

  ;; List ISO currencies only
  (count (currency/of-domain :ISO-4217))

  ;; List crypto currencies
  (count (currency/of-domain :CRYPTO))

  ;; Get specific currency
  (currency/of :PLN)
  ;; => #currency{:id :PLN, :numeric 985, :scale 2,
  ;;              :kind :iso/fiat, :domain :ISO-4217}

  (currency/of :crypto/ETH)
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
  (-> (registry/state)
      (currency/register (currency/new-currency :ACME/POINTS 0 :loyalty/points))
      (currency/register (currency/new-currency :GAME/GOLD 2 :virtual/token))
      (currency/register (currency/new-currency :GAME/GEMS 0 :virtual/token))))

(comment
  ;; Use custom registry in scope
  (currency/with-registry custom-registry
    (money/of :ACME/POINTS 1500))
  ;; => #money[1500 ACME/POINTS]

  (currency/with-registry custom-registry
    (money/of :GAME/GOLD 99.50M))
  ;; => #money[99.50 GAME/GOLD]

  ;; Query custom currencies
  (currency/with-registry custom-registry
    (currency/of-kind :virtual/token))
  ;; => [#currency GAME/GOLD, #currency GAME/GEMS]
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Dynamic registry scoping
;;; ---------------------------------------------------------------------------

(defn process-payment
  "Processes payment using current registry."
  [amount-str]
  (let [amount (money/parse amount-str)]
    {:parsed   amount
     :currency (currency/id (money/currency amount))
     :valid?   (currency/definitive? (money/currency amount))}))

(comment
  ;; With default registry
  (process-payment "100 PLN")
  ;; => {:parsed #money[100.00 PLN], :currency :PLN, :valid? true}

  ;; With custom registry
  (currency/with-registry custom-registry
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
     (currency/register reg (currency/new-currency id scale kind)))
   (registry/state)
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
    (currency/with-registry reg (f))
    (throw (ex-info "Unknown tenant" {:tenant-id tenant-id}))))

(comment
  ;; Tenant A operations
  (with-tenant :tenant-a
    #(money/of :TENANT-A/CREDITS 100.50M))
  ;; => #money[100.50 TENANT-A/CREDITS]

  ;; Tenant B operations
  (with-tenant :tenant-b
    #(money/add (money/of :TENANT-B/TOKENS 100)
                (money/of :TENANT-B/TOKENS 50)))
  ;; => #money[150 TENANT-B/TOKENS]
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Currency lookup methods
;;; ---------------------------------------------------------------------------

(comment
  ;; By ID (keyword)
  (currency/of :EUR)
  ;; => #currency EUR

  ;; By numeric code
  (currency/of 978)  ; EUR numeric code
  ;; => #currency EUR

  ;; By country
  (currency/of-country :PL)
  ;; => #currency PLN

  (currency/of-country :US)
  ;; => #currency USD

  ;; By domain
  (currency/of-domain :ISO-4217)
  ;; => [#currency AED, #currency AFN, ...]

  (currency/of-domain :CRYPTO)
  ;; => [#currency crypto/BTC, #currency crypto/ETH, ...]

  ;; By kind
  (currency/of-kind :iso/fiat)
  ;; => all fiat currencies

  (currency/of-kind :crypto/coin)
  ;; => all crypto coins
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Currency properties and traits
;;; ---------------------------------------------------------------------------

(comment
  ;; Get currency properties
  (let [eur (currency/of :EUR)]
    {:id      (currency/id eur)
     :numeric (currency/nr eur)
     :scale   (currency/scale eur)
     :domain  (currency/domain eur)
     :kind    (currency/kind eur)})
  ;; => {:id :EUR, :numeric 978, :scale 2,
  ;;     :domain :ISO-4217, :kind :iso/fiat}

  ;; Check currency characteristics
  (currency/crypto? (currency/of :crypto/BTC))
  ;; => true

  (currency/iso-strict? (currency/of :EUR))
  ;; => true

  ;; Domain hierarchy
  (currency/in-domain? (currency/of :EUR) :ISO-4217)
  ;; => true
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Modifying registry (mutable operations)
;;; ---------------------------------------------------------------------------

(comment
  ;; WARNING: These modify the global registry!
  ;; Use only for initialization or testing.

  ;; Register new currency globally
  (currency/register! (currency/new-currency :TEST/COIN 8 :test/currency))

  ;; Now available globally
  (money/of :TEST/COIN 1.23456789M)
  ;; => #money[1.23456789 TEST/COIN]

  ;; Unregister
  (currency/unregister! :TEST/COIN)

  ;; Safer approach: use with-registry for scoped changes
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Currency weights (conflict resolution)
;;; ---------------------------------------------------------------------------

;; When multiple currencies have the same code, weight determines priority

(comment
  ;; Get currency weight
  (currency/weight (currency/of :USD))
  ;; => default weight

  ;; Create registry with weighted currencies
  (def weighted-registry
    (-> (registry/state)
        (currency/register
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
      (currency/register (currency/of :USD))
      (currency/register (currency/of :EUR))
      (currency/register (currency/of :PLN))
      ;; Add game currencies
      (currency/register (currency/new-currency :GAME/GOLD 2 :game/soft))
      (currency/register (currency/new-currency :GAME/GEMS 0 :game/hard))
      (currency/register (currency/new-currency :GAME/XP 0 :game/experience))
      ;; Add seasonal currencies
      (currency/register (currency/new-currency :GAME/SNOW 0 :game/seasonal))
      (currency/register (currency/new-currency :GAME/HEARTS 0 :game/seasonal))))

(comment
  (def gaming-registry (build-gaming-registry))

  (currency/with-registry gaming-registry
    (let [purchase-price #money[4.99 USD]
          gems-granted   (money/of :GAME/GEMS 500)
          bonus-gold     (money/of :GAME/GOLD 1000.00M)]
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

  ;; Now initialize manually with custom config
  (registry/set-default-registry!
   (registry/load-registry "path/to/custom-config.edn"))
  )
