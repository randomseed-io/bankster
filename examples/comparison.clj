(ns io.randomseed.bankster.examples.comparison
  "Comparison and sorting examples for monetary amounts.

   Bankster provides type-safe comparisons that ensure amounts
   are in the same currency before comparing."
  (:require [io.randomseed.bankster.money    :as money]
            [io.randomseed.bankster.api      :as api]
            [io.randomseed.bankster.currency :as currency]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Basic comparisons
;;; ---------------------------------------------------------------------------

(comment
  ;; Equality
  (api/= #money[100.00 PLN] #money[100 PLN])
  ;; => true (same value, different scale representation)

  (api/= #money[100 PLN] #money[100 EUR])
  ;; => false (different currencies)

  ;; Inequality
  (api/not= #money[100 PLN] #money[99 PLN])
  ;; => true

  ;; Greater than / less than
  (api/> #money[100 PLN] #money[50 PLN])
  ;; => true

  (api/< #money[50 PLN] #money[100 PLN])
  ;; => true

  ;; Greater/less than or equal
  (api/>= #money[100 PLN] #money[100 PLN])
  ;; => true

  (api/<= #money[100 PLN] #money[100 PLN])
  ;; => true
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Sign predicates
;;; ---------------------------------------------------------------------------

(comment
  ;; Zero check
  (api/money-is-zero? #money[0 PLN])
  ;; => true

  (api/money-is-zero? #money[0.00 PLN])
  ;; => true

  ;; Positive/negative
  (api/pos? #money[100 PLN])
  ;; => true

  (api/neg? #money[-50 PLN])
  ;; => true

  ;; Combined predicates
  (money/is-pos-or-zero? #money[0 PLN])
  ;; => true

  (money/is-neg-or-zero? #money[-10 PLN])
  ;; => true
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Three-way comparison
;;; ---------------------------------------------------------------------------

(comment
  ;; Returns -1, 0, or 1
  (api/compare #money[50 PLN] #money[100 PLN])
  ;; => -1

  (api/compare #money[100 PLN] #money[100 PLN])
  ;; => 0

  (api/compare #money[150 PLN] #money[100 PLN])
  ;; => 1

  ;; Compare amounts only (ignores currency for comparison)
  (money/compare-amounts #money[100 PLN] #money[100 EUR])
  ;; => 0 (same numeric value)
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Sorting collections
;;; ---------------------------------------------------------------------------

(def prices
  [#money[99.99 PLN]
   #money[149.00 PLN]
   #money[79.50 PLN]
   #money[199.99 PLN]
   #money[49.00 PLN]])

(comment
  ;; Sort ascending
  (sort api/compare prices)
  ;; => (#money[49.00 PLN]
  ;;     #money[79.50 PLN]
  ;;     #money[99.99 PLN]
  ;;     #money[149.00 PLN]
  ;;     #money[199.99 PLN])

  ;; Sort descending
  (sort (fn [a b] (api/compare b a)) prices)
  ;; => (#money[199.99 PLN]
  ;;     #money[149.00 PLN]
  ;;     #money[99.99 PLN]
  ;;     #money[79.50 PLN]
  ;;     #money[49.00 PLN])

  ;; Using sort-by with amount extraction
  (sort-by api/money-amount prices)
  ;; => sorted by BigDecimal value
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Finding min/max
;;; ---------------------------------------------------------------------------

(defn find-min
  "Finds minimum amount in collection."
  [amounts]
  (reduce (fn [a b] (if (api/< a b) a b)) amounts))

(defn find-max
  "Finds maximum amount in collection."
  [amounts]
  (reduce (fn [a b] (if (api/> a b) a b)) amounts))

(comment
  (find-min prices)
  ;; => #money[49.00 PLN]

  (find-max prices)
  ;; => #money[199.99 PLN]

  ;; Alternative using sort
  (first (sort api/compare prices))
  ;; => #money[49.00 PLN]

  (last (sort api/compare prices))
  ;; => #money[199.99 PLN]

  ;; Using apply with min-key/max-key
  (apply min-key api/money-amount prices)
  ;; => #money[49.00 PLN]

  (apply max-key api/money-amount prices)
  ;; => #money[199.99 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Currency-safe comparisons
;;; ---------------------------------------------------------------------------

(defn same-currency?
  "Checks if all amounts are in the same currency."
  [& amounts]
  (apply api/money-same-currencies? amounts))

(defn compare-safe
  "Compares amounts only if they're in the same currency."
  [a b]
  (if (api/money-same-currencies? a b)
    {:comparable true
     :result     (api/compare a b)}
    {:comparable false
     :error      "Cannot compare amounts in different currencies"}))

(comment
  (same-currency? #money[100 PLN] #money[50 PLN] #money[75 PLN])
  ;; => true

  (same-currency? #money[100 PLN] #money[50 EUR])
  ;; => false

  (compare-safe #money[100 PLN] #money[50 PLN])
  ;; => {:comparable true, :result 1}

  (compare-safe #money[100 PLN] #money[50 EUR])
  ;; => {:comparable false, :error "Cannot compare..."}
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Range checks
;;; ---------------------------------------------------------------------------

(defn in-range?
  "Checks if amount is within specified range (inclusive)."
  [amount min-val max-val]
  (and (api/>= amount min-val)
       (api/<= amount max-val)))

(defn above-threshold?
  "Checks if amount exceeds threshold."
  [amount threshold]
  (api/> amount threshold))

(defn below-limit?
  "Checks if amount is below limit."
  [amount limit]
  (api/< amount limit))

(comment
  (in-range? #money[100 PLN] #money[50 PLN] #money[150 PLN])
  ;; => true

  (in-range? #money[200 PLN] #money[50 PLN] #money[150 PLN])
  ;; => false

  (above-threshold? #money[1000 PLN] #money[500 PLN])
  ;; => true

  (below-limit? #money[100 PLN] #money[500 PLN])
  ;; => true
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Filtering by amount
;;; ---------------------------------------------------------------------------

(def transactions
  [{:id 1 :amount #money[50.00 PLN]  :type :purchase}
   {:id 2 :amount #money[500.00 PLN] :type :purchase}
   {:id 3 :amount #money[25.00 PLN]  :type :refund}
   {:id 4 :amount #money[1500.00 PLN] :type :purchase}
   {:id 5 :amount #money[75.00 PLN]  :type :purchase}])

(defn filter-by-amount
  "Filters transactions by amount criteria."
  [txs {:keys [min-amount max-amount]}]
  (filter
   (fn [{:keys [amount]}]
     (and (or (nil? min-amount) (api/>= amount min-amount))
          (or (nil? max-amount) (api/<= amount max-amount))))
   txs))

(comment
  ;; Transactions between 50 and 500 PLN
  (filter-by-amount transactions
                    {:min-amount #money[50 PLN]
                     :max-amount #money[500 PLN]})
  ;; => ({:id 1 ...} {:id 2 ...} {:id 5 ...})

  ;; Transactions above 100 PLN
  (filter-by-amount transactions {:min-amount #money[100 PLN]})
  ;; => ({:id 2 ...} {:id 4 ...})
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Grouping by amount ranges
;;; ---------------------------------------------------------------------------

(defn amount-tier
  "Categorizes amount into tiers."
  [amount]
  (cond
    (api/< amount #money[100 PLN])  :small
    (api/< amount #money[500 PLN])  :medium
    (api/< amount #money[1000 PLN]) :large
    :else                                :enterprise))

(defn group-by-tier
  "Groups transactions by amount tier."
  [txs]
  (group-by (comp amount-tier :amount) txs))

(comment
  (group-by-tier transactions)
  ;; => {:small [{:id 1 ...} {:id 3 ...} {:id 5 ...}]
  ;;     :medium [{:id 2 ...}]
  ;;     :enterprise [{:id 4 ...}]}

  ;; Count per tier
  (->> transactions
       group-by-tier
       (map (fn [[k v]] [k (count v)]))
       (into {}))
  ;; => {:small 3, :medium 1, :enterprise 1}
  )

;;; ---------------------------------------------------------------------------
;;; Example 10: Price comparison for products
;;; ---------------------------------------------------------------------------

(def products
  [{:name "Widget A" :price #money[29.99 PLN]}
   {:name "Widget B" :price #money[24.99 PLN]}
   {:name "Widget C" :price #money[34.99 PLN]}
   {:name "Widget D" :price #money[24.99 PLN]}])

(defn cheapest-products
  "Finds the cheapest product(s)."
  [products]
  (let [min-price (apply min-key (comp api/money-amount :price) products)
        min-amount (:price min-price)]
    (filter #(api/= (:price %) min-amount) products)))

(defn price-sorted
  "Returns products sorted by price."
  [products direction]
  (let [comparator (if (= direction :asc)
                     (fn [a b] (api/compare (:price a) (:price b)))
                     (fn [a b] (api/compare (:price b) (:price a))))]
    (sort comparator products)))

(comment
  (cheapest-products products)
  ;; => ({:name "Widget B" :price #money[24.99 PLN]}
  ;;     {:name "Widget D" :price #money[24.99 PLN]})

  (price-sorted products :asc)
  ;; => ({:name "Widget B" ...} {:name "Widget D" ...}
  ;;     {:name "Widget A" ...} {:name "Widget C" ...})

  (price-sorted products :desc)
  ;; => ({:name "Widget C" ...} {:name "Widget A" ...}
  ;;     {:name "Widget B" ...} {:name "Widget D" ...})
  )

;;; ---------------------------------------------------------------------------
;;; Example 11: Using inter-ops for cleaner syntax
;;; ---------------------------------------------------------------------------

(comment
  ;; The money.inter-ops namespace provides operator-like functions
  (require '[io.randomseed.bankster.money.inter-ops :as ops])

  ;; These work with both Money and regular numbers
  (ops/> #money[100 PLN] #money[50 PLN])
  ;; => true

  (ops/< #money[50 PLN] #money[100 PLN])
  ;; => true

  (ops/= #money[100 PLN] #money[100 PLN])
  ;; => true

  ;; Can be used in threading macros
  (-> #money[100 PLN]
      (ops/> #money[50 PLN]))
  ;; => true
  )
