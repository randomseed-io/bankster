(ns io.randomseed.bankster.examples.rounding
  "Rounding mode control examples for financial calculations.

   Bankster provides fine-grained control over rounding behavior,
   essential for regulatory compliance and financial accuracy."
  (:require [io.randomseed.bankster.money :as money]
            [io.randomseed.bankster.scale :as scale]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Available rounding modes
;;; ---------------------------------------------------------------------------

(comment
  ;; All standard Java BigDecimal rounding modes are available:

  ;; ROUND_UP - rounds away from zero
  ;; ROUND_DOWN - rounds toward zero (truncation)
  ;; ROUND_CEILING - rounds toward positive infinity
  ;; ROUND_FLOOR - rounds toward negative infinity
  ;; ROUND_HALF_UP - rounds to nearest, ties go up (common default)
  ;; ROUND_HALF_DOWN - rounds to nearest, ties go down
  ;; ROUND_HALF_EVEN - rounds to nearest, ties go to even (banker's rounding)
  ;; ROUND_UNNECESSARY - asserts exact result, throws if rounding needed
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Basic rounding operations
;;; ---------------------------------------------------------------------------

(def sample-amount #money[10.005 PLN])

(comment
  ;; Different rounding modes on same value
  (scale/apply sample-amount 2 :HALF_UP)
  ;; => #money[10.01 PLN]

  (scale/apply sample-amount 2 :HALF_DOWN)
  ;; => #money[10.00 PLN]

  (scale/apply sample-amount 2 :HALF_EVEN)  ; banker's rounding
  ;; => #money[10.00 PLN]

  (scale/apply sample-amount 2 :CEILING)
  ;; => #money[10.01 PLN]

  (scale/apply sample-amount 2 :FLOOR)
  ;; => #money[10.00 PLN]

  (scale/apply sample-amount 2 :UP)
  ;; => #money[10.01 PLN]

  (scale/apply sample-amount 2 :DOWN)
  ;; => #money[10.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Banker's rounding (HALF_EVEN)
;;; ---------------------------------------------------------------------------

;; Banker's rounding minimizes cumulative rounding error by rounding
;; to the nearest even number when the value is exactly halfway.

(comment
  ;; Ties go to even digit
  (scale/apply #money[10.005 PLN] 2 :HALF_EVEN)
  ;; => #money[10.00 PLN] (0 is even)

  (scale/apply #money[10.015 PLN] 2 :HALF_EVEN)
  ;; => #money[10.02 PLN] (2 is even)

  (scale/apply #money[10.025 PLN] 2 :HALF_EVEN)
  ;; => #money[10.02 PLN] (2 is even)

  (scale/apply #money[10.035 PLN] 2 :HALF_EVEN)
  ;; => #money[10.04 PLN] (4 is even)

  ;; Non-ties round normally
  (scale/apply #money[10.006 PLN] 2 :HALF_EVEN)
  ;; => #money[10.01 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Scoped rounding mode
;;; ---------------------------------------------------------------------------

(comment
  ;; Set rounding mode for a block of operations
  (scale/with-rounding :HALF_EVEN
    (money/div #money[100 PLN] 3))
  ;; => #money[33.33 PLN]

  (scale/with-rounding :HALF_UP
    (money/div #money[100 PLN] 3))
  ;; => #money[33.33 PLN]

  ;; Nested scopes
  (scale/with-rounding :HALF_UP
    (let [a (money/div #money[100 PLN] 3)]
      (scale/with-rounding :FLOOR
        (let [b (money/div #money[100 PLN] 3)]
          {:outer a :inner b}))))
  ;; => {:outer #money[33.33 PLN] :inner #money[33.33 PLN]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Division with rounding
;;; ---------------------------------------------------------------------------

(defn safe-divide
  "Divides money with explicit rounding mode."
  [amount divisor rounding-mode]
  (scale/with-rounding rounding-mode
    (money/div amount divisor)))

(comment
  ;; 100 / 3 with different rounding
  (safe-divide #money[100.00 PLN] 3 :HALF_UP)
  ;; => #money[33.33 PLN]

  (safe-divide #money[100.00 PLN] 3 :CEILING)
  ;; => #money[33.34 PLN]

  (safe-divide #money[100.00 PLN] 3 :FLOOR)
  ;; => #money[33.33 PLN]

  ;; Negative amounts
  (safe-divide #money[-100.00 PLN] 3 :CEILING)
  ;; => #money[-33.33 PLN] (toward positive infinity)

  (safe-divide #money[-100.00 PLN] 3 :FLOOR)
  ;; => #money[-33.34 PLN] (toward negative infinity)
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Rounding for tax calculations
;;; ---------------------------------------------------------------------------

(defn calculate-vat
  "Calculates VAT with configurable rounding (default: HALF_UP per Polish law)."
  [net-amount vat-rate & {:keys [rounding] :or {rounding :HALF_UP}}]
  (scale/with-rounding rounding
    (let [vat (money/mul net-amount vat-rate)]
      {:net   net-amount
       :vat   vat
       :gross (money/add net-amount vat)})))

(comment
  ;; Standard VAT calculation
  (calculate-vat #money[123.45 PLN] 0.23M)
  ;; => {:net #money[123.45 PLN]
  ;;     :vat #money[28.39 PLN]
  ;;     :gross #money[151.84 PLN]}

  ;; With banker's rounding (some jurisdictions)
  (calculate-vat #money[123.45 PLN] 0.23M :rounding :HALF_EVEN)
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Interest calculation with precision
;;; ---------------------------------------------------------------------------

(defn calculate-interest
  "Calculates interest with specified precision and rounding."
  [principal rate periods & {:keys [precision rounding]
                             :or   {precision 2 rounding :HALF_EVEN}}]
  (scale/with-rounding rounding
    (let [interest-factor (Math/pow (+ 1 (double (/ rate periods))) periods)
          final-amount    (money/mul principal (bigdec interest-factor))]
      {:principal    principal
       :rate         rate
       :periods      periods
       :final-amount (scale/apply final-amount precision rounding)
       :interest     (money/sub (scale/apply final-amount precision rounding)
                                principal)})))

(comment
  ;; 5% annual interest, monthly compounding
  (calculate-interest #money[10000.00 PLN] 0.05M 12)
  ;; => {:principal #money[10000.00 PLN]
  ;;     :rate 0.05M
  ;;     :periods 12
  ;;     :final-amount #money[10511.62 PLN]
  ;;     :interest #money[511.62 PLN]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Currency conversion with rounding
;;; ---------------------------------------------------------------------------

(defn convert-currency
  "Converts between currencies with specified rounding mode."
  [amount target-currency rate & {:keys [rounding] :or {rounding :HALF_UP}}]
  (scale/with-rounding rounding
    (money/of target-currency
              (* (money/amount amount) rate))))

(comment
  ;; EUR to PLN at 4.35
  (convert-currency #money[100.00 EUR] :PLN 4.35M)
  ;; => #money[435.00 PLN]

  ;; With fractional result
  (convert-currency #money[33.33 EUR] :PLN 4.35M)
  ;; => #money[144.99 PLN] (with HALF_UP)

  (convert-currency #money[33.33 EUR] :PLN 4.35M :rounding :FLOOR)
  ;; => #money[144.98 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Rounding to specific intervals
;;; ---------------------------------------------------------------------------

(comment
  ;; Round to nearest 0.05 (nickel rounding - common in some countries)
  (money/round-to #money[1.23 PLN] 0.05M)
  ;; => #money[1.25 PLN]

  (money/round-to #money[1.22 PLN] 0.05M)
  ;; => #money[1.20 PLN]

  ;; Round to nearest 0.10
  (money/round-to #money[1.24 PLN] 0.10M)
  ;; => #money[1.20 PLN]

  (money/round-to #money[1.25 PLN] 0.10M)
  ;; => #money[1.30 PLN]

  ;; Round to nearest whole unit
  (money/round-to #money[99.50 PLN] 1M)
  ;; => #money[100 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 10: Rescaling operations
;;; ---------------------------------------------------------------------------

(comment
  ;; Check if money is at currency's nominal scale
  (money/rescaled? #money[100.00 PLN])
  ;; => true

  (money/rescaled? #money[100.001 PLN])
  ;; => false (PLN has scale 2)

  ;; Rescale to currency's nominal scale
  (money/rescale #money[100.005 PLN])
  ;; => #money[100.01 PLN] (uses default rounding)

  (money/rescale #money[100.005 PLN] :FLOOR)
  ;; => #money[100.00 PLN]

  ;; Manual scale application
  (scale/apply #money[100.12345 PLN] 4 :HALF_UP)
  ;; => #money[100.1235 PLN]

  (scale/apply #money[100.12345 PLN] 0 :HALF_UP)
  ;; => #money[100 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 11: Financial report with consistent rounding
;;; ---------------------------------------------------------------------------

(defn generate-financial-report
  "Generates financial report with consistent rounding throughout."
  [transactions rounding-mode]
  (scale/with-rounding rounding-mode
    (let [totals (reduce
                  (fn [acc {:keys [amount type]}]
                    (case type
                      :income  (update acc :income money/add amount)
                      :expense (update acc :expense money/add amount)
                      acc))
                  {:income  #money[0 PLN]
                   :expense #money[0 PLN]}
                  transactions)]
      (assoc totals
             :balance (money/sub (:income totals) (:expense totals))
             :rounding-mode rounding-mode))))

(comment
  (def monthly-transactions
    [{:type :income  :amount #money[5000.00 PLN] :desc "Salary"}
     {:type :expense :amount #money[1500.00 PLN] :desc "Rent"}
     {:type :expense :amount #money[333.33 PLN]  :desc "Utilities"}
     {:type :expense :amount #money[666.67 PLN]  :desc "Food"}
     {:type :income  :amount #money[250.50 PLN]  :desc "Side gig"}])

  (generate-financial-report monthly-transactions :HALF_EVEN)
  ;; => {:income #money[5250.50 PLN]
  ;;     :expense #money[2500.00 PLN]
  ;;     :balance #money[2750.50 PLN]
  ;;     :rounding-mode :HALF_EVEN}
  )
