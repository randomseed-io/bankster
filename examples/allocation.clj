(ns io.randomseed.bankster.examples.allocation
  "Examples of sum-preserving money allocation.

   Key feature: the sum of allocated amounts ALWAYS equals the original
   amount - no 'lost pennies'."
  (:require [io.randomseed.bankster.money :as money]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Split a bill among friends
;;; ---------------------------------------------------------------------------

(defn split-bill
  "Splits a bill equally among n people."
  [bill num-people]
  (money/distribute bill num-people))

(comment
  ;; 100 PLN split among 3 people
  (split-bill #money[100.00 PLN] 3)
  ;; => [#money[33.34 PLN] #money[33.33 PLN] #money[33.33 PLN]]

  ;; Verify: sum = exactly 100.00 PLN
  (apply money/add (split-bill #money[100.00 PLN] 3))
  ;; => #money[100.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Split by ratios (unequal shares)
;;; ---------------------------------------------------------------------------

(defn split-by-shares
  "Splits amount according to given ratios.
   Ratios is a vector of numbers representing shares."
  [amount ratios]
  (money/allocate amount ratios))

(comment
  ;; Bill of 157.43 PLN: 50% / 30% / 20%
  (split-by-shares #money[157.43 PLN] [5 3 2])
  ;; => [#money[78.72 PLN] #money[47.23 PLN] #money[31.48 PLN]]

  ;; Verify sum
  (apply money/add (split-by-shares #money[157.43 PLN] [5 3 2]))
  ;; => #money[157.43 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Dividend distribution by company shares
;;; ---------------------------------------------------------------------------

(def shareholder-stakes
  "Percentage stakes of company shareholders."
  {:anna   35
   :bob    25
   :claire 40})

(defn distribute-dividend
  "Distributes dividend among shareholders according to their stakes."
  [dividend stakes]
  (let [names      (keys stakes)
        ratios     (vals stakes)
        allocation (money/allocate dividend ratios)]
    (zipmap names allocation)))

(comment
  (distribute-dividend #money[12500.00 PLN] shareholder-stakes)
  ;; => {:anna   #money[4375.00 PLN]
  ;;     :bob    #money[3125.00 PLN]
  ;;     :claire #money[5000.00 PLN]}

  ;; Verify: sum = dividend
  (->> (distribute-dividend #money[12500.00 PLN] shareholder-stakes)
       vals
       (apply money/add))
  ;; => #money[12500.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Edge cases in allocation
;;; ---------------------------------------------------------------------------

(comment
  ;; 10 PLN split 3 ways - classic "10 / 3" problem
  (money/allocate #money[10.00 PLN] [1 1 1])
  ;; => [#money[3.34 PLN] #money[3.33 PLN] #money[3.33 PLN]]
  ;; Remainder (1 grosz) goes to first position

  ;; 1 cent split 3 ways
  (money/allocate #money[0.01 PLN] [1 1 1])
  ;; => [#money[0.01 PLN] #money[0.00 PLN] #money[0.00 PLN]]

  ;; Negative amount (e.g., refund correction)
  (money/allocate #money[-100.00 PLN] [1 1])
  ;; => [#money[-50.00 PLN] #money[-50.00 PLN]]
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Project budget allocation
;;; ---------------------------------------------------------------------------

(def project-budget
  "IT project budget to allocate across categories."
  #money[500000.00 PLN])

(def cost-categories
  "Budget proportions for cost categories."
  {:salaries       60
   :infrastructure 20
   :licenses       10
   :training        5
   :reserve         5})

(defn allocate-budget
  "Allocates project budget according to cost categories."
  [budget categories]
  (let [names      (keys categories)
        ratios     (vals categories)
        allocation (money/allocate budget ratios)]
    (zipmap names allocation)))

(comment
  (allocate-budget project-budget cost-categories)
  ;; => {:salaries       #money[300000.00 PLN]
  ;;     :infrastructure #money[100000.00 PLN]
  ;;     :licenses       #money[50000.00 PLN]
  ;;     :training       #money[25000.00 PLN]
  ;;     :reserve        #money[25000.00 PLN]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Team bonus distribution by performance
;;; ---------------------------------------------------------------------------

(defn distribute-bonus
  "Distributes bonus pool according to employee performance scores."
  [bonus-pool performance-scores]
  (let [employees (keys performance-scores)
        scores    (vals performance-scores)
        bonuses   (money/allocate bonus-pool scores)]
    (zipmap employees bonuses)))

(comment
  (def performance
    {:john  120  ; above average
     :mary  100  ; standard
     :peter  80  ; below target
     :kate  150}) ; outstanding

  (distribute-bonus #money[10000.00 PLN] performance)
  ;; => {:john  #money[2666.67 PLN]
  ;;     :mary  #money[2222.22 PLN]
  ;;     :peter #money[1777.78 PLN]
  ;;     :kate  #money[3333.33 PLN]}
  )
