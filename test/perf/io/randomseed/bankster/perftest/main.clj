(ns io.randomseed.bankster.perftest.main

  (:require [criterium.core :refer [bench quick-bench]]
            [clojurewerkz.money.amounts    :as joda]
            [clojurewerkz.money.currencies :as joda-curr]
            [io.randomseed.bankster.money  :as bankster-money]
            [io.randomseed.bankster.scale  :as scale])

  (:import  [io.randomseed.bankster Currency Registry Money]
            [java.math BigDecimal BigInteger MathContext RoundingMode]
            [java.text NumberFormat DecimalFormat DecimalFormatSymbols]
            [java.util Locale]))

(def n 10000)

;; Joda-Money (Clojurewerkz)

(def joda-zero (joda/amount-of joda-curr/PLN 0))
(def joda-item (joda/amount-of joda-curr/PLN 12.34))
(def joda-vector (vec (repeat n joda-item)))

;; Bankster

(def bankster-zero (bankster-money/of 0 "PLN"))
(def bankster-item (bankster-money/of 12.34 "PLN"))
(def bankster-vector (vec (repeat n bankster-item)))

;; 3. Pure BigDecimal (Baseline)

(def bigdec-zero (BigDecimal. "0"))
(def bigdec-item (BigDecimal. "12.34"))
(def bigdec-vector (vec (repeat n bigdec-item)))

;; Test parameters

(def divisor-bd (BigDecimal. "3"))
(def div-by-long 3)                           ;; long divider
(def ^BigDecimal div-by-bd (BigDecimal. "3")) ;; BigDecimal divider

(def ratios-3 [1 1 1])                        ;; simple
(def ratios-6 [1 2 3])                        ;; uneven

;;
;; Microbench: old vs new Money/long division path (fixed-scale currency)
;;

(defn bankster-div-old-fixed
  "Simulates the old fixed-scale Money/number division path:
  scale/apply(divisor) + BigDecimal.divide(divisor, scale, rounding-mode)."
  [^Money x d]
  (let [^BigDecimal am (.amount ^Money x)
        ^RoundingMode rm (or scale/*rounding-mode* scale/ROUND_UNNECESSARY)]
    (Money. (.currency ^Money x)
            (.divide ^BigDecimal am
                     ^BigDecimal (scale/apply d)
                     (int (.scale ^BigDecimal am))
                     ^RoundingMode rm))))

(defn bankster-div-new-fixed
  "Simulates the new fixed-scale Money/integral division fast-path:
  BigDecimal.divide(divisor, rounding-mode) (scale taken from dividend)."
  [^Money x ^long d]
  (let [^BigDecimal am (.amount ^Money x)
        ^RoundingMode rm (or scale/*rounding-mode* scale/ROUND_UNNECESSARY)]
    (Money. (.currency ^Money x)
            (.divide ^BigDecimal am
                     ^BigDecimal (BigDecimal/valueOf d)
                     ^RoundingMode rm))))

(defn -main [& _]
  ;; ----------------------------
  ;; 1) SUM
  ;; ----------------------------

  (println "SUM of " n "elements (reduce).")

  (println "\n--- 1. Baseline: Raw BigDecimal ---")
  (quick-bench
   (reduce #(.add ^BigDecimal %1 %2) bigdec-zero bigdec-vector))

  (println "\n--- 2. Clojurewerkz (Joda-Money) ---")
  (quick-bench
   (reduce joda/plus joda-zero joda-vector))

  (println "\n--- 3. Bankster ---")
  (quick-bench
   (reduce bankster-money/add bankster-zero bankster-vector))

  ;; ------------------------------------------------------------
  ;; DIVIDE + SUM
  ;; ------------------------------------------------------------

  (println "\nDIVIDE(each) + SUM of" n "elements.")

  (println "\n--- 1. Baseline: Raw BigDecimal (divide each + sum) ---")
  (quick-bench
   (reduce (fn [^BigDecimal acc ^BigDecimal x]
             (.add ^BigDecimal acc ^BigDecimal (.divide x ^BigDecimal divisor-bd ^RoundingMode RoundingMode/HALF_UP)))
           bigdec-zero
           bigdec-vector))

  (println "\n--- 2. Clojurewerkz (Joda-Money) (divide each + sum) ---")
  (quick-bench
   (reduce (fn [acc x]
             (joda/plus acc (joda/divide x 3 :half-up)))
           joda-zero
           joda-vector))

  (println "\n--- 3. Bankster (divide each + sum) ---")
  (quick-bench
   (bankster-money/with-rounding HALF_UP
     (reduce (fn [acc x]
               (bankster-money/add acc (bankster-money/div x 3)))
             bankster-zero
             bankster-vector)))

  ;; ------------------------------------------------------------
  ;; Microbench: Money/long division path (old vs new)
  ;; ------------------------------------------------------------

  (println "\nBankster microbench: Money/long division path (fixed scale).")
  (binding [scale/*rounding-mode* RoundingMode/HALF_UP]
    (println "\n--- 1. Bankster money/div (current) ---")
    (quick-bench (bankster-money/div bankster-item div-by-long))

    (println "\n--- 2. Simulated old path (divide scale+RM) ---")
    (quick-bench (bankster-div-old-fixed bankster-item div-by-long))

    (println "\n--- 3. Simulated new path (divide RM only) ---")
    (quick-bench (bankster-div-new-fixed bankster-item div-by-long))))
