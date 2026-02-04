;; Bankster precision and rounding patterns (no global with-precision macro).
;;
;; This file shows how to achieve Joda-Money-like behavior using explicit
;; operations: rescaling to nominal currency scale, contextual rounding, and
;; auto-scaled currencies for variable precision.

(ns io.randomseed.bankster.examples.precision
  "Precision and rounding patterns for Money/Currency."
  (:require [io.randomseed.bankster.api          :as          api]
            [io.randomseed.bankster.api.currency :as api-currency]
            [io.randomseed.bankster.api.money    :as    api-money]
            [io.randomseed.bankster.api.ops      :as      api-ops]
            [io.randomseed.bankster.scale        :as        scale]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Fixed scale (nominal currency scale)
;;; ---------------------------------------------------------------------------

(defn fixed-scale
  "Forces the nominal currency scale (Joda Money-like)."
  [m]
  (api-money/rescale m))

(comment
  ;; PLN has scale 2
  (fixed-scale #money[10.005 PLN])
  ;; => #money[10.01 PLN] (uses default rounding)

  ;; With explicit rounding context
  (scale/with-rounding :HALF_EVEN
    (fixed-scale #money[10.005 PLN]))
  ;; => #money[10.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Contextual rounding for a block
;;; ---------------------------------------------------------------------------

(comment
  (scale/with-rounding :HALF_UP
    (let [a (api-ops// #money[1 PLN] 3)
          b (api-ops/+ a #money[0.01 PLN])]
      (fixed-scale b)))
  ;; => #money[0.34 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Variable precision with auto-scaled currency
;;; ---------------------------------------------------------------------------

(def auto-cur
  "Ad-hoc auto-scaled currency (scale = -1)."
  (api-currency/new-currency :TEST/AUTO nil -1 :test/auto))

(comment
  ;; Auto-scaled currency keeps the amount scale as needed.
  (def a (api/money 1.23456789M auto-cur))
  (def b (api/money 0.00000001M auto-cur))

  (api/scale a) ; => 8
  (api/scale b) ; => 8

  (api-ops/+ a b)
  ;; => #money[1.23456790 TEST/AUTO]
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Explicit downscale (round-to / apply)
;;; ---------------------------------------------------------------------------

(comment
  ;; Round to a given scale (keeps original scale for Money)
  (api-money/round #money[12.3456 PLN] 2)
  ;; => #money[12.35 PLN]

  ;; Round to interval (e.g. 0.05)
  (api-money/round-to #money[1.23 PLN] 0.05M)
  ;; => #money[1.25 PLN]

  ;; Direct scale application for a number
  (scale/apply 12.3456M 2 :HALF_UP)
  ;; => 12.35M
  )
