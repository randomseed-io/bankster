(ns io.randomseed.bankster.examples.formatting
  "Money formatting examples for different locales and contexts.

   Bankster supports locale-aware formatting that follows the conventions
   of each country (thousands separators, decimal separators, symbol position)."
  (:require [io.randomseed.bankster.money    :as money]
            [io.randomseed.bankster.api      :as api]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.scale    :as scale])
  (:import [java.util Locale]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Basic locale formatting
;;; ---------------------------------------------------------------------------

(def sample-amount #money[1234567.89 EUR])

(comment
  ;; Polish format
  (api/money-format sample-amount :pl_PL)
  ;; => "1 234 567,89 €"

  ;; US format
  (api/money-format sample-amount :en_US)
  ;; => "€1,234,567.89"

  ;; German format
  (api/money-format sample-amount :de_DE)
  ;; => "1.234.567,89 €"

  ;; British format
  (api/money-format sample-amount :en_GB)
  ;; => "€1,234,567.89"

  ;; French format
  (api/money-format sample-amount :fr_FR)
  ;; => "1 234 567,89 €"
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Formatting different currencies
;;; ---------------------------------------------------------------------------

(comment
  ;; PLN in Polish locale
  (api/money-format #money[1500.50 PLN] :pl_PL)
  ;; => "1 500,50 zł"

  ;; USD in US locale
  (api/money-format #money[1500.50 USD] :en_US)
  ;; => "$1,500.50"

  ;; GBP in British locale
  (api/money-format #money[1500.50 GBP] :en_GB)
  ;; => "£1,500.50"

  ;; JPY (no decimal places)
  (api/money-format #money[150000 JPY] :ja_JP)
  ;; => "￥150,000"

  ;; CHF in Swiss locale
  (api/money-format #money[1500.50 CHF] :de_CH)
  ;; => "CHF 1'500.50"
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Formatting options
;;; ---------------------------------------------------------------------------

(comment
  ;; Without thousands grouping
  (api/money-format #money[1234567.89 PLN] :pl_PL {:grouping false})
  ;; => "1234567,89 zł"

  ;; Change scale (rounding)
  (api/money-format #money[1234.5678 PLN] :pl_PL {:scale 0})
  ;; => "1 235 zł"

  ;; Min/max fraction digits
  (api/money-format #money[100 EUR] :en_US {:min-fraction-digits 2
                                         :max-fraction-digits 2})
  ;; => "€100.00"

  ;; Rounding mode
  (api/money-format #money[99.999 PLN] :pl_PL {:scale 2
                                            :rounding-mode :HALF_UP})
  ;; => "100,00 zł"
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: International invoice formatting
;;; ---------------------------------------------------------------------------

(defn format-for-country
  "Formats amount according to country conventions."
  [amount country-code]
  (let [locale (case country-code
                 :PL :pl_PL
                 :DE :de_DE
                 :US :en_US
                 :GB :en_GB
                 :FR :fr_FR
                 :JP :ja_JP
                 :CH :de_CH
                 :en_US)] ; default
    (api/money-format amount locale)))

(defn international-invoice
  "Generates invoice line items for different countries."
  [items recipient-country]
  (for [{:keys [description amount]} items]
    {:description      description
     :amount-raw       amount
     :amount-formatted (format-for-country amount recipient-country)}))

(comment
  (def invoice-items
    [{:description "Consulting services" :amount #money[5000.00 EUR]}
     {:description "Software license"    :amount #money[1250.50 EUR]}
     {:description "Training"            :amount #money[800.00 EUR]}])

  ;; Invoice for Polish client
  (international-invoice invoice-items :PL)
  ;; => ({:description "Consulting services"
  ;;      :amount-formatted "5 000,00 €"} ...)

  ;; Invoice for German client
  (international-invoice invoice-items :DE)
  ;; => ({:description "Consulting services"
  ;;      :amount-formatted "5.000,00 €"} ...)
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Custom currency symbol
;;; ---------------------------------------------------------------------------

(comment
  ;; Use custom symbol function
  (api/money-format #money[100 PLN] :pl_PL
                {:currency-symbol-fn (constantly "PLN")})
  ;; => "100,00 PLN" (instead of "zł")

  ;; Full currency name
  (api/money-format #money[100 EUR] :en_US
                {:currency-symbol-fn (fn [_] "Euro")})
  ;; => "Euro 100.00"
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: UI formatting (frontend display)
;;; ---------------------------------------------------------------------------

(defn format-for-ui
  "Formats amount for UI display with optional abbreviation."
  [amount locale & {:keys [abbreviate?] :or {abbreviate? false}}]
  (let [amt  (api/money-amount amount)
        curr (api/money-currency amount)]
    (cond
      (and abbreviate? (>= amt 1000000M))
      (str (api/money-format (api// amount 1000000M) locale {:scale 1})
           "M")

      (and abbreviate? (>= amt 1000M))
      (str (api/money-format (api// amount 1000M) locale {:scale 1})
           "K")

      :else
      (api/money-format amount locale))))

(comment
  (format-for-ui #money[1500000 PLN] :pl_PL :abbreviate? true)
  ;; => "1,5 zł M"

  (format-for-ui #money[25000 PLN] :pl_PL :abbreviate? true)
  ;; => "25,0 zł K"

  (format-for-ui #money[999 PLN] :pl_PL :abbreviate? true)
  ;; => "999,00 zł"
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Negative amount formatting
;;; ---------------------------------------------------------------------------

(comment
  ;; Negative amounts (e.g., refunds, corrections)
  (api/money-format #money[-500.00 PLN] :pl_PL)
  ;; => "-500,00 zł"

  (api/money-format #money[-1234.56 USD] :en_US)
  ;; => "-$1,234.56"

  ;; Different countries have different conventions for negatives
  (api/money-format #money[-100 EUR] :de_DE)
  ;; => "-100,00 €"
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Financial report export
;;; ---------------------------------------------------------------------------

(defn export-report
  "Exports financial report with multiple formats."
  [transactions]
  (for [tx transactions]
    {:id          (:id tx)
     :amount-pl   (api/money-format (:amount tx) :pl_PL)
     :amount-en   (api/money-format (:amount tx) :en_US)
     :amount-raw  (str (api/money-amount (:amount tx)))
     :currency    (name (api/currency-id (api/money-currency (:amount tx))))}))

(comment
  (def transactions
    [{:id 1 :amount #money[1500.00 PLN]}
     {:id 2 :amount #money[350.50 EUR]}
     {:id 3 :amount #money[99.99 USD]}])

  (export-report transactions)
  ;; => ({:id 1
  ;;      :amount-pl "1 500,00 zł"
  ;;      :amount-en "PLN1,500.00"
  ;;      :amount-raw "1500.00"
  ;;      :currency "PLN"} ...)
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Format comparison for major currencies
;;; ---------------------------------------------------------------------------

(def major-currencies
  "Major world currencies for demonstration."
  [:PLN :EUR :USD :GBP :CHF :JPY :CNY :CAD :AUD])

(defn show-formats
  "Shows formatting of same value in different currencies."
  [value]
  (for [curr major-currencies]
    (let [amount (api/money-of curr value)]
      {:currency curr
       :pl       (api/money-format amount :pl_PL)
       :en       (api/money-format amount :en_US)
       :native   (api/money-format amount (Locale/getDefault))})))

(comment
  (show-formats 1234.56M)
  ;; => ({:currency :PLN :pl "1 234,56 zł" :en "PLN1,234.56" ...}
  ;;     {:currency :EUR :pl "1 234,56 €" :en "€1,234.56" ...}
  ;;     {:currency :JPY :pl "1 235 ¥" :en "¥1,235" ...}  ; no cents
  ;;     ...)
  )
