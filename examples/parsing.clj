(ns io.randomseed.bankster.examples.parsing
  "Parsing and validation of monetary amounts from user input.

   Bankster handles various input formats: strings with currency,
   major/minor units, and type conversions."
  (:require [io.randomseed.bankster.money    :as money]
            [io.randomseed.bankster.api      :as api]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.scale    :as scale]
            [clojure.string                  :as str]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Basic string parsing
;;; ---------------------------------------------------------------------------

(comment
  ;; Format: "amount currency"
  (api/money-parse "100.50 EUR")
  ;; => #money[100.50 EUR]

  ;; Format: "currency amount"
  (api/money-parse "PLN 1234.56")
  ;; => #money[1234.56 PLN]

  ;; With comma as decimal separator
  (api/money-parse "PLN 1234,56")
  ;; => #money[1234.56 PLN]

  ;; Large numbers
  (api/money-parse "1000000.00 USD")
  ;; => #money[1000000.00 USD]
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Parsing major/minor units
;;; ---------------------------------------------------------------------------

(comment
  ;; Major units (dollars, euros, zloty)
  (api/money-parse-major "1234" :PLN)
  ;; => #money[1234.00 PLN]

  ;; Minor units (cents, grosze)
  (api/money-parse-minor "12345" :PLN)
  ;; => #money[123.45 PLN]

  ;; Useful when importing from banking systems
  ;; that store amounts in cents
  (api/money-parse-minor "999" :EUR)
  ;; => #money[9.99 EUR]

  ;; JPY has no minor units
  (api/money-parse-minor "1000" :JPY)
  ;; => #money[1000 JPY]
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Safe parsing with error handling
;;; ---------------------------------------------------------------------------

(defn safe-parse
  "Parses amount returning nil for invalid data."
  [input]
  (try
    (api/money-parse input)
    (catch Exception _
      nil)))

(defn parse-or-default
  "Parses amount or returns default value."
  [input default]
  (or (safe-parse input) default))

(comment
  (safe-parse "100 PLN")
  ;; => #money[100.00 PLN]

  (safe-parse "invalid")
  ;; => nil

  (safe-parse "")
  ;; => nil

  (parse-or-default "abc" #money[0 PLN])
  ;; => #money[0.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Form input validation
;;; ---------------------------------------------------------------------------

(defn validate-amount
  "Validates amount from form input.
   Returns {:ok money} or {:error message}."
  [input {:keys [min-amount max-amount allowed-currencies]
          :or   {min-amount        nil
                 max-amount        nil
                 allowed-currencies nil}}]
  (let [parsed (safe-parse input)]
    (cond
      (nil? parsed)
      {:error "Invalid amount format"}

      (api/neg? parsed)
      {:error "Amount cannot be negative"}

      (api/money-is-zero? parsed)
      {:error "Amount must be greater than zero"}

      (and min-amount (api/< parsed min-amount))
      {:error (str "Minimum amount is " (api/money-format min-amount :en_US))}

      (and max-amount (api/> parsed max-amount))
      {:error (str "Maximum amount is " (api/money-format max-amount :en_US))}

      (and allowed-currencies
           (not (contains? allowed-currencies
                           (api/currency-id (api/money-currency parsed)))))
      {:error (str "Allowed currencies: " (pr-str allowed-currencies))}

      :else
      {:ok parsed})))

(comment
  ;; Valid amount
  (validate-amount "100 PLN" {:min-amount #money[10 PLN]
                              :max-amount #money[10000 PLN]
                              :allowed-currencies #{:PLN :EUR}})
  ;; => {:ok #money[100.00 PLN]}

  ;; Too small
  (validate-amount "5 PLN" {:min-amount #money[10 PLN]})
  ;; => {:error "Minimum amount is PLN10.00"}

  ;; Currency not allowed
  (validate-amount "100 USD" {:allowed-currencies #{:PLN :EUR}})
  ;; => {:error "Allowed currencies: #{:PLN :EUR}"}
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Parsing from external sources (CSV, API)
;;; ---------------------------------------------------------------------------

(defn parse-csv-amount
  "Parses amount from CSV where currency and amount are in separate columns."
  [amount-str currency-str]
  (when (and amount-str currency-str
             (not (empty? amount-str))
             (not (empty? currency-str)))
    (let [curr   (api/currency-of (keyword currency-str))
          amount (BigDecimal. (str/replace amount-str "," "."))]
      (api/money-of curr amount))))

(defn import-csv-transactions
  "Imports transactions from CSV data."
  [rows]
  (for [{:keys [id amount currency date]} rows
        :let [parsed (parse-csv-amount amount currency)]
        :when parsed]
    {:id     id
     :amount parsed
     :date   date}))

(comment
  (def csv-data
    [{:id "1" :amount "1500,50" :currency "PLN" :date "2024-01-15"}
     {:id "2" :amount "250.00"  :currency "EUR" :date "2024-01-16"}
     {:id "3" :amount ""        :currency "PLN" :date "2024-01-17"}]) ; empty

  (import-csv-transactions csv-data)
  ;; => ({:id "1" :amount #money[1500.50 PLN] :date "2024-01-15"}
  ;;     {:id "2" :amount #money[250.00 EUR] :date "2024-01-16"})
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Parsing various API response formats
;;; ---------------------------------------------------------------------------

(defn parse-api-response
  "Parses amount from various API response formats."
  [response]
  (cond
    ;; Format: {:amount "100.50", :currency "EUR"}
    (and (:amount response) (:currency response))
    (api/money-of (keyword (:currency response))
              (BigDecimal. (str (:amount response))))

    ;; Format: {:value 10050, :currency "EUR", :decimals 2}
    (and (:value response) (:currency response) (:decimals response))
    (let [divisor (Math/pow 10 (:decimals response))]
      (api/money-of (keyword (:currency response))
                (/ (:value response) divisor)))

    ;; Format: {:amount_cents 10050, :currency_code "EUR"}
    (and (:amount_cents response) (:currency_code response))
    (api/money-parse-minor (str (:amount_cents response))
                       (keyword (:currency_code response)))

    ;; Format ISO: "EUR 100.50"
    (string? response)
    (api/money-parse response)

    :else
    (throw (ex-info "Unknown API format" {:response response}))))

(comment
  ;; Stripe-like format
  (parse-api-response {:amount_cents 10050 :currency_code "EUR"})
  ;; => #money[100.50 EUR]

  ;; Standard JSON format
  (parse-api-response {:amount "250.99" :currency "PLN"})
  ;; => #money[250.99 PLN]

  ;; Integer with decimals info
  (parse-api-response {:value 12345 :currency "USD" :decimals 2})
  ;; => #money[123.45 USD]
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Converting between representations
;;; ---------------------------------------------------------------------------

(comment
  ;; From Money to components
  (let [m #money[1234.56 PLN]]
    {:amount      (api/money-amount m)           ; BigDecimal
     :currency-id (api/currency-id (api/money-currency m)) ; :PLN
     :major       (money/major m)            ; 1234
     :minor       (money/minor m)            ; 56
     :major-minor (money/major-minor m)})    ; [1234 56]

  ;; Convert to primitive types
  (money/major->long #money[1234.56 PLN])   ; => 1234
  (money/minor->int #money[1234.56 PLN])    ; => 56

  ;; To symbols (for display)
  (money/unparse #money[100.50 PLN])
  ;; => [100.50M PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: User input normalization
;;; ---------------------------------------------------------------------------

(defn normalize-input
  "Normalizes user input to standard format."
  [input]
  (-> input
      str
      str/trim
      (str/replace #"\s+" " ")      ; multiple spaces -> single
      (str/replace #"[złzl]" "PLN") ; Polish abbreviation
      (str/replace #"€" "EUR")
      (str/replace #"\$" "USD")
      (str/replace #"£" "GBP")))

(defn parse-user-input
  "Parses user input with normalization."
  [input]
  (-> input
      normalize-input
      safe-parse))

(comment
  (parse-user-input "100 zł")
  ;; => #money[100.00 PLN]

  (parse-user-input "€50")
  ;; => #money[50.00 EUR]

  (parse-user-input "$99.99")
  ;; => #money[99.99 USD]

  (parse-user-input "  1500   PLN  ")
  ;; => #money[1500.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Batch parsing with error reporting
;;; ---------------------------------------------------------------------------

(defn parse-batch
  "Parses list of strings, returning successes and errors separately."
  [inputs]
  (reduce
   (fn [acc [idx input]]
     (if-let [parsed (safe-parse input)]
       (update acc :ok conj {:index idx :input input :money parsed})
       (update acc :errors conj {:index idx :input input})))
   {:ok [] :errors []}
   (map-indexed vector inputs)))

(comment
  (parse-batch ["100 PLN" "invalid" "50 EUR" "" "25.50 USD"])
  ;; => {:ok [{:index 0 :input "100 PLN" :money #money[100.00 PLN]}
  ;;          {:index 2 :input "50 EUR" :money #money[50.00 EUR]}
  ;;          {:index 4 :input "25.50 USD" :money #money[25.50 USD]}]
  ;;     :errors [{:index 1 :input "invalid"}
  ;;              {:index 3 :input ""}]}
  )
