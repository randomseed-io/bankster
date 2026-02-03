(ns io.randomseed.bankster.examples.json-serialization
  "JSON serialization examples for API integration.

   Bankster provides lossless roundtrip serialization to/from JSON,
   supporting both minimal and full representations."
  (:require [io.randomseed.bankster.money              :as money]
            [io.randomseed.bankster.api              :as api]
            [io.randomseed.bankster.currency         :as currency]
            [io.randomseed.bankster.serializers.json :as json]
            [io.randomseed.bankster.serializers.edn  :as edn]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Basic JSON serialization
;;; ---------------------------------------------------------------------------

(comment
  ;; Money to JSON map (minimal - default)
  (json/money->json-map #money[99.99 USD])
  ;; => {:amount "99.99", :currency "USD"}

  ;; Money to JSON map (full - with metadata)
  (json/money->json-map #money[99.99 USD] :full)
  ;; => {:amount "99.99", :currency "USD", :scale 2, :numeric 840}

  ;; Money to JSON string
  (json/money->json-string #money[150.00 EUR])
  ;; => "{\"amount\":\"150.00\",\"currency\":\"EUR\"}"
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: JSON deserialization
;;; ---------------------------------------------------------------------------

(comment
  ;; From JSON text
  (json/json-text->money "{\"amount\": \"150.00\", \"currency\": \"EUR\"}")
  ;; => #money[150.00 EUR]

  ;; Handles various amount formats
  (json/json-text->money "{\"amount\": 100, \"currency\": \"PLN\"}")
  ;; => #money[100.00 PLN]

  (json/json-text->money "{\"amount\": \"99.999\", \"currency\": \"PLN\"}")
  ;; => #money[99.999 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Currency serialization
;;; ---------------------------------------------------------------------------

(comment
  ;; Currency to JSON map
  (json/currency->json-map (api/currency-of :EUR))
  ;; => {:id "EUR", :numeric 978, :scale 2, :kind "iso/fiat", :domain "ISO-4217"}

  ;; Currency to JSON string
  (json/currency->json-string (api/currency-of :PLN))
  ;; => "{\"id\":\"PLN\",\"numeric\":985,\"scale\":2,...}"

  ;; From JSON string
  (json/json-string->currency "{\"id\":\"EUR\"}")
  ;; => #currency EUR
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Cheshire integration
;;; ---------------------------------------------------------------------------

(comment
  ;; Register codecs for automatic encoding/decoding with Cheshire
  (json/register-cheshire-codecs!)

  ;; After registration, Cheshire will automatically handle Money/Currency
  (require '[cheshire.core :as cheshire])

  ;; Encode
  (cheshire/generate-string {:total #money[100 PLN]
                             :items [{:name "Widget" :price #money[25 PLN]}]})
  ;; => "{\"total\":{\"amount\":\"100.00\",\"currency\":\"PLN\"},\"items\":[...]}"

  ;; Decode (requires custom decoder setup)
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: API request/response handling
;;; ---------------------------------------------------------------------------

(defn serialize-for-api
  "Serializes data structure containing Money for API response."
  [data]
  (clojure.walk/postwalk
   (fn [x]
     (cond
       (api/money? x)    (json/money->json-map x)
       (api/currency? x) (json/currency->json-map x)
       :else x))
   data))

(defn deserialize-from-api
  "Deserializes API request containing money fields."
  [data money-fields]
  (reduce
   (fn [d field]
     (if-let [money-data (get-in d field)]
       (assoc-in d field
                 (api/money-of (keyword (:currency money-data))
                           (BigDecimal. (str (:amount money-data)))))
       d))
   data
   money-fields))

(comment
  ;; Serialize order for API response
  (serialize-for-api
   {:order-id "12345"
    :total    #money[299.99 PLN]
    :items    [{:name "Product A" :price #money[199.99 PLN]}
               {:name "Product B" :price #money[100.00 PLN]}]})
  ;; => {:order-id "12345"
  ;;     :total {:amount "299.99" :currency "PLN"}
  ;;     :items [{:name "Product A" :price {:amount "199.99" :currency "PLN"}}
  ;;             {:name "Product B" :price {:amount "100.00" :currency "PLN"}}]}

  ;; Deserialize incoming request
  (deserialize-from-api
   {:amount {:amount "50.00" :currency "EUR"}
    :description "Payment"}
   [[:amount]])
  ;; => {:amount #money[50.00 EUR], :description "Payment"}
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Batch serialization for export
;;; ---------------------------------------------------------------------------

(defn export-transactions
  "Exports transactions to JSON-ready format."
  [transactions]
  (for [{:keys [id amount date description]} transactions]
    {:id          id
     :amount      (:amount (json/money->json-map amount))
     :currency    (:currency (json/money->json-map amount))
     :date        (str date)
     :description description}))

(comment
  (def sample-transactions
    [{:id 1 :amount #money[100.00 PLN] :date "2024-01-15" :description "Payment A"}
     {:id 2 :amount #money[250.50 EUR] :date "2024-01-16" :description "Payment B"}
     {:id 3 :amount #money[75.00 USD]  :date "2024-01-17" :description "Payment C"}])

  (export-transactions sample-transactions)
  ;; => ({:id 1, :amount "100.00", :currency "PLN", :date "2024-01-15", ...}
  ;;     {:id 2, :amount "250.50", :currency "EUR", :date "2024-01-16", ...}
  ;;     {:id 3, :amount "75.00", :currency "USD", :date "2024-01-17", ...})
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: EDN serialization (alternative to JSON)
;;; ---------------------------------------------------------------------------

(comment
  ;; Money to EDN map
  (edn/money->edn-map #money[100.50 PLN])
  ;; => {:amount 100.50M, :currency :PLN}

  ;; Money to EDN string (preserves tagged literal)
  (edn/money->edn-string #money[100.50 PLN])
  ;; => "#money[100.50M PLN]"

  ;; From EDN string
  (edn/edn-string->money "#money[100.50M PLN]")
  ;; => #money[100.50 PLN]

  ;; Currency EDN
  (edn/currency->edn-map (api/currency-of :EUR))
  ;; => {:id :EUR, :numeric 978, :scale 2, :kind :iso/fiat, :domain :ISO-4217}
  )

;;; ---------------------------------------------------------------------------
;;; Example 8: Roundtrip verification
;;; ---------------------------------------------------------------------------

(defn verify-json-roundtrip
  "Verifies that JSON serialization is lossless."
  [money-value]
  (let [json-str  (json/money->json-string money-value)
        restored  (json/json-text->money json-str)
        equal?    (api/= money-value restored)]
    {:original  money-value
     :json      json-str
     :restored  restored
     :lossless? equal?}))

(comment
  (verify-json-roundtrip #money[123.456789 PLN])
  ;; => {:original #money[123.456789 PLN]
  ;;     :json "{\"amount\":\"123.456789\",\"currency\":\"PLN\"}"
  ;;     :restored #money[123.456789 PLN]
  ;;     :lossless? true}

  ;; Even works with high-precision crypto
  (verify-json-roundtrip #money/crypto[0.000000000000000001 ETH])
  ;; => {:lossless? true}
  )

;;; ---------------------------------------------------------------------------
;;; Example 9: Custom JSON schema for different APIs
;;; ---------------------------------------------------------------------------

(defn to-stripe-format
  "Converts Money to Stripe API format (amount in cents)."
  [money-value]
  (let [curr (api/money-currency money-value)
        scale (api/currency-scale curr)]
    {:amount   (long (* (api/money-amount money-value)
                        (Math/pow 10 scale)))
     :currency (clojure.string/lower-case
                (name (api/currency-id curr)))}))

(defn from-stripe-format
  "Converts Stripe API format to Money."
  [{:keys [amount currency]}]
  (let [curr  (api/currency-of (keyword (clojure.string/upper-case currency)))
        scale (api/currency-scale curr)]
    (api/money-of curr (/ amount (Math/pow 10 scale)))))

(comment
  ;; To Stripe format
  (to-stripe-format #money[99.99 USD])
  ;; => {:amount 9999, :currency "usd"}

  (to-stripe-format #money[1234.56 PLN])
  ;; => {:amount 123456, :currency "pln"}

  ;; From Stripe format
  (from-stripe-format {:amount 9999 :currency "usd"})
  ;; => #money[99.99 USD]
  )

;;; ---------------------------------------------------------------------------
;;; Example 10: Webhook payload handling
;;; ---------------------------------------------------------------------------

(defn parse-payment-webhook
  "Parses payment webhook from various providers."
  [provider payload]
  (case provider
    :stripe
    {:amount   (from-stripe-format (:data payload))
     :event    (:type payload)
     :metadata (:metadata payload)}

    :paypal
    {:amount   (api/money-of (keyword (:currency_code payload))
                         (BigDecimal. (:value payload)))
     :event    (:event_type payload)
     :metadata (:custom_id payload)}

    :default
    {:amount   (json/json-text->money (json/money->json-string payload))
     :raw      payload}))

(comment
  ;; Stripe webhook
  (parse-payment-webhook :stripe
                         {:type "payment_intent.succeeded"
                          :data {:amount 5000 :currency "pln"}
                          :metadata {:order-id "123"}})
  ;; => {:amount #money[50.00 PLN]
  ;;     :event "payment_intent.succeeded"
  ;;     :metadata {:order-id "123"}}
  )
