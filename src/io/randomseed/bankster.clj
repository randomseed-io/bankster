(ns

    ^{:doc    "Bankster library, records and protocols."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster)

;;
;; Currency record.
;;

(defrecord Currency
    [^clojure.lang.Keyword id       ; currency ID (e.g. :EUR or :crypto/ETH)
     ^long                 numeric  ; currency numeric ID (e.g. 978 or -1 for none)
     ^int                  scale    ; currency supported scale (decimal places, e.g. 2 or -1 for auto)
     ^clojure.lang.Keyword kind     ; currency kind (e.g. :FIAT or :DECENTRALIZED)
     ^clojure.lang.Keyword domain   ; currency domain (e.g. :ISO-4217, :ISO-4217-LEGACY or :CRYPTO)
     ^int                  weight]  ; currency weight (used when getting using currency code)
  Object
  (toString [^Currency c] (name (.id ^Currency c))))

;;
;; CurrencyHierarchies record.
;;

(defrecord CurrencyHierarchies
    [^clojure.lang.Associative domain
     ^clojure.lang.Associative kind])

;;
;; Registry record.
;;

(defrecord Registry
    [^clojure.lang.PersistentHashMap cur-id->cur        ; currency ID to currency record
     ^clojure.lang.PersistentHashMap cur-nr->cur        ; currency numeric ID to currency record
     ^clojure.lang.PersistentHashMap ctr-id->cur        ; country ID to currency record
     ^clojure.lang.PersistentHashMap cur-id->ctr-ids    ; currency ID to set of country IDs
     ^clojure.lang.PersistentHashMap cur-id->localized  ; locale ID to localized properties
     ^clojure.lang.PersistentHashMap cur-code->curs     ; currency code to currencies (weighted)
     ^clojure.lang.PersistentHashMap cur-nr->curs       ; currency numeric ID to currencies (weighted)
     ^CurrencyHierarchies hierarchies                   ; currency property hierarchies
     ^String version                                    ; version string
     ^clojure.lang.PersistentHashMap ext]               ; extra data

  Object
  (toString [^Registry r] (pr-str r)))

;;
;; Money record.
;;

(defrecord Money
    [^Currency currency   ; currency object
     ^BigDecimal amount]  ; currency amount

  Object
  (toString [^Money m]
    (str (.toPlainString ^BigDecimal (.amount ^Money m)) " " (.currency ^Money m))))
