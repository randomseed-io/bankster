(ns

    ^{:doc    "Bankster library, records and protocols."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster)

;;
;; Currency record.
;;

(defrecord Currency
    [^clojure.lang.Keyword id                          ; currency ID (e.g. :EUR or :crypto/ETH)
     ^long                 nr                          ; currency numeric ID (e.g. 978 or 0)
     ^long                 dp                          ; currency supported decimal places (e.g. 2 or -1)
     ^clojure.lang.Keyword ns                          ; currency domain (e.g. :ISO-4217 or :CRYPTO)
     ^clojure.lang.Keyword kind])                      ; currency kind (e.g. :FIAT or :DECENTRALIZED)

;;
;; Registry record.
;;

(defrecord Registry
    [^clojure.lang.PersistentHashMap cur-id->cur        ; currency ID to currency record
     ^clojure.lang.PersistentHashMap cur-nr->cur        ; currency numeric ID to currency record
     ^clojure.lang.PersistentHashMap ctr-id->cur        ; country ID to currency record
     ^clojure.lang.PersistentHashMap cur-id->ctr-ids])  ; currency ID to set of country IDs
