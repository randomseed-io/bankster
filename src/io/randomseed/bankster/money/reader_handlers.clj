(in-ns 'io.randomseed.bankster.money)

(defn
 lit-crypto
 [arg]
 (let
  [[a b] (if (sequential? arg) arg [arg nil])
  [c am] (if (number? a) [b a] [a b])]
  (io.randomseed.bankster.money/lit [(keyword "crypto" (str (symbol c))) am])))