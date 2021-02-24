(clojure.core/ns io.randomseed.bankster.money.reader-handlers)

(defn
 lit-crypto
 [[a b]]
 (let
  [[c am] (if (number? a) [b a] [a b])]
  (io.randomseed.bankster.money/lit (keyword "crypto" (str (symbol c))) am)))