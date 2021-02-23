(clojure.core/ns io.randomseed.bankster.money.reader-handlers)

(defn
 funds-crypto
 [[a b]]
 (let
  [[c am] (if (number? a) [b a] [a b])]
  (io.randomseed.bankster.money/funds (keyword "crypto" (str (symbol c))) am)))