(ns io.randomseed.bankster.money

  ^{:doc    "Bankster library, money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.scale    :as         scale]
            [io.randomseed.bankster.currency :as      currency]
            [io.randomseed.bankster.registry :as      registry]
            [io.randomseed.bankster.util.map :as           map]
            [io.randomseed.bankster.util.fs  :as            fs]
            [io.randomseed.bankster.util     :refer       :all])

  (:import  [io.randomseed.bankster Currency Registry Money]
            [java.math MathContext RoundingMode]))

;;
;; Global registry of currencies.
;;

(def ^:private R (registry/global))

;;
;; Accountable protocol.
;;

(defprotocol Accountable
  "This protocol is used to create monetary values."
  (funds [num] [a b] [a b rounding-mode]))

;;
;; Main coercer.
;;

(defn parse
  "Internal parser."
  {:tag Money, :no-doc true}
  (^Money [amount]
   (if (sequential? amount)
     (apply parse ^Currency (first amount) (rest amount))
     (if (number? amount)
       (parse ^Currency currency/*default* amount)
       (currency/of amount))))
  (^Money [currency amount]
   (if-some [^Currency c (currency/unit currency)]
     (let [s (int (scale/of ^Currency c))]
       (if (currency/val-auto-scaled? s)
         (Money. ^Currency c ^BigDecimal (scale/apply amount))
         (Money. ^Currency c ^BigDecimal (scale/apply amount (int s)))))
     (throw (ex-info
             (str "Cannot create money amount without a valid currency and a default currency was not set.")
             {:amount amount :currency currency}))))
  (^Money [^Currency currency amount ^RoundingMode rounding-mode]
   (if-some [^Currency c (currency/unit currency)]
     (let [s (int (scale/of ^Currency c))]
       (if (currency/val-auto-scaled? s)
         (Money. ^Currency c ^BigDecimal (scale/apply amount))
         (Money. ^Currency c ^BigDecimal (scale/apply amount (int s) ^RoundingMode rounding-mode))))
     (throw (ex-info
             (str "Cannot create money amount without a valid currency and a default currency was not set.")
             {:amount amount :currency currency})))))

(extend-protocol Accountable

  Currency

  (funds
    (^Money [a]     (currency/of a))
    (^Money [c b]   (parse ^Currency c b))
    (^Money [c b r] (parse ^Currency c b r)))

  clojure.lang.Symbol

  (funds
    (^Money [a]     (currency/of a))
    (^Money [c b]   (parse ^Currency c b))
    (^Money [c b r] (parse ^Currency c b r)))

  clojure.lang.Keyword

  (funds
    (^Money [a]     (currency/of a))
    (^Money [c b]   (parse ^Currency c b))
    (^Money [c b r] (parse ^Currency c b r)))

  String

  (funds
    (^Money [a]     (currency/of a))
    (^Money [c b]   (parse ^Currency c b))
    (^Money [c b r] (parse ^Currency c b r)))

  Number

  (funds
    (^Money [a]               (parse ^Currency currency/*default* a))
    (^Money [a ^Currency c]   (parse ^Currency c a))
    (^Money [a ^Currency c r] (parse ^Currency c a r)))

  nil

  (funds
    ([c]     nil)
    ([a b]   nil)
    ([a c r] (parse a c r)))

  clojure.lang.Sequential

  (funds
    (^Money [v]     (apply funds v))
    (^Money [v r]   (funds (first v) (second v) r))
    (^Money [v b r] (funds (first v) b r)))

  Object

  (funds
    (^Money [a]     (parse ^Currency currency/*default* a))
    (^Money [a c]   (parse a c))
    (^Money [a c r] (parse a c r))))

(defmacro of
  "Returns the amount of money as a Money object consisting of a currency and a
  value. Currency can be a currency object and for registered currencies: a keyword,
  a symbol or a string (e.g. EUR, :EUR, \"PLN\" or crypto/ETH), or even a number (for
  ISO-compliant currencies).

  The given amount can be any numeric value or a string that can be converted to
  java.math.BigDecimal.

  When a number must be downscaled to fulfill the number of decimal places for a
  currency, rounding mode must be given, which may be a symbol, a keyword or a string
  of the following:

  CEILING     - rounds towards positive infinity.
  DOWN        - rounds towards zero.
  FLOOR       - rounds towards negative infinity.
  HALF_DOWN   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  HALF_EVEN   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  HALF_UP     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  UP          – rounds away from zero
  UNNECESSARY - asserts that the requested operation has an exact result, hence no rounding is necessary."
  ([currency]
   (let [cur# (currency/parse-currency-symbol currency)]
     `(funds ~cur#)))
  ([a b]
   (let [[currency amount] (if (number? a) [b a] [a b])
         cur# (currency/parse-currency-symbol currency)]
     `(funds ~cur# ~amount)))
  ([a b rounding-mode]
   (let [[currency amount] (if (number? a) [b a] [a b])
         rms# (scale/parse-rounding rounding-mode)
         cur# (currency/parse-currency-symbol currency)]
     `(funds ~cur# ~amount ~rms#))))

;;
;; Scaling and rounding.
;;

(def ^:private ^clojure.lang.PersistentArrayMap
  rounding->context
  "Mapping of rounding modes to MathContext objects."
  {RoundingMode/UP          (MathContext. (int 0) RoundingMode/UP)
   RoundingMode/DOWN        (MathContext. (int 0) RoundingMode/DOWN)
   RoundingMode/CEILING     (MathContext. (int 0) RoundingMode/CEILING)
   RoundingMode/FLOOR       (MathContext. (int 0) RoundingMode/FLOOR)
   RoundingMode/HALF_DOWN   (MathContext. (int 0) RoundingMode/HALF_DOWN)
   RoundingMode/HALF_EVEN   (MathContext. (int 0) RoundingMode/HALF_EVEN)
   RoundingMode/HALF_UP     (MathContext. (int 0) RoundingMode/HALF_UP)
   RoundingMode/UNNECESSARY (MathContext. (int 0) RoundingMode/UNNECESSARY)})

(defn scale-core
  "Internal scaling function."
  {:no-doc true :tag 'int}
  ([m]
   (.scale ^BigDecimal (.amount ^Money m)))
  (^Money [^Money m s]
   (-> m
       (assoc :amount   ^BigDecimal (scale/apply ^BigDecimal (.amount   ^Money m) (int s)))
       (assoc :currency ^Currency   (scale/apply ^Currency   (.currency ^Money m) (int s)))))
  (^Money [^Money m s ^RoundingMode rounding-mode]
   (-> m
       (assoc :amount   ^BigDecimal (scale/apply ^BigDecimal (.amount   ^Money m) (int s) ^RoundingMode rounding-mode))
       (assoc :currency ^Currency   (scale/apply ^Currency   (.currency ^Money m) (int s) ^RoundingMode rounding-mode)))))

(defmacro scale
  "Re-scales the given money using a scale (number of decimal places) and an optional
  rounding mode (required when downscaling). The internal scale for a currency object
  is also updated. If no scale is given, returns the current scale."
  ([money]
   `(scale-core ~money))
  ([money scale]
   `(scale-core ~money ~scale))
  ([money scale rounding-mode]
   (let [rms# (scale/parse-rounding rounding-mode)]
     `(scale-core money ~scale ~rms#))))

;;
;; Properties.
;;

(defn amount
  "Returns the amount of the given money"
  {:tag BigDecimal :added "1.0.0"}
  [^Money money]
  (.amount ^Money money))

(defn currency
  "Returns the currency of the given money."
  {:tag Currency, :added "1.0.0"}
  [^Money money]
  (.currency ^Money money))

;;
;; Monetary implementation.
;;

(extend-protocol currency/Monetary

  Money

  (of
    (^Currency [money] (.currency ^Money money))
    (^Currency [money, ^Registry registry] money))

  (id
    (^clojure.lang.Keyword [money]
     (.id ^Currency (.currency ^Money money)))
    (^clojure.lang.Keyword [money, ^Registry registry]
     (.id ^Currency (.currency ^Money money))))

  (defined?
    (^Boolean [money]
     (contains? (.cur-id->cur (registry/get))
                (.id ^Currency (.currency ^Money money))))
    (^Boolean [money, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry)
                (.id ^Currency (.currency ^Money money)))))

  (same-ids?
    (^Boolean [a b]
     (= (.id ^Currency (.currency ^Money a)) (currency/id b)))
    (^Boolean [a b ^Registry registry]
     (= (.id ^Currency (.currency ^Money a)) (currency/id b registry)))))

;;
;; Scalable implementation.
;;

(extend-protocol scale/Scalable

  Money

  (^Boolean scalable? [m] true)
  (^Boolean applied?  [m] true)

  (of [m] (.scale ^BigDecimal (.amount ^Money m)))

  (^Money apply
   (^Money [m] ^Money m)
   (^Money [m scale] (scale-core ^Money m (int scale)))
   (^Money [m scale ^RoundingMode rounding-mode] (scale-core ^Money m (int scale) ^RoundingMode rounding-mode))))

;;
;; Predicates.
;;

(defn ^Boolean money?
  "Returns true if the given value is a kind of Money."
  [a]
  (instance? Money a))

(defn ^Boolean equal?
  "Return true if the money amounts and their currencies are equal."
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (and (.equals (.amount ^Money a) (.amount ^Money b))
        (currency/same-ids? (.currency ^Money a)
                            (.currency ^Money b))))
  (^Boolean [^Money a ^Money b & more]
   (if (equal? a b)
     (if (next more)
       (recur b (first more) (next more))
       (equal? b (first more)))
     false)))

(defn ^Boolean different?
  "Returns true if the money amounts or their currencies are different."
  (^Boolean [^Money a] false)
  (^Boolean [^Money a ^Money b]
   (not (equal? ^Money a ^Money b)))
  (^Boolean [^Money a ^Money b & more]
   (not (apply equal? ^Money a ^Money b more))))

;;
;; Operations.
;;

(defn add
  "Adds two or more amounts of money of the same currency. When called without any
  arguments, returns 0 or a Money of 0 with a default currency (if the default
  currency is set)."
  (^Money []
   (if currency/*default* (Money. ^Currency currency/*default* 0M) 0M))
  (^Money [^Money a] a)
  (^Money [^Money a ^Money b]
   (if (nil? a) b
       (if (nil? b) a
           (let [^Currency cur-a (.currency ^Money a)]
             (if (= (.id ^Currency cur-a)
                    (.id ^Currency (.currency ^Money b)))
               (let [^BigDecimal x (.amount ^Money a)
                     ^BigDecimal y (.amount ^Money b)
                     ^BigDecimal r (.add  ^BigDecimal x ^BigDecimal y)]
                 (if (= ^int (.scale ^BigDecimal x)
                        ^int (.scale ^BigDecimal y))
                   (Money. ^Currency cur-a ^BigDecimal r)
                   (Money. ^Currency (assoc cur-a :sc ^int (.scale ^BigDecimal r)) ^BigDecimal r)))
               (throw (ex-info
                       (str "Cannot add amounts of two different currencies.")
                       {:addend-1 a :addend-2 b})))))))
  (^Money [^Money a ^Money b & more]
   (reduce add (add ^Money a ^Money b) more)))

(defn subtract
  "Subtracts two or more amounts of money of the same currency."
  (^Money [^Money a]
   (Money. ^Currency   (.currency ^Money a)
           ^BigDecimal (.subtract 0M ^BigDecimal (.amount ^Money a))))
  (^Money [^Money a ^Money b]
   (if (nil? b) a
       (let [^Currency cur-a (.currency ^Money a)]
         (if (= (.id ^Currency cur-a)
                (.id ^Currency (.currency ^Money b)))
           (let [^BigDecimal x (.amount ^Money a)
                 ^BigDecimal y (.amount ^Money b)
                 ^BigDecimal r (.subtract ^BigDecimal x ^BigDecimal y)]
             (if (= ^int (.scale x)
                    ^int (.scale y))
               (Money. ^Currency cur-a ^BigDecimal r)
               (Money. ^Currency (assoc cur-a :sc ^int (.scale ^BigDecimal r)) ^BigDecimal r)))
           (throw (ex-info
                   (str "Cannot subtract amounts of two different currencies.")
                   {:minuend a :subtrahend b}))))))
  (^Money [^Money a ^Money b & more]
   (reduce subtract (subtract ^Money a ^Money b) more)))

(defn divide
  "Divides two or more amounts of money of the same currency or a number. If the two
  values are a kind of Money, the result will be of BigDecimal number. If the first
  value is a kind of Money and the second is a number, the result will be a
  Money. For a single value it returns a division of 1 by that number, even if it is
  a kind of Money. For more than 2 arguments it repeatedly divides them as
  described.

  When there are two amounts of the same currency, scale may vary depending on the
  result and rounding is applied only when there is no exact decimal representation.

  When there is a division of an amount by the regular number, the result is
  re-scaled to match the scale of a currency. If the scaling requires rounding
  enclosing the expression within with-rounding is required."
  ([a]
   (if (money? a)
     (Money. ^Currency   (.currency ^Money a)
             (if scale/*rounding-mode*
               ^BigDecimal (.divide 1M ^BigDecimal (.amount ^Money a) ^RoundingMode scale/*rounding-mode*)
               ^BigDecimal (.divide 1M ^BigDecimal (.amount ^Money a))))
     (if scale/*rounding-mode*
       (.divide 1M ^BigDecimal (scale/apply a) ^RoundingMode scale/*rounding-mode*)
       (.divide 1M ^BigDecimal (scale/apply a)))))
  ([^Money a b]
   (if (money? b)
     (let [b ^Money b]
       (if (= (.id ^Currency (.currency ^Money a))
              (.id ^Currency (.currency ^Money b)))
         (let [^BigDecimal x (.amount   ^Money a)
               ^BigDecimal y (.amount   ^Money b)]
           (if (.equals y BigDecimal/ONE) a
               (if scale/*rounding-mode*
                 (.divide ^BigDecimal x ^BigDecimal y ^RoundingMode scale/*rounding-mode*)
                 (.divide ^BigDecimal x ^BigDecimal y))))
         (throw (ex-info
                 (str "Cannot divide by the amount of a different currency.")
                 {:dividend a :divider b}))))
     (let [^BigDecimal x (.amount ^Money a)
           ^BigDecimal y (scale/apply b)]
       (if (.equals y BigDecimal/ONE) a
           (Money. ^Currency   (.currency ^Money a)
                   ^BigDecimal (if scale/*rounding-mode*
                                 (scale/apply (.divide ^BigDecimal x ^BigDecimal y ^RoundingMode scale/*rounding-mode*)
                                              (.scale x))
                                 (scale/apply (.divide ^BigDecimal x ^BigDecimal y)
                                              (.scale x))))))))
  (^Money [^Money a b & more]
   (reduce divide (divide ^Money a b) more)))

(defn multiply
  "Multiplies two or more amounts of money of the same currency or a number. If the two
  values are a kind of Money, . If the first
  value is a kind of Money and the second is a number, the result will be a
  Money. For a single value it returns a division of 1 by that number, even if it is
  a kind of Money. For more than 2 arguments it repeatedly divides them as
  described.

  When there are two amounts of the same currency, scale may vary depending on the
  result and rounding is applied only when there is no exact decimal representation.

  When there is a division of an amount by the regular number, the result is
  re-scaled to match the scale of a currency. If the scaling requires rounding
  enclosing the expression within with-rounding is required."
  ([]
   (if currency/*default* (Money. ^Currency currency/*default* 1M) 1))
  ([^Money a] ^Money a)
  ([a b]
   (let [^Boolean ma (money? a)
         ^Boolean mb (money? b)]
     (when (and ma mb)
       (throw (ex-info (str "At least one value must be a regular number.")
                       {:multiplicant a :multiplier b})))
     (if-not (or ma mb)
       (clojure.core/* a b)
       (let [[^Money m ^BigDecimal n] (if ma [a (scale/apply b)] [b (scale/apply a)])]
         (if (.equals BigDecimal/ONE n) m
             (let [^BigDecimal x (.amount   ^Money m)
                   ^Currency   c (.currency ^Money m)
                   s (.scale ^BigDecimal x)]
               (if (.equals BigDecimal/ONE ^BigDecimal x)
                 (Money. ^Currency c ^BigDecimal (scale/apply n (int s)))
                 (if (or (.equals BigDecimal/ZERO ^BigDecimal n)
                         (.equals BigDecimal/ZERO ^BigDecimal x))
                   (Money. ^Currency c (scale/apply BigDecimal/ZERO (int s)))
                   (Money. ^Currency c
                           ^BigDecimal (if scale/*rounding-mode*
                                         (scale/apply (.multiply ^BigDecimal x ^BigDecimal n
                                                                 ^MathContext (rounding->context scale/*rounding-mode*))
                                                      (int s))
                                         (scale/apply (.multiply ^BigDecimal x ^BigDecimal n) (int s))))))))))))
  (^Money [a b & more]
   (reduce multiply (multiply a b) more)))

;;
;; Contextual macros.
;;

(defmacro with-rounding
  "Alias for scale/with-rounding."
  [rounding-mode & body]
  (list* 'io.randomseed.bankster.scale/with-rounding rounding-mode body))

(defmacro with-currency
  "Sets a default currency in a lexical context of the body. Has the same effect as
  currency/with."
  [currency & body]
  (let [cur# (if (symbol? currency) (keyword currency) currency)]
    `(binding [currency/*default* (currency/unit ~cur#)]
       ~@body)))

;;
;; Tagged literals.
;;

(defn lit
  "Tagged literal handler."
  ([arg]
   (let [[c amount r] (if (sequential? arg) arg [arg nil nil])]
     (if (or (nil? c) (and (sequential? c) (nil? (seq c))))
       '(quote nil)
       (if (nil? amount)
         (funds c)
         (if (nil? r)
           (funds c amount)
           (funds c amount (scale/parse-rounding r))))))))

(defn defliteral
  "For the given currency identifier or a currency object it creates a tagged literal
  in a form of #m/CURRENCY where the CURRENCY is a short currency code. As a side
  effect it creates a function of name io.randomseed.bankster.money/of-CURRENCY that
  will handle the literal.

  The literals will be bound to *data-readers* in a local thread."
  [c]
  (when-some [^Currency c (currency/unit c)]
    (let [cush (currency/short-code c)
          name (str "of-" cush)
          nsnm "io.randomseed.bankster.money"
          mkfn (fn ^Money [amount] (if (nil? amount) '(quote nil) (of c amount)))
          varn (intern (symbol nsnm) (symbol name) mkfn)
          snam (symbol "m" cush)]
      (alter-var-root #'clojure.core/*data-readers*
                      (fn [m _] (assoc m snam varn))
                      (when (thread-bound? #'clojure.core/*data-readers*)
                        (set! clojure.core/*data-readers*
                              (assoc clojure.core/*data-readers* snam varn)))))))

(defn ns-lit
  {:private true :added "1.0.0"}
  [kw arg]
  (let
      [[a b r] (if (sequential? arg) arg [arg nil nil])
       [c am]  (if (and (some? b) (number? a)) [b a] [a b])
       c       (if (number? c) c (keyword kw (str (symbol c))))]
    (lit [c am r])))

(load "money/reader_handlers")

;;
;; Printing.
;;

(defmethod print-method Money
  [m w]
  (let [^Currency c (.currency ^Money m)
        ^String   n (namespace (.id ^Currency c))
        a (.amount ^Money m)]
    (print-simple
     (str "#money" (when n (str "/" n))
          "[" a " " (currency/short-code c) "]")
     w)))
