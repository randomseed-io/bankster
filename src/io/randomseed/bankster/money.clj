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
;; Dynamic registry of currencies.
;;

(def ^:dynamic ^Registry
  *registry*
  "Registry that if set to a truthy value (not nil and not false), will be used
  instead of a global, shared registry."
  nil)

;;
;; Accountable protocol.
;;

(defprotocol Accountable
  (funds [num] [a b] [a b rounding-mode]))

;;
;; Main coercer.
;;

(defn parse
  "Internal parser."
  {:tag Money, :no-doc true}
  (^Money [amount]
   (parse ^Currency currency/*default* amount))
  (^Money [currency amount]
   (if-some [^Currency c (currency/of currency (or *registry* @R))]
     (let [s (int (scale/of ^Currency c))]
       (if (currency/auto-scaled? s)
         (Money. ^Currency c ^BigDecimal (scale/apply amount))
         (Money. ^Currency c ^BigDecimal (scale/apply amount (int s)))))
     (throw (ex-info
             (str "Cannot create money amount without a valid currency and a default currency was not set.")
             {:amount amount :currency currency}))))
  (^Money [^Currency currency amount rounding-mode]
   (if-some [^Currency c (currency/of currency (or *registry* @R))]
     (let [s (int (scale/of ^Currency c))]
       (if (currency/auto-scaled? s)
         (Money. ^Currency c ^BigDecimal (scale/apply amount))
         (Money. ^Currency c ^BigDecimal (scale/apply amount (int s) (int rounding-mode)))))
     (throw (ex-info
             (str "Cannot create money amount without a valid currency and a default currency was not set.")
             {:amount amount :currency currency})))))

(extend-protocol Accountable

  Currency

  (funds
    (^Money [a]     (parse ^Currency currency/*default* a))
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
   `(funds ~currency))
  ([currency amount]
   `(funds ~currency ~amount))
  ([currency amount rounding-mode]
   (let [rms# (scale/parse-rounding rounding-mode)]
     `(funds ~currency ~amount ~rms#))))

;;
;; Scaling and rounding.
;;

(def ^:private ^clojure.lang.PersistentArrayMap
  rounding->context
  "Mapping of BigDecimal's rounding modes to MathContext objects."
  {BigDecimal/ROUND_UP          (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_UP)))
   BigDecimal/ROUND_DOWN        (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_DOWN)))
   BigDecimal/ROUND_CEILING     (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_CEILING)))
   BigDecimal/ROUND_FLOOR       (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_FLOOR)))
   BigDecimal/ROUND_HALF_DOWN   (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_HALF_DOWN)))
   BigDecimal/ROUND_HALF_EVEN   (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_HALF_EVEN)))
   BigDecimal/ROUND_HALF_UP     (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_HALF_UP)))
   BigDecimal/ROUND_UNNECESSARY (MathContext. 0 ^RoundingMode (RoundingMode/valueOf (int BigDecimal/ROUND_UNNECESSARY)))})

(defn scale-core
  "Internal scaling function."
  {:no-doc true}
  ([m]
   (.scale ^BigDecimal (.amount ^Money m)))
  (^Money [^Money m s]
   (-> m
       (assoc :amount   ^BigDecimal (scale/apply ^BigDecimal (.amount   ^Money m) (int s)))
       (assoc :currency ^Currency   (scale/apply ^Currency   (.currency ^Money m) (int s)))))
  (^Money [^Money m s rounding-mode]
   (-> m
       (assoc :amount   ^BigDecimal (scale/apply ^BigDecimal (.amount   ^Money m) (int s) (int rounding-mode)))
       (assoc :currency ^Currency   (scale/apply ^Currency   (.currency ^Money m) (int s) (int rounding-mode))))))

(defmacro scale
  "Re-scales the given money using a scale (number of decimal places) and an optional
  rounding mode (required when downscaling). The internal scale for a currency object
  is also updated. If no scale is given, returns the current scale."
  ([money]
   `(scale-core money))
  ([money scale]
   `(scale-core ~money ~scale))
  ([money scale rounding-mode]
   (let [rms# (scale/parse-rounding rounding-mode)]
     `(scale-core money ~scale ~rms#))))

;;
;; Properties.
;;

(defn amount
  [^Money money]
  "Returns the amount of the given money"
  {:tag BigDecimal}
  (.amount ^Money money))

(defn currency
  [^Money money]
  "Returns the currency of the given money."
  {:tag Currency}
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
     (contains? (.cur-id->cur ^Registry @R)
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
   (^Money [m scale]               (scale-core ^Money m (int scale)))
   (^Money [m scale rounding-mode] (scale-core ^Money m (int scale) (int rounding-mode)))))

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
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (and (.equal (.amount ^Money a) (.amount ^Money b))
        (currency/same-ids?  (.currency ^Money a)
                             (.currency ^Money b)))))

;;
;; Operations.
;;

(defn add
  "Adds two or more amounts of money of the same currency. When called without any
  arguments, returns 0 or a Money of 0 with a default currency (if the default
  currency is set)."
  (^Money []
   (if currency/*default* (.Money ^Currency currency/*default* 0M) 0M))
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
               ^BigDecimal (.divide 1M ^BigDecimal (.amount ^Money a) (int scale/*rounding-mode*))
               ^BigDecimal (.divide 1M ^BigDecimal (.amount ^Money a))))
     (if scale/*rounding-mode*
       (.divide 1M ^BigDecimal (scale/apply a) (int scale/*rounding-mode*))
       (.divide 1M ^BigDecimal (scale/apply a)))))
  ([^Money a b]
   (if (money? b)
     (let [b ^Money b]
       (if (= (.id ^Currency (.currency ^Money a))
              (.id ^Currency (.currency ^Money b)))
         (let [^BigDecimal x (.amount ^Money a)
               ^BigDecimal y (.amount ^Money b)]
           (if (.equals y BigDecimal/ONE) a
               (if scale/*rounding-mode*
                 (.divide ^BigDecimal x ^BigDecimal y (int scale/*rounding-mode*))
                 (.divide ^BigDecimal x ^BigDecimal y))))
         (throw (ex-info
                 (str "Cannot divide by the amount of a different currency.")
                 {:dividend a :divider b}))))
     (let [^BigDecimal x (.amount ^Money a)
           ^BigDecimal y (scale/apply b)]
       (if (.equals y BigDecimal/ONE) a
           (Money. ^Currency   (.currency ^Money a)
                   ^BigDecimal (if scale/*rounding-mode*
                                 (scale/apply (.divide ^BigDecimal x ^BigDecimal y (int scale/*rounding-mode*))
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
   (if currency/*default* (.Money ^Currency currency/*default* 1M) 1M))
  ([^Money a] ^Money a)
  ([a b]
   (let [^Boolean ma (money? a)]
     (when (= ma (money? b))
       (throw (ex-info
               (str "Exactly one value must be a kind of Money.")
               {:multiplicant a :multiplier b})))
     (let [[^Money m ^BigDecimal n] (if ma [a (scale/apply b)] [b (scale/apply a)])]
       (if (.equals BigDecimal/ONE n) m
           (let [^BigDecimal x (.amount   ^Money m)
                 ^Currency   c (.currency ^Money m)]
             (if (.equals BigDecimal/ONE ^BigDecimal x)
               (Money. ^Currency c ^BigDecimal (scale/apply n (.scale ^Money m)))
               (if (or (.equals BigDecimal/ZERO ^BigDecimal n)
                       (.equals BigDecimal/ZERO ^BigDecimal x))
                 (Money. ^Currency c (scale/apply BigDecimal/ZERO (.scale ^Money m)))
                 (Money. ^Currency c
                         ^BigDecimal (if scale/*rounding-mode*
                                       (scale/apply (.multiply ^BigDecimal x ^BigDecimal n
                                                               ^MathContext (rounding->context scale/*rounding-mode*))
                                                    (.scale x))
                                       (scale/apply (.multiply ^BigDecimal x ^BigDecimal n) (.scale x)))))))))))
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
  "Sets a default currency in a lexical context of the body."
  [currency & body]
  `(binding [currency/*default* (currency/of ~currency)]
     ~@body))

;;
;; Printing.
;;

(defmethod print-method Money
  [m w]
  (let [^Currency c (.currency ^Money m)
        a (.amount ^Money m)]
    (print-simple
     (str "#" "Money["
          a " "
          (currency/short-code c)
          "]")
     w)))
