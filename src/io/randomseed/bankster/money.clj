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

(defn strip
  "Strips trailing zeros from the amount. The internal scale for a currency object
  is also updated.

  Use with caution since it can make money object no longer compliant with a scale of
  the currency."
  {:tag Money :added "1.0.0"}
  [^Money a]
  (Money. (.currency ^Money a)
          (.stripTrailingZeros ^BigDecimal (.amount ^Money a))))

;;
;; Properties.
;;

(defn amount
  "Returns the amount of the given money."
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
   (^Money [m scale] (.setScale ^BigDecimal (.amount ^Money m) (int scale)))
   (^Money [m scale ^RoundingMode rounding-mode] (.setScale ^BigDecimal (.amount^Money m) (int scale) ^RoundingMode rounding-mode))))

;;
;; Comparator.
;;

(defn compare-amounts
  "Compares two monetary amounts of the same currency, regardless of their
  scales. Returns -1 if the second one is less than, 0 if equal to, and 1 if it is
  greater than the first."
  {:added "1.0.0" :tag 'int}
  (^Boolean [^Money a] (int 0))
  (^Boolean [^Money a ^Money b]
   (when-not (currency/same-ids? (.currency ^Money a) (.currency ^Money b))
     (throw (ex-info "Cannot compare different currencies."
                     {:currency-1 a :currency-2 b})))
   (int (.compareTo ^BigDecimal (.amount ^Money a) ^BigDecimal (.amount ^Money b)))))

;;
;; Predicates.
;;

(defmacro money?
  "Returns true if the given value is a kind of Money."
  {:added "1.0.0"}
  [a]
  `(instance? Money ~a))

(defn ^Boolean eq?
  "Return true if the money amounts and their currencies are equal. Note that
  currencies with different scales are considered different."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (and (.equals (.amount ^Money a) (.amount ^Money b))
        (currency/same-ids? (.currency ^Money a)
                            (.currency ^Money b))))
  (^Boolean [^Money a ^Money b & more]
   (if (eq? a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq? b (first more)))
     false)))

(defn ^Boolean eq-am?
  "Return true if the money amounts and their currencies are equal regardless of their
  scales."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (if-not (currency/same-ids? (.currency ^Money a) (.currency ^Money b))
     false
     (let [^BigDecimal am-a (.amount ^Money a)
           ^BigDecimal am-b (.amount ^Money b)
           sa (int (.scale am-a))
           sb (int (.scale am-b))]
       (if (= sa sb)
         (.equals ^BigDecimal a ^BigDecimal b)
         (if (< sa sb)
           (.equals ^BigDecimal (scale/apply a sb) ^BigDecimal b)
           (.equals ^BigDecimal a ^BigDecimal (scale/apply b sa)))))))
  (^Boolean [^Money a ^Money b & more]
   (if (eq-am? a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq-am? b (first more)))
     false)))

(defn ^Boolean ne?
  "Returns true if the money amounts or their currencies are different. Note that
  currencies with different scales are considered different."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] false)
  (^Boolean [^Money a ^Money b]
   (not (eq? ^Money a ^Money b)))
  (^Boolean [^Money a ^Money b & more]
   (not (apply eq? ^Money a ^Money b more))))

(defn gt?
  "Returns non-nil if monetary amounts are in monotonically decreasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (> (compare a b) 0))
  (^Boolean [^Money a ^Money b & more]
   (if (gt? a b)
     (if (next more)
       (recur b (first more) (next more))
       (gt? b (first more)))
     false)))

(defn ge?
  "Returns non-nil if monetary amounts are in monotonically non-increasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (>= (compare a b) 0))
  (^Boolean [^Money a ^Money b & more]
   (if (ge? a b)
     (if (next more)
       (recur b (first more) (next more))
       (ge? b (first more)))
     false)))

(defn lt?
  "Returns non-nil if monetary amounts are in monotonically increasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (< (compare a b) 0))
  (^Boolean [^Money a ^Money b & more]
   (if (lt? a b)
     (if (next more)
       (recur b (first more) (next more))
       (lt? b (first more)))
     false)))

(defn le?
  "Returns non-nil if monetary amounts are in monotonically non-decreasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (<= (compare a b) 0))
  (^Boolean [^Money a ^Money b & more]
   (if (le? a b)
     (if (next more)
       (recur b (first more) (next more))
       (le? b (first more)))
     false)))

;;
;; Operations.
;;

(defn scale
  "Re-scales the given money using a scale (number of decimal places) and an optional
  rounding mode (required when downscaling). The internal scale for a currency object
  is also updated. If no scale is given, returns the current scale.

  Use with caution since it can make money object no longer compliant with a scale of
  the currency."
  {:added "1.0.0"}
  ([^Money money]
   (int (.scale ^BigDecimal (.amount ^Money money))))
  (^Money [^Money money scale]
   (Money. ^Currency   (.currency ^Money money)
           ^BigDecimal (if scale/*rounding-mode*
                         (.setScale ^BigDecimal (.amount ^Money money)
                                    (int scale)
                                    ^RoundingMode scale/*rounding-mode*)
                         (.setScale ^BigDecimal (.amount ^Money money)
                                    (int scale)))))
  (^Money [^Money money scale ^RoundingMode rounding-mode]
   (Money. ^Currency   (.currency ^Money money)
           ^BigDecimal (.setScale ^BigDecimal (.amount ^Money money)
                                  (int scale)
                                  ^RoundingMode rounding-mode))))

(defn add
  "Adds two or more amounts of money of the same currency. When called without any
  arguments, returns 0 or a Money of 0 with a default currency (if the default
  currency is set)."
  {:tag Money :added "1.0.0"}
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
                       "Cannot add amounts of two different currencies."
                       {:addend-1 a :addend-2 b})))))))
  (^Money [^Money a ^Money b & more]
   (reduce add (add ^Money a ^Money b) more)))

(defn sub
  "Subtracts two or more amounts of money of the same currency."
  {:tag Money :added "1.0.0"}
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
                   "Cannot subtract amounts of two different currencies."
                   {:minuend a :subtrahend b}))))))
  (^Money [^Money a ^Money b & more]
   (reduce sub (sub ^Money a ^Money b) more)))

(defn div
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
  {:tag Money :added "1.0.0"}
  (^Money [a]
   (if (money? a)
     (Money. ^Currency (.currency ^Money a)
             (if scale/*rounding-mode*
               ^BigDecimal (.divide 1M ^BigDecimal (.amount ^Money a) ^RoundingMode scale/*rounding-mode*)
               ^BigDecimal (.divide 1M ^BigDecimal (.amount ^Money a))))
     (if scale/*rounding-mode*
       (.divide 1M ^BigDecimal (scale/apply a) ^RoundingMode scale/*rounding-mode*)
       (.divide 1M ^BigDecimal (scale/apply a)))))
  (^Money [^Money a b]
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
                 "Cannot divide by the amount of a different currency."
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
   (reduce div (div ^Money a b) more)))

(defn mul
  "Multiplies two or more amounts of money of the same currency or a number. If the
  first value is a kind of Money and the second is a number, the result will be a
  Money. For a single value it returns a division of 1 by that number, even if it is
  a kind of Money. For more than 2 arguments it repeatedly divides them as described.

  When there are two amounts of the same currency, scale may vary depending on the
  result and rounding is applied only when there is no exact decimal representation.

  When there is a division of an amount by the regular number, the result is
  re-scaled to match the scale of a currency. If the scaling requires rounding
  enclosing the expression within with-rounding is required."
  {:tag Money :added "1.0.0"}
  (^Money []
   (if currency/*default* (Money. ^Currency currency/*default* 1M) 1))
  ([^Money a] ^Money a)
  (^Money [a b]
   (let [ma (money? a)
         mb (money? b)]
     (when (and ma mb)
       (throw (ex-info "At least one value must be a regular number."
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
   (reduce mul (mul a b) more)))

(defn xxx
  ""
  {:tag Money :added "1.0.0"}
  [^Money a] )



;; (defn rem [a b])

;; (defn inc)
;; (defn dec)
;; (defn divide-to-integral)
;; (defn divide-and-rem)
;; (defn pow)
;; (defn abs)
;; (defn neg)
;; (defn pos)
;; (defn signum)
;; (defn round)
;; (defn strip)
;; (defn min)
;; (defn max)

(defn major
  "Returns the major part of the given amount."
  {:tag BigDecimal :added "1.0.0"}
  [^Money a]
  (scale/apply (.amount ^Money a) 0 scale/ROUND_DOWN))

(defn major->long
  "Returns the major part of the given amount as a long number."
  {:tag 'long :added "1.0.0"}
  [^Money a]
  (.longValueExact ^BigDecimal (.amount ^Money a)))

(defn major->int
  "Returns the major part of the given amount as an integer number."
  {:tag 'int :added "1.0.0"}
  [^Money a]
  (.intValueExact ^BigDecimal (.amount ^Money a)))

(defn minor
  "Returns the minor part of the given amount."
  {:tag BigDecimal :added "1.0.0"}
  [^Money a]
  (let [sc (scale/of (.currency ^Money a))]
    (-> ^BigDecimal (.amount ^Money a)
        ^BigDecimal (scale/apply sc scale/ROUND_DOWN)
        ^BigDecimal (.remainder BigDecimal/ONE)
        ^BigDecimal (.movePointRight sc))))

(defn minor->long
  "Returns the minor part of the given amount as a long number."
  {:tag 'long :added "1.0.0"}
  [^Money a]
  (.longValueExact (minor ^Money a)))

(defn minor->int
  "Returns the minor part of the given amount as an integer number."
  {:tag 'int :added "1.0.0"}
  [^Money a]
  (.intValueExact (minor ^Money a)))

(defn add-major
  "Increases major amount by the given number. If the number has decimal parts, they
  will be truncated."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (if (= BigDecimal/ZERO b) a
      (Money. ^Currency   (.currency ^Money a)
              ^BigDecimal (.add ^BigDecimal (.amount ^Money a)
                                ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN)))))

(defn sub-major
  "Decreases major amount by the given number. If the number has decimal parts, they
  will be truncated."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (if (= BigDecimal/ZERO b) a
      (Money. ^Currency   (.currency ^Money a)
              ^BigDecimal (.subtract ^BigDecimal (.amount ^Money a)
                                     ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN)))))

(defn inc-major
  "Increases major amount by 1."
  {:tag Money :added "1.0.0"}
  [^Money a]
  (add-major a BigDecimal/ONE))

(defn dec-major
  "Decreases major amount by 1."
  {:tag Money :added "1.0.0"}
  [^Money a]
  (sub-major a BigDecimal/ONE))

(defn add-minor
  "Increases minor amount by the given number. If the number has decimal parts, they
  will be truncated."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (if (= BigDecimal/ZERO b) a
      (let [cur-a (.currency ^Money a)
            sc    (scale/of (.currency ^Money a))]
        (Money.
         ^Currency cur-a
         ^BigDecimal (.add
                      ^BigDecimal (.amount ^Money a)
                      ^BigDecimal (.movePointLeft
                                   ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN) sc))))))

(defn sub-minor
  "Decreases minor amount by the given number. If the number has decimal parts, they
  will be truncated. If the major component comes from a money object, its currency
  must match the given money. This check is performed to prevent mistakes; if you
  need to subtract minor parts of money with different currencies, use (minor x) on
  the second argument."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (if (= BigDecimal/ZERO b) a
      (let [cur-a (.currency ^Money a)
            sc    (scale/of (.currency ^Money a))]
        (Money.
         ^Currency cur-a
         ^BigDecimal (.subtract
                      ^BigDecimal (.amount ^Money a)
                      ^BigDecimal (.movePointLeft
                                   ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN) sc))))))

(defn inc-minor
  "Increases minor amount by 1."
  {:tag Money :added "1.0.0"}
  [^Money a]
  (add-minor a BigDecimal/ONE))

(defn dec-minor
  "Decreases minor amount by 1."
  {:tag Money :added "1.0.0"}
  [^Money a]
  (sub-minor a BigDecimal/ONE))

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
  (let [[a b r] (if (sequential? arg) arg [arg nil nil])
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
