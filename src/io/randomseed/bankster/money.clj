(ns io.randomseed.bankster.money

  ^{:doc    "Bankster library, money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [format])

  (:require [trptr.java-wrapper.locale       :as             l]
            [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.scale    :as         scale]
            [io.randomseed.bankster.currency :as      currency]
            [io.randomseed.bankster.registry :as      registry]
            [io.randomseed.bankster.util.map :as           map]
            [io.randomseed.bankster.util.fs  :as            fs]
            [io.randomseed.bankster.util     :refer       :all])

  (:import  [io.randomseed.bankster Currency Registry Money]
            [io.randomseed.bankster.currency FormatterInstance]
            [java.math MathContext RoundingMode]
            [java.text NumberFormat DecimalFormat DecimalFormatSymbols]
            [java.util Locale]))

;;
;; Accountable protocol.
;;

(defprotocol ^{:added "1.0.0"} Accountable
  "This protocol is used to express monetary values using various numeric types and
  currency representations."

  (value
    [num] [currency num] [currency num rounding-mode]
    "Creates new Money object for the given value which will become an amount. If the
  currency is not given it will try to use the default one, taken from the
  `*default-currency*` dynamic variable. Optional rounding-mode should be a rounding
  mode used when the conversion to a scaled monetary amount requires rounding.

  In its unary form, when the argument is not numeric, it will try to get the
  currency object (identified by a string, a symbol or a keyword) from a default,
  global registry of currencies.

  For simple money creation the following macros may be convenient way to go: of,
  of-major, of-minor."))

;;
;; Main coercer.
;;

(defn parse
  "Internal parser."
  {:tag Money, :no-doc true}
  (^Money [amount]
   (when (some? amount)
     (if (sequential? amount)
       (apply parse ^Currency (first amount) (rest amount))
       (if (number? amount)
         (parse ^Currency currency/*default* amount)
         (currency/unit amount)))))
  (^Money [currency amount]
   (when (some? amount)
     (if-some [^Currency c (currency/unit currency)]
       (let [s (int (scale/of ^Currency c))]
         (if (currency/val-auto-scaled? s)
           (Money. ^Currency c ^BigDecimal (scale/apply amount))
           (Money. ^Currency c ^BigDecimal (scale/apply amount (int s)))))
       (throw (ex-info
               (str "Cannot create money amount without a valid currency and a default currency was not set.")
               {:amount amount :currency currency})))))
  (^Money [^Currency currency amount ^RoundingMode rounding-mode]
   (when (some? amount)
     (if-some [^Currency c (currency/unit currency)]
       (let [s (int (scale/of ^Currency c))]
         (if (currency/val-auto-scaled? s)
           (Money. ^Currency c ^BigDecimal (scale/apply amount))
           (Money. ^Currency c ^BigDecimal (scale/apply amount (int s) ^RoundingMode rounding-mode))))
       (throw (ex-info
               (str "Cannot create money amount without a valid currency and a default currency was not set.")
               {:amount amount :currency currency}))))))

(extend-protocol Accountable

  Currency

  (value
    (^Money [currency]                    (currency/unit currency))
    (^Money [currency amount]             (parse ^Currency currency amount))
    (^Money [currency amount rounding]    (parse ^Currency currency amount rounding)))

  clojure.lang.Symbol

  (value
    (^Money [currency-id]                 (currency/unit currency-id))
    (^Money [currency-id amount]          (parse currency-id amount))
    (^Money [currency-id amount rounding] (parse currency-id amount rounding)))

  clojure.lang.Keyword

  (value
    (^Money [currency-id]                 (currency/unit currency-id))
    (^Money [currency-id amount]          (parse currency-id amount))
    (^Money [currency-id amount rounding] (parse currency-id amount rounding)))

  String

  (value
    (^Money [currency-id]                 (currency/unit currency-id))
    (^Money [currency-id amount]          (parse currency-id amount))
    (^Money [currency-id amount rounding] (parse currency-id amount rounding)))

  Number

  (value
    (^Money [amount]                      (parse currency/*default* amount))
    (^Money [amount currency]             (parse currency amount))
    (^Money [amount currency rounding]    (parse currency amount rounding)))

  nil

  (value
    ([c]     nil)
    ([a b]   nil)
    ([a c r] (parse a c r)))

  clojure.lang.Sequential

  (value
    (^Money [v]     (apply value v))
    (^Money [v r]   (value (first v) (second v) r))
    (^Money [v b r] (value (first v) b r)))

  Object

  (value
    (^Money [a]     (parse ^Currency currency/*default* a))
    (^Money [a c]   (parse a c))
    (^Money [a c r] (parse a c r))))

(defn major-value
  "Creates new Money object for the given value which will become a major part of the
  amount. If the given number has fractional part it will be truncated. If the
  currency is not given it should try to use the default one, taken from the
  `*default-currency*` dynamic variable. Optional rounding-mode should be a rounding
  mode used when the conversion to a scaled monetary amount requires rounding.

  In its unary form, when the argument is not numeric, it will try to get a currency
  object (identified by a string, a symbol or a keyword) from a default, global
  registry of currencies.

  For simple money creation the following macros may be convenient way to go: of,
  of-major, of-minor."
  (^Money [c]     (value c))
  (^Money [c b]   (value c (let [sc (scale/of c)]
                             (if (currency/val-auto-scaled? sc)
                               (scale/integer b)
                               (scale/integer b sc)))))
  (^Money [c b r] (value c
                         (let [sc (scale/of c)]
                           (if (currency/val-auto-scaled? sc)
                             (scale/integer b)
                             (scale/integer b sc r))))))

(defn minor-value
  "Creates new Money object for the given value which will become a minor part of the
  amount. If the given number has fractional part it will be truncated. If the
  currency is not given it should try to use the default one, taken from the
  `*default-currency*` dynamic variable. Optional rounding-mode should be a rounding
  mode used when the conversion to a scaled monetary amount requires rounding.

  In its unary form, when the argument is not numeric, it will try to get a currency
  object (identified by a string, a symbol or a keyword) from a default, global
  registry of currencies.

  For simple money creation the following macros may be convenient way to go: of,
  of-major, of-minor."
  (^Money [c]     (value c))
  (^Money [c b]   (value c (let [sc (scale/of c)]
                             (if (currency/val-auto-scaled? sc)
                               (let [^BigDecimal b (scale/integer b)]
                                 (.movePointLeft ^BigDecimal b (.scale ^BigDecimal b)))
                               (.movePointLeft ^BigDecimal (scale/integer b sc) sc)))))
  (^Money [c b r] (value c (let [sc (scale/of c)]
                             (if (currency/val-auto-scaled? sc)
                               (let [^BigDecimal b (scale/integer b)]
                                 (.movePointLeft ^BigDecimal b (.scale ^BigDecimal b)))
                               (.movePointLeft ^BigDecimal (scale/integer b sc r) sc))))))

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
  UNNECESSARY - asserts that the requested operation has an exact result, hence no rounding is necessary.

  To create a monetary object using function, call value."
  ([currency]
   (let [cur# (currency/parse-currency-symbol currency)]
     `(value ~cur#)))
  ([a b]
   (let [[currency amount] (if (number? a) [b a] [a b])
         cur# (currency/parse-currency-symbol currency)]
     `(value ~cur# ~amount)))
  ([a b rounding-mode]
   (let [[currency amount] (if (number? a) [b a] [a b])
         rms# (scale/parse-rounding rounding-mode)
         cur# (currency/parse-currency-symbol currency)]
     `(value ~cur# ~amount ~rms#))))

(defmacro of-major
  "Like io.randomseed.money/of but expects an amount of major part."
  ([currency]
   (let [cur# (currency/parse-currency-symbol currency)]
     `(major-value ~cur#)))
  ([a b]
   (let [[currency amount] (if (number? a) [b a] [a b])
         cur# (currency/parse-currency-symbol currency)]
     `(major-value ~cur# ~amount)))
  ([a b rounding-mode]
   (let [[currency amount] (if (number? a) [b a] [a b])
         rms# (scale/parse-rounding rounding-mode)
         cur# (currency/parse-currency-symbol currency)]
     `(major-value ~cur# ~amount ~rms#))))

(defmacro of-minor
  "Like io.randomseed.money/of but expects an amount of minor part."
  ([currency]
   (let [cur# (currency/parse-currency-symbol currency)]
     `(minor-value ~cur#)))
  ([a b]
   (let [[currency amount] (if (number? a) [b a] [a b])
         cur# (currency/parse-currency-symbol currency)]
     `(minor-value ~cur# ~amount)))
  ([a b rounding-mode]
   (let [[currency amount] (if (number? a) [b a] [a b])
         rms# (scale/parse-rounding rounding-mode)
         cur# (currency/parse-currency-symbol currency)]
     `(minor-value ~cur# ~amount ~rms#))))

;;
;; Scaling and rounding.
;;

(def ^{:private true :tag clojure.lang.PersistentArrayMap :added "1.0.0"}
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
  "Strips trailing zeros from the amount of money. The internal scale for a currency
  object is also updated.

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

(defn stripped-amount
  "Returns the amount of the given money with trailing zeros removed."
  {:tag BigDecimal :added "1.0.0"}
  [^Money money]
  (.stripTrailingZeros ^BigDecimal (.amount ^Money money)))

;;
;; Monetary implementation.
;;

(extend-protocol currency/Monetary

  Money

  (of
    (^Currency [money] (.currency ^Money money))
    (^Currency [money ^Registry registry] (.currency ^Money money)))

  (unit
    (^Currency [money] (.currency ^Money money))
    (^Currency [money ^Registry registry] (.currency ^Money money)))

  (id
    (^clojure.lang.Keyword [money]
     (.id ^Currency (.currency ^Money money)))
    (^clojure.lang.Keyword [money ^Registry registry]
     (.id ^Currency (.currency ^Money money))))

  (defined?
    (^Boolean [money]
     (contains? (registry/currency-id->currency)
                (.id ^Currency (.currency ^Money money))))
    (^Boolean [money ^Registry registry]
     (contains? (registry/currency-id->currency registry)
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
   (^Money [m]
    ^Money m)
   (^Money [m scale]
    (if (currency/val-auto-scaled? scale)
      (Money. ^Currency   (.currency ^Money m)
              ^BigDecimal (.amount   ^Money m))
      (if-some [rm scale/*rounding-mode*]
        (Money. ^Currency   (.currency ^Money m)
                ^BigDecimal (.setScale ^BigDecimal (.amount ^Money m)
                                       (int scale)
                                       ^RoundingMode rm))
        (Money. ^Currency   (.currency ^Money m)
                ^BigDecimal (.setScale ^BigDecimal (.amount ^Money m) (int scale))))))
   (^Money [m scale ^RoundingMode rounding-mode]
    (if (currency/val-auto-scaled? scale)
      (Money. ^Currency   (.currency ^Money m)
              ^BigDecimal (.amount   ^Money m))
      (Money. ^Currency   (.currency ^Money m)
              ^BigDecimal (.setScale ^BigDecimal (.amount^Money m)
                                     (int scale)
                                     ^RoundingMode rounding-mode)))))

  (^BigDecimal amount
   (^BigDecimal [num]         (.amount ^Money num))
   (^BigDecimal [num scale]   (.amount ^Money (apply num scale)))
   (^BigDecimal [num scale r] (.amount ^Money (apply num scale r)))))

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
   (when-not (= (.id ^Currency (.currency ^Money a))
                (.id ^Currency (.currency ^Money b)))
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (int (.compareTo ^BigDecimal (.amount ^Money a) ^BigDecimal (.amount ^Money b)))))

;;
;; Predicates.
;;

(defn money?
  "Returns true if the given value is a kind of Money."
  {:tag Boolean :added "1.0.0"}
  [a]
  (instance? Money a))

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
  "Return true if the money amounts and their currencies are equal, regardless of their
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
   (pos-int? (compare-amounts a b)))
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
   (>= (compare-amounts a b) 0))
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
   (neg-int? (compare-amounts a b)))
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
   (<= (compare-amounts a b) 0))
  (^Boolean [^Money a ^Money b & more]
   (if (le? a b)
     (if (next more)
       (recur b (first more) (next more))
       (le? b (first more)))
     false)))

(defn is-zero?
  "Returns true if the given monetary amount is a positive number."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (zero? (.compareTo 0M ^BigDecimal (.amount ^Money a))))

(defn is-neg?
  "Returns true if the given monetary amount is a negative number."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (pos-int? (.compareTo 0M ^BigDecimal (.amount ^Money a))))

(defn is-pos?
  "Returns true if the given monetary amount is a positive number."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (neg-int? (.compareTo 0M ^BigDecimal (.amount ^Money a))))

(defn is-neg-or-zero?
  "Returns true if the given monetary amount is a negative number or zero."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (>= (.compareTo 0M ^BigDecimal (.amount ^Money a)) 0))

(defn is-pos-or-zero?
  "Returns true if the given monetary amount is a positive number or zero."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (<= (.compareTo 0M ^BigDecimal (.amount ^Money a)) 0))

;;
;; Operations.
;;

(defn scale
  "Re-scales the given money using a scale (number of decimal places) and an optional
  rounding mode (required when downscaling). The internal scale for a currency object
  is also updated. If no scale is given, returns the current scale.

  Auto-scaled monetary values are returned unchanged.

  Use with caution since it can make money object no longer compliant with a scale of
  the currency."
  {:added "1.0.0"}
  ([^Money money]
   (int (.scale ^BigDecimal (.amount ^Money money))))
  (^Money [^Money money scale]
   (if (currency/val-auto-scaled? scale) money
       (Money. ^Currency   (.currency ^Money money)
               (if-some [rm scale/*rounding-mode*]
                 ^BigDecimal (.setScale ^BigDecimal (.amount ^Money money)
                                        (int scale)
                                        ^RoundingMode rm)
                 ^BigDecimal (.setScale ^BigDecimal (.amount ^Money money)
                                        (int scale))))))
  (^Money [^Money money scale ^RoundingMode rounding-mode]
   (if (currency/val-auto-scaled? scale) money
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (.setScale ^BigDecimal (.amount ^Money money)
                                      (int scale)
                                      ^RoundingMode rounding-mode)))))

(defn add
  "Adds two or more amounts of money of the same currency. When called without any
  arguments, returns 0."
  {:tag Money :added "1.0.0"}
  (^Money [] 0M)
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
                   (Money. ^Currency (assoc cur-a :scale ^int (.scale ^BigDecimal r)) ^BigDecimal r)))
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
               (Money. ^Currency (assoc cur-a :scale ^int (.scale ^BigDecimal r)) ^BigDecimal r)))
           (throw (ex-info
                   "Cannot subtract amounts of two different currencies."
                   {:minuend a :subtrahend b}))))))
  (^Money [^Money a ^Money b & more]
   (reduce sub (sub ^Money a ^Money b) more)))

(defn mul-core
  "Internal multiplier."
  {:private true :added "1.0.0"}
  ([a b ^BigDecimal ma ^BigDecimal mb]
   (if ma
     (if mb
       (throw (ex-info "At least one value must be a regular number."
                       {:multiplicant a :multiplier b}))
       (.multiply ^BigDecimal ma ^BigDecimal (scale/apply b)))
     (if mb
       (.multiply ^BigDecimal (scale/apply a) ^BigDecimal mb)
       (.multiply ^BigDecimal (scale/apply a) ^BigDecimal (scale/apply b))))))

(declare mul)

(defn mul-scaled
  "Multiplies two or more amounts of money of the same currency or a number. If the
  first value is a kind of Money and the second is a number, the result will be a
  Money. For a single value it returns a division of 1 by that number, even if it is
  a kind of Money. For more than 2 arguments it repeatedly divides them as described.

  When there is a multiplication of an amount by the regular number, the result is
  re-scaled to match the scale of a currency. If the scaling requires rounding
  enclosing the expression within io.randomseed.bankster.scale/with-rounding is
  required (the macro is aliased under the same name in this namespace)."
  {:added "1.0.0"}
  ([] 1M)
  ([a] a)
  ([a b] (mul a b))
  ([x y & more]
   (let [mx  (when (instance? Money x) (.amount ^Money x))
         my  (when (instance? Money y) (.amount ^Money y))
         fir (mul-core x y mx my)
         mon (volatile! (if mx x (when my y)))
         fir (if @mon (scale/apply fir (.scale (if my ^BigDecimal my ^BigDecimal mx))) fir)
         fun (fn [a b]
               (if @mon
                 (if (instance? Money b)
                   (throw (ex-info "Only one value can be a kind of Money."
                                   {:multiplicant a :multiplier b}))
                   (scale/apply (.multiply ^BigDecimal a ^BigDecimal (scale/apply b))
                                (.scale ^BigDecimal a)))
                 (if (instance? Money b)
                   (let [^BigDecimal ab (.amount ^Money b)]
                     (do (scale/apply (.multiply ^BigDecimal a ^BigDecimal ab)
                                      (.scale ^BigDecimal ab))
                         (vreset! mon ^Money b)))
                   (.multiply ^BigDecimal a ^BigDecimal (scale/apply b)))))
         res (reduce fun fir more)]
     (if-some [m @mon]
       (Money. ^Currency   (.currency ^Money m)
               ^BigDecimal (scale/apply res (.scale ^BigDecimal (.amount ^Money m))))
       res))))

(defn mul
  "Multiplies two or more amounts of money of the same currency or a number. If the
  first value is a kind of Money and the second is a number, the result will be a
  Money. For a single value it returns a division of 1 by that number, even if it is
  a kind of Money. For more than 2 arguments it repeatedly divides them as described.

  When there is a multiplication of an amount by the regular number, the result is
  re-scaled to match the scale of a currency. If the scaling requires rounding
  enclosing the expression within io.randomseed.bankster.scale/with-rounding is
  required (the macro is aliased under the same name in this namespace), unless
  with-rescaling, described below, is in place.

  Rounding and re-scaling are performed after all calculations are done. To change
  that either use mul-scaled or set the dynamic variable
  `io.randomseed.bankster.scale/*each*` to a truthy value. You can also use a macro
  called `io.randomseed.bankster.scale/with-rescaling` (aliased in this namespace
  under the same name)."
  {:added "1.0.0"}
  ([] 1M)
  ([a] a)
  ([a b]
   (let [am (when (instance? Money a) (.amount ^Money a))
         bm (when (instance? Money b) (.amount ^Money b))
         mu (mul-core a b am bm)]
     (if am (Money.     (.currency ^Money a) (scale/apply mu (.scale ^BigDecimal am)))
         (if bm (Money. (.currency ^Money b) (scale/apply mu (.scale ^BigDecimal bm)))
             mu))))
  ([x y & more]
   (if scale/*each*
     (mul-scaled x y more)
     (let [mx  (when (instance? Money x) (.amount ^Money x))
           my  (when (instance? Money y) (.amount ^Money y))
           fir (mul-core x y mx my)
           mon (volatile! (if mx x (when my y)))
           fun (fn [a b]
                 (if @mon
                   (if (instance? Money b)
                     (throw (ex-info "Only one value can be a kind of Money."
                                     {:multiplicant a :multiplier b}))
                     (.multiply ^BigDecimal a ^BigDecimal (scale/apply b)))
                   (if (instance? Money b)
                     (.multiply ^BigDecimal a ^BigDecimal (.amount ^Money (vreset! mon ^Money b)))
                     (.multiply ^BigDecimal a ^BigDecimal (scale/apply b)))))
           res (reduce fun fir more)]
       (if-some [m @mon]
         (Money. ^Currency   (.currency ^Money m)
                 ^BigDecimal (scale/apply res (.scale ^BigDecimal (.amount ^Money m))))
         res)))))

(defn div-core
  "Internal divider."
  {:private true :added "1.0.0"}
  ([a b]
   (if (instance? Money b)
     (throw (ex-info "Cannot divide a regular number by the monetary amount."
                     {:dividend a :divider b}))
     (.divide ^BigDecimal a ^BigDecimal (scale/apply b))))
  ([a b bm]
   (if bm
     (.divide ^BigDecimal a ^BigDecimal bm)
     (.divide ^BigDecimal a ^BigDecimal (scale/apply b))))
  ([a b ^BigDecimal ma ^BigDecimal mb]
   (if ma
     (if mb
       (if (= (.id ^Currency (.currency ^Money a))
              (.id ^Currency (.currency ^Money b)))
         (.divide ^BigDecimal ma ^BigDecimal mb)
         (throw (ex-info "Cannot divide by the amount of a different currency."
                         {:dividend a :divider b})))
       (.divide ^BigDecimal ma ^BigDecimal (scale/apply b)))
     (if mb
       (throw (ex-info "Cannot divide a regular number by the monetary amount."
                       {:dividend a :divider b}))
       (.divide ^BigDecimal (scale/apply a) ^BigDecimal (scale/apply b))))))

(declare div)

(defn div-scaled
  "Divides two or more amounts of money of the same currency or a number. If the two
  values are a kind of Money, the result will be of BigDecimal number. If the earlier
  value is a kind of Money and the second is a number, the result will be a Money. If
  the number is first and the monetary amount is the second argument, an exception
  will be thrown. The result will always be either a kind of Money or a BigDecimal
  number.

  For a single value it returns a division of 1 by that number.

  For more than 2 arguments it repeatedly divides their values as described and
  re-scales each result to match the scale of a currency. If the previous
  calculations produce a regular number, all consequent dividers must be regular
  numbers. Practically, if the division begins with a monetary amount, only one
  monetary amount is permitted later (which will cause the function to switch to the
  regular numbers calculation since the units will cancel themselves).

  Scaling is applied after each operation but only when the currently accumulated
  result is a kind of Money. If the scaling requires rounding then enclosing the
  expression within with-rounding is required."
  {:added "1.0.0"}
  ([a]   (div 1M a))
  ([a b] (div a b))
  ([a b & more]
   (if-not (instance? Money a)
     (reduce div-core (div-core (scale/apply a) b) more)
     (let [ma  (.amount ^Money a)
           mb  (when (instance? Money b) (.amount ^Money b))
           fir (scale/apply (div-core ma b mb) (.scale (if mb ^BigDecimal mb ^BigDecimal ma)))]
       (loop [x fir, more more]
         (if-some [y (first more)]
           (if (instance? Money y)
             (if (= (.id ^Currency (.currency ^Money a))
                    (.id ^Currency (.currency ^Money y)))
               (reduce div-core (scale/apply (.divide ^BigDecimal x ^BigDecimal (.amount ^Money y))
                                             (.scale ^BigDecimal ma)) (rest more))
               (throw (ex-info "Cannot divide by the amount of a different currency."
                               {:dividend a :divider b})))
             (recur (scale/apply (.divide ^BigDecimal x ^BigDecimal (scale/apply y))
                                 (.scale ^BigDecimal ma)) (rest more)))
           (if mb x
               (Money. ^Currency  (.currency ^Money a)
                       ^BigDecimal (scale/apply ^BigDecimal x (.scale ^BigDecimal ma))))))))))

(defn div
  "Divides two or more amounts of money of the same currency or a number. If the two
  values are a kind of Money, the result will be of BigDecimal number. If the earlier
  value is a kind of Money and the second is a number, the result will be a Money. If
  the number is first and the monetary amount is the second argument, an exception
  will be thrown. The result will always be either a kind of Money or a BigDecimal
  number.

  For a single value it returns a division of 1 by that number.

  For more than 2 arguments it repeatedly divides their values as described. If the
  previous calculations produce a regular number, all consequent dividers must be
  regular numbers. Practically, if the division begins with a monetary amount, only
  one monetary amount at some point later is permitted (which will cause the function
  to switch to the regular numbers calculation since the units will cancel themselves).

  Scaling is applied only when the result is a kind of Money, unless the dynamic
  variable `io.randomseed.bankster.scale/*each*` is set to a truthy value. This can
  also be achieved by enclosing the expression within
  `io.randomseed.bankster.scale/with-rescaling` (aliased as
  `io.randomseed.bankster.money/with-rescaling`) macro combining the switch and
  setting the scale. If the scaling requires rounding then enclosing the expression
  within with-rounding is required."
  {:added "1.0.0"}
  ([a] (div 1M a))
  ([a b]
   (let [am (when (instance? Money a) (.amount ^Money a))
         bm (when (instance? Money b) (.amount ^Money b))
         mu (div-core a b am bm)]
     (if am
       (if bm mu
           (Money. (.currency ^Money a) (scale/apply mu (.scale ^BigDecimal am))))
       mu)))
  ([a b & more]
   (if scale/*each*
     (apply div-scaled a b more)
     (if-not (instance? Money a)
       (reduce div-core (div-core (scale/apply a) b) more)
       (let [ma  (.amount ^Money a)
             mb  (when (instance? Money b) (.amount ^Money b))
             fir (div-core ma b mb)]
         (loop [x fir, more more]
           (if-some [y (first more)]
             (if (instance? Money y)
               (if (= (.id ^Currency (.currency ^Money a))
                      (.id ^Currency (.currency ^Money y)))
                 (reduce div-core (.divide ^BigDecimal x ^BigDecimal (.amount ^Money y)) (rest more))
                 (throw (ex-info "Cannot divide by the amount of a different currency."
                                 {:dividend a :divider b})))
               (recur (.divide ^BigDecimal x ^BigDecimal (scale/apply y)) (rest more)))
             (if mb x
                 (Money. ^Currency  (.currency ^Money a)
                         ^BigDecimal (scale/apply ^BigDecimal x (int (.scale ^BigDecimal ma))))))))))))

(defn min-amount
  "Returns the least of the monetary amounts."
  {:tag Money :added "1.0.0"}
  ([^Money a] a)
  ([^Money a ^Money b]
   (when-not (= (.id ^Currency (.currency ^Money a))
                (.id ^Currency (.currency ^Money b)))
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (if (lt? a b) a b))
  ([^Money a ^Money b & more]
   (reduce min-amount (min-amount a b) more)))

(defn max-amount
  "Returns the greatest of the monetary amounts."
  {:tag Money :added "1.0.0"}
  ([^Money a] a)
  ([^Money a ^Money b]
   (when-not (= (.id ^Currency (.currency ^Money a))
                (.id ^Currency (.currency ^Money b)))
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (if (gt? a b) a b))
  ([^Money a ^Money b & more]
   (reduce max-amount (max-amount a b) more)))

(defn convert
  "Converts a monetary amount from one currency to another using the given
  multiplier (expressing a conversion rate).

  When no rounding mode is given and rounding is required during scaling to another
  currency then the value of dynamic variable
  `io.randomseed.bankster.scale/*rounding-mode*` will be used."
  {:tag Money :added "1.0.0"}
  (^Money [^Money a currency multiplier]
   (let [^Currency currency (currency/unit currency)
         sc (int (scale/of ^Currency currency))]
     (Money. ^Currency   currency
             ^BigDecimal (if (currency/val-auto-scaled? sc)
                           (.multiply ^BigDecimal (.amount ^Money a)
                                      ^BigDecimal (scale/apply multiplier))
                           (scale/apply (.multiply ^BigDecimal (.amount ^Money a)
                                                   ^BigDecimal (scale/apply multiplier))
                                        (int sc))))))
  ([^Money a currency multiplier rounding-mode]
   (let [^Currency currency (currency/unit currency)
         sc (int (scale/of ^Currency currency))]
     (Money. ^Currency   currency
             ^BigDecimal (if (currency/val-auto-scaled? sc)
                           (.multiply ^BigDecimal (.amount ^Money a)
                                      ^BigDecimal (scale/apply multiplier))
                           (scale/apply (.multiply ^BigDecimal (.amount ^Money a)
                                                   ^BigDecimal (scale/apply multiplier))
                                        (int sc)
                                        rounding-mode))))))

(defn div-rem
  "Returns the remainder of dividing an mount of money by the given number."
  {:tag BigDecimal :added "1.0.0"}
  [^Money a b]
  (Money. ^Currency   (.currency ^Money a)
          ^BigDecimal (.remainder ^BigDecimal (.amount ^Money a) (scale/apply b))))

(defn neg
  "Returns the negated amount of the given money. For negative amount it will reverse
  their sign. Same as (sub x)."
  {:added "1.0.0"}
  [a]
  (sub a))

(defn pos
  "Returns the positive (absolute) amount of the given money."
  {:tag BigDecimal :added "1.0.0"}
  [^Money a]
  (Money. ^Currency   (.currency ^Money a)
          ^BigDecimal (.abs ^BigDecimal (.amount ^Money a))))

(defn round
  "Rounds the amount of money using the given scale and rounding mode. Returns money
  with rounded amount preserving the original scale. If the rounding mode is not
  given the one from scale/*rounding-mode* is used."
  {:tag Money :added "1.0.0"}
  (^Money [^Money money scale]
   (round money scale (or scale/*rounding-mode* scale/ROUND_UNNECESSARY)))
  (^Money [^Money money scale ^RoundingMode rounding-mode]
   (let [^BigDecimal am (.amount ^Money money)
         sc (int (.scale am))]
     (if (>= scale sc)
       money
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (.setScale
                            ^BigDecimal (.setScale ^BigDecimal am
                                                   (int scale)
                                                   ^RoundingMode rounding-mode)
                            (int sc)))))))

(defn major
  "Returns the major part of the given amount."
  {:tag BigDecimal :added "1.0.0"}
  ([^Money a]
   (.setScale ^BigDecimal (.amount ^Money a) 0 scale/ROUND_DOWN)))

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
  (let [^BigDecimal am (.amount ^Money a)
        sc (.scale am)]
    (-> ^BigDecimal am
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

(defn major-minor
  "Returns a vector with major and minor parts of the given monetary amount."
  {:tag clojure.lang.IPersistentVector :added "1.0.0"}
  [^Money a]
  [(major a) (minor a)])

(defn major-minor->int
  "Returns a vector with major and minor parts of the given monetary amount represented
  as integer numbers."
  {:tag clojure.lang.IPersistentVector :added "1.0.0"}
  [^Money a]
  [(major->int a) (minor->int a)])

(defn major-minor->long
  "Returns a vector with major and minor parts of the given monetary amount represented
  as long numbers."
  {:tag clojure.lang.IPersistentVector :added "1.0.0"}
  [^Money a]
  [(major->long a) (minor->long a)])

(defn add-major
  "Increases major amount by the given number. If the number is also expressed as money
  and it has decimal parts, they will be truncated."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (Money. ^Currency   (.currency ^Money a)
          ^BigDecimal (.add ^BigDecimal (.amount ^Money a)
                            ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN))))

(defn sub-major
  "Decreases major amount by the given number. If the number is also expressed as money
  and it has decimal parts, they will be truncated."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (Money. ^Currency   (.currency ^Money a)
          ^BigDecimal (.subtract ^BigDecimal (.amount ^Money a)
                                 ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN))))

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
  "Increases minor amount by the given number. If the number is also expressed as money
  and it has decimal parts, they will be truncated.."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (let [^BigDecimal am (.amount ^Money a)]
    (Money.
     ^Currency   (.currency ^Money a)
     ^BigDecimal (.add
                  ^BigDecimal am
                  ^BigDecimal (.movePointLeft
                               ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN)
                               (.scale am))))))

(defn sub-minor
  "Decreases minor amount by the given number. If the number is also expressed as money
  and it has decimal parts, they will be truncated. If the major component comes from
  a money object, its currency must match the given money. This check is performed to
  prevent mistakes; if you need to subtract minor parts of money with different
  currencies, use (minor x) on the second argument."
  {:tag Money :added "1.0.0"}
  [^Money a b]
  (let [^BigDecimal am (.amount ^Money a)]
    (Money.
     ^Currency   (.currency ^Money a)
     ^BigDecimal (.subtract
                  ^BigDecimal am
                  ^BigDecimal (.movePointLeft
                               ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN)
                               (.scale am))))))

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
  "Alias for io.randomseed.bankster.scale/with-rounding."
  {:added "1.0.0"}
  [rounding-mode & body]
  (list* 'io.randomseed.bankster.scale/with-rounding rounding-mode body))

(defmacro with-rescaling
  "Alias for io.randomseed.bankster.scale/with-rescaling."
  {:added "1.0.0"}
  [rounding-mode & body]
  (list* 'io.randomseed.bankster.scale/with-rescaling rounding-mode body))

(defmacro with-currency
  "Sets a default currency in a lexical context of the body. Has the same effect as
  io.randomseed.bankster.currency/with."
  {:added "1.0.0"}
  [currency & body]
  (let [cur# (if (symbol? currency) (keyword currency) currency)]
    `(binding [currency/*default* (currency/unit ~cur#)]
       ~@body)))

;;
;; Tagged literals.
;;

(defn lit
  "Tagged literal handler."
  {:added "1.0.0" :no-doc true}
  ([arg]
   (let [[c amount r] (if (sequential? arg) arg [arg nil nil])]
     (if (or (nil? c) (and (sequential? c) (nil? (seq c))))
       '(quote nil)
       (if (nil? amount)
         (value c)
         (if (nil? r)
           (value c amount)
           (value c amount (scale/parse-rounding r))))))))

(defn defliteral
  "For the given currency identifier or a currency object it creates a tagged literal
  in a form of #m/CURRENCY where the CURRENCY is a short currency code. As a side
  effect it creates a function of name io.randomseed.bankster.money/of-CURRENCY that
  will handle the literal.

  The literals will be bound to *data-readers* in a local thread."
  {:added "1.0.0"}
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
;; Formatting.
;;

(defn format
  "Formats the given amount of money as a string according to localization rules. If
  the locale argument is not given then the default is used.

  It is advised to express locale using a keyword when huge amount of operations is
  expected.

  If there are 3 arguments passed the third one should be a map of options
  customizing the formatting. It can have the following keys:

  - :rounding-mode   - RoundingMode, rounding mode to apply when scaling (if `scale/*rounding-mode*` is not set)
  - :grouping        - Boolean, if true then grouping will be used
  - :grouping-size   - integer, size of a group when grouping
  - :negative-prefix - String, negative prefix to use
  - :negative-suffix - String, negative suffix to use
  - :positive-prefix - String, positive prefix to use
  - :positive-suffix - String, positive suffix to use
  - :always-sep-dec  - Boolean, if true then the decimal separator will always be shown
  - :currency-symbol-fn  - a function used on a bankster/Currency object to get its symbol as a string
  - :min-fraction-digits - integer, the minimum number of digits allowed in the fraction portion of an amount
  - :min-integer-digits  - integer, the minimum number of digits allowed in the integer portion of an amount
  - :max-fraction-digits - integer, the maximum number of digits allowed in the fraction portion of an amount
  - :max-integer-digits  - integer, the maximum number of digits allowed in the integer portion of an amount
  - :scale               - sets both :min-fraction-digits and :max-fraction-digits to the same value.

  Note that you may gain some speed by re-using the extended formatter (basic
  formatter is already cached). See the documentation of the format-with function for
  more information."
  {:tag String :added "1.0.0"}
  ([^Money money]
   (format money (Locale/getDefault)))
  ([^Money money locale]
   (if-some [rm scale/*rounding-mode*]
     (if (not= rm scale/ROUND_UNNECESSARY)
       (let [f (.dformat ^FormatterInstance (currency/formatter-instance (.currency ^Money money) locale))]
         (.format ^DecimalFormat f
                  ^BigDecimal (scale/apply (.amount ^Money money)
                                           (max (.getMaximumFractionDigits ^DecimalFormat f)
                                                (.getMinimumFractionDigits ^DecimalFormat f)) rm)))
       (.format ^DecimalFormat (.dformat ^FormatterInstance (currency/formatter-instance (.currency ^Money money) locale))
                ^BigDecimal (.amount ^Money money)))
     (.format ^DecimalFormat (.dformat ^FormatterInstance (currency/formatter-instance (.currency ^Money money) locale))
              ^BigDecimal (.amount ^Money money))))
  ([^Money money locale opts]
   (if-some [rmode (or (:rounding-mode opts) scale/*rounding-mode*)]
     (if (not= rmode scale/ROUND_UNNECESSARY)
       (.format ^DecimalFormat (currency/formatter-extended (.currency ^Money money) locale
                                                            (assoc opts :rounding-mode rmode))
                ^BigDecimal    (.amount ^Money money))
       (.format ^DecimalFormat (currency/formatter-extended (.currency ^Money money) locale opts)
                ^BigDecimal    (.amount ^Money money)))
     (.format ^DecimalFormat (currency/formatter-extended (.currency ^Money money) locale opts)
              ^BigDecimal    (.amount ^Money money)))))

(defn format-with
  "Formats the amount of money using the formatter provided. Formatters can be created
  with io.randomseed.bankster.currency/formatter and
  io.randomseed.bankster.currency/formatter-extended."
  {:tag String :added "1.0.0"}
  [^DecimalFormat formatter ^Money money]
  (.format ^DecimalFormat formatter ^BigDecimal (.amount ^Money money)))

;;
;; Printing.
;;

(defmethod print-method Money
  [m w]
  (let [c (.currency ^Money m)
        n (namespace (currency/id c))
        a (.amount ^Money m)]
    (print-simple
     (str "#money" (when n (str "/" n))
          "[" a " " (currency/short-code c) "]")
     w)))
