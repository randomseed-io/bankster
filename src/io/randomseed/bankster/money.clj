(ns io.randomseed.bankster.money

  ^{:doc    "Bankster library, money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [format compare])

  (:require [clojure.string]
            [trptr.java-wrapper.locale       :as             l]
            [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.scale    :as         scale]
            [io.randomseed.bankster.currency :as      currency]
            [io.randomseed.bankster.registry :as      registry]
            [io.randomseed.bankster.util.map :as           map]
            [io.randomseed.bankster.util.fs  :as            fs]
            [io.randomseed.bankster.util     :refer       :all])

  (:import  [io.randomseed.bankster Currency Registry Money]
            [java.math MathContext RoundingMode]
            [java.text NumberFormat DecimalFormat DecimalFormatSymbols]
            [java.util Locale]))

;;
;; Accountable protocol.
;;

(defprotocol ^{:added "1.0.0"} Accountable
  "This protocol is used to express (monetary) values using various numeric types and
  currency representations."

  (^{:tag io.randomseed.bankster.Money :added "1.0.0"}
   value
   [num] [currency num] [currency num rounding-mode]
   "Creates new Money object for the given value which will become an amount. If the
  currency is not given it will try to use the default one, taken from the
  `*default-currency*` dynamic variable. Optional rounding-mode should be a rounding
  mode used when the conversion to a scaled monetary amount requires rounding.

  In its unary form, when the argument is not numeric, it will try to get the
  currency object (identified by a string, a symbol or a keyword) from a default,
  global registry of currencies.

  For simple money creation the following macros may be convenient way to go: of,
  of-major, of-minor.

  Be careful about using number literals for big-scale amounts (16–17 digits). Use
  either big decimal literals, e.g. 1234.45689101112M, or strings."))

;;
;; Rescaling helper.
;;

(defn monetary-scale
  "Rescales the given number n using `io.randomseed.bankster.scale/apply` on the
  sc unless the scale is set to auto-scaled."
  {:tag BigDecimal :private true :added "1.0.7"}
  ([n]
   (scale/apply n))
  ([n sc]
   (if (currency/val-auto-scaled? sc)
     (scale/apply n)
     (scale/apply n (int sc))))
  ([n sc ^RoundingMode rm]
   (if (currency/val-auto-scaled? sc)
     (scale/apply n)
     (scale/apply n (int sc) ^RoundingMode rm))))

;;
;; Money generation macros.
;;

(def ^{:private true :tag clojure.lang.PersistentHashSet :added "1.0.0"}
  amount?
  #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \. \- \+})

(defn mk-bigdec
  {:added "1.0.6" :private true}
  [n]
  (if (number? n) (bigdec n) n))

(defn currency+amount
  "Splits the amount and currency."
  {:private true :tag clojure.lang.PersistentVector :added "1.0.0"}
  [amount]
  (let [lseq   (remove #{\_\ } (seq (if (keyword? amount)
                                      (if-some [n (namespace amount)]
                                        (str n "/" (name amount))
                                        (name amount))
                                      (if (number? amount)
                                        (.toPlainString (bigdec amount))
                                        (str amount)))))
        fdigi? (amount? (first lseq))
        [a b]  (split-with (if fdigi? amount? (complement amount?)) lseq)
        a      (when (seq a) (apply str a))
        b      (when (seq b) (apply str b))]
    (if fdigi? [b a] [a b])))

(defmacro currency-unit-strict
  "Int."
  {:tag Currency :added "1.0.0" :private true}
  [a]
  `(let [na# ~a] (when-not (number? na#) (currency/unit na#))))

(defn parse-int
  "Internal parser with amount modifier."
  {:tag Money :private true :added "1.0.0"}
  (^Money [mod-fn amount]
   (when (some? amount)
     (if (instance? Money amount)
       (parse-int mod-fn (.currency ^Money amount) (.amount ^Money amount))
       (if-some [^Currency cur (currency-unit-strict amount)]
         (parse-int mod-fn cur 0M)
         (let [[cur am] (currency+amount amount)]
           (parse-int mod-fn cur (or am 0M)))))))
  (^Money [mod-fn currency amount]
   (when (some? amount)
     (if-some [^Currency c (currency-unit-strict currency)]
       (Money. ^Currency c ^BigDecimal (monetary-scale (mod-fn amount)
                                                       (int (scale/of ^Currency c))))
       (throw (ex-info
               (str "Cannot create money amount without a valid currency and a default currency was not set.")
               {:amount amount :currency currency})))))
  (^Money [mod-fn ^Currency currency amount ^RoundingMode rounding-mode]
   (when (some? amount)
     (if-some [^Currency c (currency-unit-strict currency)]
       (Money. ^Currency c ^BigDecimal (monetary-scale (mod-fn amount)
                                                       (int (scale/of ^Currency c))
                                                       ^RoundingMode rounding-mode))
       (throw (ex-info
               (str "Cannot create money amount without a valid currency and a default currency was not set.")
               {:amount amount :currency currency}))))))

(defn parse
  "Internal parser without amount modifier."
  {:tag Money :no-doc :true :added "1.0.0"}
  (^Money [amount] (parse-int identity amount))
  (^Money [currency amount] (parse-int identity currency amount))
  (^Money [currency amount ^RoundingMode rounding-mode] (parse-int identity currency amount rounding-mode)))

(defn parse-major
  "Internal parser with scale/integer as an amount modifier."
  {:tag Money :no-doc :true :added "1.0.0"}
  (^Money [amount] (parse-int scale/integer amount))
  (^Money [currency amount] (parse-int scale/integer currency amount))
  (^Money [currency amount ^RoundingMode rounding-mode] (parse-int scale/integer currency amount rounding-mode)))

(defn parse-minor
  "Internal parser with scale/fractional as an amount modifier."
  {:tag Money :no-doc :true :added "1.0.0"}
  (^Money [amount] (parse-int scale/fractional amount))
  (^Money [currency amount] (parse-int scale/fractional currency amount))
  (^Money [currency amount ^RoundingMode rounding-mode] (parse-int scale/fractional currency amount rounding-mode)))

(defn of-gen
  "Internal parser for of- macros."
  {:no-doc true :added "1.0.0"}
  ([fun] `(~fun 0M))
  ([fun a]
   (if (or (ident? a) (string? a) (number? a))
     (let [[cur# am#] (currency+amount a)]
       (if (or (nil? cur#) (nil? am#))
         (let [[c# a#] (if (number? a) [nil (mk-bigdec a)] [a 0M])]
           (if (nil? c#)
             `(~fun ~a#)
             (let [curs# (currency/parse-currency-code c#)]
               (if (keyword? curs#)
                 `(~fun ~curs# ~a#)
                 `(let [cc# ~c#]
                    (if (instance? Money cc#)
                      (~fun cc#)
                      (~fun cc# ~a#)))))))
         (if (nil? am#)
           `(~fun ~cur#)
           `(~fun ~cur# ~(mk-bigdec am#)))))
     `(let [a# ~(mk-bigdec a)]
        (if (or (number? a#) (instance? Money a#))
          (~fun a#)
          (~fun a# 0M)))))
  ([fun a b]
   (if (or (ident? a) (string? a) (number? a))
     (let [[cur# am#] (currency+amount a)]
       (if (or (nil? cur#) (nil? am#))
         (let [[c# a#] (if (number? a) [b a] [a b])]
           `(~fun ~(currency/parse-currency-code c#) ~(mk-bigdec a#)))
         (if (or (ident? b) (string? b) (number? b))
           (let [rms# (scale/parse-rounding b)]
             `(~fun ~cur# ~(mk-bigdec am#) ~rms#))
           `(~fun ~cur# ~(mk-bigdec am#) ~b))))
     (let [b (if (number? b) b (currency/parse-currency-code b))]
       `(let [a# ~(mk-bigdec a) b# ~(mk-bigdec b)]
          (if (number? a#)
            (~fun b# a#)
            (when (number? b#)
              (~fun a# b#)))))))
  ([fun a b rounding-mode]
   (let [rms# (scale/parse-rounding rounding-mode)]
     (if (or (ident? a) (string? a) (number? a))
       (let [[c# a#] (if (number? a) [b a] [a b])]
         `(~parse ~(currency/parse-currency-code c#) ~(mk-bigdec a#) ~rms#))
       (let [b (if (number? b) b (currency/parse-currency-code b))]
         `(let [a# ~(mk-bigdec a) b# ~(mk-bigdec b)]
            (if (number? a#)
              (~parse b# a# ~rms#)
              (when (number? b#)
                (~parse a# b# ~rms#)))))))))

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

  * CEILING     - rounds towards positive infinity.
  * DOWN        - rounds towards zero.
  * FLOOR       - rounds towards negative infinity.
  * HALF_DOWN   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  * HALF_EVEN   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  * HALF_UP     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  * UP          – rounds away from zero
  * UNNECESSARY - asserts that the requested operation has an exact result, hence no rounding is necessary.

  To create a monetary object using function, call `io.randomseed.bankster.money/value`.

  Be careful about using number literals for big-scale amounts (16–17 digits). Use
  either big decimal literals, e.g. 1234.45689101112M, or strings."
  ([]    (of-gen parse))
  ([a]   (of-gen parse a))
  ([a b] (of-gen parse a b))
  ([a b rounding-mode] (of-gen parse a b rounding-mode)))

(defmacro of-major
  "Like io.randomseed.money/of but sets the amount of major part only."
  ([]    (of-gen parse))
  ([a]   (of-gen parse-major a))
  ([a b] (of-gen parse-major a b))
  ([a b rounding-mode] (of-gen parse-major a b rounding-mode)))

(defmacro of-minor
  "Like io.randomseed.money/of but sets the amount of minor part only."
  ([]    (of-gen parse))
  ([a]   (of-gen parse-minor a))
  ([a b] (of-gen parse-minor a b))
  ([a b rounding-mode] (of-gen parse-minor a b rounding-mode)))

;;
;; Accountable implementation.
;;

(extend-protocol Accountable

  Money

  (value
    (^Money [money]
     money)
    (^Money [money amount]
     (Money. ^Currency   (.currency ^Money money)
             ^BigDecimal (scale/apply amount (int (.scale ^BigDecimal (.amount ^Money money))))))
    (^Money [money amount rounding]
     (Money. ^Currency   (.currency ^Money money)
             ^BigDecimal (scale/apply amount (int (.scale ^BigDecimal (.amount ^Money money))) rounding))))

  Currency

  (value
    (^Money [currency]
     (Money. ^Currency currency
             ^BigDecimal (monetary-scale 0M (int (.scale ^Currency currency)))))
    (^Money [currency amount]
     (Money. ^Currency currency
             ^BigDecimal (monetary-scale amount (int (.scale ^Currency currency)))))
    (^Money [currency amount rounding]
     (Money. ^Currency currency
             ^BigDecimal (monetary-scale amount (int (.scale ^Currency currency)) rounding))))

  clojure.lang.Symbol

  (value
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id amount]          (value (currency/unit currency-id) amount))
    (^Money [currency-id amount rounding] (value (currency/unit currency-id) amount rounding)))

  clojure.lang.Keyword

  (value
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id amount]          (value (currency/unit currency-id) amount))
    (^Money [currency-id amount rounding] (value (currency/unit currency-id) amount rounding)))

  String

  (value
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id amount]          (value (currency/unit currency-id) amount))
    (^Money [currency-id amount rounding] (value (currency/unit currency-id) amount rounding)))

  Number

  (value
    (^Money [amount]                      (value (currency/unit currency/*default*) amount))
    (^Money [amount currency-id]          (value (currency/unit currency-id) amount))
    (^Money [amount currency-id rounding] (value (currency/unit currency-id) amount rounding)))

  nil

  (value
    ([c]     nil)
    ([a b]   (parse b))
    ([a c r] (parse c nil r)))

  clojure.lang.Sequential

  (value
    (^Money [v]     (apply parse ^Currency (first v) (rest v)))
    (^Money [v r]   (value (first v) (second v) r))
    (^Money [v b r] (value (first v) b r)))

  Object

  (value
    (^Money [a]     (parse a))
    (^Money [a c]   (parse a c))
    (^Money [a c r] (parse a c r))))

(defn major-value
  "Creates new Money object for the given value which will become a major part of the
  amount. If the given number has fractional part it will be truncated. If the
  currency is not given it should try to use the default one, taken from the
  `io.randomseed.bankster.currency/*default*` dynamic variable. Optional
  rounding-mode should be a rounding mode used when the conversion to a scaled
  monetary amount requires rounding.

  For simple money creation the following macros may be convenient way to go: of,
  of-major, of-minor."
  (^Money [a]     (when-some [v (value a)] (major-value (.currency ^Money v) (.amount ^Money v))))
  (^Money [c b]   (value c (scale/integer b)))
  (^Money [c b r] (value c (scale/integer b) r)))

(defn minor-value
  "Creates new Money object for the given value which will become a minor part of the
  amount. If the given number has fractional part it will be truncated. If the
  currency is not given it should try to use the default one, taken from the
  `io.randomseed.bankster.currency/*default*` dynamic variable. Optional
  rounding-mode should be a rounding mode used when the conversion to a scaled
  monetary amount requires rounding.

  For simple money creation the following macros may be convenient way to go: of,
  of-major, of-minor."
  (^Money [a]     (when-some [v (value a)] (minor-value (.currency ^Money v) (.amount ^Money v))))
  (^Money [c b]   (value c (scale/fractional b)))
  (^Money [c b r] (value c (scale/fractional b) r)))

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
  object is NOT updated.

  Use with caution since it can make money object no longer compliant with a scale of
  the registered currency."
  {:tag Money :added "1.0.0"}
  [^Money a]
  (Money. ^Currency   (.currency ^Money a)
          ^BigDecimal (.stripTrailingZeros ^BigDecimal (.amount ^Money a))))

;;
;; Properties.
;;

(defn amount
  "Returns the amount of the given money."
  {:tag BigDecimal :added "1.0.0"}
  (^BigDecimal [money] (when-some [m (value money)] (.amount ^Money m)))
  (^BigDecimal [a b]   (when-some [m (value a b)]   (.amount ^Money m)))
  (^BigDecimal [a b r] (when-some [m (value a b r)] (.amount ^Money m))))

(defn currency
  "Returns the currency of the given money."
  {:tag Currency :added "1.0.0"}
  (^Currency [money] (when-some [m (value money)] (.currency ^Money m)))
  (^Currency [a b]   (when-some [m (value a b)]   (.currency ^Money m)))
  (^Currency [a b r] (when-some [m (value a b r)] (.currency ^Money m))))

(defn stripped-amount
  "Returns the amount of the given money with trailing zeros removed."
  {:tag BigDecimal :added "1.0.0"}
  (^BigDecimal [money] (when-some [m (value money)] (.stripTrailingZeros ^BigDecimal (.amount ^Money m))))
  (^BigDecimal [a b]   (when-some [m (value a b)]   (.stripTrailingZeros ^BigDecimal (.amount ^Money m))))
  (^BigDecimal [a b r] (when-some [m (value a b r)] (.stripTrailingZeros ^BigDecimal (.amount ^Money m)))))

(defn unparse
  "Returns a vector with symbolic representations of amount and currency. Useful for
  printing to EDN or displaying on a console. The letter M will be added to the
  amount if its precision exceeds 15."
  {:tag clojure.lang.IPersistentVector :added "1.0.7"}
  ([money]
   (when-some [m (value money)]
     [(symbol (scale/to-plain-string ^BigDecimal (.amount ^Money m)))
      (symbol (currency/id ^Currency (.currency ^Money m)))]))
  ([a b]
   (when-some [m (value a b)]
     [(symbol (scale/to-plain-string ^BigDecimal (.amount ^Money m)))
      (symbol (currency/id ^Currency (.currency ^Money m)))]))
  ([a b r]
   (when-some [m (value a b r)]
     [(symbol (scale/to-plain-string ^BigDecimal (.amount ^Money m)))
      (symbol (currency/id ^Currency (.currency ^Money m)))])))

;;
;; Monetary implementation.
;;

(extend-protocol currency/Monetary

  Money

  (of-id
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

  (present?
    (^Boolean [money]
     (let [id (.id ^Currency (.currency ^Money money))]
       (if (namespace id)
         (contains? (registry/currency-id->currency) id)
         (contains? (registry/currency-code->currencies) id))))
    (^Boolean [money ^Registry registry]
     (let [id (.id ^Currency (.currency ^Money money))]
       (if (namespace id)
         (contains? (registry/currency-id->currency registry) id)
         (contains? (registry/currency-code->currencies registry) id)))))

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
    (let [^Currency cur (.currency ^Money m)
          sc (int (.scale ^Currency cur))]
      (if (currency/val-auto-scaled? sc) m
          (Money. ^Currency cur
                  (if-some [rm scale/*rounding-mode*]
                    ^BigDecimal (.setScale ^BigDecimal (.amount ^Money m)
                                           (int sc)
                                           ^RoundingMode rm)
                    ^BigDecimal (.setScale ^BigDecimal (.amount ^Money m)
                                           (int sc)))))))
   (^Money [m scale]
    (if-some [rm scale/*rounding-mode*]
      (Money. ^Currency   (.currency ^Money m)
              ^BigDecimal (.setScale ^BigDecimal (.amount ^Money m)
                                     (int scale)
                                     ^RoundingMode rm))
      (Money. ^Currency   (.currency ^Money m)
              ^BigDecimal (.setScale ^BigDecimal (.amount ^Money m) (int scale)))))
   (^Money [m scale ^RoundingMode rounding-mode]
    (Money. ^Currency   (.currency ^Money m)
            ^BigDecimal (.setScale ^BigDecimal (.amount^Money m)
                                   (int scale)
                                   ^RoundingMode rounding-mode))))

  (^BigDecimal amount
   (^BigDecimal [money]
    (.amount ^Money money))
   (^BigDecimal [money scale]
    (scale/apply ^BigDecimal (.amount ^Money money) (int scale)))
   (^BigDecimal [money scale ^RoundingMode r]
    (scale/apply ^BigDecimal (.amount ^Money money) (int scale) ^RoundingMode r))))

;;
;; Comparators.
;;

(declare same-currencies?)

(defn compare-amounts
  "Compares two monetary amounts of the same currency, regardless of their
  scales. Returns -1 if the second one is less than, 0 if equal to, and 1 if it is
  greater than the first."
  {:added "1.0.0" :tag 'int}
  (^Boolean [^Money a] (int 0))
  (^Boolean [^Money a ^Money b]
   (when-not (same-currencies? a b)
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (int (.compareTo ^BigDecimal (.amount ^Money a) ^BigDecimal (.amount ^Money b)))))

(defn compare
  "Compares two monetary amounts of the same currency and scale. Returns -1 if the
  second one is less than, 0 if equal to, and 1 if it is greater than the first."
  {:added "1.0.0" :tag 'int}
  (^Boolean [^Money a] (int 0))
  (^Boolean [^Money a ^Money b]
   (let [^BigDecimal am-a (.amount ^Money a)
         ^BigDecimal am-b (.amount ^Money b)]
     (when-not (same-currencies? a b)
       (throw (ex-info "Cannot compare amounts of different currencies."
                       {:money-1 a :money-2 b})))
     (when-not (= (.scale ^BigDecimal am-a) (.scale ^BigDecimal am-b))
       (throw (ex-info "Cannot compare amounts having different decimal scales."
                       {:money-1 a :money-2 b})))
     (int (.compareTo ^BigDecimal am-a ^BigDecimal am-b)))))

;;
;; Predicates.
;;

(defn money?
  "Returns true if the given value is a kind of Money."
  {:tag Boolean :added "1.0.0"}
  [a]
  (instance? Money a))

(defn rescaled?
  "Returns true if the given monetary value has different scale than its currency (so
  it was rescaled). Returns false if the scale is no different. Returns nil if the
  currency does not have a fixed scale."
  {:added "1.0.0"}
  [a]
  (let [csc (int (.scale ^Currency (.currency ^Money a)))]
    (when-not (currency/val-auto-scaled? csc)
      (not= csc (.scale ^BigDecimal (.amount ^Money a))))))

(defn same-currencies?
  "Returns true if both currencies are the same for the given money objects."
  {:tag Boolean :added "1.0.0"}
  [^Money a ^Money b]
  (= ^Currency (.currency ^Money a)
     ^Currency (.currency ^Money b)))

(defn same-currency-ids?
  "Returns true if both currencies have the same IDs for the given money objects."
  {:tag Boolean :added "1.0.0"}
  [^Money a ^Money b]
  (= (.id ^Currency (.currency ^Money a))
     (.id ^Currency (.currency ^Money b))))

(defn eq?
  "Return true if the money amounts and their currencies are equal. Note that
  currencies with different scales are considered different. Use eq-am? to compare
  amounts regardless of their scales."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (and (.equals (.amount ^Money a) (.amount ^Money b))
        (same-currencies? a b)))
  (^Boolean [^Money a ^Money b & more]
   (if (eq? a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq? b (first more)))
     false)))

(defn eq-am?
  "Return true if the money amounts and their currencies are equal, regardless of their
  scales."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] true)
  (^Boolean [^Money a ^Money b]
   (if-not (same-currencies? a b)
     false
     (let [^BigDecimal am-a (.amount ^Money a)
           ^BigDecimal am-b (.amount ^Money b)
           sa (int (.scale am-a))
           sb (int (.scale am-b))]
       (if (= sa sb)
         (.equals ^BigDecimal am-a ^BigDecimal am-b)
         (if (< sa sb)
           (.equals ^BigDecimal (scale/apply am-a sb) ^BigDecimal am-b)
           (.equals ^BigDecimal am-a ^BigDecimal (scale/apply am-b sa)))))))
  (^Boolean [^Money a ^Money b & more]
   (if (eq-am? a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq-am? b (first more)))
     false)))

(defn ne?
  "Returns true if the money amounts or their currencies are different, regardless of
  their scales. Note that currencies with different scales are considered different."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] false)
  (^Boolean [^Money a ^Money b]
   (not (eq? ^Money a ^Money b)))
  (^Boolean [^Money a ^Money b & more]
   (not (apply eq? ^Money a ^Money b more))))

(defn ne-am?
  "Returns true if the money amounts or their currencies are different, regardless of
  their scales."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money a] false)
  (^Boolean [^Money a ^Money b]
   (not (eq-am? ^Money a ^Money b)))
  (^Boolean [^Money a ^Money b & more]
   (not (apply eq-am? ^Money a ^Money b more))))

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
  is NOT updated. If no scale is given, returns the current scale of an amount (not
  the nominal scale of a currency – which is important in rescaled amount or
  auto-scaled currencies).

  Auto-scaled monetary values are returned unchanged.

  Use with caution since it can make money object no longer compliant with a scale of
  the currency."
  {:added "1.0.0"}
  ([^Money money]
   (int (.scale ^BigDecimal (.amount ^Money money))))
  (^Money [^Money money scale]
   (scale/apply ^Money money (int scale)))
  (^Money [^Money money scale ^RoundingMode rounding-mode]
   (scale/apply ^Money money (int scale) ^RoundingMode rounding-mode)))

(defn rescale
  "Same as scale but its unary variant it will rescale an amount of the given money to
  conform it to its currency settings instead of returning the scale. It has the same
  effect as calling `io.randomseed.bankster.scale/apply` on a money object without
  passing any other arguments."
  {:tag Money :added "1.0.0"}
  (^Money [^Money money]
   (scale/apply ^Money money))
  (^Money [^Money money scale]
   (scale/apply ^Money money (int scale)))
  (^Money [^Money money scale ^RoundingMode rounding-mode]
   (scale/apply ^Money money (int scale) ^RoundingMode rounding-mode)))

(defn add
  "Adds two or more amounts of money of the same currency. When called without any
  arguments, returns 0."
  {:tag Money :added "1.0.0"}
  (^Money [] 0M)
  (^Money [^Money a]
   (when-not (instance? Money a)
     (throw (ex-info "Argument must be a kind of Money."
                     {:augend a})))
   a)
  (^Money [^Money a ^Money b]
   (when-not (and (instance? Money a) (instance? Money b))
     (throw (ex-info "Both arguments must be a kind of Money."
                     {:augend a :addend b})))
   (if (same-currencies? a b)
     (Money. ^Currency   (.currency ^Money a)
             ^BigDecimal (.add  ^BigDecimal (.amount ^Money a)
                                ^BigDecimal (.amount ^Money b)))
     (throw (ex-info
             "Cannot add amounts of two different currencies."
             {:augend a :addend b}))))
  (^Money [^Money a ^Money b & more]
   (when-not (instance? Money a)
     (throw (ex-info "Both arguments must be a kind of Money."
                     {:augend a :addend b})))
   (let [^Currency cur-a (.currency ^Money a)
         fun (fn [^BigDecimal a b]
               (when-not (instance? Money b)
                 (throw (ex-info
                         "Cannot add a regular number to the monetary amount."
                         {:augend a :addend b})))
               (when-not (= cur-a (.currency ^Money b))
                 (throw (ex-info
                         "Cannot add amounts of two different currencies."
                         {:augend a :addend b})))
               (.add ^BigDecimal a ^BigDecimal (.amount ^Money b)))]
     (Money. ^Currency   (.currency ^Money a)
             ^BigDecimal (reduce fun (fun (.amount ^Money a) b) more)))))

(def ^{:tag Money :added "1.0.0"
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  add-scaled
  "Alias for add."
  add)

(defn sub
  "Subtracts two or more amounts of money of the same currency. When called with a
  single argument, negates its value."
  {:tag Money :added "1.0.0"}
  (^Money [^Money a]
   (when-not (instance? Money a)
     (throw (ex-info "Argument must be a kind of Money."
                     {:augend a})))
   (Money. ^Currency   (.currency ^Money a)
           ^BigDecimal (.negate ^BigDecimal (.amount ^Money a))))
  (^Money [^Money a ^Money b]
   (when-not (and (instance? Money a) (instance? Money b))
     (throw (ex-info "Both arguments must be a kind of Money."
                     {:augend a :addend b})))
   (if (same-currencies? a b)
     (Money. ^Currency   (.currency ^Money a)
             ^BigDecimal (.subtract  ^BigDecimal (.amount ^Money a)
                                     ^BigDecimal (.amount ^Money b)))
     (throw (ex-info
             "Cannot subtract amounts of two different currencies."
             {:minuend a :subtrahend b}))))
  (^Money [^Money a ^Money b & more]
   (when-not (instance? Money a)
     (throw (ex-info "Both arguments must be a kind of Money."
                     {:augend a :addend b})))
   (let [^Currency cur-a (.currency ^Money a)
         fun (fn [^BigDecimal a b]
               (when-not (instance? Money b)
                 (throw (ex-info
                         "Cannot subtract a regular number from the monetary amount."
                         {:minuend a :subtrahend b})))
               (when-not (= cur-a (.currency ^Money b))
                 (throw (ex-info
                         "Cannot subtract amounts of two different currencies."
                         {:minuend a :subtrahend b})))
               (.subtract ^BigDecimal a ^BigDecimal (.amount ^Money b)))]
     (Money. ^Currency   (.currency ^Money a)
             ^BigDecimal (reduce fun (fun (.amount ^Money a) b) more)))))

(def ^{:tag Money :added "1.0.0"
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  sub-scaled
  "Alias for sub."
  sub)

(defmacro mul-core
  "Internal multiplier."
  {:added "1.0.0" :private true}
  ([a b scale r]
   `(.setScale ^BigDecimal (.multiply ^BigDecimal ~a ^BigDecimal (scale/apply ~b))
               (int ~scale)
               ^RoundingMode ~r))
  ([a b]
   `(.multiply ^BigDecimal ~a ^BigDecimal (scale/apply ~b))))

(declare mul)
(defrecord LastMoney [^Money m ^int sc])

(defn mul-scaled
  "Multiplies two or more values where one may be a Money object. If one of the values
  is a kind of Money then the result will also be a kind of Money. For a single value
  it simply returns it. For more than 2 arguments it repeatedly multiplies them as
  described.

  When there is a multiplication of a monetary value by the regular number, the final
  result of all operations is re-scaled to match the scale of the monetary amount. If
  the scaling requires rounding, enclosing the expression within
  `io.randomseed.bankster.scale/with-rounding` is required (the macro is aliased
  under the same name in this namespace).

  Note that rescaling is not based on the original scale of a currency. This is
  designed that way is to handle cases when monetary amount was intentionally
  rescaled earlier.

  For regular numbers dynamic rescaling is performed only to handle corner cases
  where there would be a non-terminating decimal expansion.

  Rescaling is performed on each operation unless/until all values are regular
  numbers (without any Money object among them)."
  {:added "1.0.0"}
  ([] 1M)
  ([a] a)
  ([a b] (mul a b))
  ([a b & more]
   (let [am? (instance? Money a)
         bm? (instance? Money b)
         ^BigDecimal am (if am? (.amount ^Money a) (scale/apply a))
         bm  (if bm? (.amount ^Money b) b)
         mon (volatile! (when am? (LastMoney. ^Money a (int (.scale am)))))
         ^RoundingMode rm (or scale/*rounding-mode* scale/ROUND_UNNECESSARY)
         fun (fn [^BigDecimal a b]
               (if-let [^LastMoney m @mon]
                 (if (instance? Money b)
                   ;; money, money
                   (throw (ex-info "Only one multiplied value can be a kind of Money."
                                   {:multiplicant a :multiplier b}))
                   ;; money, number
                   (mul-core ^BigDecimal a b (int (.sc ^LastMoney m)) ^RoundingMode rm))
                 (if (instance? Money b)
                   ;; number, money
                   (let [^BigDecimal bm (.amount ^Money b)
                         bsc (int (.scale ^BigDecimal bm))]
                     (vreset! mon (LastMoney. ^Money b (int bsc)))
                     (mul-core ^BigDecimal a ^BigDecimal bm (int bsc) ^RoundingMode rm))
                   ;; number, number
                   (mul-core ^BigDecimal a ^BigDecimal b))))
         ^BigDecimal res (reduce fun (fun ^BigDecimal am bm) more)]
     (if-some [m @mon]
       (Money. ^Currency (.currency ^Money (.m ^LastMoney m)) ^BigDecimal res)
       ^BigDecimal res))))

(defn mul
  "Multiplies two or more values where one may be a Money object. If one of the values
  is a kind of Money then the result will also be a kind of Money. For a single value
  it simply returns it. For more than 2 arguments it repeatedly multiplies them as
  described.

  When there is a multiplication of a monetary value by the regular number, the final
  result of all operations is re-scaled to match the scale of monetary amount. If the
  scaling requires rounding, enclosing the expression within
  `io.randomseed.bankster.scale/with-rounding` is required (the macro is aliased
  under the same name in this namespace).

  Note that rescaling is not based on the original scale of a currency. This is
  designed that way is to handle cases when monetary amount was intentionally
  rescaled earlier.

  For regular numbers dynamic rescaling is performed only to handle corner cases
  where there would be a non-terminating decimal expansion.

  Rescaling is performed after performing all operations unless all values are
  regular numbers (without any Money object among them)."
  {:added "1.0.0"}
  ([] 1M)
  ([a] a)
  ([a b]
   (let [am? (instance? Money a)
         bm? (instance? Money b)
         ^BigDecimal am (if am?
                          ^BigDecimal (.amount ^Money a)
                          ^BigDecimal (scale/apply a))
         bm (if bm? (.amount ^Money b) b)]
     (if am?
       (if bm?
         ;; money, money
         (throw (ex-info "Only one multiplied value can be a kind of Money."
                         {:multiplicant a :multiplier b}))
         ;; money, number
         (Money. ^Currency   (.currency ^Money a)
                 ^BigDecimal (mul-core ^BigDecimal am bm
                                       (int (.scale ^BigDecimal am))
                                       (or ^RoundingMode scale/*rounding-mode*
                                           ^RoundingMode scale/ROUND_UNNECESSARY))))
       (if bm?
         ;; number, money
         (Money. ^Currency   (.currency ^Money b)
                 ^BigDecimal (mul-core ^BigDecimal am ^BigDecimal bm
                                       (int (.scale ^BigDecimal bm))
                                       (or ^RoundingMode scale/*rounding-mode*
                                           ^RoundingMode scale/ROUND_UNNECESSARY)))
         ;; number, number
         (mul-core ^BigDecimal am bm)))))
  ([a b & more]
   (if scale/*each*
     (mul-scaled a b more)
     (let [am? (instance? Money a)
           bm? (instance? Money b)
           ^BigDecimal am (if am?
                            ^BigDecimal (.amount ^Money a)
                            ^BigDecimal (scale/apply a))
           bm  (if bm? (.amount ^Money b) b)
           mon (volatile! (when am? ^Money a))
           fun (fn [^BigDecimal a b]
                 (if-some [^Money m @mon]
                   (if (instance? Money b)
                     ;; money, money
                     (throw (ex-info "Only one multiplied value can be a kind of Money."
                                     {:multiplicant a :multiplier b}))
                     ;; money, number
                     (mul-core ^BigDecimal a b))
                   (if (instance? Money b)
                     ;; number, money
                     (mul-core ^BigDecimal a
                               ^BigDecimal (.amount ^Money (vreset! mon ^Money b)))
                     ;; number, number
                     (mul-core ^BigDecimal a ^BigDecimal b))))
           ^BigDecimal res (reduce fun ^BigDecimal (fun ^BigDecimal am bm) more)]
       (if-some [^Money m @mon]
         (Money. ^Currency (.currency ^Money m)
                 (if-some [^RoundingMode rm scale/*rounding-mode*]
                   ^BigDecimal (.setScale ^BigDecimal res
                                          (int (.scale ^BigDecimal (.amount ^Money m)))
                                          ^RoundingMode rm)
                   ^BigDecimal (.setScale ^BigDecimal res
                                          (int (.scale ^BigDecimal (.amount ^Money m))))))
         ^BigDecimal res)))))

(defn div-core-fn
  "Performs division without rounding and rescaling."
  {:tag BigDecimal :added "1.0.0" :private true}
  (^BigDecimal [^BigDecimal a b]
   (if (instance? Money b)
     (throw (ex-info "Cannot divide a regular number by the monetary amount."
                     {:dividend a :divisor b}))
     (.divide ^BigDecimal a ^BigDecimal (scale/apply b)))))

(defmacro div-core
  "In its binary variant it performs division without rounding and rescaling.

  In its ternary variant divides with precision calculated to fit the longest possible
  result (unless there is non-terminating decimal expansion then the result will be
  rounded).

  In its quartary variant divides with scale and rounding mode."
  {:added "1.0.0" :private true}
  ([a b scale r]
   `(.divide ^BigDecimal ~a
             ^BigDecimal (scale/apply ~b)
             (int ~scale)
             ^RoundingMode ~r))
  ([a b r]
   `(let [^BigDecimal a# ~a
          ^BigDecimal b# (scale/apply ~b)]
      (.divide ^BigDecimal  a#
               ^BigDecimal  b#
               ^MathContext (scale/div-math-context
                             ^BigDecimal   a#
                             ^BigDecimal   b#
                             ^RoundingMode ~r))))
  (^BigDecimal [^BigDecimal a b]
   `(let [b# ~b]
      (if (instance? Money b#)
        (throw (ex-info "Cannot divide a regular number by the monetary amount."
                        {:dividend ~a :divisor b#}))
        (.divide ^BigDecimal ~a ^BigDecimal (scale/apply b#))))))

(declare div)

(defn div-scaled
  "Divides two or more amounts of money of the same currency or numbers. Scales and
  optionally rounds each intermediate result (if there are more divisors).

  If the first given argument is a kind of money and one of the divisors is also a
  kind of Money, the result will be a BigDecimal kind of number. If the earlier value
  is a kind of Money and the second is a number, the result will be a Money. If the
  first argument is a number and the second argument is a monetary amount, an
  exception will be thrown.

  For a single value it returns a division of 1 by that number and only works for
  numbers.

  For more than 2 arguments it repeatedly divides their values as described and
  re-scales each result to match the scale of the first encountered monetary
  amount. If there are no monetary amounts involved the scale is calculated
  dynamically.

  Note that rescaling is not based on the original scale of a currency. This is
  designed that way is to handle cases when monetary amount was intentionally
  rescaled earlier.

  For regular numbers dynamic rescaling is performed only to handle corner cases
  where there would be a non-terminating decimal expansion.

  If the scaling requires rounding then enclosing the expression within
  `with-rounding` (also present in `io.randomseed.bankster.scale` namespace) is
  required."
  {:added "1.0.0"}
  ([a]   (div 1M a))
  ([a b] (div a b))
  ([a b & more]
   (let [^RoundingMode rm (or scale/*rounding-mode* nil)]
     (if-not (instance? Money a)
       (if (instance? Money b)
         (throw (ex-info "Cannot divide a regular number by the monetary amount."
                         {:dividend a :divisor b}))
         ;;
         ;; decimals only - auto-scaling the results
         ;;
         (if rm
           ;;
           ;; rounding to max. possible precision
           ;;
           (reduce (fn ^BigDecimal [^BigDecimal a b]
                     (when (instance? Money b)
                       (throw (ex-info "Cannot divide a regular number by the monetary amount."
                                       {:dividend a :divisor b})))
                     (div-core ^BigDecimal a b ^RoundingMode rm))
                   (div-core ^BigDecimal (scale/apply a) b ^RoundingMode rm)
                   more)
           ;;
           ;; not rounding, not rescaling (since it's a decimal and rounding is not set)
           ;;
           (reduce div-core-fn
                   (div-core ^BigDecimal (scale/apply a) b)
                   more)))
       ;;
       ;; first argument is money
       ;;
       (let [^RoundingMode rm (or rm scale/ROUND_UNNECESSARY)
             ^BigDecimal am (.amount ^Money a)
             am-scale (int (.scale ^BigDecimal am))
             bm?      (instance? Money b)
             bm       (if bm? ^BigDecimal (.amount ^Money b) b)]
         (loop [^BigDecimal x (div-core ^BigDecimal am bm
                                        (int (.scale ^BigDecimal am))
                                        ^RoundingMode rm)
                more more]
           (if-some [y (first more)]
             ;; there is next element
             (if (instance? Money y)
               ;; next is money
               (if-not (same-currencies? a y)
                 (throw (ex-info "Cannot divide by the amount of a different currency."
                                 {:dividend a :divisor b}))
                 ;; first 2 are money - treat like a regular decimals
                 ;; but scale each time to match the scale of first encountered money
                 ;; stop iteration and reduce since we don't expect money
                 (reduce
                  (fn ^BigDecimal [^BigDecimal a b]
                    (div-core ^BigDecimal a b
                              (int am-scale)
                              ^RoundingMode rm))
                  (div-core ^BigDecimal x
                            ^BigDecimal (.amount ^Money y)
                            (int am-scale)
                            ^RoundingMode rm)
                  (rest more)))
               ;; second is a regular number, first is already a decimal
               ;; loop and rescale to money with rounding
               (recur (div-core ^BigDecimal x y
                                (int am-scale)
                                ^RoundingMode rm)
                      (rest more)))
             ;; no more data to process
             ;; if we have not reduced monetary value, rescale it to match the initial
             (if bm? x
                 (Money. ^Currency   (.currency ^Money a)
                         ^BigDecimal (scale/apply x am-scale rm))))))))))

(defn div
  "Divides two or more amounts of money of the same currency or numbers. If two of the
  given values are a kind of Money, the result will be a BigDecimal number. If the
  earlier value is a kind of Money and the second is a number, the result will be a
  Money. If the first argument is a number and the second argument is a monetary
  amount, an exception will be thrown. The result will always be either a kind of
  Money or a BigDecimal number.

  For a single value it returns a division of 1 by that number.

  For more than 2 arguments it repeatedly divides their values as described. If the
  previous calculations produce a regular number, all consequent divisors must be
  regular numbers. Practically, if the division begins with a monetary amount, only
  one monetary amount at some point later is permitted (which will cause the function
  to switch to the regular numbers calculation since the units will cancel themselves).

  Note that rescaling is not based on the original scale of a currency. This is
  designed that way is to handle cases when monetary amount was intentionally
  rescaled earlier.

  For regular numbers dynamic rescaling is performed only to handle corner cases
  where there would be a non-terminating decimal expansion.

  Rescaling is performed after all calculations are done. However, the scale can be
  applied on each calculation if the dynamic variable
  `io.randomseed.bankster.scale/*each*` is set to a truthy value. If the scaling
  requires rounding then enclosing the expression within `with-rounding` is
  required. This can also be achieved by enclosing the expression within
  `io.randomseed.bankster.scale/with-rescaling` (aliased in this namespace under the
  same name) macro combining the switch and setting the scale."
  {:added "1.0.0"}
  ([a] (div 1M a))
  ([a b]
   (let [am? (instance? Money a)
         bm? (instance? Money b)
         ^RoundingMode rm (or scale/*rounding-mode* scale/ROUND_UNNECESSARY)
         ^BigDecimal am (if am? (.amount ^Money a) (scale/apply a))
         bm (if bm? (.amount ^Money b) b)]
     (if am?
       (if bm?
         ^BigDecimal (div-core ^BigDecimal am bm
                               (int (.scale am))
                               ^RoundingMode rm)
         (Money. ^Currency   (.currency ^Money a)
                 ^BigDecimal (div-core ^BigDecimal am bm
                                       (int (.scale am))
                                       ^RoundingMode rm)))
       (if bm?
         (throw (ex-info "Cannot divide a regular number by the monetary amount."
                         {:dividend a :divisor b}))
         (if rm
           (div-core ^BigDecimal am bm ^RoundingMode rm)
           (.divide     ^BigDecimal am ^BigDecimal (scale/apply bm)))))))
  ([a b & more]
   (if scale/*each*
     (apply div-scaled a b more)
     (let [^RoundingMode rm (or scale/*rounding-mode* nil)]
       (if-not (instance? Money a)
         (if rm
           (reduce (fn ^BigDecimal [^BigDecimal a b] (div-core ^BigDecimal a b ^RoundingMode rm))
                   (div-core ^BigDecimal (scale/apply a) b ^RoundingMode rm)
                   more)
           (reduce div-core-fn (div-core ^BigDecimal (scale/apply a) b) more))
         (let [^RoundingMode rm (or rm scale/ROUND_UNNECESSARY)
               ^BigDecimal   am (.amount ^Money a)
               bm? (instance? Money b)
               ^BigDecimal   bm (if bm? (.amount ^Money b) b)]
           (loop [^BigDecimal x (div-core ^BigDecimal am bm ^RoundingMode rm)
                  more more]
             (if-some [y (first more)]
               (if (instance? Money y)
                 (if bm?
                   (throw (ex-info "Cannot divide a regular number by the monetary amount."
                                   {:dividend x :divisor y}))
                   (if (same-currencies? a y)
                     (reduce
                      (fn ^BigDecimal [^BigDecimal a b] (div-core ^BigDecimal a b ^RoundingMode rm))
                      (div-core ^BigDecimal x ^BigDecimal (.amount ^Money y) ^RoundingMode rm)
                      (rest more))
                     (throw (ex-info "Cannot divide by the amount of a different currency."
                                     {:dividend a :divisor b}))))
                 (recur (div-core ^BigDecimal x y ^RoundingMode rm) (rest more)))
               (if bm? x
                   (Money. ^Currency  (.currency ^Money a)
                           ^BigDecimal (scale/apply x (.scale ^BigDecimal am))))))))))))

(defn min-amount
  "Returns the least of the monetary amounts."
  {:tag Money :added "1.0.0"}
  ([^Money a] a)
  ([^Money a ^Money b]
   (when-not (same-currencies? a b)
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (if (lt? a b) a b))
  ([^Money a ^Money b & more]
   (reduce min-amount (min-amount ^Money a ^Money b) more)))

(defn max-amount
  "Returns the greatest of the monetary amounts."
  {:tag Money :added "1.0.0"}
  ([^Money a] a)
  ([^Money a ^Money b]
   (when-not (same-currencies? a b)
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (if (gt? a b) a b))
  ([^Money a ^Money b & more]
   (reduce max-amount (max-amount a b) more)))

(defn convert
  "Converts a monetary amount from one currency to another using the given
  multiplier (expressing a conversion rate). When two money objects are given, the
  second one should express a price of 1 unit of the first currency.

  When no rounding mode is given and rounding is required during scaling to another
  currency then the value of dynamic variable
  `io.randomseed.bankster.scale/*rounding-mode*` will be used."
  {:tag Money :added "1.0.0"
   :arglists '([money price]
               [money price rounding-mode]
               [money to-currency exchange-rate]
               [money to-currency exchange-rate rounding-mode])}
  (^Money [a price]
   (let [^Money price (value price)]
     (convert a
              (.currency ^Money price)
              (.amount   ^Money price))))
  (^Money [a currency-or-price rate-or-rounding-mode]
   (if (instance? Money currency-or-price)
     (convert a
              (.currency ^Money currency-or-price)
              (.amount   ^Money currency-or-price)
              rate-or-rounding-mode)
     (let [^Currency currency (currency/unit currency-or-price)
           ^Money a (value a)
           sc (int (scale/of ^Currency currency))]
       (Money. ^Currency   currency
               ^BigDecimal (if (currency/val-auto-scaled? sc)
                             (.multiply ^BigDecimal (.amount ^Money a)
                                        ^BigDecimal (scale/apply rate-or-rounding-mode))
                             (scale/apply (.multiply ^BigDecimal (.amount ^Money a)
                                                     ^BigDecimal (scale/apply rate-or-rounding-mode))
                                          (int sc)))))))
  (^Money [a currency rate rounding-mode]
   (let [^Currency currency (currency/unit currency)
         ^Money a (value a)
         sc (int (scale/of ^Currency currency))]
     (Money. ^Currency   currency
             ^BigDecimal (if (currency/val-auto-scaled? sc)
                           (.multiply ^BigDecimal (.amount ^Money a)
                                      ^BigDecimal (scale/apply rate))
                           (scale/apply (.multiply ^BigDecimal (.amount ^Money a)
                                                   ^BigDecimal (scale/apply rate))
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
  "Returns the major part of the given amount of money."
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

(defn ->symbol
  "Converts an amount of the given money to a symbol."
  {:tag clojure.lang.Symbol :added "1.0.9"}
  [^Money a]
  (scale/to-symbol ^BigDecimal (.amount ^Money a)))

(defn ->clojure-symbol
  "Converts an amount of the given money to a clojure symbol (with added M when
  needed)."
  {:tag clojure.lang.Symbol :added "1.0.9"}
  [^Money a]
  (scale/to-clojure-symbol ^BigDecimal (.amount ^Money a)))

(defn ->double
  "Converts an amount of the given money to double."
  {:tag 'double :added "1.0.0"}
  [^Money a]
  (.doubleValue ^BigDecimal (.amount ^Money a)))

(defn ->float
  "Converts an amount of the given money to float."
  {:tag 'float :added "1.0.0"}
  [^Money a]
  (.floatValue ^BigDecimal (.amount ^Money a)))

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
         (of-gen parse c)
         (if (nil? r)
           (of-gen parse c amount)
           (of-gen parse c amount r)))))))

(defn defliteral
  "For the given currency identifier or a currency object it creates a tagged literal
  in a form of #money/CURRENCY where the CURRENCY is a currency code. As a side
  effect it creates a function of name io.randomseed.bankster.money/of-CURRENCY that
  will handle the literal.

  The literals will be bound to *data-readers* in a local thread."
  {:added "1.0.0"}
  [c]
  (when-some [^Currency c (currency/unit c)]
    (let [cush (currency/code c)
          name (str "of-" cush)
          nsnm "io.randomseed.bankster.money"
          mkfn (fn [amount] (if (nil? amount) '(quote nil) (of c amount)))
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

  The function assigned to the :currency-symbol-fn should take 3 arguments:
  currency, locale and registry.

  Note that you may gain some speed by re-using the extended formatter (basic
  formatter is already cached). See the documentation of the format-with function for
  more information."
  {:tag String :added "1.0.0"}
  ([^Money money]
   (format money (Locale/getDefault)))
  ([^Money money locale]
   (if-some [rm scale/*rounding-mode*]
     (if (not= rm scale/ROUND_UNNECESSARY)
       (let [f (currency/formatter-instance (.currency ^Money money) locale)]
         (.format ^DecimalFormat f
                  ^BigDecimal (scale/apply (.amount ^Money money)
                                           (max (.getMaximumFractionDigits ^DecimalFormat f)
                                                (.getMinimumFractionDigits ^DecimalFormat f)) rm)))
       (.format ^DecimalFormat (currency/formatter-instance (.currency ^Money money) locale)
                ^BigDecimal    (.amount ^Money money)))
     (.format ^DecimalFormat (currency/formatter-instance (.currency ^Money money) locale)
              ^BigDecimal    (.amount ^Money money))))
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
        a (scale/to-clojure-string ^BigDecimal (.amount ^Money m))]
    (print-simple
     (str "#money" (when n (str "/" n))
          "[" a " " (currency/code c) "]")
     w)))
