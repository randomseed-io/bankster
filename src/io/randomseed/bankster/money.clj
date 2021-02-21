(ns io.randomseed.bankster.money

  ^{:doc    "Bankster library, money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [clojure.string                  :as           str]
            [clojure.reflect                 :as            cr]
            [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.scale    :as         scale]
            [io.randomseed.bankster.currency :as      currency]
            [io.randomseed.bankster.registry :as      registry]
            [io.randomseed.bankster.util.map :as           map]
            [io.randomseed.bankster.util.fs  :as            fs]
            [io.randomseed.bankster.util     :refer       :all])

  (:import  [io.randomseed.bankster Currency Registry Money]
            [java.math MathContext RoundingMode]))

;;
;; Global registry.
;;

(def ^:private R (registry/global))

;;
;; Dynamic registry.
;;

(def ^:dynamic
  *registry*
  "Registry that if set to a truthy value (not nil and not false), will be used
  instead of a global, shared registry."
  nil)

;;
;; Rounding modes.
;;

(defn ^:private parse-rounding
  "Internal helper for parsing rounding modes in macros. Accepted input:
  ROUND_mode, :ROUND_mode, BigDecimal/ROUND_mode, money/ROUND_mode, :mode,
  mode."
  [n]
  (if (ident? n)
    (let [sname (name n)
          ns-ok (if-some [ns (namespace n)]
                  (contains?
                   #{"BigDecimal"
                     "Money"
                     "money"
                     (cr/typename Money)
                     (cr/typename BigDecimal)} ns) true)]
      (if-not ns-ok n
              (if (str/starts-with? sname "ROUND_")
                (symbol "java.math.BigDecimal" sname)
                (symbol "java.math.BigDecimal" (str "ROUND_" sname)))))
    n))

;;
;; Main coercer.
;;

(defn parse
  "Internal parser."
  {:tag Money, :no-doc true}
  (^Money [^Currency currency]
   (parse ^Currency currency 0M))
  (^Money [^Currency currency amount]
   (let [^Currency c (currency/of currency (or *registry* @R))
         s (int (scale/of ^Currency c))]
     (if (currency/auto-scaled? s)
       (Money. ^Currency c ^BigDecimal (scale/apply amount))
       (Money. ^Currency c ^BigDecimal (scale/apply amount (int s))))))
  (^Money [^Currency currency amount rounding-mode]
   (let [^Currency c (currency/of currency (or *registry* @R))
         s (int (scale/of ^Currency c))]
     (if (currency/auto-scaled? s)
       (Money. ^Currency c ^BigDecimal (scale/apply amount))
       (Money. ^Currency c ^BigDecimal (scale/apply amount (int s) (int rounding-mode)))))))

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
   `(parse ~currency))
  ([currency amount]
   `(parse ~currency ~amount))
  ([currency amount rounding-mode]
   (let [rms# (parse-rounding rounding-mode)]
     `(parse ~currency ~amount ~rms#))))

;;
;; Scaling and rounding.
;;

(defn scale-core
  "Internal scaling function."
  {:no-doc true}
  ([m]
   (.scale ^BigDecimal (.amount ^Money m)))
  (^Money [^Money m s]
   (-> m
       (assoc :amount   ^BigDecimal (.setScale   ^BigDecimal (.amount   ^Money m) (int s)))
       (assoc :currency ^Currency   (scale/apply ^Currency   (.currency ^Money m) (int s)))))
  (^Money [^Money m s rounding-mode]
   (-> m
       (assoc :amount   ^BigDecimal (.setScale   ^BigDecimal (.amount   ^Money m) (int s) (int rounding-mode)))
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
   (let [rms# (parse-rounding rounding-mode)]
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
   (^Money [m scale rounding-mode] (scale-core ^Money m (int scale) rounding-mode))))

;;
;; Operations.
;;

(defn equal?
  ([^Money a ^Money b]
   nil))

(defn add
  "Adds two or more amounts of money of the same currency."
  (^Money [] nil)
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
  (^Money [^Money a ^Money b ^Money c & more]
   (reduce add (add ^Money a ^Money b) more)))

(defn subtract
  "Subtracts two or more amounts of money of the same currency."
  (^Money [] nil)
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
  (^Money [^Money a ^Money b ^Money c & more]
   (reduce subtract (subtract ^Money a ^Money b) more)))

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
