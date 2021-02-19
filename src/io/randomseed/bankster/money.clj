(ns io.randomseed.bankster.money

  ^{:doc    "Bankster library, money operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [ns])

  (:require [clojure.string                  :as           str]
            [clojure.reflect                 :as            cr]
            [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.scalable :as      scalable]
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

(def ^:dynamic *registry* nil)

;;
;; Rounding modes.
;;

(def ^{:const true :tag 'int} ROUND_CEILING     (int BigDecimal/ROUND_CEILING))
(def ^{:const true :tag 'int} ROUND_FLOOR       (int BigDecimal/ROUND_FLOOR))
(def ^{:const true :tag 'int} ROUND_HALF_UP     (int BigDecimal/ROUND_HALF_UP))
(def ^{:const true :tag 'int} ROUND_HALF_DOWN   (int BigDecimal/ROUND_HALF_DOWN))
(def ^{:const true :tag 'int} ROUND_HALF_EVEN   (int BigDecimal/ROUND_HALF_EVEN))
(def ^{:const true :tag 'int} ROUND_UP          (int BigDecimal/ROUND_UP))
(def ^{:const true :tag 'int} ROUND_DOWN        (int BigDecimal/ROUND_DOWN))
(def ^{:const true :tag 'int} ROUND_UNNECESSARY (int BigDecimal/ROUND_UNNECESSARY))

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
  (^Money [currency]
   (parse currency 0M ROUND_UNNECESSARY))
  (^Money [currency amount]
   (parse currency amount ROUND_UNNECESSARY))
  (^Money [currency amount rounding-mode]
   (let [^Currency c (currency/of currency (or *registry* @R))
         s (.dp ^Currency c)]
     (if (= s currency/any-decimal-places)
       (Money. ^Currency c ^BigDecimal (scalable/of amount))
       (Money. ^Currency c ^BigDecimal (scalable/of amount s rounding-mode))))))

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
   (update m :amount (fn ^BigDecimal [^BigDecimal v]
                       (.setScale ^BigDecimal v s))))
  (^Money [^Money m s rounding-mode]
   (update m :amount (fn ^BigDecimal [^BigDecimal v]
                       (.setScale ^BigDecimal v s rounding-mode)))))

(defmacro scale
  "Scales the given money using a number of decimal places and a an optional rounding
  mode (required when downscaling). If no scale is given, returns the current scale."
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
  "Returns amount of the given money"
  {:tag BigDecimal}
  (.amount ^Money money))

(defn currency
  [^Money money]
  "Returns currency of the given money."
  {:tag Currency}
  (.currency ^Money money))

;;
;; Relation to a currency.
;;

(extend-protocol currency/Payable
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

  (same?
    (^Boolean [a b]
     (= (.id ^Currency (.currency ^Money a)) (currency/id b)))
    (^Boolean [a b ^Registry registry]
     (= (.id ^Currency (.currency ^Money a)) (currency/id b registry)))))

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