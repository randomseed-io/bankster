(ns

    ^{:doc    "Bankster library, money operations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.money

  (:refer-clojure :exclude [apply format compare cast rem
                            = == not= not== < > >= <=
                            + - * / abs min max
                            pos? neg? zero?])

  (:require [clojure.edn]
            [io.randomseed.bankster]
            [io.randomseed.bankster.scale            :as            scale]
            [io.randomseed.bankster.currency         :as         currency]
            [io.randomseed.bankster.registry         :as         registry]
            [io.randomseed.bankster.serializers.json :as serializers-json]
            [io.randomseed.bankster.util.fs          :as               fs])

  (:import  (io.randomseed.bankster       Currency
                                          Registry
                                          Money)
            (io.randomseed.bankster.scale Scalable)
            (java.math                    BigDecimal
                                          BigInteger
                                          MathContext
                                          RoundingMode)
            (java.text                    NumberFormat
                                          DecimalFormat
                                          DecimalFormatSymbols)
            (java.util                    Locale)))

;;
;; Accountable protocol.
;;

(defprotocol ^{:added "1.0.0"} Accountable
  "This protocol is used to express (monetary) values using various numeric types and
  currency representations."

  (^{:tag io.randomseed.bankster.Money :added "1.0.0"}
   value
   [num] [currency num] [currency num rounding-mode]
   "Creates new `Money` object for the given value `num` which will become an
  amount. If the `currency` is not given it will try to use the default one, taken
  from the `io.randomseed.bankster.currency/*default*` dynamic variable. Optional
  `rounding-mode` should be a rounding mode used when the conversion to a scaled
  monetary amount requires rounding.

  In its unary form, when the argument is not numeric, it will try to get the
  currency object (identified by a string, a symbol or a keyword) from the default,
  global registry of currencies.

  For simple money creation the following macros may be convenient way to go: `of`,
  `of-major`, `of-minor`.

  Be careful about using number literals for big-scale amounts (16–17 digits). Use
  either big decimal literals, e.g. 1234.45689101112M, or strings.")

  (^{:tag io.randomseed.bankster.Money :added "1.1.0"}
   cast
   [money] [money currency] [money currency rounding-mode]
   "Casts an existing Money object to another having a different currency, rescaling
  the amount and optionally rounding it. This is useful when operating on multiple
  currency registries compatible with different data sources or processing
  engines. For simply ensuring that a currency is sourced in the right registry, use
  `of-registry`."))

;;
;; Rescaling helper.
;;

(defmacro arith-ex
  "Executes `body` and, if an ArithmeticException is thrown, rethrows it as
  ExceptionInfo with standardized ex-data keys.

  Adds:
  - `:arithmetic-exception true`
  - `:arithmetic-exception/cause <ArithmeticException>`"
  {:private true :added "2.0.0"}
  [op data & body]
  `(try
     ~@body
     (catch ArithmeticException e#
       (throw
        (ex-info (or (.getMessage e#) "Arithmetic error.")
                 (assoc ~data
                        :op ~op
                        :arithmetic-exception true
                        :arithmetic-exception/cause e#)
                 e#)))))

(defmacro bd-set-scale
  "Rescales `bd` (a BigDecimal) to `scale` using the optional rounding mode `rm`
  without protocol dispatch. `rm` is evaluated only when scaling is needed.

  On ArithmeticException it throws ExceptionInfo via `arith-ex`."
  {:private true :added "2.0.0"}
  [bd scale rm op data]
  `(let [^BigDecimal bd# ~bd
         sc#             (int ~scale)]
     (if (clojure.core/== (.scale bd#) sc#)
       bd#
       (let [rm# ~rm]
         (arith-ex ~op
                   (assoc ~data :value bd# :scale sc# :rounding-mode rm#)
                   (if (some? rm#)
                     (.setScale ^BigDecimal bd# sc# ^RoundingMode rm#)
                     (.setScale ^BigDecimal bd# sc#)))))))

(defn monetary-scale
  "Rescales the given number `n` using `io.randomseed.bankster.scale/apply` on the
  sc unless the scale is set to auto-scaled."
  {:tag BigDecimal :private true :added "1.0.7"}
  ([n]
   (if (instance? BigDecimal n)
     ^BigDecimal n
     (scale/apply n)))
  ([n ^long sc]
   (if (currency/val-auto-scaled*? (int sc))
     (if (instance? BigDecimal n)
       ^BigDecimal n
       (scale/apply n))
     (let [^BigDecimal bd (if (instance? BigDecimal n) n (scale/apply n))]
       (bd-set-scale bd (int sc) (scale/rounding-mode) :scale/apply {}))))
  ([n ^long sc ^RoundingMode rm]
   (if (currency/val-auto-scaled*? (int sc))
     (if (instance? BigDecimal n)
       ^BigDecimal n
       (scale/apply n))
     (let [^BigDecimal bd (if (instance? BigDecimal n) n (scale/apply n))]
       (bd-set-scale bd (int sc) rm :scale/apply {})))))

(defmacro currency-auto-scaled?
  "Returns true if the given Currency object is auto-scaled."
  {:private true :added "1.1.2"}
  [c] `(clojure.core/== currency/auto-scaled (.scale ~c)))

(defn auto-scaled?
  "Returns true if the given Money object is based on a currency which is auto-scaled."
  {:tag Boolean :added "1.1.2"}
  [^Money m] (currency-auto-scaled? ^Currency (.currency ^Money m)))

;;
;; Predicate helpers
;;

(defmacro same-currency-objs-native?
  "Returns true if both Currency objects are the same for Money operations.

  Currency weight is ignored."
  {:private true :added "2.0.0"}
  [ca cb]
  `(let [^Currency ca# ~ca
         ^Currency cb# ~cb]
     (or (clojure.core/identical? ca# cb#)
         (and (clojure.core/identical? (.id      ^Currency  ca#)        (.id      ^Currency cb#))
              (clojure.core/== (int    (.scale   ^Currency  ca#)) (int  (.scale   ^Currency cb#)))
              (clojure.core/== (long   (.numeric ^Currency  ca#)) (long (.numeric ^Currency cb#)))
              (clojure.core/identical? (.domain  ^Currency  ca#)        (.domain  ^Currency cb#))
              (clojure.core/identical? (.kind    ^Currency  ca#)        (.kind    ^Currency cb#))))))

(defmacro same-currency-objs?
  "Returns true if both Currency objects get from .currency field of Money objects are
  the same for Money operations.

  Currency weight is ignored."
  {:private true :added "2.0.0"}
  [ma mb]
  `(let [^Money     ma# ~ma
         ^Money     mb# ~mb
         ^Currency  ca# (.currency ma#)
         ^Currency  cb# (.currency mb#)]
     (or (clojure.core/identical? ca# cb#)
         (and (clojure.core/identical? (.id      ^Currency  ca#)        (.id      ^Currency cb#))
              (clojure.core/== (int    (.scale   ^Currency  ca#)) (int  (.scale   ^Currency cb#)))
              (clojure.core/== (long   (.numeric ^Currency  ca#)) (long (.numeric ^Currency cb#)))
              (clojure.core/identical? (.domain  ^Currency  ca#)        (.domain  ^Currency cb#))
              (clojure.core/identical? (.kind    ^Currency  ca#)        (.kind    ^Currency cb#))))))

;;
;; Money generation macros.
;;

(defn- sep-char?
  {:private true :tag Boolean :added "2.0.0"}
  [^Character ch]
  (or (clojure.core/= ch \_)
      (Character/isWhitespace ^Character ch)))

(defn- digit-or-dot?
  {:private true :tag Boolean :added "2.0.0"}
  [^Character ch]
  (or (clojure.core/<= 48 (int ch) 57)
      (clojure.core/= ch \.)))

(defn- sign-char?
  {:private true :tag Boolean :added "2.0.0"}
  [^Character ch]
  (or (clojure.core/= ch \-)
      (clojure.core/= ch \+)))

(defn- skip-seps
  {:private true :tag 'long :added "2.0.0"}
  ^long [^String s ^long i]
  (let [len (long (.length s))]
    (loop [i i]
      (if (and (clojure.core/< i len) (sep-char? (.charAt s i)))
        (recur (inc i))
        i))))

(defn- numeric-start?
  {:private true :tag Boolean :added "2.0.0"}
  [^String s ^long i]
  (let [len (long (.length s))
        i   (skip-seps s i)]
    (if (clojure.core/>= i len)
      false
      (let [ch (.charAt s i)]
        (cond
          (digit-or-dot? ch)
          true

          (sign-char? ch)
          (let [j (skip-seps s (inc i))]
            (and (clojure.core/< j len)
                 (digit-or-dot? (.charAt s j))))

          :else
          false)))))

(defn- parse-number
  "Parses a numeric token from `s` starting at `i` (allowing separators inside only
  when followed by more digits/dots). Returns [number-str end-index]."
  {:private true :tag clojure.lang.PersistentVector :added "2.0.0"}
  [^String s ^long i]
  (let [len               (long (.length s))
        ^StringBuilder sb (StringBuilder.)]
    (loop [i        (skip-seps s i)
           sign-ok? true]
      (if (clojure.core/>= i len)
        [(when (clojure.core/pos? (.length sb)) (.toString sb)) i]
        (let [ch (.charAt s i)]
          (cond
            (and sign-ok? (clojure.core/zero? (.length sb)) (sign-char? ch))
            (do (.append sb ch)
                (recur (skip-seps s (inc i)) false))

            (digit-or-dot? ch)
            (do (.append sb ch)
                (recur (inc i) false))

            (sep-char? ch)
            (let [j (skip-seps s (inc i))]
              (if (and (clojure.core/< j len) (digit-or-dot? (.charAt s j)))
                (recur j sign-ok?)
                [(when (clojure.core/pos? (.length sb)) (.toString sb)) i]))

            :else
            [(when (clojure.core/pos? (.length sb)) (.toString sb)) i]))))))

(defn- remainder->token
  {:private true :tag String :added "2.0.0"}
  ^String [^String s ^long i]
  (let [len               (long (.length s))
        ^StringBuilder sb (StringBuilder.)]
    (loop [i (skip-seps s i)]
      (if (clojure.core/>= i len)
        (when (clojure.core/pos? (.length sb)) (.toString sb))
        (let [ch (.charAt s i)]
          (if (sep-char? ch)
            (recur (inc i))
            (do (.append sb ch)
                (recur (inc i)))))))))

(defn- split-currency-first
  "Splits `s` into [currency-str amount-str] assuming it starts with a currency token.

  Plus/minus is treated as a numeric sign only when separated from the currency by a
  separator (whitespace/_)."
  {:private true :tag clojure.lang.PersistentVector :added "2.0.0"}
  [^String s]
  (let [len               (long (.length s))
        ^StringBuilder sb (StringBuilder.)]
    (loop [i        0
           saw-cur? false]
      (if (clojure.core/>= i len)
        [(when (clojure.core/pos? (.length sb)) (.toString sb)) nil]
        (let [ch (.charAt s i)]
          (cond
            (sep-char? ch)
            (recur (inc i) saw-cur?)

            ;; Amount starts with a digit/dot, but never right after a non-sign '-'/'+'.
            (and saw-cur?
                 (digit-or-dot? ch)
                 (not (and (clojure.core/pos? i)
                           (sign-char? (.charAt s (dec i)))
                           (not (sep-char? (.charAt s (dec i)))))))
            (let [[num _] (parse-number s i)]
              [(when (clojure.core/pos? (.length sb)) (.toString sb)) num])

            ;; Amount starts with a sign only when preceded by a separator.
            (and saw-cur?
                 (sign-char? ch)
                 (clojure.core/pos? i)
                 (sep-char? (.charAt s (dec i)))
                 (numeric-start? s i))
            (let [[num _] (parse-number s i)]
              [(when (clojure.core/pos? (.length sb)) (.toString sb)) num])

            :else
            (do (.append sb ch)
                (recur (inc i) true))))))))

(defn mk-bigdec
  {:added "1.0.6" :private true}
  [n]
  (if (number? n) (bigdec n) n))

(defn currency+amount
  "Splits the amount and currency."
  {:private true :tag clojure.lang.PersistentVector :added "1.0.0"}
  [amount]
  (let [^String s (if (keyword? amount)
                    (if-some [n (namespace amount)]
                      (str n "/" (name amount))
                      (name amount))
                    (if (number? amount)
                      (.toPlainString (bigdec amount))
                      (str amount)))
        len (long (.length s))]
    (if (clojure.core/zero? len)
      [nil nil]
      (let [i0 (skip-seps s 0)]
        (if (clojure.core/>= i0 len)
          [nil nil]
          (if (numeric-start? s i0)
            ;; amount first: 100EUR, -100 EUR, 1_000.00 PLN
            (let [[num end] (parse-number s i0)
                  cur       (remainder->token s end)]
              [cur num])
            ;; currency first: EUR100, EUR 100, crypto/ETH1.0, BSC-USD
            (let [slash (.indexOf s "/")]
              (if (clojure.core/neg? slash)
                (split-currency-first s)
                ;; Protect namespace part (may contain digits/hyphens) from being treated as an amount.
                (let [prefix (subs s 0 (inc slash))
                      name   (subs s (inc slash))
                      [c a]  (split-currency-first name)
                      c      (when c (str prefix c))]
                  [c a])))))))))

(defmacro currency-unit-strict
  "Internal currency coercion helper for money parsing/constructors.

  - For maps, treats the value as a currency specification and creates an ad-hoc
    `Currency` using `io.randomseed.bankster.currency/new-currency`.
  - For other non-numeric inputs, uses `io.randomseed.bankster.currency/unit` (registry
    lookup).

  Returns `nil` for numeric inputs."
  {:tag Currency :added "1.0.0" :private true}
  [a]
  `(let [na# ~a]
     (when-not (number? na#)
       (cond
         (map? na#)
         (currency/to-currency na#)

         :else
         (currency/unit na#)))))

(defn parse-int
  "Internal parser with amount modifier."
  {:tag Money :private true :added "1.0.0"}
  (^Money [^clojure.lang.IFn mod-fn amount]
   (when (some? amount)
     (if (instance? Money amount)
       (parse-int mod-fn ^Currency (.currency ^Money amount) ^BigDecimal (.amount ^Money amount))
       (if-some [^Currency cur (currency-unit-strict amount)]
         (parse-int mod-fn cur 0M)
         (let [[cur am] (currency+amount amount)
               am       (or am 0M)]
           (if (nil? cur)
             (if-some [d currency/*default*]
               (parse-int mod-fn d am)
               ;; Preserve the existing exception shape thrown by the 2-arity
               ;; variant (strict: explicit currency `nil` is not allowed).
               (parse-int mod-fn nil am))
             (parse-int mod-fn cur am)))))))
  (^Money [mod-fn currency amount]
   (when (some? amount)
     (if-some [^Currency c (currency-unit-strict currency)]
       (Money. ^Currency c ^BigDecimal (monetary-scale (mod-fn amount)
                                                       (long (.scale ^Currency c))))
       (throw (ex-info
               "Cannot create money amount without a valid currency."
               {:amount   amount
                :currency currency
                :default  currency/*default*})))))
  (^Money [mod-fn ^Currency currency amount ^RoundingMode rounding-mode]
   (when (some? amount)
     (if-some [^Currency c (currency-unit-strict currency)]
       (Money. ^Currency c ^BigDecimal (monetary-scale (mod-fn amount)
                                                       (long (.scale ^Currency c))
                                                       ^RoundingMode rounding-mode))
       (throw (ex-info
               "Cannot create money amount without a valid currency."
               {:amount   amount
                :currency currency
                :default  currency/*default*}))))))

(defn parse
  "Internal parser without amount modifier."
  {:tag Money :no-doc :true :added "1.0.0"}
  (^Money [amount] (parse-int identity amount))
  (^Money [currency amount] (parse-int identity currency amount))
  (^Money [currency amount ^RoundingMode rounding-mode] (parse-int identity currency amount rounding-mode)))

(defn parse-major
  "Internal parser with `scale/integer` as an amount modifier."
  {:tag Money :no-doc :true :added "1.0.0"}
  (^Money [amount] (parse-int scale/integer amount))
  (^Money [currency amount] (parse-int scale/integer currency amount))
  (^Money [currency amount ^RoundingMode rounding-mode] (parse-int scale/integer currency amount rounding-mode)))

(defn parse-minor
  "Internal parser with `scale/fractional` as an amount modifier."
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
         `(~fun ~cur# ~(mk-bigdec am#))))
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
             `(~fun ~cur# ~(mk-bigdec am#) (scale/post-parse-rounding ~rms#)))
           `(~fun ~cur# ~(mk-bigdec am#) ~b))))
     (let [b (if (number? b) b (currency/parse-currency-code b))]
       `(let [a# ~(mk-bigdec a) b# ~(mk-bigdec b)]
          (if (number? a#)
            (~fun b# a#)
            (when (number? b#)
              (~fun a# b#)))))))
  ([_fun a b rounding-mode]
   (let [rms# (scale/parse-rounding rounding-mode)]
     (if (or (ident? a) (string? a) (number? a))
       (let [[c# a#] (if (number? a) [b a] [a b])]
         `(~parse ~(currency/parse-currency-code c#) ~(mk-bigdec a#) (scale/post-parse-rounding ~rms#)))
       (let [b (if (number? b) b (currency/parse-currency-code b))]
         `(let [a# ~(mk-bigdec a) b# ~(mk-bigdec b)]
            (if (number? a#)
              (~parse b# a# (scale/post-parse-rounding ~rms#))
              (when (number? b#)
                (~parse a# b# (scale/post-parse-rounding ~rms#))))))))))

(defmacro of
  "Returns the amount of money as a Money object consisting of a currency and a
  value. Currency can be a currency object and for registered currencies: a keyword,
  a symbol or a string (e.g. `EUR`, `:EUR`, \"PLN\" or crypto/ETH), or even a
  number (for ISO-compliant currencies).

  The given amount can be any numeric value or a string that can be converted to
  `java.math.BigDecimal`.

  When a number must be downscaled to fulfill the number of decimal places for a
  currency, rounding mode must be given, which may be a symbol, a keyword or a string
  of the following:

  * `CEILING`     - rounds towards positive infinity.
  * `DOWN`        - rounds towards zero.
  * `FLOOR`       - rounds towards negative infinity.
  * `HALF_DOWN`   - rounds towards nearest neighbor unless both neighbors are equidistant, in which case rounds down.
  * `HALF_EVEN`   - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds towards the even.
  * `HALF_UP`     - rounds towards the nearest neighbor unless both neighbors are equidistant, and if so, rounds up.
  * `UP`          – rounds away from zero
  * `UNNECESSARY` - asserts that the requested operation has an exact result, hence no rounding is necessary.

  To create a monetary object using function, call `io.randomseed.bankster.money/value`.

  Be careful about using number literals for big-scale amounts (16–17 digits). Use
  either big decimal literals, e.g. 1234.45689101112M, or strings."
  ([]    (of-gen parse))
  ([a]   (of-gen parse a))
  ([a b] (of-gen parse a b))
  ([a b rounding-mode] (of-gen parse a b rounding-mode)))

(defmacro of-major
  "Like `io.randomseed.bankster.money/of` but sets the amount of major part only."
  ([]    (of-gen parse))
  ([a]   (of-gen parse-major a))
  ([a b] (of-gen parse-major a b))
  ([a b rounding-mode] (of-gen parse-major a b rounding-mode)))

(defmacro of-minor
  "Like `io.randomseed.bankster.money/of` but sets the amount of minor part only."
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
     (when (some? amount)
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (scale/apply amount (long (.scale ^BigDecimal (.amount ^Money money)))))))
    (^Money [money amount ^RoundingMode rounding]
     (when (some? amount)
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (scale/apply amount (long (.scale ^BigDecimal (.amount ^Money money))) rounding)))))

  (cast
    (^Money [money]
     money)
    (^Money [money currency]
     (let [^Currency cur (currency/unit currency)]
       (Money. ^Currency cur
               ^BigDecimal (monetary-scale (.amount ^Money money) (long (.scale ^Currency cur))))))
    (^Money [money currency ^RoundingMode rounding]
     (let [^Currency cur (currency/unit currency)]
       (Money. ^Currency cur
               ^BigDecimal (monetary-scale (.amount ^Money money) (long (.scale ^Currency cur)) rounding)))))

  Currency

  (value
    (^Money [currency]
     (Money. ^Currency currency
             ^BigDecimal (monetary-scale 0M (long (.scale ^Currency currency)))))
    (^Money [currency amount]
     (when (some? amount)
       (Money. ^Currency currency
               ^BigDecimal (monetary-scale (scale/amount amount) (long (.scale ^Currency currency))))))
    (^Money [currency amount ^RoundingMode rounding]
     (when (some? amount)
       (Money. ^Currency currency
               ^BigDecimal (monetary-scale (scale/amount amount) (long (.scale ^Currency currency)) rounding)))))

  (cast
    (^Money [currency]
     (value currency))
    (^Money [currency ^Money money]
     (let [^Currency cur (currency/unit currency)]
       (Money. ^Currency   cur
               ^BigDecimal (monetary-scale (.amount ^Money money) (long (.scale ^Currency cur))))))
    (^Money [currency ^Money money ^RoundingMode rounding]
     (let [^Currency cur (currency/unit currency)]
       (Money. ^Currency   cur
               ^BigDecimal (monetary-scale (.amount ^Money money) (long (.scale ^Currency cur)) rounding)))))

  clojure.lang.Symbol

  (value
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id amount]          (value (currency/unit currency-id) amount))
    (^Money [currency-id amount rounding] (value (currency/unit currency-id) amount rounding)))

  (cast
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id money]           (cast  (currency/unit currency-id) money))
    (^Money [currency-id money rounding]  (cast  (currency/unit currency-id) money rounding)))

  clojure.lang.Keyword

  (value
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id amount]          (value (currency/unit currency-id) amount))
    (^Money [currency-id amount rounding] (value (currency/unit currency-id) amount rounding)))

  (cast
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id money]           (cast (currency/unit currency-id) money))
    (^Money [currency-id money rounding]  (cast (currency/unit currency-id) money rounding)))

  String

  (value
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id amount]          (value (currency/unit currency-id) amount))
    (^Money [currency-id amount rounding] (value (currency/unit currency-id) amount rounding)))

  (cast
    (^Money [currency-id]                 (value (currency/unit currency-id)))
    (^Money [currency-id money]           (cast  (currency/unit currency-id) money))
    (^Money [currency-id money rounding]  (cast  (currency/unit currency-id) money rounding)))

  Number

  (value
    (^Money [amount]                      (value (currency/unit currency/*default*) amount))
    (^Money [amount currency-id]          (value (currency/unit currency-id) amount))
    (^Money [amount currency-id rounding] (value (currency/unit currency-id) amount rounding)))

  (cast
    (^Money [amount]                      (value (currency/unit currency/*default*) amount))
    (^Money [currency-id money]           (cast  (currency/unit currency-id) money))
    (^Money [currency-id money rounding]  (cast  (currency/unit currency-id) money rounding)))

  nil

  (value
    ([_]     nil)
    ([_ b]   (when (some? b) (parse b)))
    ([_ c r] (when (some? c) (parse c nil r))))

  (cast
    ([_]     nil)
    ([_ _]   nil)
    ([_ _ _] nil))

  clojure.lang.Sequential

  (value
    (^Money [v]     (clojure.core/apply parse (first v) (rest v)))
    (^Money [v r]   (value (first v) (second v) r))
    (^Money [v b r] (value (first v) b r)))

  (cast
    (^Money [v]     (clojure.core/apply cast (first v) (rest v)))
    (^Money [v r]   (cast (first v) (second v) r))
    (^Money [v b r] (cast (first v) b r)))

  Object

  (value
    (^Money [a]     (parse a))
    (^Money [a c]   (when (some? c) (parse a c)))
    (^Money [a c r] (when (some? c) (parse a c r))))

  (cast
    (^Money [a]     (cast (parse a)))
    (^Money [a c]   (cast (parse a) c))
    (^Money [a c r] (cast (parse a) c r))))

(defn major-value
  "Creates new `Money` object for the given value which will become a major part of the
  amount. If the given number has fractional part it will be truncated. If the
  currency is not given, it will try to use the default one, taken from the
  `io.randomseed.bankster.currency/*default*` dynamic variable. Optional
  rounding-mode should be a rounding mode used when the conversion to a scaled
  monetary amount requires rounding.

  For simple money creation the following macros may be a convenient way to go: of,
  of-major, of-minor."
  (^Money [a]     (when-some [v (value a)] (major-value (.currency ^Money v) (.amount ^Money v))))
  (^Money [currency amount]   (value currency (scale/integer amount)))
  (^Money [currency amount rounding-mode] (value currency (scale/integer amount) rounding-mode)))

(defn minor-value
  "Creates new `Money` object for the given value which will become a minor part of the
  amount. If the given number has fractional part it will be truncated. If the
  currency is not given, it will try to use the default one, taken from the
  `io.randomseed.bankster.currency/*default*` dynamic variable. Optional
  rounding-mode should be a rounding mode used when the conversion to a scaled
  monetary amount requires rounding.

  For simple money creation the following macros may be a convenient way to go: of,
  of-major, of-minor."
  (^Money [a]     (when-some [v (value a)] (minor-value (.currency ^Money v) (.amount ^Money v))))
  (^Money [currency amount]   (value currency (scale/fractional amount)))
  (^Money [currency amount rounding-mode] (value currency (scale/fractional amount) rounding-mode)))

;;
;; Ensuring the right registry is used.
;;

(defn of-registry
  "Ensures that a currency of the given money originates from the given registry. If
  the registry is not given and a dynamic registry is not set, the default one is
  used. Rescales the amount if needed to match the nominal scale. Optional
  rounding-mode can be supplied to be used when downscaling is needed (nominal
  currency from a registry has lower number of decimal places than the amount of
  money).

  Money can be expressed as a `Money` object or any other object that will create
  `Money` when passed to the `value` function. Returns money."
  {:tag Money :added "1.1.2"}
  (^Money [money]
   (of-registry (registry/get) money))
  (^Money [^Registry registry money]
   (let [^Money    money (value money)
         ^Currency cur   (currency/unit ^Currency (.id ^Currency (.currency ^Money money))
                                        ^Registry registry)]
     (Money. ^Currency   cur
             ^BigDecimal (monetary-scale (.amount ^Money money)
                                         (long (.scale ^Currency cur))))))
  (^Money [^Registry registry money ^RoundingMode rounding-mode]
   (let [^Money    money (value money)
         ^Currency cur   (currency/unit ^Currency (.id ^Currency (.currency ^Money money))
                                        ^Registry registry)]
     (Money. ^Currency   cur
             ^BigDecimal (monetary-scale (.amount ^Money money)
                                         (long (.scale ^Currency cur))
                                         rounding-mode)))))

;;
;; Operating on an amount.
;;

(defn on-amount
  "Performs an operation expressed with a function `f` on an amount of the given
  money. Additional arguments will be passed to the `f`. Returns the money with the
  amount updated. The function `f` must return a number. Short-circuits on `nil` as
  an argument. Rescales the result to the existing scale of an amount, not the
  nominal scale of a currency."
  {:tag Money :added "1.1.2"}
  (^Money [money f]
   (when-some [money (value money)]
     (let [^BigDecimal am (.amount ^Money money)]
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (monetary-scale (f am)
                                           (long (.scale ^BigDecimal am)))))))
  (^Money [money f a]
   (when-some [money (value money)]
     (let [^BigDecimal am (.amount ^Money money)]
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (monetary-scale (f am a)
                                           (long (.scale ^BigDecimal am)))))))
  (^Money [money f a b]
   (when-some [money (value money)]
     (let [^BigDecimal am (.amount ^Money money)]
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (monetary-scale (f am a b)
                                           (long (.scale ^BigDecimal am)))))))
  (^Money [money f a b & more]
   (when-some [money (value money)]
     (let [^BigDecimal am (.amount ^Money money)]
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (monetary-scale (clojure.core/apply f am a b more)
                                           (long (.scale ^BigDecimal am))))))))

(def ^{:tag      Money :added "1.2.7"
       :arglists '([money f]
                   [money f a]
                   [money f a b]
                   [money f a b & more])}
  apply
  "Alias for on-amount."
  on-amount)

(defn set-amount
  "Sets the amount of the given monetary object. Rescales value `v` to a scale of the
  given monetary object, not the nominal scale of its currency."
  {:tag Money :added "1.2.7"}
  (^Money [money v]
   (when-some [money (value money)]
     (let [^BigDecimal v (scale/apply v)]
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (monetary-scale ^BigDecimal v
                                           (long (.scale ^BigDecimal (.amount ^Money money))))))))
  (^Money [money v ^RoundingMode rounding-mode]
   (when-some [money (value money)]
     (let [^BigDecimal v (scale/apply v)]
       (Money. ^Currency   (.currency ^Money money)
               ^BigDecimal (monetary-scale ^BigDecimal v
                                           (long (.scale ^BigDecimal (.amount ^Money money)))
                                           ^RoundingMode rounding-mode))))))

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
  "Returns the amount of the given money. For more than one argument the money is
  created ad-hoc using a and b objects passed to the function named value."
  {:tag BigDecimal :added "1.0.0"}
  (^BigDecimal [money] (when-some [m (value money)] (.amount ^Money m)))
  (^BigDecimal [a b]   (when-some [m (value a b)]   (.amount ^Money m)))
  (^BigDecimal [a b r] (when-some [m (value a b r)] (.amount ^Money m))))

(defn currency
  "Returns the currency of the given money. For more than one argument the money is
  created ad-hoc using a and b objects passed to the function named value."
  {:tag Currency :added "1.0.0"}
  (^Currency [money] (when-some [m (value money)] (.currency ^Money m)))
  (^Currency [a b]   (when-some [m (value a b)]   (.currency ^Money m)))
  (^Currency [a b r] (when-some [m (value a b r)] (.currency ^Money m))))

(defn stripped-amount
  "Returns the amount of the given money with trailing zeros removed. For more than one
  argument the money is created ad-hoc using a and b objects passed to the function
  named value."
  {:tag BigDecimal :added "1.0.0"}
  (^BigDecimal [money] (when-some [m (value money)] (.stripTrailingZeros ^BigDecimal (.amount ^Money m))))
  (^BigDecimal [a b]   (when-some [m (value a b)]   (.stripTrailingZeros ^BigDecimal (.amount ^Money m))))
  (^BigDecimal [a b r] (when-some [m (value a b r)] (.stripTrailingZeros ^BigDecimal (.amount ^Money m)))))

(defn unparse
  "Returns a vector with symbolic representations of amount and currency. Useful for
  printing to EDN or displaying on a console. The letter M will be added to the
  amount if its precision exceeds 15. For more than one argument the money is created
  ad-hoc using a and b objects passed to the function named value."
  {:tag clojure.lang.IPersistentVector :added "1.0.7"}
  ([money]
   (when-some [m (value money)]
     [(scale/to-clojure-symbol ^BigDecimal (.amount ^Money m))
      (symbol (currency/id ^Currency (.currency ^Money m)))]))
  ([a b]
   (when-some [m (value a b)]
     [(scale/to-clojure-symbol ^BigDecimal (.amount ^Money m))
      (symbol (currency/id ^Currency (.currency ^Money m)))]))
  ([a b r]
   (when-some [m (value a b r)]
     [(scale/to-clojure-symbol ^BigDecimal (.amount ^Money m))
      (symbol (currency/id ^Currency (.currency ^Money m)))])))

;;
;; Monetary implementation.
;;

(extend-protocol currency/Monetary

  Money

  (to-id
    ^clojure.lang.Keyword [money]
    (currency/to-id ^Currency (.currency ^Money money)))

  (to-code
    ^clojure.lang.Keyword [money]
    (currency/to-code ^Currency (.currency ^Money money)))

  (to-id-str
    ^String [money]
    (currency/to-id-str ^Currency (.currency ^Money money)))

  (to-code-str
    ^String [money]
    (currency/to-code-str ^Currency (.currency ^Money money)))

  (to-numeric-id
    ^long [money]
    (currency/to-numeric-id ^Currency (.currency ^Money money)))

  (to-currency
    ^Currency [money]
    (.currency ^Money money))

  (to-map
    ^clojure.lang.IPersistentMap [money]
    {:currency (currency/id ^Currency (.currency ^Money money))
     :amount   ^BigDecimal (.amount ^Money money)})

  (definitive?
    (^Boolean [money]
     (currency/definitive? ^Currency (.currency ^Money money))))

  (resolve
    (^Currency [money]
     (currency/resolve ^Currency (.currency ^Money money)))
    (^Currency [money ^Registry registry]
     (currency/resolve ^Currency (.currency ^Money money) ^Registry registry)))

  (resolve-all
    (^clojure.lang.IPersistentSet [money]
     (currency/resolve-all ^Currency (.currency ^Money money)))
    (^clojure.lang.IPersistentSet [money ^Registry registry]
     (currency/resolve-all ^Currency (.currency ^Money money) ^Registry registry)))

  (of-id
    (^Currency [money]
     (currency/of-id ^Currency (.currency ^Money money)))
    (^Currency [money ^Registry registry]
     (currency/of-id ^Currency (.currency ^Money money) ^Registry registry)))

  (unit
    (^Currency [money]
     (currency/unit ^Currency (.currency ^Money money)))
    (^Currency [money ^Registry registry]
     (currency/unit ^Currency (.currency ^Money money) ^Registry registry)))

  (id
    (^clojure.lang.Keyword [money]
     (.id ^Currency (.currency ^Money money)))
    (^clojure.lang.Keyword [money ^Registry _registry]
     (.id ^Currency (.currency ^Money money))))

  (defined?
    (^Boolean [money]
     (contains? (registry/currency-id->currency*)
                (.id ^Currency (.currency ^Money money))))
    (^Boolean [money ^Registry registry]
     (contains? (registry/currency-id->currency* registry)
                (.id ^Currency (.currency ^Money money)))))

  (present?
    (^Boolean [money]
     (currency/present? ^Currency (.currency ^Money money)))
    (^Boolean [money ^Registry registry]
     (currency/present? ^Currency (.currency ^Money money) ^Registry registry)))
  )

;;
;; Scalable implementation.
;;

(extend-protocol scale/Scalable

  Money

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] true)

  (of [m] (.scale ^BigDecimal (.amount ^Money m)))

  (apply
    (^Money [m]
     (let [^Currency cur (.currency ^Money m)
           sc            (int (.scale ^Currency cur))]
       (if (currency/val-auto-scaled*? sc) m
           (Money. ^Currency cur
                   (bd-set-scale (.amount ^Money m)
                                 sc
                                 (scale/rounding-mode)
                                 :scale/apply
                                 {:money m :currency cur})))))
    (^Money [m ^long scale]
     (let [^Currency cur (.currency ^Money m)]
       (Money. ^Currency   cur
               (bd-set-scale (.amount ^Money m)
                             (int scale)
                             (scale/rounding-mode)
                             :scale/apply
                             {:money m :currency cur}))))
    (^Money [m ^long scale ^RoundingMode rounding-mode]
     (let [^Currency cur (.currency ^Money m)]
       (Money. ^Currency   cur
               (bd-set-scale (.amount ^Money m)
                             (int scale)
                             rounding-mode
                             :scale/apply
                             {:money m :currency cur})))))

  (amount
    (^BigDecimal [money]
     (.amount ^Money money))
    (^BigDecimal [money ^long scale]
     (bd-set-scale (.amount ^Money money)
                   (int scale)
                   (scale/rounding-mode)
                   :scale/apply
                   {:money money}))
    (^BigDecimal [money ^long scale ^RoundingMode r]
     (bd-set-scale (.amount ^Money money)
                   (int scale)
                   r
                   :scale/apply
                   {:money money}))))

;;
;; Comparators.
;;

(declare same-currencies?)

(defn compare-amounts
  "Compares two monetary amounts of the same currency, regardless of their
  scales. Returns -1 if the first one is less than the second, 0 if equal to, and 1
  if it is greater than the second. Nil values are always considered lower when
  comparing."
  {:added "1.0.0"}
  (^long [^Money _] (unchecked-long 0))
  (^long [^Money a ^Money b]
   (let [nila (nil? a)
         nilb (nil? b)]
     (when-not (or nila nilb (same-currency-objs? ^Money a ^Money b))
       (throw
        (ex-info
         (str "Can only compare amounts of the same currency and/or nil values: "
              a ", " b)
         {:money-1 a :money-2 b})))
     (if nila
       (if nilb
         (unchecked-long 0)
         (unchecked-long -1))
       (if nilb
         (unchecked-long 1)
         (unchecked-long (.compareTo ^BigDecimal (.amount ^Money a) ^BigDecimal (.amount ^Money b))))))))

(defn compare
  "Compares two monetary amounts of the same currency and scale. Returns -1 if the
  first one is less than the second, 0 if equal to, and 1 if it is greater than the
  second. Nil values are always considered lower when comparing."
  {:added "1.0.0"}
  (^long [^Money _] (unchecked-long 0))
  (^long [^Money a ^Money b]
   (let [nila (nil? a)
         nilb (nil? b)]
     (when-not (or nila nilb (same-currency-objs? ^Money a ^Money b))
       (throw
        (ex-info
         (str "Can only compare amounts of the same currency and/or nil values: "
              a ", " b)
         {:money-1 a :money-2 b})))
     (if nila
       (if nilb
         (unchecked-long 0)
         (unchecked-long -1))
       (if nilb
         (unchecked-long 1)
         (let [^BigDecimal am-a (.amount ^Money a)
               ^BigDecimal am-b (.amount ^Money b)]
           (when-not (clojure.core/== (.scale ^BigDecimal am-a) (.scale ^BigDecimal am-b))
             (throw
              (ex-info
               (str "Cannot compare monetary amounts having different decimal scales: "
                    a " (" (.scale ^BigDecimal am-a) "), "
                    b " (" (.scale ^BigDecimal am-b) ").")
               {:money-1 a
                :money-2 b
                :scale-1 (.scale ^BigDecimal am-a)
                :scale-2 (.scale ^BigDecimal am-b)})))
           (unchecked-long (.compareTo ^BigDecimal am-a ^BigDecimal am-b))))))))

;;
;; Predicates.
;;

(defn money?
  "Returns true if the given value is a kind of Money."
  {:tag Boolean :added "1.0.0"}
  [a]
  (instance? Money a))

(defn rescaled?
  "Returns `true` if the given monetary value has different scale than its currency (so
  it was rescaled). Returns `false` if the scale is no different. Returns `nil` if
  the currency does not have a fixed scale."
  {:added "1.0.0"}
  [a]
  (let [csc (int (.scale ^Currency (.currency ^Money a)))]
    (when-not (currency/val-auto-scaled*? csc)
      (not (clojure.core/== csc (.scale ^BigDecimal (.amount ^Money a)))))))

(defn same-currencies?
  "Returns `true` if both currencies are the same for the given money objects.

  Note: currency weight is ignored."
  {:tag Boolean :added "1.0.0"}
  [^Money a ^Money b]
  (same-currency-objs? a b))

(defn same-currency-ids?
  "Returns `true` if both currencies have the same IDs for the given money objects."
  {:tag Boolean :added "1.0.0"}
  [^Money a ^Money b]
  (clojure.core/identical? (.id ^Currency (.currency ^Money a))
                           (.id ^Currency (.currency ^Money b))))

(defn eq?
  "Returns true if the money amounts and their currencies are equal. Note that
  currencies with different scales or other properties are considered different. Use
  eq-am? (aliased as ==) to compare amounts regardless of their scales."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] true)
  (^Boolean [^Money a ^Money b]
   (and (.equals ^BigDecimal (.amount ^Money a) ^BigDecimal (.amount ^Money b))
        (same-currency-objs? a b)))
  (^Boolean [^Money a ^Money b & more]
   (if (eq? a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq? b (first more)))
     false)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  =
  "Alias for eq?."
  eq?)

(defn eq-am?
  "Returns true if the monetary amounts and their currencies are equal, regardless of
  their scales."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] true)
  (^Boolean [^Money a ^Money b]
   (if-not (same-currency-objs? a b)
     false
     (let [^BigDecimal am-a (.amount ^Money a)
           ^BigDecimal am-b (.amount ^Money b)
           sa               (int (.scale am-a))
           sb               (int (.scale am-b))]
       (if (clojure.core/== sa sb)
         (.equals ^BigDecimal am-a ^BigDecimal am-b)
         (if (clojure.core/< sa sb)
           (.equals ^BigDecimal (scale/apply am-a sb) ^BigDecimal am-b)
           (.equals ^BigDecimal am-a ^BigDecimal (scale/apply am-b sa)))))))
  (^Boolean [^Money a ^Money b & more]
   (if (eq-am? a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq-am? b (first more)))
     false)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  ==
  "Alias for eq-am?."
  eq-am?)

(defn ne?
  "Returns true if the money amounts or their currencies are different, regardless of
  their scales. Note that currencies with different scales are considered different."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] false)
  (^Boolean [^Money a ^Money b]
   (not (eq? ^Money a ^Money b)))
  (^Boolean [^Money a ^Money b & more]
   (not (clojure.core/apply eq? ^Money a ^Money b more))))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  not=
  "Alias for ne?."
  ne?)

(defn ne-am?
  "Returns true if the money amounts or their currencies are different, regardless of
  their scales."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] false)
  (^Boolean [^Money a ^Money b]
   (not (eq-am? ^Money a ^Money b)))
  (^Boolean [^Money a ^Money b & more]
   (not (clojure.core/apply eq-am? ^Money a ^Money b more))))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  not==
  "Alias for ne-am?."
  ne-am?)

(defn gt?
  "Returns non-nil if monetary amounts are in monotonically decreasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] true)
  (^Boolean [^Money a ^Money b]
   (pos-int? (compare-amounts a b)))
  (^Boolean [^Money a ^Money b & more]
   (if (gt? a b)
     (if (next more)
       (recur b (first more) (next more))
       (gt? b (first more)))
     false)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  >
  "Alias for gt?."
  gt?)

(defn ge?
  "Returns non-nil if monetary amounts are in monotonically non-increasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] true)
  (^Boolean [^Money a ^Money b]
   (clojure.core/>= (compare-amounts a b) 0))
  (^Boolean [^Money a ^Money b & more]
   (if (ge? a b)
     (if (next more)
       (recur b (first more) (next more))
       (ge? b (first more)))
     false)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  >=
  "Alias for ge?."
  ge?)

(defn lt?
  "Returns non-nil if monetary amounts are in monotonically increasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] true)
  (^Boolean [^Money a ^Money b]
   (neg-int? (compare-amounts a b)))
  (^Boolean [^Money a ^Money b & more]
   (if (lt? a b)
     (if (next more)
       (recur b (first more) (next more))
       (lt? b (first more)))
     false)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  <
  "Alias for lt?."
  lt?)

(defn le?
  "Returns non-nil if monetary amounts are in monotonically non-decreasing order,
  otherwise false."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^Money _] true)
  (^Boolean [^Money a ^Money b]
   (clojure.core/<= (compare-amounts a b) 0))
  (^Boolean [^Money a ^Money b & more]
   (if (le? a b)
     (if (next more)
       (recur b (first more) (next more))
       (le? b (first more)))
     false)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a]
                   ^Boolean [^Money a ^Money b]
                   ^Boolean [^Money a ^Money b & more])}
  <=
  "Alias for le?."
  le?)

(defn is-zero?
  "Returns true if the given monetary amount is zero."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (clojure.core/zero? (.compareTo ^BigDecimal (.amount ^Money a) 0M)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a])}
  zero?
  "Alias for is-zero?."
  is-zero?)

(defn is-neg?
  "Returns true if the given monetary amount is a negative number."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (neg-int? (.compareTo ^BigDecimal (.amount ^Money a) 0M)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a])}
  neg?
  "Alias for is-neg?."
  is-neg?)

(defn is-pos?
  "Returns true if the given monetary amount is a positive number."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (pos-int? (.compareTo ^BigDecimal (.amount ^Money a) 0M)))


(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a])}
  pos?
  "Alias for is-pos?."
  is-pos?)

(defn is-neg-or-zero?
  "Returns true if the given monetary amount is a negative number or zero."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (clojure.core/<= (.compareTo ^BigDecimal (.amount ^Money a) 0M) 0))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a])}
  neg-or-zero?
  "Alias for is-neg-or-zero?."
  is-neg-or-zero?)

(defn is-pos-or-zero?
  "Returns true if the given monetary amount is a positive number or zero."
  {:tag Boolean :added "1.0.0"}
  [^Money a]
  (clojure.core/>= (.compareTo ^BigDecimal (.amount ^Money a) 0M) 0))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Boolean [^Money a])}
  pos-or-zero?
  "Alias for is-pos-or-zero?."
  is-pos-or-zero?)

;;
;; Operations.
;;

(defn scale
  "Re-scales the given money using a scale (number of decimal places) and an optional
  rounding mode (required when downscaling). The internal scale for a currency object
  is NOT updated. If no scale is given, returns a current scale of an amount (not
  the nominal scale of a currency – which is important in rescaled amount or
  auto-scaled currencies).

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
  "Same as scale but its unary variant will rescale an amount of the given money to
  conform it to its currency settings instead of returning the scale. It has the same
  effect as calling `io.randomseed.bankster.scale/apply` on a Money object."
  {:tag Money :added "1.0.0"}
  (^Money [^Money money]
   (scale/apply ^Money money))
  (^Money [^Money money ^long scale]
   (scale/apply ^Money money (int scale)))
  (^Money [^Money money ^long scale ^RoundingMode rounding-mode]
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
   (if (same-currency-objs? a b)
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
         fun             (fn [^BigDecimal a b]
                           (when-not (instance? Money b)
                             (throw (ex-info
                                     "Cannot add a regular number to the monetary amount."
                                     {:augend a :addend b})))
                           (when-not (same-currency-objs-native? cur-a (.currency ^Money b))
                             (throw (ex-info
                                     "Cannot add amounts of two different currencies."
                                     {:augend a :addend b})))
                           (.add ^BigDecimal a ^BigDecimal (.amount ^Money b)))]
     (Money. ^Currency   (.currency ^Money a)
             ^BigDecimal (reduce fun (fun (.amount ^Money a) b) more)))))

(def ^{:tag      Money :added "1.0.0"
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  add-scaled
  "Alias for add."
  add)

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Money []
                   ^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  +
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
   (if (same-currency-objs? a b)
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
         fun             (fn [^BigDecimal a b]
                           (when-not (instance? Money b)
                             (throw (ex-info
                                     "Cannot subtract a regular number from the monetary amount."
                                     {:minuend a :subtrahend b})))
                           (when-not (same-currency-objs-native? cur-a (.currency ^Money b))
                             (throw (ex-info
                                     "Cannot subtract amounts of two different currencies."
                                     {:minuend a :subtrahend b})))
                           (.subtract ^BigDecimal a ^BigDecimal (.amount ^Money b)))]
     (Money. ^Currency   (.currency ^Money a)
             ^BigDecimal (reduce fun (fun (.amount ^Money a) b) more)))))

(def ^{:tag      Money :added "1.0.0"
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  sub-scaled
  "Alias for sub."
  sub)

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  -
  "Alias for sub."
  sub)

(defmacro mul-core
  "Internal multiplier."
  {:added "1.0.0" :private true}
  ([a b scale r]
   `(let [^BigDecimal a#    ~a
          ^BigDecimal b#    (scale/apply ~b)
          sc#               (int ~scale)
          ^RoundingMode rm# ~r]
      (try
        (.setScale ^BigDecimal (.multiply ^BigDecimal a# ^BigDecimal b#) sc# rm#)
        (catch ArithmeticException e#
          (throw
           (ex-info (or (.getMessage e#) "Arithmetic error.")
                    {:op                         :mul
                     :multiplicant               a#
                     :multiplier                 b#
                     :scale                      sc#
                     :rounding-mode              rm#
                     :arithmetic-exception       true
                     :arithmetic-exception/cause e#}
                    e#))))))
  ([a b scale r _]
   `(let [^BigDecimal a#    ~a
          ^BigDecimal b#    ~b
          sc#               (int ~scale)
          ^RoundingMode rm# ~r]
      (try
        (.setScale ^BigDecimal (.multiply ^BigDecimal a# ^BigDecimal b#) sc# rm#)
        (catch ArithmeticException e#
          (throw
           (ex-info (or (.getMessage e#) "Arithmetic error.")
                    {:op                         :mul
                     :multiplicant               a#
                     :multiplier                 b#
                     :scale                      sc#
                     :rounding-mode              rm#
                     :arithmetic-exception       true
                     :arithmetic-exception/cause e#}
                    e#))))))
  ([a b]
   `^BigDecimal (.multiply ^BigDecimal ~a ^BigDecimal (scale/apply ~b)))
  ([a b _]
   `^BigDecimal (.multiply ^BigDecimal ~a ^BigDecimal ~b)))

(declare mul)
(defrecord LastMoney [^Money m ^int scale ^Boolean auto-scaled])

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
   (let [am?              (instance? Money a)
         bm?              (instance? Money b)
         ^BigDecimal am   (if am? (.amount ^Money a) (scale/apply a))
         bm               (if bm? (.amount ^Money b) b)
         mon              (volatile! (when am? (LastMoney. ^Money a (int (.scale am)) ^Boolean (auto-scaled? ^Money a))))
         ^RoundingMode rm (or (scale/rounding-mode) scale/ROUND_UNNECESSARY)
         fun              (fn [^BigDecimal a b]
                            (if-let [^LastMoney m @mon]
                              (if (instance? Money b)
                                ;; money, money
                                (throw (ex-info "Only one multiplied value can be a kind of Money."
                                                {:multiplicant a :multiplier b}))
                                ;; money, number
                                (if (.auto-scaled ^LastMoney m)
                                  (mul-core ^BigDecimal a b)
                                  (mul-core ^BigDecimal a b (int (.scale ^LastMoney m)) ^RoundingMode rm)))
                              (if (instance? Money b)
                                ;; number, money
                                (let [^BigDecimal bm (.amount ^Money b)]
                                  (if (auto-scaled? ^Money b)
                                    (do (vreset! mon (LastMoney. ^Money b (int (.scale ^BigDecimal bm)) true))
                                        (mul-core ^BigDecimal a ^BigDecimal bm nil))
                                    (let [bsc (int (.scale ^BigDecimal bm))]
                                      (vreset! mon (LastMoney. ^Money b (int bsc) false))
                                      (mul-core ^BigDecimal a ^BigDecimal bm (int bsc) ^RoundingMode rm nil))))
                                ;; number, number
                                (mul-core ^BigDecimal a b))))
         ^BigDecimal res  (reduce fun (fun ^BigDecimal am bm) more)]
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
   (let [am?            (instance? Money a)
         bm?            (instance? Money b)
         ^BigDecimal am (if am?
                          ^BigDecimal (.amount ^Money a)
                          ^BigDecimal (scale/apply a))
         bm             (if bm? (.amount ^Money b) b)]
     (if am?
       (if bm?
         ;; money, money
         (throw (ex-info "Only one multiplied value can be a kind of Money."
                         {:multiplicant a :multiplier b}))
         ;; money, number
	 (let [^Currency c (.currency ^Money a)]
	   (if (currency-auto-scaled? ^Currency c)
	     (Money. ^Currency   c
	             (mul-core ^BigDecimal am bm))
	     (Money. ^Currency   c
	             (mul-core ^BigDecimal am bm
	                       (int (.scale ^BigDecimal am))
	                       (or ^RoundingMode (scale/rounding-mode)
	                           ^RoundingMode scale/ROUND_UNNECESSARY))))))
       (if bm?
         ;; number, money
	 (let [^Currency c (.currency ^Money b)]
	   (if (currency-auto-scaled? c)
	     (Money. ^Currency   (.currency ^Money b)
	             (mul-core ^BigDecimal am ^BigDecimal bm nil))
	     (Money. ^Currency   (.currency ^Money b)
	             (mul-core ^BigDecimal am ^BigDecimal bm
	                       (int (.scale ^BigDecimal bm))
	                       (or ^RoundingMode (scale/rounding-mode)
	                           ^RoundingMode scale/ROUND_UNNECESSARY)
	                       nil))))
         ;; number, number
         (mul-core ^BigDecimal am bm)))))
  ([a b & more]
   (if scale/*each*
     (clojure.core/apply mul-scaled a b more)
     (let [step                       (fn [[^BigDecimal acc ^Money m] x]
	                                (if (instance? Money x)
	                                  (if (some? m)
	                                    (throw (ex-info "Only one multiplied value can be a kind of Money."
	                                                    {:multiplicant m :multiplier x}))
	                                    [(mul-core ^BigDecimal acc ^BigDecimal (.amount ^Money x) nil)
	                                     ^Money x])
	                                  [(mul-core ^BigDecimal acc x)
	                                   m]))
	   [^BigDecimal res ^Money m] (reduce step [1M nil] (cons a (cons b more)))]
       (if (some? m)
         (let [^Currency c (.currency ^Money m)]
           (Money. ^Currency c
                   (if (currency-auto-scaled? c)
                     res
                     (bd-set-scale res
                                   (.scale ^BigDecimal (.amount ^Money m))
                                   (scale/rounding-mode)
                                   :mul
                                   {:money m :currency c}))))
         ^BigDecimal res)))))

(def ^{:tag      Money :added "1.2.0"
       :arglists '([]
                   [a]
                   [a b]
                   [a b & more])}
  *
  "Alias for mul."
  mul)

(defn div-core-fn
  "Performs division without rounding and rescaling."
  {:tag BigDecimal :added "1.0.0" :private true}
  ([^BigDecimal a b]
   (if (instance? Money b)
     (throw (ex-info "Cannot divide a regular number by the monetary amount."
                     {:dividend a :divisor b}))
     (let [^BigDecimal b (scale/apply b)]
       (try
         ^BigDecimal (.divide ^BigDecimal a ^BigDecimal b)
         (catch ArithmeticException e
           (throw
            (ex-info (or (.getMessage e) "Arithmetic error.")
                     {:op                         :div
                      :dividend                   a
                      :divisor                    b
                      :arithmetic-exception       true
                      :arithmetic-exception/cause e}
                     e))))))))

(defmacro div-core
  "In its binary variant it performs division without rounding and rescaling.

  In its ternary variant divides with precision calculated to fit the longest possible
  result (unless there is a non-terminating decimal expansion then the result will be
  rounded).

  In its quaternary variant divides with scale and rounding mode."
  {:added "1.0.0" :private true}
  ([a b scale r]
   `(let [a#  ^BigDecimal ~a
          b#  ^BigDecimal (scale/apply ~b)
          sc# (int ~scale)
          rm# ^RoundingMode ~r]
      (try
        ^BigDecimal (.divide ^BigDecimal a# ^BigDecimal b# sc# rm#)
        (catch ArithmeticException e#
          (throw
           (ex-info (or (.getMessage e#) "Arithmetic error.")
                    {:op                         :div
                     :dividend                   a#
                     :divisor                    b#
                     :scale                      sc#
                     :rounding-mode              rm#
                     :arithmetic-exception       true
                     :arithmetic-exception/cause e#}
                    e#))))))
  ([a b r]
   `(let [^BigDecimal a# (.stripTrailingZeros ^BigDecimal ~a)
          ^BigDecimal b# (.stripTrailingZeros ^BigDecimal (scale/apply ~b))]
      (try
        ^BigDecimal (.divide ^BigDecimal  a#
                             ^BigDecimal  b#
                             ^MathContext (scale/div-math-context
                                           ^BigDecimal   a#
                                           ^BigDecimal   b#
                                           ^RoundingMode ~r))
        (catch ArithmeticException e#
          (throw
           (ex-info (or (.getMessage e#) "Arithmetic error.")
                    {:op                         :div
                     :dividend                   a#
                     :divisor                    b#
                     :rounding-mode              ~r
                     :arithmetic-exception       true
                     :arithmetic-exception/cause e#}
                    e#))))))
  ([^BigDecimal a b]
   `(let [b# ~b]
      (if (instance? Money b#)
        (throw (ex-info "Cannot divide a regular number by the monetary amount."
                        {:dividend ~a :divisor b#}))
        (let [^BigDecimal a# (.stripTrailingZeros ^BigDecimal ~a)
              ^BigDecimal b# (.stripTrailingZeros ^BigDecimal (scale/apply b#))]
          (try
            ^BigDecimal (.divide ^BigDecimal a# ^BigDecimal b#)
            (catch ArithmeticException e#
              (throw
               (ex-info (or (.getMessage e#) "Arithmetic error.")
                        {:op                         :div
                         :dividend                   a#
                         :divisor                    b#
                         :arithmetic-exception       true
                         :arithmetic-exception/cause e#}
                        e#)))))))))

(declare div)

(defn div-scaled
  "Divides two or more amounts of money of the same currency or numbers. Scales and
  optionally rounds each intermediate result (if there are more divisors).

  If the first given argument is a kind of money and one of the divisors is also a
  kind of Money, the result will be a BigDecimal kind of number. If the earlier value
  is a kind of Money and the second is a number, the result will be a Money. If the
  first argument is a number and the second argument is a monetary amount, an
  exception will be thrown.

  For more than 2 arguments, if currency cancels out (Money divided by Money), the
  intermediate result becomes a regular number, so all consequent divisors must also
  be regular numbers.

  For a single value it returns a division of 1 by the given number. For a monetary
  value it will throw, because dividing a regular number by Money is not allowed.

  For more than 2 arguments it repeatedly divides their values as described and
  re-scales each result to match the scale of the first encountered monetary
  amount. If there are no monetary amounts involved the scale is calculated
  dynamically.

  Note that rescaling is not based on the original scale of a currency. This is
  designed that way is to handle cases when monetary amount was intentionally
  rescaled earlier.

  For regular numbers and for monetary values of auto-scaled currencies the dynamic
  rescaling is performed to handle corner cases where there would be a
  non-terminating decimal expansion. Trailing zeros are stripped before any
  calculation.

  If the scaling requires rounding then enclosing the expression within
  `with-rounding` (also present in `io.randomseed.bankster.scale` namespace) is
  required."
  {:added "1.0.0"}
  ([a]   (div a))
  ([a b] (div a b))
  ([a b & more]
   (let [^RoundingMode rm (scale/rounding-mode)]
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
             ^BigDecimal   am (.amount   ^Money a)
             ^Currency      c (.currency ^Money a)
             am-scale         (int (.scale ^BigDecimal am))
             bm?              (instance? Money b)
             bm               (if bm? ^BigDecimal (.amount ^Money b) b)]
         (if (currency-auto-scaled? ^Currency c)
           ;;
           ;; currency is auto-scaled
           ;;
           (loop [^BigDecimal x (div-core ^BigDecimal am bm  ^RoundingMode rm)
                  more          more]
             (if-some [y (first more)]
               ;; there is next element
               (if (instance? Money y)
                 ;; next is money
                 (if-not (same-currency-objs? a y)
                   (throw (ex-info "Cannot divide by the amount of a different currency."
                                   {:dividend a :divisor y}))
                   ;; first 2 are money - treat like a regular decimals
                   ;; stop iteration and reduce since we don't expect money
                   (reduce
                    (fn ^BigDecimal [^BigDecimal a b]
                      (when (instance? Money b)
                        (throw (ex-info "Cannot divide a regular number by the monetary amount."
                                        {:dividend a :divisor b})))
                      (div-core ^BigDecimal a b ^RoundingMode rm))
                    (div-core ^BigDecimal x ^BigDecimal (.amount ^Money y) ^RoundingMode rm)
                    (rest more)))
                 ;; second is a regular number, first is already a decimal
                 (recur (div-core ^BigDecimal x y ^RoundingMode rm) (rest more)))
               ;; no more data to process
               (if bm? x
                   (Money. ^Currency   (.currency ^Money a)
                           ^BigDecimal x))))
           ;;
           ;; currency is not auto-scaled
           ;;
           (loop [^BigDecimal x (div-core ^BigDecimal am bm
                                          (int (.scale ^BigDecimal am))
                                          ^RoundingMode rm)
                  more          more]
             (if-some [y (first more)]
               ;; there is next element
               (if (instance? Money y)
                 ;; next is money
                 (if-not (same-currency-objs? a y)
                   (throw (ex-info "Cannot divide by the amount of a different currency."
                                   {:dividend a :divisor y}))
                   ;; first 2 are money - treat like a regular decimals
                   ;; but scale each time to match the scale of first encountered money
                   ;; stop iteration and reduce since we don't expect money
                   (reduce
                    (fn ^BigDecimal [^BigDecimal a b]
                      (when (instance? Money b)
                        (throw (ex-info "Cannot divide a regular number by the monetary amount."
                                        {:dividend a :divisor b})))
                      (div-core ^BigDecimal a b (int am-scale) ^RoundingMode rm))
                    (div-core ^BigDecimal x ^BigDecimal
                              (.amount ^Money y) (int am-scale) ^RoundingMode rm)
                    (rest more)))
                 ;; second is a regular number, first is already a decimal
                 ;; loop and rescale to money with rounding
                 (recur (div-core ^BigDecimal x y (int am-scale) ^RoundingMode rm)
                        (rest more)))
               ;; no more data to process
               ;; if we have not reduced monetary value, rescale it to match the initial
               (if bm? x
                   (Money. ^Currency   (.currency ^Money a)
                           ^BigDecimal (scale/apply x am-scale rm)))))))))))

(defn div
  "Divides two or more amounts of money of the same currency or numbers. If two of the
  given values are a kind of Money, the result will be a BigDecimal number. If the
  earlier value is a kind of Money and the second is a number, the result will be a
  Money. If the first argument is a number and the second argument is a monetary
  amount, an exception will be thrown. The result will always be either a kind of
  Money or a BigDecimal number.

  For a single value it returns a division of 1 by the given number. For a monetary
  value it will throw, because dividing a regular number by Money is not allowed.

  For more than 2 arguments it repeatedly divides their values as described. If the
  previous calculations produce a regular number, all consequent divisors must be
  regular numbers. Practically, if the division begins with a monetary amount, only
  one monetary amount at some point later is permitted (which will cause the function
  to switch to the regular numbers calculation since the units will cancel themselves).

  Note that rescaling is not based on the original scale of a currency. This is
  designed that way is to handle cases when monetary amount was intentionally
  rescaled earlier.

  For regular numbers and for monetary values of auto-scaled currencies the dynamic
  rescaling is performed to handle corner cases where there would be a
  non-terminating decimal expansion. Trailing zeros are stripped before any
  calculation.

  Rescaling is performed after all calculations are done. However, the scale can be
  applied on each calculation if the dynamic variable
  `io.randomseed.bankster.scale/*each*` is set to a truthy value. If the scaling
  requires rounding then enclosing the expression within `with-rounding` is
  required. This can also be achieved by enclosing the expression within
  `io.randomseed.bankster.scale/with-rescaling` (aliased in this namespace under the
  same name) macro combining the switch and setting the scale."
  {:added "1.0.0"}
  ([a]
   (if (instance? Money a)
     (throw (ex-info "Cannot divide a regular number by the monetary amount."
                     {:dividend 1M :divisor a}))
     (div 1M a)))
  ([a b]
   (let [am?              (instance? Money a)
         bm?              (instance? Money b)
         ^RoundingMode rm (or (scale/rounding-mode) scale/ROUND_UNNECESSARY)
         ^BigDecimal am   (if am? (.amount ^Money a) (scale/apply a))
         bm               (if bm? (.amount ^Money b) b)]
     (if am?
       (let [^Currency c (.currency ^Money a)]
         (if bm?
           ;; money, money
           (if-not (same-currency-objs? a b)
             (throw (ex-info "Cannot divide by the amount of a different currency."
                             {:dividend a :divisor b}))
             (div-core ^BigDecimal am ^BigDecimal bm ^RoundingMode rm))
           ;; money, number
           (if (currency-auto-scaled? ^Currency c)
             (Money. ^Currency c
                     (div-core ^BigDecimal am ^BigDecimal bm ^RoundingMode rm))
             (let [^BigDecimal d (cond
                                   ;; Fast path: integral divisor preserves dividend scale, so we
                                   ;; can use BigDecimal.divide(divisor, roundingMode) which is
                                   ;; cheaper than the scale+rounding overload.
                                   (instance? Long bm)             (BigDecimal/valueOf (long bm))
                                   (instance? Integer bm)          (BigDecimal/valueOf (long (.intValue ^Integer bm)))
                                   (and (instance? BigDecimal bm)
                                        (clojure.core/==
                                         0
                                         (.scale ^BigDecimal bm))) ^BigDecimal bm
                                   :else                           nil)]
               (Money. ^Currency c
                       (if (some? d)
                         (try
                           ^BigDecimal (.divide ^BigDecimal am ^BigDecimal d ^RoundingMode rm)
                           (catch ArithmeticException e
                             (throw
                              (ex-info (or (.getMessage e) "Arithmetic error.")
                                       {:op                         :div
                                        :dividend                   a
                                        :divisor                    b
                                        :rounding-mode              rm
                                        :arithmetic-exception       true
                                        :arithmetic-exception/cause e}
                                       e))))
                         (div-core ^BigDecimal am bm
                                   (int (.scale ^BigDecimal am))
                                   ^RoundingMode rm)))))))
       (if bm?
         ;; number, money
         (throw (ex-info "Cannot divide a regular number by the monetary amount."
                         {:dividend a :divisor b}))
         ;; number, number
         (if rm
           (div-core ^BigDecimal am bm ^RoundingMode rm)
           (let [^BigDecimal bm (scale/apply bm)]
             (try
               (.divide ^BigDecimal am ^BigDecimal bm)
               (catch ArithmeticException e
                 (throw
                  (ex-info (or (.getMessage e) "Arithmetic error.")
                           {:op                         :div
                            :dividend                   a
                            :divisor                    b
                            :arithmetic-exception       true
                            :arithmetic-exception/cause e}
                           e))))))))))
  ([a b & more]
   (if scale/*each*
     (clojure.core/apply div-scaled a b more)
     (let [^RoundingMode rm (scale/rounding-mode)]
       (if-not (instance? Money a)
         ;; number, number
         (if rm
           (reduce (fn ^BigDecimal [^BigDecimal a b] (div-core ^BigDecimal a b ^RoundingMode rm))
                   (div-core ^BigDecimal (scale/apply a) b ^RoundingMode rm)
                   more)
           (reduce div-core-fn (div-core ^BigDecimal (scale/apply a) b) more))
         ;; money, ...
         (let [^RoundingMode rm (or rm scale/ROUND_UNNECESSARY)
               ^Currency      c (.currency ^Money a)
               ^BigDecimal   am (.amount   ^Money a)
               bm?              (instance? Money b)
               ^BigDecimal   bm (if bm? (.amount ^Money b) b)]
           (loop [^BigDecimal x (div-core ^BigDecimal am bm ^RoundingMode rm)
                  more          more]
             (if-some [y (first more)]
               (if (instance? Money y)
                 (if bm?
                   ;; number, money
                   (throw (ex-info "Cannot divide a regular number by the monetary amount."
                                   {:dividend x :divisor y}))
                   ;; money, money
                   (if (same-currency-objs? a y)
                     (reduce
                      (fn ^BigDecimal [^BigDecimal a b]
                        (when (instance? Money b)
                          (throw (ex-info "Cannot divide a regular number by the monetary amount."
                                          {:dividend a :divisor b})))
                        (div-core ^BigDecimal a b ^RoundingMode rm))
                      (div-core ^BigDecimal x
                                ^BigDecimal (.amount ^Money y) ^RoundingMode rm)
                      (rest more))
                     (throw (ex-info "Cannot divide by the amount of a different currency."
                                     {:dividend a :divisor y}))))
                 (recur (div-core ^BigDecimal x y ^RoundingMode rm) (rest more)))
               (if bm? x
                   (Money. ^Currency   c
                           (if (currency-auto-scaled? ^Currency c)
                             ^BigDecimal x
                             ^BigDecimal (scale/apply x (.scale ^BigDecimal am)))))))))))))

(def ^{:tag      Money :added "1.2.0"
       :arglists '([a]
                   [a b]
                   [a b & more])}
  /
  "Alias for div."
  div)

(defn min-amount
  "Returns the least of the monetary amounts."
  {:tag Money :added "1.0.0"}
  (^Money [^Money a] a)
  (^Money [^Money a ^Money b]
   (when-not (same-currency-objs? a b)
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (if (lt? a b) a b))
  (^Money  [^Money a ^Money b & more]
   (reduce min-amount (min-amount ^Money a ^Money b) more)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  min
  "Alias for min-amount."
  min-amount)

(defn max-amount
  "Returns the greatest of the monetary amounts."
  {:tag Money :added "1.0.0"}
  (^Money [^Money a] a)
  (^Money [^Money a ^Money b]
   (when-not (same-currency-objs? a b)
     (throw (ex-info "Cannot compare amounts of different currencies."
                     {:money-1 a :money-2 b})))
   (if (gt? a b) a b))
  (^Money [^Money a ^Money b & more]
   (reduce max-amount (max-amount a b) more)))

(def ^{:tag      Money :added "1.2.0"
       :arglists '(^Money [^Money a]
                   ^Money [^Money a ^Money b]
                   ^Money [^Money a ^Money b & more])}
  max
  "Alias for max-amount."
  max-amount)

(defn convert
  "Converts a monetary amount from one currency to another using the given
  multiplier (expressing a conversion rate). When two money objects are given, the
  second one should express a price of 1 unit of the first currency.

  When no rounding mode is given and rounding is required during scaling to another
  currency then the value of dynamic variable
  `io.randomseed.bankster.scale/*rounding-mode*` will be used."
  {:tag      Money
   :added    "1.0.0"
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
           ^Money a           (value a)
           sc                 (int (.scale ^Currency currency))
           ^BigDecimal rate   (scale/apply rate-or-rounding-mode)
           ^BigDecimal raw    (.multiply ^BigDecimal (.amount ^Money a) ^BigDecimal rate)]
       (Money. ^Currency currency
               (if (currency/val-auto-scaled*? sc)
                 raw
                 (bd-set-scale raw sc (scale/rounding-mode) :convert
                               {:money a :currency currency :rate rate-or-rounding-mode}))))))
  (^Money [a currency rate rounding-mode]
   (let [^Currency currency (currency/unit currency)
         ^Money a           (value a)
         sc                 (int (.scale ^Currency currency))
         ^BigDecimal ratebd (scale/apply rate)
         ^BigDecimal raw    (.multiply ^BigDecimal (.amount ^Money a) ^BigDecimal ratebd)]
     (Money. ^Currency currency
             (if (currency/val-auto-scaled*? sc)
               raw
               (bd-set-scale raw sc rounding-mode :convert
                             {:money a :currency currency :rate rate}))))))

(defmacro rem-core
  "In its binary variant it performs remainder calculation without rounding and rescaling.

  In its ternary variant gets the remainder with precision calculated to fit the
  longest possible result (unless there is a non-terminating decimal expansion then
  the result will be rounded).

  In its quaternary variant calculates the remainder with scale and rounding mode."
  {:added "1.2.0" :private true}
  ([a b scale r]
   `(let [^BigDecimal   a# ~a
	  ^BigDecimal   b# (scale/apply ~b)
	  ^RoundingMode r# ~r]
      (bd-set-scale (.remainder ^BigDecimal  a#
	                        ^BigDecimal  b#
	                        ^MathContext (scale/div-math-context
	                                      ^BigDecimal   a#
	                                      ^BigDecimal   b#
	                                      ^RoundingMode r#))
                    (int ~scale)
                    r#
                    :rem
                    {:dividend a# :divisor b#})))
  ([a b r]
   `(let [^BigDecimal a# ~a
          ^BigDecimal b# (scale/apply ~b)]
      ^BigDecimal (.remainder ^BigDecimal  a#
                              ^BigDecimal  b#
                              ^MathContext (scale/div-math-context
                                            ^BigDecimal   a#
                                            ^BigDecimal   b#
                                            ^RoundingMode ~r))))
  ([^BigDecimal a b]
   `(let [b# ~b]
      (if (instance? Money b#)
        (throw (ex-info "Cannot divide a regular number by the monetary amount."
                        {:dividend ~a :divisor b#}))
        ^BigDecimal (.remainder ^BigDecimal ~a
                                ^BigDecimal (scale/apply b#))))))

(defn rem
  "Returns the remainder of dividing an amount of money by the given number. For two
  Money objects the result is a BigDecimal. When only the first argument is a kind
  of Money, the result will also be a Money, rescaled to the scale of the monetary
  amount. When the rounding mode is not given,
  `io.randomseed.bankster.scale/*rounding-mode*` will be used when set."
  {:added "1.2.0"}
  ([a b]
   (rem a b (or (scale/rounding-mode) scale/ROUND_UNNECESSARY)))
  ([a b ^RoundingMode rounding-mode]
   (let [am?            (instance? Money a)
         bm?            (instance? Money b)
         ^BigDecimal am (if am? (.amount ^Money a) (scale/apply a))
         bm             (if bm? (.amount ^Money b) b)]
     (if am?
       (let [^Currency c (.currency ^Money a)]
         (if bm?
           ;; money, money
           (if-not (same-currency-objs? a b)
             (throw (ex-info "Cannot divide by the amount of a different currency."
                             {:dividend a :divisor b}))
             (rem-core ^BigDecimal am ^BigDecimal bm ^RoundingMode rounding-mode))
           ;; money, number
           (if (currency-auto-scaled? ^Currency c)
             (Money. ^Currency c
                     (rem-core ^BigDecimal am ^BigDecimal bm ^RoundingMode rounding-mode))
             (Money. ^Currency c
                     (rem-core ^BigDecimal am bm
                               (.scale ^BigDecimal am)
                               ^RoundingMode rounding-mode)))))
       (if bm?
         ;; number, money
         (throw (ex-info "Cannot divide a regular number by the monetary amount."
                         {:dividend a :divisor b}))
         ;; number, number
         (if rounding-mode
           (rem-core   ^BigDecimal am bm ^RoundingMode rounding-mode)
           (.remainder ^BigDecimal am ^BigDecimal (scale/apply bm))))))))

(def ^{:tag      Money
       :added    "1.1.0"
       :arglists '([^Money a b])}
  div-rem
  "Alias for rem."
  rem)

(defn neg
  "Returns the negated amount of the given money. For a negative amount it will reverse
  its sign. Same as `(sub a)`."
  {:tag Money :added "1.0.0"}
  ^Money [^Money a]
  (sub a))

(defn pos
  "Returns the given money object."
  {:tag Money :added "1.0.0"}
  ^Money [^Money a]
  a)

(defn abs
  "Returns the absolute value of the given money."
  {:tag BigDecimal :added "1.2.0"}
  [^Money a]
  (if (is-pos-or-zero? a) a
      (Money. ^Currency   (.currency ^Money a)
              ^BigDecimal (.abs ^BigDecimal (.amount ^Money a)))))

(defn round
  "Rounds the amount of money using the given scale and rounding mode. Returns money
  with rounded amount preserving the original scale. If the rounding mode is not
  given the one from `scale/*rounding-mode*` is used."
  {:tag Money :added "1.0.0"}
  (^Money [^Money money ^long scale]
   (round money scale (or (scale/rounding-mode) scale/ROUND_UNNECESSARY)))
  (^Money [^Money money ^long scale ^RoundingMode rounding-mode]
   (let [^BigDecimal am (.amount ^Money money)
         sc             (int (.scale am))]
     (if (clojure.core/>= scale sc)
       money
       (let [^BigDecimal rounded (bd-set-scale am (int scale) rounding-mode :round
                                               {:money money})
             ;; Restore original scale by appending zeros (does not require rounding).
             ^BigDecimal res     (.setScale ^BigDecimal rounded sc)]
         (Money. ^Currency (.currency ^Money money) res))))))

(defn round-to
  "Rounds a Money to an interval i (which should also be a Money or a number). If
  rounding mode is not provided (via the last argument or `scale/*rounding-mode*`) then
  it defaults to HALF_EVEN. Retains original scale of the amount (but not the nominal
  scale of a currency, if money was given). For nil intervals or intervals whose
  amounts are negative or zero, returns an unaltered monetary object."
  {:tag Money :added "1.2.9"}
  (^Money [^Money money] money)
  (^Money [^Money money ^Scalable i] (round-to money i nil))
  (^Money [^Money money ^Scalable i ^RoundingMode rounding-mode]
   (when-not (or (nil? i) (scale/scalable? i))
     (throw
      (ex-info
       (str "Interval must be scalable or nil: " i)
       {:interval i})))
   (let [^BigDecimal am-s (scale/amount i)]
     (if (or (nil? am-s) (clojure.core/not= 1 (.compareTo ^BigDecimal am-s BigDecimal/ZERO)))
       money
       (do  (when (and (instance? Money i) (not (same-currency-objs? money i)))
              (throw
               (ex-info
                (str "Money and interval should be of the same currency: " money ", " i ".")
                {:money    money
                 :interval i})))
            (let [sn               (long (.scale ^BigDecimal (.amount ^Money money)))
                  ^Money an        (scale/apply money)
                  ^RoundingMode rm (or rounding-mode (scale/rounding-mode) scale/ROUND_HALF_EVEN)]
              (scale/with-rounding rm
                (scale/apply
                 (mul ^BigDecimal am-s
                      ^Money (scale/apply ^Money (scale/apply ^Money (div an am-s) 0 rm) sn rm))
                 sn))))))))

(defn major
  "Returns the major part of the given amount of money."
  {:tag BigDecimal :added "1.0.0"}
  (^BigDecimal [^Money a]
   (.setScale ^BigDecimal (.amount ^Money a) 0 scale/ROUND_DOWN)))

(defn major->long
  "Returns the major part of the given amount as a long number."
  {:added "1.0.0"}
  ^long [^Money a]
  (long (scale/->long ^BigDecimal (.amount ^Money a) scale/ROUND_DOWN)))

(defn major->int
  "Returns the major part of the given amount as an int number widened to long."
  {:added "1.0.0"}
  ^long [^Money a]
  (long (scale/->int ^BigDecimal (.amount ^Money a) scale/ROUND_DOWN)))

(defn minor
  "Returns the minor part of the given amount."
  {:tag BigDecimal :added "1.0.0"}
  ^BigDecimal [^Money a]
  (let [^BigDecimal am (.amount ^Money a)
        sc             (int (.scale am))]
    (-> ^BigDecimal am
        ^BigDecimal (scale/apply (long sc) scale/ROUND_DOWN)
        ^BigDecimal (.remainder BigDecimal/ONE)
        ^BigDecimal (.movePointRight sc))))

(defn minor->long
  "Returns the minor part of the given amount as a long number."
  {:added "1.0.0"}
  ^long [^Money a]
  (long (.longValueExact ^BigDecimal (minor ^Money a))))

(defn minor->int
  "Returns the minor part of the given amount as an int number widened to long."
  {:tag 'long :added "1.0.0"}
  ^long [^Money a]
  (long (.intValueExact ^BigDecimal (minor ^Money a))))

(defn major-minor
  "Returns a vector with major and minor parts of the given monetary amount."
  {:tag clojure.lang.IPersistentVector :added "1.0.0"}
  ^clojure.lang.IPersistentVector [^Money a]
  [(major a) (minor a)])

(defn major-minor->int
  "Returns a vector with major and minor parts of the given monetary amount represented
  as integer numbers (widened to long)."
  {:tag clojure.lang.IPersistentVector :added "1.0.0"}
  ^clojure.lang.IPersistentVector [^Money a]
  [(major->int a) (minor->int a)])

(defn major-minor->long
  "Returns a vector with major and minor parts of the given monetary amount represented
  as long numbers."
  {:tag clojure.lang.IPersistentVector :added "1.0.0"}
  ^clojure.lang.IPersistentVector [^Money a]
  [(major->long a) (minor->long a)])

(defn ->symbol
  "Converts an amount of the given money to a symbol."
  {:tag clojure.lang.Symbol :added "1.1.0"}
  ^clojure.lang.Symbol [^Money a]
  (scale/to-symbol ^BigDecimal (.amount ^Money a)))

(defn ->clojure-symbol
  "Converts an amount of the given money to a clojure symbol (with added M when
  needed)."
  {:tag clojure.lang.Symbol :added "1.1.0"}
  ^clojure.lang.Symbol [^Money a]
  (scale/to-clojure-symbol ^BigDecimal (.amount ^Money a)))

(defn ->double
  "Converts an amount of the given money to a double with optional rescaling."
  {:added    "1.0.0"
   :arglists '(^double [a]
               ^double [a ^long scale]
               ^double [a rounding-mode]
               ^double [a ^long scale rounding-mode])}
  (^double [^Money a]
   (scale/->double ^BigDecimal (.amount ^Money a)))
  (^double [^Money a scale-or-rounding]
   (scale/->double ^BigDecimal (.amount ^Money a) scale-or-rounding))
  (^double [^Money a ^long scale rounding-mode]
   (scale/->double ^BigDecimal (.amount ^Money a) scale rounding-mode)))

(defn ->float
  "Converts an amount of the given money to a float with optional rescaling. Resulting
  type will be widened to double."
  {:tag      'double :added "1.0.0"
   :arglists '(^double [a]
               ^double [a ^long scale]
               ^double [a rounding-mode]
               ^double [a ^long scale rounding-mode])}
  (^double [^Money a]
   (scale/->float ^BigDecimal (.amount ^Money a)))
  (^double [^Money a scale-or-rounding]
   (scale/->float ^BigDecimal (.amount ^Money a) scale-or-rounding))
  (^double [^Money a ^long scale rounding-mode]
   (scale/->float ^BigDecimal (.amount ^Money a) scale rounding-mode)))

(defn add-major
  "Increases major amount by the given number. If the number is also expressed as money
  and it has decimal parts, they will be truncated."
  {:tag Money :added "1.0.0"}
  ^Money [^Money a b]
  (Money. ^Currency   (.currency ^Money a)
          ^BigDecimal (.add ^BigDecimal (.amount ^Money a)
                            ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN))))

(defn sub-major
  "Decreases major amount by the given number. If the number is also expressed as money
  and it has decimal parts, they will be truncated."
  {:tag Money :added "1.0.0"}
  ^Money [^Money a b]
  (Money. ^Currency   (.currency ^Money a)
          ^BigDecimal (.subtract ^BigDecimal (.amount ^Money a)
                                 ^BigDecimal (scale/apply b 0 scale/ROUND_DOWN))))

(defn inc-major
  "Increases major amount by 1."
  {:tag Money :added "1.0.0"}
  ^Money [^Money a]
  (add-major a BigDecimal/ONE))

(defn dec-major
  "Decreases major amount by 1."
  {:tag Money :added "1.0.0"}
  ^Money [^Money a]
  (sub-major a BigDecimal/ONE))

(defn add-minor
  "Increases minor amount by the given number. If the number is also expressed as money
  and it has decimal parts, they will be truncated.."
  {:tag Money :added "1.0.0"}
  ^Money [^Money a b]
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
;; Distribution and allocation
;;

(defmacro split-scale-int
  "Expands to an (int ...) expression computing scale for Money."
  [m]
  `(let [^Money      m# ~m
         ^Currency   c# (.currency m#)]
     (int (if (currency-auto-scaled? c#)
            (.scale ^java.math.BigDecimal (.amount ^io.randomseed.bankster.Money m#))
            (.scale ^io.randomseed.bankster.Currency c#)))))

(defmacro to-units
  [amount sc]
  `(-> ^java.math.BigDecimal ~amount
       (.movePointRight (int ~sc))
       (.toBigIntegerExact)))

(defn- from-units
  ^BigDecimal [^BigInteger units ^long sc]
  (-> (BigDecimal. units)
      (.movePointLeft (int sc))))

(defn- to-bigint
  ^BigInteger [x]
  (cond
    (int?                          x) (BigInteger/valueOf (long x))
    (instance? BigInteger          x) x
    (instance? clojure.lang.BigInt x) (.toBigInteger ^clojure.lang.BigInt x)
    (integer?                      x) (BigInteger/valueOf (long x))
    :else
    (throw (ex-info "Ratio must be integer-like"
                    {:got x :type (type x)}))))

(defn- int-like-pos?
  "Fast: true if x is integer-like and > 0. No BigInteger allocations for longs or
  ints."
  [x]
  (cond
    (int?                          x) (clojure.core/> (long x) 0)
    (instance? BigInteger          x) (clojure.core/> (.signum ^java.math.BigInteger x) 0)
    (integer?                      x) (clojure.core/pos? x)
    :else
    (throw (ex-info "Ratio must be integer-like"
                    {:got x :type (type x)}))))

(defn allocate
  "Allocates a monetary amount `a` into parts according to integer `ratios`.

  Returns a vector of `Money` values (same currency as `a`) whose amounts:

  - sum exactly to `a` (sum-preserving),
  - are expressed in the minimal unit implied by the effective scale
  (currency nominal scale; for auto-scaled currencies: the scale of `a`),
  - differ only by whole minor units (no fractional minor units are created),
  - distribute the leftover remainder deterministically from left to right.

  `ratios` is any sequence of integer-like values (`Long`, `BigInt`,
  `BigInteger`). Zeros are allowed (yielding zero parts); negative ratios are treated
  as non-positive and produce zero base allocation.

  For negative `a`, allocation is performed on the absolute value and the sign is
  applied to all resulting parts (preserving the exact sum).

  Throws `ExceptionInfo` when:
  - `ratios` is empty,
  - the sum of ratios is not strictly positive.

  Examples:

  ```
  (allocate #money[10.00 PLN] [1 1 1])
  ;; => [#money[3.34 PLN] #money[3.33 PLN] #money[3.33 PLN]]

  (allocate #money[1.00 PLN] [1 2 3])
  ;; 100 units split by 1:2:3 => bases [16 33 50] remainder 1 => [17 33 50]
  ;; => [#money[0.17 PLN] #money[0.33 PLN] #money[0.50 PLN]]
  ```
  "
  {:tag clojure.lang.IPersistentVector :added "1.2.18"}
  ^clojure.lang.IPersistentVector [^Money a ratios]
  (let [^long bcnt (bounded-count 2 ratios)]
    (when (clojure.core/zero? bcnt)
      (throw (ex-info "Ratios cannot be empty" {:ratios ratios})))
    (if (clojure.core/== bcnt 1)
      (if (int-like-pos? (first ratios))
        [a]
        (throw (ex-info "Sum of ratios must be > 0" {:ratios ratios :sum (first ratios)})))
      (let [ratiosBI           (mapv to-bigint ratios)
            n                  (count ratiosBI)
            ^BigInteger sumrBI (reduce (fn ^BigInteger [^BigInteger v1 ^BigInteger v2]
                                         (.add v1 v2))
                                       BigInteger/ZERO
                                       ratiosBI)]
        (when (clojure.core/<= (.signum sumrBI) 0)
          (throw (ex-info "Sum of ratios must be > 0" {:ratios ratios :sum sumrBI})))
        (let [sc                 (int (split-scale-int a))
              ^BigDecimal am     (.amount a)
              negv?              (clojure.core/neg? (.signum am))
              ^BigInteger units0 (to-units (.abs am) sc)
              bases              (mapv (fn ^BigInteger [^BigInteger r]
                                         (if (clojure.core/<= (.signum r) 0)
                                           BigInteger/ZERO
                                           (.divide (.multiply units0 r) sumrBI)))
                                       ratiosBI)
              ^BigInteger used   (reduce #(.add ^BigInteger %1 ^BigInteger %2) BigInteger/ZERO bases)
              ^BigInteger rem    (.subtract units0 used)
              ;; distribute remainder: left-to-right
              bases2             (loop [i             (unchecked-int 0)
                                        bs            bases
                                        ^BigInteger r rem]
                                   (if (clojure.core/zero? (.signum ^BigInteger r))
                                     bs
                                     (recur (unchecked-inc i)
                                            (update bs (mod i n)
                                                    (fn ^BigInteger [^BigInteger v]
                                                      (.add ^BigInteger v BigInteger/ONE)))
                                            (.subtract ^BigInteger r BigInteger/ONE))))]
          (mapv (if negv?
                  (fn ^Money [^BigInteger u]
                    (Money. ^Currency (.currency a) ^BigDecimal (from-units ^BigInteger (.negate u) sc)))
                  (fn ^Money [^BigInteger u]
                    (Money. ^Currency (.currency a) ^BigDecimal (from-units ^BigInteger u sc))))
                bases2))))))

(defn distribute
  "Distributes a monetary amount `a` into `n` as-even-as-possible parts.

  This is equivalent to:
  `(allocate a (repeat n 1))`

  Returns a vector of `Money` values (same currency as `a`) such that:

  - the parts sum exactly to `a`,
  - each part differs from another by at most one minor unit (relative to the
    effective scale used for splitting),
  - any remainder is distributed deterministically from left to right.

  `n` must be integer-like and strictly positive; otherwise throws `ExceptionInfo`.

  Examples:

  ```
  (distribute #money[10.00 PLN] 3)
  ;; => [#money[3.34 PLN] #money[3.33 PLN] #money[3.33 PLN]]

  (distribute #money[-10.00 PLN] 3)
  ;; => [#money[-3.34 PLN] #money[-3.33 PLN] #money[-3.33 PLN]]
  ```
  "
  {:tag clojure.lang.IPersistentVector :added "1.2.18"}
  ^clojure.lang.IPersistentVector [^Money a n]
  (let [^BigInteger nBI (to-bigint n)]
    (when (clojure.core/<= (.signum nBI) 0)
      (throw (ex-info "n must be > 0" {:n n})))
    (let [n* (.intValueExact nBI)]
      (allocate ^Money a (repeat n* 1)))))

;;
;; Contextual macros.
;;

(defmacro with-rounding
  "Alias for `io.randomseed.bankster.scale/with-rounding`."
  {:added "1.0.0"}
  [rounding-mode & body]
  (list* 'io.randomseed.bankster.scale/with-rounding rounding-mode body))

(defmacro with-rescaling
  "Alias for `io.randomseed.bankster.scale/with-rescaling`."
  {:added "1.0.0"}
  [rounding-mode & body]
  (list* 'io.randomseed.bankster.scale/with-rescaling rounding-mode body))

(defmacro with-currency
  "Sets a default currency in a lexical context of the body. Has the same effect as
  `io.randomseed.bankster.currency/with`."
  {:added "1.0.0"}
  [currency & body]
  (let [cur# (if (symbol? currency) (keyword currency) currency)]
    `(binding [currency/*default* (currency/unit ~cur#)]
       ~@body)))

;;
;; Tagged literals.
;;

(defn lit-parse
  "Internal pre-parser for tagged literals. Takes a parsing function and arguments
  in decomposed form."
  {:added "1.2.4" :private true}
  ([a]
   (of-gen parse a))
  ([a b]
   (if (nil? b)
     (of-gen parse a)
     (of-gen parse a b)))
  ([a b r]
   (if (nil? r)
     (if (nil? b)
       (lit-parse a)
       (lit-parse a b))
     (of-gen parse a b r))))

(defn code-literal
  "Tagged literal handler for Money objects expressed as tagged literals in Clojure
  code. Returns compound forms (parsing function invocations that can be influenced
  by the runtime environment, e.g. dynamic variables like `scale/*rounding-mode*`)."
  {:added "1.2.4"}
  [arg]
  (if (nil? arg)
    (lit-parse nil)
    (if (sequential? arg)
      (clojure.core/apply lit-parse (take 3 arg))
      (if (map? arg)
        (lit-parse (:currency arg)
                   (:amount   arg)
                   (or (:rounding-mode arg)
                       (:rounding arg)))
        (lit-parse arg)))))

(defn data-literal
  "Data reader for Money objects expressed as tagged literals in EDN data
  files."
  {:added "1.2.4" :tag io.randomseed.bankster.Money}
  [arg]
  (let [r (code-literal arg)]
    (if (seq? r)
      (clojure.core/apply (first r) (rest r))
      r)))

(defn defliteral
  "For the given currency identifier or a currency object it creates a tagged literal
  in a form of #money/CURRENCY where the CURRENCY is a currency code. As a side
  effect it creates a function of name `io.randomseed.bankster.money/of-CURRENCY` that
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

(defn ns-data-literal
  {:private true :added "1.0.0"}
  [kw arg]
  (let [[a b r] (if (sequential? arg) arg [arg nil nil])
        [c am]  (if (and (some? b) (number? a)) [b a] [a b])
        c       (if (number? c) c (keyword kw (str (symbol c))))]
    (data-literal [c am r])))

(defn ns-code-literal
  {:private true :added "1.0.0"}
  [kw arg]
  (let [[a b r] (if (sequential? arg) arg [arg nil nil])
        [c am]  (if (and (some? b) (number? a)) [b a] [a b])
        c       (if (number? c) c (keyword kw (str (symbol c))))]
    (code-literal [c am r])))

(load "money/reader_handlers")

;;
;; Data readers maps.
;;

(defn- load-readers
  {:private true :added "2.1.1"}
  [resource-name]
  (when-some [r (fs/get-resource resource-name)]
    (when-some [d (clojure.edn/read-string (slurp r))]
      (when (map? d)
        (into {} (for [[k v] d] [k (resolve v)]))))))

(def ^{:tag clojure.lang.IPersistentMap :added "1.2.4"}
  data-readers
  "Data readers map for currency and money (intended to be used when reading EDN data
  files containing tagged literals: #currency and #money). Handlers assigned to
  literals are always emitting constant forms. Please note that tagged literal
  handlers for Clojure code are using different readers (defined as code-readers)."
  (load-readers "data_readers_edn.clj"))

(def ^{:tag clojure.lang.IPersistentMap :added "1.2.4"}
  code-readers
  "Data readers map for currency and money (intended to be used when evaluating Clojure
  code with tagged literals: #currency and #money). Handlers assigned to literals may
  emit compound forms, like function call forms (as Clojure code). To operate on EDN
  data files, see data-readers."
  (load-readers "data_readers.clj"))

(defn readers
  "Returns a data readers map (tag symbol -> handler function) suitable for
  `clojure.edn/read` / `clojure.edn/read-string`.

  The returned map always includes the base tags:
  - `#money` and `#bankster.money`,
  - `#currency` and `#bankster.currency`.

  When a registry is available it also includes tags of form `#money/NS` (and
  `#bankster.money/NS`) for each currency namespace present in the registry, so EDN
  can disambiguate codes between currency domains (e.g. `#money/crypto[1 ETH]`).

  Note: this helper does not mutate `clojure.core/*data-readers*`."
  {:tag clojure.lang.IPersistentMap :added "2.0.0"}
  ([]
   (readers (registry/get)))
  ([^Registry registry]
   (let [^Registry registry (registry/get registry)
         nses (->> (.cur-id->cur registry)
                   (keys)
                   (keep namespace)
                   (distinct)
                   (sort-by str))]
     (into {'money            data-literal
            'bankster.money   data-literal
            'currency         currency/data-literal
            'bankster.currency currency/data-literal}
           (mapcat (fn [^String ns]
                     (let [h (fn [arg] (ns-data-literal ns arg))]
                       [[(symbol "money" ns) h]
                        [(symbol "bankster.money" ns) h]])))
           nses))))

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

  - `:rounding-mode`   - RoundingMode, rounding mode to apply when scaling (if `scale/*rounding-mode*` is not set)
  - `:grouping`        - Boolean, if true then grouping will be used
  - `:grouping-size`   - integer, size of a group when grouping
  - `:negative-prefix` - String, negative prefix to use
  - `:negative-suffix` - String, negative suffix to use
  - `:positive-prefix` - String, positive prefix to use
  - `:positive-suffix` - String, positive suffix to use
  - `:always-sep-dec`  - Boolean, if true then the decimal separator will always be shown
  - `:currency-symbol-fn`  - a function used on a bankster/Currency object to get its symbol as a string
  - `:min-fraction-digits` - integer, the minimum number of digits allowed in the fraction portion of an amount
  - `:min-integer-digits`  - integer, the minimum number of digits allowed in the integer portion of an amount
  - `:max-fraction-digits` - integer, the maximum number of digits allowed in the fraction portion of an amount
  - `:max-integer-digits`  - integer, the maximum number of digits allowed in the integer portion of an amount
  - `:scale`               - sets both `:min-fraction-digits` and `:max-fraction-digits` to the same value.

  The function assigned to the `:currency-symbol-fn` should take 3 arguments:
  currency, locale and registry.

  Note that you may gain some speed by re-using the extended formatter (basic
  formatter is already cached). See the documentation of the format-with function for
  more information."
  {:tag String :added "1.0.0"}
  ([^Money money]
   (format money (Locale/getDefault)))
  ([^Money money locale]
   (if-some [rm (scale/rounding-mode)]
     (if (clojure.core/not= rm scale/ROUND_UNNECESSARY)
       (let [f (currency/formatter-instance (.currency ^Money money) locale)]
         (.format ^DecimalFormat f
                  ^BigDecimal (scale/apply (.amount ^Money money)
                                           (clojure.core/max
                                            (.getMaximumFractionDigits ^DecimalFormat f)
                                            (.getMinimumFractionDigits ^DecimalFormat f)) rm)))
       (.format ^DecimalFormat (currency/formatter-instance (.currency ^Money money) locale)
                ^BigDecimal    (.amount ^Money money)))
     (.format ^DecimalFormat (currency/formatter-instance (.currency ^Money money) locale)
              ^BigDecimal    (.amount ^Money money))))
  ([^Money money locale opts]
   (if-some [rmode (or (:rounding-mode opts) (scale/rounding-mode))]
     (if (clojure.core/not= rmode scale/ROUND_UNNECESSARY)
       (.format ^DecimalFormat (currency/formatter-extended (.currency ^Money money) locale
                                                            (assoc opts :rounding-mode rmode))
                ^BigDecimal    (.amount ^Money money))
       (.format ^DecimalFormat (currency/formatter-extended (.currency ^Money money) locale opts)
                ^BigDecimal    (.amount ^Money money)))
     (.format ^DecimalFormat (currency/formatter-extended (.currency ^Money money) locale opts)
              ^BigDecimal    (.amount ^Money money)))))

(defn format-with
  "Formats the amount of money using the formatter provided. Formatters can be created
  with `io.randomseed.bankster.currency/formatter` and
  `io.randomseed.bankster.currency/formatter-extended`."
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

;;
;; EDN-friendly map conversion.
;;

(defn to-map
  "Coerces a money-like value into an EDN-friendly map.

  Canonical shape:
  - `:currency` - currency ID keyword (may be namespaced, e.g. `:crypto/USDT`)
  - `:amount`   - BigDecimal amount (scale preserved)

  This is intended as a stable, data-oriented representation (close to what Bankster
  already supports in `#money{...}` literals). For JSON wire formats see
  `to-json-map` / `to-json-string`."
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  ^clojure.lang.IPersistentMap [money]
  (when-some [^Money m (value money)]
    {:currency (currency/id ^Currency (.currency m))
     :amount   ^BigDecimal (.amount m)}))

(defn of-map
  "Creates Money from a map representation.

  Expected keys:
  - `:currency` (or `:cur`)  - any currency representation accepted by Bankster
  - `:amount`               - amount as BigDecimal/number/string
  - optional `:rounding-mode` / `:rounding`

  This is a general-purpose helper meant for EDN-like data. It accepts keyword/symbol
  rounding-mode names (e.g. `:HALF_UP`), not only `java.math.RoundingMode`."
  {:tag Money :added "2.1.0"}
  ^Money [m]
  (when (some? m)
    (when-not (map? m)
      (throw
       (ex-info
        "Money map representation must be a map."
        {:op    :bankster.money/of-map
         :value m
         :class (class m)})))
    (let [cur (or (get m :currency) (get m :cur) (get m "currency") (get m "cur"))
          amt (or (get m :amount) (get m "amount"))
          rm  (or (get m :rounding-mode) (get m :rounding)
                  (get m "rounding-mode") (get m "rounding"))
          rm  (when (some? rm) (scale/post-parse-rounding rm))]
      (when-not (some? cur)
        (throw
         (ex-info
          "Money map is missing :currency."
          {:op    :bankster.money/of-map
           :value m})))
      (when-not (some? amt)
        (throw
         (ex-info
          "Money map is missing :amount."
          {:op    :bankster.money/of-map
           :value m})))
      (if (some? rm)
        (do
          (when-not (instance? RoundingMode rm)
            (throw
             (ex-info
              "Money :rounding-mode must resolve to java.math.RoundingMode."
              {:op    :bankster.money/of-map
               :value rm
               :class (class rm)})))
          (value cur amt rm))
        (value cur amt)))))

;;
;; JSON serialization helpers.
;;

(defn to-json-map
  "Serializes a money-like value to a JSON-friendly map.

  The returned map uses keyword keys but is safe to pass to JSON encoders.

  Options:
  - `:code-only?` - when truthy, namespace is omitted: `:crypto/ETH` → `\"ETH\"`"
  {:tag clojure.lang.IPersistentMap :added "2.1.0"}
  (^clojure.lang.IPersistentMap [money]
   (when-some [m (value money)]
     (serializers-json/money->json-map m)))
  (^clojure.lang.IPersistentMap [money opts]
   (when-some [m (value money)]
     (serializers-json/money->json-map m opts))))

(defn to-json-string
  "Serializes a money-like value to a canonical JSON string.

  Options:
  - `:code-only?` - when truthy, namespace is omitted: `:crypto/ETH` → `\"ETH\"`"
  {:tag String :added "2.1.0"}
  (^String [money]
   (when-some [m (value money)]
     (serializers-json/money->json-string m)))
  (^String [money opts]
   (when-some [m (value money)]
     (serializers-json/money->json-string m opts))))

(defn from-json-map
  "Deserializes Money from a JSON map.

  Options:
  - `:registry`      - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode` - `java.math.RoundingMode` for rescaling"
  {:tag Money :added "2.1.0"}
  (^Money [m]
   (serializers-json/json-map->money m))
  (^Money [m opts]
   (serializers-json/json-map->money m opts)))

(defn from-json-string
  "Deserializes Money from a JSON string.

  Options:
  - `:registry`      - registry to use for currency lookup (default: `registry/get`)
  - `:rounding-mode` - `java.math.RoundingMode` for rescaling"
  {:tag Money :added "2.1.0"}
  (^Money [s]
   (serializers-json/json-string->money s))
  (^Money [s opts]
   (serializers-json/json-string->money s opts)))
