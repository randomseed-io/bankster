# Sneak peeks

Note on code literals (`#money`, `#currency`): tagged literals in Clojure code are
handled at read time via `data_readers.clj`. Clojure does not auto-`require` the
handler namespaces, so make sure Bankster namespaces are loaded before code
containing these literals is read (otherwise you may see `Attempting to call unbound
fn .../code-literal`). For scripts, prefer using `read-string` after `require` (or
`clojure.edn/read-string` with `money/readers`).

## It **shows information** about a currency

```clojure
;; front API helpers

(require '[io.randomseed.bankster.api.currency :as currency])

;; global registry lookup with a keyword

(currency/of :PLN)
#currency{:id :PLN, :domain :ISO-4217, :kind :iso/fiat, :numeric 985, :scale 2}

;; global registry lookup using namespaced symbol

(currency/of crypto/ETH)
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :virtual/native, :scale 18, :weight 5}

;; global registry lookup with a string (incl. namespace a.k.a domain)

(currency/of "crypto/BTC")
#currency{:id :crypto/BTC, :domain :CRYPTO, :kind :virtual/native, :scale 8, :weight 5}

;; global registry lookup with a currency code
;; (weight solves potential conflicts when two currencies have the same currency code)

(currency/of ETH)
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :virtual/native, :scale 18, :weight 5}

;; global registry lookup using ISO currency number

(currency/of 840)
#currency{:id :USD, :domain :ISO-4217, :kind :iso/fiat, :numeric 840, :scale 2}

;; global registry lookup using tagged literal with a currency code

#currency XLM
#currency{:id :crypto/XLM, :domain :CRYPTO, :kind :virtual/native, :scale 8}

;; tagged literal accepts single-element vector (unwrapped)

#currency [EUR]
#currency{:id :EUR, :domain :ISO-4217, :kind :iso/fiat, :numeric 978, :scale 2}

;; global registry lookup using tagged literal with a namespaced identifier

#currency crypto/XLM
#currency{:id :crypto/XLM, :domain :CRYPTO, :kind :virtual/native, :scale 8}

;; global registry lookup using tagged literal with an ISO currency number

#currency 978
#currency{:id :EUR, :domain :ISO-4217, :kind :iso/fiat, :numeric 978, :scale 2}

;; Full currency information (including registry metadata).
(currency/info :PLN)
{:id :PLN,
 :numeric 985,
 :scale 2,
 :domain :ISO-4217,
 :kind :iso/fiat,
 :weight 0,
 :countries #{:PL},
 :localized {:pl {:name "złoty polski", :symbol "zł"}}}

(currency/info :crypto/USDC)
{:id :crypto/USDC,
 :numeric -1,
 :scale 8,
 :domain :CRYPTO,
 :kind :virtual.stable.peg/fiat,
 :weight 4,
 :localized {:* {:name "USD Coin", :symbol "USDC"}},
 :traits #{:peg/fiat :stable/coin :token/erc20}}
```

## It allows to **create a currency** and **register it**

```clojure
;; front API helpers

(require '[io.randomseed.bankster.api.currency :as currency])

;; ad hoc currency creation using constructor function

(currency/new :petro/USD 999 2 :COMBANK)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :numeric 999, :scale 2}

;; ad-hoc currency creation using tagged literal

#currency{:id :crypto/ETH :scale 18}
#currency{:id :crypto/ETH, :domain :CRYPTO, :scale 18}

;; putting new currency into a global, shared registry

(currency/register! (currency/new :petro/USD 9999 2 :COMBANK) :USA)
#Registry[{:currencies 221, :countries 250, :version "2021022121170359"} 0x11efe93f]

;; getting currency from a global registry

(currency/of :petro/USD)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :numeric 9999, :scale 2}

;; registering new currency expressed as a tagged literal

(currency/register! #currency{:id :crypto/AAA :scale 8})
#Registry[{:currencies 221, :countries 249, :version "2021022121170359"} 0x7eaf7a70]

;; creating an ISO currency (must have: a simple 3-letter code w/o ns and a numeric ID)

(currency/new :XOX 999 2 :COMBANK)
#currency{:id :XOX, :domain :ISO-4217, :kind :COMBANK, :numeric 999, :scale 2}

;; creating a strange ISO currency (forced by a namespace but w/o a numerical ID)

(currency/new :ISO-4217/XOX nil 2 :COMBANK)
#currency{:id :XOX, :domain :ISO-4217, :kind :COMBANK, :scale 2}
```

## It allows to create **monetary amounts**

```clojure
;; front API helpers

(require '[io.randomseed.bankster.api.money    :as    money]
         '[io.randomseed.bankster.api.currency :as currency])

;; using money/of macro with keyword ID and an amount

(money/of :EUR 25)
#money[25.00 EUR]

;; using money/of macro with keyword ID and an amount as a first argument

(money/of 25 :EUR)
#money[25.00 EUR]

;; using money/of macro with joint keyword ID and an amount as a first argument

(money/of :25_EUR)
#money[25.00 EUR]

(money/of :25EUR)
#money[25.00 EUR]

;; using money/of macro with unquoted symbolic ID and an amount

(money/of EUR 25)
#money[25.00 EUR]

;; using money/of macro with joint unquoted symbolic ID and an amount

(money/of EUR_25)
#money[25.00 EUR]

(money/of EUR25)
#money[25.00 EUR]

;; using money/of macro with namespaced keyword ID and an amount

(money/of crypto/BTC 10.1)
#money/crypto[10.10000000 BTC]

;; using money/of macro with currency code and an amount

(money/of BTC 10.1)
#money/crypto[10.10000000 BTC]

;; using tagged literals

#money EUR
#money[0.00 EUR]

#money/crypto ETH
#money/crypto[0.000000000000000000 ETH]

#money[PLN 2.50]
#money[2.50 PLN]

;; using tagged literal with a namespace

#money/crypto[1.31337 ETH]
#money/crypto[1.313370000000000000 ETH]

;; using tagged literal with a currency code

#money[1.31337 ETH]
#money/crypto[1.313370000000000000 ETH]

;; using tagged literal with a namespace but the amount goes first

#money/crypto[BTC 1.31337]
#money/crypto[1.31337000 BTC]

;; using default currency in a lexical context

(currency/with EUR (money/of 1000))
#money[1000.00 EUR]

;; using default currency in a lexical context (alias for the above)

(money/with-currency EUR (money/of 1000))
#money[1000.00 EUR]

;; using composed amounts and currencies

#money EUR100
#money[100 EUR]

#money :100_EUR
#money[100 EUR]

#money :100EUR
#money[100 EUR]

#money "100 EUR"
#money[100 EUR]

(money/of "100EUR")
#money[100 EUR]
```

## It allows to perform **logical operations** on monetary amounts

``` clojure
;; front API helper

(require '[io.randomseed.bankster.api.money :as money])

(money/eq? #money[5 GBP] #money[GBP 5])
true

(money/ne? #money[5 GBP] #money[GBP 5])
false

(money/eq? #money[5 GBP] #money/crypto[5 ETH])
false

(money/gt? #money[1 JPY] #money[0 JPY])
true

(money/ge? #money[1 JPY] #money[1 JPY])
true

(money/lt? #money[1 JPY] #money[0 JPY])
false

(money/le? #money[1 JPY] #money[0 JPY])
false

(money/zero? #money[0 USD])
true

(money/neg? #money[-2 XXX])
true

(money/pos? #money[-2 XXX])
false
```

## It allows to perform **math operations** on monetary amounts

``` clojure
;; front API helpers

(require '[io.randomseed.bankster.api       :as   api]
         '[io.randomseed.bankster.api.money :as money])

;; adding money expressed with tagged literals and with a macro call

(money/add #money[EUR 7] #money[0.54 EUR] (money/of 4.40 EUR))
#money[11.94 EUR]

;; dividing money by a number

(money/div #money/crypto[5 USDT] 2)
#money/crypto[2.50000000 USDT]

;; dividing money by numbers that separately would require rounding

(money/div #money[1 GBP] 8 0.5)
#money[0.25 GBP]

;; dividing money by numbers with rounding after each consecutive calculation

(api/with-rescaling :HALF_UP
  (money/div #money[1 GBP] 8 0.5))
#money[0.26 GBP]

;; dividing money by money (of the same currency)

(money/div #money/crypto[5 BTC] #money/crypto[2 BTC])
2.5M

;; dividing causing scale to exceed in one of the steps
;; but no rounding is necessary due to later operation

(money/div #money[1 PLN] 8 0.5)
#money[0.25 PLN]

;; dividing, scaled and rounded with each operation

(api/with-rounding :HALF_UP
  (money/div-scaled #money[1 PLN] 8 0.5))
#money[0.26 PLN]

;; same as the above but shorter

(api/with-rescaling :HALF_UP
  (money/div #money[1 PLN] 8 0.5))
#money[0.26 PLN]

;; handling non-terminating decimal expansion

(api/with-rounding :HALF_UP
  (money/div #money[1 PLN] 3))
#money[0.33 PLN]

;; rounding with unit reduction

(api/with-rounding :HALF_UP
  (money/div #money[1 PLN] #money[3 PLN]))
0.33M

;; rounding and unit reduction (regular numbers, dynamic scale)

(api/with-rounding :HALF_UP
  (money/div 1 3))
0.33333M

;; multiplying money by numbers

(money/mul #money/crypto[5 ETH] 1 2 3 4 5)
#money/crypto[600.000000000000000000 ETH]

;; adding to major part

(money/add-major #money[1.23 PLN] 100)
#money[101.23 PLN]

;; adding to minor part

(money/add-minor #money[1.23 PLN] 77)
#money[2.00 PLN]

;; converting

(money/convert #money/crypto[1.5 ETH] :crypto/USDT 1646.75)
#money/crypto[2470.12500000 USDT]

;; comparing

(sort money/compare [(money/of 10    PLN)
                     (money/of  0    PLN)
                     (money/of 30    PLN)
                     (money/of  1.23 PLN)])
(#money[0.00 PLN]
 #money[1.23 PLN]
 #money[10.00 PLN]
 #money[30.00 PLN])

;; rounding to the given interval

(money/round-to #money[31.33 USD] 0.5)
#money[31.50 USD]

;; allocation

(money/allocate #money[10.00 PLN] [1 1 1])
[#money[3.34 PLN]
 #money[3.33 PLN]
 #money[3.33 PLN]]

(money/allocate #money[1.00 PLN] [1 2 3])
[#money[0.17 PLN]
 #money[0.33 PLN]
 #money[0.50 PLN]]

;; distribution

(money/distribute (money/of 1 PLN) 3)
[#money[0.34 PLN]
 #money[0.33 PLN]
 #money[0.33 PLN]]

(money/distribute (money/of 3 PLN) 3)
[#money[1.00 PLN]
 #money[1.00 PLN]
 #money[1.00 PLN]]

;;
;; using API ops (money-aware operators)
;;

(require '[io.randomseed.bankster.api.ops :refer :all])

;; NOTE: `api.ops` is intentionally polymorphic (Money + numeric fallbacks).
;; If you need the lower-level namespace, use `io.randomseed.bankster.money.inter-ops`.

(+ 1 2 3)
6

(+ #money[USD 8] #money[USD 7.12])
#money[15.12 USD]

(* 1 2 3 4 5 #money/crypto[0.7 ETH])
#money/crypto[84.000000000000000000 ETH]

;;
;; using api (front API)
;;

(require '[io.randomseed.bankster.api          :as      api]
         '[io.randomseed.bankster.api.money    :as    money]
         '[io.randomseed.bankster.api.currency :as currency])

;; NOTE: `api/amount` is an alias for `scale/amount`.
;; NOTE: `api/scale` returns scale (Money amount scale / Currency nominal scale).

;; For auto-scaled currencies, Money scale reflects the current amount's scale
;; (it adapts to the value), not a nominal currency scale.

(api/with-rescaling :HALF_UP
  (api/auto-scaled? :XAU))
;; => true

(api/amount 12.30M)
;; => 12.30M  ; BigDecimal

(api/scale #money[12.30 EUR])
;; => 2

(api/scale :XAU)
;; => -1  ; auto-scaled currency

(money/add #money[10 EUR] #money[5 EUR])
;; => #money[15 EUR]

(money/gt? #money[10 EUR] #money[5 EUR])
;; => true

(currency/resolve-all :EUR)
;; => #{#currency{:id :EUR, ...}}

(currency/id-str :crypto/eth)
;; => "crypto/ETH"

(currency/code-str :crypto/eth)
;; => "ETH"

(currency/symbol :USD :en_US)
;; => "$"

(currency/name :EUR :en_US)
;; => "Euro"

(money/of-registry (api/default-registry) #money[10 EUR])
;; => #money[10.00 EUR]

(money/cast #money[10 EUR] :USD :HALF_UP)
;; => #money[... USD]

(money/cast-try #money[10 EUR] :NOPE)
;; => nil

(money/format #money[1234.50 PLN] :pl_PL)
;; => "1 234,50 zł"
```

## It allows to perform **generic, polymorphic operations**

```clojure
;; front API helpers

(require '[io.randomseed.bankster.api          :as      api]
         '[io.randomseed.bankster.api.money    :as    money]
         '[io.randomseed.bankster.api.currency :as currency])

(api/scale #currency PLN)
2

(api/scale #currency crypto/ETH)
18

(api/scale 123.45)
2

(api/scale #money[100 EUR])
2

(api/scale :GBP)
2

;; scale of the currency (low-level)

(api/scale :XXX)
-1

;; scale of the amount

(api/scale #money[12.34567 XXX])
5 ; current scale

;; nominal scale of the currency

(currency/scale #money[12.34567 XXX])
-1 ; auto-scaled

(currency/auto-scaled? :XXX)
true

(currency/auto-scaled? #money[12.34567 XXX])
true

(api/auto-scaled? :XXX)
true

(api/auto-scaled? #money[12.34567 XXX])
false ; scale of the amount, not the currency

(api/scale-apply #money[10 USD] 8) ;; use with caution
#money[10.00000000 USD]

(api/scale-apply #currency USD 8)  ;; use with caution
#currency{:id :USD, :domain :ISO-4217, :kind :iso/fiat, :numeric 840, :scale 8}

(money/rescale #money[10 USD] 8)
#money[10.00000000 USD]

;; unary variant of money/rescale
;; rescales back to nominal scale

(money/rescale
 (money/rescale #money[10 USD] 8))
#money[10.00 USD]

(api/amount #money[108.11 CHF])
108.11M

(scale/integer #money[108.11 CHF])
108M

(scale/fractional #money[108.11 CHF])
11M

(currency/iso? #money[1 GBP])
true

(currency/code #money[1 GBP])
"GBP"
```

## It allows to **serialize and deserialize** monetary amounts

```clojure
(require '[io.randomseed.bankster.serializers.json :as sj]
         '[io.randomseed.bankster.serializers.edn  :as se])

;; JSON serialization (minimal - only currency ID and amount)

(sj/money->json-map #money[12.30 PLN])
{:currency "PLN", :amount "12.30"}

;; JSON full serialization (currency as nested map)

(sj/money->json-full-map #money[12.30 PLN])
{:currency {:id "PLN", :numeric 985, :scale 2, :kind "iso/fiat", :domain "ISO-4217"},
 :amount "12.30"}

;; JSON with :full? option

(sj/to-json-map #money[12.30 PLN] {:full? true})
{:currency {:id "PLN", :numeric 985, :scale 2, :kind "iso/fiat", :domain "ISO-4217"},
 :amount "12.30"}

;; JSON with :keys filtering (nested options for currency)

(sj/money->json-full-map #money[12.30 PLN]
                        {:keys [:amount {:currency {:keys [:id :numeric]}}]})
{:amount "12.30", :currency {:id "PLN", :numeric 985}}

;; JSON string representation

(sj/money->json-string #money[12.30 PLN])
"12.30 PLN"

;; EDN serialization (BigDecimals, keyword IDs)

(se/money->edn-map #money[12.30 PLN])
{:currency :PLN, :amount 12.30M}

;; EDN tagged literal string

(se/money->edn-string #money[12.30 PLN])
"#money[12.30M PLN]"

(se/money->edn-string #money/crypto[1.5 ETH])

"#money/crypto[1.500000000000000000M ETH]"

;; Deserialization

(sj/json-map->money {:currency "PLN" :amount "12.30"})
#money[12.30 PLN]

;; Note: for JSON inputs prefer string amounts (or configure your
;; JSON parser to produce BigDecimal) to avoid double-precision loss.

(sj/json-text->money "{\"currency\":\"PLN\",\"amount\":12.30}")
#money[12.30 PLN]

(sj/json-string->money "12.30 PLN")
#money[12.30 PLN]

(se/edn-string->money "#money[12.30M PLN]")
#money[12.30 PLN]

;; With custom registry and rounding (also accepts keywords/strings)

(sj/json-map->money {:currency "PLN" :amount "1.005"}
                    {:rounding-mode :HALF_UP})
#money[1.01 PLN]

;; Rescaling - preserve precision beyond currency's nominal scale

(sj/json-map->money {:currency "PLN" :amount "12.3456"}
                    {:rescale 4})
#money[12.3456 PLN]  ; Currency has scale 4, not the registry's 2

;; Rescaling during serialization

(sj/money->json-map #money[12.30 PLN] {:rescale 4})
{:currency "PLN", :amount "12.3000"}

;; Currency serialization (minimal by default)

(sj/currency->json-map #currency PLN)
{:id "PLN"}

(sj/currency->json-full-map #currency PLN)
{:id "PLN", :numeric 985, :scale 2, :kind "iso/fiat", :domain "ISO-4217"}

;; :code-only? omits namespace

(sj/money->json-map #money/crypto[1.5 ETH] {:code-only? true})
{:currency "ETH", :amount "1.500000000000000000"}
```

For more complete, runnable examples see the `examples/` directory in the
[source repository](https://github.com/randomseed-io/bankster/tree/main/examples).
