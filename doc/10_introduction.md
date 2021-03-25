# Introduction

**Money Creation Made Easy!**

[![Bankster on Clojars](https://img.shields.io/clojars/v/io.randomseed/bankster.svg)](https://clojars.org/io.randomseed/bankster)
[![CircleCI](https://circleci.com/gh/randomseed-io/bankster.svg?style=svg)](https://circleci.com/gh/randomseed-io/bankster)

Clojure library to operate on monetary units with cryptocurrencies and custom
currencies support.

## Features

* Pure Clojure implementation based on Java's BigDecimal.
* Uses records to organize data: `Registry`, `Currency`, `Money`.
* Built-in standard currencies database, extendable using EDN file.
* Ability to create *ad hoc* currencies (with optional registering).
* Ability to switch between dynamic, global and local currency registries.
* Polymorphic interface for currencies and monetary amounts.
* Ability to cast and convert monetary amounts.
* Useful macros to express currencies and monetary amounts with various forms.
* Namespaced identifiers for non-ISO currencies (e.g. `crypto/ETH`).
* Common math operators which can be used interchangeably with other numeric data.
* Optional rescaling of monetary amounts with keeping track of nominal scales.
* Auto-rescaling of numeric values in math operations to handle non-terminating decimal expansion.
* Tagged literals for currencies and monetary amounts.
* Customizable currency and money formatting with locale support.

## Installation

To use Bankster in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/bankster "1.1.0"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/bankster {:mvn/version "1.1.0"}
```

Additionally, if you want to utilize specs and generators provided by the Bankster
you can use (in your development profile):

```clojure
org.clojure/spec.alpha {:mvn/version "0.2.176"}
org.clojure/test.check {:mvn/version "0.10.0-alpha4"}
```

You can also download JAR from [Clojars](https://clojars.org/io.randomseed/bankster).

## Design

There is a global, shared **registry** of currencies, which is thread-safe and
encapsulated in an Atom. It consists of databases (maps) for quickly accessing
important currency properties. Most of the functions will use this global registry,
unless a registry is explicitly passed as an argument or set as a dynamic variable.

Registry is implemented as a record of maps keeping the following associations:

* currency ID to currency object;
* currency ID to a set of country IDs;
* currency numeric ID to currency object;
* country ID to currency object;
* locale ID to localized properties map;
* currency code to a sorted set of currency objects.

In most cases you won't have to worry about the internals of a registry. However,
when working with multiple data sources or data processing engines (like currency
exchange platforms), you may find it useful to have different registries (with the
same currencies but of different scales).

When the library loads the predefined configuration is read from the default EDN file
and its contents populates the default, global registry.

Each **currency** is a record having the following fields, reflecting its properties:

* `id` – a keyword **identifying** a currency unit (e.g. `:EUR` or `:crypto/ETH`);
* `numeric` – a long value being a **numeric identifier** of ISO-standardized currencies;
* `scale` – an integer of supported **scale** (decimal places);
* `kind` – a keyword with currency **kind**;
* `domain` – a keyword with currency **domain**;
* `weight` – an integer which helps in case there is a conflict of currency codes.

**Currency ID** is a unique identifier of a currency within
a registry. Internally it is a keyword and optionally it can have a namespace. By
default Bankster identifies standard currencies with IDs reflecting their official
currency codes and cryptocurrencies with identifiers having `crypto` namespace.

**Currency code** is a name of currency ID without its namespace. It is potentially
conflicting attribute, therefore the mapping of currency codes to sets of currency
objects exists in a registry. It allows to get currencies using their codes (and not
add namespace, especially when interacting with some external API) and still maintain
uniqueness of identifiers. If custom currency is created with the same code as
already existing currency, it is possible to give it a **weight** which will decide
whether its code will have priority during resolution (and getting from a registry).

**Currency domain** is by default the same as a namespace of currency ID (if it is
namespaced). For non-namespaced identifiers it can be set to anything. The domain of
`:ISO-4217` informs the library that this is ISO-standardized currency. The purpose
of domain is to classify currencies into different realms and create a boundary
preventing naming conflicts when codes are the same.

**Currency kind** is a keyword that should describe a class of currency. It can be
set to anything, even nil, but the suggested values are:

  - `:FIAT`          – legal tender issued by government or other authority,
  - `:FIDUCIARY`     - accepted medium of exchange issued by a fiduciary or fiduciaries,
  - `:DECENTRALIZED` - accepted medium of exchange issued by a distributed ledger,
  - `:COMBANK`       - commercial bank money,
  - `:COMMODITY`     - accepted medium of exchange based on commodities,
  - `:EXPERIMENTAL`  - pseudo-currency used for testing purposes.

**Currency scale** is the nominal scale of a currency. If it is not set, the
automatic scale will be used on monetary amounts using such a currency.

Registry-related functions are accepting currency representations based on their
**identifiers**. Other functions and macros will usually accept **currency
codes**. In case of conflict the currency with higher **currency weight** will be
picked up.

Currencies can also have **additional**, external properties, like relations to
countries, internationalized (i18n) settings etc. They are stored in registries too.

Having currency we can create **money** objects which are based on records having 2
fields:

* `currency` – a `Currency` object;
* `amount` – a `BigDecimal` value.

The initial scale (number of decimal places) of an amount will be set to nominal
scale of the currency. Math operations will then respect this scale during
calculations and preserve it. In rare cases it is possible to rescale the amount,
check whether the monetary object is rescaled and scale it back to a scale of
the currency.

## Sneak peeks

* It **shows information** about a currency:

```clojure
;; global registry lookup with a keyword
(currency/of :PLN)
#currency{:id :PLN, :domain :ISO-4217, :kind :FIAT, :numeric 985, :scale 2}

;; global registry lookup using namespaced symbol
(currency/of crypto/ETH)
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :DECENTRALIZED, :scale 18, :weight 5}

;; global registry lookup with a string (incl. namespace a.k.a domain)
(currency/of "crypto/BTC")
#currency{:id :crypto/BTC, :domain :CRYPTO, :kind :DECENTRALIZED, :scale 8, weight 5}

;; global registry lookup with a currency code
;; (weight solves potential conflicts when two currencies have the same currency code)
(currency/of ETH)
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :DECENTRALIZED, :scale 18, :weight 5}

;; global registry lookup using ISO currency number
(currency/of 840)
#currency{:id :USD, :domain :ISO-4217, :kind :FIAT, :numeric 840, :scale 2}

;; global registry lookup using tagged literal with a currency code
#currency XLM
#currency{:id :crypto/XLM, :domain :CRYPTO, :kind :FIDUCIARY, :scale 8}

;; global registry lookup using tagged literal with a namespaced identifier
#currency crypto/XLM
#currency{:id :crypto/XLM, :domain :CRYPTO, :kind :FIDUCIARY, :scale 8}

;; global registry lookup using tagged literal with an ISO currency number
#currency 978
#currency{:id :EUR, :domain :ISO-4217, :kind :FIAT, :numeric 978, :scale 2}
```

* It allows to **create a currency** and **register it**:

```clojure
;; ad hoc currency creation using constructor function
(currency/new :petro/USD 999 2 :COMBANK)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :numeric 999, :scale 2}

;; ad-hoc currency creation using tagged literal
#currency{:id :crypto/ETH :scale 18}
#currency{:id :crypto/ETH, :domain :CRYPTO, :scale 18}

;; putting new currency into a global, shared registry
(currency/register! (currency/new :petro/USD 9999 2 :COMBANK) :USA)
#Registry[{:currencies 221, :countries 250, :version "2021022121170359"} 0x11efe93f]

;; getting currency from a global registry
(currency/of :petro/USD)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :numeric 9999, :scale 2}

;; registering new currency expressed as a tagged literal
(currency/register! #currency{:id :crypto/AAA :scale 8})
#Registry[{:currencies 221, :countries 249, :version "2021022121170359"} 0x7eaf7a70]
```

* It allows to create **monetary amounts**:

```clojure
;; using money/of macro with keyword ID and an amount
(money/of :EUR 25)
#money[25.00 EUR]

;; using money/of macro with keyword ID and an amount as a first argument
(money/of 25 :EUR)
#money[25.00 EUR]

;; using money/of macro with joint keyword ID and an amount as a first argument
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

;; using tagged literal with a namespace
#money/crypto[1.31337 ETH]
#money/crypto[1.313370000000000000 ETH]

;; using tagged literal with a currency code
#money[1.31337 ETH]
#money/crypto[1.313370000000000000 ETH]

;; using tagged literal with a namespace but the amount goes first
#money/crypto[BTC 1.31337]
#money/crypto[1.31337000 BTC]

;; using default currency in a lexical context
(currency/with EUR (money/of 1000))
#money[1000.00 EUR]

;; using default currency in a lexical context (alias for the above)
(money/with-currency EUR (money/of 1000))
#money[1000.00 EUR]

;; using composed amounts and currencies
#money EUR100
#money [100 EUR]

#money :100_EUR
#money [100 EUR]

#money :100EUR
#money [100 EUR]

#money "100 EUR"
#money [100 EUR]

(money/of "100EUR")
#money [100 EUR]
```

It allows to perform **logical operations** on monetary amounts:

``` clojure
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

(money/is-zero? #money[0 USD])
true

(money/is-neg? #money[-2 XXX])
true

(money/is-pos? #money[-2 XXX])
true
```

It allows to perform **math operations** on monetary amounts:

``` clojure
;; adding money expressed with tagged literals and with a macro call
(money/add #money[EUR 7] #money[0.54 EUR] (money/of 4.40 EUR))
#money[11.94 EUR]

;; dividing money by a number
(money/div #money/crypto[5 USDT] 2)
#money/crypto[2.50000000 USDT]

;; dividing money by numbers that separately would require rounding
(money/div #money[1 GBP] 8 0.5)
#money[0.25 GBP]

;; dividing money by numbers with rounding after each consecutive calculation
(money/with-rescaling HALF_UP
  (money/div #money[1 GBP] 8 0.5))
#money[0.26 GBP]

;; dividing money by money (of the same currency)
(money/div #money/crypto[5 BTC] #money/crypto[2 BTC])
2.5M

;; dividing that causes scale to exceed in one of the steps
(money/div #money[1 PLN] 8 0.5)

;; dividing that causes scale to exceed with rounding
(scale/with-rounding HALF_UP
  (money/div-scaled #money[1 PLN] 8 0.5))
#money[0.26 PLN]

;; handling non-terminating decimal expansion (currency scale)
(scale/with-rounding HALF_UP
  (money/div #money[1 PLN] 3))
#money[0.33 PLN]

;; rounding and unit reduction (currency scale)
(scale/with-rounding HALF_UP
  (money/div #money[1 PLN] #money[3 PLN]))
0.33M

;; rounding and unit reduction (regular numbers, dynamic scale)
(scale/with-rounding HALF_UP
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
#Money[2.00 PLN]

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

;;
;; using inter-ops
;;

(require '[io.randomseed.bankster.money.inter-ops :refer :all])

(+ 1 2 3)
6

(+ #money[USD 8] #money[USD 7.12])
#money[15.12 USD]

(* 1 2 3 4 5 #money/crypto[0.7 ETH])
#money/crypto[84.000000000000000000 ETH]
```

It allows to perform **generic, polymorphic operations** on monetary amounts and
currencies:

```clojure
(scale/of #currency PLN)
2

(scale/of #currency crypto/ETH)
18

(scale/of 123.45)
2

(scale/of #money[100 EUR])
2

(scale/of :GBP)
2

;; scale of the currency (low-level)
(scale/of :XXX)
-1

;; scale of the amount
(scale/of #money[12.34567 XXX])
5 ; current scale

;; nominal scale of the currency
(currency/scale #money[12.34567 XXX])
nil ; auto-scaled

(currency/auto-scaled? :XXX)
true

(currency/auto-scaled? #money[12.34567 XXX])
true

(scale/apply #money[10 USD] 8) ;; use with caution
#money[10.00000000 USD]

(scale/apply #currency USD 8)  ;; use with caution
#currency{:id :USD, :domain :ISO-4217, :kind :FIAT, :numeric 840, :scale 8}

(money/rescale #money[10 USD] 8)
#money[10.00000000 USD]

;; unary variant of money/rescale
;; rescales back to nominal scale
(money/rescale
 (money/rescale #money[10 USD] 8))
#money[10.00 USD]

(scale/amount #money[108.11 CHF])
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

And more…

### Warning about literal amounts

Clojure changes number literals into objects of various numeric data types.
Some of them will have fixed precision when there is a decimal separator
present, yet they will not be big decimals before entering monetary functions of
Bankster.

Putting a decimal number having more than 16–17 digits will often effect in
**accidental approximation** and casting it to a double value. This value may
become the amount of money which probably is not what you want:

```clojure
1234.5678910111213000001
; => 1234.5678910111212
```

To work around that you should:

* Use **big decimal literals** (e.g. `(money/of XXX 1234.56789101112M)` – note the `M`).
* Use **strings** (e.g. `(money/of "1234.56789101112 XXX")`).
* Use `money/of` macro or `#money` tagged literal with amount and currency in joint
  form (or with the above tactics applied), e.g.:
      * `(money/of XXX123.45678)`,
      * `#money XXX123.45678`,
      * `#money "XXX123.45678"`,
      * `#money "123.456789101112 XXX"`,
      * `#money[123.45678M XXX]`.

As it may not be a problem in case of regular currencies, it may pop-up when using
scale-wide cryptocurrencies, like Ether or Ethereum tokens, having 18 decimal places.

## Documentation

Full documentation including usage examples is available at:

* https://randomseed.io/software/bankster/

## Deum Ethereum

I write Free Software for fun. If you are finding it useful and you are the Ether fan
too, here it is: `0x2Bed4D2d9240F9fB321bC0194222A4888F62dd0d`.

Stellar Lumens are cool too:
`GBMUQ6U6334Y5HWF3XGMCRQZKVMFDCSX4YADEVZO7ZXIBDJDXXX2BSME`.

## Why?

In one of my personal projects I needed to support both ISO-standardized and custom
currencies. My first try was Money (by Clojurewerkz), which is quite mature library
based on Java's Joda Money. However, I needed cryptocurrencies support, and I mean
all of them, including those having non-standard codes (like `DASH`).

First I tried to modify Money and work-around this limitation by imitating such
currencies with an additional map translating custom codes into standardized
ones. Then I looked at Joda Money to see that the important classes are marked as
final and the support for currencies is limited to the "official" ones.

## License

Copyright © 2021 Paweł Wilk

Bankster is copyrighted software owned by Paweł Wilk (pw@gnu.org). You may
redistribute and/or modify this software as long as you comply with the terms of
the [GNU Lesser General Public License][LICENSE] (version 3).

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Development

### Building docs

```bash
make docs
```

### Building JAR

```bash
make jar
```

### Rebuilding POM

```bash
make pom
```

### Signing POM

```bash
make sig
```

### Deploying to Clojars

```bash
make deploy
```

### Interactive development

```bash
bin/repl
```

Starts REPL and nREPL server (port number is stored in `.nrepl-port`).

[LICENSE]:    https://github.com/randomseed-io/bankster/blob/master/LICENSE
