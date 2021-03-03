# Introduction

## Bankster /ˈbæŋkstə/

**Money Creation Made Easy!**

[![Bankster on Clojars](https://img.shields.io/clojars/v/io.randomseed/bankster.svg)](https://clojars.org/io.randomseed/bankster)
[![CircleCI](https://circleci.com/gh/randomseed-io/bankster.svg?style=svg)](https://circleci.com/gh/randomseed-io/bankster)

Clojure library to operate on monetary units with cryptocurrencies and custom
currencies support.

**This code is in alpha stage.** It should work but lacks some tests, string
formatting functions and some common operations. Please check it in a few days.

## Features

* Pure Clojure implementation based on Java's BigDecimal.
* Built-in standard currencies database, extendable using EDN file.
* Uses records to organize data: `Registry`, `Currency`, `Money`.
* Polymorphic interface for currencies and monetary amounts.
* Namespaced identifiers for non-ISO currencies (e.g. `crypto/ETH`).
* Additional, common operators that can also be used on other numeric data.
* Tagged literals for currencies and monetary amounts.

## Installation

To use Bankster in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/bankster "1.0.0"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/bankster {:mvn/version "1.0.0"}
```

Additionally, if you want to utilize specs and generators provided by the Bankster
you can use (in your development profile):

```clojure
org.clojure/spec.alpha {:mvn/version "0.2.176"}
org.clojure/test.check {:mvn/version "0.10.0-alpha4"}
```

You can also download JAR from [Clojars](https://clojars.org/io.randomseed/bankster).

 ## Sneak peeks

* It **shows information** about a currency:

```clojure
;; global registry lookup with a keyword
(currency/of :PLN)
#currency{:id :PLN, :domain :ISO-4217, :kind :FIAT, :nr 985, :sc 2}

;; global registry lookup with a string (incl. namespace a.k.a domain)
(currency/of "crypto/BTC")
#currency{:id :crypto/BTC, :domain :CRYPTO, :kind :DECENTRALIZED, :sc 8}

;; global registry lookup using namespaced symbol
(currency/of 'crypto/ETH)
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :DECENTRALIZED, :sc 18}

;; global registry lookup using ISO currency number
(currency/of 840)
#currency{:id :USD, :domain :ISO-4217, :kind :FIAT, :nr 840, :sc 2}

;; global registry lookup using tagged literal with a namespace
#currency crypto/XLM
#currency{:id :crypto/XLM, :domain :CRYPTO, :kind :FIDUCIARY, :sc 8}

;; global registry lookup using tagged literal with an ISO currency number
#currency 978
#currency{:id :EUR, :domain :ISO-4217, :kind :FIAT, :nr 978, :sc 2}

;; global registry lookup using money tagged literal without an amount
#money EUR
#currency{:id :EUR, :domain :ISO-4217, :kind :FIAT, :nr 978, :sc 2}

;; global registry lookup using namespaced money tagged literal
#money/crypto ETH
#currency{:id :crypto/ETH, :domain :CRYPTO, :kind :DECENTRALIZED, :sc 18}
```

* It allows to **create a currency** and **register it**:

```clojure
;; getting currency from a global registry
(currency/of :petro/USD)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :nr 999, :sc 2}

;; ad-hoc currency creation using constructor function
(currency/new :petro/USD 999 2 :COMBANK)
#currency{:id :petro/USD, :domain :PETRO, :kind :COMBANK, :nr 999, :sc 2}

;; ad-hoc currency creation using tagged literal
#currency{:id :crypto/ETH :sc 18}
#currency{:id :crypto/ETH, :domain :CRYPTO, :sc 18}

;; putting new currency into a global registry
(currency/register! (currency/new :petro/USD 9999 2 :COMBANK) :USA)
#Registry@11efe93f{:currencies 221, :countries 250, :version "2021022121170359"}

;; registering new currency expressed as a tagged literal
(currency/register! #currency{:id :crypto/AAA :sc 8})
#Registry@7eaf7a70{:currencies 221, :countries 249, :version "2021022121170359"}
```

* It allows to create **monetary amounts**:

```clojure
;; using money/of macro with keyword ID and an amount
(money/of :EUR 25)
#money[25.00 EUR]

;; using money/of macro with keyword ID and an amount as a first argument
(money/of 25 :EUR)
#money[25.00 EUR]

;; using money/of macro with unquoted symbolic ID and an amount
(money/of EUR 25)
#money[25.00 EUR]

;; using money/of macro with namespaced keyword ID and an amount
(money/of crypto/BTC 10.1)
#money/crypto[10.10000000 BTC]

;; using tagged literal
#money[PLN 2.50]
#money[2.50 PLN]

;; using tagged literal with a namespace
#money/crypto[1.31337 ETH]
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
(sort money/compare-amounts [(money/of 10    PLN)
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

(scale/of :XXX)
-1

(scale/of #money[12.34567 XXX])
5

(currency/auto-scaled? :XXX)
true

(currency/auto-scaled? #money[12.34567 XXX])
true

(scale/apply #money[10 USD] 8) ;; use with caution or better avoid
#money[10.00000000 USD]

(scale/apply #currency USD 8)  ;; use with caution or better avoid
#currency{:id :USD, :domain :ISO-4217, :kind :FIAT, :nr 840, :sc 8}

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

[LICENSE]:    https://github.com/randomseed-io/bankster/blob/master/LICENSE
