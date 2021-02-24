# Bankster
## Currency and Money Processing in Clojure

[![Bankster on Clojars](https://img.shields.io/clojars/v/io.randomseed/bankster.svg)](https://clojars.org/io.randomseed/bankster)

Clojure library to operate on monetary units with cryptocurrencies and other
non-standard currencies support.

**This code is in alpha stage.** It should work but lacks tests, formatting and some
common operations. Please check it in a few days.

## Features

* Pure Clojure implementation based on Java's BigDecimal.

* Built-in standard currencies database, extendable using EDN file.

* Uses records to organize data: `Registry`, `Currency`, `Money`.

* Polymorphic interface for currencies and monetary amounts.

* Namespaced identifiers for non-ISO currencies (e.g. `crypto/ETH`).

* Additional, common operators that can also be used on other numeric data.

* Tagged literals for currencies and monetary amounts.

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
(currency/register! (currency/new :petro/USD 999 2 :COMBANK) :USA)
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
```

It allows to perform **math operations** on monetary amounts:

``` clojure
;; adding money expressed with tagged literals and with a macro call
(money/add #money[EUR 7] #money[0.54 EUR] (money/of 4.40 EUR))
#money[11.94 EUR]

;; dividing money by a number
(money/divide #money/crypto[5 BTC] 2)
#money/crypto[2.50000000 BTC]

;; dividing money by a number of money (of the same currency)
(money/divide #money/crypto[5 BTC] #money/crypto[2 BTC])
2.5M

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

And more…

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

## Documentation

Full documentation including usage examples is available at:

* https://randomseed.io/software/bankster/

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

[![CircleCI](https://circleci.com/gh/randomseed-io/bankster.svg?style=svg)](https://circleci.com/gh/randomseed-io/bankster)

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
