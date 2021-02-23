# Bankster
## Currency and Money Processing in Clojure

[![Bankster on Clojars](https://img.shields.io/clojars/v/io.randomseed/bankster.svg)](https://clojars.org/io.randomseed/bankster)

Clojure library to operate on monetary units with cryptocurrencies and other
non-standard currencies support.

## Features

* Pure Clojure implementation based on Java's BigDecimal.

* Built-in standard currencies database, extendable using EDN file.

* Uses records to organize data: Registry, Currency, Money.

* Polymorphic interface for currencies and monetary amounts.

* Namespaced identifiers for non-ISO currencies (e.g. `crypto/ETH`).

* Additional, common operators that can also be used on other numeric data.

* Tagged literals for monetary amounts.

## Sneak peeks

* It **shows information** about a currency:

```clojure
(currency/of :PLN)
#currency{:id :PLN, :ns :ISO-4217, :kind :FIAT, :nr 985, :sc 2}

(currency/of "crypto/BTC")
#currency{:id :crypto/BTC, :ns :CRYPTO, :kind :DECENTRALIZED, :sc 8}

(currency/of 'crypto/ETH)
#currency{:id :crypto/ETH, :ns :CRYPTO, :kind :DECENTRALIZED, :sc 18}

(currency/of 840)
currency{:id :USD, :ns :ISO-4217, :kind :FIAT, :nr 840, :sc 2}
```

* It allows to **create a currency** and **register it**:

```clojure
(currency/new :petro/USD 999 2 :COMBANK)
#currency{:id :petro/USD, :ns :PETRO, :kind :COMBANK, :nr 999, :sc 2}

(currency/register! (currency/new :petro/USD 999 2 :COMBANK) :USA)
#Registry@11efe93f[221 currencies, 250 countries, version: 2021022121170359]

(currency/of :petro/USD)
#currency{:id :petro/USD, :ns :PETRO, :kind :COMBANK, :nr 999, :sc 2}
```

* It allows to create **monetary amounts** with a currency:

``` clojure
(money/of :EUR 25)
#money[25.00 EUR]

(money/of EUR 25)
#money[25.00 EUR]

(money/of 25 :EUR)
#money[25.00 EUR]

(money/of crypto/BTC 10.1)
#money/crypto[10.10000000 BTC]

#money/crypto[1.31337 ETH]
#money/crypto[1.313370000000000000 ETH]

#money/crypto[BTC 1.31337]
#money/crypto[1.31337000 BTC]

#money[PLN 2.50]
#money[2.50 PLN]
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
