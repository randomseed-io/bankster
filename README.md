# Bankster
## Currency and Money Processing in Clojure

[![Bankster on Clojars](https://img.shields.io/clojars/v/io.randomseed/bankster.svg)](https://clojars.org/io.randomseed/bankster)

Clojure library to operate on monetary units with cryptocurrency and other
non-standard token support.

## Features

* Pure Clojure implementation.

* Polymorphic interface (registered currencies can be expressed as currency records,
  numbers, strings or keywords).

* Supported operations: calculation, validation, matching, formatting.

* Namespaced identifiers for non-ISO currencies (e.g. `crypto/ETH`).

## Sneak peeks

* It **shows information** about a currency:

```clojure
(require '[io.randomseed.bankster.currency :as currency])

TBD
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
