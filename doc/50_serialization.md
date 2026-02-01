# Serialization

Bankster provides per-format serialization protocols for JSON and EDN, with a clean
separation between minimal and full representations.

## Namespaces

| Namespace | Purpose |
|-----------|---------|
| `io.randomseed.bankster.serializers.json` | JSON serialization (strings for amounts, string IDs) |
| `io.randomseed.bankster.serializers.edn` | EDN serialization (BigDecimals, keyword IDs, tagged literals) |

## JSON protocols

```clojure
(defprotocol JsonSerializable
  (to-json-map    [this] [this opts])   ; minimal or full map
  (to-json-full-map [this] [this opts]) ; always full map
  (to-json-string [this] [this opts]))  ; canonical string

(defprotocol JsonDeserializable
  (from-json-map    [type-token m] [type-token m opts])
  (from-json-string [type-token s] [type-token s opts]))
```

## EDN protocols

```clojure
(defprotocol EdnSerializable
  (to-edn-map     [this] [this opts])   ; minimal or full map
  (to-edn-full-map [this] [this opts])  ; always full map
  (to-edn-string  [this] [this opts]))  ; tagged literal

(defprotocol EdnDeserializable
  (from-edn-map    [type-token m] [type-token m opts])
  (from-edn-string [type-token s] [type-token s opts]))
```

## Minimal vs full serialization

By default, serialization produces **minimal** output:

| Type | JSON minimal | EDN minimal |
|------|-------------|-------------|
| `Currency` | `{:id "PLN"}` | `{:id :PLN}` |
| `Money` | `{:currency "PLN", :amount "12.30"}` | `{:currency :PLN, :amount 12.30M}` |

Full serialization (`to-*-full-map` or `:full? true`) includes all fields:

| Type | JSON full | EDN full |
|------|-----------|----------|
| `Currency` | `{:id, :numeric, :scale, :kind, :domain}` | `{:id, :numeric, :scale, :kind, :domain}` |
| `Money` | `{:currency {...}, :amount}` + extended fields | `{:currency {...}, :amount}` + extended fields |

## Options

| Key | Type | Context | Description |
|-----|------|---------|-------------|
| `:code-only?` | boolean | ser | Omit namespace: `:crypto/ETH` → `"ETH"` (JSON) / `:ETH` (EDN) |
| `:full?` | boolean | ser | Delegate `to-*-map` to `to-*-full-map` |
| `:keys` | vector | ser | Filter output keys; supports nested opts for recursive serialization |
| `:rescale` | integer | ser/deser | Scale to apply to amounts (see Rescaling section) |
| `:registry` | Registry | deser | Registry for deserialization (default: `registry/get`) |
| `:rounding-mode` | RoundingMode/keyword/string | ser/deser | Rounding mode for rescaling |

### Rounding mode formats

The `:rounding-mode` option accepts multiple formats:
- `java.math.RoundingMode` object: `RoundingMode/HALF_UP`
- Keyword: `:HALF_UP`, `:HALF_DOWN`, `:CEILING`, etc.
- String: `"HALF_UP"`, `"ROUND_HALF_UP"`

When not specified, falls back to `scale/*rounding-mode*` dynamic var.

## Nested `:keys` filtering

The `:keys` option supports nested options via map elements:

```clojure
;; Filter Money to only :amount and currency's :id/:numeric
(to-json-full-map money {:keys [:amount {:currency {:keys [:id :numeric]}}]})
;; => {:amount "12.30", :currency {:id "PLN", :numeric 985}}
```

Syntax for `:keys` vector:
- Keyword elements select that field without recursion
- Map elements `{:field-name {:keys [...]}}` select that field with nested options

## Extended fields

Money records may carry extended fields (via `assoc`). Full serialization includes
them:

```clojure
(def m (assoc (money/of :PLN 100) :memo "Payment"))
(to-json-full-map m)
;; => {:currency {...}, :amount "100.00", :memo "Payment"}
```

Extended fields in JSON are stringified (BigDecimal → `.toPlainString`, keyword → `name`,
other → `str`). In EDN they are preserved as-is.

## Format semantics

| Aspect | JSON | EDN |
|--------|------|-----|
| Currency ID | String (`"PLN"`, `"crypto/ETH"`) | Keyword (`:PLN`, `:crypto/ETH`) |
| Amount | String (`.toPlainString`) | BigDecimal |
| Tagged literal | N/A | `#money[12.30M PLN]`, `#money/crypto[1.5M ETH]` |
| Kind | String with namespace (`"iso/fiat"`) | Keyword with namespace (`:iso/fiat`) |
| Domain | String (`"ISO-4217"`) | Keyword (`:ISO-4217`) |

## Convenience functions

Both namespaces expose helper functions that mirror protocol methods:

```clojure
;; JSON
(money->json-map m)          ; minimal
(money->json-full-map m)     ; full
(money->json-string m)       ; "12.30 PLN"
(json-map->money m)
(json-string->money s)
(currency->json-map c)       ; minimal {:id "PLN"}
(currency->json-full-map c)  ; full
(currency->json-string c)    ; "PLN"
(json-map->currency m)
(json-string->currency s)

;; EDN
(money->edn-map m)           ; minimal
(money->edn-full-map m)      ; full
(money->edn-string m)        ; "#money[12.30M PLN]"
(edn-map->money m)
(edn-string->money s)
(currency->edn-map c)        ; minimal {:id :PLN}
(currency->edn-full-map c)   ; full
(currency->edn-string c)     ; "#currency :PLN"
(edn-map->currency m)
(edn-string->currency s)
(edn-keyword->currency k)
```

## Cheshire integration (JSON)

```clojure
(require '[io.randomseed.bankster.serializers.json :as sj])

;; Register Money encoder with Cheshire (map representation)
(sj/register-cheshire-codecs!)

;; Or use string representation
(sj/register-cheshire-codecs! {:representation :string})

;; With :code-only? option
(sj/register-cheshire-codecs! {:code-only? true})
```

Note: Cheshire registration affects encoding only. Decoding remains explicit via
`json-map->money` / `json-string->money`.

## Codec helpers

Both namespaces provide `money-codec` for building encode/decode function pairs:

```clojure
(let [{:keys [encode decode]} (sj/money-codec {:representation :map
                                               :code-only? true
                                               :registry my-reg
                                               :rounding-mode RoundingMode/HALF_UP})]
  (encode money)   ; -> map or string
  (decode data))   ; -> Money
```

The codec returns a map with:
- `:encode` - function `(Money -> json-value)`
- `:decode` - function `(json-value -> Money)`
- `:representation` - `:map` or `:string`

## Usage examples

### Basic serialization

```clojure
(require '[io.randomseed.bankster.serializers.json :as sj]
         '[io.randomseed.bankster.serializers.edn  :as se])

;; JSON minimal (default)
(sj/money->json-map #money[12.30 PLN])
;; => {:currency "PLN", :amount "12.30"}

;; JSON full
(sj/money->json-full-map #money[12.30 PLN])
;; => {:currency {:id "PLN", :numeric 985, :scale 2, :kind "iso/fiat", :domain "ISO-4217"},
;;     :amount "12.30"}

;; EDN minimal
(se/money->edn-map #money[12.30 PLN])
;; => {:currency :PLN, :amount 12.30M}

;; EDN tagged literal
(se/money->edn-string #money[12.30 PLN])
;; => "#money[12.30M PLN]"

(se/money->edn-string #money/crypto[1.5 ETH])
;; => "#money/crypto[1.500000000000000000M ETH]"
```

### Deserialization

```clojure
(sj/json-map->money {:currency "PLN" :amount "12.30"})
;; => #money[12.30 PLN]

(sj/json-string->money "12.30 PLN")
;; => #money[12.30 PLN]

(se/edn-string->money "#money[12.30M PLN]")
;; => #money[12.30 PLN]

;; With custom registry and rounding
(sj/json-map->money {:currency "PLN" :amount "1.005"}
                    {:rounding-mode java.math.RoundingMode/HALF_UP})
;; => #money[1.01 PLN]
```

### Using :full? option

```clojure
;; Via to-json-map with :full? true
(sj/to-json-map #money[12.30 PLN] {:full? true})
;; => {:currency {:id "PLN", ...}, :amount "12.30"}

;; Via protocol
(sj/to-json-map (currency/of :PLN) {:full? true})
;; => {:id "PLN", :numeric 985, :scale 2, :kind "iso/fiat", :domain "ISO-4217"}
```

### Filtering with :keys

```clojure
;; Select specific fields
(sj/money->json-full-map #money[12.30 PLN] {:keys [:amount {:currency {:keys [:id :numeric]}}]})
;; => {:amount "12.30", :currency {:id "PLN", :numeric 985}}

;; Currency only
(sj/currency->json-full-map (currency/of :PLN) {:keys [:id :scale]})
;; => {:id "PLN", :scale 2}
```

### Code-only mode

```clojure
;; Omit namespace in currency IDs
(sj/money->json-map #money/crypto[1.5 ETH] {:code-only? true})
;; => {:currency "ETH", :amount "1.500000000000000000"}

(se/money->edn-string #money/crypto[1.5 ETH] {:code-only? true})
;; => "#money[1.500000000000000000M ETH]"
```

### Rescaling

The `:rescale` option allows serializing/deserializing Money with a different scale
than the currency's nominal scale.

#### Serialization with `:rescale`

Rescale amounts before outputting:

```clojure
;; Upscale PLN (scale 2) to 4 decimal places
(sj/money->json-map #money[12.30 PLN] {:rescale 4})
;; => {:currency "PLN", :amount "12.3000"}

;; Downscale requires rounding mode (or throws ArithmeticException)
(sj/money->json-map #money/crypto[1.12345 ETH] {:rescale 2 :rounding-mode :HALF_UP})
;; => {:currency "crypto/ETH", :amount "1.12"}

;; Works with all serialization functions
(sj/money->json-string #money[12.30 PLN] {:rescale 4})
;; => "12.3000 PLN"
```

#### Deserialization with `:rescale`

Preserve precision when the incoming data has more decimal places than the registry
currency. Without `:rescale`, the amount would be truncated to the currency's nominal
scale, potentially losing data.

```clojure
;; PLN has scale 2, but incoming data has 4 decimal places
;; Without :rescale - data loss!
(sj/json-map->money {:currency "PLN" :amount "12.3456"})
;; => ArithmeticException (or truncated if rounding-mode set)

;; With :rescale - precision preserved
(sj/json-map->money {:currency "PLN" :amount "12.3456"} {:rescale 4})
;; => #money[12.3456 PLN] (Currency has scale 4, not the registry's 2)

;; The resulting Money has a Currency with the custom scale
(let [m (sj/json-map->money {:currency "PLN" :amount "12.3456"} {:rescale 4})]
  (.scale (currency/of m)))
;; => 4
```

#### How `:rescale` works in deserialization

1. Currency is looked up from the registry (by ID)
2. The Currency object is cloned with the `:rescale` value as its scale
3. Money is created with this modified Currency

This means the resulting Money object carries a Currency with a non-standard scale.
This is intentional - it preserves all information from the wire format without
data loss.

#### Use cases for `:rescale`

- **APIs with variable precision**: When external systems send amounts with more
  precision than your currency's default scale
- **Cross-system communication**: When systems have different scale conventions
- **Migration scenarios**: When converting from one scale to another
- **Display formatting**: When you need amounts in a specific format for display
