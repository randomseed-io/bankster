# Data Structures

## Currency

Currency is a record describing currency and having the following fields:

* `id` – a keyword identifying a currency unit (e.g. `:EUR` or `:crypto/ETH`);
* `numeric` – a long value being a numeric identifier of ISO-standardized currencies (e.g. `978` or `-1` for none);
* `scale` – an integer of supported scale (decimal places, e.g. `2` or `-1` for auto);
* `kind` – a keyword with currency kind (e.g. `:FIAT` or `:DECENTRALIZED`);
* `domain` – a keyword with currency domain (e.g. `:ISO-4217` or `:CRYPTO`);
* `weight` – an integer value used when there are conflicting currency codes during lookup (defaults to `0`).

Internally `numeric` can be `-1`, meaning there is no numeric ID and `scale` can be
`-1` too, meaning the amount of currency can have any number of decimal places.

The currency domain is derived from an identifier if the identifier is
namespaced. The derived ID is upper-cased. If there is no namespace the domain can be
set to any keyword, with `:ISO-4217` having special meaning in some operations as it
marks official currencies.

## Registry

Registry is a record describing a database of currencies with associated metdata and
having the following fields:

* `cur-id->cur` – a map of currency ID to currency record;
* `cur-nr->cur` – a map of currency numeric ID to currency record;
* `ctr-id->cur` – a map of country ID to currency record;
* `cur-id->ctr-ids` – a map of currency ID to set of country IDs;
* `cur-id->localized` – a map of locale ID to localized properties;
* `cur-code->currencies` – a map of currency codes to sorted sets of currencies;
* `version` – an optional string with registry version.

## Money

Money is a record describing a monetary value and having the following fields:

* `currency` – a Currency object;
* `amount` – an amount of currency stored as a `BigDecimal` number.

