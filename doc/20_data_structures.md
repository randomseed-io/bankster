# Data Structures

## Currency

Currency is a record describing currency and having the following fields:

* `id` – a keyword identifying a currency unit (e.g. `:EUR` or `:crypto/ETH`);
* `numeric` – a long value being a numeric identifier of ISO-standardized currencies (e.g. `978` or `-1` for none);
* `scale` – an integer of supported scale (decimal places, e.g. `2` or `-1` for auto);
* `kind` – a case-sensitive keyword with currency kind (may be namespaced, e.g. `:iso/fiat` or `:virtual/native`);
* `domain` – a keyword with currency domain (e.g. `:ISO-4217`, `:ISO-4217-LEGACY` or `:CRYPTO`).

Additionally, currency may carry a non-inherent attribute `weight` (an integer value
used when there are conflicting currency codes or numeric IDs during lookup; defaults
to `0`).

The source of truth for weights is the registry base map `cur-id->weight` (and the
top-level `:weights` branch in EDN configs/exports). Currency instances stored in
registries also carry the weight in metadata as an optimization (hot-path: weighted
buckets), but weight does not participate in `= / hash` semantics. Use
`currency/weight` to read it.

Internally `numeric` can be `-1`, meaning there is no numeric ID and `scale` can be
`-1` too, meaning the amount of currency can have any number of decimal places.

The currency domain is derived from an identifier if the identifier is namespaced
(namespace is upper-cased). The derived code (name part) is upper-cased. Namespace
`ISO-4217` is treated specially: it is stripped (case-insensitive) and implies
`:domain :ISO-4217`. If there is no namespace the domain can be set to any keyword,
with `:ISO-4217` having special meaning in some operations as it marks official
currencies.

See also `doc/30_currency-kinds.md` (kind hierarchy) and `doc/32_currency_traits.md`
(traits hierarchy).

## Registry

Registry is a record describing a database of currencies with associated metadata and
having the following fields:

* `cur-id->cur` – a map of currency ID to currency record;
* `cur-nr->cur` – a map of currency numeric ID to currency record;
* `ctr-id->cur` – a map of country ID to currency record;
* `cur-id->ctr-ids` – a map of currency ID to set of country IDs;
* `cur-id->localized` – a map of currency ID to localized properties map;
* `cur-id->traits` – a map of currency ID to traits (advisory tags);
* `cur-id->weight` – a map of currency ID to weight (integer; conflict resolution);
* `cur-code->curs` – a map of currency code keywords to weighted buckets (sorted sets) of currencies;
* `cur-nr->curs` – a map of currency numeric IDs to weighted buckets (sorted sets) of currencies;
* `cur-dom->curs` – a map of currency domains to weighted buckets (sorted sets) of currencies;
* `hierarchies` – a record holding hierarchies for currency classification axes (e.g. `:domain`, `:kind`, `:traits`);
* `version` – a string with registry version;
* `ext` – a map with extra registry data.

Derived index maps (`cur-code->curs`, `cur-nr->cur`, `cur-nr->curs`, `cur-dom->curs`,
`cur-id->ctr-ids`) are built during registry initialization. `registry/new-registry`
accepts base maps; even the arity that accepts a Registry map/record ignores any
derived index fields (they are recomputed during initialization).

## Money

Money is a record describing a monetary value and having the following fields:

* `currency` – a Currency object;
* `amount` – an amount of currency stored as a `BigDecimal` number.
