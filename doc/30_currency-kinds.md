# Currency kinds

This is a list of predefined currency kinds and their relationships.

## `:all`

**Parents:** -
**Meaning:** Global root tag for the entire currency kind taxonomy.

### `:iso`

**Parents:** `:all`  
**Meaning:** Root for ISO-oriented classifications (ISO-4217 and related ISO "special" codes).

#### `:iso/currency`

**Parents:** `[:iso :currency]`  
**Meaning:** Any ISO currency-like unit, classified both as ISO and as "currency" in the meta layer.

##### `:iso/fiduciary`

**Parents:** `[:iso/currency :fiduciary]`  
**Meaning:** ISO currency units that are fiduciary in nature (accepted money issued by a trusted authority/issuer).

###### `:iso/fiat`

**Parents:** `[:iso/fiduciary :fiat/issuer]`  
**Meaning:** ISO fiat currencies (legal tender / sovereign-issued money), explicitly mapped to the "issuer fiat" meta-branch.

#### `:iso/funds`

**Parents:** `[:iso :funds]`  
**Meaning:** ISO "funds / settlement units / units of account" category (e.g., codes used for specific financial or settlement contexts).

##### `:iso.funds/international`

**Parents:** `:iso/funds`  
**Meaning:** ISO funds used in international contexts (e.g., cross-border or supranational settlement references).

##### `:iso.funds/institutional`

**Parents:** `:iso/funds`  
**Meaning:** ISO funds used by institutions (e.g., internal or institutional settlement/accounting units).

##### `:iso.funds/market`

**Parents:** `:iso/funds`  
**Meaning:** ISO funds used primarily in market contexts (trading/market conventions).

##### `:iso.funds/settlement`

**Parents:** `:iso/funds`  
**Meaning:** ISO funds used explicitly as settlement units.

###### `:iso.funds.settlement/combanks`

**Parents:** `:iso.funds/settlement`  
**Meaning:** Commercial bank money / settlement instruments categorized under ISO settlement funds.

#### `:iso/commodity`

**Parents:** `[:iso :commodity]`  
**Meaning:** ISO commodity-denominated units (commodity-based mediums of exchange or accounting units).

##### `:iso/metal`

**Parents:** `[:iso/commodity :metal]`  
**Meaning:** ISO commodity units specifically based on metals (e.g., precious metal codes).

#### `:iso/special`

**Parents:** `[:iso :special]`  
**Meaning:** ISO "special" codes and markers (non-standard monetary placeholders, test codes, no-currency markers).

##### `:iso/experimental`

**Parents:** `[:iso/special :experimental]`  
**Meaning:** ISO experimental/test-like codes used for controlled scenarios or non-production contexts.

###### `:iso/test`

**Parents:** `[:iso/experimental :test]`  
**Meaning:** ISO test codes explicitly intended for testing and simulations.

##### `:iso/null`

**Parents:** `[:iso/special :NULL]`  
**Meaning:** ISO "no currency" marker within the ISO branch (e.g., "no currency involved").

---

### `:virtual`

**Parents:** `:all`  
**Meaning:** Root for non-ISO / virtual monetary units (crypto-assets, platform credits, loyalty points, etc.).

#### `:virtual/stable`

**Parents:** `[:virtual :stable]`  
**Meaning:** Stable-value virtual units (stablecoins / stable tokens) as a kind of virtual asset.

##### `:virtual.stable/peg`

**Parents:** `[:virtual/stable :peg]`  
**Meaning:** Stable tokens whose stability is achieved through a peg (value anchored to a reference).

###### `:virtual.stable.peg/fiat`

**Parents:** `[:virtual.stable/peg :fiat/referenced]`  
**Meaning:** Fiat-referenced stable tokens (pegged to a fiat anchor such as USD/EUR, without being fiat issuer money).

###### `:virtual.stable.peg/asset`

**Parents:** `[:virtual.stable/peg :asset/referenced]`  
**Meaning:** Asset-referenced stable tokens (pegged to an asset or basket of assets, broadly understood).

###### `:virtual.stable.peg/commodity`

**Parents:** `[:virtual.stable/peg :commodity/referenced]`  
**Meaning:** Commodity-referenced stable tokens (pegged to commodity values).

###### `:virtual.stable.peg/metal`

**Parents:** `[:virtual.stable/peg :metal/referenced]`  
**Meaning:** Metal-referenced stable tokens (pegged to metal values such as gold/silver).

#### `:virtual/credit`

**Parents:** `[:virtual :credit]`  
**Meaning:** Virtual credit/IOU units (platform balances, exchange credits, loyalty points), modeled as claims.

##### `:virtual.credit/platform`

**Parents:** `:virtual/credit`  
**Meaning:** Credits issued/managed by a platform (e.g., app/service credits).

##### `:virtual.credit/exchange`

**Parents:** `:virtual/credit`  
**Meaning:** Credits issued/managed by an exchange or trading venue.

##### `:virtual.credit/loyalty`

**Parents:** `:virtual/credit`  
**Meaning:** Loyalty points or loyalty-style credits (program-issued virtual value units).

#### `:virtual/native`

**Parents:** `[:virtual :asset]`  
**Meaning:** Native virtual assets of an ecosystem (e.g., a network's primary token), modeled as assets but not necessarily stable or credit/claim units.

#### `:virtual/token`

**Parents:** `[:virtual :asset]`  
**Meaning:** Virtual tokens (typically fungible units issued on top of a platform or a base asset), modeled as assets.

#### `:virtual/staked`

**Parents:** `[:virtual :staked]`  
**Meaning:** Staked / staking-derivative virtual units (claims representing staked positions), modeled as claims in the virtual branch.

#### `:virtual/wrapped`

**Parents:** `[:virtual :wrapped]`  
**Meaning:** Wrapped virtual units (claims representing wrapped assets), modeled as claims in the virtual branch.

#### `:virtual/special`

**Parents:** `[:virtual :special]`  
**Meaning:** Special-purpose markers within the virtual branch (test, null/no-currency, etc.).

##### `:virtual/experimental`

**Parents:** `[:virtual/special :experimental]`  
**Meaning:** Virtual experimental/test-only units (non-production, simulated, sandbox).

##### `:virtual/null`

**Parents:** `[:virtual/special :NULL]`  
**Meaning:** Virtual "no currency" marker (no currency involved in a virtual context).

---

### `:currency`

**Parents:** `:all`  
**Meaning:** Meta tag for "currency-like" units (broad classification; used as a parent for fiduciary money).

#### `:fiduciary`

**Parents:** `:currency`  
**Meaning:** Fiduciary money: accepted money issued by an identifiable fiduciary/issuer.

---

### `:asset`

**Parents:** `:all`  
**Meaning:** Meta tag for value-bearing units that can be priced/held/transferred (broad "asset-like" classification).

#### `:reference`

**Parents:** `:asset`  
**Meaning:** Meta tag for reference/anchor concepts used to define or stabilize value (e.g., pegs, numeraires).

##### `:asset/reference`

**Parents:** `:reference`  
**Meaning:** A reference specifically understood as "asset reference" (an anchor in terms of an asset's value).

##### `:peg`

**Parents:** `:reference`  
**Meaning:** A pegging relationship: value anchoring to a reference (typically via a stability mechanism, not modeled here).

#### `:asset/referenced`

**Parents:** `[:asset :reference]`  
**Meaning:** An asset whose nature is reference-based (i.e., its value is anchored to some reference).

#### `:stable`

**Parents:** `:asset`  
**Meaning:** Meta tag for stable-value assets (stability as a defining property of the asset).

#### `:claim`

**Parents:** `:asset`  
**Meaning:** Meta tag for claim-like assets (rights/IOUs/receivables-something owed by an issuer or system).

##### `:credit`

**Parents:** `:claim`  
**Meaning:** Credits as claims (platform credits, exchange credits, loyalty credits).

##### `:staked`

**Parents:** `:claim`  
**Meaning:** Staked / staking-derivative claim units (claims representing staked positions).

##### `:wrapped`

**Parents:** `:claim`  
**Meaning:** Wrapped claim units (claims representing wrapped assets).

---

### `:fiat`

**Parents:** `:all`  
**Meaning:** Fiat-related umbrella tag (topic-level), intentionally separated from issuer fiat vs anchor fiat.

#### `:fiat/issuer`

**Parents:** `[:fiat :fiduciary]`  
**Meaning:** Issuer fiat: real fiduciary money issued by a sovereign/monetary authority (issuer-centric fiat).

##### `:fiat/legal-tender`

**Parents:** `:fiat/issuer`  
**Meaning:** Legal tender fiat: formally recognized by law as acceptable for settling debts/public charges.

#### `:fiat/anchor`

**Parents:** `[:fiat :asset/reference]`  
**Meaning:** Fiat-as-anchor: fiat used strictly as a value anchor/numeraire (reference), not as issuer money.

##### `:fiat/reference`

**Parents:** `:fiat/anchor`  
**Meaning:** Compatibility alias for fiat-as-anchor (reference role), retained for API stability.

##### `:fiat/referenced`

**Parents:** `[:asset/referenced :fiat/anchor]`  
**Meaning:** Fiat-referenced assets: assets pegged/anchored to fiat value (e.g., USD-referenced stable tokens).

---

### `:funds`

**Parents:** `:all`  
**Meaning:** Meta tag for funds/settlement units/units of account (generally "accounting/settlement" nature).

---

### `:commodity`

**Parents:** `:all`  
**Meaning:** Meta tag for commodity-based monetary units or commodity-denominated units.

#### `:commodity/reference`

**Parents:** `[:commodity :asset/reference]`  
**Meaning:** Commodity-as-anchor: commodity used as a value anchor/reference (e.g., commodity numeraire).

#### `:commodity/referenced`

**Parents:** `:asset/referenced`  
**Meaning:** Commodity-referenced assets: assets pegged/anchored to commodity value.

#### `:metal`

**Parents:** `:commodity`  
**Meaning:** Meta tag for metal-based commodity units (precious metals, etc.).

##### `:metal/reference`

**Parents:** `[:metal :commodity/reference]`  
**Meaning:** Metal-as-anchor: metal used as a value anchor/reference.

##### `:metal/referenced`

**Parents:** `:commodity/referenced`  
**Meaning:** Metal-referenced assets: assets pegged/anchored to metal value.

---

### `:special`

**Parents:** `:all`  
**Meaning:** Meta tag for special-purpose markers and non-economic placeholders.

#### `:experimental`

**Parents:** `:special`  
**Meaning:** Marker for experimental units (non-production semantics).

##### `:test`

**Parents:** `:experimental`  
**Meaning:** Marker for explicit testing-only units.

#### `:NULL`

**Parents:** `:special`  
**Meaning:** Global "no currency" marker (represents absence of a currency).

##### `:nil`

**Parents:** `:NULL`  
**Meaning:** Alias marker representing "missing / nil-like absence" as a Named tag (useful for mapping `nil` -> `:nil`).

##### `:none`

**Parents:** `:NULL`  
**Meaning:** Alias marker representing "explicit none" as a Named tag (useful for UIs or explicit user intent).
