# Currency traits

This is a list of predefined currency traits and their relationships.

Traits are advisory tags stored in a registry separately from currency identity
(see `:traits` and `:hierarchies/:traits` in configuration EDN, e.g. `config.edn` or
`seed.edn`). They can be queried via:

- `io.randomseed.bankster.currency/has-trait?` (exact membership),
- `io.randomseed.bankster.currency/of-trait?` (hierarchy-aware, uses `isa?`).

This document reflects the default traits hierarchy shipped in `seed.edn` (see
`:hierarchies/:traits`).

## `:all`

**Parents:** -
**Meaning:** Global root tag for the entire traits taxonomy.

### `:legacy`

**Parents:** `:all`
**Meaning:** Marks a legacy / obsolete currency (typically ISO legacy, e.g. inferred from
"Old, now ..." comments in Joda Money CSV, or represented with `iso-4217-legacy` ID
namespace / `:ISO-4217-LEGACY` domain). This trait is often auto-added during ISO-like
imports/merges.

### `:token`

**Parents:** `:all`
**Meaning:** Token-like units (typically transferable, often issued on platforms).

#### `:token/fungible`

**Parents:** `:token`
**Meaning:** Fungible tokens (units are interchangeable).

##### `:token/erc20`

**Parents:** `:token/fungible`
**Meaning:** ERC-20 token standard (EVM).

##### `:token/bep20`

**Parents:** `:token/fungible`
**Meaning:** BEP-20 token standard (EVM-compatible, BSC).

### `:stable`

**Parents:** `:all`
**Meaning:** Stable-value units (stability as a trait, not necessarily a kind).

#### `:stable/coin`

**Parents:** `[:stable :token/fungible]`
**Meaning:** Stablecoin (a stable fungible token).

### `:peg`

**Parents:** `:all`
**Meaning:** Peg/anchoring relationship as a trait.

#### `:peg/fiat`

**Parents:** `[:peg :stable/coin]`
**Meaning:** Fiat-pegged stablecoin.

### `:collateral`

**Parents:** `:all`
**Meaning:** Collateralization as a trait.

#### `:collateral/crypto`

**Parents:** `[:collateral :stable/coin]`
**Meaning:** Crypto-collateralized stablecoin.

### `:defi`

**Parents:** `:all`
**Meaning:** DeFi-related traits.

#### `:defi/governance`

**Parents:** `:defi`
**Meaning:** Governance token / governance trait.

#### `:defi/oracle`

**Parents:** `:defi`
**Meaning:** Oracle-related token / oracle trait.

### `:privacy`

**Parents:** `:all`
**Meaning:** Privacy-focused traits.

#### `:privacy/coin`

**Parents:** `:privacy`
**Meaning:** Privacy coin / privacy-oriented unit.

### `:control`

**Parents:** `:all`
**Meaning:** Control/administration model traits.

#### `:control/centralized`

**Parents:** `:control`
**Meaning:** Centralized control model.

#### `:control/decentralized`

**Parents:** `:control`
**Meaning:** Decentralized control model.

#### `:control/federated`

**Parents:** `:control`
**Meaning:** Federated control model.

### `:network`

**Parents:** `:all`
**Meaning:** Network topology traits.

#### `:network/distributed`

**Parents:** `:network`
**Meaning:** Distributed network.

### `:blockchain`

**Parents:** `:all`
**Meaning:** Native blockchain / network membership traits (what chain the unit is native to).

#### `:blockchain/arbitrum`

**Parents:** `:blockchain`
**Meaning:** Arbitrum.

#### `:blockchain/avalanche`

**Parents:** `:blockchain`
**Meaning:** Avalanche.

#### `:blockchain/bitcoin`

**Parents:** `:blockchain`
**Meaning:** Bitcoin.

#### `:blockchain/bitcoin-cash`

**Parents:** `:blockchain`
**Meaning:** Bitcoin Cash.

#### `:blockchain/bitcoin-gold`

**Parents:** `:blockchain`
**Meaning:** Bitcoin Gold.

#### `:blockchain/bitcoin-sv`

**Parents:** `:blockchain`
**Meaning:** Bitcoin SV.

#### `:blockchain/bnb-chain`

**Parents:** `:blockchain`
**Meaning:** BNB Chain.

#### `:blockchain/canton`

**Parents:** `:blockchain`
**Meaning:** Canton Network.

#### `:blockchain/cardano`

**Parents:** `:blockchain`
**Meaning:** Cardano.

#### `:blockchain/cronos`

**Parents:** `:blockchain`
**Meaning:** Cronos.

#### `:blockchain/dash`

**Parents:** `:blockchain`
**Meaning:** Dash.

#### `:blockchain/dogecoin`

**Parents:** `:blockchain`
**Meaning:** Dogecoin.

#### `:blockchain/eos`

**Parents:** `:blockchain`
**Meaning:** EOS.

#### `:blockchain/ethereum`

**Parents:** `:blockchain`
**Meaning:** Ethereum.

#### `:blockchain/ethereum-classic`

**Parents:** `:blockchain`
**Meaning:** Ethereum Classic.

#### `:blockchain/gamecredits`

**Parents:** `:blockchain`
**Meaning:** GameCredits.

#### `:blockchain/hedera`

**Parents:** `:blockchain`
**Meaning:** Hedera.

#### `:blockchain/hyperliquid`

**Parents:** `:blockchain`
**Meaning:** Hyperliquid.

#### `:blockchain/infinity-economics`

**Parents:** `:blockchain`
**Meaning:** Infinity Economics.

#### `:blockchain/kz-cash`

**Parents:** `:blockchain`
**Meaning:** KZ Cash.

#### `:blockchain/lisk`

**Parents:** `:blockchain`
**Meaning:** Lisk.

#### `:blockchain/litecoin`

**Parents:** `:blockchain`
**Meaning:** Litecoin.

#### `:blockchain/mantle`

**Parents:** `:blockchain`
**Meaning:** Mantle.

#### `:blockchain/monero`

**Parents:** `:blockchain`
**Meaning:** Monero.

#### `:blockchain/namecoin`

**Parents:** `:blockchain`
**Meaning:** Namecoin.

#### `:blockchain/polkadot`

**Parents:** `:blockchain`
**Meaning:** Polkadot.

#### `:blockchain/provenance`

**Parents:** `:blockchain`
**Meaning:** Provenance.

#### `:blockchain/solana`

**Parents:** `:blockchain`
**Meaning:** Solana.

#### `:blockchain/steem`

**Parents:** `:blockchain`
**Meaning:** Steem.

#### `:blockchain/stellar`

**Parents:** `:blockchain`
**Meaning:** Stellar.

#### `:blockchain/sui`

**Parents:** `:blockchain`
**Meaning:** Sui.

#### `:blockchain/tezos`

**Parents:** `:blockchain`
**Meaning:** Tezos.

#### `:blockchain/ton`

**Parents:** `:blockchain`
**Meaning:** TON.

#### `:blockchain/tron`

**Parents:** `:blockchain`
**Meaning:** Tron.

#### `:blockchain/vertcoin`

**Parents:** `:blockchain`
**Meaning:** Vertcoin.

#### `:blockchain/xrp-ledger`

**Parents:** `:blockchain`
**Meaning:** XRP Ledger.

#### `:blockchain/zcash`

**Parents:** `:blockchain`
**Meaning:** Zcash.
