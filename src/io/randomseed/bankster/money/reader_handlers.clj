;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(in-ns 'io.randomseed.bankster.money)

(defn
 code-literal-iso-4217-legacy
 {:no-doc true}
 [arg]
 (ns-code-literal "iso-4217-legacy" arg))

(defn code-literal-crypto {:no-doc true} [arg] (ns-code-literal "crypto" arg))

(defn
 data-literal-iso-4217-legacy
 {:no-doc true}
 [arg]
 (ns-data-literal "iso-4217-legacy" arg))

(defn data-literal-crypto {:no-doc true} [arg] (ns-data-literal "crypto" arg))