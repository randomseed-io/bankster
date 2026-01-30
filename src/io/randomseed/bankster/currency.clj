(ns

    ^{:doc    "Bankster library, currency operations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.currency

  (:refer-clojure :rename {ns      core-ns
                           new     core-new
                           symbol  core-symbol
                           name    core-name
                           update  core-update
                           resolve core-resolve})

  (:require [trptr.java-wrapper.locale       :as        l]
            [smangler.api                    :as       sm]
            [clojure.string                  :as      str]
            [io.randomseed.bankster]
            [io.randomseed.bankster.config   :as   config]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.scale    :as    scale]
            [io.randomseed.bankster.util.map :as      map]
            [io.randomseed.bankster.util     :as       bu])

  (:import  (io.randomseed.bankster Currency
                                    CurrencyHierarchies
                                    Registry)
            (java.math              RoundingMode)
            (java.text              NumberFormat
                                    DecimalFormat
                                    DecimalFormatSymbols)
            (java.util              Locale)))

;;
;; Constants.
;;

(def ^{:tag 'long :const true :added "1.0.0"}
  ^long no-numeric-id
  "Expresses the value of currency's numeric ID which does not exist."
  (long -1))

(def ^{:tag 'int :const true :added "1.0.0"}
  ^int auto-scaled
  "Expresses the scale of a currency which is automatic and not limited to certain
  decimal places."
  (int -1))

(def ^{:tag clojure.lang.Keyword :const true :private true :added "3.0.0"}
  ^clojure.lang.Keyword explicit-nil
  "Internal sentinel used to express explicit `nil` values coming from currency maps.

  It is used to distinguish between \"value not provided\" (which may trigger
  inference/defaulting) and \"value explicitly set to nil\" (which should be trusted
  and suppress inference)."
  ::nil)

;;
;; Default currency.
;;

(def ^{:added "1.0.0" :tag Currency :dynamic true}
  ^Currency *default*
  "Default currency unit to be applied when creating Money objects without the currency
  specified."
  nil)

;;
;; Auto-scaling predicate for scale.
;;

(defn val-auto-scaled?
  "Returns `true` if the given scale is equal to auto-scaled."
  {:added "1.0.0" :tag Boolean}
  ^Boolean [^long scale]
  (== auto-scaled (int scale)))

(defmacro val-auto-scaled*?
  "Returns `true` if the given scale is equal to auto-scaled."
  {:added "2.0.0"}
  [scale]
  `(clojure.core/== auto-scaled (int ~scale)))

;;
;; Set of all Java currencies.
;;

(def ^{:tag clojure.lang.PersistentHashSet :added "2.0.0"}
  java-all-set
  "A set of Java currencies (instances of `java.util.Currency`, including obsolete
  currencies)."
  (set (java.util.Currency/getAvailableCurrencies)))

;;
;; Basic helpers
;;

(defn iso-strict-code?
  "Returns `true` when the given currency code, expressed as keyword, is exactly a
  3-character, simple keyword consisting only of uppercase letters in range of
  A-Z. Otherwise it returns `false`."
  {:tag Boolean :added "2.0.0"}
  ^Boolean [^clojure.lang.Keyword kid]
  (and (keyword? kid)
       (nil? (.getNamespace kid))
       (let [^String n (.getName kid)]
         (and (== 3 (unchecked-int (.length n)))
              (let [c0 (unchecked-int (.charAt n 0))
                    c1 (unchecked-int (.charAt n 1))
                    c2 (unchecked-int (.charAt n 2))]
                (and (<= 65 c0 90) (<= 65 c1 90) (<= 65 c2 90)))))))

(defn valid-numeric-id?
  "Returns `true` if a numeric ID is valid."
  {:tag Boolean :private true :added "2.0.0"}
  ^Boolean [^long nr]
  (and (not (== (int nr) no-numeric-id))
       (pos? nr)))

(defn iso-strict-currency?
  {:tag Boolean :private true :added "2.0.0"}
  ^Boolean [^Currency c]
  (and (some? c)
       (identical?    :ISO-4217 (.domain  c))
       (valid-numeric-id? (long (.numeric c)))
       (iso-strict-code?        (.id      c))))

(defn try-to-make-iso-domain
  "Returns `:ISO-4217` when the given numeric ID is valid and the given currency code
  is an uppercase 3-letter simple keyword. Otherwise it returns `nil`."
  {:private true :added "2.0.0"}
  [^long numeric-id ^clojure.lang.Keyword code]
  (when (and (valid-numeric-id? numeric-id) (iso-strict-code? code))
    :ISO-4217))

(defn- needs-upper-ascii?
  "Returns `true` when `s` contains at least one ASCII lower-case letter."
  {:tag Boolean :private true :added "3.0.0"}
  ^Boolean [^String s]
  (when (some? s)
    (let [len (unchecked-int (.length s))]
      (loop [i (unchecked-int 0)]
        (if (== i len)
          false
          (let [c (unchecked-int (.charAt s i))]
            (if (<= 97 c 122)
              true
              (recur (unchecked-inc-int i)))))))))

(defn- upper-ascii-if-needed
  "Upper-cases `s` only when it contains ASCII lower-case letters."
  {:tag String :private true :added "3.0.0"}
  [^String s]
  (if (needs-upper-ascii? s)
    (bu/try-upper-case s)
    s))

(defn- normalize-id-input
  "Canonicalizes a currency identifier input:

  - never changes the namespace casing,
  - upper-cases the name part (code) when needed.

  NOTE: for string inputs this may intern a keyword; do not use on untrusted input
  outside of constructors / controlled code paths."
  {:tag clojure.lang.Keyword :private true :added "3.0.0"}
  [id]
  (when (some? id)
    (cond
      (keyword? id)
      (let [^clojure.lang.Keyword kid id
            ns (.getNamespace kid)
            nm (.getName kid)
            nm' (upper-ascii-if-needed nm)]
        (if (identical? nm nm')
          kid
          (if (nil? ns)
            (keyword nm')
            (keyword ns nm'))))

      (symbol? id)
      (let [^clojure.lang.Symbol sid id
            ns (.getNamespace sid)
            nm (upper-ascii-if-needed (.getName sid))]
        (if (nil? ns) (keyword nm) (keyword ns nm)))

      (string? id)
      (let [^String s id]
        (when (pos? (unchecked-int (.length s)))
          (let [[^String ns ^String nm] (bu/split-on-first-slash s)]
            (if (nil? nm)
              (keyword (upper-ascii-if-needed ns))
              (keyword ns (upper-ascii-if-needed nm))))))

      :else
      (normalize-id-input (str id)))))

(defn- lookup-id-keys
  "Returns a small vector of candidate keywords to use for registry lookup.

  Preserves namespace casing, but tries an upper-cased name variant first when the
  name contains ASCII lower-case letters. Does not intern new keywords."
  {:tag clojure.lang.IPersistentVector :private true :added "3.0.0"}
  [^clojure.lang.Keyword id]
  (let [ns  (.getNamespace id)
        nm  (.getName id)
        nm' (upper-ascii-if-needed nm)]
    (if (identical? nm nm')
      [id]
      (if-some [kid (clojure.lang.Keyword/find ns nm')]
        [kid id]
        [id]))))

;;
;; Currency constructor
;;

(declare map->new)

(defn new-currency
  "Creates new currency record from values passed as arguments.

  Identifier casing rules:

  - Currency *name* (code) is canonicalized to upper-case (`:pln` -> `:PLN`,
    `:crypto/usdt` -> `:crypto/USDT`).
  - Namespace casing is preserved as provided (except `ISO-4217`, which is treated
    case-insensitively and always stripped).

  When the given `id` has the `ISO-4217` namespace (case insensitive), it will be
  stripped of it.

  When the namespace `ISO-4217` was removed from the ID, the domain will be set to
  `:ISO-4217`, unless the `domain` argument is given and it is not `nil`.

  In short: setting `ISO-4217` namespace in `id` will always cause it to be removed,
  and a new currency will be treated as if it was an ISO currency, unless the domain
  was explicitly given.

  Domain property will also be automatically set to `:ISO-4217` when:

  - `domain` was not given (or is `nil`) and was not set automatically,
  - `numeric-id` is given and is greater than 0,
  - the given `id` is a simple keyword (after potential stripping the ISO namespace)
    and consists of exactly 3 uppercase letters from A to Z only.

  Setting the domain manually to `:ISO-4217` (with `domain` argument or by setting a
  namespace of `id` to `ISO-4217`) will cause the currency to be treated as an ISO
  currency without any checks regarding its scale or numerical identifier. Use with
  caution. This is intended for applications requiring high elasticity."
  {:added "1.0.0" :tag Currency}
  (^Currency [id] (if (map? id) (map->new id)   (new-currency id nil nil nil nil nil)))
  (^Currency [id numeric-id]                    (new-currency id numeric-id nil nil nil nil))
  (^Currency [id numeric-id ^long scale]        (new-currency id numeric-id scale nil nil nil))
  (^Currency [id numeric-id ^long scale kind]   (new-currency id numeric-id scale kind nil nil))
  (^Currency [id numeric-id scale kind domain]  (new-currency id numeric-id scale kind domain nil))
  (^Currency [id numeric-id scale kind domain weight]
   (when (some? id)
     (let [kid        (normalize-id-input id)
           numeric-id (long (or numeric-id no-numeric-id))
           scale      (unchecked-int (or scale auto-scaled))
           weight     (unchecked-int (or weight 0))
           ns-domain  (some-> (namespace kid) bu/try-upper-case keyword)
           iso-ns?    (identical? ns-domain :ISO-4217)
           kid        (if iso-ns? (keyword (upper-ascii-if-needed (core-name kid))) kid)
           explicit-nil? (identical? domain explicit-nil)
           domain        (if explicit-nil?
                           nil
                           (if (nil? domain)
                             (or ns-domain (try-to-make-iso-domain numeric-id kid))
                             (keyword
                              (str/upper-case
                               (if (ident? domain)
                                 (core-name domain)
                                 (let [d (str domain)] (when (seq d) d)))))))
           ns-domain  (when-not iso-ns? ns-domain)]
       (when (and (some? ns-domain) (not= domain ns-domain))
         (throw (ex-info
                 "Currency domain should reflect its namespace (upper-cased) if a namespace is set."
                 {:id kid :domain domain :namespace (namespace kid)})))
       (Currency. kid
                  (unchecked-long numeric-id)
                  (unchecked-int  scale)
                  (keyword        kind)
                  (keyword        domain)
                  (unchecked-int  weight))))))

(defn map->new
  "Creates a new currency record from a map."
  {:added "1.0.0" :tag Currency}
  [^clojure.lang.IPersistentMap m]
  (when (and (some? m) (> (count m) 0))
    (let [id              (or (:id m) (:code m))
          nr0             (or (:nr m) (:numeric m) no-numeric-id)
          nr              (long (if (number? nr0) nr0 (or (bu/try-parse-long nr0) no-numeric-id)))
          nr              (long (if (< nr 1) no-numeric-id nr))
          sc0             (or (:sc m) (:scale   m) auto-scaled)
          sc              (unchecked-int (if (number? sc0) sc0 (or (bu/try-parse-int sc0) auto-scaled)))
          sc              (unchecked-int (if (< sc 0) auto-scaled sc))
          we0             (or (:we m) (:weight  m) (int 0))
          weight          (unchecked-int (if (number? we0) we0 (or (bu/try-parse-int we0) 0)))
          kind            (or (:ki m) (:kind    m))
          domain-present? (or (contains? m :do) (contains? m :domain))
          domain-val      (cond
                            (contains? m :do)     (get m :do)
                            (contains? m :domain) (get m :domain)
                            :else                 nil)
          domain          (if domain-present?
                            (if (nil? domain-val) explicit-nil domain-val)
                            nil)]
      (when-some [c ^Currency (new-currency id nr sc kind domain weight)]
        (if (> (count m) 1)
          (merge c (dissoc m :id :nr :numeric :sc :scale :ki :kind :do :domain :we :weight))
          c)))))

(def ^{:tag      Currency
       :arglists '(^Currency [id]
                   ^Currency [id numeric-id]
                   ^Currency [id numeric-id scale]
                   ^Currency [id numeric-id scale kind]
                   ^Currency [id numeric-id scale kind domain]
                   ^Currency [id numeric-id scale kind domain weight])}
  new
  "Alias for new-currency."
  new-currency)

;;
;; Monetary protocol.
;;

(defprotocol ^{:added "1.0.0"} Monetary
  "The Monetary protocol describes basic operations on currencies. It uses single
  dispatch to allow currencies to be expressed with different kinds of
  data (keywords, symbols, strings, native Currency objects etc.).

  Methods prefixed with `to-` are cheap, local, and registry-free coercions.
  Methods `resolve` and `resolve-all` consult a registry to map hints to concrete
  registered currencies."

  (^{:tag clojure.lang.Keyword :added "2.0.0"}
   to-id
   [this]
   "Coerces a currency representation to a currency identifier (keyword).
  Registry-free. May return `nil` if the identifier cannot be derived.")

  (^{:tag clojure.lang.Keyword :added "2.0.0"}
   to-code
   [this]
   "Coerces a currency representation to a currency code (unqualified keyword).
  Registry-free. May return `nil` if the code cannot be derived.")

  (^{:tag String :added "3.0.0"}
   to-id-str
   [this]
   "Coerces a currency representation to a currency identifier string.

  Registry-free. Must not intern keywords. Intended for security-sensitive code
  paths where the input may be untrusted.

  Suggested canonical form:
  - unqualified IDs: upper-case name (e.g. `\"PLN\"`),
  - qualified IDs: original namespace + `/` + upper-case name
    (e.g. `\"crypto/USDT\"` or `\"CrYpTo/USDT\"`).")

  (^{:tag String :added "3.0.0"}
   to-code-str
   [this]
   "Coerces a currency representation to a currency code string.

  Registry-free. Must not intern keywords. Typically the upper-cased name part
  without a namespace (e.g. `\"USDT\"`).")

  (^{:tag long :added "2.0.0"}
   to-numeric-id
   [this]
   "Coerces a currency representation to its numeric identifier (ISO 4217 numeric
  code, when available).

  Registry-free. Implementations should return a number when it can be derived,
  otherwise they may return `nil` or a sentinel numeric value.")

  (^{:tag io.randomseed.bankster.Currency :added "2.0.0"}
   to-currency
   [this]
   "Coerces a currency representation to a `Currency` object without consulting a
  registry.

  Returns a `Currency` instance or `nil` when it cannot be constructed.")

  (^{:tag clojure.lang.IPersistentMap :added "2.0.0"}
   to-map
   [this]
   "Coerces a currency representation to a map of currency fields.
  Registry-free. Returns a map (possibly partial) or `nil`.")

  (^{:tag Boolean :added "3.0.0"}
   definitive?
   [this]
   "Returns `true` when the given value is a *definitive* currency representation: it
  carries enough information (either explicitly or by type implication) to create a
  currency and to make negative property checks meaningful for the attributes it
  determines (e.g. when `domain` is not `:ISO-4217` then it is definitely not an ISO
  currency).

  For map-based currency specifications, definitiveness is expressed by the presence
  of keys for core attributes (domain, scale, numeric id). Key presence is
  significant: an explicitly present key with a `nil` value means \"specified as
  nil\" and should be trusted.

  Currency `kind` is treated as an optional classification attribute. If present in
  a representation, it is trusted; if absent, it remains unknown (not inferred).

  This method is registry-free.")

  (^{:tag io.randomseed.bankster.Currency :added "2.0.0"}
   resolve
   [this] [this registry]
   "Resolves a currency representation to a registered `Currency` by consulting a
  registry.

  When `registry` is `nil`, the default registry is used (preferring
  `io.randomseed.bankster.registry/*default*` when bound). Returns the resolved
  `Currency` or `nil` when it cannot be resolved.")

  (^{:tag clojure.lang.IPersistentSet :added "2.0.0"}
   resolve-all
   [this] [this registry]
   "Resolves a currency representation to all matching registered currencies by
  consulting a registry.

  When `registry` is `nil`, the default registry is used (preferring
  `io.randomseed.bankster.registry/*default*` when bound). Returns a set of resolved
  currencies or `nil` when nothing matches.")

  (^{:tag clojure.lang.Keyword :added "1.0.0"}
   id
   [id] [id registry]
   "Returns a unique identifier of the given currency as a keyword. The currency can
  be expressed as a `Currency` object, a keyword, a string, or any other object which
  can be handled by the `io.randomseed.bankster.currency/unit`.

  The role of the default registry is advisory. If the registry argument is not
  given (or it is `nil`) then the ID will be returned regardless of whether the
  currency exists in a registry, by simply converting it to a keyword or getting a
  field from a currency-like object. Still, the default registry will be consulted to
  resolve possible currency code. For example: if `BTC` is a currency code of a
  registered currency identified as `:crypto/BTC` then the resulting value for `:BTC`
  will be `:crypto/BTC`; but for `:BLABLA` (which does not exist in a registry) the
  resulting value will be `:BLABLA`.

  If a registry is given (or `true` value is passed, indicating to use default
  registry) then trying to use a non-existing currency will cause an exception to be
  thrown.")

  (^{:tag io.randomseed.bankster.Currency :added "1.0.0"}
   of-id
   [id] [id registry]
   "Returns a currency object for the given ID and registry.

  If the registry is not given, it will use the default registry (will first try a
  registry bound to the `io.randomseed.bankster.registry/*default*` dynamic
  variable, then the global registry).

  The currency is always taken from a registry (on a basis of the extracted ID) even
  if a `Currency` or currency-like object was given, with one exception: if
  `registry` is explicitly set to `nil` then a `Currency` object will be returned as
  is (or created out of currency-like object and returned) without consulting any
  registry.")

  (^{:tag io.randomseed.bankster.Currency :added "1.0.2"}
   unit
   [id] [id registry]
   "Returns a `Currency` object for the given ID, currency code, numeric ID, or
  a currency-like object.

  If a `Currency` record is passed and `registry`:
  - is not given (unary arity), or
  - is explicitly set to `nil`,

  then the registry will NOT be consulted and the `Currency` object will be returned
  as-is.

  If a valid `registry` is passed (or set to `true` value indicating that a default
  registry should be used), it will be consulted to find the exact match. For
  currency identifiers (keywords, strings, numbers) it will resolve them from the
  registry.

  For currency maps, `unit` treats the map as a *registry lookup specification*: it
  will query the registry using meaningful keys present in the map (a mask-like
  match). If more than one currency matches, the best match is selected by weight
  (smallest weight wins). If no currency matches, an exception will be thrown. Use
  `resolve-all` to obtain all matches.

  For `Currency` objects all fields must be exact except `:domain` and `:kind` which
  may be `nil` to act as wildcards. Only if the currency exists in the registry, the
  `Currency` record will be returned.

  If the registry is consulted and the currency does not exist in a registry, an
  exception will be thrown.")

  (^{:tag Boolean :added "1.0.0"}
   defined?
   [id] [id registry]
   "Returns `true` if *any* currency can be resolved from `id` in the registry.

  `id` may be:
  - a keyword (looked up as-is and, when namespaced, also by its unqualified name),
  - an `io.randomseed.bankster.Currency` (looked up by its `:id`),
  - a `java.util.Currency` (looked up by its currency code converted to a keyword),
  - a number (looked up as a numeric currency identifier).

  This is an existence check only. It may return `true` even when `id` is a
  currency-like object whose properties do not match the registered currency.

  If `registry` is not provided, the global registry is used, preferring the dynamic
  `io.randomseed.bankster.registry/*default*` when bound.")

  (^{:tag Boolean :added "1.0.2"}
   present?
   [id] [id registry]
   "Returns `true` if the registry contains a currency consistent with `id`.

  If `id` is a plain identifier (e.g. a keyword), this behaves as a simple presence
  test: it returns `true` when any currency resolves from that identifier.

  If `id` is a currency-like object, this performs a *field match* against the
  registered `io.randomseed.bankster.Currency`:

  - only fields that can be derived from `id` *and* are fields of `Currency`
    participate in the match,
  - all such derived fields must match exactly.

  Examples:
  - If `id` is a number, only the numeric currency identifier must match.
  - If `id` is a `java.util.Currency`, the currency ID/code, scale (fraction digits),
    and numeric ISO identifier must all match the registered currency.

  If `registry` is not provided, the global registry is used, preferring the dynamic
  `io.randomseed.bankster.registry/*default*` when bound."))

;;
;; Monetary helpers.
;;

(declare weighted-currencies)

(defn- compare-currency-ids
  "Compares whether two `Currency` objects share the same numeric identifier, scale,
  identifier, domain, and kind. Arguments are not commutative. Currency
  `compared-currency` is compared to currency `registered-currency`. It is not
  considered a mismatch when currency `compared-currency` has `:domain` and/or
  `:kind` set to `nil` but currency `registered-currency` has those fields set to any
  values. Returns `registered-currency` if comparison is successful, `nil`
  otherwise."
  {:tag Currency :private true :added "2.0.0"}
  ^Currency [^Currency registered-currency ^Currency compared-currency]
  (when
      (and (some? registered-currency)
           (== (unchecked-long (.numeric  registered-currency)) (unchecked-long (.numeric compared-currency)))
           (== (unchecked-int  (.scale    registered-currency)) (unchecked-int  (.scale   compared-currency)))
           (identical? (.id registered-currency) (.id compared-currency))
           (or (nil? (.domain compared-currency))
               (identical? (.domain registered-currency) (.domain compared-currency)))
           (or (nil? (.kind compared-currency))
               (identical? (.kind   registered-currency) (.kind   compared-currency))))
    registered-currency))

(defn- unit-registry
  "Normalizes a registry argument for strict operations.

  `nil` and `true` mean: use the default registry (preferring
  `io.randomseed.bankster.registry/*default*` when bound)."
  {:tag Registry :private true :added "3.0.0"}
  ^Registry [registry]
  (cond
    (nil?  registry) (registry/get)
    (true? registry) (registry/get)
    :else            registry))

(defn- unit-resolve!
  "Resolves `c` in `registry` and throws if it cannot be resolved.

  This is a strict helper for `Monetary/unit` implementations. It is meant for
  identifier-like currency representations. For already constructed currencies
  (e.g. `Currency` records) the `nil` registry convention may still mean
  \"return as-is\" and should be handled by the caller."
  {:tag Currency :private true :added "3.0.0"}
  ^Currency [c ^Registry registry]
  (or (resolve c registry)
      (throw
       (ex-info
        "Currency not found in a registry."
        {:registry registry
         :value    c
         :op       :unit}))))

(defn- id->str
  "Canonical string form of a keyword currency identifier.

  Uses original namespace and upper-case name. Returns `nil` for `nil` input."
  {:tag String :private true :added "3.0.0"}
  ^String [^clojure.lang.Keyword id]
  (when (some? id)
    (let [ns (.getNamespace ^clojure.lang.Keyword id)
          nm (.getName      ^clojure.lang.Keyword id)
          nm (upper-ascii-if-needed nm)]
      (if (nil? ns)
        nm
        (str ns "/" nm)))))

(defn- id->str-fast
  "Fast string form of a keyword currency identifier.

  Assumes the ID has already been canonicalized (upper-case name). Intended for
  `Currency` values which are treated as the source of truth."
  {:tag String :private true :added "3.0.0"}
  ^String [^clojure.lang.Keyword id]
  (when (some? id)
    (let [ns (.getNamespace ^clojure.lang.Keyword id)
          nm (.getName      ^clojure.lang.Keyword id)]
      (if (nil? ns)
        nm
        (str ns "/" nm)))))

(defn- code->str
  "Canonical string form of a keyword currency code (no namespace)."
  {:tag String :private true :added "3.0.0"}
  ^String [^clojure.lang.Keyword id]
  (when (some? id)
    (bu/try-upper-case (.getName ^clojure.lang.Keyword id))))

(defn- candidate-ids
  "Returns a set of possible currency IDs for `x`.

  This is a best-effort, non-throwing helper used by `same-ids?`."
  {:tag clojure.lang.IPersistentSet :private true :added "3.0.0"}
  [x ^Registry registry]
  (try
    (cond
      (nil? x)
      nil

      ;; A Currency value is never treated as a registry reference.
      (instance? Currency x)
      (when-some [s (id->str-fast (.id ^Currency x))] #{s})

      ;; Map is treated as a currency representation, not a registry reference.
      (map? x)
      (when-some [s (to-id-str x)] #{s})

      ;; Definitive objects are treated as self-contained representations.
      (definitive? x)
      (when-some [s (to-id-str x)] #{s})

      :else
      (or
       ;; Numeric identifiers are registry-only hints (including numeric strings).
       (when-some [nr (to-numeric-id x)]
           (when (valid-numeric-id? (long nr))
             (when-some [curs (resolve-all (long nr) registry)]
               (not-empty
              (into #{} (map (fn [^Currency c] (id->str-fast (.id c))) curs))))))
       (when-some [s (to-id-str x)] #{s})))
    (catch Throwable _ nil)))

(defn same-ids?
  "Returns `true` when two currency representations refer to the same currency ID.

  Contract:
  - symmetric (argument order does not matter),
  - non-throwing (returns `false` when IDs cannot be established),
  - `registry=nil` or `registry=true` means: use the default registry,
  - `Currency` values are never treated as registry references."
  {:tag Boolean :added "3.0.0"}
  (^Boolean [a b]
   (same-ids? a b nil))
  (^Boolean [a b registry]
   (let [^Registry registry (unit-registry registry)
         a-ids              (candidate-ids a registry)
         b-ids              (candidate-ids b registry)]
     (boolean
      (and (seq a-ids)
           (seq b-ids)
           (if (<= (count a-ids) (count b-ids))
             (some b-ids a-ids)
             (some a-ids b-ids)))))))

(def ^{:tag clojure.lang.Keyword :const true :private true :added "3.0.0"}
  invalid-map-hint
  ::invalid-map-hint)

(defn- normalize-id-hint
  "Best-effort, registry-free normalization of an ID hint.

  Strips the ISO namespace (`ISO-4217`, case-insensitive) if present."
  {:tag clojure.lang.Keyword :private true :added "3.0.0"}
  [x]
  (when-some [^clojure.lang.Keyword kid (to-id x)]
    (let [ns (some-> (namespace kid) bu/try-upper-case)]
      (if (= ns "ISO-4217")
        (keyword (core-name kid))
        kid))))

(defn- normalize-code-hint
  "Best-effort, registry-free normalization of a currency code hint."
  {:tag clojure.lang.Keyword :private true :added "3.0.0"}
  [x]
  (to-code x))

(defn- normalize-numeric-hint
  "Normalizes a numeric currency identifier hint.

  - Missing key means \"not specified\" (handled by the caller via `contains?`).
  - Present key with `nil` value means \"specified as nil\" -> `no-numeric-id`.
  - Non-nil values are parsed and must satisfy `valid-numeric-id?`."
  {:tag Object :private true :added "3.0.0"}
  [x]
  (cond
    (nil? x)
    no-numeric-id

    (number? x)
    (let [n (long x)]
      (cond (< n 1)               no-numeric-id
            (valid-numeric-id? n) n
            :else                 invalid-map-hint))

    :else
    (let [n (try (bu/try-parse-long x) (catch Throwable _ nil))]
      (if (nil? n)
        invalid-map-hint
        (normalize-numeric-hint n)))))

(defn- normalize-scale-hint
  "Normalizes a currency scale hint.

  - Missing key means \"not specified\" (handled by the caller via `contains?`).
  - Present key with `nil` value means \"specified as nil\" -> `auto-scaled`.
  - Non-nil values are parsed to integer.

  Negative scales are treated as `auto-scaled` (matching `map->new`)."
  {:tag Object :private true :added "3.0.0"}
  [x]
  (cond
    (nil? x)
    auto-scaled

    (number? x)
    (let [s (int x)]
      (if (neg? s) auto-scaled s))

    :else
    (let [s (try (bu/try-parse-int x) (catch Throwable _ nil))]
      (if (nil? s)
        invalid-map-hint
        (normalize-scale-hint s)))))

(defn- normalize-domain-hint
  "Normalizes a currency domain hint to an upper-cased keyword."
  {:tag clojure.lang.Keyword :private true :added "3.0.0"}
  [x]
  (when-not (nil? x)
    (when-some [s (not-empty (if (ident? x) (core-name x) (str x)))]
      (keyword (str/upper-case s)))))

(defn- normalize-kind-hint
  "Normalizes a currency kind hint to a keyword.

  Kind is treated as case-sensitive and may be namespaced."
  {:tag clojure.lang.Keyword :private true :added "3.0.0"}
  [x]
  (when-not (nil? x)
    (cond
      (keyword? x)
      x

      (symbol? x)
      (keyword (namespace x) (core-name x))

      :else
      (let [s (not-empty (str x))]
        (when s
          ;; `keyword` parses "ns/name" automatically, preserving case.
          (keyword s))))))

(defn- normalize-weight-hint
  "Normalizes a currency weight hint.

  - Missing key means \"not specified\" (handled by the caller via `contains?`).
  - Present key with `nil` value means \"specified as nil\" -> `0`.
  - Non-nil values are parsed to integer."
  {:tag Object :private true :added "3.0.0"}
  [x]
  (cond (nil? x)    0
        (number? x) (int x)
        :else       (let [w (try (bu/try-parse-int x) (catch Throwable _ nil))]
                      (if (nil? w)
                        invalid-map-hint
                        (int w)))))

(defn- compare-map-ids
  "Compares a registry `Currency` to a map of currency constraints.

  Constraints are **key-presence based**: an attribute is compared only when its key
  exists in the map. A present key with a `nil` value is meaningful (\"specified as
  nil\") and is compared using Bankster sentinels (`no-numeric-id`, `auto-scaled`) or
  `nil` where applicable.

  The function is soft: invalid hints or inconsistent alias values result in `nil`
  (no match) rather than an exception."
  {:tag Currency :private true :added "2.0.0"}
  ^Currency [^Currency registered-currency ^clojure.lang.IPersistentMap compared-map]
  (when (and (some? registered-currency) (seq compared-map))
    (let [id?   (contains? compared-map :id)
          code? (contains? compared-map :code)
          nr?   (or (contains? compared-map :nr) (contains? compared-map :numeric))
          sc?   (or (contains? compared-map :sc) (contains? compared-map :scale))
          we?   (or (contains? compared-map :we) (contains? compared-map :weight))
          do?   (or (contains? compared-map :do) (contains? compared-map :domain))
          ki?   (or (contains? compared-map :ki) (contains? compared-map :kind))

          id-val   (when id?   (normalize-id-hint   (get compared-map :id)))
          code-val (when code? (normalize-code-hint (get compared-map :code)))

          nr1?   (contains? compared-map :nr)
          nr2?   (contains? compared-map :numeric)
          nr1    (when nr1? (normalize-numeric-hint (get compared-map :nr)))
          nr2    (when nr2? (normalize-numeric-hint (get compared-map :numeric)))
          nr-val (cond (and nr1? nr2? (not= nr1 nr2)) invalid-map-hint
                       nr1?                           nr1
                       nr2?                           nr2
                       :else                          nil)

          sc1?   (contains? compared-map :sc)
          sc2?   (contains? compared-map :scale)
          sc1    (when sc1? (normalize-scale-hint (get compared-map :sc)))
          sc2    (when sc2? (normalize-scale-hint (get compared-map :scale)))
          sc-val (cond (and sc1? sc2? (not= sc1 sc2)) invalid-map-hint
                       sc1?                           sc1
                       sc2?                           sc2
                       :else                          nil)

          we1?   (contains? compared-map :we)
          we2?   (contains? compared-map :weight)
          we1    (when we1? (normalize-weight-hint (get compared-map :we)))
          we2    (when we2? (normalize-weight-hint (get compared-map :weight)))
          we-val (cond (and we1? we2? (not= we1 we2)) invalid-map-hint
                       we1?                           we1
                       we2?                           we2
                       :else                          nil)

          do1?   (contains? compared-map :do)
          do2?   (contains? compared-map :domain)
          do1    (when do1? (normalize-domain-hint (get compared-map :do)))
          do2    (when do2? (normalize-domain-hint (get compared-map :domain)))
          do-val (cond (and do1? do2? (not= do1 do2)) invalid-map-hint
                       do1?                           do1
                       do2?                           do2
                       :else                          nil)

          ki1?   (contains? compared-map :ki)
          ki2?   (contains? compared-map :kind)
          ki1    (when ki1? (normalize-kind-hint (get compared-map :ki)))
          ki2    (when ki2? (normalize-kind-hint (get compared-map :kind)))
          ki-val (cond (and ki1? ki2? (not= ki1 ki2)) invalid-map-hint
                       ki1?                           ki1
                       ki2?                           ki2
                       :else                          nil)

          id-ns (when id-val (some-> (namespace id-val) bu/try-upper-case keyword))]
      (when-not (or (and id?   (nil? id-val))
                    (and code? (nil? code-val))
                    (identical? invalid-map-hint nr-val)
                    (identical? invalid-map-hint sc-val)
                    (identical? invalid-map-hint we-val)
                    (identical? invalid-map-hint do-val)
                    (identical? invalid-map-hint ki-val)
                    (and do? id-ns (not (identical? id-ns :ISO-4217))
                         (not= do-val id-ns)))
        (when
            (and (or (not id?)   (identical? id-val         (.id      registered-currency)))
                 (or (not code?) (identical? code-val       (to-code  registered-currency)))
                 (or (not nr?)   (== (long nr-val)    (long (.numeric registered-currency))))
                 (or (not sc?)   (== (int  sc-val)    (int  (.scale   registered-currency))))
                 (or (not we?)   (== (int  we-val)    (int  (.weight  registered-currency))))
                 (or (not do?)   (identical? do-val         (.domain  registered-currency)))
                 (or (not ki?)   (identical? ki-val         (.kind    registered-currency))))
            registered-currency)))))

(defn- map-registry-op!
  "Throws when a currency map is used in a strict, identifier-oriented registry operation.

  Bankster treats maps as currency specifications/constraints. They can be used with
  `resolve`, `resolve-all` and `unit` (as lookup specs), but not with operations like
  `id`, `of-id`, `defined?` or `present?` which expect a concrete identifier."
  [op m registry]
  (throw
   (ex-info
    "Currency maps cannot be used as strict registry references."
    {:op       op
     :value    m
     :registry registry
     :hint     "Use resolve/resolve-all/unit for map-based lookup, or a keyword/string/number identifier to reference a registered currency."})))

(defn- merge-candidates
  "Ensures that merged structures are sorted sets."
  [a b]
  (cond
    (and a b) (into (weighted-currencies a) b)
    a         (not-empty a)
    b         (not-empty b)
    :else     nil))

(defn- keep-in-sorted-set-where
  "Returns a sorted set with all elements *not* matching `pred` removed.

  Intended for `clojure.lang.PersistentTreeSet` (sorted-set). Unlike
  `io.randomseed.bankster.util/keep-in-set-where`, this does not rely on transients.

  Returns `nil` when `s` is `nil` or when the resulting set is empty."
  {:tag clojure.lang.PersistentTreeSet :private true :added "3.0.0"}
  [^clojure.lang.IFn pred ^clojure.lang.PersistentTreeSet s]
  (when (some? s)
    (let [s (reduce (fn [acc x] (if (pred x) acc (disj acc x))) s s)]
      (when (seq s) s))))

;;
;; Currency querying functions, Monetary implementation.
;;

(extend-protocol Monetary

  Currency

  (to-id
    ^clojure.lang.Keyword [c]
    (.id ^Currency c))

  (to-code
    ^clojure.lang.Keyword [c]
    (let [id (.id ^Currency c)]
      (if (nil? (.getNamespace ^clojure.lang.Keyword id))
        id
        (keyword (.getName ^clojure.lang.Keyword id)))))

  (to-id-str
    ^String [c]
    (id->str-fast (.id ^Currency c)))

  (to-code-str
    ^String [c]
    (code->str (.id ^Currency c)))

  (to-numeric-id
    ^long [c]
    (.numeric ^Currency c))

  (to-currency
    ^Currency [^Currency c]
    c)

  (to-map
    [c]
    {:id (.id      ^Currency c)
     :nr (.numeric ^Currency c)
     :sc (.scale   ^Currency c)
     :do (.domain  ^Currency c)
     :ki (.kind    ^Currency c)
     :we (.weight  ^Currency c)})

  (definitive?
    (^Boolean [c]
     (some? (.id ^Currency c))))

  (resolve
    (^Currency [c]
     (resolve c (registry/get)))
    (^Currency [c ^Registry registry]
     (let [^Registry registry (registry/get registry)]
       (or (compare-currency-ids (registry/currency-id->currency* (.id ^Currency c) registry) c)
           (some #(compare-currency-ids ^Currency % c) (resolve-all (.numeric ^Currency c) registry))))))

  (resolve-all
    (^clojure.lang.IPersistentSet [c]
     (resolve-all c (registry/get)))
    (^clojure.lang.IPersistentSet [c ^Registry registry]
     (let [^Registry registry (registry/get registry)
           by-id              (registry/currency-id->currency*   (.id      ^Currency c) registry)
           by-num             (registry/currency-nr->currencies* (.numeric ^Currency c) registry)
           ^Currency fhit     (when by-id  (compare-currency-ids by-id c))
           hitset             (when by-num (bu/keep-in-set-where #(compare-currency-ids ^Currency % c) by-num))]
       (if (nil? hitset)
         (if (nil? fhit)
           nil
           #{fhit})
         (if (nil? fhit)
           hitset
           (conj hitset fhit))))))

  (of-id
    (^Currency [^Currency currency]
     (of-id (.id ^Currency currency) (registry/get)))
    (^Currency [^Currency currency ^Registry registry]
     (cond (nil?  registry) currency
           (true? registry) (of-id ^clojure.lang.Keyword (.id ^Currency currency) (registry/get))
           :else            (of-id ^clojure.lang.Keyword (.id ^Currency currency) registry))))

  (unit
    (^Currency [^Currency currency]
     currency)
    (^Currency [^Currency currency ^Registry registry]
     (cond
       (nil? registry)
       currency

       :else
       (let [^Registry registry (unit-registry registry)]
         (unit-resolve! currency registry)))))

  (id
    (^clojure.lang.Keyword [^Currency currency] (.id ^Currency currency))
    (^clojure.lang.Keyword [^Currency currency ^Registry _registry] (.id ^Currency currency)))

  (defined?
    (^Boolean [^Currency currency]
     (contains? (registry/currency-id->currency*) (.id ^Currency currency)))
    (^Boolean [^Currency currency ^Registry registry]
     (contains? (registry/currency-id->currency* registry) (.id ^Currency currency))))

  (present?
    (^Boolean [^Currency currency]
     (boolean (resolve currency (registry/get))))
    (^Boolean [^Currency currency ^Registry registry]
     (boolean (resolve currency registry))))

  java.util.Currency

  (to-id
    ^clojure.lang.Keyword [c]
    (keyword (.getCurrencyCode ^java.util.Currency c)))

  (to-code
    ^clojure.lang.Keyword [c]
    (keyword (.getCurrencyCode ^java.util.Currency c)))

  (to-id-str
    ^String [c]
    (bu/try-upper-case (.getCurrencyCode ^java.util.Currency c)))

  (to-code-str
    ^String [c]
    (bu/try-upper-case (.getCurrencyCode ^java.util.Currency c)))

  (to-numeric-id
    ^long [c]
    (long (.getNumericCode ^java.util.Currency c)))

  (to-currency
    ^Currency [c]
    (new-currency (keyword (.getCurrencyCode ^java.util.Currency c))
                  (long    (.getNumericCode  ^java.util.Currency c))
                  (long    (or (.getDefaultFractionDigits ^java.util.Currency c) auto-scaled))
                  nil
                  :ISO-4217))

  (to-map
    [c]
    {:id (keyword (.getCurrencyCode ^java.util.Currency c))
     :nr (long    (.getNumericCode  ^java.util.Currency c))
     :sc (long    (or (.getDefaultFractionDigits ^java.util.Currency c) auto-scaled))
     :do :ISO-4217})

  (definitive?
    (^Boolean [_]
     true))

  (resolve
    (^Currency [c]
     (resolve ^java.util.Currency c (registry/get)))
    (^Currency [c ^Registry registry]
     (let [^Registry registry (registry/get registry)]
       (resolve ^Currency (to-currency ^java.util.Currency c) ^Registry registry))))

  (resolve-all
    (^clojure.lang.IPersistentSet [c]
     (resolve-all c (registry/get)))
    (^clojure.lang.IPersistentSet [c ^Registry registry]
     (let [^Registry registry (registry/get registry)]
       (resolve-all ^Currency (to-currency ^java.util.Currency c) ^Registry registry))))

  (of-id
    (^Currency [jc]
     (of-id jc (registry/get)))
    (^Currency [^java.util.Currency jc ^Registry registry]
     (or (when-some [curs (some-> (.getNumericCode jc) long
                                  (registry/currency-nr->currencies* registry))]
           (when-some [jcode (keyword ^String (.getCurrencyCode jc))]
             (let [jsca (int (.getDefaultFractionDigits jc))]
               (some (fn [^Currency c]
                       (and (== jsca     ^int (.scale c))
                            (identical? jcode (.id    c))))
                     curs))))
         (throw (ex-info
                 (str "Currency with the properties of Java currency "
                      (.getCurrencyCode jc)
                      " not found in a registry.")
                 {:registry registry :currency jc})))))

  (unit
    (^Currency [jc]
     (unit jc (registry/get)))
    (^Currency [jc ^Registry registry]
     (let [^Registry registry (unit-registry registry)]
       (unit-resolve! jc registry)))
    (^Currency [jc ^Registry registry _]
     (unit jc registry)))

  (id
    (^clojure.lang.Keyword [jc]
     (id (to-id jc)))
    (^clojure.lang.Keyword [jc ^Registry registry]
     (id (to-id jc) registry)))

  (defined?
    (^Boolean [jc]
     (defined? (to-id jc)))
    (^Boolean [jc ^Registry registry]
     (defined? (to-id jc) registry)))

  (present?
    (^Boolean [jc]
     (present? (to-currency jc) nil))
    (^Boolean [jc ^Registry registry]
     (present? (to-currency jc) registry)))

  Number

  (to-id
    [_]
    nil)

  (to-code
    [_]
    nil)

  (to-id-str
    [_]
    nil)

  (to-code-str
    [_]
    nil)

  (to-numeric-id
    ^long [id]
    (long id))

  (to-currency
    [_]
    nil)

  (to-map
    [id]
    {:nr (long id)})

  (definitive?
    (^Boolean [_]
     false))

  (resolve
    (^Currency [num]
     (resolve num nil))
    (^Currency [num ^Registry registry]
     (let [^Registry registry (registry/get registry)
           nr                 (long num)]
       (or (registry/currency-nr->currency* nr registry)
           (when-some [curs (not-empty (registry/currency-nr->currencies* nr registry))]
             (registry/inconsistency-warning
              (str "Currency no. " nr " found in cur-nr->curs but not in cur-nr->cur")
              {:nr       nr
               :reason   :missing-cur-nr->cur
               :registry registry}
              (first curs)))))))

  (resolve-all
    (^clojure.lang.IPersistentSet [num]
     (resolve-all num (registry/get)))
    (^clojure.lang.IPersistentSet [num ^Registry registry]
     (let [^Registry registry (registry/get registry)
           nr                 (long num)]
       (or (not-empty (registry/currency-nr->currencies* nr registry))
           (some-> (registry/currency-nr->currency* nr registry) hash-set)))))

  (of-id
    (^Currency [num]
     (of-id num (registry/get)))
    (^Currency [num ^Registry registry]
     (or (registry/currency-nr->currency* num registry)
         (when-some [^Currency f (first (registry/currency-nr->currencies* num registry))]
           (registry/inconsistency-warning
            (str "Currency no. " num " found in cur-nr->curs as " (core-symbol (.id f)) " but not in cur-nr->cur")
            {:nr       num
             :id       (.id f)
             :reason   :missing-cur-nr->cur
             :registry registry}
            f))
         (throw (ex-info
                 (str "Currency with the numeric ID of " num " not found in a registry.")
                 {:registry registry})))))

  (unit
    (^Currency [num]
     (unit num (registry/get)))
    (^Currency [num ^Registry registry]
     (let [^Registry registry (unit-registry registry)]
       (unit-resolve! num registry)))
    (^Currency [num ^Registry registry _]
     (unit num registry)))

  (id
    (^clojure.lang.Keyword [num]
     (id (long num) (registry/get)))
    (^clojure.lang.Keyword [num ^Registry registry]
     (if-some [^Currency c (resolve num registry)]
       (.id c)
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry registry})))))

  (defined?
    (^Boolean [num]
     (or (contains? (registry/currency-nr->currency*) num)
         (and (contains? (registry/currency-nr->currencies*) num)
              (registry/inconsistency-warning
               (str "Currency no. " num " found in cur-nr->curs but not in cur-nr->cur")
               {:nr       num
                :reason   :missing-cur-nr->cur
                :registry (registry/get)}
               true))))
    (^Boolean [num ^Registry registry]
     (or (contains? (registry/currency-nr->currency* registry)   num)
         (and (contains? (registry/currency-nr->currencies* registry) num)
              (registry/inconsistency-warning
               (str "Currency no. " num " found in cur-nr->curs but not in cur-nr->cur")
               {:nr       num
                :reason   :missing-cur-nr->cur
                :registry registry}
               true)))))

  (present?
    (^Boolean [num]
     (or (contains? (registry/currency-nr->currency*)   num)
         (contains? (registry/currency-nr->currencies*) num)))
    (^Boolean [num ^Registry registry]
     (or (contains? (registry/currency-nr->currency* registry)   num)
         (contains? (registry/currency-nr->currencies* registry) num))))

  clojure.lang.Keyword

  (to-id
    ^clojure.lang.Keyword [id]
    id)

  (to-code
    ^clojure.lang.Keyword [id]
    (if (nil? (.getNamespace ^clojure.lang.Keyword id))
      id
      (keyword (.getName ^clojure.lang.Keyword id))))

  (to-id-str
    ^String [id]
    (id->str id))

  (to-code-str
    ^String [id]
    (code->str id))

  (to-numeric-id
    ^long [_]
    no-numeric-id)

  (to-currency
    ^Currency [id]
    (new-currency id))

  (to-map
    [id]
    {:id id})

  (definitive?
    (^Boolean [_]
     false))

  (resolve
    (^Currency [id]
     (resolve id (registry/get)))
    (^Currency [id ^Registry registry]
     (let [^Registry registry (registry/get registry)
           ^String nspace     (.getNamespace ^clojure.lang.Keyword id)]
       (if (nil? nspace)
         (let [ids (lookup-id-keys id)]
           (or (some #(first (registry/currency-code->currencies* % registry)) ids)
               (some #(registry/currency-id->currency* % registry) ids)))
         (if (= (str/upper-case nspace) "ISO-4217")
           (let [nm  (.getName ^clojure.lang.Keyword id)
                 nm' (upper-ascii-if-needed nm)]
             (when-some [kid (or (clojure.lang.Keyword/find nil nm')
                                 (clojure.lang.Keyword/find nil nm))]
               (when-some [^Currency hit (registry/currency-id->currency* kid registry)]
                 (when (identical? (.domain ^Currency hit) :ISO-4217) hit))))
           (let [ids (lookup-id-keys id)]
             (some #(registry/currency-id->currency* % registry) ids)))))))

  (resolve-all
    (^clojure.lang.IPersistentSet [id]
     (resolve-all id (registry/get)))
    (^clojure.lang.IPersistentSet [id ^Registry registry]
     (let [^Registry registry (registry/get registry)
           ^String nspace     (.getNamespace ^clojure.lang.Keyword id)]
       (if (nil? nspace)
         (let [ids (lookup-id-keys id)]
           (not-empty
            (reduce
             (fn [acc kid]
               (let [acc (if-some [by-code (registry/currency-code->currencies* kid registry)]
                           (into (or acc #{}) by-code)
                           acc)]
                 (if-some [^Currency cur (registry/currency-id->currency* kid registry)]
                   (conj (or acc #{}) cur)
                   acc)))
             nil
             ids)))
         (if (= (str/upper-case nspace) "ISO-4217")
           (let [nm  (.getName ^clojure.lang.Keyword id)
                 nm' (upper-ascii-if-needed nm)]
             (when-some [kid (or (clojure.lang.Keyword/find nil nm')
                                 (clojure.lang.Keyword/find nil nm))]
               (when-some [^Currency hit (registry/currency-id->currency* kid registry)]
                 (when (identical? (.domain ^Currency hit) :ISO-4217) #{hit}))))
           (let [ids (lookup-id-keys id)]
             (not-empty
              (reduce
               (fn [acc kid]
                 (if-some [^Currency cur (registry/currency-id->currency* kid registry)]
                   (conj (or acc #{}) cur)
                   acc))
               nil
               ids))))))))

  (of-id
    (^Currency [id]
     (of-id id (registry/get)))
    (^Currency [id ^Registry registry]
     (let [ids (lookup-id-keys id)]
       (or (some #(registry/currency-id->currency* % registry) ids)
           (throw (ex-info
                   (str "Currency " (core-symbol id) " not found in a registry.")
                   {:registry registry}))))))

  (unit
    (^Currency [id]
     (unit id (registry/get)))
    (^Currency [id ^Registry registry]
     (let [^Registry registry (unit-registry registry)]
       (unit-resolve! id registry))))

  (id
    (^clojure.lang.Keyword [c]
     (if-let [^Registry registry registry/*default*]
       (.id ^Currency (unit ^clojure.lang.Keyword c ^Registry registry))
       (if (namespace c)
         c
         (let [^Registry registry (registry/get)]
           (if-some [^Currency cur (resolve c registry)]
             (.id ^Currency cur)
             c)))))
    (^clojure.lang.Keyword [c ^Registry registry]
     (if (nil? registry) c
         (.id ^Currency (unit ^clojure.lang.Keyword c ^Registry registry)))))

  (defined?
    (^Boolean [id]
     (boolean (some #(contains? (registry/currency-id->currency*) %)
                    (lookup-id-keys id))))
    (^Boolean [id ^Registry registry]
     (boolean (some #(contains? (registry/currency-id->currency* registry) %)
                    (lookup-id-keys id)))))

  (present?
    (^Boolean [id]
     (if (namespace id)
       (boolean (some #(contains? (registry/currency-id->currency*) %)
                      (lookup-id-keys id)))
       (let [ids (lookup-id-keys id)]
         (boolean
          (or (some #(contains? (registry/currency-code->currencies*) %) ids)
              (some #(contains? (registry/currency-id->currency*) %) ids))))))
    (^Boolean [id ^Registry registry]
     (if (namespace id)
       (boolean (some #(contains? (registry/currency-id->currency* registry) %)
                      (lookup-id-keys id)))
       (let [ids (lookup-id-keys id)]
         (boolean
          (or (some #(contains? (registry/currency-code->currencies* registry) %) ids)
              (some #(contains? (registry/currency-id->currency* registry) %) ids)))))))

  String

  (to-id
    ^clojure.lang.Keyword [id]
    (when (pos? (unchecked-int (.length ^String id)))
      (keyword id)))

  (to-code
    ^clojure.lang.Keyword [id]
    (when (pos? (unchecked-int (.length ^String id)))
      (let [kid (keyword id)]
        (if (nil? (.getNamespace ^clojure.lang.Keyword kid))
          kid
          (keyword (.getName ^clojure.lang.Keyword kid))))))

  (to-id-str
    ^String [id]
    (when (pos? (unchecked-int (.length ^String id)))
      (let [[^String ns ^String nm] (bu/split-on-first-slash id)]
        (if (nil? nm)
          (upper-ascii-if-needed ns)
          (let [nm (upper-ascii-if-needed nm)]
            (when (and (some? ns) (some? nm))
              (str ns "/" nm)))))))

  (to-code-str
    ^String [id]
    (when (pos? (unchecked-int (.length ^String id)))
      (let [[^String ns ^String nm] (bu/split-on-first-slash id)]
        (if (nil? nm)
          (upper-ascii-if-needed ns)
          (upper-ascii-if-needed nm)))))

  (to-numeric-id
    ^long [id]
    (bu/try-parse-long id))

  (to-currency
    ^Currency [id]
    (when (pos? (unchecked-int (.length ^String id)))
      (new-currency id)))

  (to-map
    [id]
    {:id (to-id id)})

  (definitive?
    (^Boolean [_]
     false))

  (resolve
    (^Currency [id]
     (resolve id (registry/get)))
    (^Currency [id ^Registry registry]
     (let [^Registry registry (registry/get registry)]
       (when (pos? (unchecked-int (.length ^String id)))
         (let [[^String ns ^String nm] (bu/split-on-first-slash id)]
           (if (nil? nm)
             (when-some [kid (clojure.lang.Keyword/find nil (bu/try-upper-case ns))]
               (or (first (registry/currency-code->currencies* kid registry))
                   (registry/currency-id->currency* kid registry)))
             (let [nsu (bu/try-upper-case ns)
                   nmu (bu/try-upper-case nm)]
               (when (and (some? nsu) (some? nmu))
                 (if (= nsu "ISO-4217")
                   (when-some [kid (clojure.lang.Keyword/find nil nmu)]
                     (when-some [^Currency hit (registry/currency-id->currency* kid registry)]
                       (when (identical? (.domain ^Currency hit) :ISO-4217) hit)))
                   (when-some [kid (clojure.lang.Keyword/find ns nmu)]
                     (registry/currency-id->currency* kid registry)))))))))))

  (resolve-all
    (^clojure.lang.IPersistentSet [id]
     (resolve-all id (registry/get)))
    (^clojure.lang.IPersistentSet [id ^Registry registry]
     (let [^Registry registry (registry/get registry)]
       (when (pos? (unchecked-int (.length ^String id)))
         (let [[^String ns ^String nm] (bu/split-on-first-slash id)]
           (if (nil? nm)
             (when-some [kid (clojure.lang.Keyword/find nil (bu/try-upper-case ns))]
               (or (registry/currency-code->currencies* kid registry)
                   (some-> (registry/currency-id->currency* kid registry) hash-set)))
             (let [nsu (bu/try-upper-case ns)
                   nmu (bu/try-upper-case nm)]
               (when (and (some? nsu) (some? nmu))
                 (if (= nsu "ISO-4217")
                   (when-some [kid (clojure.lang.Keyword/find nil nmu)]
                     (when-some [^Currency hit (registry/currency-id->currency* kid registry)]
                       (when (identical? (.domain ^Currency hit) :ISO-4217) #{hit})))
                   (when-some [kid (clojure.lang.Keyword/find ns nmu)]
                     (some-> (registry/currency-id->currency* kid registry) hash-set)))))))))))

  (of-id
    (^Currency [id] (of-id (keyword id)))
    (^Currency [id ^Registry registry] (of-id (keyword id) registry)))

  (unit
    (^Currency [id] (unit (keyword id)))
    (^Currency [id ^Registry registry] (unit (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [c] (id (keyword c)))
    (^clojure.lang.Keyword [c ^Registry registry] (id (keyword c) registry)))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency*) (keyword id)))
    (^Boolean [id ^Registry registry]
     (contains? (registry/currency-id->currency* registry) (keyword id))))

  (present?
    (^Boolean [id]
     (present? (keyword id)))
    (^Boolean [id ^Registry registry]
     (present? (keyword id) registry)))

  clojure.lang.Symbol

  (to-id
    ^clojure.lang.Keyword [id]
    (keyword id))

  (to-code
    ^clojure.lang.Keyword [id]
    (keyword (.getName ^clojure.lang.Symbol id)))

  (to-id-str
    ^String [id]
    (let [^String ns (.getNamespace ^clojure.lang.Symbol id)
          ^String nm (.getName      ^clojure.lang.Symbol id)
          nm         (upper-ascii-if-needed nm)]
      (if (nil? ns)
        nm
        (str ns "/" nm))))

  (to-code-str
    ^String [id]
    (bu/try-upper-case (.getName ^clojure.lang.Symbol id)))

  (to-numeric-id
    [_]
    nil)

  (to-currency
    ^Currency [id]
    (new-currency (keyword id)))

  (to-map
    [id]
    {:id (keyword id)})

  (definitive?
    (^Boolean [_]
     false))

  (resolve
    (^Currency [id]
     (resolve (keyword id) (registry/get)))
    (^Currency [id ^Registry registry]
     (resolve (keyword id) registry)))

  (resolve-all
    (^clojure.lang.IPersistentSet [id]
     (resolve-all (keyword id) (registry/get)))
    (^clojure.lang.IPersistentSet [id ^Registry registry]
     (resolve-all (keyword id) registry)))

  (of-id
    (^Currency [id] (of-id (keyword id)))
    (^Currency [id ^Registry registry] (of-id (keyword id) registry)))

  (unit
    (^Currency [id] (unit (keyword id)))
    (^Currency [id ^Registry registry] (unit (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [c] (id (keyword c)))
    (^clojure.lang.Keyword [c ^Registry registry] (id (keyword c) registry)))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency*) (keyword id)))
    (^Boolean [id ^Registry registry]
     (contains? (registry/currency-id->currency* registry) (keyword id))))

  (present?
    (^Boolean [id]
     (present? (keyword id)))
    (^Boolean [id ^Registry registry]
     (present? (keyword id) registry)))

  clojure.lang.IPersistentMap

  (to-id
    ^clojure.lang.Keyword [m]
    (or (some-> (get m :id)   to-id)
        (some-> (get m :code) to-id)))

  (to-code
    ^clojure.lang.Keyword [m]
    (or (some-> (get m :id)   to-code)
        (some-> (get m :code) to-code)))

  (to-id-str
    ^String [m]
    (or (some-> (get m :id)   to-id-str)
        (some-> (get m :code) to-id-str)))

  (to-code-str
    ^String [m]
    (or (some-> (get m :id)   to-code-str)
        (some-> (get m :code) to-code-str)))

  (to-numeric-id
    ^long [m]
    (or (some-> (get m :nr)      to-numeric-id)
        (some-> (get m :numeric) to-numeric-id)
        no-numeric-id))

  (to-currency
    ^Currency [m]
    (map->new m))

  (to-map
    [m]
    (let [id (or (get m :id) (get m :code))
          nr (or (get m :nr) (get m :numeric))
          sc (or (get m :sc) (get m :scale))
          ki (or (get m :ki) (get m :kind))
          we (or (get m :we) (get m :weight))
          do (or (get m :do) (get m :domain))
          r  {}
          r  (if id (assoc r :id (to-id id)) r)
          r  (if nr (let [nr (to-numeric-id nr)]         (if nr (assoc r :nr (long nr)) r)) r)
          r  (if sc (let [sc (normalize-scale-hint sc)]  (if (identical? sc invalid-map-hint) r (assoc r :sc (int sc)))) r)
          r  (if ki (let [ki (normalize-kind-hint ki)]   (if ki (assoc r :ki ki) r)) r)
          r  (if we (let [we (normalize-weight-hint we)] (if (identical? we invalid-map-hint) r (assoc r :we (int we)))) r)
          r  (if do (let [do (normalize-domain-hint do)] (if do (assoc r :do do) r)) r)]
      r))

  (definitive?
    (^Boolean [m]
     (let [id-ok?      (some? (to-id m))
           has-domain? (or (contains? m :do) (contains? m :domain))
           has-scale?  (or (contains? m :sc) (contains? m :scale))
           has-nr?     (or (contains? m :nr) (contains? m :numeric))]
       (and id-ok? has-domain? has-scale? has-nr?))))

  (resolve
    (^Currency [m]
     (resolve m (registry/get)))
    (^Currency [m ^Registry registry]
     (some-> (resolve-all m registry) first)))

  (resolve-all
    (^clojure.lang.IPersistentSet [m]
     (resolve-all m (registry/get)))
    (^clojure.lang.IPersistentSet [m ^Registry registry]
     (let [^Registry registry (registry/get registry)
           id                 (when (contains? m :id)      (to-id   (get m :id)))
           code               (when (contains? m :code)    (to-code (get m :code)))
           nr-raw             (cond (contains? m :nr)      (get m :nr)
                                    (contains? m :numeric) (get m :numeric)
                                    :else                  nil)
           nr                 (when (some? nr-raw) (to-numeric-id nr-raw))
           nr                 (when (and (some? nr) (valid-numeric-id? (long nr))) (long nr))
           candidates         (-> nil
                                  (merge-candidates (some-> id   (resolve-all registry)))
                                  (merge-candidates (some-> code (resolve-all registry)))
                                  (merge-candidates (some-> nr   (resolve-all registry))))]
       (when (seq candidates)
         (keep-in-sorted-set-where #(compare-map-ids ^Currency % m) candidates)))))

  (of-id
    (^Currency [m]
     (map-registry-op! :of-id m nil))
    (^Currency [m ^Registry registry]
     (map-registry-op! :of-id m registry)))

  (unit
    (^Currency [m ^Registry registry]
     (let [^Registry registry (unit-registry registry)
           hits               (resolve-all m registry)]
       (cond
         (nil? hits)
         (throw
          (ex-info
           "Currency not found in a registry."
           {:registry registry
            :value    m
            :op       :unit}))
         :else
         (first hits))))
    (^Currency [m]
     (unit m (registry/get))))

  (id
    (^clojure.lang.Keyword [m] (to-id m))
    (^clojure.lang.Keyword [m ^Registry registry]
     (if (nil? registry)
       (to-id m)
       (map-registry-op! :id m registry))))

  (defined?
    (^Boolean [m]
     (map-registry-op! :defined? m (registry/get)))
    (^Boolean [m ^Registry registry]
     (map-registry-op! :defined? m registry)))

  (present?
    (^Boolean [m]
     (map-registry-op! :present? m (registry/get)))
    (^Boolean [m ^Registry registry]
     (map-registry-op! :present? m registry)))

  nil

  (to-id
    [_]
    nil)

  (to-code
    [_]
    nil)

  (to-id-str
    [_]
    nil)

  (to-code-str
    [_]
    nil)

  (to-numeric-id
    [_]
    nil)

  (to-currency
    [_]
    nil)

  (definitive?
    (^Boolean [_]
     false))

  (to-map
    [_]
    nil)

  (resolve
    ([_] nil)
    ([_ ^Registry _registry] nil))

  (resolve-all
    ([_] nil)
    ([_ ^Registry _registry] nil))

  (of-id
    ([_] nil)
    ([_ ^Registry _registry] nil))

  (unit
    ([_] nil)
    ([_ ^Registry _registry] nil))

  (id
    (^clojure.lang.Keyword [_] nil)
    (^clojure.lang.Keyword [_ ^Registry _registry] nil))

  (defined?
    (^Boolean [_] false)
    (^Boolean [_ ^Registry _registry] false))

  (present?
    (^Boolean [_] false)
    (^Boolean [_ ^Registry _registry] false)))

(defn parse-currency-code
  "Internal helper which transforms currency codes into keywords."
  {:no-doc true :added "1.0.2"}
  ([c]
   (if (and (map? c) (contains? c :id))
     (core-update c :id keyword)
     (if (and (symbol? c) (present? c))
       (keyword c) c)))
  ([c env]
   (if (and (map? c) (contains? c :id))
     (core-update c :id keyword)
     (if-not (symbol? c) c
             (if (or (contains? env c) (clojure.core/resolve c)) c
                 (keyword c))))))

(defmacro attempt*
  "Soft currency coercion macro.

  Returns `c` when it is already an instance of `io.randomseed.bankster.Currency`.
  If `c` is a definitive currency representation (see `Monetary/definitive?`), it is
  coerced locally using `to-currency`. Otherwise it attempts to resolve `c` in a
  registry and returns the registered `Currency` instance or `nil` when it cannot be
  resolved.

  Unlike `unit` and `of-id`, this helper is meant to be used in \"non-throwing\" code
  paths (e.g. property predicates). Argument `c` is evaluated exactly once.

  When `registry` is not provided, the default registry is used (preferring
  `io.randomseed.bankster.registry/*default*` when bound)."
  {:added "2.0.0"}
  ([c]
   `(let [c# ~c]
      (cond (instance? Currency c#) c#
            (map? c#)               (to-currency c#)
            (definitive? c#)        (or (to-currency c#) (resolve c# (registry/get)))
            :else                   (resolve c# (registry/get)))))
  ([c registry]
   (if (or (nil? registry) (identical? 'nil registry))
     `(attempt* ~c)
     `(let [c#        ~c
            registry# ^io.randomseed.bankster.Registry ~registry]
        (cond
          (instance? Currency c#) c#
          (map? c#)               (to-currency c#)
          (definitive? c#)        (or (to-currency c#) (resolve c# registry#))
          :else                   (resolve c# registry#))))))

(defn attempt
  "Soft currency coercion function.

  Returns `c` when it is already an instance of `io.randomseed.bankster.Currency`.
  If `c` is a definitive currency representation (see `Monetary/definitive?`), it is
  coerced locally using `to-currency`. Otherwise it attempts to resolve `c` in a
  registry and returns the registered `Currency` instance or `nil` when it cannot be
  resolved.

  This is intended for \"non-throwing\" code paths (e.g. property predicates) and
  complements `unit` and `of-id` which may throw when a currency is missing."
  {:added "2.0.0"}
  ([c]
   (cond
     (instance? Currency c) c
     (map? c)               (to-currency c)
     (definitive? c)        (or (to-currency c) (resolve c (registry/get)))
     :else                  (resolve c (registry/get))))
  ([c ^Registry registry]
   (cond
     (instance? Currency c) c
     (map? c)               (to-currency c)
     (definitive? c)        (or (to-currency c) (resolve c registry))
     :else                  (resolve c registry))))

(defmacro with-attempt
  "Evaluates `c` and tries to coerce it to a `Currency`.

  If coercion succeeds, binds the resulting currency to the given symbol and
  evaluates `body`, returning its result. If coercion fails, returns `false`.

  Uses `attempt*` for coercion (non-throwing).

  The binding name can be given either as a symbol or as a single-element vector
  (mirroring `if-some` style):

  (with-attempt x registry c   (.id c))
  (with-attempt x registry [c] (.id c))"
  {:added "2.0.0"}
  [c registry binding & body]
  (let [sym (cond (symbol? binding)
                  binding

                  (and (vector? binding)
                       (== 1 (count binding))
                       (symbol? (nth binding 0)))
                  (nth binding 0)

                  :else
                  (throw
                   (ex-info "with-attempt expects a symbol or a single-element vector as a binding."
                            {:binding binding})))
        ;; Hint the local binding to avoid reflection on field/method access in body
        sym (with-meta sym (assoc (meta sym) :tag io.randomseed.bankster.Currency))]
    `(if-some [~sym (attempt* ~c ~registry)]
       (do ~@body)
       false)))

(defmacro of
  "Returns a currency for the given value by querying the given registry or a global
  registry, which may be shadowed by the value of
  `io.randomseed.bankster.registry/*default* (see
  ``io.randomseed.bankster.registry/with`` or `with-registry`)."
  {:added "1.0.0"}
  ([currency]
   (if (map? currency)
     (let [cur# (parse-currency-code currency &env)]
       `(new-currency ~cur#))
     (let [cur# (parse-currency-code currency &env)]
       `(unit ~cur#))))
  ([currency registry]
   (let [cur# (parse-currency-code currency &env)]
     `(unit ~cur# ~registry))))

;;
;; Currency properties.
;;

(defn nr
  "Returns currency numeric ID as a long number. For currencies without an assigned
  number it will return nil. Locale argument is ignored."
  {:added "1.0.0"}
  ([c]
   (when-some [^Currency c (unit c)]
     (let [n (long (.numeric ^Currency c))]
       (when-not (== n no-numeric-id) n))))
  ([c ^Registry registry]
   (when-some [^Currency c (if (instance? Currency c)
                             c
                             (unit c (unit-registry registry)))]
     (let [n (long (.numeric ^Currency c))]
       (when-not (== n no-numeric-id) n))))
  ([c _locale ^Registry registry]
   (nr c registry)))

(def ^{:arglists '([c]
                   [c ^Registry registry]
                   [c locale ^Registry registry])}
  numeric-id
  "Alias for nr."
  nr)

(def ^{:arglists '([c]
                   [c ^Registry registry]
                   [c locale ^Registry registry])}
  numeric
  "Alias for nr."
  nr)

(defn sc
  "Returns currency scale (decimal places) as a number. For currencies without the
  assigned decimal places it will return `nil` (the value of auto-scaled). Locale
  argument is ignored."
  {:added "1.0.0"}
  ([c]
   (when-some [^Currency c (unit c)]
     (let [sc (unchecked-int (.scale ^Currency c))]
       (when-not (== sc auto-scaled) (long sc)))))
  ([c ^Registry registry]
   (when-some [^Currency c (if (instance? Currency c)
                             c
                             (unit c (unit-registry registry)))]
     (let [sc (unchecked-int (.scale ^Currency c))]
       (when-not (== sc auto-scaled) (long sc)))))
  ([c _locale ^Registry registry]
   (sc c registry)))

(def ^{:arglists '([c]
                   [c ^Registry registry]
                   [c locale ^Registry registry])}
  scale
  "Alias for sc."
  sc)

(defn domain
  "Returns currency domain as a keyword. For currencies added with simple
  identifiers (without a namespace) and numerical IDs present it will be
  `:ISO-4217`. For currencies with namespace-qualified identifiers it will be the
  upper-cased namespace name (e.g. `:CRYPTO`) set during creation of a currency
  object. Locale argument is ignored."
  {:tag clojure.lang.Keyword :added "1.0.0"}
  (^clojure.lang.Keyword [c]
   (when-some [^Currency c (unit c)] (.domain ^Currency c)))
  (^clojure.lang.Keyword [c ^Registry registry]
   (when-some [^Currency c (if (instance? Currency c)
                             c
                             (unit c (unit-registry registry)))]
     (.domain ^Currency c)))
  (^clojure.lang.Keyword [c _locale ^Registry registry]
   (domain c registry)))

(def ^{:tag      clojure.lang.Keyword
       :arglists '(^clojure.lang.Keyword [c]
                   ^clojure.lang.Keyword [c ^Registry registry]
                   ^clojure.lang.Keyword [c locale ^Registry registry])}
  ns
  "Alias for domain."
  domain)

(defn kinds
  "Returns a map of all registered currency kinds as keys and their ancestors as
  sets. It will not contain ad-hoc kinds that wasn't added to a registry's
  hierarchy. Uses the given `registry` or a global one."
  {:added "2.0.0" :tag clojure.lang.IPersistentMap}
  ([]
   (kinds nil))
  ([^Registry registry]
   (let [^Registry registry (registry/get registry)]
     (when-some [h (registry/hierarchy* :kind registry)]
       (get h :parents)))))

(defn kind
  "Returns currency kind. It is a keyword which describes origin of its value. Currently
  known top-level kinds are:

  - `:iso`       - ISO-4217 currencies, funds, commodities, special markers.
  - `:virtual`   - Virtual units (stable tokens, credits, native tokens, special).
  - `:currency`  - Meta: currency-like units; parent for `:fiduciary` money.
  - `:asset`     - Meta: value-bearing units (`:assets`, `:claims`, `:stable`, reference-based).
  - `:fiat`      - Meta umbrella: fiat-related tags (issuer fiats vs fiat-as-anchor).
  - `:funds`     - Meta: funds, settlement units, units of account.
  - `:commodity` - Meta: commodity-based units and commodity anchoring.
  - `:special`   - Meta: special-purpose markers (`:experimental`, `:test`, `:null`).

  The function may return `nil` if the currency is a no-currency. Locale argument is
  ignored. To list all known kinds use `kinds`."
  {:tag clojure.lang.Keyword :added "1.0.0"}
  (^clojure.lang.Keyword [c]
   (when-some [c (unit c)] (.kind ^Currency c)))
  (^clojure.lang.Keyword [c ^Registry registry]
   (when-some [^Currency c (if (instance? Currency c) c (unit c (unit-registry registry)))]
     (.kind ^Currency c)))
  (^clojure.lang.Keyword [c _locale ^Registry registry]
   (kind c registry)))

(defn ns-code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced the namespace will be used as a prefix and slash character
  as a separator. Locale argument is ignored."
  {:tag String :added "1.0.0"}
  (^String [c]
   (when-some [cids (not-empty (str (id c)))] (subs cids 1)))
  (^String [c ^Registry registry]
   (let [^Registry registry (unit-registry registry)]
     (when-some [cids (not-empty (str (id c registry)))] (subs cids 1))))
  (^String [c _locale ^Registry registry]
   (ns-code c registry)))

(defn code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced only the base code (without a namespace) will be
  returned. Locale argument is ignored."
  {:tag String :added "1.0.0"}
  (^String [c] (when-some [cid (id c)] (core-name cid)))
  (^String [c ^Registry registry]
   (let [^Registry registry (unit-registry registry)]
     (when-some [cid (id c registry)] (core-name cid))))
  (^String [c _locale ^Registry registry]
   (code c registry)))

(defn weight
  "Returns weight of the given currency (used to resolve conflicts when getting
  currencies having conflicting currency codes). Returned type should be int but may
  cast to long."
  {:tag 'int :added "1.0.2"}
  ([c] (when-some [c (unit c)] (int (.weight ^Currency c))))
  ([c ^Registry registry]
   (when-some [^Currency c (if (instance? Currency c)
                             c
                             (unit c (unit-registry registry)))]
     (int (.weight ^Currency c))))
  ([c _locale ^Registry registry]
   (weight c registry)))

(defn info
  "Returns a map describing the given currency, including registry-associated
  properties when available.

  Base fields come from the Monetary `to-map` representation. When a registry can be
  consulted (default or explicitly passed), this function also adds:

  - `:countries` - a set of associated country IDs,
  - `:localized` - a localized properties map (locale keywords, including `:*`),
  - `:traits`    - a set of associated traits.

  Returns `nil` when the given value points to a registry currency but cannot be
  resolved. Locale argument is ignored."
  {:tag clojure.lang.IPersistentMap :added "2.0.0"}
  ([c]
   (info c (registry/get)))
  ([c ^Registry registry]
   (let [^Registry registry (unit-registry registry)
         r (with-attempt c registry [cur]
             (let [cid  (.id ^Currency cur)
                   ctrs (registry/currency-id->country-ids* cid registry)
                   lcl  (registry/currency-id->localized* cid registry)
                   lcl  (when (and (map? lcl) (pos? (count lcl)))
                          (not-empty
                           (map/map-keys
                            (comp keyword str l/locale)
                            lcl)))
                   trts (registry/currency-id->traits* cid registry)]
               (cond-> (to-map cur)
                 (seq ctrs) (assoc :countries ctrs)
                 (seq lcl)  (assoc :localized lcl)
                 (seq trts) (assoc :traits trts))))]
     (when-not (false? r) r)))
  ([c _locale ^Registry registry]
   (info c registry)))

;;
;; Currency - country relations.
;;

(defn countries
  "Returns a set of country IDs (keywords) for which the given currency is main
  currency. If there are no countries associated with a currency, returns nil. Locale
  argument is ignored.

  Throws when a currency cannot be resolved in the registry. This lets consumers
  distinguish \"no countries\" (`nil`) from \"unknown currency\" (exception)."
  {:tag clojure.lang.PersistentHashSet :added "1.0.0"}
  (^clojure.lang.PersistentHashSet [c]
   (countries c (registry/get)))
  (^clojure.lang.PersistentHashSet [c ^Registry registry]
   (when (some? c)
     (let [^Registry registry (unit-registry registry)
           cid               (id c registry)]
       (if (registry/currency-id->currency* cid registry)
         (registry/currency-id->country-ids* cid registry)
         (throw (ex-info
                 "Currency not found in a registry."
                 {:op       :countries
                  :value    c
                  :id       cid
                  :registry registry}))))))
  (^clojure.lang.PersistentHashSet [c _locale ^Registry registry]
   (countries c registry)))

(defn of-country
  "Returns a currency for the given country identified by a country ID (which should be
  a keyword). If there is no currency or country of the given ID does not exist,
  returns nil. Locale argument is ignored."
  {:tag Currency :added "1.0.0"}
  (^Currency [^clojure.lang.Keyword country-id]
   (of-country country-id (registry/get)))
  (^Currency [^clojure.lang.Keyword country-id ^Registry registry]
   (registry/country-id->currency* (keyword country-id) (unit-registry registry)))
  (^Currency [^clojure.lang.Keyword country-id _locale ^Registry registry]
   (of-country country-id registry)))

;;
;; Converting to Java object.
;;

(defn java
  "For ISO-standardized currency, returns corresponding `java.util.Currency` object. If
  the currency does not exist, is not ISO (its `:domain` is not `:ISO-4217`), has a
  different scale (fraction digits), or a different numeric code, `nil` is returned."
  {:tag java.util.Currency :added "1.0.0"}
  (^java.util.Currency [currency]
   (java currency nil))
  (^java.util.Currency [currency ^Registry registry]
   (let [^Registry registry (unit-registry registry)]
     (when-some [^Currency currency (attempt currency registry)]
       (when (identical? :ISO-4217 (.domain ^Currency currency))
         (let [nr (.numeric ^Currency currency)
               sc (.scale   ^Currency currency)]
           (when (and (not (== no-numeric-id nr))
                      (not (val-auto-scaled*? sc)))
             (when-some [^String code (code currency)]
               (when-some [^java.util.Currency jc (try (java.util.Currency/getInstance code)
                                                      (catch Throwable _ nil))]
                 (when (and (== nr (long (.getNumericCode ^java.util.Currency jc)))
                            (== sc (int  (.getDefaultFractionDigits ^java.util.Currency jc))))
                   jc))))))))))

;;
;; Parsing and structuring helpers.
;;

(def ^{:tag clojure.lang.PersistentHashSet :const true :private true :added "2.0.0"}
  currency-attr-keys
  "Currency record keys that may be present in a config map (excluding :id)."
  #{:numeric :kind :scale :domain :weight})

(def ^{:tag clojure.lang.PersistentHashSet :const true :private true :added "2.0.0"}
  currency-map-non-propagatable-keys
  "Keys reserved by currency construction and/or config loading and not eligible for
  propagation into a Currency extension map."
  #{:id :code
    :nr :numeric
    :sc :scale
    :ki :kind
    :do :domain
    :we :weight
    :propagate-keys})

(defn- prep-propagate-keys
  "Prepares a propagate-keys value by converting it into a set of keywords.

  This helper is used when loading currencies from config maps. Keys that overlap
  with currency constructor keys (e.g. :kind, :numeric) are ignored, so that config
  cannot accidentally corrupt Currency fields via extension merges."
  {:tag clojure.lang.PersistentHashSet :added "2.0.0" :private true}
  [ks]
  (when (some? ks)
    (let [ks (cond
               (set? ks)        ks
               (sequential? ks) ks
               (and (seqable? ks) (not (string? ks)))
               (seq ks)
               :else
               (list ks))
          ks (remove nil? ks)
          ks (into #{}
                   (comp (map keyword)
                         (remove currency-map-non-propagatable-keys))
                   ks)]
      (when (seq ks) ks))))

(defn prep-currency
  "Prepares currency attributes which may come from an external data source. Returns a
  currency."
  {:tag Currency :added "1.0.0" :private true}
  (^Currency [[id attrs]]
   (prep-currency id attrs nil))
  (^Currency [id attrs]
   (prep-currency id attrs nil))
  (^Currency [id {:keys [numeric kind scale domain weight] :as attrs} propagate-keys]
   (let [missing (reduce (fn [s k] (if (contains? attrs k) s (conj s k)))
                         #{}
                         currency-attr-keys)
         pk-spec (if (contains? attrs :propagate-keys)
                   (clojure.core/get attrs :propagate-keys)
                   propagate-keys)
         pk      (prep-propagate-keys pk-spec)
         extra   (when (seq pk)
                   (let [m (select-keys attrs pk)]
                     (when (pos? (count m)) m)))
         c       (prep-currency id numeric kind scale domain weight)
         c       (if extra (merge c extra) c)]
     (when (some? c)
       (with-meta c (assoc (or (meta c) {}) ::missing-fields missing)))))
  (^Currency [id numeric kind scale]
   (prep-currency id numeric kind scale nil 0))
  (^Currency [id numeric kind scale domain weight]
   (when (some? id)
     (let [numeric (long (if (number? numeric) numeric (or (bu/try-parse-long numeric) no-numeric-id)))
           numeric (long (if (< numeric 1) no-numeric-id numeric))
           scale   (int  (if (number? scale) scale (or (bu/try-parse-int scale) auto-scaled)))
           scale   (int  (if (< scale 0) auto-scaled scale))
           weight  (long (if (number? weight) weight (or (bu/try-parse-int weight) 0)))
           kind    (when (some? kind) (keyword kind))]
       (new-currency (keyword id) (long numeric) (long scale) kind domain weight)))))

(defn prep-currencies
  "Prepares a map of currency ID to currency based on a configuration map of currency
  ID to currency attributes."
  {:tag clojure.lang.IPersistentMap :added "1.0.0" :private true}
  ([^clojure.lang.IPersistentMap m]
   (prep-currencies m nil))
  ([^clojure.lang.IPersistentMap m propagate-keys]
   (let [propagate-keys (prep-propagate-keys propagate-keys)]
     (pmap (fn [[id attrs]] (prep-currency id attrs propagate-keys)) m))))

(defn prep-cur->ctr
  "Prepares countries map which may come from an external data source. Expects a map of
  country ID to currency ID. Returns a map of currency ID to sets of country IDs."
  {:tag clojure.lang.IPersistentMap :added "1.0.0" :private true}
  [ctr-id->cur-id]
  (->> ctr-id->cur-id
       (map/remove-empty-values)
       (map/map-keys-and-vals #(vector (keyword %1) (keyword %2)))
       (map/invert-in-sets)
       (map/remove-empty-values)))

(defn prep-country-ids
  "Prepares country identifiers by converting the given object into a sequence of
  keywords."
  {:tag clojure.lang.LazySeq :added "1.0.0" :private true}
  [country-ids]
  (when country-ids
    (let [cids (if (sequential? country-ids) country-ids
                   (if (and (seqable? country-ids) (not (string? country-ids)))
                     (seq country-ids)
                     (list country-ids)))
          cids (if (set? cids) cids (distinct cids))]
      (map keyword cids))))

(defn prep-localized-props
  "Prepares localized properties map for a single currency."
  {:tag clojure.lang.IPersistentMap :added "1.0.0" :private true}
  [^clojure.lang.IPersistentMap p]
  (map/map-keys-and-vals
   #(vector (let [k (keyword %1)] (if (identical? :* k) k (l/locale k)))
            (map/map-vals str %2)) p))

(defn prep-all-localized-props
  "Prepares localized properties map for all currencies in a map."
  {:tag clojure.lang.IPersistentMap :added "1.0.0" :private true}
  [^clojure.lang.IPersistentMap p]
  (map/map-vals prep-localized-props p))

(defn prep-traits
  "Prepares currency traits by converting the given object into a set of keywords."
  {:tag clojure.lang.PersistentHashSet :added "2.0.0" :private true}
  [traits]
  (when traits
    (let [ts (cond
               (set? traits)        traits
               (sequential? traits) traits
               (and (seqable? traits) (not (string? traits)))
               (seq traits)
               :else
               (list traits))
          ts (map keyword ts)
          ts (remove nil? ts)]
      (when (seq ts) (set ts)))))

(defn prep-all-traits
  "Prepares traits map for all currencies in a map."
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [^clojure.lang.IPersistentMap p]
  (into {}
        (keep (fn [[cid traits]]
                (when-some [ts (prep-traits traits)]
                  (vector (keyword cid) ts))))
        p))

(defn weighted-currencies
  "Constructor for weighted currency buckets: smallest weight wins.
  Total order: (weight asc) then (id asc)."
  {:tag clojure.lang.PersistentTreeSet :private true :added "2.0.0"}
  ([]
   (sorted-set-by
    (fn [^Currency a ^Currency b]
      (let [wa (int (.weight a))
            wb (int (.weight b))]
        (if (== wa wb)
          (compare (.id a) (.id b))
          (compare wa wb))))))
  (^clojure.lang.PersistentTreeSet [^clojure.lang.PersistentTreeSet s]
   (or s (weighted-currencies))))

(defn remove-currency-by-id-from-set
  "Removes all currencies with the given ID from a sorted set.
  Returns nil if empty afterwards."
  {:tag clojure.lang.PersistentTreeSet :private true :added "2.0.0"}
  [^clojure.lang.PersistentTreeSet s ^clojure.lang.Keyword cid]
  (when s
    (let [r (reduce (fn [^clojure.lang.PersistentTreeSet acc ^Currency cur]
                      (if (identical? cid (.id cur))
                        acc
                        (conj acc cur)))
                    (weighted-currencies)
                    s)]
      (when (pos? (count r)) r))))

(defn remove-weighted-currency
  "Removes a currency object from a sorted set associated with a currency code keyword
  in a map."
  {:tag clojure.lang.PersistentHashMap :private true :added "1.0.2"}
  [^clojure.lang.PersistentHashMap m cid]
  (let [code (if (namespace cid) (keyword (core-name cid)) cid)]
    (if-some [currencies-set (get m code)]
      (if-some [new-currencies (remove-currency-by-id-from-set currencies-set cid)]
        (assoc  m code new-currencies)
        (dissoc m code))
      m)))

(defn register-numeric
  "Updates numeric indexes:
   - `:cur-nr->curs` => sorted-set
   - `:cur-nr->cur`  => canonical currency (first from set)"
  {:tag Registry :private true :added "1.0.2"}
  [^Registry registry ^Currency c]
  (let [nr (long (.numeric c))]
    (if-not (valid-numeric-id? nr)
      registry
      (let [nr->curs (registry/currency-nr->currencies* registry)
            old-set  (get nr->curs nr)
            ;; safety/idempotence: drop any old entry with same ID (even if unregister missed it)
            old-set  (remove-currency-by-id-from-set old-set (.id c))
            new-set  (conj (weighted-currencies old-set) c)
            canon    (first new-set)
            nr->cur  (registry/currency-nr->currency* registry)]
        (-> registry
            (assoc :cur-nr->curs (assoc nr->curs nr new-set))
            (assoc :cur-nr->cur  (assoc nr->cur  nr canon)))))))

;;
;; Adding and removing to/from registry.
;;

(defn remove-countries-core
  "Removes countries from the given registry. Also unlinks constrained currencies in
  proper locations. Returns updated registry."
  {:tag Registry :private true :added "1.0.0"}
  [^Registry registry country-ids]
  (if-not (some? (seq country-ids))
    registry
    (let [ctr-to-cur   (registry/country-id->currency* registry)
          cid-to-ctrs  (registry/currency-id->country-ids* registry)
          currency-ids (map #(.id ^Currency %) (distinct (filter identity (map ctr-to-cur country-ids))))
          new-cid-ctr  (reduce #(apply core-update %1 %2 disj country-ids) cid-to-ctrs currency-ids)]
      (-> registry
          (assoc :cur-id->ctr-ids (map/remove-empty-values new-cid-ctr currency-ids))
          (assoc :ctr-id->cur     (apply dissoc ctr-to-cur country-ids))))))

(defn unregister
  "Removes a currency from the given registry. Also removes country mappings and all
  localized properties associated with a currency. Returns updated registry.

  Numeric-ID indexes:

  - removes the currency from `:cur-nr->curs` (shared numeric IDs bucket)
  - updates `:cur-nr->cur` to the canonical currency (first in bucket), or removes the
    numeric entry if bucket becomes empty."
  {:tag Registry :added "1.0.0"}
  [^Registry registry currency]
  (when registry
    (let [^Currency cur        (if (instance? Currency currency) currency (of-id currency registry))
          cid                  (.id ^Currency cur)
          ;; prefer actual record from registry (for disj/removal and consistency)
          ^Currency registered (get (registry/currency-id->currency* registry) cid cur)
          ;; numeric handling (bucket + canonical)
          nr                   (long (.numeric ^Currency registered))
          has-nr?              (valid-numeric-id? nr)
          nr->curs             (when has-nr? (registry/currency-nr->currencies* registry))
          old-bucket           (when has-nr? (get nr->curs nr))
          new-bucket           (when has-nr? (remove-currency-by-id-from-set old-bucket cid))
          ^Registry registry   (if-not has-nr?
                                 registry
                                 (let [nr->cur (registry/currency-nr->currency* registry)]
                                   (if new-bucket
                                     (-> registry
                                         (assoc :cur-nr->curs (assoc nr->curs nr new-bucket))
                                         (assoc :cur-nr->cur  (assoc nr->cur  nr (first new-bucket))))
                                     (-> registry
                                         (assoc :cur-nr->curs (dissoc nr->curs nr))
                                         (assoc :cur-nr->cur  (dissoc nr->cur  nr))))))

          ;; countries
          country-ids (registry/currency-id->country-ids* cid registry)

          ;; main removals
          ^Registry registry (-> registry
                                 (map/dissoc-in [:cur-id->cur       cid])
                                 (map/dissoc-in [:cur-id->localized cid])
                                 (map/dissoc-in [:cur-id->ctr-ids   cid])
                                 (core-update   :cur-code->curs remove-weighted-currency cid))]

      ;; remove country -> currency entries
      (if (seq country-ids)
        (apply core-update registry :ctr-id->cur dissoc country-ids)
        registry))))

(defn remove-countries
  "Removes countries from the given registry. Also unlinks constrained currencies in
  proper locations. Returns updated registry."
  {:tag Registry :added "1.0.0"}
  [^Registry registry country-ids]
  (when registry
    (if (nil? country-ids)
      registry
      (remove-countries-core registry (prep-country-ids country-ids)))))

(defn add-countries
  "Associates the given country or countries with a currency. If the currency does not
  exist, exception is thrown. If the currency exists but differs in any detail from
  the existing currency from the registry, exception is thrown. If the currency
  exists and equals to the given in any aspect, country associations are added. Links
  from other countries to the currency are not removed unless the country is already
  linked with some other currency; in this case it will be unlinked first."
  {:tag Registry :added "1.0.0"}
  [^Registry registry currency-id country-ids]
  (when (some? registry)
    (when-not (defined? currency-id registry)
      (throw
       (ex-info (str "Currency "
                     (if (instance? Currency currency-id) (.id ^Currency currency-id) currency-id)
                     " does not exist in a registry.") {:currency-id currency-id})))
    (let [^Currency c (of-id currency-id registry)
          cid         (.id ^Currency c)
          cids        (prep-country-ids country-ids)]
      (if (nil? (seq cids)) registry
          (as-> registry regi
            (remove-countries-core regi cids)
            (apply update-in regi [:cur-id->ctr-ids cid] (fnil conj #{}) (set cids))
            (core-update regi :ctr-id->cur (partial apply assoc) (interleave cids (repeat c))))))))

(defn remove-localized-properties
  "Removes localized properties assigned to a currency in a registry. Returns updated
  registry. Expects currency ID (or equivalent) to be passed as a second argument."
  {:tag Registry :added "1.0.0"}
  [^Registry registry currency-id]
  (when registry
    (if (nil? currency-id)
      registry
      (map/dissoc-in registry [:cur-id->localized (.id ^Currency (of-id currency-id registry))]))))

(defn add-localized-properties
  "Adds localized properties of a currency to the given registry. Overwrites existing
  properties."
  {:tag Registry :added "1.0.0"}
  [^Registry registry ^Currency currency-id properties]
  (when (some? registry)
    (when-not (defined? currency-id registry)
      (throw
       (ex-info (str "Currency "
                     (if (instance? Currency currency-id) (.id ^Currency currency-id) currency-id)
                     " does not exist in a registry.") {:currency-id currency-id})))
    (let [^Currency c (of-id currency-id registry)
          cid         (.id ^Currency c)]
      (if (and (map? properties) (pos? (count properties)))
        (assoc-in registry [:cur-id->localized cid] (prep-localized-props properties))
        registry))))

(defn add-weighted-code
  "Associates the existing currency code with the currency object in the given registry
  using a value of the .weight field. The given currency-id may be expressed with any
  object that can be used to get the currency from a registry (internally the unit
  function is used). Therefore, passing the currency object having a different weight
  will not cause the weight to be updated since it will be used for identification
  only.

  Currency must exist in the cur-id->cur database of the registry. This function will
  add an association to the cur-code->curs database. If both, the weight and code are
  the same, registry is returned as is.

  This is a low-level function used to ensure that a code-to-currency mapping is
  created within a registry. To simply update currency's weight, use the update
  or update! function."
  {:tag Registry :added "1.2.5"}
  [^Registry registry currency-id]
  (when (some? registry)
    (when-not (defined? currency-id registry)
      (throw
       (ex-info (str "Currency "
                     (if (instance? Currency currency-id) (.id ^Currency currency-id) currency-id)
                     " does not exist in a registry.") {:currency-id currency-id})))
    (let [^Currency c (of-id currency-id registry)
          cid         (.id c)
          kw-code     (if (simple-keyword? cid) cid (keyword (core-name cid)))
          curs        (registry/currency-code->currencies* kw-code registry)
          already?    (some #(identical? (.id ^Currency %) cid) curs)]
      (if already?
        registry
        (update-in registry [:cur-code->curs kw-code]
                   (fn [^clojure.lang.PersistentTreeSet s]
                     (conj (weighted-currencies (remove-currency-by-id-from-set s cid)) c)))))))

(defn register
  "Adds a currency and optional, associated country mappings and/or localized
  properties to the given registry. Returns updated registry.

  The optional country-ids argument should be a sequence of keywords (however, if
  a single keyword is given it will be converted to a single-element sequence) with
  country codes which should be associated with the given currency.

  The optional localized-properties argument should be a map of localized properties
  of the given currency. See the Data Structures documentation section for more
  information.

  If the update mode is enabled then all of the existing countries associated with the
  currency are removed and replaced with the provided ones. To simply add new
  countries, use add-countries. Also note that update mode removes localized
  properties so new one must be provided."
  {:tag      Registry :added "1.0.0"
   :arglists '(^Registry [^Registry registry currency]
               ^Registry [^Registry registry currency country-ids]
               ^Registry [^Registry registry currency update-mode?]
               ^Registry [^Registry registry currency country-ids update-mode?]
               ^Registry [^Registry registry currency country-ids localized-properties]
               ^Registry [^Registry registry currency country-ids localized-properties update-mode?])}
  (^Registry [^Registry registry currency]
   (register registry currency nil nil false))
  (^Registry [^Registry registry currency country-ids-or-update?]
   (if (boolean? country-ids-or-update?)
     (register registry currency nil nil country-ids-or-update?)
     (register registry currency country-ids-or-update? nil false)))
  (^Registry [^Registry registry currency country-ids localized-or-update]
   (if (boolean? localized-or-update)
     (register registry currency country-ids nil localized-or-update)
     (register registry currency country-ids localized-or-update false)))
  (^Registry [^Registry registry currency country-ids localized-properties ^Boolean update?]
   (when (some? registry)
     (let [^Currency c (if (instance? Currency currency) currency (of-id currency registry))
           cid         (.id ^Currency c)
           cid-to-cur  (registry/currency-id->currency* registry)]
       (when-not update?
         (when-some [^Currency p (get cid-to-cur cid)]
           (throw (ex-info
                   (str "Currency " cid " already exists in a registry.")
                   {:currency c, :existing-currency p}))))
       (let [^Registry registry (unregister registry c)
             cid-to-cur         (registry/currency-id->currency* registry)
             ^Registry registry (assoc registry :cur-id->cur (assoc cid-to-cur cid c))
             ^Registry registry (register-numeric registry c)]
         (-> registry
             (add-weighted-code        currency)
             (add-countries            currency country-ids)
             (add-localized-properties currency localized-properties)))))))

(declare localized-properties)

(defn update
  "Replaces a currency in the given registry by a new one, preserving localized
  properties, relation to countries and code if not explicitly given. Returns updated
  registry. If the currency does not exist in a registry yet, it will be registered."
  {:tag Registry :added "1.1.0"}
  ([^Registry registry currency]
   (update registry currency nil nil))
  ([^Registry registry currency country-ids]
   (update registry currency country-ids nil))
  ([^Registry registry currency country-ids localized-properties-map]
   (let [present (if (instance? Currency currency) currency (of-id currency registry))]
     (register registry
               present
               (or country-ids (countries present))
               (or localized-properties-map (localized-properties present))
               true))))

(defn update!
  "Replaces a currency in the global, shared registry by a new one, preserving
  localized properties, relation to countries and code if not explicitly
  given. Returns updated registry. If the currency does not exist in a registry yet,
  it will be registered."
  {:tag Registry :added "1.1.0"}
  ([currency]
   (swap! registry/R update currency nil nil))
  ([currency country-ids]
   (swap! registry/R update currency country-ids nil))
  ([currency country-ids localized-properties-map]
   (swap! registry/R update currency country-ids localized-properties-map)))

(defn register!
  "Adds currency and (optional) country to the global registry. Returns updated
  registry. When the currency is nil, returns current state of the global, shared
  registry (but not a dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  (^Registry [^Currency currency]
   (if (nil? currency) @registry/R (swap! registry/R register currency)))
  (^Registry [^Currency currency ^clojure.lang.Keyword country-id-or-update?]
   (if (nil? currency) @registry/R (swap! registry/R register currency country-id-or-update?)))
  (^Registry [^Currency currency ^clojure.lang.Keyword country-id localized-properties ^Boolean update?]
   (if (nil? currency) @registry/R (swap! registry/R register currency country-id localized-properties update?))))

(defn unregister!
  "Removes currency from the global registry. Automatically removes country constraints
  when necessary and localized properties associated with a currency. Returns updated
  registry.

  If the currency is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [^Currency currency]
  (if (nil? currency) @registry/R (swap! registry/R unregister currency)))

(defn add-countries!
  "Associates the given country (a keyword) or countries (seqable collection of
  keywords) with a currency in the global registry. If the currency does not exist,
  exception is thrown. If the currency exists but differs in any detail from the
  existing currency from the registry, exception is thrown. If the currency exists
  and equals to the given in any aspect, country associations are added. Links from
  other countries to the currency are not removed unless the country is already
  linked with some other currency; in this case it will be unlinked first.

  If the currency is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [^Currency currency country-ids]
  (when (nil? currency) @registry/R (swap! registry/R add-countries currency country-ids)))

(defn remove-countries!
  "Removes country (a keyword) or countries (seqable collection of keywords) from the
  global registry. Automatically removes currency constraints when necessary. Returns
  updated registry.

  If the country-ids is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [country-ids]
  (if (nil? country-ids) @registry/R (swap! registry/R remove-countries country-ids)))

(defn add-localized-props!
  "Associates the given currency with a map of localized properties.

  If the currency is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [^Currency currency properties]
  (when (nil? currency) @registry/R (swap! registry/R add-localized-properties currency properties)))

(defn remove-localized-props!
  "Removes localized properties of the given currency from the global registry.

  If the currency is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [^Currency currency]
  (if (nil? currency) @registry/R (swap! registry/R remove-localized-properties currency)))

;;
;; Currencies loading.
;;

(defn config->registry
  "Loads currencies and countries from an EDN file. First argument should be a string
  with path to the EDN resource file containing registry data, second should be a
  registry. Returns a registry initialized using values from the EDN file."
  {:tag Registry :added "1.0.0"}
  (^Registry []
   (config->registry config/default-resource-path (registry/new-registry)))
  (^Registry [^String resource-path]
   (config->registry resource-path (registry/new-registry)))
    (^Registry [^String resource-path ^Registry regi]
     (if-some [cfg (config/load resource-path)]
       (let [regi (or regi (registry/new-registry))
             hier (clojure.core/get cfg :hierarchies)
             regi (if (some? hier)
                    (registry/new-registry (assoc (into {} regi) :hierarchies hier))
                    regi)
             pks  (let [pks (prep-propagate-keys (config/propagate-keys cfg))]
                    (if (seq pks) (vec (sort-by str pks)) []))
             regi (clojure.core/update regi :ext (fnil assoc {}) :propagate-keys pks)
             curs (prep-currencies          (config/currencies cfg) pks)
             ctrs (prep-cur->ctr            (config/countries  cfg))
             lpro (config/localized                            cfg)
             trts (prep-all-traits          (config/traits     cfg))
             vers (str                      (config/version    cfg))
             regi (if (nil? vers) regi (assoc regi :version vers))
           ^Registry regi
           (reduce (fn ^Registry [^Registry r ^Currency c]
                     (let [cid (.id ^Currency c)]
                       (register r c (get ctrs cid) (get lpro cid) false)))
                   regi
                   curs)]
       (assoc regi :cur-id->traits trts))
     regi)))

;;
;; Setting default registry.
;;

(defn set-default-registry!
  "Sets default, global registry using a global configuration file and optional user's
  configuration file. The highlighted version of a registry will be sourced from the
  last configuration used."
  {:tag Registry :added "1.0.0"}
  (^Registry []
   (set-default-registry! config/default-resource-path config/user-resource-path))
  (^Registry [resource-path]
   (registry/set! (config->registry resource-path)))
  (^Registry [resource-path & more]
   (set-default-registry! resource-path)
   (doseq [path more]
     (registry/update! (partial config->registry path)))))

;;
;; Setting default currency.
;;

(defn set-default!
  "Sets default currency by altering `io.randomseed.bankster.currency/*default*`
  dynamic variable."
  {:tag Currency :added "1.0.0"}
  [c]
  (alter-var-root #'*default* (constantly ^Currency (unit c))))

(defn unset-default!
  "Sets default currency to nil by altering `io.randomseed.bankster.currency/*default*`
  dynamic variable."
  {:tag nil :added "1.0.0"}
  []
  (alter-var-root #'*default* (constantly nil)))

;;
;; Predicates.
;;

(defn currency?
  "Returns `true` if the given value is a currency. Registry argument is ignored."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (instance? Currency c))
  (^Boolean [c _registry] (instance? Currency c)))

(defn possible?
  "Returns `true` if the given value is a possible currency: can be used to get a
  currency from a registry, can be used to create currency on its own, or is a
  currency.

  This predicate is semantically wide. If the string or keyword alone can be used to
  create a valid, ad-hoc currency with automatic scale, and without any properties
  except identifier, then it will return `true`. Same for certain numbers: if they
  cannot be used to create a currency on their own (since there is no ID) but can be
  used to look one up in a registry (treating a number as an ISO-4217 code), they
  will be positively tested as possible values for currency creation (if there was a
  hit).

  If `registry` is not given or is `nil`, the default one is used."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c]
   (and (some? c)
        (some? (or (to-currency c) (attempt* c nil)))))
  (^Boolean [c ^Registry registry]
   (and (some? c)
        (some? (or (to-currency c) (attempt* c registry))))))

(defn iso-possible?
  "Returns `true` if the given value is a possible ISO-4217 currency: can be used to
  get an ISO currency from a registry, can be used to create ISO currency on its own,
  or is an ISO currency.

  This predicate is semantically wide. If the string or keyword alone cannot be used
  to create a valid, ad-hoc ISO currency but it can get an ISO currency from a
  registry, then the function will return `true`. Same for certain numbers: if they
  cannot be used to create an ISO currency on their own (since there is no ID) but
  can be used to look one up in a registry (treating a number as an ISO-4217 code),
  they will be positively tested as possible values for currency creation (if there
  was a hit).

  If `registry` is not given or is `nil`, the default one is used."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c]
   (and (some? c)
        (or (iso-strict-currency? (to-currency c))
            (iso-strict-currency? (attempt* c nil)))))
  (^Boolean [c ^Registry registry]
   (and (some? c)
        (or (iso-strict-currency? (to-currency c))
            (iso-strict-currency? (attempt* c registry))))))

(defn has-numeric-id?
  "Returns `true` if the given currency has a numeric ID."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c]
   (with-attempt c nil [c]
     (not (== no-numeric-id (.numeric ^Currency c)))))
  (^Boolean [c ^Registry registry]
   (with-attempt c registry [c]
     (not (== no-numeric-id (.numeric ^Currency c))))))

(defn has-country?
  "Returns `true` if the given currency has at least one country for which it is an
  official currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c]
   (let [^Registry registry (registry/get)]
     (with-attempt c registry [c]
       (contains? (registry/currency-id->country-ids* registry) (.id ^Currency c)))))
  (^Boolean [c ^Registry registry]
   (let [^Registry registry (registry/get registry)]
     (with-attempt c registry [c]
       (contains? (registry/currency-id->country-ids* registry) (.id ^Currency c))))))

(defn in-domain?
  "Returns `true` if the given currency has a domain set to the first given argument."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [ns c]
   (with-attempt c nil [c] (identical? ns (.domain ^Currency c))))
  (^Boolean [ns c ^Registry registry]
   (with-attempt c registry [c] (identical? ns (.domain ^Currency c)))))

(defn has-domain?
  "Returns `true` if the given currency `c` has its domain defined (when only currency
  and optional registry is given). If `ns` is given it returns `true` if the currency
  has its domain set to be the exact keyword given."
  {:tag      Boolean
   :added    "2.0.0"
   :arglists '([c] [c ^Registry registry] [c ns] [c ns ^Registry registry])}
  (^Boolean [c]
   (with-attempt c nil [c] (some? (.domain ^Currency c))))
  (^Boolean [c registry-or-ns]
   (if (nil? registry-or-ns)
     (has-domain? c)
     (if (instance? Registry registry-or-ns)
     (with-attempt c ^Registry registry-or-ns [c] (some? (.domain ^Currency c)))
     (with-attempt c nil [c] (identical? registry-or-ns (.domain ^Currency c))))))
  (^Boolean [c ns ^Registry registry]
   (with-attempt c registry [c]
     (identical? ns (.domain ^Currency c)))))

(defn of-domain?
  "Checks if a domain of the given currency `c` equals to the one given as a first
  argument `domain` or if it belongs to a `domain` (checked with `clojure.core/isa?`)."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [^clojure.lang.Keyword domain c]
   (let [^Registry registry (registry/get)
         h                  (some-> registry .hierarchies :domain)]
     (with-attempt c registry [c]
       (let [d (.domain ^Currency c)]
         (or (identical? domain d)
             (if h (isa? h d domain) (isa? d domain)))))))
  (^Boolean [^clojure.lang.Keyword domain c ^Registry registry]
   (let [^Registry registry (registry/get registry)
         h                  (some-> registry .hierarchies :domain)]
     (with-attempt c registry [c]
       (let [d (.domain ^Currency c)]
         (or (identical? domain d)
             (if h (isa? h d domain) (isa? d domain))))))))

(defn big?
  "Returns `true` if the given currency has an automatic scale (decimal places)."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c]
   (with-attempt c nil [c]
     (val-auto-scaled*? (.scale ^Currency c))))
  (^Boolean [c ^Registry registry]
   (with-attempt c registry [c]
     (val-auto-scaled*? (.scale ^Currency c)))))

(def ^{:tag      Boolean
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  auto-scaled?
  "Alias for big?."
  big?)

(defn crypto?
  "Returns `true` if the given currency is a cryptocurrency. It is just a helper that
  check if the domain of a currency equals to `:CRYPTO`."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (in-domain? :CRYPTO c))
  (^Boolean [c ^Registry registry] (in-domain? :CRYPTO c registry)))

(defn iso-strict?
  "Returns `true` if the given currency is an official currency which is currently in
  use (not a legacy money) and its both identifier and numerical identifier is
  compliant with the ISO standard."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c] (with-attempt c nil [c] (iso-strict-currency? c)))
  (^Boolean [c ^Registry registry] (with-attempt c registry [c] (iso-strict-currency? c))))

(defn iso-legacy?
  "Returns `true` if the given currency was an official currency but is now considered a
  legacy currency having an identifier compliant with the ISO standard. It is just a
  helper which checks if the `:domain` field of a currency equals to
  `:ISO-4217-LEGACY`."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c] (in-domain? :ISO-4217-LEGACY c))
  (^Boolean [c ^Registry registry] (in-domain? :ISO-4217-LEGACY c registry)))

(def ^{:tag      Boolean
       :added    "1.0.0"
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  official?
  "Alias for iso-strict?"
  iso-strict?)

(def ^{:tag      Boolean
       :added    "1.0.0"
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  standard?
  "Alias for iso-strict?"
  iso-strict?)

(def ^{:tag      Boolean
       :added    "1.3.0"
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  legacy?
  "Alias for iso-legacy?"
  iso-legacy?)

(def ^{:tag      Boolean
       :added    "1.3.0"
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  old?
  "Alias for iso-legacy?"
  iso-legacy?)

(defn has-kind?
  "Returns `true` if the given currency `c` has its kind defined (when only currency
  and optional registry is given). If `tag` is given it returns `true` if the
  currency has its kind set to be the exact keyword given."
  {:tag      Boolean
   :added    "2.0.0"
   :arglists '([c] [c ^Registry registry] [c tag ^Registry registry])}
  (^Boolean [c]
   (with-attempt c nil [c] (some? (.kind ^Currency c))))
  (^Boolean [c registry-or-tag]
   (cond
     (nil? registry-or-tag)
     (has-kind? c)

     (instance? Registry registry-or-tag)
     (with-attempt c ^Registry registry-or-tag [c]
       (some? (.kind ^Currency c)))

     :else
     (with-attempt c nil [c]
       (identical? registry-or-tag (.kind ^Currency c)))))
  (^Boolean [c tag ^Registry registry]
   (with-attempt c registry [c]
     (identical? tag (.kind ^Currency c)))))

(defn of-kind?
  "Checks if a kind of the given currency `c` equals to the one given as a second
  argument `kind` or if it belongs to a `kind` (checked with `clojure.core/isa?`)."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^clojure.lang.Keyword kind c]
   (of-kind? kind c (registry/get)))
  (^Boolean [^clojure.lang.Keyword kind c ^Registry registry]
   (let [^Registry registry (registry/get registry)
         h                  (some-> registry .hierarchies :kind)]
     (with-attempt c registry [c]
       (if-some [k (.kind ^Currency c)]
         (or (identical? kind k)
             (if h
               (isa? h k kind)
               (isa? k kind)))
         false)))))

(defn has-trait?
  "Returns `true` if the given currency `c` has any trait defined (when only currency
  and optional registry is given). If `tag` is given it returns `true` if the
  currency has one of its traits set to be the exact keyword given."
  {:tag      Boolean
   :added    "2.0.0"
   :arglists '(^Boolean [c] [c registry] [c tag] [c tag registry])}
  (^Boolean [c]
   (let [^Registry registry (registry/get)]
     (with-attempt c registry [c]
       (some?
        (not-empty
         (registry/currency-id->traits* (.id ^Currency c) registry))))))
  (^Boolean [c registry-or-tag]
   (if (nil? registry-or-tag)
     (has-trait? c)
     (if (instance? Registry registry-or-tag)
       (let [^Registry registry registry-or-tag]
         (with-attempt c registry [c]
           (some?
            (not-empty
             (registry/currency-id->traits* (.id ^Currency c) registry)))))
       (let [tag               registry-or-tag
             ^Registry registry (registry/get)]
         (with-attempt c registry [c]
           (contains? (registry/currency-id->traits* (.id ^Currency c) registry) tag))))))
  (^Boolean [c tag registry]
   (let [^Registry registry (registry/get registry)]
     (with-attempt c registry [c]
       (contains? (registry/currency-id->traits* (.id ^Currency c) registry) tag)))))

(defn of-trait?
  "Checks if any trait of the given currency `c` equals to the one given as a second
  argument `tag`, or if it belongs to a `tag` (checked with `clojure.core/isa?`)."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [^clojure.lang.Keyword tag c]
   (of-trait? tag c (registry/get)))
  (^Boolean [^clojure.lang.Keyword tag c ^Registry registry]
   (let [^Registry registry (registry/get registry)
         h                  (some-> registry .hierarchies :traits)]
     (with-attempt c registry [c]
       (if-let [t (registry/currency-id->traits* (.id ^Currency c) registry)]
         (if h
           (boolean (some #(isa? h % tag) t))
           (contains? t tag))
         false)))))

(defn iso?
  "Returns `true` if the given currency is classified as ISO because its kind is set to
  `:iso` or its descendants. It may include not just current ISO currencies but also
  legacy ones. See also `io.randomseed.bankster.currency/iso-strict?`."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :iso c))
  (^Boolean [c ^Registry registry] (of-kind? :iso c registry)))

(defn virtual?
  "Returns `true` if the given currency is a kind of virtual currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :virtual c))
  (^Boolean [c ^Registry registry] (of-kind? :virtual c registry)))

(defn asset?
  "Returns `true` if the given currency is a kind of asset."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :asset c))
  (^Boolean [c ^Registry registry] (of-kind? :asset c registry)))

(defn claim?
  "Returns `true` if the given currency is a kind of claim."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :claim c))
  (^Boolean [c ^Registry registry] (of-kind? :claim c registry)))

(defn credit?
  "Returns `true` if the given currency is a kind of credit."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :credit c))
  (^Boolean [c ^Registry registry] (of-kind? :credit c registry)))

(defn fiat?
  "Returns `true` if the given currency is a kind of fiat currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :fiat c))
  (^Boolean [c ^Registry registry] (of-kind? :fiat c registry)))

(defn real?
  "Returns `true` if the given currency is a kind of real currency (has its `:kind` set
  to `:currency` or its descendants."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :currency c))
  (^Boolean [c ^Registry registry] (of-kind? :currency c registry)))

(defn fiduciary?
  "Returns `true` if the given currency is a kind of fiduciary currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :fiduciary c))
  (^Boolean [c ^Registry registry] (of-kind? :fiduciary c registry)))

(defn funds?
  "Returns `true` if the given currency is a kind of funds/settlement currency."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c] (of-kind? :funds c))
  (^Boolean [c ^Registry registry] (of-kind? :funds c registry)))

(defn metal?
  "Returns `true` if the given currency is a kind of precious metal."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c] (of-kind? :metal c))
  (^Boolean [c ^Registry registry] (of-kind? :metal c registry)))

(defn commodity?
  "Returns `true` if the given currency is a kind of commodity currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :commodity c))
  (^Boolean [c ^Registry registry] (of-kind? :commodity c registry)))

(defn peg?
  "Returns `true` if the given currency is a kind of PEG currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :peg c))
  (^Boolean [c ^Registry registry] (of-kind? :peg c registry)))

(defn stable?
  "Returns `true` if the given currency is a kind of stable currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :stable c))
  (^Boolean [c ^Registry registry] (of-kind? :stable c registry)))

(defn staked?
  "Returns `true` if the given currency is a kind of staked currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :staked c))
  (^Boolean [c ^Registry registry] (of-kind? :staked c registry)))

(defn wrapped?
  "Returns `true` if the given currency is a kind of wrapped currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :wrapped c))
  (^Boolean [c ^Registry registry] (of-kind? :wrapped c registry)))

(defn referenced?
  "Returns `true` if the given currency is a kind of referenced currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :referenced c))
  (^Boolean [c ^Registry registry] (of-kind? :referenced c registry)))

(defn experimental?
  "Returns `true` if the given currency is a kind of experimental currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :experimental c))
  (^Boolean [c ^Registry registry] (of-kind? :experimental c registry)))

(defn special?
  "Returns `true` if the given currency is a kind of special currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :special c))
  (^Boolean [c ^Registry registry] (of-kind? :special c registry)))

(defn test?
  "Returns `true` if the given currency is a kind of test currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (of-kind? :test c))
  (^Boolean [c ^Registry registry] (of-kind? :test c registry)))

(defn null?
  "Returns `true` if the given currency is a kind of null currency."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c] (of-kind? :NULL c))
  (^Boolean [c ^Registry registry] (of-kind? :NULL c registry)))

(defn none?
  "Returns `true` if the given currency does not exist, or there is no currency (value
  is `nil`, `false`, an empty collection), or it is a pseudo-currency with its kind
  set to `:NULL` or its descendants."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c]
   (none? c nil))
  (^Boolean [c ^Registry registry]
   (or (not c)
       (and (seqable? c) (empty? c))
       (null? c registry)
       (and (not  (instance? Currency c))
            (nil? (resolve c registry))))))

(defn decentralized?
  "Returns `true` if the given currency is a kind of decentralized currency. It uses
  currency traits database in a registry to find a trait named
  `:control/decentralized`."
  {:tag Boolean :added "2.0.0"}
  (^Boolean [c] (of-trait? :decentralized c))
  (^Boolean [c ^Registry registry] (of-trait? :decentralized c registry)))

(def ^{:tag      Boolean
       :arglists '([c] [c ^Registry registry])}
  decentralised?
  "Alias for decentralized?"
  decentralized?)

;;
;; Localized properties.
;;

(def ^{:private true :tag clojure.lang.PersistentHashSet :added "1.0.0"}
  locale-seps
  "Locale parts separators."
  #{\_\-\.\#})

(def ^{:private true :tag clojure.lang.PersistentArrayMap}
  nat-helper
  "Translates certain currencies to the preferred locales."
  {:EUR :en    :USD :en_US :CHF :de_CH :MAD :ar_MA
   :GBP :en_GB :XPF :fr_PF :XAF :fr_CF :FKP :en_FK
   :NZD :en_NZ :ILS :he_IL :NOK :nb_NO :AUD :en_AU
   :XCD :en_CB :DKK :da    :ANG :nl_NL :XOF :fr_CF})

(defn localized-properties
  "Returns a map of localized properties for the given currency. Locale objects are
  translated back to their keyword representations.

  Throws when a currency cannot be resolved in the registry. Returns `nil` when there
  are no localized properties for the currency."
  {:tag clojure.lang.PersistentHashMap :added "1.0.8"}
  ([c]
   (localized-properties c (registry/get)))
  ([c ^Registry registry]
   (when (some? c)
     (let [^Registry registry (unit-registry registry)
           cid               (id c registry)]
       (when-not (registry/currency-id->currency* cid registry)
         (throw (ex-info
                 "Currency not found in a registry."
                 {:op       :localized-properties
                  :value    c
                  :id       cid
                  :registry registry})))
       (when-some [m (registry/currency-id->localized* cid registry)]
         (not-empty
          (map/map-keys
           (comp keyword str l/locale)
           m)))))))

(defn get-localized-property
  "Returns localized properties of the currency object for the given locale."
  {:private true :tag clojure.lang.IPersistentMap :added "1.0.0"}
  [p ^Locale locale m] (get (get m locale) p))

(defn localized-property
  "Gets the localized property of a currency from the given registry and locale. If the
  registry is not given, the global one will be used. If the locale is not given, the
  default locale for the environment will be used. Locale can be expressed as a
  Locale object or any object which can be used to identify the locale (e.g. a keyword
  or a string).

  Throws when a currency cannot be resolved in the registry. Returns `nil` when the
  property is not present for the currency or when the currency has no localized
  properties.

  Localized properties are maps keyed by currency identifiers, containing another
  maps keyed by locale objects. There is a special key :* which identifies default
  properties used when there are no properties in locale-keyed maps.

  Let's take a hypothetical entry of a registry database `.cur-id->localized`:

  ```clojure
  {:XXX {#object[java.util.Locale 0x3f5af72f \"en_US\"] {:symbol \"$\"}}
        {#object[java.util.Locale 0x3f5af72f \"en\"]    {:symbol \"X$\"}
        {:* {:symbol \"ABC\"}}
  ```

  * Calling `(localized-property :symbol :XXX :en_US)` will return `$`.
  * Calling `(localized-property :symbol :XXX :en_GB)` will return `X$`.
  * Calling `(localized-property :symbol :XXX :pl_PL)` will return `ABC`.

  The first is self-explanatory.

  The second falls back to `en` because the function will re-try the language
  part (`en`) alone when the property was not found. The same method is repeatedly
  applied to other components of a locale (variant, script and extensions – if
  any). So for the locale identified by `th_TH_TH_#u-nu-thai` the following keys will
  be tried:

  - `th_TH_TH_#u-nu-thai`,
  - `th_TH_TH_#u-nu`,
  - `th_TH_TH_#u`,
  - `th_TH_TH`,
  - `th_TH` and `th`.

  The third example renders `ABC` because there is an entry of `:symbol` in the
  default properties map under the key `:*`.

  Please note that functions for getting particular properties may apply additional
  steps to obtain them. For instance, the display-name function will first call the
  localized-property and if it fails it will fall back to Java methods (if the
  currency is ISO-standardized). If that will fail too then it will return a currency
  code."
  {:tag clojure.lang.IPersistentMap :added "1.0.0"}
  ([property currency-id] (localized-property property
                                              currency-id
                                              (Locale/getDefault)
                                              (registry/get)))
  ([property currency-id locale] (localized-property property
                                                     currency-id
                                                     locale
                                                     (registry/get)))
  ([property currency-id locale ^Registry registry]
   (let [^Registry registry (unit-registry registry)
         cid    (id currency-id registry)
         locale (l/locale locale)]
     (when (some? cid)
       (when-not (registry/currency-id->currency* cid registry)
         (throw (ex-info
                 "Currency not found in a registry."
                 {:op       :localized-property
                  :property property
                  :value    currency-id
                  :id       cid
                  :registry registry})))
       (when-some [m (registry/currency-id->localized* cid registry)]
         (or (get (get m locale) property)
             (some #(and (some? %)
                         (not (contains? locale-seps
                                         (.charAt ^String %
                                                  (unchecked-dec (count %)))))
                         (get (get m (l/locale %)) property))
                   (sm/all-prefixes locale-seps (str locale)))
             (get (get m :*) property)))))))

(def ^{:tag      String :added "1.0.0"
       :arglists '(^String [currency]
                   ^String [currency locale]
                   ^String [currency locale ^Registry registry])}
  symbol
  "Returns a currency symbol as a string for the given currency object and locale. If
  the locale is not given, a default one is used. Uses global registry if a registry
  is not given.

  The following tactic is applied:

  - The registry field .cur-id->localized is looked up for the currency ID key. If
  it's found then a key with the given locale object (a kind of `java.util.Locale`) is
  obtained. If there is no such key, the default one :* is tried (a keyword). The
  resulting value should be a map of localized properties in which an entry under the
  key `:symbol` should exist. Its value will be returned, if found.

  - If the above method failed and the given currency is ISO-standardized then Java
  methods will be tried to obtain it.

  A locale can be expressed as `java.util.Locale` object or any other object (like a
  string or a keyword) which can be used to look up the locale."

  (memoize
   (fn symbol
     (^String [c]        (symbol c (Locale/getDefault) (registry/get)))
     (^String [c locale] (symbol c locale (registry/get)))
     (^String [c locale ^Registry registry]
      (let [^Registry registry (unit-registry registry)
            lc                 (l/locale locale)
            lp                 (try (localized-property :symbol c lc registry)
                                    (catch clojure.lang.ExceptionInfo _e nil))]
        (if (some? lp)
          lp
          (let [scode (code c registry)]
            (or (when (iso? c registry)
                  (try (-> scode
                           ^java.util.Currency (java.util.Currency/getInstance)
                           (.getSymbol lc))
                       (catch IllegalArgumentException _e nil)))
                scode))))))))

(defn symbol-native
  "Like symbol but for ISO-standardized currencies uses locale assigned to the first
  country where a currency is the default. When locale is given it is ignored."
  {:tag      String :added "1.0.0"
   :arglists '(^String [currency]
               ^String [currency ^Registry registry])}
  ([c]
   (let [^Registry registry (registry/get)
         cid               (id c registry)
         lc                (map/lazy-get nat-helper
                                        cid
                                        (first (registry/currency-id->country-ids* cid registry)))]
     (symbol c lc registry)))
  ([c registry]
   (let [^Registry registry (unit-registry registry)
         cid               (id c registry)
         lc                (map/lazy-get nat-helper
                                        cid
                                        (first (registry/currency-id->country-ids* cid registry)))]
     (symbol c lc registry)))
  ([c _locale registry]
   (symbol-native c registry)))

(def ^{:tag      String :added "1.0.0"
       :arglists '(^String [currency]
                   ^String [currency locale]
                   ^String [currency locale ^Registry registry])}
  display-name
  "Returns a currency display name as a string for the given currency object and
  locale. If the locale is not given, a default one is used. Uses global registry if
  a registry is not given.

  The following tactic is applied:

  - The registry field .cur-id->localized is looked up for the currency ID key. If
    it's found then a key with the given locale object (a kind of `java.util.Locale`)
    is obtained. If there is no such key, the default one :* is tried (a keyword). The
    resulting value should be a map of localized properties in which an entry under the
    key `:symbol` should exist. Its value will be returned, if found.

  - If the above method failed and the given currency is ISO-standardized then Java
    methods will be tried to obtain it.

  - If the above method failed a currency code will be returned.

  A locale can be expressed as `java.util.Locale` object or any other object (like a
  string or a keyword) which can be used to look up the locale."

  (memoize
   (fn display-name
     (^String [c]        (display-name c (Locale/getDefault) (registry/get)))
     (^String [c locale] (display-name c locale (registry/get)))
     (^String [c locale ^Registry registry]
      (let [^Registry registry (unit-registry registry)
            lc                 (l/locale locale)
            lp                 (try (localized-property :name c lc registry)
                                    (catch clojure.lang.ExceptionInfo _e nil))]
        (if (some? lp) lp
            (let [scode (code c registry)]
              (or (when (iso? c registry)
                    (try (-> scode
                             ^java.util.Currency (java.util.Currency/getInstance)
                             (.getDisplayName lc))
                         (catch IllegalArgumentException _e nil)))
                  scode))))))))

(defn display-name-native
  "Like display-name but for ISO-standardized currencies uses locale assigned to the
  first country where a currency is the default. When locale is given it is ignored."
  {:tag      String :added "1.0.0"
   :arglists '(^String [currency]
               ^String [currency ^Registry registry])}
  ([c]
   (let [^Registry registry (registry/get)
         cid               (id c registry)
         lc                (map/lazy-get nat-helper
                                        cid
                                        (first (registry/currency-id->country-ids* cid registry)))]
     (display-name c lc registry)))
  ([c registry]
   (let [^Registry registry (unit-registry registry)
         cid               (id c registry)
         lc                (map/lazy-get nat-helper
                                        cid
                                        (first (registry/currency-id->country-ids* cid registry)))]
     (display-name c lc registry)))
  ([c _locale registry]
   (display-name-native c registry)))

(def ^{:tag String :added "1.0.0"
       :arglists '(^String [currency]
                   ^String [currency locale]
                   ^String [currency locale ^Registry registry])}
  name
  "Alias for display-name."
  display-name)

(def ^{:tag String :added "1.0.0"
       :arglists '(^String [currency]
                   ^String [currency ^Registry registry])}
  name-native
  "Alias for display-name-native."
  display-name-native)

;;
;; Scalable implementation.
;;

(extend-protocol scale/Scalable

  Currency

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] true)

  (of ^long [c] (long (.scale ^Currency c)))

  (^Currency apply
   (^Currency [c] ^Currency c)
   (^Currency [c ^long scale] (assoc c :scale (int scale)))
   (^Currency [c ^long scale ^RoundingMode _rounding-mode] (assoc c :scale (int scale))))

  (amount
    ([_]     nil)
    ([_ _]   nil)
    ([_ _ _] nil))

  java.util.Currency

  (^Boolean scalable? [_] true)
  (^Boolean applied?  [_] true)

  (of ^long [c] (long (.getDefaultFractionDigits ^java.util.Currency c)))

  (^Currency apply
   (^Currency [c] ^Currency c)
   (^Currency [c ^long scale] (assoc c :scale (int scale)))
   (^Currency [c ^long scale ^RoundingMode _rounding-mode] (assoc c :scale (int scale))))

  (amount
    ([_]     nil)
    ([_ _]   nil)
    ([_ _ _] nil))

  clojure.lang.Keyword

  (^Boolean scalable? [c] (defined? c))
  (^Boolean applied?  [c] (defined? c))

  (of ^long [c] (long (.scale ^Currency (unit c))))

  (^Currency apply
   (^Currency [c] ^Currency (unit c))
   (^Currency [c ^long scale] (assoc (unit c) :scale (int scale)))
   (^Currency [c ^long scale ^RoundingMode _rounding-mode] (assoc (unit c) :scale (int scale))))

  (amount
    ([_]     nil)
    ([_ _]   nil)
    ([_ _ _] nil)))

;;
;; Contextual macros.
;;

(defmacro with
  "Sets a default currency in a lexical context of the body."
  {:added "1.0.0"}
  [currency & body]
  `(binding [*default* (of ~currency)]
     ~@body))

(defmacro with-registry
  "Sets a registry in a lexical context of the body to be used instead of a global one
  in functions which require the registry and it was not passed as an argument. Has
  the same effect as `registry/with`."
  {:added "1.0.0"}
  [^Registry registry & body]
  `(binding [registry/*default* ^Registry ~registry]
     ~@body))

;;
;; Tagged literals.
;;

(defn code-literal
  "Tagged literal handler for Clojure code. Emits compound forms that are going to be
  evaluated."
  {:added "1.2.4"}
  [arg]
  (if (or (nil? arg) (and (map? arg) (< (count arg) 1)))
    '(quote nil)
    `(of ~arg)))

(defn data-literal
  "Tagged literal handler for EDN data files. Emits Currency objects or nil values."
  {:added "1.2.4"}
  [arg]
  (if (or (nil? arg) (and (map? arg) (< (count arg) 1)))
    '(quote nil)
    (of arg)))

;;
;; Formatting.
;;

(def ^{:private true :tag java.util.Currency :added "1.0.0"}
  iso-ref-currency
  "Reference ISO currency used to construct a formatter for non-ISO currencies."
  (java.util.Currency/getInstance "XXX"))

(def ^{:no-doc true :tag DecimalFormat :added "1.0.0"}
  formatter-instance
  "For the specified locale and currency, returns a cached, mutable currency
  text-formatter (`java.text.DecimalFormat`). If no locale is given, uses the
  default one. If no registry is given, uses dynamic or global registry. Due to
  caching strategy it is advised to express locale with a keyword.

  Do not use this function directly since operating on the returned object may
  change it for the whole program. Use formatter instead."
  (memoize
   (fn formatter-instance
     (^DecimalFormat [currency]
      (formatter-instance currency ^Locale (Locale/getDefault)))
     (^DecimalFormat [currency locale]
      (formatter-instance currency locale nil))
     (^DecimalFormat [currency locale registry]
      (let [cur                   (if (nil? registry) (unit currency) (unit currency registry))
            ^Locale locale-native (l/locale locale)
            f                     (NumberFormat/getCurrencyInstance ^Locale locale-native)
            iso                   (iso? cur registry)
            sc                    (int (.scale ^Currency cur))]
        (.setCurrency              ^DecimalFormat f ^Currency (if iso (java ^Currency cur) iso-ref-currency))
        (.setRoundingMode          ^DecimalFormat f ^RoundingMode scale/ROUND_UNNECESSARY)
        (.setParseIntegerOnly      ^DecimalFormat f false)
        (.setMaximumFractionDigits ^DecimalFormat f sc)
        (.setMinimumFractionDigits ^DecimalFormat f sc)
        (when-not iso
          (let [syms (.getDecimalFormatSymbols ^DecimalFormat f)]
            (.setCurrencySymbol ^DecimalFormatSymbols syms ^String (symbol ^Currency cur))
            (.setDecimalFormatSymbols ^DecimalFormat f ^DecimalFormatSymbols syms)))
        f)))))

(defn formatter
  "Returns currency formatter as `java.text.DecimalFormat` instance for the given
  currency and locale. If locale is not given the default one will be used. Due to
  caching strategy it is advised to express locale with a keyword.

  The formatter is a mutable clone of Java data structure.

  In case of currencies other than ISO-standardized (and predefined in Java) the
  currency field of this formatter will be set to the currency of XXX."
  {:tag DecimalFormat :added "1.0.0"}
  ([currency]
   (.clone
    ^DecimalFormat (formatter-instance currency
                                       (Locale/getDefault)
                                       (registry/get))))
  ([currency locale]
   (.clone
    ^DecimalFormat (formatter-instance currency
                                       locale
                                       (registry/get))))
  ([currency locale registry]
   (.clone
    ^DecimalFormat (formatter-instance
                    currency
                    locale
                    registry))))

(defn formatter-extended
  "Returns a currency formatter as `java.text.DecimalFormat` instance for the given
  currency, customizable with the given opts map. If the locale is not given then the
  default one will be used. Due to caching strategy it is advised to express locale
  with a keyword.

  The formatter is a mutable clone of Java data structure.

  In case of currencies other than ISO-standardized (and predefined in Java) the
  currency field of this formatter will be set to the currency of XXX.

  Options map can have the following keys:

  - `:rounding-mode`   - RoundingMode, rounding mode to apply when scaling
  - `:grouping`        - Boolean, if true then grouping will be used
  - `:grouping-size`   - integer, size of a group when grouping
  - `:negative-prefix` - String, negative prefix to use
  - `:negative-suffix` - String, negative suffix to use
  - `:positive-prefix` - String, positive prefix to use
  - `:positive-suffix` - String, positive suffix to use
  - `:always-sep-dec`  - Boolean, if true then the decimal separator will always be shown
  - `:currency-symbol-fn`  - a function used on a bankster/Currency object to get its symbol as a string
  - `:min-fraction-digits` - integer, the minimum number of digits allowed in the fraction portion of an amount
  - `:min-integer-digits` - integer, the minimum number of digits allowed in the integer portion of an amount
  - `:max-fraction-digits` - integer, the maximum number of digits allowed in the fraction portion of an amount
  - `:max-integer-digits`  - integer, the maximum number of digits allowed in the integer portion of an amount
  - `:scale`               - sets both `:min-fraction-digits` and `:max-fraction-digits` to the same value.

  When choosing different currency, all parameters of a formatter are initially set
  to that currency. Additionally re-scaling may take place for the amount if scales
  are different.

  The function assigned to the `:currency-symbol-fn` should take 3 arguments:
  currency, locale and registry.

  It is advised to express locale using a keyword when huge amount of operations is
  expected."
  {:tag DecimalFormat :added "1.0.0"}
  ([currency]
   (formatter currency (Locale/getDefault) (registry/get)))
  ([currency locale]
   (formatter currency locale (registry/get)))
  ([currency locale opts]
   (formatter-extended currency locale opts (registry/get)))
  ([currency locale
    {:keys
     [scale rounding-mode grouping grouping-size negative-prefix negative-suffix positive-prefix positive-suffix
      always-sep-dec currency-symbol-fn min-fraction-digits max-fraction-digits
      min-integer-digits max-integer-digits] :as opts}
    registry]
   (let [f                   (.clone ^DecimalFormat (formatter-instance currency locale registry))
         rounding-mode       (or rounding-mode scale/ROUND_UNNECESSARY)
         min-fraction-digits (or scale min-fraction-digits)
         max-fraction-digits (or scale max-fraction-digits)]
     (when currency-symbol-fn
       (let [syms (.getDecimalFormatSymbols ^DecimalFormat f)]
         (.setCurrencySymbol ^DecimalFormatSymbols syms ^String (currency-symbol-fn currency locale registry))
         (.setDecimalFormatSymbols ^DecimalFormat f ^DecimalFormatSymbols syms)))
     (when (contains? opts :grouping)       (.setGroupingUsed ^DecimalFormat f (boolean grouping)))
     (when (contains? opts :always-sep-dec) (.setDecimalSeparatorAlwaysShown ^DecimalFormat f (boolean always-sep-dec)))
     (when rounding-mode       (.setRoundingMode          ^DecimalFormat f ^RoundingMode rounding-mode))
     (when grouping-size       (.setGroupingSize          ^DecimalFormat f (int grouping-size)))
     (when negative-prefix     (.setNegativePrefix        ^DecimalFormat f ^String (str negative-prefix)))
     (when negative-suffix     (.setNegativeSuffix        ^DecimalFormat f ^String (str negative-suffix)))
     (when positive-prefix     (.setPositivePrefix        ^DecimalFormat f ^String (str positive-prefix)))
     (when positive-suffix     (.setPositiveSuffix        ^DecimalFormat f ^String (str positive-suffix)))
     (when min-integer-digits  (.setMinimumIntegerDigits  ^DecimalFormat f (int min-integer-digits)))
     (when max-integer-digits  (.setMaximumIntegerDigits  ^DecimalFormat f (int max-integer-digits)))
     (when min-fraction-digits (.setMinimumFractionDigits ^DecimalFormat f (int min-fraction-digits)))
     (when max-fraction-digits (.setMaximumFractionDigits ^DecimalFormat f (int max-fraction-digits)))
     f)))

;;
;; Printing.
;;

(defmethod ^{:added "1.0.0"} print-method Currency
  [c w]
  (let [sc  (int  (.scale   ^Currency c))
        nr  (long (.numeric ^Currency c))
        wei (int  (.weight  ^Currency c))
        ki  (.kind          ^Currency c)
        dom (.domain        ^Currency c)]
    (print-simple
     (str "#currency{"
          ":id "    (.id ^Currency c)
          (when     (some? dom)            (str ", :domain " dom))
          (when     (some? ki)             (str ", :kind "    ki))
          (when-not (== nr no-numeric-id)  (str ", :numeric " nr))
          (when-not (val-auto-scaled*? sc) (str ", :scale "   sc))
          (when-not (zero? wei)            (str ", :weight " wei))
          "}")
     w)))

;;
;; Populating registry with defaults.
;;

(when io.randomseed.bankster/*initialize-registry*
  (set-default-registry!))
