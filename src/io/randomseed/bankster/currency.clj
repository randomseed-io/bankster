(ns io.randomseed.bankster.currency

  ^{:doc    "Bankster library, currency operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :rename {ns core-ns new core-new symbol core-symbol
                           name core-name update core-update})

  (:require [trptr.java-wrapper.locale       :as            l]
            [smangler.api                    :as           sm]
            [clojure.string                  :as          str]
            [io.randomseed.bankster          :refer      :all]
            [io.randomseed.bankster.config   :as       config]
            [io.randomseed.bankster.registry :as     registry]
            [io.randomseed.bankster.scale    :as        scale]
            [io.randomseed.bankster.util.map :as          map]
            [io.randomseed.bankster.util     :refer      :all])

  (:import  [io.randomseed.bankster Currency Registry]
            [java.math RoundingMode]
            [java.text NumberFormat DecimalFormat DecimalFormatSymbols]
            [java.util Locale]))

;;
;; Constants.
;;

(def ^{:tag 'long :const true :added "1.0.0"}
  no-numeric-id
  "Expresses the value of currency's numeric ID which does not exist."
  (long -1))

(def ^{:tag 'int :const true :added "1.0.0"}
  auto-scaled
  "Expresses the scale of a currency which is automatic and not limited to certain
  decimal places."
  (int -1))

;;
;; Default currency.
;;

(def ^{:added "1.0.0" :tag Currency :dynamic true}
  *default*
  "Default currency unit to be applied when creating Money objects without the currency
  specified."
  nil)

;;
;; Auto-scaling predicate for scale.
;;

(defn val-auto-scaled?
  "Returns true if the given scale is equal to auto-scaled."
  {:added "1.0.0"}
  [scale]
  (= auto-scaled scale))

;;
;; Currency constructor
;;

(declare map->new)

(defn new-currency
  "Creates new currency record from values passed as arguments."
  {:added "1.0.0" :tag Currency}
  (^Currency [id]
   (if (map? id)
     (map->new id)
     (new-currency id nil nil nil nil nil)))
  (^Currency [id numeric-id]                   (new-currency id numeric-id nil nil nil nil))
  (^Currency [id numeric-id scale]             (new-currency id numeric-id scale nil nil nil))
  (^Currency [id numeric-id scale kind]        (new-currency id numeric-id scale kind nil nil))
  (^Currency [id numeric-id scale kind domain] (new-currency id numeric-id scale kind domain nil))
  (^Currency [id numeric-id scale kind domain weight]
   (when (some? id)
     (let [numeric-id (or numeric-id no-numeric-id)
           scale      (or scale auto-scaled)
           weight     (or weight 0)
           ns-domain  (keyword (try-upper-case (namespace id)))
           domain     (if (nil? domain) ns-domain
                          (keyword
                           (str/upper-case
                            (if (ident? domain)
                              (core-name domain)
                              (let [d (str domain)] (when (seq d) d))))))]
       (when (and (some? ns-domain) (not= domain ns-domain))
         (throw (ex-info
                 (str "Currency domain should reflect its namespace (upper-cased) if the namespace is set.")
                 {:id id :domain domain :namespace (namespace id)})))
       (Currency. (keyword id)
                  (long numeric-id)
                  (int scale)
                  (keyword kind)
                  (keyword domain)
                  (int weight))))))

(defn map->new
  "Creates new currency record from a map."
  {:added "1.0.0" :tag Currency}
  [^clojure.lang.IPersistentMap m]
  (when (and (some? m) (> (count m) 0))
    (let [id     (:id m)
          nr     (or (:nr m) (:numeric m) no-numeric-id)
          sc     (or (:sc m) (:scale   m) auto-scaled)
          kind   (or (:ki m) (:kind    m))
          domain (or (:do m) (:domain  m))
          weight (or (:we m) (:weight  m) (int 0))]
      (when-some [c ^Currency (new-currency id nr sc kind domain weight)]
        (if (> (count m) 1)
          (merge c (dissoc m :id :nr :numeric :sc :scale :ki :kind :do :domain :we :weight))
          c)))))

(def ^{:tag Currency
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
  data (keywords, symbols, strings, native Currency objects etc.)."
  (^{:tag clojure.lang.Keyword :added "1.0.0"}
   id
   [id] [id registry]
   "Returns a unique identifier of the given currency as a keyword. The currency can
  be expressed as a Currency object, a keyword, a string or any other type which is
  recognized by the `unit` protocol method.

  The default registry role is advisory here. If the registry argument is not
  given (or it is nil) and the dynamic variable
  `io.randomseed.bankster.registry/*default*` does not refer to a truthy value then
  the ID will be returned regardless of whether the currency exists in a registry, by
  simply converting it to a keyword or getting a field from a Currency object. Still,
  the default registry will be consulted to resolve possible currency code. For
  instance: if `BTC` is a currency code of a registered currency identified as
  `:crypto/BTC` then the resulting value for `:BTC` will be `:crypto/BTC`; but for
  `:BLABLA` (which does not exist in a registry) the resulting value will be
  `:BLABLA`.

  If a registry is given (or a dynamic registry is set) then trying to use a
  non-existing currency will cause an exception to be thrown.")

  (^{:tag io.randomseed.bankster.Currency :added "1.0.0"}
   of-id
   [id] [id registry]
   "Returns a currency object for the given ID and a registry. If the registry is not
  given, it will use the global one, but will first try a registry bound to the
  `io.randomseed.bankster.registry/*default*` dynamic variable.")

  (^{:tag io.randomseed.bankster.Currency :added "1.0.2"}
   unit
   [id] [id registry]
   "Returns a currency object for the given ID or currency code. If the registry is
  not given, it will try a registry bound to the
  `io.randomseed.bankster.registry/*default*` dynamic variable and if it is not
  set (or is falsy) it will use the global registry.

  If a Currency object is passed, it will be returned as is without consulting the
  registry, unless the registry is given (and not nil) or the dynamic registry is
  set. In such case the currency will be obtained from the registry on a basis of the
  ID extracted from the given currency object.

  If the registry is given (or dynamic registry is set) and the currency does not
  exist in a registry an exception will be thrown, regardless of whether a currency
  object was passed.

  Explicitly passing nil as a second argument when a Currency object is given can
  speed things up a bit by bypassing dynamic variable check.")

  (^{:tag Boolean :added "1.0.0"}
   defined?
   [id] [id registry]
   "Returns true if the given currency (identified by its ID) exists in a
  registry. If the registry is not given, the global one will be used, trying a
  dynamic registry bound to the registry/*default* first.")

  (^{:tag Boolean :added "1.0.2"}
   present?
   [id] [id registry]
   "Returns true if a currency of the given currency code or ID exists in a
  registry. If the registry is not given, the global one will be used, trying a
  dynamic registry bound to the registry/*default* first.")

  (^{:tag Boolean :added "1.0.0"}
   same-ids?
   [a b] [a b registry]
   "Returns true if two currencies have the same ID. If the registry is not given,
  it will use the global one, but will first try a dynamic registry bound to the
  `io.randomseed.bankster.registry/*default*` dynamic variable."))

;;
;; Currency querying functions, Monetary implementation.
;;

(extend-protocol Monetary

  Currency

  (of-id
    (^Currency [currency]
     (if-let [r registry/*default*] (of-id currency r) currency))
    (^Currency [currency ^Registry registry]
     (if (nil? registry) currency
         (unit (.id ^Currency currency) registry))))

  (unit
    (^Currency [currency]
     (if-let [r registry/*default*] (unit currency r) currency))
    (^Currency [currency ^Registry registry]
     (if (nil? registry) currency
         (of-id (.id ^Currency currency) ^Registry registry))))

  (id
    (^clojure.lang.Keyword [currency] (.id ^Currency currency))
    (^clojure.lang.Keyword [currency ^Registry registry] (.id ^Currency currency)))

  (defined?
    (^Boolean [currency]
     (contains? (registry/currency-id->currency) (.id ^Currency currency)))
    (^Boolean [currency ^Registry registry]
     (contains? (registry/currency-id->currency registry) (.id ^Currency currency))))

  (present?
    (^Boolean [currency]
     (present? (.id ^Currency currency)))
    (^Boolean [currency ^Registry registry]
     (present? (.id ^Currency currency) registry)))

  (same-ids?
    (^Boolean [a b] (= (.id ^Currency a) (id b)))
    (^Boolean [a b ^Registry registry] (= (.id ^Currency a) (id b registry))))

  Number

  (of-id
    (^Currency [num]
     (of-id num (registry/get)))
    (^Currency [num ^Registry registry]
     (or (get (registry/currency-nr->currency registry) num)
         (throw (ex-info
                 (str "Currency with the numeric ID of " num " not found in a registry.")
                 {:registry registry})))))

  (unit
    (^Currency [num]
     (of-id num (registry/get)))
    (^Currency [num ^Registry registry]
     (of-id num registry))
    (^Currency [num ^Registry registry _]
     (of-id num registry)))

  (id
    (^clojure.lang.Keyword [num]
     (id (long num) (registry/get)))
    (^clojure.lang.Keyword [num ^Registry registry]
     (if-some [c (get (registry/currency-nr->currency registry) num)]
       (.id ^Currency c)
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry registry})))))

  (defined?
    (^Boolean [num]
     (contains? (registry/currency-nr->currency) num))
    (^Boolean [num ^Registry registry]
     (contains? (registry/currency-nr->currency registry) num)))

  (present?
    (^Boolean [num]
     (contains? (registry/currency-nr->currency) num))
    (^Boolean [num ^Registry registry]
     (contains? (registry/currency-nr->currency registry) num)))

  (same-ids?
    (^Boolean [a b]
     (let [r (registry/get)]
       (if-some [^Currency c (get (registry/currency-nr->currency r) a)]
         (= (.id ^Currency c) (id b))
         (throw (ex-info
                 (str "Currency with the numeric ID of " num " not found in a registry.")
                 {:registry r})))))
    (^Boolean [a b ^Registry registry]
     (if-some [^Currency c (get (registry/currency-nr->currency registry) a)]
       (= (.id ^Currency c) (id b registry))
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry registry})))))

  clojure.lang.Keyword

  (of-id
    (^Currency [id]
     (of-id id (registry/get)))
    (^Currency [id ^Registry registry]
     (or (get (registry/currency-id->currency registry) id)
         (throw (ex-info
                 (str "Currency " (core-symbol id) " not found in a registry.")
                 {:registry registry})))))

  (unit
    (^Currency [id]
     (unit id (registry/get)))
    (^Currency [id ^Registry registry]
     (or (if (namespace id)
           (get (registry/currency-id->currency registry) id)
           (first (get (registry/currency-code->currencies registry) id)))
         (throw (ex-info
                 (str "Currency " (core-symbol id) " not found in a registry.")
                 {:registry registry})))))

  (id
    (^clojure.lang.Keyword [c]
     (if-let [r registry/*default*]
       (.id ^Currency (unit ^clojure.lang.Keyword c ^Registry r))
       (if (namespace c) c
           (if-some [cur (first (get (registry/currency-code->currencies) c))]
             (.id ^Currency cur) c))))
    (^clojure.lang.Keyword [c ^Registry registry]
     (if (nil? registry) c
         (.id ^Currency (unit ^clojure.lang.Keyword c ^Registry registry)))))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency) id))
    (^Boolean [id ^Registry registry]
     (contains? (registry/currency-id->currency registry) id)))

  (present?
    (^Boolean [id]
     (if (namespace id)
       (contains? (registry/currency-id->currency) id)
       (contains? (registry/currency-code->currencies) id)))
    (^Boolean [id ^Registry registry]
     (if (namespace id)
       (contains? (registry/currency-id->currency registry) id)
       (contains? (registry/currency-code->currencies registry) id))))

  (same-ids?
    (^Boolean [a b]
     (if-let [r registry/*default*]
       (= (id a r) (id b r))
       (= (id a nil) (id b nil))))
    (^Boolean [a b ^Registry registry]
     (= (id a registry) (id b registry))))

  String

  (of-id
    (^Currency [id] (of-id (keyword id)))
    (^Currency [id, ^Registry registry] (of-id (keyword id) registry)))

  (unit
    (^Currency [id] (unit (keyword id)))
    (^Currency [id ^Registry registry] (unit (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [c] (id (keyword c)))
    (^clojure.lang.Keyword [c ^Registry registry] (id (keyword c) registry)))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency) (keyword id)))
    (^Boolean [id ^Registry registry]
     (contains? (registry/currency-id->currency registry) (keyword id))))

  (present?
    (^Boolean [id]
     (present? (keyword id)))
    (^Boolean [id ^Registry registry]
     (present? (keyword id) registry)))

  (same-ids?
    (^Boolean [a b] (= (keyword a) (id b)))
    (^Boolean [a b ^Registry registry] (= (keyword a) (id b registry))))

  clojure.lang.Symbol

  (of-id
    (^Currency [id] (of-id (keyword id)))
    (^Currency [id, ^Registry registry] (of-id (keyword id) registry)))

  (unit
    (^Currency [id] (unit (keyword id)))
    (^Currency [id ^Registry registry] (unit (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [c] (id (keyword c)))
    (^clojure.lang.Keyword [c ^Registry registry] (id (keyword c) registry)))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency) (keyword id)))
    (^Boolean [id, ^Registry registry]
     (contains? (registry/currency-id->currency registry) (keyword id))))

  (present?
    (^Boolean [id]
     (present? (keyword id)))
    (^Boolean [id ^Registry registry]
     (present? (keyword id) registry)))

  (same-ids?
    (^Boolean [a b] (= (keyword a) (id b)))
    (^Boolean [a b ^Registry registry] (= (keyword a) (id b registry))))

  clojure.lang.IPersistentMap

  (of-id
    (^Currency [m] (map->new m))
    (^Currency [m ^Registry registry] (map->new m)))

  (unit
    (^Currency [m] (map->new m))
    (^Currency [m ^Registry registry] (map->new m)))

  (id
    (^clojure.lang.Keyword [m] (id (keyword (:id m))))
    (^clojure.lang.Keyword [m, ^Registry registry] (id (keyword (:id m)) registry)))

  (defined?
    (^Boolean [m]
     (contains? (registry/currency-id->currency) (keyword (:id m))))
    (^Boolean [m ^Registry registry]
     (contains? (registry/currency-id->currency registry) (keyword (:id m)))))

  (present?
    (^Boolean [m]
     (present? (keyword (:id m))))
    (^Boolean [m ^Registry registry]
     (present? (keyword (:id m)) registry)))

  (same-ids?
    (^Boolean [m b] (= (keyword (:id m)) (id b)))
    (^Boolean [m b ^Registry registry] (= (keyword (:id m)) (id b registry))))

  nil

  (of-id
    ([currency] (if-some [d *default*] (of-id d) nil))
    ([currency ^Registry registry] (if-some [d *default*] (of-id d) nil)))

  (unit
    ([currency] (if-some [d *default*] (unit d) nil))
    ([currency ^Registry registry] (if-some [d *default*] (unit d) nil)))

  (id
    (^clojure.lang.Keyword [currency] nil)
    (^clojure.lang.Keyword [currency ^Registry registry] nil))

  (defined?
    (^Boolean [currency] false)
    (^Boolean [currency ^Registry registry] false))

  (present?
    (^Boolean [currency] false)
    (^Boolean [currency ^Registry registry] false))

  (same-ids?
    (^Boolean [a b] false)
    (^Boolean [a b ^Registry registry] false)))

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
             (if (or (contains? env c) (resolve c)) c
                 (keyword c))))))

(defmacro of
  "Returns a currency for the given value by querying the given registry or a global
  registry, which may be shadowed by the value of registry/*default* (see
  registry/with or with-registry)."
  {:added "1.0.0"}
  ([currency]
   (let [cur# (parse-currency-code currency &env)]
     `(unit ~cur#)))
  ([currency registry]
   (let [cur# (parse-currency-code currency &env)]
     `(unit ~cur# ~registry))))

;;
;; Currency properties.
;;

(defn nr
  "Returns currency numeric ID as a long number. For currencies without the assigned
  number it will return nil. Locale argument is ignored."
  {:tag Long :added "1.0.0"}
  (^Long [c]
   (let [n (.numeric ^Currency (unit c))]
     (when-not (= n no-numeric-id) n)))
  (^Long [c ^Registry registry]
   (let [n (.numeric ^Currency (unit c registry))]
     (when-not (= n no-numeric-id) n)))
  (^Long [c ^Registry locale registry]
   (let [n (.numeric ^Currency (unit c registry))]
     (when-not (= n no-numeric-id) n))))

(def ^{:tag Long
       :arglists '(^Long [c]
                   ^Long [c ^Registry registry]
                   ^Long [c locale ^Registry registry])}
  numeric-id
  "Alias for nr."
  nr)

(def ^{:tag Long
       :arglists '(^Long [c]
                   ^Long [c ^Registry registry]
                   ^Long [c locale ^Registry registry])}
  numeric
  "Alias for nr."
  nr)

(defn sc
  "Returns currency scale (decimal places) as an integer number. For currencies without
  the assigned decimal places it will return nil (the value of auto-scaled). Locale
  argument is ignored."
  {:tag Integer :added "1.0.0"}
  (^Integer [c]
   (let [sc (.scale ^Currency (unit c))]
     (when-not (= sc auto-scaled) sc)))
  (^Integer [c ^Registry registry]
   (let [sc (.scale ^Currency (unit c registry))]
     (when-not (= sc auto-scaled) sc)))
  (^Integer [c ^Registry locale registry]
   (let [sc (.scale ^Currency (unit c registry))]
     (when-not (= sc auto-scaled) sc))))

(def ^{:tag Integer
       :arglists '(^Integer [c]
                   ^Integer [c ^Registry registry]
                   ^Integer [c locale ^Registry registry])}
  scale
  "Alias for sc."
  sc)

(defn domain
  "Returns currency domain as a keyword. For currencies with simple identifiers it will
  be :ISO-4217. For currencies with namespace-qualified identifiers it will be the
  upper-cased namespace name (e.g. :CRYPTO) set during creation a currency
  object. Locale argument is ignored."
  {:tag clojure.lang.Keyword :added "1.0.0"}
  (^clojure.lang.Keyword [c] (.domain ^Currency (unit c)))
  (^clojure.lang.Keyword [c ^Registry registry] (.domain ^Currency (unit c registry)))
  (^clojure.lang.Keyword [c locale ^Registry registry] (.domain ^Currency (unit c registry))))

(def ^{:tag clojure.lang.Keyword
       :arglists '(^clojure.lang.Keyword [c]
                   ^clojure.lang.Keyword [c, ^Registry registry]
                   ^clojure.lang.Keyword [c, locale ^Registry registry])}
  ns
  "Alias for domain."
  domain)

(defn kind
  "Returns currency kind. It is a keyword which describes origin of its value. Currently
  known kinds are:

  - :FIAT          – legal tender issued by government or other authority
  - :FIDUCIARY     - accepted medium of exchange issued by a fiduciary or fiduciaries
  - :DECENTRALIZED - accepted medium of exchange issued by a distributed ledger
  - :COMBANK       - commercial bank money
  - :COMMODITY     - accepted medium of exchange based on commodities
  - :EXPERIMENTAL  - pseudo-currency used for testing purposes.

  The function may return nil if the currency is a no-currency. Locale argument is
  ignored."
  {:tag clojure.lang.Keyword :added "1.0.0"}
  (^clojure.lang.Keyword [c] (.kind ^Currency (unit c)))
  (^clojure.lang.Keyword [c ^Registry registry] (.kind ^Currency (unit c registry)))
  (^clojure.lang.Keyword [c locale ^Registry registry] (.kind ^Currency (unit c registry))))

(defn ns-code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced the namespace will be used as a prefix and slash character
  as a separator. Locale argument is ignored."
  {:tag String :added "1.0.0"}
  (^String [c] (subs (str (id c)) 1))
  (^String [c ^Registry registry] (subs (str (id c registry)) 1))
  (^String [c locale ^Registry registry] (subs (str (id c registry)) 1)))

(defn code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced only the base code (without a namespace) will be
  returned. Locale argument is ignored."
  {:tag String :added "1.0.0"}
  (^String [c] (core-name (id c)))
  (^String [c ^Registry registry] (core-name (id c registry)))
  (^String [c locale ^Registry registry] (core-name (id c registry))))

(defn weight
  "Returns weight of the given currency (used to resolve conflicts when getting
  currencies having conflicting currency codes)."
  {:tag 'int :added "1.0.2"}
  ([c] (int (.weight ^Currency (unit c))))
  ([c ^Registry registry] (int (.weight ^Currency (unit c registry))))
  ([c locale ^Registry registry] (int (.weight ^Currency (unit c registry)))))

;;
;; Currency - country relations.
;;

(defn countries
  "Returns a set of country IDs (keywords) for which the given currency is main
  currency. If there are no countries associated with a currency, returns nil. Locale
  argument is ignored."
  {:tag clojure.lang.PersistentHashSet :added "1.0.0"}
  (^clojure.lang.PersistentHashSet [c]
   (countries c (registry/get)))
  (^clojure.lang.PersistentHashSet [c ^Registry registry]
   (get (registry/currency-id->country-ids registry) (id c)))
  (^clojure.lang.PersistentHashSet [c locale ^Registry registry]
   (get (registry/currency-id->country-ids registry) (id c))))

(defn of-country
  "Returns a currency for the given country identified by a country ID (which should be
  a keyword). If there is no currency or country of the given ID does not exist,
  returns nil. Locale argument is ignored."
  {:tag Currency :added "1.0.0"}
  (^Currency [^clojure.lang.Keyword country-id]
   (of-country country-id (registry/get)))
  (^Currency [^clojure.lang.Keyword country-id ^Registry registry]
   (get (registry/country-id->currency registry) (keyword country-id)))
  (^Currency [^clojure.lang.Keyword country-id locale ^Registry registry]
   (get (registry/country-id->currency registry) (keyword country-id))))

;;
;; Parsing helpers.
;;

(defn prep-currency
  "Prepares currency attributes which may come from an external data source. Returns a
  currency."
  {:tag Currency :added "1.0.0" :private true}
  (^Currency [[id {:keys [numeric kind scale domain weight]}]]
   (prep-currency id numeric kind scale domain weight))
  (^Currency [id {:keys [numeric kind scale domain weight]}]
   (prep-currency id numeric kind scale domain weight))
  (^Currency [id numeric kind scale]
   (prep-currency id numeric kind scale nil 0))
  (^Currency [id numeric kind scale domain weight]
   (when (some? id)
     (let [numeric (if (number? numeric) numeric (or (try-parse-long numeric) no-numeric-id))
           numeric (if (< numeric 1) no-numeric-id numeric)
           scale   (if (number? scale) scale (or (try-parse-int scale) auto-scaled))
           scale   (if (< scale 0) auto-scaled scale)
           kind    (when (some? kind) (keyword kind))
           weight  (if (number? weight) weight (try-parse-int weight))]
       (new-currency (keyword id) (long numeric) (int scale) kind domain weight)))))

(defn prep-currencies
  "Prepares a map of currency ID to currency based on a configuration map of currency
  ID to currency attributes."
  {:tag clojure.lang.IPersistentMap :added "1.0.0" :private true}
  [^clojure.lang.IPersistentMap m]
  (map prep-currency m))

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
   #(vector (let [k (keyword %1)] (if (= :* k) k (l/locale k)))
            (map/map-vals str %2)) p))

(defn prep-all-localized-props
  "Prepares localized properties map for all currencies in a map."
  {:tag clojure.lang.IPersistentMap :added "1.0.0" :private true}
  [^clojure.lang.IPersistentMap p]
  (map/map-vals prep-localized-props p))

(defn weighted-currencies
  "Constructor for weighted currencies entry in :cur-code->currencies database of a
  registry."
  {:tag clojure.lang.PersistentTreeSet :private true :added "1.0.2"}
  [^clojure.lang.PersistentTreeSet s]
  (or s (sorted-set-by
         (fn [^Currency a ^Currency b] (compare (:weight b) (:weight a))))))

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
    (let [ctr-to-cur   (registry/country-id->currency registry)
          cid-to-ctrs  (registry/currency-id->country-ids registry)
          currency-ids (map #(.id ^Currency %) (distinct (filter identity (map ctr-to-cur country-ids))))
          new-cid-ctr  (reduce #(apply core-update %1 %2 disj country-ids) cid-to-ctrs currency-ids)]
      (-> registry
          (assoc :cur-id->ctr-ids (map/remove-empty-values new-cid-ctr currency-ids))
          (assoc :ctr-id->cur     (apply dissoc ctr-to-cur country-ids))))))

(defn remove-currency-from-set
  "Removed currency object from a set and returns nil if the set is empty after
  this operation."
  {:tag clojure.lang.PersistentTreeSet :private true :added "1.0.2"}
  [^clojure.lang.PersistentTreeSet s ^Currency cur]
  (when s
    (if cur
      (let [r (disj s cur)] (if (zero? (count r)) nil r))
      s)))

(defn remove-weighted-currency
  "Removes currency object from a sorted set associated with a currency code keyword in
  a map."
  {:tag clojure.lang.PersistentHashMap :private true :added "1.0.2"}
  [^clojure.lang.PersistentHashMap m cur-code ^Currency cur]
  (if-not (contains? m cur-code) m
          (if-some [ncurs (remove-currency-from-set (get m cur-code) cur)]
            (assoc  m cur-code ncurs)
            (dissoc m cur-code))))

(defn unregister
  "Removes a currency from the given registry. Also removes country and numeric ID
  constrains when necessary and all localized properties associated with a
  currency. Returns updated registry.

  Please note that removal of a currency whose identifier and numeric identifier are
  the same as the currencies which are already registered, will not only remove the
  existing currency identified by the ID but also remove numeric ID from within
  all currency objects present in a registry."
  {:tag Registry :added "1.0.0"}
  [^Registry registry currency]
  (when registry
    (let [^Currency cur       (if (instance? Currency currency) currency (of-id currency registry))
          id                  (.id ^Currency cur)
          cur-code          (if (namespace id) (keyword (core-name id)) id)
          proposed-nr         (.numeric ^Currency cur)
          proposed-nr         (when (not= proposed-nr no-numeric-id) proposed-nr)
          registered-id       id
          registered-cur      (get (registry/currency-id->currency registry) registered-id)
          registered-nr       (when registered-cur (.numeric ^Currency registered-cur))
          registered-nr       (when (and registered-nr (not= registered-nr no-numeric-id)) registered-nr)
          registered-by-nr    (when proposed-nr (get (registry/currency-nr->currency registry) (long proposed-nr)))
          registered-by-nr-id (when registered-by-nr (.id ^Currency registered-by-nr))
          new-by-nr           (when (and registered-by-nr-id
                                         (or (not= registered-by-nr-id registered-id)
                                             (not= registered-by-nr registered-nr)))
                                (assoc registered-by-nr :numeric (long no-numeric-id)))
          country-ids         (get (registry/currency-id->country-ids registry) registered-id)
          ^Registry registry  (if-not new-by-nr
                                registry
                                (-> registry
                                    (assoc-in [:cur-id->cur registered-by-nr-id] new-by-nr)
                                    (assoc-in [:ctr-id->cur registered-by-nr-id] new-by-nr)
                                    (map/dissoc-in [:cur-nr->cur proposed-nr])))
          ^Registry registry  (-> registry
                                  (map/dissoc-in [:cur-id->cur registered-id])
                                  (map/dissoc-in [:cur-nr->cur registered-nr])
                                  (map/dissoc-in [:cur-id->localized registered-id])
                                  (core-update :cur-code->curs remove-weighted-currency cur-code registered-cur))]
      (if-not (contains? (registry/currency-id->country-ids registry) registered-id)
        registry
        (as-> registry regi
          (map/dissoc-in regi [:cur-id->ctr-ids registered-id])
          (apply core-update regi :ctr-id->cur dissoc country-ids))))))

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
  [^Registry registry ^Currency currency country-ids]
  (when (some? registry)
    (when-not (defined? currency registry)
      (throw
       (ex-info (str "Currency "
                     (if (instance? Currency currency) (.id ^Currency currency) currency)
                     " does not exist in a registry.") {:currency currency})))
    (let [^Currency c (if (instance? Currency currency) currency (of-id currency registry))
          cid         (.id ^Currency c)
          ^Currency p (get (registry/currency-id->currency registry) cid)
          cids        (prep-country-ids country-ids)]
      (when-not (= c p)
        (throw
         (ex-info (str "Currency " cid " differs from the currency existing in a registry.")
                  {:currency c, :existing-currency p})))
      (if (nil? (seq cids)) registry
          (as-> registry regi
            (remove-countries-core regi cids)
            (apply update-in regi [:cur-id->ctr-ids cid] (fnil conj #{}) (set cids))
            (core-update regi :ctr-id->cur (partial apply assoc) (interleave cids (repeat c))))))))

(defn remove-localized-properties
  "Removes localized properties assigned to a currency. Returns updated registry."
  {:tag Registry :added "1.0.0"}
  [^Registry registry ^Currency currency]
  (when registry
    (if (nil? currency)
      registry
      (map/dissoc-in registry [:cur-id->localized (.id ^Currency currency)]))))

(defn add-localized-properties
  "Adds localized properties of a currency to the given registry. Overwrites existing
  properties."
  {:tag Registry :added "1.0.0"}
  [^Registry registry ^Currency currency properties]
  (when (some? registry)
    (when-not (defined? currency registry)
      (throw
       (ex-info (str "Currency "
                     (if (instance? Currency currency) (.id ^Currency currency) currency)
                     " does not exist in a registry.") {:currency currency})))
    (let [^Currency c (if (instance? Currency currency) currency (of-id currency registry))
          cid         (.id ^Currency c)
          ^Currency p (get (registry/currency-id->currency registry) cid)]
      (when-not (= c p)
        (throw
         (ex-info (str "Currency " cid " differs from the currency existing in a registry.")
                  {:currency c, :existing-currency p})))
      (if (and (map? properties) (pos? (count properties)))
        (assoc-in registry [:cur-id->localized cid] (prep-localized-props properties))
        registry))))

(defn add-weighted-currency
  "Adds currency code to the given registry using .weight field of a currency. Currency
  must exist in a cur-id->cur database of the registry as it will be the source
  object when adding to cur-code->curs database. The registry will not be updated if
  the given weight is lower than the existing. If it is the same, exception will be
  thrown."
  {:tag Registry :added "1.0.2"}
  [^Registry registry ^Currency currency]
  (when (some? registry)
    (when-not (defined? currency registry)
      (throw
       (ex-info (str "Currency "
                     (if (instance? Currency currency) (.id ^Currency currency) currency)
                     " does not exist in a registry.") {:currency currency})))
    (let [^Currency c (if (instance? Currency currency) currency (of-id currency registry))
          cid         (.id ^Currency c)
          ^Currency p (of-id cid registry)
          p-weight    (int (.weight ^Currency p))
          kw-code     (if (simple-keyword? cid) cid (keyword (core-name cid)))
          currencies  (get (registry/currency-code->currencies registry) kw-code)
          same-code   (first (drop-while #(not= p-weight (.weight ^Currency %)) currencies))]
      (when same-code
        (throw (ex-info "Currency code with the same weight already exists."
                        {:existing-currency same-code :currency c :registry registry})))
      (update-in registry [:cur-code->curs kw-code] #(conj (weighted-currencies %) p)))))

(defn register
  "Adds a currency and optional, associated country mappings and/or localized
  properties to the given registry. Returns updated registry.

  The optional country-ids argument should be a sequence of keywords (however, if a
  single keyword is given it will be converted to a single-element sequence) with
  country codes which should be associated with the given currency.

  The optional localized-properties argument should be a map of localized properties
  of the given currency. See the Data Structures documentation section for more
  information.

  If the update mode is enabled then all of the current countries associated with the
  currency are removed and replaced with the provided ones. To simply add new
  countries, use add-countries. Also note that update mode removed localized
  properties so new one must be provided."
  {:tag Registry :added "1.0.0"
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
           cnr         (.numeric ^Currency c)
           cid-to-cur  (registry/currency-id->currency registry)
           cnr-to-cur  (registry/currency-nr->currency registry)]
       (when-not update?
         (when-some [^Currency p (get cid-to-cur cid)]
           (throw (ex-info
                   (str "Currency " cid " already exists in a registry.")
                   {:currency c, :existing-currency p})))
         (when-some [^Currency p (get cnr-to-cur cnr)]
           (throw (ex-info
                   (str "Currency with numeric ID of " cnr " already exists in a registry.")
                   {:currency c, :existing-currency p}))))
       (let [registry    (unregister registry c)
             cid-to-cur  (registry/currency-id->currency registry)
             registry    (assoc registry :cur-id->cur (assoc cid-to-cur cid c))
             numeric-id  (.numeric ^Currency c)
             cnr-to-cur  (registry/currency-nr->currency ^Registry registry)
             registry    (if (or (nil? numeric-id) (= numeric-id no-numeric-id) (<= numeric-id 0)) registry
                             (assoc registry :cur-nr->cur (assoc cnr-to-cur (long numeric-id) c)))]
         (-> registry
             (add-weighted-currency    currency)
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
   (let [present (if (instance? Currency currency) currency (unit currency registry))]
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
  "Removes currency from the global registry. Automatically removes country constrains
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
  global registry. Automatically removes currency constrains when necessary. Returns
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
           curs (prep-currencies          (config/currencies cfg))
           ctrs (prep-cur->ctr            (config/countries  cfg))
           lpro                           (config/localized  cfg)
           vers (str                      (config/version    cfg))
           regi (if (nil? vers) regi (assoc regi :version vers))]
       (reduce (fn ^Registry [^Registry r ^Currency c]
                 (let [cid (.id ^Currency c)]
                   (register r c (get ctrs cid) (get lpro cid) false)))
               regi curs))
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
  "Sets default currency by altering `*default*` dynamic variable."
  {:tag Currency :added "1.0.0"}
  [c]
  (alter-var-root #'*default* (constantly ^Currency (unit c))))

(defn unset-default!
  "Sets default currency to nil by altering `*default*` dynamic variable."
  {:tag nil :added "1.0.0"}
  []
  (alter-var-root #'*default* (constantly nil)))

;;
;; Predicates.
;;

(defn currency?
  "Returns true if the given value is represented by a valid currency object. Registry
  argument is ignored."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (and (instance? Currency c) (keyword? (.id ^Currency c))))
  (^Boolean [c ^Registry registry] (and (instance? Currency c) (keyword? (.id ^Currency c)))))

(defn possible?
  "Returns true if the given value is a possible currency representation. If the
  registry is not given, the global one is used. By possible representation we mean
  that it is a currency with a keyword identifier or any other data type which can be
  successfully converted into such using the registry provided."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (let [r (registry/get)] (or (currency? c r) (defined? c r))))
  (^Boolean [c ^Registry registry] (or (currency? c registry) (defined? c registry))))

(defn has-numeric-id?
  "Returns true if the given currency has a numeric ID."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (not= no-numeric-id (.numeric ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (not= no-numeric-id (.numeric ^Currency (unit c registry)))))

(defn has-country?
  "Returns true if the given currency has at least one country for which it is an
  official currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c]
   (has-country? c (registry/get)))
  (^Boolean [c ^Registry registry]
   (contains? (registry/currency-id->country-ids registry) (id c))))

(defn in-domain?
  "Returns true if the given currency has a domain set to the first given
  argument."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [ns c] (= ns (.domain ^Currency (unit c))))
  (^Boolean [ns c ^Registry registry] (= ns (.domain ^Currency (unit c registry)))))

(defn big?
  "Returns true if the given currency has an automatic scale (decimal places)."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (val-auto-scaled? (.scale ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (val-auto-scaled? (.scale ^Currency (unit c registry)))))

(def ^{:tag Boolean
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  auto-scaled?
  "Alias for big?."
  big?)

(defn crypto?
  "Returns true if the given currency is a cryptocurrency. It is just a helper that
  check if the domain of a currency equals to :CRYPTO."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :CRYPTO (.domain ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :CRYPTO (.domain ^Currency (unit c registry)))))

(defn iso?
  "Returns true if the given currency is an official currency and its identifier is
  compliant with ISO standard. It is just a helper which checks if the :domain field of
  a currency equals :ISO-4217."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :ISO-4217 (.domain ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :ISO-4217 (.domain ^Currency (unit c registry)))))

(def ^{:tag Boolean
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  official?
  "Alias for iso?"
  iso?)

(def ^{:tag Boolean
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  standard?
  "Alias for iso?"
  iso?)

(defn has-kind?
  "Returns true if the given currency has its kind defined."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (some? (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (some? (.kind ^Currency (unit c)))))

(defn kind-of?
  "Returns a kind of the given currency equals to the one given as a second argument."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c ^clojure.lang.Keyword kind] (= kind (.kind ^Currency (unit c))))
  (^Boolean [c ^clojure.lang.Keyword kind ^Registry registry] (= kind (.kind ^Currency (unit c)))))

(defn fiat?
  "Returns true if the given currency is a kind of :FIAT."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :FIAT (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :FIAT (.kind ^Currency (unit c)))))

(defn fiduciary?
  "Returns true if the given currency is a kind of :FIDUCIARY."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :FIDUCIARY (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :FIDUCIARY (.kind ^Currency (unit c)))))

(defn combank?
  "Returns true if the given currency is a kind of :COMBANK."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :COMBANK (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :COMBANK (.kind ^Currency (unit c)))))

(defn commodity?
  "Returns true if the given currency is a kind of :COMMODITY."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :COMMODITY (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :COMMODITY (.kind ^Currency (unit c)))))

(defn decentralized?
  "Returns true if the given currency is a kind of :DECENTRALIZED."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :DECENTRALIZED (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :DECENTRALIZED (.kind ^Currency (unit c)))))

(def ^{:tag Boolean
       :arglists '([c] [c ^Registry registry])}
  decentralised?
  "Alias for decentralized?"
  decentralized?)

(defn experimental?
  "Returns true if the given currency is a kind of :EXPERIMENTAL."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :EXPERIMENTAL (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :EXPERIMENTAL (.kind ^Currency (unit c)))))

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
  translated back to their keyword representations."
  {:tag clojure.lang.PersistentHashMap :added "1.0.8"}
  ([c]
   (map/map-keys
    (comp keyword str l/locale)
    (get (registry/currency-id->localized ) (id c))))
  ([c ^Registry registry]
   (map/map-keys
    (comp keyword str l/locale)
    (get (registry/currency-id->localized registry) (id c registry)))))

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
   (let [cid    (id currency-id)
         locale (l/locale locale)]
     (when-some [m (get (registry/currency-id->localized registry) cid)]
       (or (get (get m locale) property)
           (some #(and (some? %)
                       (not (contains? locale-seps
                                       (.charAt ^String %
                                                (unchecked-dec (count %)))))
                       (get (get m (l/locale %)) property))
                 (sm/all-prefixes locale-seps (str locale)))
           (get (get m :*) property))))))

(def ^{:tag String :added "1.0.0"
       :arglists '(^String [currency]
                   ^String [currency locale]
                   ^String [currency locale ^Registry registry])}
  symbol
  "Returns a currency symbol as a string for the given currency object and locale. If
  the locale is not given, a default one is used. Uses global registry if a registry
  is not given.

  The following tactic is applied:

  - The registry field .cur-id->localized is looked up for the currency ID key. If
  it's found then a key with the given locale object (a kind of java.util.Locale) is
  obtained. If there is no such key, the default one :* is tried (a keyword). The
  resulting value should be a map of localized properties in which an entry under the
  key :symbol should exist. Its value will be returned, if found.

  - If the above method failed and the given currency is ISO-standardized then Java
  methods will be tried to obtain it.

  A locale can be expressed as java.util.Locale object or any other object (like a
  string or a keyword) which can be used to look up the locale."
  (memoize
   (fn symbol
     (^String [c]        (symbol c (Locale/getDefault) (registry/get)))
     (^String [c locale] (symbol c locale (registry/get)))
     (^String [c locale ^Registry registry]
      (let [lc (l/locale locale)
            lp (localized-property :symbol c lc registry)]
        (if (some? lp)
          lp
          (let [scode (code c)]
            (or (when (iso? c)
                  (try (-> scode
                           ^java.util.Currency (java.util.Currency/getInstance)
                           (.getSymbol lc))
                       (catch IllegalArgumentException e nil)))
                scode))))))))

(defn symbol-native
  "Like symbol but for ISO-standardized currencies uses locale assigned to the first
  country where a currency is the default. When locale is given it is ignored."
  {:tag String :added "1.0.0"
   :arglists '(^String [currency]
               ^String [currency ^Registry registry])}
  ([c]
   (symbol c (map/lazy-get nat-helper (id c) (first (countries c)))))
  ([c registry]
   (symbol c (map/lazy-get nat-helper (id c) (first (countries c))) registry))
  ([c locale registry]
   (symbol c (map/lazy-get nat-helper (id c) (first (countries c))) registry)))

(def ^{:tag String :added "1.0.0"
       :arglists '(^String [currency]
                   ^String [currency locale]
                   ^String [currency locale ^Registry registry])}
  display-name
  "Returns a currency display name as a string for the given currency object and
  locale. If the locale is not given, a default one is used. Uses global registry if
  a registry is not given.

  The following tactic is applied:

  - The registry field .cur-id->localized is looked up for the currency ID key. If
  it's found then a key with the given locale object (a kind of java.util.Locale) is
  obtained. If there is no such key, the default one :* is tried (a keyword). The
  resulting value should be a map of localized properties in which an entry under the
  key :symbol should exist. Its value will be returned, if found.

  - If the above method failed and the given currency is ISO-standardized then Java
  methods will be tried to obtain it.

  - If the above method failed a currency code will be returned.

  A locale can be expressed as java.util.Locale object or any other object (like a
  string or a keyword) which can be used to look up the locale."
  (memoize
   (fn display-name
     (^String [c]        (display-name c (Locale/getDefault) (registry/get)))
     (^String [c locale] (display-name c locale (registry/get)))
     (^String [c locale ^Registry registry]
      (let [lc (l/locale locale)
            lp (localized-property :name c lc registry)]
        (if (some? lp) lp
            (let [scode (code c)]
              (or (when (iso? c)
                    (try (-> scode
                             ^java.util.Currency (java.util.Currency/getInstance)
                             (.getDisplayName lc))
                         (catch IllegalArgumentException e nil)))
                  scode))))))))

(defn display-name-native
  "Like display-name but for ISO-standardized currencies uses locale assigned to the
  first country where a currency is the default. When locale is given it is ignored."
  {:tag String :added "1.0.0"
   :arglists '(^String [currency]
               ^String [currency ^Registry registry])}
  ([c]
   (display-name c (map/lazy-get nat-helper (id c) (first (countries c)))))
  ([c registry]
   (display-name c (map/lazy-get nat-helper (id c registry) (first (countries c)))
                 registry))
  ([c locale registry]
   (display-name c (map/lazy-get nat-helper (id c registry) (first (countries c)))
                 registry)))

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
;; Converting to Java object.
;;

(defn java
  "For ISO-standardized currency, returns corresponding java.util.Currency object."
  {:tag java.util.Currency :added "1.0.0"}
  (^java.util.Currency [currency] (java.util.Currency/getInstance (code currency)))
  (^java.util.Currency [currency ^Registry registry] (java.util.Currency/getInstance (code currency registry))))

;;
;; Scalable implementation.
;;

(extend-protocol scale/Scalable

  Currency

  (^Boolean scalable? [c] true)
  (^Boolean applied?  [c] true)

  (of [c] (.scale ^Currency c))

  (^Currency apply
   (^Currency [c] ^Currency c)
   (^Currency [c scale] (assoc c :scale (int scale)))
   (^Currency [c scale ^RoundingMode rounding-mode] (assoc c :scale (int scale))))

  clojure.lang.Keyword

  (^Boolean scalable? [c] (defined? c))
  (^Boolean applied?  [c] (defined? c))

  (of [c] (.scale ^Currency (unit c)))

  (^Currency apply
   (^Currency [c] ^Currency (unit c))
   (^Currency [c scale] (assoc (unit c) :scale (int scale)))
   (^Currency [c scale ^RoundingMode rounding-mode] (assoc (unit c) :scale (int scale)))))

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
  the same effect as registry/with."
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
  "For the specified locale and currency, returns a vector of mutable instance of a
  currency text-formatter, currency object and locale. If no locale is given, uses
  the default one. If no registry is given, uses dynamic or global registry. Due to
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
      (let [cur (if (nil? registry) (unit currency) (unit currency registry))
            ^Locale locale-native (l/locale locale)
            f     (NumberFormat/getCurrencyInstance ^Locale locale-native)
            iso   (iso? cur registry)
            sc    (.scale ^Currency cur)]
        (.setCurrency              ^DecimalFormat f ^Currency (if iso (java ^Currency cur) iso-ref-currency))
        (.setRoundingMode          ^DecimalFormat f ^RoundingMode scale/ROUND_UNNECESSARY)
        (.setParseIntegerOnly      ^DecimalFormat f false)
        (.setMaximumFractionDigits ^DecimalFormat f (int sc))
        (.setMinimumFractionDigits ^DecimalFormat f (int sc))
        (when-not iso
          (let [syms (.getDecimalFormatSymbols ^DecimalFormat f)]
            (.setCurrencySymbol ^DecimalFormatSymbols syms ^String (symbol ^Currency cur))
            (.setDecimalFormatSymbols ^DecimalFormat f ^DecimalFormatSymbols syms)))
        f)))))

(defn formatter
  "Returns currency formatter as java.text.DecimalFormat instance for the given
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
  "Returns a currency formatter as java.text.DecimalFormat instance for the given
  currency, customizable with the given opts map. If the locale is not given then the
  default one will be used. Due to caching strategy it is advised to express locale
  with a keyword.

  The formatter is a mutable clone of Java data structure.

  In case of currencies other than ISO-standardized (and predefined in Java) the
  currency field of this formatter will be set to the currency of XXX.

  Options map can have the following keys:

  - :rounding-mode   - RoundingMode, rounding mode to apply when scaling
  - :grouping        - Boolean, if true then grouping will be used
  - :grouping-size   - integer, size of a group when grouping
  - :negative-prefix - String, negative prefix to use
  - :negative-suffix - String, negative suffix to use
  - :positive-prefix - String, positive prefix to use
  - :positive-suffix - String, positive suffix to use
  - :always-sep-dec  - Boolean, if true then the decimal separator will always be shown
  - :currency-symbol-fn  - a function used on a bankster/Currency object to get its symbol as a string
  - :min-fraction-digits - integer, the minimum number of digits allowed in the fraction portion of an amount
  - :min-integer-digits  - integer, the minimum number of digits allowed in the integer portion of an amount
  - :max-fraction-digits - integer, the maximum number of digits allowed in the fraction portion of an amount
  - :max-integer-digits  - integer, the maximum number of digits allowed in the integer portion of an amount
  - :scale               - sets both :min-fraction-digits and :max-fraction digits to the same value.

  When choosing different currency, all parameters of a formatter are initially set
  to that currency. Additionally re-scaling may take place for the amount if scales
  are different.

  The function assigned to the :currency-symbol-fn should take 3 arguments:
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
    {:keys [scale rounding-mode grouping grouping-size
            negative-prefix negative-suffix positive-prefix positive-suffix
            always-sep-dec currency-symbol-fn min-fraction-digits max-fraction-digits
            min-integer-digits max-integer-digits] :as opts}
    registry]
   (let [f (.clone ^DecimalFormat (formatter-instance currency locale registry))
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
  (let [sc  (.scale   ^Currency c)
        nr  (.numeric ^Currency c)
        ki  (.kind    ^Currency c)
        dom (.domain  ^Currency c)
        wei (.weight  ^Currency c)]
    (print-simple
     (str "#currency{"
          ":id " (.id ^Currency c)
          (when (some? dom) (str ", :domain "  dom))
          (when (some? ki)  (str ", :kind "    ki))
          (when-not (= nr no-numeric-id)  (str ", :numeric " nr))
          (when-not (val-auto-scaled? sc) (str ", :scale " sc))
          (when-not (zero? wei)           (str ", :weight " wei))
          "}")
     w)))

;;
;; Populating registry with defaults.
;;

(set-default-registry!)
