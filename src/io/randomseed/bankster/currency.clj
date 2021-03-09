(ns io.randomseed.bankster.currency

  ^{:doc    "Bankster library, currency operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [ns new symbol name])

  (:require [trptr.java-wrapper.locale       :as            l]
            [smangler.api                    :as           sm]
            [io.randomseed.bankster          :refer      :all]
            [io.randomseed.bankster.config   :as       config]
            [io.randomseed.bankster.registry :as     registry]
            [io.randomseed.bankster.scale    :as        scale]
            [io.randomseed.bankster.util.map :as          map]
            [io.randomseed.bankster.util     :refer      :all])

  (:import  [io.randomseed.bankster Currency Registry]
            [java.math RoundingMode]
            [java.util Locale]))

;;
;; Constants.
;;

(def ^{:tag 'long :const true :added "1.0.0"}
  no-numeric-id
  "Expresses the value of currency's numeric ID that does not exist."
  (long -1))

(def ^{:tag 'int  :const true :added "1.0.0"}
  auto-scaled
  "Expresses the scale of currency that is automatic and not limited to certain
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
     (when (some? id)
       (Currency. (keyword id) (long no-numeric-id) (int auto-scaled)
                  (keyword (or (try-upper-case (namespace id)) :ISO-4217))
                  nil))))
  (^Currency [id numeric-id]
   (when (some? id)
     (Currency. (keyword id) (long numeric-id) (int auto-scaled)
                (keyword (or (try-upper-case (namespace id)) :ISO-4217))
                nil)))
  (^Currency [id numeric-id scale]
   (when (some? id)
     (Currency. (keyword id) (long numeric-id) (int scale)
                (keyword (or (try-upper-case (namespace id)) :ISO-4217))
                nil)))
  (^Currency [id numeric-id scale kind]
   (when (some? id)
     (Currency. (keyword id) (long numeric-id) (int scale)
                (keyword (or (try-upper-case (namespace id)) :ISO-4217))
                (keyword kind)))))

(defn map->new
  "Creates new currency record from a map."
  {:added "1.0.0" :tag Currency}
  [^clojure.lang.IPersistentMap m]
  (when (and (some? m) (> (count m) 0))
    (let [id     (:id m)
          nr     (or (:nr m) (:numeric m) no-numeric-id)
          sc     (or (:sc m) (:scale   m) auto-scaled)
          kind   (or (:ki m) (:kind    m))
          domain (or (:do m) (:domain  m))]
      (when-some [c ^Currency
                  (if (some? domain)
                    (new-currency id nr sc kind domain)
                    (new-currency id nr sc kind))]
        (if (> (count m) 1)
          (merge c (dissoc m :id :nr :numeric :sc :scale :ki :kind :do :domain))
          c)))))

(def ^{:tag Currency
       :arglists '(^Currency [id]
                   ^Currency [id numeric-id]
                   ^Currency [id numeric-id scale]
                   ^Currency [id numeric-id scale kind])}
  new
  "Alias for new-currency."
  new-currency)

;;
;; Monetary protocol.
;;

(defprotocol ^{:added "1.0.0"} Monetary
  (^{:tag clojure.lang.Keyword :added "1.0.0"}
   id
   [id] [id registry]
   "Returns currency identifier as a keyword. If the registry is not given, it will
  use the global one, but will first try a registry bound to the
  `io.randomseed.bankster.registry/*default*` dynamic variable. If the given argument
  is already an identifier (a keyword), it will be returned as is.")

  (^{:tag Currency :added "1.0.0"}
   unit
   [id] [id registry]
   "Returns a currency object for the given id and registry. If the registry is not
  given, it will use the global one, but will first try a registry bound to the
  `io.randomseed.bankster.registry/*default*` dynamic variable. If the currency
  record is passed, it will be returned as is without consulting the registry.")

  (^{:tag Boolean :added "1.0.0"}
   defined?
   [id] [id registry]
   "Returns true if the given currency exists in a registry. If the registry is not
  given, the global one will be used, trying a registry bound to the
  registry/*default* first.")

  (^{:tag Boolean :added "1.0.0"}
   same-ids?
   [a b] [a b registry]
   "Returns true if two currencies have the same ID. That does not mean the objects
  are of the same contents (e.g. numerical IDs or scales may differ) but it's more
  performant in 99% cases. If the registry is not given, it will use the global one,
  but will first try a registry bound to the
  `io.randomseed.bankster.registry/*default*` dynamic variable."))

;;
;; Currency querying functions, Monetary implementation.
;;

(extend-protocol Monetary

  Currency

  (unit
    (^Currency [currency] currency)
    (^Currency [currency, ^Registry registry] currency))

  (id
    (^clojure.lang.Keyword [currency] (.id ^Currency currency))
    (^clojure.lang.Keyword [currency, ^Registry registry] (.id ^Currency currency)))

  (defined?
    (^Boolean [currency]
     (contains? (registry/currency-id->currency) (.id ^Currency currency)))
    (^Boolean [currency, ^Registry registry]
     (contains? (registry/currency-id->currency registry) (.id ^Currency currency))))

  (same-ids?
    (^Boolean [a b] (= (.id ^Currency a) (id b)))
    (^Boolean [a b ^Registry registry] (= (.id ^Currency a) (id b registry))))

  Number

  (unit
    (^Currency [^clojure.lang.Keyword num]
     (unit num (registry/get)))
    (^Currency [^clojure.lang.Keyword num
                ^Registry registry]
     (or (get (registry/currency-nr->currency registry) num)
         (throw (ex-info
                 (str "Currency with the numeric ID of " num " not found in a registry.")
                 {:registry registry})))))

  (id
    (^clojure.lang.Keyword [num]
     (id (long num) (registry/get)))
    (^clojure.lang.Keyword [num, ^Registry registry]
     (if-some [c (get (registry/currency-nr->currency registry) num)]
       (.id ^Currency c)
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry registry})))))

  (defined?
    (^Boolean [num]
     (contains? (registry/currency-nr->currency) num))
    (^Boolean [num, ^Registry registry]
     (contains? (registry/currency-nr->currency registry) num)))

  (same-ids?
    (^Boolean [a b]
     (let [r (registry/get)]
       (if-some [c (get (registry/currency-nr->currency r) a)]
         (= (.id ^Currency c) (id b r))
         (throw (ex-info
                 (str "Currency with the numeric ID of " num " not found in a registry.")
                 {:registry r})))))
    (^Boolean [a b ^Registry registry]
     (if-some [c (get (registry/currency-nr->currency registry) a)]
       (= (.id ^Currency c) (id b registry))
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry registry})))))

  clojure.lang.Keyword

  (unit
    (^Currency [^clojure.lang.Keyword id]
     (unit id (registry/get)))
    (^Currency [^clojure.lang.Keyword id, ^Registry registry]
     (or (get (registry/currency-id->currency registry) id)
         (throw (ex-info
                 (str "Currency " (clojure.core/symbol id) " not found in a registry.")
                 {:registry registry})))))

  (id
    (^clojure.lang.Keyword [id] id)
    (^clojure.lang.Keyword [id, ^Registry registry] id))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency) id))
    (^Boolean [id, ^Registry registry]
     (contains? (registry/currency-id->currency registry) id)))

  (same-ids?
    (^Boolean [a b] (= a (id b (registry/get))))
    (^Boolean [a b ^Registry registry] (= a (id b registry))))

  String

  (unit
    (^Currency [id] (unit (keyword id)))
    (^Currency [id, ^Registry registry] (unit (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [id] (keyword id))
    (^clojure.lang.Keyword [id, ^Registry registry] (keyword id)))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency) (keyword id)))
    (^Boolean [id, ^Registry registry]
     (contains? (registry/currency-id->currency registry) (keyword id))))

  (same-ids?
    (^Boolean [a b] (= (keyword a) (id b)))
    (^Boolean [a b ^Registry registry] (= (keyword a) (id b registry))))

  clojure.lang.Symbol

  (unit
    (^Currency [id] (unit (keyword id)))
    (^Currency [id, ^Registry registry] (unit (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [id] (keyword id))
    (^clojure.lang.Keyword [id, ^Registry registry] (keyword id)))

  (defined?
    (^Boolean [id]
     (contains? (registry/currency-id->currency) (keyword id)))
    (^Boolean [id, ^Registry registry]
     (contains? (registry/currency-id->currency registry) (keyword id))))

  (same-ids?
    (^Boolean [a b] (= (keyword a) (id b)))
    (^Boolean [a b ^Registry registry] (= (keyword a) (id b registry))))

  clojure.lang.IPersistentMap

  (unit
    (^Currency [m] (map->new m))
    (^Currency [m, ^Registry registry] (map->new m)))

  (id
    (^clojure.lang.Keyword [m] (keyword (:id m)))
    (^clojure.lang.Keyword [m, ^Registry registry] (keyword (:id m))))

  (defined?
    (^Boolean [m]
     (contains? (registry/currency-id->currency) (keyword (:id m))))
    (^Boolean [m, ^Registry registry]
     (contains? (registry/currency-id->currency registry) (keyword (:id m)))))

  (same-ids?
    (^Boolean [m b] (= (keyword (:id m)) (id b)))
    (^Boolean [m b ^Registry registry] (= (keyword (:id m)) (id b registry))))

  nil

  (unit
    ([currency] nil)
    ([currency, ^Registry registry] nil))

  (id
    (^clojure.lang.Keyword [currency] nil)
    (^clojure.lang.Keyword [currency, ^Registry registry] nil))

  (defined?
    (^Boolean [currency] false)
    (^Boolean [currency, ^Registry registry] false))

  (same-ids?
    (^Boolean [a b] false)
    (^Boolean [a b ^Registry registry] false)))


(defn parse-currency-symbol
  "Internal helper that transforms currency codes into keywords."
  {:no-doc true :added "1.0.0"}
  [c]
  (if (and (clojure.core/symbol? c) (defined? c))
    (keyword c) c))

(defmacro of
  "Returns a currency for the given value by querying the given registry or a global
  registry, which may be shadowed by the value of registry/*default* (see
  registry/with or with-registry)."
  {:added "1.0.0"}
  ([currency]
   (let [cur# (parse-currency-symbol currency)]
     `(unit ~cur#)))
  ([currency registry]
   (let [cur# (parse-currency-symbol currency)]
     `(unit ~cur# ~registry))))

;;
;; Currency properties.
;;

(defn nr
  "Returns currency numeric ID as a long number. For currencies without the assigned
  number it will return -1 (or currency/no-numeric-id)."
  {:tag 'long :added "1.0.0"}
  (^long [c] (.numeric ^Currency (unit c)))
  (^long [c ^Registry registry] (.numeric ^Currency (unit c registry))))

(def ^{:tag 'long
       :arglists '([c]
                   [c, ^Registry registry])}
  numeric-id
  "Alias for nr."
  nr)

(defn sc
  "Returns currency scale (decimal places) as an integer number. For currencies without
  the assigned decimal places it will return -1 (the value of auto-scaled)."
  {:tag clojure.lang.Keyword :added "1.0.0"}
  ([c] (.scale ^Currency (unit c)))
  ([c ^Registry registry] (.scale ^Currency (unit c registry))))

(def ^{:tag 'int
       :arglists '(^int [c] ^int [c, ^Registry registry])}
  scale
  "Alias for sc."
  sc)

(defn ^clojure.lang.Keyword domain
  "Returns currency domain as a keyword. For currencies with simple identifiers it will
  be :ISO-4217. For currencies with namespace-qualified identifiers it will be the
  upper-cased namespace name (e.g. :CRYPTO) set during creation a currency object."
  {:tag clojure.lang.Keyword :added "1.0.0"}
  (^clojure.lang.Keyword [c] (.domain ^Currency (unit c)))
  (^clojure.lang.Keyword [c, ^Registry registry] (.domain ^Currency (unit c registry))))

(def ^{:tag clojure.lang.Keyword
       :arglists '(^clojure.lang.Keyword [c]
                   ^clojure.lang.Keyword [c, ^Registry registry])}
  ns
  "Alias for domain."
  domain)

(defn ^clojure.lang.Keyword kind
  "Returns currency kind. It is a keyword that describes origin of its value. Currently
  known kinds are:

  - :FIAT          – legal tender issued by government or other authority
  - :FIDUCIARY     - accepted medium of exchange issued by a fiduciary or fiduciaries
  - :DECENTRALIZED - accepted medium of exchange issued by a distributed ledger
  - :COMBANK       - commercial bank money
  - :COMMODITY     - accepted medium of exchange based on commodities
  - :EXPERIMENTAL  - pseudo-currency used for testing purposes.

  The function may return nil if the currency is a no-currency."
  {:tag clojure.lang.Keyword :added "1.0.0"}
  (^clojure.lang.Keyword [c] (.kind ^Currency (unit c)))
  (^clojure.lang.Keyword [c ^Registry registry] (.kind ^Currency (unit c registry))))

(defn ^String code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced the namespace will be used as a prefix and slash character
  as a separator."
  {:tag String :added "1.0.0"}
  (^String [c] (subs (str (id c)) 1))
  (^String [c ^Registry registry] (subs (str (id c registry)) 1)))

(defn ^String short-code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced only the base code (without a namespace) will be
  returned (which may lead to misinformation if there are two or more currencies with
  the same base ID but different namespaces)."
  {:tag String :added "1.0.0"}
  (^String [c] (clojure.core/name (id c)))
  (^String [c ^Registry registry] (clojure.core/name (id c registry))))

;;
;; Currency - country relations.
;;

(defn countries
  "Returns a set of country IDs (keywords) for which the given currency is main
  currency. If there are no countries associated with a currency, returns nil."
  {:tag clojure.lang.PersistentHashSet :added "1.0.0"}
  (^clojure.lang.PersistentHashSet [c]
   (countries c (registry/get)))
  (^clojure.lang.PersistentHashSet [c ^Registry registry]
   (get (registry/currency-id->country-ids registry) (id c))))

(defn ^Currency of-country
  "Returns a currency for the given country identified by a country ID (which should be
  a keyword). If there is no currency or country of the given ID does not exist,
  returns nil."
  {:tag Currency :added "1.0.0"}
  (^Currency [^clojure.lang.Keyword country-id]
   (of-country country-id (registry/get)))
  (^Currency [^clojure.lang.Keyword country-id, ^Registry registry]
   (get (registry/country-id->currency registry) (keyword country-id))))

;;
;; Parsing helpers.
;;

(defn ^Currency prep-currency
  "Prepares currency attributes which may come from an external data source. Returns a
  currency."
  {:tag Currency :added "1.0.0" :private true}
  (^Currency [[id {:keys [numeric kind scale domain]}]]
   (prep-currency id numeric kind scale domain))
  (^Currency [id {:keys [numeric kind scale domain]}]
   (prep-currency id numeric kind scale domain))
  (^Currency [id numeric kind scale]
   (prep-currency id numeric kind scale nil))
  (^Currency [id numeric kind scale domain]
   (when (some? id)
     (let [numeric (if (number? numeric) numeric (or (try-parse-long numeric) no-numeric-id))
           numeric (if (< numeric 1) no-numeric-id numeric)
           scale   (if (number? scale) scale (or (try-parse-int scale) auto-scaled))
           scale   (if (< scale 0) auto-scaled scale)
           kind    (when (some? kind) (keyword kind))]
       (if (some? domain)
         (new-currency (keyword id) (long numeric) (int scale) kind domain)
         (new-currency (keyword id) (long numeric) (int scale) kind))))))

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

;;
;; Adding and removing to/from registry.
;;

(defn- ^Registry remove-countries-core
  "Removes countries from the given registry. Also unlinks constrained currencies in
  proper locations. Returns updated registry."
  {:tag Registry :added "1.0.0"}
  [^Registry registry, country-ids]
  (if-not (some? (seq country-ids))
    registry
    (let [ctr-to-cur   (registry/country-id->currency registry)
          cid-to-ctrs  (registry/currency-id->country-ids registry)
          currency-ids (map #(.id ^Currency %) (distinct (filter identity (map ctr-to-cur country-ids))))
          new-cid-ctr  (reduce #(apply update %1 %2 disj country-ids) cid-to-ctrs currency-ids)]
      (-> registry
          (assoc :cur-id->ctr-ids (map/remove-empty-values new-cid-ctr currency-ids))
          (assoc :ctr-id->cur     (apply dissoc ctr-to-cur country-ids))))))

(defn ^Registry unregister
  "Removes currency from the given registry. Also removes country and numeric ID
  constrains when necessary and all localized properties associated with a
  currency. Returns updated registry.

  Please note that removal of a currency whose identifier and numeric identifier are
  the same as the currencies which are already registered, will not only remove the
  existing currency identified by the ID but also remove numeric ID from within
  currency objects present."
  {:tag Registry :added "1.0.0"}
  [^Registry registry currency]
  (when registry
    (let [^Currency cur       (unit currency registry)
          proposed-nr         (.numeric ^Currency cur)
          proposed-nr         (when (not= proposed-nr no-numeric-id) proposed-nr)
          registered-id       (.id ^Currency cur)
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
                                  (map/dissoc-in [:cur-nr->cur registered-nr]))]
      (if-not (contains? (registry/currency-id->country-ids registry) registered-id)
        registry
        (as-> registry regi
          (map/dissoc-in regi [:cur-id->localized registered-id])
          (map/dissoc-in regi [:cur-id->ctr-ids   registered-id])
          (apply update regi :ctr-id->cur dissoc country-ids))))))

(defn ^Registry remove-countries
  "Removes countries from the given registry. Also unlinks constrained currencies in
  proper locations. Returns updated registry."
  {:tag Registry :added "1.0.0"}
  [^Registry registry country-ids]
  (when registry
    (if (nil? country-ids)
      registry
      (remove-countries-core registry (prep-country-ids country-ids)))))

(defn ^Registry add-countries
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
    (let [^Currency c (unit currency registry)
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
            (update regi :ctr-id->cur (partial apply assoc) (interleave cids (repeat c))))))))

(defn ^Registry remove-localized-properties
  "Removes localized properties assigned to a currency. Returns updated registry."
  {:tag Registry :added "1.0.0"}
  [^Registry registry ^Currency currency]
  (when registry
    (if (nil? currency)
      registry
      (map/dissoc-in registry [:cur-id->localized (.id ^Currency currency)]))))

(defn ^Registry add-localized-properties
  "Adds localized properties for a currency to the given registry. Overwrites existing
  properties."
  {:tag Registry :added "1.0.0"}
  [^Registry registry ^Currency currency properties]
  (when (some? registry)
    (when-not (defined? currency registry)
      (throw
       (ex-info (str "Currency "
                     (if (instance? Currency currency) (.id ^Currency currency) currency)
                     " does not exist in a registry.") {:currency currency})))
    (let [^Currency c (unit currency registry)
          cid         (.id ^Currency c)
          ^Currency p (get (registry/currency-id->currency registry) cid)]
      (when-not (= c p)
        (throw
         (ex-info (str "Currency " cid " differs from the currency existing in a registry.")
                  {:currency c, :existing-currency p})))
      (if (and (map? properties) (pos? (count properties)))
        (assoc-in registry [:cur-id->localized cid] (prep-localized-props properties))
        registry))))

(defn ^Registry register
  "Adds currency and (optional) countries to the given registry. Returns updated
  registry. If the updating occurs then all of the current countries associated with
  the currency are removed and replaced with the provided ones. To simply add new
  countries, use add-countries."
  {:tag Registry :added "1.0.0"}
  (^Registry [^Registry registry currency]
   (register registry currency nil false))
  (^Registry [^Registry registry
              ^Currency currency
              country-ids-or-update?]
   (if (boolean? country-ids-or-update?)
     (register registry currency nil nil country-ids-or-update?)
     (register registry currency country-ids-or-update? nil false)))
  (^Registry [^Registry registry
              ^Currency currency
              country-ids
              localized-properties
              ^Boolean update?]
   (when (some? registry)
     (let [^Currency c (unit currency registry)
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
             (add-localized-properties currency localized-properties)
             (add-countries            currency country-ids)))))))

(defn ^Registry register!
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

(defn ^Registry unregister!
  "Removes currency from the global registry. Automatically removes country constrains
  when necessary and localized properties associated with a currency. Returns updated
  registry.

  If the currency is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [^Currency currency]
  (if (nil? currency) @registry/R (swap! registry/R unregister currency)))

(defn ^Registry add-countries!
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

(defn ^Registry remove-countries!
  "Removes country (a keyword) or countries (seqable collection of keywords) from the
  global registry. Automatically removes currency constrains when necessary. Returns
  updated registry.

  If the country-ids is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [country-ids]
  (if (nil? country-ids) @registry/R (swap! registry/R remove-countries country-ids)))

(defn ^Registry add-localized-props!
  "Associates the given currency with a map of localized properties.

  If the currency is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [^Currency currency properties]
  (when (nil? currency) @registry/R (swap! registry/R add-localized-properties currency properties)))

(defn ^Registry remove-localized-props!
  "Removes localized properties of the given currency from the global registry.

  If the currency is nil, returns current state of a global registry (but not a
  dynamic registry, even if it is set)."
  {:tag Registry :added "1.0.0"}
  [^Currency currency]
  (if (nil? currency) @registry/R (swap! registry/R remove-localized-properties currency)))

;;
;; Currencies loading.
;;

(defn ^Registry config->registry
  "Loads currencies and countries from an EDN file. First argument should be a function
  used to construct new currency objects and the second should be a string with a
  path to an EDN resource file containing registry data. Returns a registry
  initialized using values from the EDN file.

  If there are 2 arguments the last one should be an existing registry to use instead
  creating an empty one."
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
           lpro (prep-all-localized-props (config/localized  cfg))
           vers (str                      (config/version    cfg))
           regi (if (nil? vers) regi (assoc regi
                                            :version vers
                                            :cur-id->localized lpro))]
       (reduce (fn ^Registry [^Registry r, ^Currency c]
                 (register r c (get ctrs (.id ^Currency c))))
               regi curs))
     regi)))

;;
;; Setting default registry.
;;

(defn ^Registry set-default-registry!
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

(defn ^Boolean currency?
  "Returns true if the given value is represented by a valid currency object."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (and (instance? Currency c) (keyword? (.id ^Currency c))))
  (^Boolean [c ^Registry registry] (and (instance? Currency c) (keyword? (.id ^Currency c)))))

(defn possible?
  "Returns true if the given value is a possible currency representation. If the
  registry is not given, the global one is used. By possible representation we mean
  that it is a currency with a keyword identifier or any other data type that can be
  successfully converted into such using the registry provided."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (keyword? (id c (registry/get))))
  (^Boolean [c, ^Registry registry] (keyword? (id c registry))))

(defn ^Boolean has-numeric-id?
  "Returns true if the given currency has a numeric ID."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (not= no-numeric-id (.numeric ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (not= no-numeric-id (.numeric ^Currency (unit c registry)))))

(defn ^Boolean has-country?
  "Returns true if the given currency has at least one country for which it is an
  official currency."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c]
   (has-country? c (registry/get)))
  (^Boolean [c, ^Registry registry]
   (contains? (registry/currency-id->country-ids registry) (id c))))

(defn ^Boolean in-domain?
  "Returns true if the given currency has a domain set to the first given
  argument."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [ns c] (= ns (.domain ^Currency (unit c))))
  (^Boolean [ns c ^Registry registry] (= ns (.domain ^Currency (unit c registry)))))

(defn ^{:tag Boolean} big?
  "Returns true if the given currency has an automatic scale (decimal places)."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (val-auto-scaled? (.scale ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (val-auto-scaled? (.scale ^Currency (unit c registry)))))

(def ^{:tag Boolean
       :arglists '(^Boolean [c] ^Boolean [c ^Registry registry])}
  auto-scaled?
  "Alias for big?."
  big?)

(defn ^Boolean crypto?
  "Returns true if the given currency is a cryptocurrency. It is just a helper that
  check if the domain of a currency equals to :CRYPTO."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :CRYPTO (.domain ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :CRYPTO (.domain ^Currency (unit c registry)))))

(defn ^Boolean iso?
  "Returns true if the given currency is an official currency and its identifier is
  compliant with ISO standard. It is just a helper that check if the :domain field of
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

(defn ^Boolean has-kind?
  "Returns true if the given currency has its kind defined."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (some? (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (some? (.kind ^Currency (unit c)))))

(defn ^Boolean kind-of?
  "Returns a kind of the given currency equals to the one given as a second argument."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c ^clojure.lang.Keyword kind] (= kind (.kind ^Currency (unit c))))
  (^Boolean [c ^clojure.lang.Keyword kind ^Registry registry] (= kind (.kind ^Currency (unit c)))))

(defn ^Boolean fiat?
  "Returns true if the given currency is a kind of :FIAT."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :FIAT (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :FIAT (.kind ^Currency (unit c)))))

(defn ^Boolean fiduciary?
  "Returns true if the given currency is a kind of :FIDUCIARY."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :FIDUCIARY (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :FIDUCIARY (.kind ^Currency (unit c)))))

(defn ^Boolean combank?
  "Returns true if the given currency is a kind of :COMBANK."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :COMBANK (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :COMBANK (.kind ^Currency (unit c)))))

(defn ^Boolean commodity?
  "Returns true if the given currency is a kind of :COMMODITY."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :COMMODITY (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :COMMODITY (.kind ^Currency (unit c)))))

(defn ^Boolean decentralized?
  "Returns true if the given currency is a kind of :DECENTRALIZED."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (= :DECENTRALIZED (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :DECENTRALIZED (.kind ^Currency (unit c)))))

(def ^{:tag Boolean
       :arglists '([c] [c ^Registry registry])}
  decentralised?
  "Alias for decentralized?"
  decentralized?)

(defn ^Boolean experimental?
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

(defn get-localized-property
  "Returns localized properties of the currency object for the given locale."
  {:private true :tag clojure.lang.IPersistentMap :added "1.0.0"}
  [p ^Locale locale m] (get (get m locale) p))

(defn localized-property
  "Gets the localized property of a currency from the given registry and locale. If the
  registry is not given, the global one will be used. If the locale is not given, the
  default locale for the environment will be used. Locale can be expressed as a
  Locale object or any object that can be used to identify the locale (e.g. a keyword
  or a string).

  Localized properties are maps keyed by currency identifiers, containing another
  maps keyed by locale objects. There is a special key :* that contains default
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
  - 0`th_TH` and `th`.

  The third example renders `ABC` because there is an entry of `:symbol` in the
  default properties map under the key `:*`.

  Please note that functions for getting particular properties may apply additional
  steps to obtain them. For instance, the display-name function will first call the
  localized-property and if it fails it will fall back to Java methods (if the
  currency is ISO-standardized). If that will fail too then it will return a
  short-code of a currency."
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
  string or a keyword) that can be used to look up the locale."
  (memoize
   (fn symbol
     (^String [c]        (symbol c (Locale/getDefault) (registry/get)))
     (^String [c locale] (symbol c locale (registry/get)))
     (^String [c locale ^Registry registry]
      (let [lc (l/locale locale)
            lp (localized-property :symbol c lc registry)]
        (if (some? lp)
          lp
          (let [scode (short-code c)]
            (or (when (iso? c)
                  (try (-> scode
                           ^java.util.Currency (java.util.Currency/getInstance)
                           (.getSymbol lc))
                       (catch IllegalArgumentException e nil)))
                scode))))))))

(defn symbol-native
  "Like symbol but for ISO-standardized currencies uses locale assigned to the first
  country where a currency is the default."
  {:tag String :added "1.0.0"
   :arglists '(^String [currency]
               ^String [currency ^Registry registry])}
  ([c]
   (symbol c (map/lazy-get nat-helper (id c) (first (countries c)))))
  ([c registry]
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

  A locale can be expressed as java.util.Locale object or any other object (like a
  string or a keyword) that can be used to look up the locale."
  (memoize
   (fn display-name
     (^String [c]        (display-name c (Locale/getDefault) (registry/get)))
     (^String [c locale] (display-name c locale (registry/get)))
     (^String [c locale ^Registry registry]
      (let [lc (l/locale locale)
            lp (localized-property :name c lc registry)]
        (if (some? lp) lp
            (let [scode (short-code c)]
              (or (when (iso? c)
                    (try (-> scode
                             ^java.util.Currency (java.util.Currency/getInstance)
                             (.getDisplayName lc))
                         (catch IllegalArgumentException e nil)))
                  scode))))))))

(defn display-name-native
  "Like display-name but for ISO-standardized currencies uses locale assigned to the
  first country where a currency is the default."
  {:tag String :added "1.0.0"
   :arglists '(^String [currency]
               ^String [currency ^Registry registry])}
  ([c]
   (display-name c (map/lazy-get nat-helper (id c) (first (countries c)))))
  ([c registry]
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
  `(binding [registry/*default* ^Registry registry]
     ~@body))

;;
;; Tagged literal handler.
;;

(defn lit
  "Tagged literal handler."
  {:added "1.0.0"}
  [arg]
  (if (or (nil? arg) (and (map? arg) (< (count arg) 1)))
    '(quote nil) (unit arg)))

;;
;; Printing.
;;

(defmethod ^{:added "1.0.0"} print-method Currency
  [c w]
  (let [sc  (.scale   ^Currency c)
        nr  (.numeric ^Currency c)
        ki  (.kind    ^Currency c)
        do  (.domain  ^Currency c)
        nr  (when (and no-numeric-id (not= nr no-numeric-id)) nr)]
    (print-simple
     (str "#currency{"
          ":id " (.id ^Currency c)
          (when do (str ", :domain "  do))
          (when ki (str ", :kind "    ki))
          (when nr (str ", :numeric " nr))
          (when-not (val-auto-scaled? sc)  (str ", :scale " sc))
          "}")
     w)))

;;
;; Populating registry with defaults.
;;

(set-default-registry!)
