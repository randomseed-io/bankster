(ns io.randomseed.bankster.currency

  ^{:doc    "Bankster library, currency operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [ns new])

  (:require [io.randomseed.bankster          :refer      :all]
            [io.randomseed.bankster.config   :as       config]
            [io.randomseed.bankster.registry :as     registry]
            [io.randomseed.bankster.scale    :as        scale]
            [io.randomseed.bankster.util.map :as          map]
            [io.randomseed.bankster.util     :refer      :all])

  (:import  [io.randomseed.bankster Currency Registry]))

;;
;; Constants.
;;

(def ^{:tag 'long, :const true} no-numeric-id (long 0))
(def ^{:tag 'int,  :const true} auto-scaled   (int -1))

;;
;; Default currency.
;;

(def ^:dynamic ^Currency
  *default*
  "Default currency unit to be applied when creating Money objects without the currency
  specified."
  nil)

;;
;; Global registry.
;;

(def ^:private R (registry/global))

;;
;; Auto-scaling predicate.
;;

(defn ^Boolean auto-scaled?
  [scale]
  (= auto-scaled scale))

;;
;; Currency constructor
;;

(defn ^Currency map->new
  "Creates new currency record from a map."
  [^clojure.lang.IPersistentMap map]
  (when-some [c (map->Currency map)]
    (assoc c :ns (keyword (or (namespace (.id ^Currency c)) :ISO-4217)))))

(defn new-currency
  "Creates new currency record from values passed as arguments."
  (^Currency [^clojure.lang.Keyword id]
   (Currency. id no-numeric-id auto-scaled
              (keyword (or (try-upper-case (namespace id)) :ISO-4217))
              nil))
  (^Currency [^clojure.lang.Keyword id, ^long numeric-id]
   (Currency. id (long numeric-id) auto-scaled
              (keyword (or (try-upper-case (namespace id)) :ISO-4217))
              nil))
  (^Currency [^clojure.lang.Keyword id, ^long numeric-id, scale]
   (Currency. id (long numeric-id) (int scale)
              (keyword (or (try-upper-case (namespace id)) :ISO-4217))
              nil))
  (^Currency [^clojure.lang.Keyword id, ^long numeric-id, scale, ^clojure.lang.Keyword kind]
   (Currency. id (long numeric-id) (int scale)
              (keyword (or (try-upper-case (namespace id)) :ISO-4217))
              kind)))

(def ^{:tag Currency
       :arglists '(^Currency [^clojure.lang.Keyword id]
                   ^Currency [^clojure.lang.Keyword id, ^long numeric-id]
                   ^Currency [^clojure.lang.Keyword id, ^long numeric-id, scale]
                   ^Currency [^clojure.lang.Keyword id, ^long numeric-id, scale, ^clojure.lang.Keyword kind])}
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
   "Returns currency identifier as keyword. If the registry is not given it will use
  the default one. If the given argument is already an identifier (a keyword), it
  will be returned as is.")

  (^{:tag Currency :added "1.0.0"}
   unit
   [id] [id registry]
   "Returns a currency object for the given id and registry. If the registry is not
  given it will use the default one. If the currency record is passed, it will be
  returned as is without consulting the registry.")

  (^{:tag Boolean :added "1.0.0"}
   defined?
   [id] [id registry]
   "Returns true if the given currency exists in a registry. If the registry is not given,
  the default one is used.")

  (^{:tag Boolean :added "1.0.0"}
   same-ids?
   [a b] [a b registry]
   "Returns true if two currencies have the same ID. That does not mean the objects
  are of the same contents (e.g. numerical IDs or scales may differ) but it's more
  performant in 99% cases."))

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
     (contains? (.cur-id->cur ^Registry @R) (.id ^Currency currency)))
    (^Boolean [currency, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry) (.id ^Currency currency))))

  (same-ids?
    (^Boolean [a b] (= (.id ^Currency a) (id b)))
    (^Boolean [a b ^Registry registry] (= (.id ^Currency a) (id b registry))))

  Number

  (unit
    (^Currency [^clojure.lang.Keyword num]
     (unit num (or *registry* @R)))
    (^Currency [^clojure.lang.Keyword num
                ^Registry registry]
     (or (get (.cur-nr->cur ^Registry registry) num)
         (throw (ex-info
                 (str "Currency with the numeric ID of " num " not found in a registry.")
                 {:registry registry})))))

  (id
    (^clojure.lang.Keyword [num]
     (id (long num) @R))
    (^clojure.lang.Keyword [num, ^Registry registry]
     (if-some [c (get (.cur-nr->cur ^Registry registry) num)]
       (.id ^Currency c)
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry registry})))))

  (defined?
    (^Boolean [num]
     (contains? (.cur-nr->cur ^Registry @R) num))
    (^Boolean [num, ^Registry registry]
     (contains? (.cur-nr->cur ^Registry registry) num)))

  (same-ids?
    (^Boolean [a b]
     (if-some [c (get (.cur-nr->cur ^Registry @R) a)]
       (= (.id ^Currency c) (id b @R))
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry @R}))))
    (^Boolean [a b ^Registry registry]
     (if-some [c (get (.cur-nr->cur ^Registry registry) a)]
       (= (.id ^Currency c) (id b registry))
       (throw (ex-info
               (str "Currency with the numeric ID of " num " not found in a registry.")
               {:registry registry})))))

  clojure.lang.Keyword

  (unit
    (^Currency [^clojure.lang.Keyword id]
     (unit id (or *registry* @R)))
    (^Currency [^clojure.lang.Keyword id, ^Registry registry]
     (or (get (.cur-id->cur ^Registry registry) id)
         (throw (ex-info
                 (str "Currency " (symbol id) " not found in a registry.")
                 {:registry registry})))))

  (id
    (^clojure.lang.Keyword [id] id)
    (^clojure.lang.Keyword [id, ^Registry registry] id))

  (defined?
    (^Boolean [id]
     (contains? (.cur-id->cur ^Registry @R) id))
    (^Boolean [id, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry) id)))

  (same-ids?
    (^Boolean [a b] (= a (id b @R)))
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
     (contains? (.cur-id->cur ^Registry @R) (keyword id)))
    (^Boolean [id, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry) (keyword id))))

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
     (contains? (.cur-id->cur ^Registry @R) (keyword id)))
    (^Boolean [id, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry) (keyword id))))

  (same-ids?
    (^Boolean [a b] (= (keyword a) (id b)))
    (^Boolean [a b ^Registry registry] (= (keyword a) (id b registry))))

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
  {:no-doc true}
  [c]
  (if (and (symbol? c)
           (if *registry*
             (defined? c *registry*)
             (defined? c)))
    (keyword c)
    c))

(defmacro of
  "Returns Currency object."
  ([currency]
   (let [cur# (parse-currency-symbol currency)]
     `(unit ~cur#)))
  ([currency registry]
   (let [cur# (parse-currency-symbol currency)]
     `(unit ~cur# ~registry))))

;;
;; Currency properties.
;;

(defn ^{:tag 'long} nr
  "Returns currency numeric ID as a long number. For currencies without the assigned
  number it will return 0."
  (^long [c] (.nr ^Currency (unit c)))
  (^long [c ^Registry registry] (.nr ^Currency (unit c registry))))

(def ^{:tag 'long
       :arglists '([c]
                   [c, ^Registry registry])}
  numeric-id
  "Alias for nr."
  nr)

(defn sc
  "Returns currency scale (decimal places) as an integer number. For currencies without
  the assigned decimal places it will return -1 (the value of auto-scaled)."
  ([c] (.sc ^Currency (unit c)))
  ([c ^Registry registry] (.sc ^Currency (unit c registry))))

(def ^{:tag 'int
       :arglists '(^int [c] ^int [c, ^Registry registry])}
  scale
  "Alias for sc."
  sc)

(defn ^clojure.lang.Keyword domain
  "Returns currency domain as a keyword. For currencies with simple identifiers it will
  be :ISO-4217. For currencies with namespace-qualified identifiers it will be the
  upper-cased namespace name (e.g. CRYPTO) set during creation a currency object."
  (^clojure.lang.Keyword [c] (.ns ^Currency (unit c)))
  (^clojure.lang.Keyword [c, ^Registry registry] (.ns ^Currency (unit c registry))))

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
  (^clojure.lang.Keyword [c] (.kind ^Currency (unit c)))
  (^clojure.lang.Keyword [c ^Registry registry] (.kind ^Currency (unit c registry))))

(defn ^String code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced the namespace will be used as a prefix and slash character
  as a separator."
  (^String [c] (subs (str (id c)) 1))
  (^String [c ^Registry registry] (subs (str (id c registry)) 1)))

(defn ^String short-code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced only the base code (without a namespace) will be
  returned (which may lead to misinformation if there are two or more currencies with
  the same base ID but different namespaces)."
  (^String [c] (name (id c)))
  (^String [c ^Registry registry] (name (id c registry))))

;;
;; Currency - country relations.
;;

(defn ^clojure.lang.PersistentHashSet countries
  "Returns a set of country IDs (keywords) for which the given currency is main
  currency. If there are no countries associated with a currency, returns nil."
  (^clojure.lang.PersistentHashSet [c]
   (countries c @R))
  (^clojure.lang.PersistentHashSet [c, ^Registry registry]
   (get (.cur-id->ctr-ids ^Registry registry) ^clojure.lang.Keyword (id c))))

(defn ^Currency of-country
  "Returns a currency for the given country identified by a country ID (which should be
  a keyword). If there is no currency or country of the given ID does not exist,
  returns nil."
  (^Currency [^clojure.lang.Keyword country-id]
   (of-country country-id (or *registry* @R)))
  (^Currency [^clojure.lang.Keyword country-id, ^Registry registry]
   (get (.ctr-id->cur ^Registry registry))))

;;
;; Parsing helpers.
;;

(defn- ^Currency prep-currency
  "Prepares currency attributes which may come from an external data source. Returns a
  currency."
  ([[id {:keys [numeric kind scale]}]]
   (prep-currency id numeric kind scale))
  ([id {:keys [numeric kind scale]}]
   (prep-currency id numeric kind scale))
  ([id numeric kind scale]
   (when (some? id)
     (let [numeric (if (number? numeric) numeric (or (try-parse-long numeric) no-numeric-id))
           numeric (if (< numeric 0) no-numeric-id numeric)
           scale   (if (number? scale) scale (or (try-parse-int scale) auto-scaled))
           scale   (if (< scale 0) auto-scaled scale)
           kind    (when (some? kind) (keyword kind))]
       (new-currency (keyword id) (long numeric) (int scale) kind)))))

(defn- prep-currencies
  "Prepares a map of currency ID to currency based on a configuration map of currency
  ID to currency attributes."
  [m]
  (map prep-currency m))

(defn- prep-cur->ctr
  "Prepares countries map which may come from an external data source. Expects a map of
  country ID to currency ID. Returns a map of currency ID to sets of country IDs."
  [ctr-id->cur-id]
  (->> ctr-id->cur-id
       (map/remove-empty-values)
       (map/map-keys-and-vals #(vector (keyword %1) (keyword %2)))
       (map/invert-in-sets)
       (map/remove-empty-values)))

(defn- prep-country-ids
  "Prepares country identifiers by converting the given object into a sequence of
  keywords."
  [country-ids]
  (when country-ids
    (let [cids (if (sequential? country-ids) country-ids
                   (if (and (seqable? country-ids) (not (string? country-ids)))
                     (seq country-ids)
                     (list country-ids)))
          cids (if (set? cids) cids (distinct cids))]
      (map keyword cids))))

;;
;; Adding and removing to/from registry.
;;

(defn- ^Registry remove-countries-core
  "Removes countries from the given registry. Also unlinks constrained currencies in
  proper locations. Returns updated registry."
  [^Registry registry, country-ids]
  (if-not (some? (seq country-ids))
    registry
    (let [ctr-to-cur   (.ctr-id->cur ^Registry registry)
          cid-to-ctrs  (.cur-id->ctr-ids ^Registry registry)
          currency-ids (map #(.id ^Currency %) (distinct (filter identity (map ctr-to-cur country-ids))))
          new-cid-ctr  (reduce #(apply update %1 %2 disj country-ids) cid-to-ctrs currency-ids)]
      (-> registry
          (assoc :cur-id->ctr-ids (map/remove-empty-values new-cid-ctr currency-ids))
          (assoc :ctr-id->cur (apply dissoc ctr-to-cur country-ids))))))

(defn ^Registry unregister
  "Removes currency from the given registry. Also removes country constrains when
  necessary. Returns updated registry."
  [^Registry registry, currency]
  (let [^Currency c (unit currency registry)
        currency-id (id c)
        country-ids (get (.cur-id->ctr-ids ^Registry registry) currency-id)
        ^Registry registry (-> registry
                               (map/dissoc-in [:cur-id->cur currency-id])
                               (map/dissoc-in [:cur-nr->cur (nr c)]))]
    (if-not (contains? (.cur-id->ctr-ids ^Registry registry) currency-id) registry
            (as-> registry regi
              (map/dissoc-in regi [:cur-id->ctr-ids currency-id])
              (apply update regi :ctr-id->cur dissoc country-ids)))))

(defn ^Registry remove-countries
  "Removes countries from the given registry. Also unlinks constrained currencies in
  proper locations. Returns updated registry."
  [^Registry registry, country-ids]
  (remove-countries-core registry (prep-country-ids country-ids)))

(defn ^Registry add-countries
  "Associates the given country or countries with a currency. If the currency does not
  exist, exception is thrown. If the currency exists but differs in any detail from
  the existing currency from the registry, exception is thrown. If the currency
  exists and equals to the given in any aspect, country associations are added. Links
  from other countries to the currency are not removed unless the country is already
  linked with some other currency; in this case it will be unlinked first."
  [^Registry registry, currency, country-ids]
  (when-not (defined? currency registry)
    (throw
     (ex-info (str "Currency "
                   (if (instance? Currency currency) (.id ^Currency currency) currency)
                   " does not exist in a registry.") {:currency currency})))
  (let [^Currency c (unit currency registry)
        cid         (.id ^Currency c)
        ^Currency p (get (.cur-id->cur ^Registry registry) cid)
        cids        (prep-country-ids country-ids)]
    (when-not (= c p)
      (throw
       (ex-info (str "Currency " cid " differs from the currency existing in a registry.")
                {:currency c, :existing-currency p})))
    (if (nil? (seq cids)) registry
        (as-> registry regi
          (remove-countries-core regi cids)
          (apply update-in regi [:cur-id->ctr-ids cid] (fnil conj #{}) (set cids))
          (update regi :ctr-id->cur (partial apply assoc) (interleave cids (repeat c)))))))

(defn ^Registry register
  "Adds currency and (optional) countries to the given registry. Returns updated
  registry. If the updating occurs then all of the current countries associated with
  the currency are removed and replaced with the provided ones. To simply add new
  countries, use add-countries."
  (^Registry [^Registry registry, currency]
   (register registry currency nil false))
  (^Registry [^Registry registry
              ^Currency currency
              country-ids-or-update?]
   (if (boolean? country-ids-or-update?)
     (register registry currency nil country-ids-or-update?)
     (register registry currency country-ids-or-update? false)))
  (^Registry [^Registry registry
              ^Currency currency
              country-ids
              ^Boolean update?]
   (let [^Currency c (unit currency registry)
         cid         (.id ^Currency c)
         cid-to-cur  (.cur-id->cur ^Registry registry)]
     (when-not update?
       (when-some [^Currency p (get cid-to-cur cid)]
         (throw (ex-info
                 (str "Currency " cid " already exists in a registry.")
                 {:currency c, :existing-currency p}))))
     (let [registry    (unregister registry c)
           cid-to-cur  (.cur-id->cur ^Registry registry)
           registry    (assoc registry :cur-id->cur (assoc cid-to-cur cid c))
           numeric-id  (.nr ^Currency c)
           nr-to-cur   (.cur-nr->cur ^Registry registry)
           registry    (if (or (nil? numeric-id) (<= numeric-id 0)) registry
                           (assoc registry :cur-nr->cur (assoc nr-to-cur (long numeric-id) c)))]
       (add-countries registry currency country-ids)))))

(defn ^Registry register!
  "Adds currency and (optional) country to the global registry. Returns updated
  registry."
  (^Registry [^Currency currency]
   (swap! R register currency))
  (^Registry [^Currency currency, ^clojure.lang.Keyword country-id-or-update?]
   (swap! R register currency country-id-or-update?))
  (^Registry [^Currency currency, ^clojure.lang.Keyword country-id, ^Boolean update?]
   (swap! R register currency country-id update?)))

(defn ^Registry unregister!
  "Removes currency from the global registry. Automatically removes country constrains
  when necessary. Returns updated registry."
  [^Currency currency]
  (swap! R unregister currency))

(defn ^Registry add-countries!
  "Associates the given country (a keyword) or countries (seqable collection of
  keywords) with a currency in the global registry. If the currency does not exist,
  exception is thrown. If the currency exists but differs in any detail from the
  existing currency from the registry, exception is thrown. If the currency exists
  and equals to the given in any aspect, country associations are added. Links from
  other countries to the currency are not removed unless the country is already
  linked with some other currency; in this case it will be unlinked first."
  [country-ids]
  (swap! R add-countries country-ids))

(defn ^Registry remove-countries!
  "Removes country (a keyword) or countries (seqable collection of keywords) from the
  global registry. Automatically removes currency constrains when necessary. Returns
  updated registry."
  [country-ids]
  (swap! R remove-countries country-ids))

;;
;; Currencies loading.
;;

(defn ^Registry config->registry
  "Loads currencies and countries from an EDN file. First argument should be a function
  used to construct new currency objects and the second should be a string with a
  path to an EDN resource file containing registry data. Returns a registry
  initialized using values from the EDN file."
  ([]
   (config->registry config/default-resource-path))
  ([^String resource-path]
   (when-some [cfg (config/load resource-path)]
     (let [regi (registry/new-registry)
           curs (prep-currencies (config/currencies cfg))
           ctrs (prep-cur->ctr   (config/countries  cfg))
           vers (get cfg :version)
           regi (if (nil? vers) regi (assoc regi :version (str vers)))]
       (reduce (fn ^Registry [^Registry r, ^Currency c]
                 (register r c (get ctrs (.id ^Currency c))))
               regi curs)))))

;;
;; Setting default registry.
;;

(defn ^Registry set-default-registry!
  (^Registry []
   (set-default-registry! config/default-resource-path))
  (^Registry [resource-path]
   (registry/set! (config->registry resource-path))))

;;
;; Setting default currency.
;;

(defn set-default!
  [c]
  (alter-var-root #'*default* (constantly ^Currency (unit c))))

(defn unset-default!
  []
  (alter-var-root #'*default* (constantly nil)))

;;
;; Predicates.
;;

(defn ^Boolean currency?
  "Returns true if the given value is represented by a valid currency object."
  (^Boolean [c] (and (instance? Currency c) (keyword? (.id ^Currency c))))
  (^Boolean [c ^Registry registry] (and (instance? Currency c) (keyword? (.id ^Currency c)))))

(defn possible?
  "Returns true if the given value is a possible currency representation. If the
  registry is not given, the global one is used. By possible representation we mean
  that it is a currency with a keyword identifier or any other data type that can be
  successfully converted into such using the registry provided."
  {:tag Boolean :added "1.0.0"}
  (^Boolean [c] (keyword? (id c @R)))
  (^Boolean [c, ^Registry registry] (keyword? (id c registry))))

(defn ^Boolean has-numeric-id?
  "Returns true if the given currency has a numeric ID."
  (^Boolean [c] (> 0 (.nr ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (> 0 (.nr ^Currency (unit c registry)))))

(defn ^Boolean has-country?
  "Returns true if the given currency has at least one country for which it is an
  official currency."
  (^Boolean [c]
   (has-country? c @R))
  (^Boolean [c, ^Registry registry]
   (contains? (.cur-id->ctr-ids ^Registry registry) (id c))))

(defn ^Boolean in-domain?
  "Returns true if the given currency has a domain set to the first given
  argument."
  (^Boolean [ns c] (= ns (.ns ^Currency (unit c))))
  (^Boolean [ns c ^Registry registry] (= ns (.ns ^Currency (unit c registry)))))

(defn ^{:tag Boolean} big?
  "Returns true if the given currency has an automatic scale (decimal places)."
  (^Boolean [c] (auto-scaled? (.sc ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (auto-scaled? (.sc ^Currency (unit c registry)))))

(defn ^Boolean crypto?
  "Returns true if the given currency is a cryptocurrency. It is just a helper that
  check if the domain of a currency equals to :CRYPTO."
  (^Boolean [c] (= :CRYPTO (.ns ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :CRYPTO (.ns ^Currency (unit c registry)))))

(defn ^Boolean iso?
  "Returns true if the given currency is an official currency and its identifier is
  compliant with ISO standard. It is just a helper that check if the :ns field of a
  currency equals :ISO-4217."
  (^Boolean [c] (= :ISO-4217 (.ns ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :ISO-4217 (.ns ^Currency (unit c registry)))))

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
  (^Boolean [c] (some? (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (some? (.kind ^Currency (unit c)))))

(defn ^Boolean kind-of?
  "Returns a kind of the given currency equals to the one given as a second argument."
  (^Boolean [c ^clojure.lang.Keyword kind] (= kind (.kind ^Currency (unit c))))
  (^Boolean [c ^clojure.lang.Keyword kind ^Registry registry] (= kind (.kind ^Currency (unit c)))))

(defn ^Boolean fiat?
  "Returns true if the given currency is a kind of :FIAT."
  (^Boolean [c] (= :FIAT (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :FIAT (.kind ^Currency (unit c)))))

(defn ^Boolean fiduciary?
  "Returns true if the given currency is a kind of :FIDUCIARY."
  (^Boolean [c] (= :FIDUCIARY (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :FIDUCIARY (.kind ^Currency (unit c)))))

(defn ^Boolean combank?
  "Returns true if the given currency is a kind of :COMBANK."
  (^Boolean [c] (= :COMBANK (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :COMBANK (.kind ^Currency (unit c)))))

(defn ^Boolean commodity?
  "Returns true if the given currency is a kind of :COMMODITY."
  (^Boolean [c] (= :COMMODITY (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :COMMODITY (.kind ^Currency (unit c)))))

(defn ^Boolean decentralized?
  "Returns true if the given currency is a kind of :DECENTRALIZED."
  (^Boolean [c] (= :DECENTRALIZED (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :DECENTRALIZED (.kind ^Currency (unit c)))))

(def ^{:tag Boolean
       :arglists '([c] [c ^Registry registry])}
  decentralised?
  "Alias for decentralized?"
  decentralized?)

(defn ^Boolean experimental?
  "Returns true if the given currency is a kind of :EXPERIMENTAL."
  (^Boolean [c] (= :EXPERIMENTAL (.kind ^Currency (unit c))))
  (^Boolean [c ^Registry registry] (= :EXPERIMENTAL (.kind ^Currency (unit c)))))

;;
;; Scalable implementation.
;;

(extend-protocol scale/Scalable

  Currency

  (^Boolean scalable? [c] true)
  (^Boolean applied?  [c] true)

  (of [c] (.sc ^Currency c))

  (^Currency apply
   (^Currency [c] ^Currency c)
   (^Currency [c scale]               (assoc c :sc (int scale)))
   (^Currency [c scale rounding-mode] (assoc c :sc (int scale)))))

;;
;; Contextual macro.
;;

(defmacro with
  "Sets a default currency in a lexical context of the body."
  [currency & body]
  `(binding [*default* (currency/of ~currency)]
     ~@body))

;;
;; Printing.
;;

(defmethod print-method Currency
  [c w]
  (let [sc  (.sc   ^Currency c)
        nr  (.nr   ^Currency c)
        ki  (.kind ^Currency c)
        nr  (when (> nr 0) nr)]
    (print-simple
     (str "#currency{"
          ":id " (.id ^Currency c)
          ", :ns " (.ns ^Currency c)
          (when ki (str ", :kind " ki) )
          (when nr (str ", :nr " nr))
          (when-not (auto-scaled? sc)  (str ", :sc " sc))
          "}")
     w)))

;;
;; Populating registry with defaults.
;;

(set-default-registry!)
