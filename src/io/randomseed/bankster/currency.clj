(ns io.randomseed.bankster.currency

  ^{:doc    "Bankster library, currency operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [ns new])

  (:require [clojure.string                  :as           str]
            [clojure.edn                     :as           edn]
            [clojure.java.io                 :as            io]
            [io.randomseed.bankster          :refer       :all]
            [io.randomseed.bankster.registry :as      registry]
            [io.randomseed.bankster.db       :as            db]
            [io.randomseed.bankster.util.map :as           map]
            [io.randomseed.bankster.util.fs  :as            fs]
            [io.randomseed.bankster.util     :refer       :all])

  (:import  [io.randomseed.bankster Currency Registry]))

;;
;; Constants.
;;

(def ^{:tag 'long} unknown-decimal-places (long -1))
(def ^{:tag 'long} no-numeric-id (long 0))

;;
;; Global registry.
;;

(def ^:private R (registry/global))

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
   (->Currency id no-numeric-id unknown-decimal-places
               (keyword (or (try-upper-case (namespace id)) :ISO-4217))
               nil))
  (^Currency [^clojure.lang.Keyword id, ^long numeric-id]
   (->Currency id numeric-id unknown-decimal-places
               (keyword (or (try-upper-case (namespace id)) :ISO-4217))
               nil))
  (^Currency [^clojure.lang.Keyword id, ^long numeric-id, ^long decimal-places]
   (->Currency id numeric-id decimal-places
               (keyword (or (try-upper-case (namespace id)) :ISO-4217))
               nil))
  (^Currency [^clojure.lang.Keyword id, ^long numeric-id, ^long decimal-places, ^clojure.lang.Keyword kind]
   (->Currency id numeric-id decimal-places
               (keyword (or (try-upper-case (namespace id)) :ISO-4217))
               kind)))

(def ^{:tag Currency
       :arglists '(^Currency [^clojure.lang.Keyword id]
                   ^Currency [^clojure.lang.Keyword id, ^long numeric-id]
                   ^Currency [^clojure.lang.Keyword id, ^long numeric-id, ^long decimal-places]
                   ^Currency [^clojure.lang.Keyword id, ^long numeric-id, ^long decimal-places, ^clojure.lang.Keyword kind])}
  new
  "Alias for new-currency."
  new-currency)

;;
;; Payable protocol.
;;

(defprotocol ^{:added "1.0.0"} Payable
  (^{:tag clojure.lang.Keyword :added "1.0.0"}
   id
   [id] [id registry]
   "Returns currency identifier as keyword. If the registry is not given it will use
    the default one. If the given argument is already an identifier (a keyword), it
    will be returned as is.")

  (^{:tag Currency :added "1.0.0"}
   of
   [id] [id registry]
   "Returns a currency object for the given id and registry. If the registry is not
    given it will use the default one. If the currency record is passed, it will be
    returned as is.")

  (^{:tag Boolean :added "1.0.0"}
   defined?
   [id] [id registry]
   "Returns true if the given currency exists in a registry. If the registry is not given,
    the default one is used.")

  (^{:tag Boolean :added "1.0.0"}
   same?
   [a b] [a b registry]
   "Returns true if two currencies have the same ID. That does not mean the objects
   are of the same contents (e.g. numerical IDs or decimal places may differ) but
   it's more performant in 99% cases."))

;;
;; Currency querying functions
;;

(extend-protocol Payable

  Currency

  (of
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

  (same?
    (^Boolean [a b] (= (.id ^Currency a) (id b)))
    (^Boolean [a b ^Registry registry] (= (.id ^Currency a) (id b registry))))

  Number

  (of
    (^Currency [^clojure.lang.Keyword num]
     (of num @R))
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

  (same?
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

  (of
    (^Currency [^clojure.lang.Keyword id]
     (of id @R))
    (^Currency [^clojure.lang.Keyword id, ^Registry registry]
     (or (get (.cur-id->cur ^Registry registry) id)
         (throw (ex-info
                 (str "Currency " id " not found in a registry.")
                 {:registry registry})))))

  (id
    (^clojure.lang.Keyword [id] id)
    (^clojure.lang.Keyword [id, ^Registry registry] id))

  (defined?
    (^Boolean [id]
     (contains? (.cur-id->cur ^Registry @R) id))
    (^Boolean [id, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry) id)))

  (same?
    (^Boolean [a b] (= a (id b @R)))
    (^Boolean [a b ^Registry registry] (= a (id b registry))))

  String

  (of
    (^Currency [id] (of (keyword id)))
    (^Currency [id, ^Registry registry] (of (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [id] (keyword id))
    (^clojure.lang.Keyword [id, ^Registry registry] (keyword id)))

  (defined?
    (^Boolean [id]
     (contains? (.cur-id->cur ^Registry @R) (keyword id)))
    (^Boolean [id, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry) (keyword id))))

  (same?
    (^Boolean [a b] (= (keyword a) (id b)))
    (^Boolean [a b ^Registry registry] (= (keyword a) (id b registry))))

  clojure.lang.Symbol

  (of
    (^Currency [id] (of (keyword id)))
    (^Currency [id, ^Registry registry] (of (keyword id) registry)))

  (id
    (^clojure.lang.Keyword [id] (keyword id))
    (^clojure.lang.Keyword [id, ^Registry registry] (keyword id)))

  (defined?
    (^Boolean [id]
     (contains? (.cur-id->cur ^Registry @R) (keyword id)))
    (^Boolean [id, ^Registry registry]
     (contains? (.cur-id->cur ^Registry registry) (keyword id))))

  (same?
    (^Boolean [a b] (= (keyword a) (id b)))
    (^Boolean [a b ^Registry registry] (= (keyword a) (id b registry)))))

;;
;; Currency properties.
;;

(defn ^{:tag 'long} nr
  "Returns currency numeric ID as a long number. For currencies without the assigned
  number it will return 0."
  (^long [c] (.nr ^Currency (of c)))
  (^long [c ^Registry registry] (.nr ^Currency (of c registry))))

(def ^{:tag 'long
       :arglists '([c]
                   [c, ^Registry registry])}
  numeric-id
  "Alias for nr."
  nr)

(defn ^{:tag 'long} dp
  "Returns currency's decimal places as a long number. For currencies without the
  assigned decimal places it will return -1."
  (^long [c] (.dp ^Currency (of c)))
  (^long [c ^Registry registry] (.dp ^Currency (of c registry))))

(def ^{:tag 'long
       :arglists '([c] [c, ^Registry registry])}
  decimal-places
  "Alias for dp."
  dp)

(defn ^clojure.lang.Keyword domain
  "Returns currency domain as a keyword. For currencies with simple identifiers it will
  be :ISO-4217. For currencies with namespace-qualified identifiers it will be the
  upper-cased namespace name (e.g. CRYPTO) set during creation a currency object."
  (^clojure.lang.Keyword [c] (.ns ^Currency (of c)))
  (^clojure.lang.Keyword [c, ^Registry registry] (.ns ^Currency (of c registry))))

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
  (^clojure.lang.Keyword [c] (.kind ^Currency (of c)))
  (^clojure.lang.Keyword [c ^Registry registry] (.kind ^Currency (of c registry))))

(defn ^String code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced the namespace will be used as a prefix and slash character
  as a separator."
  (^String [c] (subs 1 (str (id c))))
  (^String [c ^Registry registry] (subs 1 (str (id c registry)))))

(defn ^String short-code
  "Returns a currency code as a string for the given currency object. If the currency
  identifier is namespaced only the base code (without a namespace) will be
  returned (which may lead to misinformation if there are two or more currencies with
  the same base ID but different namespaces)."
  (^String [c] (subs 1 (name (id c))))
  (^String [c ^Registry registry] (subs 1 (name (id c registry)))))

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
   (of-country country-id @R))
  (^Currency [^clojure.lang.Keyword country-id, ^Registry registry]
   (get (.ctr-id->cur ^Registry registry))))

;;
;; Adding and removing currency to/from registry.
;;

(defn ^Registry unregister
  "Removes currency from the given registry. Also removes country constrains when
  necessary. Returns updated registry."
  [^Registry registry, currency]
  (let [^Currency c (of currency registry)
        currency-id (id c)
        country-ids (get (.cur-id->ctr-ids ^Registry registry) currency-id)
        ^Registry registry (-> registry
                               (map/dissoc-in [:cur-id->cur currency-id])
                               (map/dissoc-in [:cur-nr->cur (nr c)]))]
    (if-not (contains? (.cur-id->ctr-ids ^Registry registry) currency-id) registry
            (as-> registry regi
              (map/dissoc-in regi [:cur-id->ctr-ids currency-id])
              (apply update regi :ctr-id->cur dissoc country-ids)))))

(defn ^Registry unregister-country
  "Removes country from the given registry. Also removes constrained currencies from a
  proper locations. Returns updated registry."
  [^Registry registry
   ^clojure.lang.Keyword country-id]
  (let [^clojure.lang.Keyword country-id (keyword country-id)
        ^Currency currency (get (.ctr-id->cur ^Registry registry) country-id)
        ^Registry registry (map/dissoc-in registry [:ctr-id->cur country-id])]
    (if (nil? currency) registry
        (update-in registry [:cur-id->ctr-ids country-id] disj currency))))

(defn ^Registry register
  "Adds currency and (optional) countries to the given registry. Returns updated
  registry. If the updating occurs then the current, all countries associated with
  the currency are removed and replaced with the provided. To add new countries, use
  add-countries."
  (^Registry [^Registry registry, currency]
   (register registry currency nil false))
  (^Registry [^Registry registry, currency, country-ids-or-update?]
   (if (boolean? country-ids-or-update?)
     (register registry currency nil country-ids-or-update?)
     (register registry currency country-ids-or-update? false)))
  (^Registry [^Registry registry, currency, country-ids, ^Boolean update?]
   (let [^Currency c (of currency registry)]
     (when-not update?
       (when-some [^Currency p (get-in registry [:cur-id->cur (id c)])]
         (throw (ex-info
                 (str "Currency " (id c) " already exists in a registry.")
                 {:currency c, :existing-currency p}))))
     (let [currency-id (id c registry)
           registry    (assoc-in (unregister registry c) [:cur-id->cur currency-id] c)
           numeric-id  (nr c registry)
           registry    (if (<= numeric-id 0) registry
                           (assoc-in registry [:cur-nr->cur numeric-id] c))
           country-ids (when country-ids
                         (if (sequential? country-ids) country-ids
                             (if (and (seqable? country-ids) (not (string? country-ids)))
                               (seq country-ids)
                               (list country-ids))))]
       (if (nil? (seq country-ids)) registry
           (let [cids (map keyword country-ids)]
             (as-> registry regi
               (apply update-in regi [:cur-id->ctr-ids currency-id] (fnil conj #{}) (set cids))
               (update regi :ctr-id->cur (partial apply assoc) (interleave cids (repeat c))))))))))


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

(defn ^Registry unregister-country!
  "Removes country from the global registry. Automatically removes currency constrains
  when necessary. Returns updated registry."
  [^clojure.lang.Keyword country-id]
  (swap! R unregister-country (keyword country-id)))

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
  (^Boolean [c] (> 0 (.nr ^Currency (of c))))
  (^Boolean [c ^Registry registry] (> 0 (.nr ^Currency (of c registry)))))

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
  (^Boolean [ns c] (= ns (.ns ^Currency (of c))))
  (^Boolean [ns c ^Registry registry] (= ns (.ns ^Currency (of c registry)))))

(defn ^Boolean crypto?
  "Returns true if the given currency is a cryptocurrency. It is just a helper that
  check if the domain of a currency equals to :CRYPTO."
  (^Boolean [c] (= :CRYPTO (.ns ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :CRYPTO (.ns ^Currency (of c registry)))))

(defn ^Boolean iso?
  "Returns true if the given currency is an official currency and its identifier is
  compliant with ISO standard. It is just a helper that check if the :ns field of a
  currency equals :ISO-4217."
  (^Boolean [c] (= :ISO-4217 (.ns ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :ISO-4217 (.ns ^Currency (of c registry)))))

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
  (^Boolean [c] (some? (.kind ^Currency (of c))))
  (^Boolean [c ^Registry registry] (some? (.kind ^Currency (of c)))))

(defn ^Boolean kind-of?
  "Returns a kind of the given currency equals to the one given as a second argument."
  (^Boolean [c ^clojure.lang.Keyword kind] (= kind (.kind ^Currency (of c))))
  (^Boolean [c ^clojure.lang.Keyword kind ^Registry registry] (= kind (.kind ^Currency (of c)))))

(defn ^Boolean fiat?
  "Returns true if the given currency is a kind of :FIAT."
  (^Boolean [c] (= :FIAT (.kind ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :FIAT (.kind ^Currency (of c)))))

(defn ^Boolean fiduciary?
  "Returns true if the given currency is a kind of :FIDUCIARY."
  (^Boolean [c] (= :FIDUCIARY (.kind ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :FIDUCIARY (.kind ^Currency (of c)))))

(defn ^Boolean combank?
  "Returns true if the given currency is a kind of :COMBANK."
  (^Boolean [c] (= :COMBANK (.kind ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :COMBANK (.kind ^Currency (of c)))))

(defn ^Boolean commodity?
  "Returns true if the given currency is a kind of :COMMODITY."
  (^Boolean [c] (= :COMMODITY (.kind ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :COMMODITY (.kind ^Currency (of c)))))

(defn ^Boolean decentralized?
  "Returns true if the given currency is a kind of :DECENTRALIZED."
  (^Boolean [c] (= :DECENTRALIZED (.kind ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :DECENTRALIZED (.kind ^Currency (of c)))))

(def ^{:tag Boolean
       :arglists '([c] [c ^Registry registry])}
  decentralised?
  "Alias for decentralized?"
  decentralized?)

(defn ^Boolean experimental?
  "Returns true if the given currency is a kind of :EXPERIMENTAL."
  (^Boolean [c] (= :EXPERIMENTAL (.kind ^Currency (of c))))
  (^Boolean [c ^Registry registry] (= :EXPERIMENTAL (.kind ^Currency (of c)))))
