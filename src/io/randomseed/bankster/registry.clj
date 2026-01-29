(ns

    ^{:doc    "Bankster, registry management."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.registry

  (:refer-clojure :exclude [new get set! update])

  (:require [io.randomseed.bankster          :as bankster]
            [io.randomseed.bankster.util.map :as      map]
            [clojure.tools.logging           :as      log])

  (:import  (io.randomseed.bankster Registry CurrencyHierarchies)
            (java.time              LocalDateTime)
            (java.time.format       DateTimeFormatter)))

;;
;; Empty hash maps
;;

(defn- h-m
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  ^clojure.lang.PersistentHashMap []
  (dissoc (hash-map nil nil) nil))

;;
;; Empty hierarchy record
;;

(defn- h-r
  {:tag CurrencyHierarchies :added "2.0.0"}
  ^CurrencyHierarchies []
  (CurrencyHierarchies. (make-hierarchy) (make-hierarchy)))

(defn- hierarchy-map?
  "Returns `true` if `x` looks like a hierarchy map produced by `make-hierarchy`."
  {:tag Boolean :added "2.0.0" :private true}
  [x]
  (and (map? x)
       (contains? x :parents)
       (contains? x :ancestors)
       (contains? x :descendants)
       (map? (clojure.core/get x :parents))
       (map? (clojure.core/get x :ancestors))
       (map? (clojure.core/get x :descendants))))

(defn- parent-map->hierarchy
  "Builds a hierarchy map out of a \"parent map\" (child -> parent/parents).

  The map value may be:
  - a single parent (keyword/symbol/class), or
  - a set/vector/list/seq of parents (multiple inheritance)."
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [^clojure.lang.IPersistentMap rels]
  (reduce (fn [h [child parent]]
            (let [parents (cond
                            (set? parent)        (sort-by str parent)
                            (sequential? parent) parent
                            :else               (list parent))]
              (reduce (fn [h p] (derive h child p)) h parents)))
          (make-hierarchy)
          ;; Stable order makes failures deterministic (cycles, invalid derives, etc.).
          (sort-by (fn [[child parent]]
                     (let [parent (cond
                                    (set? parent)        (sort-by str parent)
                                    (sequential? parent) parent
                                    :else               parent)]
                       (str child "->" parent)))
                   rels)))

(defn- ->hierarchy
  {:tag clojure.lang.IPersistentMap :added "2.0.0" :private true}
  [spec hierarchy-type]
  (cond
    (nil? spec)
    (make-hierarchy)

    (hierarchy-map? spec)
    spec

    (map? spec)
    (parent-map->hierarchy spec)

    :else
    (throw
     (ex-info
      "Invalid currency hierarchy specification."
      {:type  hierarchy-type
       :value spec}))))

(defn- ->currency-hierarchies
  {:tag CurrencyHierarchies :added "2.0.0" :private true}
  [spec]
  (cond
    (nil? spec)
    (h-r)

    (instance? CurrencyHierarchies spec)
    (CurrencyHierarchies. (->hierarchy (.domain ^CurrencyHierarchies spec) :domain)
                          (->hierarchy (.kind   ^CurrencyHierarchies spec) :kind))

    (map? spec)
    (CurrencyHierarchies. (->hierarchy (clojure.core/get spec :domain) :domain)
                          (->hierarchy (clojure.core/get spec :kind)   :kind))

    :else
    (throw
     (ex-info
      "Invalid currency hierarchies specification."
      {:value spec}))))

;;
;; Registry version generator.
;;

(defn default-version
  {:tag String :added "1.0.0"}
  []
  (. (LocalDateTime/now) format (DateTimeFormatter/ofPattern "yyyyMMddHHmmssSS")))

;;
;; Global, shared registry.
;;

(def ^{:tag clojure.lang.Atom :added "1.0.0"}
  R
  "Global registry object based on an Atom."
  (atom (Registry. (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-r) (default-version) (h-m))))

(defn global
  "Returns global registry object."
  {:tag clojure.lang.Atom :added "1.0.0"}
  []
  R)

(defn state
  "Returns current state of a global registry."
  {:tag Registry :added "1.0.0"}
  []
  (deref R))

;;
;; Dynamic registry.
;;

(def ^{:dynamic true :tag Registry :added "1.0.0"}
  *default*
  "Registry that, if set to a truthy value (not nil and not false), will be used
  instead of a global, shared registry."
  nil)

;;
;; Registry state getter.
;;

(defmacro get
  "Gets a current state of a global registry. If the dynamic variable
  `io.randomseed.bankster.registry/*default*` is set to a truthy value, it will be
  used instead."
  {:added "1.0.0"}
  []
  `(let [d# ^Registry *default*]
     (or d# ^Registry (deref R))))

;;
;; Diagnostics.
;;

(def ^{:tag Boolean :dynamic true :added "2.0.0"}
  *warn-on-inconsistency*
  "Dynamic flag which enables warnings when inconsistencies are found in a
  registry. Default is false."
  false)

(def ^{:tag clojure.lang.IFn :dynamic true :added "2.0.0"}
  *warnings-logger*
  "A logging function which should take a message string and an optional map. Used to
  issue registry warnings. Defaults to `clojure.tools.logging/warn`."
  (fn [ex-message ex-data] (log/warn ex-message ex-data)))

(defmacro inconsistency-warning
  "Wrapper that displays an inconsistency warning when
  `io.randomseed.bankster.registry/*warn-on-inconsistency*` is truthy. Uses
  `io.randomseed.bankster.registry/*warnings-logger*` function and passes message and
  data to it. Always evaluates body in an implicit do."
  {:added "2.0.0"}
  [ex-message ex-data & body]
  `(do (when io.randomseed.bankster.registry/*warn-on-inconsistency*
         (when-some [f# io.randomseed.bankster.registry/*warnings-logger*]
           (try (f# (str "Registry inconsistency: " ~ex-message) ~ex-data)
                (catch Throwable ~'_ nil))))
       ~@body))

;;
;; Java.
;;

;;
;; Registry constructor.
;;

(defn new-registry
  "Creates a new registry."
  {:tag Registry :added "1.0.0"}
  (^Registry []
   (bankster/->Registry (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-r) (default-version) (h-m)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs
              ^clojure.lang.PersistentHashMap cur-nr->curs
              cur-hierarchies
              ^String version]
   (let [^CurrencyHierarchies cur-hierarchies (->currency-hierarchies cur-hierarchies)]
     (bankster/->Registry cur-id->cur
                          cur-nr->cur
                          ctr-id->cur
                          cur-id->ctr-ids
                          cur-id->localized
                          cur-code->curs
                          cur-nr->curs
                          cur-hierarchies
                          version
                          (h-m))))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs
              ^clojure.lang.PersistentHashMap cur-nr->curs
              cur-hierarchies]
   (new-registry cur-id->cur
                 cur-nr->cur
                 ctr-id->cur
                 cur-id->ctr-ids
                 cur-id->localized
                 cur-code->curs
                 cur-nr->curs
                 cur-hierarchies
                 (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs
              ^clojure.lang.PersistentHashMap cur-nr->curs
              cur-hierarchies
              ^String version]
   (let [^CurrencyHierarchies cur-hierarchies (->currency-hierarchies cur-hierarchies)]
     (bankster/->Registry cur-id->cur
                          (map/map-keys-by-v :nr cur-id->cur)
                          ctr-id->cur
                          (map/invert-in-sets ctr-id->cur)
                          cur-id->localized
                          cur-code->curs
                          cur-nr->curs
                          cur-hierarchies
                          version
                          (h-m))))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs
              ^clojure.lang.PersistentHashMap cur-nr->curs]
   (new-registry cur-id->cur
                 ctr-id->cur
                 cur-id->localized
                 cur-code->curs
                 cur-nr->curs
                 (h-r)
                 (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap m]
   (let [r (bankster/map->Registry m)]
     (assoc r :hierarchies (->currency-hierarchies (clojure.core/get m :hierarchies))))))

(def ^{:tag      Registry
       :added    "1.0.0"
       :arglists '(^Registry []
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs
                              ^clojure.lang.PersistentHashMap cur-nr->curs
                              ^CurrencyHierarchies            cur-hierarchies
                              ^String version]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs
                              ^clojure.lang.PersistentHashMap cur-nr->curs
                              ^CurrencyHierarchies            cur-hierarchies]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs
                              ^clojure.lang.PersistentHashMap cur-nr->curs
                              ^CurrencyHierarchies            cur-hierarchies
                              ^String version]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs
                              ^clojure.lang.PersistentHashMap cur-nr->curs]
                   ^Registry [^clojure.lang.PersistentHashMap m])}
  new
  "Alias for new-registry."
  new-registry)

;;
;; Registry operations.
;;

(defn registry?
  "Returns true if the given object is a registry."
  {:tag Boolean :added "1.0.0"}
  [obj]
  (instance? Registry obj))

(defn update
  "Updates a registry with a function that should take a registry as its first argument
  and return the updated one. It is a simple apply-based implementation provided for
  the sake of symmetry with update! which operates on a global registry object."
  {:tag Registry :added "1.0.0"}
  [^Registry r ^clojure.lang.IFn fun & more]
  (apply fun r more))

(defn set-state
  "Sets current state of a global registry."
  {:added "1.0.0" :tag Registry :private true}
  (^Registry [^Registry registry]
   (if (registry? registry)
     (reset! R ^Registry registry)
     (reset! R (new-registry ^clojure.lang.PersistentHashMap registry)))))

(def ^{:tag      Registry :added "1.0.0"
       :arglists '(^Registry [^Registry registry])}
  set!
  "Sets current state of a global registry."
  set-state)

(defn update!
  "Updates a global registry using a function that should take a registry and return
  the updated version of it."
  {:tag Registry :added "1.0.0"}
  [^clojure.lang.IFn fun & more]
  (apply swap! R fun more))

;;
;; Contextual macro.
;;

(defmacro with
  "Sets a registry in a lexical context of the body to be used instead of a global one
  in functions which require the registry and it was not passed as an argument."
  {:added "1.0.0"}
  [^Registry registry & body]
  `(binding [*default* ^io.randomseed.bankster.Registry ~registry]
     ~@body))

;;
;; Getters and helpers.
;;

(defmacro currency-id->currency*
  "Returns the currency ID to currency map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-id->cur ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.cur-id->cur ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->cur ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro currency-nr->currency*
  "Returns the currency number to currency map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-nr->cur ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.cur-nr->cur ^io.randomseed.bankster.Registry ~registry))
  ([nr registry] `(clojure.core/get (.cur-nr->cur ^io.randomseed.bankster.Registry ~registry) ~nr)))

(defmacro currency-nr->currencies*
  "Returns the currency number to currencies map from a registry. If the registry is
  not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-nr->curs ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.cur-nr->curs ^io.randomseed.bankster.Registry ~registry))
  ([nr registry] `(clojure.core/get (.cur-nr->curs ^io.randomseed.bankster.Registry ~registry) ~nr)))

(defmacro currency-code->currencies*
  "Returns the currency short-code to currencies map from a registry. If the registry
  is not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-code->curs ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.cur-code->curs ^io.randomseed.bankster.Registry ~registry))
  ([code registry] `(clojure.core/get (.cur-code->curs ^io.randomseed.bankster.Registry ~registry) ~code)))

(defmacro country-id->currency*
  "Returns the country ID to currency map from a registry. If the registry is not given
  the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If it is
  not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.ctr-id->cur ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.ctr-id->cur ^io.randomseed.bankster.Registry ~registry))
  ([country registry] `(clojure.core/get (.ctr-id->cur ^io.randomseed.bankster.Registry ~registry) ~country)))

(defmacro currency-id->country-ids*
  "Returns the currency ID to country IDs map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-id->ctr-ids ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.cur-id->ctr-ids ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->ctr-ids ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro currency-id->localized*
  "Returns the currency ID to localized properties map from a registry. If the registry
  is not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-id->localized ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.cur-id->localized ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->localized ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro ext*
  "Returns extra data map of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:added "2.0.0"}
  ([] `(.ext ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.ext ^io.randomseed.bankster.Registry ~registry))
  ([k registry] `(clojure.core/get (.ext ^io.randomseed.bankster.Registry ~registry) ~k)))

(defmacro version*
  "Returns a version string of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:added "2.0.0"}
  ([] `(.version ^io.randomseed.bankster.Registry (get)))
  ([registry] `(.version ^io.randomseed.bankster.Registry ~registry)))

(defn currency-id->currency
  "Returns the currency ID to currency map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-id->currency*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-id->currency* registry))
  (^clojure.lang.PersistentHashMap [id ^Registry registry] (currency-id->currency* id registry)))

(defn currency-nr->currency
  "Returns the currency number to currency map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-nr->currency*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-nr->currency* registry))
  (^clojure.lang.PersistentHashMap [nr ^Registry registry] (currency-nr->currency* nr registry)))

(defn currency-nr->currencies
  "Returns the currency number to currencies map from a registry. If the registry is
  not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-nr->currencies*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-nr->currencies* registry))
  (^clojure.lang.PersistentHashMap [nr ^Registry registry] (currency-nr->currencies* nr registry)))

(defn currency-code->currencies
  "Returns the currency short-code to currencies map from a registry. If the registry
  is not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-code->currencies*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-code->currencies* registry))
  (^clojure.lang.PersistentHashMap [code ^Registry registry] (currency-code->currencies* code registry)))

(defn country-id->currency
  "Returns the country ID to currency map from a registry. If the registry is not given
  the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If it is
  not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (country-id->currency*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (country-id->currency* registry))
  (^clojure.lang.PersistentHashMap [country-id ^Registry registry] (country-id->currency* country-id registry)))

(defn currency-id->country-ids
  "Returns the currency ID to country IDs map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-id->country-ids*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-id->country-ids* registry))
  (^clojure.lang.PersistentHashMap [id ^Registry registry] (currency-id->country-ids* id registry)))

(defn currency-id->localized
  "Returns the currency ID to localized properties map from a registry. If the registry
  is not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-id->localized*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-id->localized* registry))
  (^clojure.lang.PersistentHashMap [id ^Registry registry] (currency-id->localized* id registry)))

(defn version
  "Returns a version string of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^String [] (version*))
  (^String [^Registry registry] (version* registry)))

(defn ext
  "Returns extra data map of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (ext*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (ext* registry))
  (^clojure.lang.PersistentHashMap [k ^Registry registry] (ext* k registry)))

;;
;; Printing.
;;

(defmethod print-method io.randomseed.bankster.Registry
  [r w]
  (let [sid (Integer/toHexString (System/identityHashCode r))]
    (print-simple
     (str "#Registry[{"
          ":currencies " (count (.cur-id->cur ^io.randomseed.bankster.Registry r)) ", "
          ":countries "  (count (.ctr-id->cur ^io.randomseed.bankster.Registry r)) ", "
          ":version \""  (.version ^io.randomseed.bankster.Registry r) "\"} 0x" sid "]")
     w)))
