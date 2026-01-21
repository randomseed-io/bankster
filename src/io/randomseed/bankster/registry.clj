(ns io.randomseed.bankster.registry

  ^{:doc    "Bankster, registry management."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [new get set! update])

  (:require [io.randomseed.bankster          :as bankster]
            [io.randomseed.bankster.util.map :as      map])

  (:import  (io.randomseed.bankster Registry)
            (java.time              LocalDateTime)
            (java.time.format       DateTimeFormatter)))

;;
;; Empty hash maps
;;

(defn- h-m
  {:tag clojure.lang.PersistentHashMap :added "1.2.20"}
  ^clojure.lang.PersistentHashMap []
  (dissoc (hash-map nil nil) nil))

;;
;; Registry version generator.
;;

(defn default-version
  {:tag String :added "1.0.0"}
  []
  (. (LocalDateTime/now) format (DateTimeFormatter/ofPattern "YYYYMMddHHmmssSS")))

;;
;; Global, shared registry.
;;

(def ^{:tag clojure.lang.Atom :added "1.0.0"}
  R
  "Global registry object based on an Atom."
  (atom (Registry. (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (default-version))))

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
  "Registry, that if set to a truthy value (not nil and not false), will be used
  instead of a global, shared registry."
  nil)

;;
;; Registry state getter.
;;

(defmacro get
  "Gets a current state of a global registry. If the dynamic variable *default* is
  set to a truthy value, it will be used instead."
  {:added "1.0.0"}
  []
  `(or ^Registry *default* ^Registry (deref R)))

;;
;; Registry constructor.
;;

(defn new-registry
  "Creates a new registry."
  {:tag Registry :added "1.0.0"}
  (^Registry []
   (bankster/->Registry (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs
              ^String version]
   (bankster/->Registry cur-id->cur
                        cur-nr->cur
                        ctr-id->cur
                        cur-id->ctr-ids
                        cur-id->localized
                        cur-code->curs
                        version))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs]
   (new-registry cur-id->cur
                 cur-nr->cur
                 ctr-id->cur
                 cur-id->ctr-ids
                 cur-code->curs
                 (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs
              ^String version]
   (bankster/->Registry cur-id->cur
                        (map/map-keys-by-v :nr cur-id->cur)
                        ctr-id->cur
                        (map/invert-in-sets ctr-id->cur)
                        cur-id->localized
                        cur-code->curs
                        version))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-code->curs]
   (new-registry cur-id->cur
                 ctr-id->cur
                 cur-id->localized
                 cur-code->curs
                 (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap m]
   (bankster/map->Registry m)))

(def ^{:tag Registry
       :added "1.0.0"
       :arglists '(^Registry []
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs
                              ^String version]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs
                              ^String version]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs]
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
     (reset! R (new-registry ^clojure.lang.PersistentHashMap registry))))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^clojure.lang.PersistentHashMap cur-id->localized]
   (set-state (new-registry cur-id->cur
                            cur-nr->cur
                            ctr-id->cur
                            cur-id->ctr-ids
                            cur-id->localized
                            (default-version))))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^String version]
   (set-state (new-registry cur-id->cur
                            cur-nr->cur
                            ctr-id->cur
                            cur-id->ctr-ids
                            cur-id->localized
                            version))))

(def ^{:tag Registry :added "1.0.0"
       :arglists '(^Registry [^Registry registry]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-code->curs
                              ^String version])}
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
  `(binding [*default* ^Registry ~registry]
     ~@body))

;;
;; Getters and helpers.
;;

(defmacro currency-id->currency
  "Returns the currency ID to currency map from a registry. If the registry is not
  given the dynamic variable *default* is tried. If it is not set, current state of a
  global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-id->cur (get)))
  ([registry] `(.cur-id->cur ^Registry ~registry)))

(defmacro currency-nr->currency
  "Returns the currency number to currency map from a registry. If the registry is not
  given the dynamic variable *default* is tried. If it is not set, current state of a
  global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-nr->cur (get)))
  ([registry] `(.cur-nr->cur ^Registry ~registry)))

(defmacro currency-code->currencies
  "Returns the currency short-code to currencies map from a registry. If the registry
  is not given the dynamic variable *default* is tried. If it is not set, current
  state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-code->curs (get)))
  ([registry] `(.cur-code->curs ^Registry ~registry)))

(defmacro country-id->currency
  "Returns the country ID to currency map from a registry. If the registry is not given
  the dynamic variable *default* is tried. If it is not set, current state of a
  global registry is used instead."
  {:added "1.0.0"}
  ([] `(.ctr-id->cur (get)))
  ([registry] `(.ctr-id->cur ^Registry ~registry)))

(defmacro currency-id->country-ids
  "Returns the currency ID to country IDs map from a registry. If the registry is not
  given the dynamic variable *default* is tried. If it is not set, current state of a
  global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-id->ctr-ids (get)))
  ([registry] `(.cur-id->ctr-ids ^Registry ~registry)))

(defmacro currency-id->localized
  "Returns the currency ID to localized propertied map from a registry. If the registry
  is not given the dynamic variable *default* is tried. If it is not set, current
  state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(.cur-id->localized (get)))
  ([registry] `(.cur-id->localized ^Registry ~registry)))

;;
;; Printing.
;;

(defmethod print-method Registry
  [r w]
  (let [sid (Integer/toHexString (System/identityHashCode r))]
    (print-simple
     (str "#Registry[{"
          ":currencies " (count (.cur-id->cur ^Registry r)) ", "
          ":countries "  (count (.ctr-id->cur ^Registry r)) ", "
          ":version \""  (.version ^Registry r) "\"} 0x" sid "]")
     w)))
