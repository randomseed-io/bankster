(ns io.randomseed.bankster.registry

  ^{:doc    "Bankster, registry management."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [new get set! update])

  (:require     [clojure.string                  :as      str]
                [io.randomseed.bankster          :as bankster]
                [io.randomseed.bankster.util.map :as      map]
                [io.randomseed.bankster.util.fs  :as       fs]
                [io.randomseed.bankster.util     :refer  :all])

  (:import  [io.randomseed.bankster Registry]
            [io.randomseed.bankster Currency Registry]
            [java.time LocalDateTime format.DateTimeFormatter]))

;;
;; Registry version generator.
;;

(defn ^String default-version
  []
  (. (LocalDateTime/now) format (DateTimeFormatter/ofPattern "YYYYMMddHHmmssSS")))

;;
;; Global, shared registry.
;;

(def R
  "Global registry object based on an Atom."
  (atom (Registry. {} {} {} {} (default-version))))

(defn ^clojure.lang.Atom global
  "Returns global registry object."
  []
  R)

(defn ^Registry state
  "Returns current state of a global registry."
  []
  (deref R))

;;
;; Dynamic registry.
;;

(def ^:dynamic ^Registry
  *default*
  "Registry, that if set to a truthy value (not nil and not false), will be used
  instead of a global, shared registry."
  nil)

;;
;; Registry state getter.
;;

(defmacro get
  "Gets the current state of a global registry. If the dynamic variable *default* is
  set to a truthy value, it will be used instead."
  {:added "1.0.0"}
  []
  `(or ^Registry *default* ^Registry (deref R)))

;;
;; Registry constructor.
;;

(defn ^Registry new-registry
  "Creates a new registry."
  (^Registry []
   (bankster/->Registry {} {} {} {} (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^String version]
   (bankster/->Registry cur-id->cur cur-nr->cur ctr-id->cur cur-id->ctr-ids version))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids]
   (new-registry cur-id->cur cur-nr->cur ctr-id->cur cur-id->ctr-ids (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^String version]
   (bankster/->Registry cur-id->cur
                        (map/map-keys-by-v :nr cur-id->cur)
                        ctr-id->cur
                        (map/invert-in-sets ctr-id->cur)
                        version))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur]
   (new-registry cur-id->cur ctr-id->cur (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap m]
   (bankster/map->Registry m)))

(def ^{:tag Registry
       :arglists '(^Registry []
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^String version]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^String version]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur]
                   ^Registry [^clojure.lang.PersistentHashMap m])}
  new
  "Alias for new-registry."
  new-registry)

;;
;; Registry operations.
;;

(defn ^Boolean registry?
  "Returns true if the given object is a registry."
  [obj]
  (instance? Registry obj))

(defn update
  "Updates a registry with a function that should take a registry as its first argument
  and return the updated one. It is a simple apply-based implementation provided for
  the sake of symmetry with update! which operates on a global registry object."
  {:added "1.0.0" :tag Registry}
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
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids]
   (set-state (new-registry cur-id->cur
                            cur-nr->cur
                            ctr-id->cur
                            cur-id->ctr-ids
                            (default-version))))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap cur-nr->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
              ^String version]
   (set-state (new-registry cur-id->cur
                            cur-nr->cur
                            ctr-id->cur
                            cur-id->ctr-ids
                            version))))

(def ^{:tag Registry :added "1.0.0"
       :arglists '(^Registry [^Registry registry]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap cur-nr->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->ctr-ids
                              ^String version])}
  set!
  "Sets current state of a global registry."
  set-state)

(defn update!
  "Updates a global registry using a function that should take a registry and return
  the updated version of it."
  [^clojure.lang.IFn fun & more]
  (apply swap! R fun more))

;;
;; Contextual macro.
;;

(defmacro with
  "Sets a registry in a lexical context of the body to be used instead of a global one
  in functions which require the registry and it was not passed as an argument."
  [^Registry registry & body]
  `(binding [*default* ^Registry registry]
     ~@body))

;;
;; Getters and helpers.
;;

(defmacro currency-by-id              [id registry] `(clojure.core/get (cur-id->cur     ^Registry registry) id))
(defmacro currency-by-nr              [nr registry] `(clojure.core/get (cur-nr->cur     ^Registry registry) nr))
(defmacro currency-by-country-id      [id registry] `(clojure.core/get (ctr-id->cur     ^Registry registry) id))
(defmacro country-ids-for-currency-id [id registry] `(clojure.core/get (cur-id->ctr-ids ^Registry registry) id))

;;
;; Printing.
;;

(defmethod print-method Registry
  [r w]
  (let [sid (Integer/toHexString (System/identityHashCode r))]
    (print-simple
     (str "#Registry@" sid "{"
          ":currencies " (count (.cur-id->cur ^Registry r)) ", "
          ":countries "  (count (.ctr-id->cur ^Registry r)) ", "
          ":version \""  (.version ^Registry r) "\"}")
     w)))
