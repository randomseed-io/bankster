(ns io.randomseed.bankster.registry

  ^{:doc    "Bankster, registry management."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [clojure.string                  :as      str]
            [io.randomseed.bankster          :as bankster]
            [io.randomseed.bankster.util.map :as      map]
            [io.randomseed.bankster.util.fs  :as       fs]
            [io.randomseed.bankster.util     :refer  :all])

  (:import  [io.randomseed.bankster Registry]))

;;
;; Registry constructor.
;;

(defn ^Registry new-registry
  "Creates a new registry."
  ([]
   (bankster/->Registry {} {} {} {}))
  ([^clojure.lang.PersistentHashMap cur-id->cur
    ^clojure.lang.PersistentHashMap cur-nr->cur
    ^clojure.lang.PersistentHashMap ctr-id->cur
    ^clojure.lang.PersistentHashMap cur-id->ctr-ids]
   (bankster/->Registry cur-id->cur cur-nr->cur ctr-id->cur cur-id->ctr-ids))
  ([^clojure.lang.PersistentHashMap cur-id->cur
    ^clojure.lang.PersistentHashMap ctr-id->cur]
   (bankster/->Registry cur-id->cur
                        (map/kmap-v :numeric cur-id->cur {})
                        ctr-id->cur
                        (map/invert-in-sets ctr-id->cur)))
  ([^clojure.lang.PersistentHashMap m]
   (bankster/map->Registry m)))

;;
;; Global, shared registry
;;

(def ^:private R (atom (new-registry)))

;;
;; Registry operations
;;

(defn ^clojure.lang.Atom global
  "Returns global registry object."
  []
  R)

(defn ^Boolean registry?
  "Returns true if the given object is a registry."
  [obj]
  (instance? Registry obj))

(defn ^Registry state
  "Returns current state of a global registry."
  []
  (deref R))

(defn ^Registry set!
  "Sets current state of a global registry."
  ([registry]
   (if (registry? registry)
     (reset! R ^Registry registry)
     (reset! R (new-registry ^clojure.lang.PersistentHashMap registry))))
  ([^clojure.lang.PersistentHashMap cur-id->cur
    ^clojure.lang.PersistentHashMap cur-nr->cur
    ^clojure.lang.PersistentHashMap ctr-id->cur
    ^clojure.lang.PersistentHashMap cur-id->ctr-ids]
   (set! R
         (new-registry cur-id->cur
                       cur-nr->cur
                       ctr-id->cur
                       cur-id->ctr-ids))))
