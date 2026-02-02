(ns

    ^{:doc    "Bankster, registry management."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.registry

  (:refer-clojure :exclude [new get update])

  (:require [io.randomseed.bankster          :as      bankster]
            [io.randomseed.bankster.util.map :as           map]
            [clojure.tools.logging           :as           log]
            [clojure.tools.logging.impl      :as      log-impl])

  (:import  (io.randomseed.bankster Registry CurrencyHierarchies Currency)
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
  (let [h (bankster/map->CurrencyHierarchies {})]
    ;; Initialize all declared hierarchy axes (record fields) with empty hierarchies.
    (reduce (fn [^CurrencyHierarchies h k]
              (assoc h k (make-hierarchy)))
            h
            (keys h))))

;;
;; Weighted indexes helpers.
;;

(def ^{:tag clojure.lang.Keyword :const true :private true :added "2.1.0"}
  ^clojure.lang.Keyword weight-meta-key
  "Metadata key used to store currency weight."
  :io.randomseed.bankster.currency/weight)

(defn- currency-weight
  "Returns currency weight stored in metadata, defaulting to 0."
  {:private true :added "2.1.0"}
  ^long [^Currency c]
  (let [m (meta c)]
    (unchecked-long
     (if (and (some? m) (contains? m weight-meta-key))
       (long (clojure.core/get m weight-meta-key))
       0))))

(defn- with-weight*
  "Attaches a weight hint to a currency using metadata.

  Weight of 0 is treated as default and is not stored."
  {:tag Currency :private true :added "2.1.0"}
  ^Currency [^Currency c weight]
  (let [w (unchecked-int (or weight 0))
        m (meta c)]
    (if (zero? w)
      (if (and (some? m) (contains? m weight-meta-key))
        (with-meta c (dissoc m weight-meta-key))
        c)
      (with-meta c (assoc (or m {}) weight-meta-key w)))))

(defn- weighted-currencies
  "Constructor for weighted currency buckets: smallest weight wins.
  Total order: (weight asc) then (id asc)."
  {:tag clojure.lang.PersistentTreeSet :private true :added "2.1.0"}
  ([]
   (sorted-set-by
    (fn [^Currency a ^Currency b]
      ;; Comparator must be consistent with `Currency` equality which ignores weight.
      (let [ida (.id a)
            idb (.id b)]
        (if (identical? ida idb)
          0
          (let [wa (currency-weight a)
                wb (currency-weight b)]
            (if (== wa wb)
              (compare ida idb)
              (compare wa wb))))))))
  (^clojure.lang.PersistentTreeSet [^clojure.lang.PersistentTreeSet s]
   (or s (weighted-currencies))))

(defn- valid-numeric-id?
  "Returns `true` if a numeric ID is valid."
  {:tag Boolean :private true :added "2.1.0"}
  [^long nr]
  (pos? nr))

(defn- currency-code-key
  "Returns a currency code keyword (without namespace) for a currency ID."
  {:tag clojure.lang.Keyword :private true :added "2.1.0"}
  [^clojure.lang.Keyword cid]
  (when (keyword? cid)
    (if (simple-keyword? cid) cid (keyword (name cid)))))

(defn- apply-weights
  "Applies registry weights to currencies by attaching weight metadata."
  {:tag clojure.lang.PersistentHashMap :private true :added "2.1.0"}
  [^clojure.lang.PersistentHashMap cur-id->cur
   ^clojure.lang.PersistentHashMap cur-id->weight]
  (let [cur-id->cur    (or cur-id->cur (h-m))
        cur-id->weight (or cur-id->weight (h-m))]
    (reduce-kv (fn [m cid ^Currency c]
                 (assoc m cid (with-weight* c (clojure.core/get cur-id->weight cid 0))))
               cur-id->cur
               cur-id->cur)))

(defn- derive-indexes
  "Derives secondary indexes from base currency maps."
  {:tag clojure.lang.IPersistentMap :private true :added "2.1.0"}
  [^clojure.lang.PersistentHashMap cur-id->cur
   ^clojure.lang.PersistentHashMap ctr-id->cur
   ^clojure.lang.PersistentHashMap cur-id->weight]
  (let [cur-id->cur   (apply-weights cur-id->cur cur-id->weight)
        ctr-id->cur   (or ctr-id->cur (h-m))
        ctr-id->cid   (reduce-kv (fn [m k v]
                                   (let [cid (if (instance? Currency v) (.id ^Currency v) v)]
                                     (assoc m k cid)))
                                 (h-m)
                                 ctr-id->cur)
        idx           (reduce-kv
                       (fn [m _cid ^Currency c]
                         (let [cid  (.id c)
                               code (currency-code-key cid)
                               nr   (long (.numeric c))
                               dom  (.domain c)]
                           (cond-> m
                             (some? code)
                             (update-in [:cur-code->curs code]
                                        (fn [^clojure.lang.PersistentTreeSet s]
                                          (conj (weighted-currencies s) c)))

                             (valid-numeric-id? nr)
                             (update-in [:cur-nr->curs nr]
                                        (fn [^clojure.lang.PersistentTreeSet s]
                                          (conj (weighted-currencies s) c)))

                             (some? dom)
                             (update-in [:cur-dom->curs dom]
                                        (fn [^clojure.lang.PersistentTreeSet s]
                                          (conj (weighted-currencies s) c))))))
                       {:cur-code->curs (h-m)
                        :cur-nr->curs   (h-m)
                        :cur-dom->curs  (h-m)}
                       cur-id->cur)
        nr->cur        (reduce-kv (fn [m nr ^clojure.lang.PersistentTreeSet curs]
                                    (assoc m nr (first curs)))
                                  (h-m)
                                  (:cur-nr->curs idx))
        cur-id->ctr-ids (map/invert-in-sets ctr-id->cid)]
    (assoc idx
           :cur-id->cur     cur-id->cur
           :cur-nr->cur     nr->cur
           :cur-id->ctr-ids cur-id->ctr-ids)))

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
                            :else                (list parent))]
              (reduce (fn [h p] (derive h child p)) h parents)))
          (make-hierarchy)
          ;; Stable order makes failures deterministic (cycles, invalid derives, etc.).
          (sort-by (fn [[child parent]]
                     (let [parent (cond
                                    (set? parent)        (sort-by str parent)
                                    (sequential? parent) parent
                                    :else                parent)]
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
  (let [m (cond
            (nil? spec)                          {}
            (instance? CurrencyHierarchies spec) (into {} spec)
            (map? spec)                          spec
            :else
            (throw
             (ex-info
              "Invalid currency hierarchies specification."
              {:value spec})))
        h (h-r)
        h (assoc h
                 :domain (->hierarchy (clojure.core/get m :domain) :domain)
                 :kind   (->hierarchy (clojure.core/get m :kind)   :kind))]
    ;; Any additional keys are treated as separate hierarchy axes.
    (reduce (fn [^CurrencyHierarchies h [k v]]
              (assoc h k (->hierarchy v k)))
            h
            (dissoc m :domain :kind))))

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
  (atom (Registry. (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-r) (default-version) (h-m))))

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
  "Without arguments it gets a current state of a global registry. If the dynamic
  variable `io.randomseed.bankster.registry/*default*` is set to a truthy value, it
  will be used instead.

  When a `registry` argument is provided it will be tried before using a dynamic or a
  global registry, unless its value is `nil` or `false`.

  When the literal `true` is provided, it acts as a sentinel meaning: use the default
  registry. Caution: this works only on syntactic level."
  {:added "1.0.0"}
  ([]
   ;; Use a typed local to avoid losing type hints through macroexpansion of `or`.
   ;; This keeps downstream record field/method access free of reflection.
   `(let [^io.randomseed.bankster.Registry r# (or *default* (deref R))]
      r#))
  ([registry]
   (if (true? registry)
     `(let [^io.randomseed.bankster.Registry r# (or *default* (deref R))]
        r#)
     `(let [^io.randomseed.bankster.Registry r# (or ~registry *default* (deref R))]
        r#))))

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
  (fn [ex-message ex-data]
    (let [^String msg (if (some? ex-data)
                        (str ex-message " " (pr-str ex-data))
                        (str ex-message))
          logger      (log-impl/get-logger log/*logger-factory* "io.randomseed.bankster.registry")]
      (log/log* logger :warn nil msg))))

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
  "Creates a new registry.

  Accepts base maps and builds derived index maps during initialization. Even the
  arity that accepts a Registry map/record ignores any derived index fields (they are
  recomputed during initialization)."
  {:tag Registry :added "1.0.0"}
  (^Registry []
   (bankster/->Registry (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-m) (h-r) (default-version) (h-m)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-id->traits
              ^clojure.lang.PersistentHashMap cur-id->weight
              cur-hierarchies
              ^String version]
   (let [cur-id->cur       (or cur-id->cur (h-m))
         ctr-id->cur       (or ctr-id->cur (h-m))
         cur-id->localized (or cur-id->localized (h-m))
         cur-id->traits    (or cur-id->traits (h-m))
         cur-id->weight    (or cur-id->weight (h-m))
         version           (or version (default-version))
         idx               (derive-indexes cur-id->cur ctr-id->cur cur-id->weight)]
     (bankster/->Registry (:cur-id->cur idx)
                          (:cur-nr->cur idx)
                          ctr-id->cur
                          (:cur-id->ctr-ids idx)
                          cur-id->localized
                          cur-id->traits
                          cur-id->weight
                          (:cur-code->curs idx)
                          (:cur-nr->curs idx)
                          (:cur-dom->curs idx)
                          (->currency-hierarchies cur-hierarchies)
                          version
                          (h-m))))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-id->traits
              ^clojure.lang.PersistentHashMap cur-id->weight
              cur-hierarchies]
   (new-registry cur-id->cur
                 ctr-id->cur
                 cur-id->localized
                 cur-id->traits
                 cur-id->weight
                 cur-hierarchies
                 (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap cur-id->cur
              ^clojure.lang.PersistentHashMap ctr-id->cur
              ^clojure.lang.PersistentHashMap cur-id->localized
              ^clojure.lang.PersistentHashMap cur-id->traits
              ^clojure.lang.PersistentHashMap cur-id->weight]
   (new-registry cur-id->cur
                 ctr-id->cur
                 cur-id->localized
                 cur-id->traits
                 cur-id->weight
                 (h-r)
                 (default-version)))
  (^Registry [^clojure.lang.PersistentHashMap m]
   (let [r   (bankster/map->Registry m)
         r   (-> r
                 (clojure.core/update :cur-id->cur       #(or % (h-m)))
                 (clojure.core/update :ctr-id->cur       #(or % (h-m)))
                 (clojure.core/update :cur-id->localized #(or % (h-m)))
                 (clojure.core/update :cur-id->traits    #(or % (h-m)))
                 (clojure.core/update :cur-id->weight    #(or % (h-m)))
                 (assoc  :hierarchies (->currency-hierarchies (clojure.core/get m :hierarchies)))
                 (clojure.core/update :version           #(or % (default-version)))
                 (clojure.core/update :ext               #(or % (h-m))))
         ^Registry base (new-registry (:cur-id->cur r)
                                      (:ctr-id->cur r)
                                      (:cur-id->localized r)
                                      (:cur-id->traits r)
                                      (:cur-id->weight r)
                                      (:hierarchies r)
                                      (:version r))]
     (assoc base :ext (:ext r)))))

(def ^{:tag      Registry
       :added    "1.0.0"
       :arglists '(^Registry []
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-id->traits
                              ^clojure.lang.PersistentHashMap cur-id->weight
                              ^CurrencyHierarchies            cur-hierarchies
                              ^String version]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-id->traits
                              ^clojure.lang.PersistentHashMap cur-id->weight
                              ^CurrencyHierarchies            cur-hierarchies]
                   ^Registry [^clojure.lang.PersistentHashMap cur-id->cur
                              ^clojure.lang.PersistentHashMap ctr-id->cur
                              ^clojure.lang.PersistentHashMap cur-id->localized
                              ^clojure.lang.PersistentHashMap cur-id->traits
                              ^clojure.lang.PersistentHashMap cur-id->weight]
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
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-id->cur r#)))
  ([registry] `(.cur-id->cur ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->cur ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro currency-nr->currency*
  "Returns the currency number to currency map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-nr->cur r#)))
  ([registry] `(.cur-nr->cur ^io.randomseed.bankster.Registry ~registry))
  ([nr registry] `(clojure.core/get (.cur-nr->cur ^io.randomseed.bankster.Registry ~registry) ~nr)))

(defmacro currency-nr->currencies*
  "Returns the currency number to currencies map from a registry. If the registry is
  not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-nr->curs r#)))
  ([registry] `(.cur-nr->curs ^io.randomseed.bankster.Registry ~registry))
  ([nr registry] `(clojure.core/get (.cur-nr->curs ^io.randomseed.bankster.Registry ~registry) ~nr)))

(defmacro currency-code->currencies*
  "Returns the currency short-code to currencies map from a registry. If the registry
  is not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-code->curs r#)))
  ([registry] `(.cur-code->curs ^io.randomseed.bankster.Registry ~registry))
  ([code registry] `(clojure.core/get (.cur-code->curs ^io.randomseed.bankster.Registry ~registry) ~code)))

(defmacro currency-domain->currencies*
  "Returns the currency domain to currencies map from a registry. If the registry is
  not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:added "2.1.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-dom->curs r#)))
  ([registry] `(.cur-dom->curs ^io.randomseed.bankster.Registry ~registry))
  ([domain registry] `(clojure.core/get (.cur-dom->curs ^io.randomseed.bankster.Registry ~registry) ~domain)))

(defmacro country-id->currency*
  "Returns the country ID to currency map from a registry. If the registry is not given
  the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If it is
  not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.ctr-id->cur r#)))
  ([registry] `(.ctr-id->cur ^io.randomseed.bankster.Registry ~registry))
  ([country registry] `(clojure.core/get (.ctr-id->cur ^io.randomseed.bankster.Registry ~registry) ~country)))

(defmacro currency-id->country-ids*
  "Returns the currency ID to country IDs map from a registry. If the registry is not
  given the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If
  it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-id->ctr-ids r#)))
  ([registry] `(.cur-id->ctr-ids ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->ctr-ids ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro currency-id->localized*
  "Returns the currency ID to localized properties map from a registry. If the registry
  is not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:added "1.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-id->localized r#)))
  ([registry] `(.cur-id->localized ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->localized ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro currency-id->traits*
  "Returns the currency ID to traits map from a registry. If the registry is not given
  the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If it is
  not set, current state of a global registry is used instead."
  {:added "2.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-id->traits r#)))
  ([registry] `(.cur-id->traits ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->traits ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro currency-id->weight*
  "Returns the currency ID to weight map from a registry. If the registry is not given
  the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If it is
  not set, current state of a global registry is used instead."
  {:added "2.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.cur-id->weight r#)))
  ([registry] `(.cur-id->weight ^io.randomseed.bankster.Registry ~registry))
  ([id registry] `(clojure.core/get (.cur-id->weight ^io.randomseed.bankster.Registry ~registry) ~id)))

(defmacro hierarchies*
  "Returns hierarchies map of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead.

  When `k` is given the macro will extract a specific hierarchy from a record
  field. it should be a simple keyword. If it is a constant form of a keyword
  field-access byte code will be generated."
  {:added "2.0.0"}
  ([]
   `(let [^io.randomseed.bankster.Registry r# (get)]
      (.hierarchies r#)))
  ([registry]
   `^CurrencyHierarchies (.hierarchies ^io.randomseed.bankster.Registry ~registry))
  ([k registry]
   (if (and (simple-keyword? k)
            (contains? (set (keys (bankster/map->CurrencyHierarchies {}))) k))
     (let [k' (symbol (str "." (clojure.lang.Compiler/munge (name k))))]
       `(~k'
         ^io.randomseed.bankster.CurrencyHierarchies
         (.hierarchies ^io.randomseed.bankster.Registry ~registry)))
     `(clojure.core/get (.hierarchies ^io.randomseed.bankster.Registry ~registry) ~k))))

(defmacro hierarchy*
  "Returns a hierarchy identified by the given key `k` in a `registry`. If the registry
  is not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead.

  When `k` is given it should be a simple keyword. If it is a constant form of a
  keyword field-access byte code will be generated."
  {:added "2.0.0"}
  ([k] `(hierarchies* ~k (get)))
  ([k registry] `(hierarchies* ~k ~registry)))

(defmacro ext*
  "Returns extra data map of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:added "2.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.ext r#)))
  ([registry] `(.ext ^io.randomseed.bankster.Registry ~registry))
  ([k registry] `(clojure.core/get (.ext ^io.randomseed.bankster.Registry ~registry) ~k)))

(defmacro version*
  "Returns a version string of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:added "2.0.0"}
  ([] `(let [^io.randomseed.bankster.Registry r# (get)]
         (.version r#)))
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

(defn currency-domain->currencies
  "Returns the currency domain to currencies map from a registry. If the registry is
  not given the dynamic variable `io.randomseed.bankster.registry/*default*` is
  tried. If it is not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.1.0"}
  (^clojure.lang.PersistentHashMap [] (currency-domain->currencies*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-domain->currencies* registry))
  (^clojure.lang.PersistentHashMap [domain ^Registry registry] (currency-domain->currencies* domain registry)))

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

(defn currency-id->traits
  "Returns the currency ID to traits map from a registry. If the registry is not given
  the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If it is
  not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-id->traits*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-id->traits* registry))
  (^clojure.lang.PersistentHashMap [id ^Registry registry] (currency-id->traits* id registry)))

(defn currency-id->weight
  "Returns the currency ID to weight map from a registry. If the registry is not given
  the dynamic variable `io.randomseed.bankster.registry/*default*` is tried. If it is
  not set, current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (currency-id->weight*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (currency-id->weight* registry))
  (^clojure.lang.PersistentHashMap [id ^Registry registry] (currency-id->weight* id registry)))

(defn hierarchies
  "Returns hierarchies map of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:tag CurrencyHierarchies :added "2.0.0"}
  (^CurrencyHierarchies [] (hierarchies*))
  (^CurrencyHierarchies [registry] (hierarchies* (get registry)))
  (^CurrencyHierarchies [k registry] (hierarchies* k (get registry))))

(defn hierarchy
  "Returns a hierarchy identified by the given key `k` in a `registry`. The key should
  be a keyword. If the registry is not given the dynamic variable
  `io.randomseed.bankster.registry/*default*` is tried. If it is not set, current
  state of a global registry is used instead.

  For static keywords it is advised to use `hierarchy*` macro whenever possible as it
  compiles to a field-access byte code."
  {:tag clojure.lang.Associative :added "2.0.0"}
  ([k] (hierarchy* k))
  ([k registry] (hierarchy* k (get registry))))

(defn- hierarchy-derive*
  {:tag Registry :added "2.0.0" :private true}
  [^Registry registry hierarchy-name tag parent]
  (let [parents (cond
                  (set? parent)        (sort-by str parent)
                  (sequential? parent) parent
                  :else                (list parent))]
    (update-in registry
               [:hierarchies hierarchy-name]
               (fn [h]
                 (reduce (fn [h p]
                           (derive (or h (make-hierarchy)) tag p))
                         h
                         parents)))))

(defn hierarchy-derive
  "Returns `registry` updated by deriving `tag` from `parent` inside a hierarchy
  identified by `hierarchy-name`."
  {:tag Registry :added "2.0.0"}
  [hierarchy-name tag parent registry]
  (update (get registry) hierarchy-derive* hierarchy-name tag parent))

(defn hierarchy-derive!
  "Updates global registry by deriving `tag` from `parent` inside a hierarchy
  identified by `hierarchy-name`."
  {:tag Registry :added "2.0.0"}
  [hierarchy-name tag parent]
  (update! hierarchy-derive* hierarchy-name tag parent))

(defn ext
  "Returns extra data map of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^clojure.lang.PersistentHashMap [] (ext*))
  (^clojure.lang.PersistentHashMap [^Registry registry] (ext* registry))
  (^clojure.lang.PersistentHashMap [k ^Registry registry] (ext* k registry)))

(defn version
  "Returns a version string of a registry. If the registry is not given the dynamic
  variable `io.randomseed.bankster.registry/*default*` is tried. If it is not set,
  current state of a global registry is used instead."
  {:tag clojure.lang.PersistentHashMap :added "2.0.0"}
  (^String [] (version*))
  (^String [^Registry registry] (version* registry)))

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
