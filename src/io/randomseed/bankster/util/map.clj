(ns io.randomseed.bankster.util.map

  (:require [io.randomseed.bankster.util :refer :all]))

(defmacro lazy-get
  [m k exp]
  `(let [m# ~m, k# ~k]
     (if (and (associative? m#) (contains? m# k#)) (get m# k#) ~exp)))

(defn update-existing
  "Updates the key k of the given collection coll by calling a function fun and passing
  optional arguments specified as additional arguments. Will not perform any update
  if the given key does not exist within the collection. Returns a collection."
  [^clojure.lang.IPersistentMap coll k fun & more]
  (if (contains? coll k)
    (let [fun (if (fn? fun) fun (constantly fun))]
      (apply update coll k fun more))
    coll))

(defn update-missing
  [coll k fun & more]
  (if-not (contains? coll k)
    (let [fun (if (fn? fun) (fn [& args] (apply fun (next args))) (constantly fun))]
      (apply update coll k fun more))
    coll))

(defmacro assoc-if
  ([coll pred k val]
   `(let [kol# ~coll] (if ~pred (assoc kol# ~k ~val) kol#))))

(defmacro assoc-if-not
  ([coll pred k val]
   `(let [kol# ~coll] (if ~pred kol# (assoc kol# ~k ~val)))))

(defmacro assoc-if-key
  ([coll k pred val]
   `(let [kol# ~coll key# ~k]
      (if (~pred (get kol# key#)) (assoc kol# key# ~val) kol#))))

(defmacro assoc-if-not-key
  [coll k pred val]
  `(let [kol# ~coll key# ~k]
     (if (~pred (get kol# key#)) kol# (assoc kol# key# ~val))))

(defn ^clojure.lang.IPersistentMap remove-if-value
  [^clojure.lang.IPersistentMap m
   ^clojure.lang.IFn pred]
  (reduce-kv
   (fn [^clojure.lang.IPersistentMap mp k v]
     (if (pred v) (dissoc mp k) mp))
   m m))

(defn ^clojure.lang.IPersistentMap remove-if-value-in
  [^clojure.lang.IPersistentMap m vals]
  (let [vset (set vals)]
    (remove-if-value m #(contains? vset %))))

(defn ^clojure.lang.IPersistentMap remove-if-value-not-in
  [^clojure.lang.IPersistentMap m vals]
  (let [vset (set vals)
        not-contains? (complement contains?)]
    (remove-if-value m #(not-contains? vset %))))

(defn ^clojure.lang.IPersistentMap remove-except
  [^clojure.lang.IPersistentMap m ^clojure.lang.ISeq keyseq]
  (select-keys m keyseq))

(defn ^clojure.lang.IPersistentMap remove-by-if-value-in
  [^clojure.lang.IPersistentMap m
   ^clojure.lang.IFn pred
   ^clojure.lang.PersistentHashSet only]
  (reduce #(if (pred (get %1 %2)) (dissoc %1 %2) %1) m only))

(defn ^clojure.lang.IPersistentMap remove-empty-values
  [^clojure.lang.IPersistentMap m]
  (remove-if-value
   m #(or (nil? %) (and (seqable? %) (nil? (seq %))))))

(defn fmap-k
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with
  values updated by the results returned by the function. When the third argument is
  given it should be a map on which operations are performed instead of using the
  original map. This may be helpful when we want to avoid merging the results with
  another map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (fmap-k f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (reduce-kv
    (fn [^clojure.lang.IPersistentMap mp k v] (assoc mp k (f k)))
    dst m)))

(defn fmap-kv
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys and values during calls to it) and generates a
  map with values updated by the results returned by the function. When the third
  argument is given it should be a map on which operations are performed instead of
  using the original map. This may be helpful when we want to avoid merging the
  results with another map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (fmap-k f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (reduce-kv
    (fn [^clojure.lang.IPersistentMap mp k v] (assoc mp k (f k v)))
    dst m)))

(defn fmap-v
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive values during calls to it) and generates a map with
  values updated by the results returned by the function. When the third argument is
  given it should be a map on which operations are performed instead of using the
  original map. This may be helpful when we want to avoid merging the results with
  another map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (fmap-v f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (reduce-kv
    (fn [^clojure.lang.IPersistentMap mp k v] (assoc mp k (f v)))
    dst m)))

(defn kmap-v
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with
  keys updated by the results returned by the function. When the third argument is
  given it should be a map on which operations are performed instead of using the
  original map. This may be helpful when we want to avoid merging the results with
  another map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (kmap-v f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (reduce-kv
    (fn [^clojure.lang.IPersistentMap mp k v] (assoc mp (f v) v))
    dst m)))

(defn map-of-sets-invert
  "Like `clojure.set/map-invert` but for map of sets (as values) to preserve all
  possible values (as keys of newly created map)."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  [^clojure.lang.IPersistentMap m]
  (reduce (fn [^clojure.lang.IPersistentMap am [k v]]
            (assoc am k (conj (am k (hash-set)) v)))
          (hash-map)
          (for [[k st] m v st] [v k])))

(defn invert-in-sets
  "Like `clojure.set/map-invert` but preserves all possible values in sets."
  ([^clojure.lang.IPersistentMap m]
   (invert-in-sets m #{}))
  ([^clojure.lang.IPersistentMap m ^clojure.lang.PersistentHashSet dst]
   (persistent!
    (reduce (fn [am [k v]]
              (assoc! am v (conj (am v dst) k)))
            (transient {}) m))))

(defn map-of-vectors-invert-flatten
  "Like `clojure.set/map-invert` but for map of vectors (as values). Duplicated keys
  are replaced."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  [^clojure.lang.IPersistentMap m]
  (->> (mapcat (fn [[k v]] (interleave v (repeat k))) m)
       (partition 2)
       (map vec)
       (into {})))

(defn map-values
  "Recursively transforms values of a coll using function f. The function should take a
  value and return new value."
  [^clojure.lang.IFn f, ^clojure.lang.IPersistentMap coll]
  (reduce-kv (fn [^clojure.lang.IPersistentMap m, ^clojure.lang.Keyword k, v]
               (assoc m k (if (map? v) (map-values f v) (f v))))
             (empty coll) coll))

(defn update-values
  "Returns the map with its values identified with keys from vmap updated with the
  associated functions from vmap."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap]
   (update-values map vmap false))
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap
    ^Boolean create-keys?]
   (if create-keys?
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v]
        (update mp k (if (fn? v) v (constantly v))))
      map vmap)
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v]
        (if (contains? mp k) (update mp k (if (fn? v) v (constantly v))) mp))
      map vmap))))

(defn update-values-recur
  "Returns the map with its values identified with keys from vmap recursively updated
  with the associated functions from vmap. Shape is not reflected, second map (vmap)
  should be flat, searching for keys is recursive, including nested vectors."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap]
   (update-values-recur map vmap false))
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap
    ^Boolean create-keys?]
   (reduce-kv
    (fn [^clojure.lang.IPersistentMap mp k v]
      (if (or (map? v) (vector? v))
        (assoc mp k (update-values-recur v vmap create-keys?))
        mp))
    (if (vector? map) map
        (update-values map vmap create-keys?)) map)))

(defn ^clojure.lang.IPersistentMap dissoc-in
  "Like assoc-in but removes entries. Leaves empty maps."
  [^clojure.lang.IPersistentMap m [k & ks :as keys]]
  (if ks
    (if-some [nmap (get m k)]
      (assoc m k (dissoc-in nmap ks))
      m)
    (dissoc m k)))
