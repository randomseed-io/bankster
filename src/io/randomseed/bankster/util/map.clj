(ns io.randomseed.bankster.util.map)

(defmacro lazy-get
  "Like get but the default value is not evaluated if the key is found."
  {:added "1.0.0"}
  [m k exp]
  `(let [m# ~m, k# ~k]
     (if (and (associative? m#) (contains? m# k#)) (get m# k#) ~exp)))

(defn update-existing
  "Updates the key k of the given collection coll by calling a function fun and passing
  optional arguments specified as additional arguments. Will not perform any update
  if the given key does not exist within the collection. Returns a collection."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ^clojure.lang.Associative [^clojure.lang.Associative coll k fun & more]
  (if (contains? coll k)
    (let [fun (if (fn? fun) fun (constantly fun))]
      (apply update coll k fun more))
    coll))

(defn update-missing
  {:added "1.0.0" :tag clojure.lang.Associative}
  ^clojure.lang.Associative [coll k fun & more]
  (if-not (contains? coll k)
    (let [fun (if (fn? fun) (fn [& args] (apply fun (next args))) (constantly fun))]
      (apply update coll k fun more))
    coll))

(defmacro assoc-if
  {:added "1.0.0"}
  ([coll pred k val]
   `(let [kol# ~coll] (if ~pred (assoc kol# ~k ~val) kol#))))

(defmacro assoc-if-not
  {:added "1.0.0"}
  ([coll pred k val]
   `(let [kol# ~coll] (if ~pred kol# (assoc kol# ~k ~val)))))

(defmacro assoc-if-key
  {:added "1.0.0"}
  ([coll k pred val]
   `(let [kol# ~coll key# ~k]
      (if (~pred (get kol# key#)) (assoc kol# key# ~val) kol#))))

(defmacro assoc-if-not-key
  {:added "1.0.0"}
  [coll k pred val]
  `(let [kol# ~coll key# ~k]
     (if (~pred (get kol# key#)) kol# (assoc kol# key# ~val))))

(defn remove-if-value
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ^clojure.lang.Associative [^clojure.lang.Associative m
                                ^clojure.lang.IFn pred]
  (reduce-kv
   (fn [^clojure.lang.Associative mp k v]
     (if (pred v) (dissoc mp k) mp))
   m m))

(defn remove-if-value-in
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m vals]
  (let [vset (set vals)]
    (remove-if-value m #(contains? vset %))))

(defn remove-if-value-not-in
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m vals]
  (let [vset          (set vals)
        not-contains? (complement contains?)]
    (remove-if-value m #(not-contains? vset %))))

(defn remove-except
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m ^clojure.lang.ISeq keyseq]
  (select-keys m keyseq))

(defn remove-by-if-value-in
  "Removes map entries if the given predicate returns true and value is in the given
  set."
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m
   ^clojure.lang.IFn pred
   ^clojure.lang.PersistentHashSet only]
  (reduce #(if (pred (get %1 %2)) (dissoc %1 %2) %1) m only))

(defn remove-empty-values
  "Removes entries with empty values from a map."
  {:added "1.0.0" :tag clojure.lang.Associative}
  (^clojure.lang.Associative [^clojure.lang.Associative m]
   (remove-if-value m #(or (nil? %)
                           (and (counted? %) (< (count %) 1))
                           (and (seqable? %) (nil? (seq %))))))
  (^clojure.lang.Associative [^clojure.lang.Associative m
                              ^clojure.lang.PersistentHashSet only]
   (remove-by-if-value-in m #(or (nil? %)
                                 (and (counted? %) (< (count %) 1))
                                 (and (seqable? %) (nil? (seq %)))) only)))

(defn map-vals-by-k
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with
  values updated by results returned by the function. When the third argument is
  given it should be a map on which operations are performed instead of using the
  original map. This may be helpful when we want to avoid merging the results with
  another map."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m]
   (map-vals-by-k f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m
    ^clojure.lang.Associative dst]
   (reduce-kv
    (fn [^clojure.lang.Associative mp k _v] (assoc mp k (f k)))
    dst m)))

(defn map-vals-by-kv
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys and values during calls to it) and generates a
  map with values updated by results returned by the function. When the third
  argument is given it should be a map on which operations are performed instead of
  using the original map. This may be helpful when we want to avoid merging the
  results with another map."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m]
   (map-vals-by-kv f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m
    ^clojure.lang.Associative dst]
   (reduce-kv
    (fn [^clojure.lang.Associative mp k v] (assoc mp k (f k v)))
    dst m)))

(defn map-vals
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive values during calls to it) and generates a map with
  values updated by results returned by the function. When the third argument is
  given it should be a map on which operations are performed instead of using the
  original map. This may be helpful when we want to avoid merging the results with
  another map."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m]
   (map-vals f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m
    ^clojure.lang.Associative dst]
   (reduce-kv
    (fn [^clojure.lang.Associative mp k v] (assoc mp k (f v)))
    dst m)))

(defn map-keys-by-v
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive values during calls to it) and generates a map with
  keys updated by results returned by the function. When the third argument is
  given then it should be a map on which operations are performed instead of using
  an empty map."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m]
   (map-keys-by-v f m {}))
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m
    ^clojure.lang.Associative dst]
   (reduce-kv
    (fn [^clojure.lang.Associative mp _k v] (assoc mp (f v) v))
    dst m)))

(defn map-keys
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with keys
  updated by results returned by the function. When the third argument is given then
  it should be a map on which operations are performed instead of using an empty
  map."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m]
   (map-keys f m {}))
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m
    ^clojure.lang.Associative dst]
   (reduce-kv
    (fn [^clojure.lang.Associative mp k v] (assoc mp (f k) v))
    dst m)))

(defn map-keys-and-vals
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with keys
  updated by results returned by the function and values also updated by results of
  the same function. The function should return a sequential collection of 2
  elements: first containing a new value of a key and second containing a new value
  of a transformed value associated with that key. When the third argument is given
  then it should be a map on which operations are performed instead of using an empty
  map."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m]
   (map-keys-and-vals f m {}))
  ([^clojure.lang.IFn f
    ^clojure.lang.Associative m
    ^clojure.lang.Associative dst]
   (reduce-kv
    (fn [^clojure.lang.Associative mp k v]
      (let [[new-k new-v] (f k v)] (assoc mp new-k new-v)))
    dst m)))

(defn map-of-sets-invert
  "Like `clojure.set/map-invert` but for map of sets (as values) to preserve all
  possible values (as keys of newly created map)."
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m]
  (reduce (fn [^clojure.lang.Associative am [k v]]
            (assoc am k (conj (am k (hash-set)) v)))
          (hash-map)
          (for [[k st] m v st] [v k])))

(defn invert-in-sets
  "Like `clojure.set/map-invert` but preserves all possible values in sets."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.Associative m]
   (invert-in-sets m #{}))
  ([^clojure.lang.Associative m ^clojure.lang.PersistentHashSet dst]
   (persistent!
    (reduce (fn [am [k v]]
              (assoc! am v (conj (am v dst) k)))
            (transient {}) m))))

(defn map-of-vectors-invert-flatten
  "Like `clojure.set/map-invert` but for map of vectors (as values). Duplicated keys
  are replaced."
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m]
  (->> (mapcat (fn [[k v]] (interleave v (repeat k))) m)
       (partition 2)
       (map vec)
       (into {})))

(defn map-values
  "Recursively transforms values of a coll using function f. The function should take a
  value and return a new value."
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.IFn f, ^clojure.lang.Associative coll]
  (reduce-kv (fn [^clojure.lang.Associative m, ^clojure.lang.Keyword k, v]
               (assoc m k (if (map? v) (map-values f v) (f v))))
             (empty coll) coll))

(defn update-values
  "Returns the given map with its values identified with keys from vmap updated with
  the associated functions from vmap."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.Associative map
    ^clojure.lang.Associative vmap]
   (update-values map vmap false))
  ([^clojure.lang.Associative map
    ^clojure.lang.Associative vmap
    ^Boolean create-keys?]
   (if create-keys?
     (reduce-kv
      (fn [^clojure.lang.Associative mp k v]
        (update mp k (if (fn? v) v (constantly v))))
      map vmap)
     (reduce-kv
      (fn [^clojure.lang.Associative mp k v]
        (if (contains? mp k) (update mp k (if (fn? v) v (constantly v))) mp))
      map vmap))))

(defn update-values-recur
  "Returns the given map with its values identified with keys from vmap recursively
  updated with the associated functions from vmap. Shape is not reflected, second
  map (vmap) should be flat, searching for keys is recursive, including nested
  vectors."
  {:added "1.0.0" :tag clojure.lang.Associative}
  ([^clojure.lang.Associative map
    ^clojure.lang.Associative vmap]
   (update-values-recur map vmap false))
  ([^clojure.lang.Associative map
    ^clojure.lang.Associative vmap
    ^Boolean create-keys?]
   (reduce-kv
    (fn [^clojure.lang.Associative mp k v]
      (if (or (map? v) (vector? v))
        (assoc mp k (update-values-recur v vmap create-keys?))
        mp))
    (if (vector? map) map
        (update-values map vmap create-keys?)) map)))

(defn dissoc-in
  "Like assoc-in but removes entries. Leaves empty maps."
  {:added "1.0.0" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m [k & ks :as _keys]]
  (if ks
    (if-some [nmap (get m k)]
      (assoc m k (dissoc-in nmap ks))
      m)
    (dissoc m k)))

(defn remove-keys-ns
  "Removes namespace component from qualified keys (keywords and
  symbols). Non-qualified identifiers and other data types are not renamed."
  {:added "1.0.2" :tag clojure.lang.Associative}
  [^clojure.lang.Associative m]
  (map-keys m (fn [k] (if (qualified-ident? k) (name k) k))))
