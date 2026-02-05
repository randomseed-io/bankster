(ns

    ^{:doc    "Support functions and macros."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.util

  (:require    [clojure.string            :as      str]
               [clojure.spec.alpha        :as        s]))

(s/def ::set set?)
(s/def ::map map?)
(s/def ::number number?)
(s/def ::integer int?)
(s/def ::natural nat-int?)
(s/def ::positive pos-int?)
(s/def ::bytes bytes?)
(s/def ::callable ifn?)
(s/def ::vector vector?)
(s/def ::identifier ident?)
(s/def ::not-empty-string (s/and string? not-empty))
(s/def ::simple-identifier simple-ident?)
(s/def ::not-conflicting-ns (fn [[ns id]] (or (simple-ident? id) (= ns (namespace id)))))

(defn must-have-ns
  [id ^String ns]
  (s/assert ::not-empty-string ns)
  (s/assert ::identifier id)
  (if (and (qualified-ident? id) (= (namespace id) ns))
    id ((if (keyword? id) keyword symbol) ns (name id))))

(defn ensure-keyword
  [id]
  (if (keyword? id)
    id
    (if (ident? id)
      (if (simple-ident? id)
        (keyword id)
        (keyword (namespace id) (name id)))
      (keyword (str id)))))

(defn ensure-keyword-having-ns
  [id ^String ns]
  (must-have-ns (ensure-keyword id) ns))

(defmacro ^:no-doc defalias [name target]
  (let [v              (clojure.core/resolve target)
        m0             (meta v)
        arglists       (:arglists m0)
        primitive-tags {'long    Long/TYPE
                        'int     Integer/TYPE
                        'double  Double/TYPE
                        'float   Float/TYPE
                        'short   Short/TYPE
                        'byte    Byte/TYPE
                        'boolean Boolean/TYPE
                        'char    Character/TYPE}
        tag0           (:tag m0)
        tag            (or (when (symbol? tag0) (get primitive-tags tag0 tag0))
                           (when (class? tag0)  tag0)
                           (when (var? tag0)
                             (let [s (-> tag0 meta :name symbol)]
                               (get primitive-tags s s))))
        m              (-> m0
                           (dissoc :ns :name :file :line :column :tag)
                           (cond-> tag (assoc :tag tag))
                           (assoc :added "2.2.0"
                                  :auto-alias true
                                  :cloverage/ignore true)
                           (cond-> arglists (assoc :arglists (list 'quote arglists))))]
    (if (:macro m0)
      `(def ~(with-meta name m)
         (deref (var ~target)))
      `(def ~(with-meta name m)
         ~target))))

(defn- simple-arglist?
  [args]
  (and (vector? args)
       (every? symbol? args)
       (let [amp-idx (.indexOf ^clojure.lang.APersistentVector args '&)]
         (or (neg? amp-idx)
             (and (< amp-idx (dec (count args)))
                  (= amp-idx (- (count args) 2)))))))

(defn- registry-arglist?
  [args]
  (some #{'registry} (remove #{'&} args)))

(defn- arglist-varargs?
  [args]
  (some #{'&} args))

(defn- fixed-arity
  [args]
  (let [argsv   (vec args)
        amp-idx (.indexOf ^clojure.lang.APersistentVector argsv '&)]
    (if (neg? amp-idx) (count argsv) amp-idx)))

(defn- distinct-arities?
  [arglists]
  (let [arglists     (map vec arglists)
        variadics    (filter arglist-varargs? arglists)
        fixed-args   (remove arglist-varargs? arglists)
        fixed-counts (map fixed-arity fixed-args)]
    (and (<= (count variadics) 1)
         (= (count fixed-counts) (count (distinct fixed-counts))))))

(defn- qualify-tag
  [tag ns-obj primitive-tags]
  (cond
    (nil? tag) nil
    (symbol? tag)
    (or (get primitive-tags tag)
        (when (namespace tag) tag)
        (let [resolved (ns-resolve ns-obj tag)]
          (cond
            (class? resolved) resolved
            (var? resolved)   (let [vtag (-> resolved meta :tag)]
                                (or (get primitive-tags vtag) vtag))
            :else             tag)))
    :else tag))

(defn- qualify-arglist
  [args ns-obj primitive-tags]
  (mapv (fn [arg]
          (if (symbol? arg)
            (let [m   (meta arg)
                  tag (qualify-tag (:tag m) ns-obj primitive-tags)]
              (if (and m (contains? m :tag))
                (with-meta arg (assoc m :tag tag))
                arg))
            arg))
        args))

(defmacro ^:no-doc defalias-reg [name target]
  (let [v              (clojure.core/resolve target)
        m0             (meta v)
        arglists       (:arglists m0)
        has-registry?  (and (sequential? arglists)
                            (some registry-arglist? arglists))
        doc0           (:doc m0)
        doc            (when (and has-registry? (string? doc0))
                         (if (re-find #"registry` is `true`" doc0)
                           doc0
                           (str doc0
                                "\n\nWhen `registry` is `true`, the default registry "
                                "(global or dynamically bound) is used.")))
        primitive-tags {'long    Long/TYPE
                        'int     Integer/TYPE
                        'double  Double/TYPE
                        'float   Float/TYPE
                        'short   Short/TYPE
                        'byte    Byte/TYPE
                        'boolean Boolean/TYPE
                        'char    Character/TYPE}
        ns-obj         (:ns m0)
        tag0           (:tag m0)
        tag            (or (when (symbol? tag0) (get primitive-tags tag0 tag0))
                           (when (class? tag0)  tag0)
                           (when (var? tag0)
                             (let [s (-> tag0 meta :name symbol)]
                               (get primitive-tags s s))))
        arglists*      (when (seq arglists)
                         (mapv #(qualify-arglist % ns-obj primitive-tags) arglists))
        m              (-> m0
                           (dissoc :ns :name :file :line :column :tag)
                           (cond-> tag (assoc :tag tag))
                           (assoc :added "2.2.0"
                                  :auto-alias true
                                  :cloverage/ignore true)
                           (cond-> doc (assoc :doc doc))
                           (cond-> arglists (assoc :arglists (list 'quote arglists))))]
    (when (:macro m0)
      (throw (ex-info "defalias-reg does not support macros" {:target target})))
    (when (seq arglists*)
      (doseq [args arglists*]
        (when-not (simple-arglist? (vec args))
          (throw (ex-info "defalias-reg requires simple arglists" {:target target :args args})))))
    (when-not (distinct-arities? arglists*)
      (throw (ex-info "defalias-reg requires distinct non-variadic arities"
                      {:target target :arglists arglists})))
    (if (seq arglists*)
      `(defn ~(with-meta name m)
         ~@(for [args arglists*]
             (let [argsv   (vec args)
                   amp-idx (.indexOf ^clojure.lang.APersistentVector argsv '&)
                   fixed   (if (neg? amp-idx) argsv (subvec argsv 0 amp-idx))
                   rest-s  (when (>= amp-idx 0) (nth argsv (inc amp-idx)))
                   has-reg (registry-arglist? fixed)
                   call    (if rest-s
                             (if (seq fixed)
                               `(apply ~target ~@fixed ~rest-s)
                               `(apply ~target ~rest-s))
                             `(~target ~@fixed))]
               (if has-reg
                 `(~argsv
                   (let [~'registry (if (true? ~'registry)
                                      (or io.randomseed.bankster.registry/*default*
                                          (io.randomseed.bankster.registry/state))
                                      ~'registry)]
                     ~call))
                 `(~argsv ~call)))))
      `(def ~(with-meta name m)
         ~target))))

(defmacro auto-alias
  "Creates aliases in the current namespace for all Vars in `source-ns` that have
  truthy `:auto-alias` metadata, skipping symbols already defined."
  {:added "2.2.0" :auto-alias true}
  [source-ns]
  (let [ns-sym  (cond
                  (symbol? source-ns)
                  source-ns

                  (and (seq? source-ns)
                       (= 'quote (first source-ns))
                       (= 2 (count source-ns))
                       (symbol? (second source-ns)))
                  (second source-ns)

                  (keyword? source-ns)
                  (symbol (name source-ns))

                  :else
                  (symbol (str source-ns)))]
    (clojure.core/require ns-sym)
    (let [vars    (->> (ns-interns (the-ns ns-sym))
                       (filter (fn [[_ v]] (:auto-alias (meta v)))))
          existing (ns-interns *ns*)
          aliases (->> vars
                       (sort-by (comp str first))
                       (remove #(contains? existing (first %)))
                       (map (fn [[sym v]]
                              (let [m        (meta v)
                                    arglists (:arglists m)
                                    arglists (when (and (sequential? arglists)
                                                        (every? sequential? arglists))
                                               (mapv vec arglists))
                                    use-reg? (and (not (:macro m))
                                                  (seq arglists)
                                                  (every? simple-arglist? arglists)
                                                  (distinct-arities? arglists)
                                                  (some registry-arglist? arglists))
                                    alias-sym (symbol "io.randomseed.bankster.util"
                                                      (if use-reg? "defalias-reg" "defalias"))]
                                `(~alias-sym
                                  ~sym
                                  ~(symbol (str ns-sym) (name sym))))))
                       (vec))]
      (cond
        (empty? aliases) nil
        (= 1 (count aliases)) (first aliases)
        :else `(do ~@aliases)))))

(defmacro try-null
  "Evaluates body and if a NullPointerException is caught it returns nil. Otherwise
  it returns the value of the last expression in the body."
  {:added "1.0.0"}
  [& body]
  `(try ~@body
        (catch NullPointerException  e# nil)))

(defmacro when-not-empty
  "Evaluates body when the given value is a non-empty collection."
  {:added "1.0.0"}
  [val & body]
  `(when (seq ~val)
     ~@body))

(defn with-not-empty
  "Returns the collection if it's not empty. Otherwise returns `nil`."
  {:added "1.0.0"}
  [obj]
  (when (seq obj) obj))

(defn remove-from-set-where
  "Returns a set with all elements matching `pred` removed.

  Returns `nil` when `s` is `nil` or when the resulting set is empty."
  {:tag clojure.lang.IPersistentSet :added "2.0.0"}
  [^clojure.lang.IFn pred ^clojure.lang.IPersistentSet s]
  (when (some? s)
    (not-empty
     (if (instance? clojure.lang.IEditableCollection s)
       (persistent!
        (reduce (fn [ts x] (if (pred x) (disj! ts x) ts))
                (transient s)
                s))
       (reduce (fn [acc x] (if (pred x) (disj acc x) acc)) s s)))))

(defn keep-in-set-where
  "Returns a set with all elements *not* matching `pred` removed.

  Returns `nil` when `s` is `nil` or when the resulting set is empty."
  {:tag clojure.lang.IPersistentSet :added "2.0.0"}
  [^clojure.lang.IFn pred ^clojure.lang.IPersistentSet s]
  (when (some? s)
    (not-empty
     (if (instance? clojure.lang.IEditableCollection s)
       (persistent!
        (reduce (fn [ts x] (if (pred x) ts (disj! ts x)))
                (transient s)
                s))
       (reduce (fn [acc x] (if (pred x) acc (disj acc x))) s s)))))

(defn split-on-first-slash
  "Splits a string on the first slash character ('/').

  Returns a vector of two strings: `[before after]`. When `s` is `nil` or empty,
  returns `[nil nil]`. When there is no slash, returns `[s nil]`. Uses Java
  `indexOf` and `substring`."
  {:added "2.0.0"}
  [^String s]
  (if (and (some? s) (not (.isEmpty ^String s)))
    (let [i (unchecked-int (.indexOf ^String s (int \/)))]
      (if (neg? i)
        [s nil]
        [(not-empty ^String (.substring ^String s 0 i))
         (not-empty ^String (.substring ^String s (unchecked-inc-int i)))]))
    [nil nil]))

(defmacro is
  [pred val & body]
  `(let [v# ~val]
     (if (~pred v#) ~@body v#)))

(defmacro is-not
  [pred val & body]
  `(let [v# ~val]
     (if (~pred v#) v# ~@body)))

(defn count-digits
  {:added "1.0.0"}
  ^long [^long n]
  (if (zero? n) 1
      (unchecked-inc
       (long (Math/floor (Math/log10 n))))))

(defn ns-infer
  "Takes a string of namespace name and a keyword. If the given keyword is not
  namespace-qualified it returns a new keyword with the given namespace added. If the
  given keyword is already equipped with a namespace it returns it."
  {:added "1.0.0" :tag clojure.lang.Keyword}
  ([^String ns-name
    ^clojure.lang.Keyword k]
   (if (simple-keyword? k)
     (keyword ns-name (name k))
     k))
  ([^String ns-name
    ^clojure.lang.Keyword k
    ^Boolean use-infer]
   (if use-infer (ns-infer ns-name k) k)))

(defn inferred-contains?
  "Just like the contains? but if the keyword is namespace-qualified it also checks if
  the collection contains the same keyword as its key but without a namespace."
  {:added "1.0.0" :tag Boolean}
  [^clojure.lang.IPersistentMap coll
   ^clojure.lang.Keyword k]
  (or (contains? coll k)
      (if (simple-keyword? k)
        false
        (contains? coll (keyword (name k))))))

(defn inferred-get
  "Just like the get function but if the keyword is namespace-qualified it first
  attempts to look for the value associated with it. If that fails it uses the
  variant of the keyword without any namespace."
  {:added "1.0.0"}
  ([^clojure.lang.IPersistentMap coll
    ^clojure.lang.Keyword k]
   (inferred-get coll k nil))
  ([^clojure.lang.IPersistentMap coll
    ^clojure.lang.Keyword k
    default]
   (if (simple-keyword? k)
     (k coll default)
     ((if (contains? coll k) k (keyword (name k))) coll default))))

(defn replace-in-set
  [^clojure.lang.PersistentHashSet s old-val new-val]
  (s/assert ::set s)
  (conj (disj s old-val) new-val))

(defn current-thread-id
  []
  (let [^Thread t (Thread/currentThread)]
    (try
      ;; Java 19+: Thread.threadId()
      (long (.invoke (.getMethod Thread "threadId" (make-array Class 0))
                     t (object-array 0)))
      (catch Throwable _
        ;; Older Java: Thread.getId()
        (long (.invoke (.getMethod Thread "getId" (make-array Class 0))
                       t (object-array 0)))))))
(defn current-thread-name [] (.. Thread currentThread getName))
(defn current-thread      [] (Thread/currentThread))

(defn get-rand-int
  "Like rand-int but optionally uses random number generator."
  {:tag "int" :added "1.0.0"}
  ([^long n]
   (when (some? n)
     (int (rand-int n))))
  ([^long n
    ^java.util.Random rng]
   (when (some? n)
     (if (nil? rng)
       (int (get-rand-int n))
       (if (zero? n) (int n) (int (.nextInt ^java.util.Random rng n)))))))

(defn random-digits-len
  "For 0 or 1 it returns its argument. For other positive numbers it returns a random
  natural number from 1 to this number (inclusive) in 50% cases. In other 50% cases
  it returns its argument."
  {:added "1.0.0" :tag "long"}
  (^long [^long x
          ^long iteration
          ^Boolean shrink-now]
   (when (some? x)
     (if (zero? x) x
         (if-not shrink-now x
                 (if (zero? iteration) 1
                     (if (or (< iteration 6) (zero? (rand-int 2)))
                       (unchecked-inc (rand-int x)) x))))))
  (^long [^long x
          ^long iteration
          ^Boolean shrink-now
          ^java.util.Random rng]
   (when (some? x)
     (if (nil? rng)
       (random-digits-len x iteration shrink-now)
       (if (zero? x) x
           (if-not shrink-now x
                   (if (zero? iteration) 1
                       (if (or (< iteration 6) (zero? (get-rand-int 2 rng)))
                         (unchecked-inc (get-rand-int x rng)) x))))))))

(defn gen-digits
  "Generates the given number of random digits and converts all into a single string.
  When a second argument is present, it should be an instance of a random number
  generator used to get the digits."
  {:added "1.0.0" :tag String}
  ([^long num]
   (apply str (repeatedly num #(rand-int 10))))
  ([^long num
    ^java.util.Random rng]
   (when (some? num)
     (if (nil? rng)
       (gen-digits num)
       (apply str (repeatedly num #(.nextInt rng 10)))))))

(defn get-rand-nth
  "Returns a random element of the given vector. When a second argument is present, it
  should be an instance of a random number generator used to get the random position."
  {:added "1.0.0" :tag clojure.lang.Keyword}
  ([^clojure.lang.IPersistentVector v]
   (when-not-empty v (rand-nth v)))
  ([^clojure.lang.IPersistentVector v
    ^java.util.Random rng]
   (when-not-empty v
     (if (nil? rng)
       (rand-nth v)
       (nth v (.nextInt rng (count v)))))))

(defn lazy-iterator-seq
  "Returns a lazy sequence as an interface to the given iterable Java object."
  {:added "1.0.0" :tag clojure.lang.LazySeq}
  ([^Iterable coll]
   (lazy-iterator-seq coll (.iterator coll)))
  ([^Iterable coll ^java.util.Iterator iter]
   (lazy-seq
    (when (.hasNext iter)
      (cons (.next iter) (lazy-iterator-seq coll iter))))))

(defn char-ranges->set
  "Returns a set of characters defined as a collection of collections with start and
  stop character, e.g.: [\\A \\Z][\\0 \\9]"
  {:added "1.0.0" :tag clojure.lang.PersistentHashSet}
  [& ranges]
  (set (mapcat #(map char (range (byte (first %)) (inc (byte (second %))))) ranges)))

;; Text handling

(defn some-string
  ^String [^String s]
  (if (or (not (string? s)) (empty? s)) nil s))

(defn to-bytes
  [obj]
  (if (bytes? obj) obj (.getBytes (str obj) "UTF-8")))

(def bzero
  (to-bytes nil))

(defn bytes-to-string
  "Converts bytes into a string"
  ^String [b]
  (s/assert ::bytes b)
  (apply str (map #(char (bit-and % 255)) b)))

(defn bytes-concat
  ([]
   nil)
  ([bary]
   (not-empty bary))
  ([bary & byte-arys]
   (let [byte-arys (remove empty? (cons bary byte-arys))]
     (when (seq byte-arys)
       (let [^long sum-size             (apply + (map count byte-arys))
             ^bytes buff                (byte-array sum-size)
             ^java.nio.ByteBuffer bbuff (java.nio.ByteBuffer/wrap buff)]
         (doseq [^bytes a byte-arys] (.put bbuff a)) buff)))))

(defn text-to-bytes
  [t]
  (if (bytes? t) t (if (nil? t) bzero (to-bytes t))))

(defn to-long
  [^String s default]
  (s/assert ::integer default)
  (if (nil? s)
    default
    (try
      (java.lang.Long/valueOf s)
      (catch NumberFormatException _e default))))

(defn juxt-seq
  "Like juxt but returns a lazy sequence."
  ^clojure.lang.LazySeq [& functions]
  (fn [& args]
    (map #(apply %1 %2) functions (repeat args))))

(defn uuid
  [^String s]
  (java.util.UUID/fromString s))

(defn try-upper-case
  ^String [^String s]
  (when (some? s) (str/upper-case s)))

(defn try-parse-int
  "Returns an integer from an object or nil if the given object cannot be converted to
  an integer."
  {:tag Integer}
  [s]
  (if (number? s)
    (try (int s) (catch IllegalArgumentException _e nil))
    (when (and (seqable? s) (some? (seq s)))
      (try (Integer/parseInt s)
           (catch NumberFormatException _e nil)))))

(defn try-parse-long
  "Returns a long from an object or nil if the given object cannot be converted to a
  long."
  {:tag Long}
  [s]
  (if (number? s)
    (try (long s) (catch IllegalArgumentException _e nil))
    (when (some? (seq s))
      (try (Long/parseLong s)
           (catch NumberFormatException _e nil)))))
