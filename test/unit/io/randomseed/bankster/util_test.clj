(ns

    ^{:doc    "bankster library, util tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.util-test

  (:require [clojure.test                 :refer [deftest testing is]]
            [clojure.spec.alpha           :as s]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.util  :as u])

  (:import (java.util ArrayList)))

(deftest keyword-coercions
  (testing "ensure-keyword preserves and converts identifiers"
    (is (= :a (u/ensure-keyword :a)))
    (is (= :a (u/ensure-keyword 'a)))
    (is (= :ns/a (u/ensure-keyword 'ns/a)))
    (is (= :123 (u/ensure-keyword 123))))
  (testing "must-have-ns adds namespace when missing"
    (is (= :x/a (u/must-have-ns :a "x")))
    (is (= 'x/a (u/must-have-ns 'a "x")))
    (is (= :x/a (u/must-have-ns :x/a "x"))))
  (testing "ensure-keyword-having-ns forces namespace"
    (is (= :x/a (u/ensure-keyword-having-ns :a "x")))
    (is (= :x/a (u/ensure-keyword-having-ns 'a "x")))))

(deftest spec-asserts-coverage
  (let [prev (s/check-asserts true)]
    (try
      (is (= :x/a (u/must-have-ns :a "x")))
      (is (= #{:b} (u/replace-in-set #{:a} :a :b)))
      (let [b (byte-array [1 2 3])]
        (is (identical? b (u/to-bytes b))))
      (let [b (u/to-bytes "x")]
        (is (= "x" (u/bytes-to-string b))))
      (is (= 7 (u/to-long "7" 0)))
      (finally
        (s/check-asserts prev)))))

(deftest spec-not-conflicting-ns
  (testing "not-conflicting-ns spec predicate"
    (is (true? (s/valid? ::u/not-conflicting-ns ["x" :a])))
    (is (true? (s/valid? ::u/not-conflicting-ns ["x" :x/a])))
    (is (false? (s/valid? ::u/not-conflicting-ns ["x" :y/a])))))

(deftest slash-splitting
  (testing "split-on-first-slash"
    (is (= [nil nil] (u/split-on-first-slash nil)))
    (is (= [nil nil] (u/split-on-first-slash "")))
    (is (= ["abc" nil] (u/split-on-first-slash "abc")))
    (is (= ["a" "b/c"] (u/split-on-first-slash "a/b/c")))
    (is (= [nil "a"] (u/split-on-first-slash "/a")))
    (is (= ["a" nil] (u/split-on-first-slash "a/"))))
  (testing "split-on-last-slash"
    (is (= [nil nil] (u/split-on-last-slash nil)))
    (is (= [nil nil] (u/split-on-last-slash "")))
    (is (= [nil "abc"] (u/split-on-last-slash "abc")))
    (is (= ["a/b" "c"] (u/split-on-last-slash "a/b/c")))
    (is (= [nil "a"] (u/split-on-last-slash "/a")))
    (is (= ["a" nil] (u/split-on-last-slash "a/")))))

(deftest inferred-key-access
  (testing "inferred-contains?/get check both qualified and simple key variants"
    (let [m {:a 1 :ns/a 2}]
      (is (true? (u/inferred-contains? m :a)))
      (is (true? (u/inferred-contains? m :ns/a)))
      (is (true? (u/inferred-contains? {:a 1} :ns/a)))
      (is (= 2 (u/inferred-get m :ns/a)))
      (is (= 1 (u/inferred-get {:a 1} :a)))
      (is (= 1 (u/inferred-get {:a 1} :ns/a)))
      (is (= :nope (u/inferred-get {} :ns/a :nope)))
      (is (= false (u/inferred-contains? {} :a))))))

(deftest collection-helpers
  (testing "with-not-empty returns nil for empty and identity for non-empty"
    (is (= nil (u/with-not-empty [])))
    (is (= [1] (u/with-not-empty [1]))))
  (testing "remove-from-set-where and keep-in-set-where return nil when empty"
    (is (= nil (u/remove-from-set-where even? #{2})))
    (is (= nil (u/keep-in-set-where even? #{1})))
    (is (= #{1 3} (u/remove-from-set-where even? #{1 2 3})))
    (is (= #{2} (u/keep-in-set-where even? #{1 2 3}))))
  (testing "remove-from-set-where supports non-transient sets"
    (is (= #{1 3} (u/remove-from-set-where even? (sorted-set 1 2 3)))))
  (testing "keep-in-set-where supports non-transient sets"
    (is (= #{2} (u/keep-in-set-where even? (sorted-set 1 2 3)))))
  (testing "replace-in-set swaps values"
    (is (= #{:b} (u/replace-in-set #{:a} :a :b)))))

(deftest macros-try-null-and-is
  (testing "try-null catches NPE"
    (is (= nil (u/try-null (.toString nil)))))
  (testing "is / is-not return original value when predicate fails/succeeds"
    (is (= -10 (u/is pos? -10 123)))
    (is (= 123 (u/is pos? 10 123)))
    (is (= 10 (u/is-not pos? 10 123)))
    (is (= 123 (u/is-not pos? -10 123)))))

(defn- auto-alias-in-temp-ns
  [form]
  (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.auto-alias-" (gensym)))
        ns-obj (create-ns ns-sym)]
    (binding [*ns* ns-obj]
      (eval '(clojure.core/require 'io.randomseed.bankster.util))
      (eval '(clojure.core/require 'io.randomseed.bankster.api))
      (eval form))
    (ns-resolve ns-obj 'money)))

(deftest auto-alias-macro-branches
  (testing "auto-alias accepts symbol, quoted symbol and keyword"
    (is (auto-alias-in-temp-ns '(io.randomseed.bankster.util/auto-alias io.randomseed.bankster.api)))
    (is (auto-alias-in-temp-ns '(io.randomseed.bankster.util/auto-alias 'io.randomseed.bankster.api)))
    (is (auto-alias-in-temp-ns '(io.randomseed.bankster.util/auto-alias :io.randomseed.bankster.api)))
    (is (auto-alias-in-temp-ns '(io.randomseed.bankster.util/auto-alias "io.randomseed.bankster.api"))))
  (testing "auto-alias macro function execution"
    (let [f     (deref #'io.randomseed.bankster.util/auto-alias)
          forms (f nil nil 'io.randomseed.bankster.api)]
      (is forms))))

(deftest auto-alias-metadata-and-rename
  (let [src-ns (the-ns 'io.randomseed.bankster.util-test)
        tgt-sym (symbol (str "io.randomseed.bankster.util-test.auto-alias-tgt-" (gensym)))
        tgt-ns  (create-ns tgt-sym)
        v-str   (symbol (str "auto-alias-src-str-" (gensym)))
        v-sym   (symbol (str "auto-alias-src-sym-" (gensym)))
        v-kw    (symbol (str "auto-alias-src-kw-" (gensym)))
        v-kw-ns (symbol (str "auto-alias-src-kw-ns-" (gensym)))
        v-str-as (symbol (str "auto-alias-src-str-as-" (gensym)))
        v-nil   (symbol (str "auto-alias-src-nil-" (gensym)))
        v-else  (symbol (str "auto-alias-src-else-" (gensym)))
        v-str-ns (symbol (str "auto-alias-src-str-ns-" (gensym)))
        syms    [v-str v-sym v-kw v-kw-ns v-str-as v-nil v-else v-str-ns]]
    (letfn [(intern-auto [sym val auto]
              (let [v (intern src-ns sym val)]
                (alter-meta! v assoc :auto-alias auto)
                v))]
      (intern-auto v-str 1 "str-alias")
      (intern-auto v-sym 2 {:alias/as 'sym-alias :doc "sym-doc"})
      (intern-auto v-kw 3 {:alias/as :kw-alias})
      (intern-auto v-kw-ns 4 {:alias/as :tgt/kw-ns-alias})
      (intern-auto v-str-as 5 {:alias/as "str-as"})
      (intern-auto v-nil 6 {:alias/as nil})
      (intern-auto v-else 7 {:alias/as \x})
      (intern-auto v-str-ns 8 {:alias/as "foo/bar"}))
    (binding [*ns* tgt-ns]
      (eval '(clojure.core/require 'io.randomseed.bankster.util))
      (eval '(io.randomseed.bankster.util/auto-alias 'io.randomseed.bankster.util-test)))
    (is (ns-resolve tgt-ns 'str-alias))
    (is (= "sym-doc" (:doc (meta (ns-resolve tgt-ns 'sym-alias)))))
    (is (ns-resolve tgt-ns 'kw-alias))
    (is (ns-resolve tgt-ns 'kw-ns-alias))
    (is (ns-resolve tgt-ns 'str-as))
    (is (ns-resolve tgt-ns v-nil))
    (is (ns-resolve tgt-ns 'x))
    (is (ns-resolve tgt-ns 'bar))
    (doseq [sym syms]
      (ns-unmap src-ns sym))))

(deftest defalias-nonstandard-tag
  (testing "defalias ignores non-tag values in :tag metadata"
    (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.defalias-" (gensym)))
          ns-obj (create-ns ns-sym)]
      (binding [*ns* ns-obj]
        (eval '(clojure.core/require 'io.randomseed.bankster.util))
        (eval '(def ^{:tag 1} weird-tagged 42))
        (eval '(io.randomseed.bankster.util/defalias weird-tagged-alias weird-tagged)))
      (let [v (ns-resolve ns-obj 'weird-tagged-alias)]
        (is (nil? (:tag (meta v)))))))
  (testing "defalias accepts :added string"
    (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.defalias-added-" (gensym)))
          ns-obj (create-ns ns-sym)]
      (binding [*ns* ns-obj]
        (eval '(clojure.core/require 'io.randomseed.bankster.util))
        (eval '(clojure.core/defn f [] 1))
        (eval '(io.randomseed.bankster.util/defalias f-alias f "2.2.1")))
      (let [v (ns-resolve ns-obj 'f-alias)]
        (is (= "2.2.1" (:added (meta v)))))))
  (testing "defalias rejects non-string :added"
    (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.defalias-added-err-" (gensym)))
          ns-obj (create-ns ns-sym)]
      (binding [*ns* ns-obj]
        (eval '(clojure.core/require 'io.randomseed.bankster.util))
        (eval '(clojure.core/defn f [] 1)))
      (letfn [(compiler-ex [form]
                (try
                  (binding [*ns* ns-obj]
                    (eval form))
                  nil
                  (catch clojure.lang.Compiler$CompilerException e
                    e)))]
        (let [ex (compiler-ex '(io.randomseed.bankster.util/defalias f-alias f 123))]
          (is (instance? clojure.lang.Compiler$CompilerException ex))
          (is (= {:name 'f-alias :target 'f :added 123}
                 (some-> ex ex-cause ex-data)))))))
  (testing "defalias maps var tags to primitive type hints"
    (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.defalias-var-tag-" (gensym)))
          ns-obj (create-ns ns-sym)]
      (binding [*ns* ns-obj]
        (eval '(clojure.core/require 'io.randomseed.bankster.util))
        (eval '(def ^{:tag #'clojure.core/long} tagged-var 42))
        (eval '(io.randomseed.bankster.util/defalias tagged-var-alias tagged-var)))
      (let [v (ns-resolve ns-obj 'tagged-var-alias)]
        (is (= Long/TYPE (:tag (meta v))))))))

(deftest defalias-reg-behavior
  (testing "helper arglist predicates"
    (is (true? (#'u/simple-arglist? ['a 'b])))
    (is (true? (#'u/simple-arglist? ['a '& 'more])))
    (is (false? (#'u/simple-arglist? (vector 'a '{:keys [x]}))))
    (is (false? (#'u/simple-arglist? ['a '& 'more 'x])))
    (is (#'u/registry-arglist? ['registry]))
    (is (#'u/registry-arglist? ['a 'registry 'b]))
    (is (nil? (#'u/registry-arglist? ['a])))
    (is (#'u/arglist-varargs? ['a '& 'more]))
    (is (nil? (#'u/arglist-varargs? ['a 'b])))
    (is (= 2 (#'u/fixed-arity ['a 'b])))
    (is (= 1 (#'u/fixed-arity ['a '& 'more])))
    (is (true? (#'u/distinct-arities? [['a] ['a 'b] ['a '& 'more]])))
    (is (false? (#'u/distinct-arities? [['a 'b] ['c 'd]])))))

(deftest defalias-reg-branches
  (let [primitive-tags {'long    Long/TYPE
                        'int     Integer/TYPE
                        'double  Double/TYPE
                        'float   Float/TYPE
                        'short   Short/TYPE
                        'byte    Byte/TYPE
                        'boolean Boolean/TYPE
                        'char    Character/TYPE}]
    (testing "qualify-tag branches"
      (is (nil? (#'u/qualify-tag nil (the-ns 'io.randomseed.bankster.util-test) primitive-tags)))
      (is (= Long/TYPE (#'u/qualify-tag 'long (the-ns 'io.randomseed.bankster.util-test) primitive-tags)))
      (is (= 'foo/bar (#'u/qualify-tag 'foo/bar (the-ns 'io.randomseed.bankster.util-test) primitive-tags)))
      (is (= 'nope (#'u/qualify-tag 'nope (the-ns 'io.randomseed.bankster.util-test) primitive-tags)))
      (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.qualify-tag-" (gensym)))
            ns-obj (create-ns ns-sym)]
        (binding [*ns* ns-obj]
          (eval '(def ^{:tag 'long} v 1)))
        (is (= Long/TYPE (#'u/qualify-tag 'v ns-obj primitive-tags)))))
    (testing "qualify-arglist branches"
      (let [ns-obj (the-ns 'io.randomseed.bankster.currency)
            args   [(with-meta 'registry {:tag 'Registry}) :x]
            out    (#'u/qualify-arglist args ns-obj primitive-tags)]
        (is (class? (:tag (meta (first out)))))
        (is (= :x (second out)))))
    (testing "defalias-reg error branches"
      (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.defalias-reg-err-" (gensym)))
            ns-obj (create-ns ns-sym)]
        (binding [*ns* ns-obj]
          (eval '(clojure.core/require 'io.randomseed.bankster.util))
          (eval '(clojure.core/defmacro m [] nil))
          (eval '(clojure.core/defn d [{:keys [a]}] a))
          (eval '(clojure.core/defn g [a] a))
          (eval '(def ^{:tag #'clojure.core/long} tagged-var 1))
          (eval '(clojure.core/alter-meta! #'g clojure.core/assoc :arglists '([a] [b]))))
        (letfn [(compiler-ex [form]
                  (try
                    (binding [*ns* ns-obj]
                      (eval form))
                    nil
                    (catch clojure.lang.Compiler$CompilerException e
                      e)))]
          (let [ex (compiler-ex '(io.randomseed.bankster.util/defalias-reg m-alias m))]
            (is (instance? clojure.lang.Compiler$CompilerException ex))
            (is (= {:target 'm} (some-> ex ex-cause ex-data))))
          (let [ex (compiler-ex '(io.randomseed.bankster.util/defalias-reg d-alias d))]
            (is (instance? clojure.lang.Compiler$CompilerException ex))
            (is (= 'd (:target (some-> ex ex-cause ex-data)))))
          (let [ex (compiler-ex '(io.randomseed.bankster.util/defalias-reg g-alias g))]
            (is (instance? clojure.lang.Compiler$CompilerException ex))
            (is (= 'g (:target (some-> ex ex-cause ex-data)))))
          (let [ex (compiler-ex '(io.randomseed.bankster.util/defalias-reg g-alias g 123))]
            (is (instance? clojure.lang.Compiler$CompilerException ex))
            (is (= {:name 'g-alias :target 'g :added 123}
                   (some-> ex ex-cause ex-data)))))
          (binding [*ns* ns-obj]
            (eval '(io.randomseed.bankster.util/defalias-reg tagged-var-alias tagged-var "2.2.1")))
          (let [v (ns-resolve ns-obj 'tagged-var-alias)]
            (is (= Long/TYPE (:tag (meta v))))
            (is (= "2.2.1" (:added (meta v)))))))))
(deftest defalias-reg-default-registry
  (testing "defalias-reg resolves registry true to default"
    (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.defalias-reg-" (gensym)))
          ns-obj (create-ns ns-sym)]
      (binding [*ns* ns-obj]
        (eval '(clojure.core/require 'io.randomseed.bankster.util))
        (eval '(clojure.core/require 'io.randomseed.bankster.registry))
        (eval '(clojure.core/defn target [registry] registry))
        (eval '(io.randomseed.bankster.util/defalias-reg target-alias target)))
      (let [f (ns-resolve ns-obj 'target-alias)]
        (is (registry/registry? (f true)))
        (is (identical? (registry/get) (f true)))
        (is (= :x (f :x)))
        (is (nil? (f nil)))))))

(deftest defalias-reg-varargs-coverage
  (testing "defalias-reg handles varargs with and without fixed args"
    (let [ns-sym (symbol (str "io.randomseed.bankster.util-test.defalias-reg-varargs-" (gensym)))
          ns-obj (create-ns ns-sym)]
      (binding [*ns* ns-obj]
        (eval '(clojure.core/require 'io.randomseed.bankster.util))
        (eval '(clojure.core/defn v1 [a & more] (clojure.core/cons a more)))
        (eval '(io.randomseed.bankster.util/defalias-reg v1-alias v1))
        (eval '(clojure.core/defn v2 [& more] more))
        (eval '(clojure.core/alter-meta! #'v2 clojure.core/assoc :arglists '([& more])))
        (eval '(io.randomseed.bankster.util/defalias-reg v2-alias v2)))
      (let [v1 (ns-resolve ns-obj 'v1-alias)
            v2 (ns-resolve ns-obj 'v2-alias)]
        (is (= '(1 2 3) (v1 1 2 3)))
        (is (= '(2 3) (v2 2 3)))))))

(deftest numeric-utils
  (testing "count-digits"
    (is (= 1 (u/count-digits 0)))
    (is (= 1 (u/count-digits 9)))
    (is (= 2 (u/count-digits 10)))
    (is (= 3 (u/count-digits 999))))
  (testing "try-parse-int/long"
    (is (= 12 (u/try-parse-int "12")))
    (is (= nil (u/try-parse-int "nope")))
    (is (= 12 (u/try-parse-int 12.0)))
    (is (= 12 (u/try-parse-long "12")))
    (is (= 12 (u/try-parse-long 12.0)))
    (is (= nil (u/try-parse-long "")))
    (is (= nil (u/try-parse-long "nope")))))

(deftest ns-infer-and-thread-utils
  (testing "ns-infer respects existing namespace and opt flag"
    (is (= :x/a (u/ns-infer "x" :a)))
    (is (= :ns/a (u/ns-infer "x" :ns/a)))
    (is (= :a (u/ns-infer "x" :a false))))
  (testing "current-thread helpers return sensible values"
    (is (integer? (u/current-thread-id)))
    (is (string? (u/current-thread-name)))
    (is (instance? Thread (u/current-thread))))
  (testing "current-thread-id falls back when threadId method is unavailable"
    (let [orig-make-array clojure.core/make-array
          calls (atom 0)]
      (with-redefs [clojure.core/make-array (fn [c n]
                                              (swap! calls inc)
                                              (if (= 1 @calls)
                                                (into-array Class [String])
                                                (orig-make-array c n)))]
        (is (integer? (u/current-thread-id)))))))

(deftest bytes-and-text
  (testing "bytes roundtrip is stable for ASCII"
    (let [b (u/to-bytes "abc")]
      (is (= "abc" (String. b "UTF-8")))
      (is (= "abc" (u/bytes-to-string b)))))
  (testing "bytes-concat skips empty arrays and returns nil for empty input"
    (is (= nil (u/bytes-concat)))
    (let [a (u/to-bytes "a")
          b (byte-array 0)
          c (u/to-bytes "c")]
      (is (= "ac" (u/bytes-to-string (u/bytes-concat a b c))))))
  (testing "bytes-concat accepts single array"
    (let [a (u/to-bytes "a")]
      (is (= "a" (u/bytes-to-string (u/bytes-concat a))))))
  (testing "text-to-bytes handles bytes and nil"
    (let [a (u/to-bytes "x")]
      (is (= a (u/text-to-bytes a)))
      (let [b (u/text-to-bytes nil)]
        (is (bytes? b))
        (is (zero? (alength b))))
      (is (= "x" (u/bytes-to-string (u/text-to-bytes "x"))))))
  (testing "to-long uses default on parse failure"
    (is (= 7 (u/to-long nil 7)))
    (is (= 12 (u/to-long "12" 7)))
    (is (= 7 (u/to-long "nope" 7)))))

(deftest random-and-seq-helpers
  (testing "get-rand-int uses optional RNG"
    (let [rng (java.util.Random. 0)]
      (is (<= 0 (u/get-rand-int 10) 9))
      (is (<= 0 (u/get-rand-int 10 rng) 9))
      (is (<= 0 (u/get-rand-int 10 nil) 9))
      (is (= 0 (u/get-rand-int 0 rng)))))
  (testing "random-digits-len handles shrink and RNG"
    (let [rng (java.util.Random. 0)]
      (is (= 0 (u/random-digits-len 0 1 true)))
      (is (pos? (u/random-digits-len 5 0 true)))
      (is (pos? (u/random-digits-len 5 5 true)))
      (is (pos? (u/random-digits-len 5 1 true nil)))
      (is (pos? (u/random-digits-len 5 0 true rng)))
      (is (= 0 (u/random-digits-len 0 1 true rng)))
      (is (pos? (u/random-digits-len 5 5 true rng)))
      (is (= 5 (u/random-digits-len 5 3 false)))
      (is (= 5 (u/random-digits-len 5 3 false rng)))))
  (testing "gen-digits supports RNG"
    (let [rng (java.util.Random. 0)]
      (is (string? (u/gen-digits 4)))
      (is (string? (u/gen-digits 4 rng)))
      (is (string? (u/gen-digits 4 nil)))))
  (testing "get-rand-nth supports RNG and empty input"
    (let [rng (java.util.Random. 0)]
      (is (nil? (u/get-rand-nth [])))
      (is (#{:a :b} (u/get-rand-nth [:a :b] rng)))
      (is (#{:a :b} (u/get-rand-nth [:a :b] nil)))
      (is (#{:a :b} (u/get-rand-nth [:a :b])))))
  (testing "juxt-seq returns a lazy seq of results"
    (let [f (u/juxt-seq inc dec)]
      (is (= [2 0] (vec (f 1))))))
  (testing "uuid and try-upper-case helpers"
    (is (= (java.util.UUID/fromString "00000000-0000-0000-0000-000000000000")
           (u/uuid "00000000-0000-0000-0000-000000000000")))
    (is (= "AB" (u/try-upper-case "ab")))
    (is (nil? (u/try-upper-case nil)))))

(deftest lazy-iterator-seq-test
  (testing "lazy-iterator-seq exposes Java Iterable as a seq"
    (let [al (doto (ArrayList.) (.add 1) (.add 2) (.add 3))]
      (is (= [1 2 3] (vec (u/lazy-iterator-seq al)))))))

(deftest char-ranges
  (testing "char-ranges->set expands inclusive ranges"
    (let [s (u/char-ranges->set [\A \C] [\0 \2])]
      (is (contains? s \A))
      (is (contains? s \C))
      (is (contains? s \2))
      (is (not (contains? s \D))))))

(deftest some-string-helpers
  (testing "some-string returns nil for non-strings and empty strings"
    (is (nil? (u/some-string nil)))
    (is (nil? (u/some-string "")))
    (is (nil? (u/some-string 123))))
  (testing "some-string returns value for non-empty strings"
    (is (= "x" (u/some-string "x")))))
