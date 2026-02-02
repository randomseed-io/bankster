(ns

    ^{:doc    "bankster library, registry tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.registry-test

  (:require [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.currency :as c]
            [io.randomseed.bankster          :as bankster]))

(deftest new-registry-hierarchies
  (testing "default registry has CurrencyHierarchies initialized"
    (let [r  (registry/new)
          hs (:hierarchies r)]
      (is (instance? io.randomseed.bankster.CurrencyHierarchies hs))
      (is (map? (:domain hs)))
      (is (map? (:kind hs)))))

  (testing "hierarchies can be passed as a parent-map in a map spec"
    (let [r (registry/new {} {} {} {} {}
                          {:domain {:ISO-4217-LEGACY :ISO-4217}
                           :kind   {:COMBANK :FIAT}}
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:kind hs)   :COMBANK         :FIAT))))

  (testing "hierarchies can include custom axes (additional keys)"
    (let [r (registry/new {} {} {} {} {}
                          {:domain {:ISO-4217-LEGACY :ISO-4217}
                           :kind   {:COMBANK :FIAT}
                           :traits {:stablecoin :stable
                                    :fiat-backed :stable}}
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:traits hs) :stablecoin :stable))
      (is (isa? (:traits hs) :fiat-backed :stable))))

  (testing "parent-map supports multiple parents via set/vector values"
    (let [r (registry/new {} {} {} {} {}
                          {:domain {:ISO-4217-LEGACY #{:ISO-4217 :MONEY}
                                    :ISO-4217-LEGACY2 [:ISO-4217 :MONEY]}
                           :kind   {:COMBANK #{:FIAT :FUNDS}}}
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:domain hs) :ISO-4217-LEGACY :MONEY))
      (is (isa? (:domain hs) :ISO-4217-LEGACY2 :ISO-4217))
      (is (isa? (:domain hs) :ISO-4217-LEGACY2 :MONEY))
      (is (isa? (:kind hs)   :COMBANK :FIAT))
      (is (isa? (:kind hs)   :COMBANK :FUNDS))))

  (testing "hierarchies can be passed as a record with parent-maps"
    (let [r (registry/new {} {} {} {} {}
                          (bankster/->CurrencyHierarchies {:ISO-4217-LEGACY :ISO-4217} nil nil)
                          "test")
          hs (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (map? (:kind hs)))))

  (testing "hierarchies can be passed as already constructed hierarchy maps"
    (let [dom (derive (make-hierarchy) :ISO-4217-LEGACY :ISO-4217)
          r   (registry/new {} {} {} {} {}
                            {:domain dom}
                            "test")
          hs  (:hierarchies r)]
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (map? (:kind hs))))))

(deftest registry-get-and-hierarchy-nil-safe
  (testing "registry/get treats literal true as a sentinel meaning: use the default registry"
    (let [r (registry/get)]
      (is (= r (registry/get true)))))

  (testing "hierarchies/hierarchy accept nil registry and fall back to the default registry"
    (let [hs (registry/hierarchies)]
      (is (= hs (registry/hierarchies nil)))
      (is (= (:kind hs) (registry/hierarchy :kind nil)))
      (is (= (:traits hs) (registry/hierarchy :traits nil))))))

(deftest hierarchy-derive-updates-selected-hierarchy
  (testing "hierarchy-derive returns a new registry with derived relationship"
    (let [r0 (registry/new)
          r1 (registry/hierarchy-derive :kind :iso/funds :iso/money r0)
          h0 (registry/hierarchy :kind r0)
          h1 (registry/hierarchy :kind r1)]
      (is (not (isa? h0 :iso/funds :iso/money)))
      (is (isa? h1 :iso/funds :iso/money))))

  (testing "hierarchy-derive! updates the global registry"
    (let [orig (registry/state)]
      (try
        (registry/set! (registry/new))
        (registry/hierarchy-derive! :domain :ISO-4217-LEGACY :ISO-4217)
        (let [h (registry/hierarchy :domain (registry/state))]
          (is (isa? h :ISO-4217-LEGACY :ISO-4217)))
        (finally
          (registry/set! orig))))))

(deftest hierarchy-spec-coercion-and-errors
  (testing "->hierarchy accepts nil, hierarchy map, parent map, and rejects invalid input"
    (let [->hierarchy #'io.randomseed.bankster.registry/->hierarchy
          h1          (->hierarchy nil :kind)
          h2          (derive (make-hierarchy) :child :parent)
          h3          (->hierarchy {:child :parent} :kind)]
      (is (map? h1))
      (is (map? h2))
      (is (true? (isa? h2 :child :parent)))
      (is (true? (isa? h3 :child :parent))))
    (try
      (#'io.randomseed.bankster.registry/->hierarchy 42 :kind)
      (is false)
      (catch clojure.lang.ExceptionInfo e
        (is (= :kind (:type (ex-data e))))
        (is (= 42 (:value (ex-data e)))))))
  (testing "->currency-hierarchies accepts nil/map/record and rejects invalid input"
    (let [->currency-hierarchies #'io.randomseed.bankster.registry/->currency-hierarchies
          hmap {:domain {:ISO-4217-LEGACY :ISO-4217}
                :kind   {:COMBANK :FIAT}
                :traits {:stablecoin :stable}}
          hs1  (->currency-hierarchies nil)
          hs2  (->currency-hierarchies hmap)
          hs3  (->currency-hierarchies (bankster/map->CurrencyHierarchies hmap))]
      (is (instance? io.randomseed.bankster.CurrencyHierarchies hs1))
      (is (instance? io.randomseed.bankster.CurrencyHierarchies hs2))
      (is (instance? io.randomseed.bankster.CurrencyHierarchies hs3))
      (is (isa? (:domain hs2) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:kind hs2)   :COMBANK :FIAT))
      (is (isa? (get hs2 :traits) :stablecoin :stable)))
    (try
      ((#'io.randomseed.bankster.registry/->currency-hierarchies 42))
      (is false)
      (catch clojure.lang.ExceptionInfo e
        (is (= 42 (:value (ex-data e))))))))

(deftest inconsistency-warning-macro
  (testing "macroexpansion covers both branches and body always runs"
    (let [form1 '(io.randomseed.bankster.registry/inconsistency-warning "m" {:x 1} :ok)
          form2 '(io.randomseed.bankster.registry/inconsistency-warning "m" {:x 1} (throw (ex-info "boom" {})))]
      (is (seq (macroexpand form1)))
      (is (seq (macroexpand form2)))))
  (testing "when warnings are disabled, the logger is not called"
    (let [called? (atom false)]
      (binding [registry/*warn-on-inconsistency* false
                registry/*warnings-logger*       (fn [_ _] (reset! called? true))]
        (is (= :ok (registry/inconsistency-warning "x" {:y 1} :ok)))
        (is (= false @called?)))))
  (testing "when warnings are enabled, the logger is called; logger exceptions are swallowed"
    (let [seen (atom nil)]
      (binding [registry/*warn-on-inconsistency* true
                registry/*warnings-logger*       (fn [msg data] (reset! seen {:msg msg :data data}))]
        (is (= :ok (registry/inconsistency-warning "hello" {:a 1} :ok)))
        (is (re-find #"Registry inconsistency:" (:msg @seen)))
        (is (= {:a 1} (:data @seen))))
      (binding [registry/*warn-on-inconsistency* true
                registry/*warnings-logger*       (fn [_ _] (throw (RuntimeException. "nope")))]
        (is (= :ok (registry/inconsistency-warning "hello" {:a 1} :ok)))))))

(deftest global-registry-state-update-and-set
  (testing "default-version returns a plausible timestamp string"
    (let [v (registry/default-version)]
      (is (string? v))
      (is (<= 16 (count v)))))
  (testing "set!/update!/state/global work and accept map inputs"
    (let [orig (registry/state)]
      (try
        (registry/set! (registry/new))
        (is (true? (registry/registry? (registry/state))))
        (registry/update! (fn [r] (assoc r :ext {:a 1})))
        (is (= 1 (registry/ext :a (registry/state))))
        (registry/set! {:ext {:b 2}})
        (is (= {:b 2} (registry/ext (registry/state))))
        (finally
          (registry/set! orig))))))

(deftest update-helpers-and-getters
  (testing "update applies the given function to the registry"
    (let [r0 (registry/new)
          r1 (registry/update r0 (fn [r k v] (assoc-in r [:ext k] v)) :x 1)]
      (is (= 1 (get-in r1 [:ext :x])))))
  (testing "with binds *default* and get respects registry arg precedence"
    (let [rA (assoc (registry/new) :ext {:a 1})
          rB (assoc (registry/new) :ext {:b 2})]
      (registry/with rA
        (is (= {:a 1} (registry/ext)))
        (is (= {:a 1} (registry/ext (registry/get))))
        (is (= {:b 2} (registry/ext (registry/get rB))))
        (is (= {:a 1} (registry/ext (registry/get nil))))
        (is (= {:a 1} (registry/ext (registry/get false)))))))
  (testing "currency map getters (macros + wrapper fns) cover all arities"
    (let [r (-> (registry/new)
                (assoc :cur-id->cur {:EUR :eur})
                (assoc :cur-nr->cur {978 :eur})
                (assoc :cur-nr->curs {978 [:eur]})
                (assoc :cur-code->curs {:EUR [:eur]})
                (assoc :ctr-id->cur {:PL :eur})
                (assoc :cur-id->ctr-ids {:EUR #{:PL}})
                (assoc :cur-id->localized {:EUR {:* {:name "Euro"}}})
                (assoc :cur-id->traits {:EUR #{:fiat}})
                (assoc :cur-id->weight {:EUR 7})
                (assoc :ext {:x 1})
                (assoc :version "v"))]
      (is (= {:EUR :eur} (registry/currency-id->currency r)))
      (is (= :eur       (registry/currency-id->currency :EUR r)))
      (is (= {978 :eur} (registry/currency-nr->currency r)))
      (is (= :eur       (registry/currency-nr->currency 978 r)))
      (is (= {978 [:eur]} (registry/currency-nr->currencies r)))
      (is (= [:eur]        (registry/currency-nr->currencies 978 r)))
      (is (= {:EUR [:eur]} (registry/currency-code->currencies r)))
      (is (= [:eur]        (registry/currency-code->currencies :EUR r)))
      (is (= {:PL :eur}    (registry/country-id->currency r)))
      (is (= :eur          (registry/country-id->currency :PL r)))
      (is (= {:EUR #{:PL}} (registry/currency-id->country-ids r)))
      (is (= #{:PL}        (registry/currency-id->country-ids :EUR r)))
      (is (= {:EUR {:* {:name "Euro"}}} (registry/currency-id->localized r)))
      (is (= {:* {:name "Euro"}}        (registry/currency-id->localized :EUR r)))
      (is (= {:EUR #{:fiat}} (registry/currency-id->traits r)))
      (is (= #{:fiat}        (registry/currency-id->traits :EUR r)))
      (is (= {:EUR 7} (registry/currency-id->weight r)))
      (is (= 7       (registry/currency-id->weight :EUR r)))
      (is (= {:x 1} (registry/ext r)))
      (is (= 1      (registry/ext :x r)))
      (is (= "v"    (registry/version r)))))
  (testing "hierarchies* optimized branch for known fields and generic branch for custom keys"
    (let [r  (-> (registry/new)
                 (assoc :hierarchies (-> (bankster/->CurrencyHierarchies (make-hierarchy) (make-hierarchy) (make-hierarchy))
                                         (assoc :traits (derive (make-hierarchy) :child :parent)))))
          h1 (registry/hierarchy :domain r)
          h2 (registry/hierarchy :traits r)]
      (is (map? h1))
      (is (true? (isa? h2 :child :parent)))
      (is (nil? (registry/hierarchy :nope r))))))

(deftest registry-print-method
  (testing "Registry has a stable printable representation"
    (let [s (pr-str (registry/new))]
      (is (re-find #"^#Registry" s))
      (is (re-find #":version" s)))))

(deftest default-warnings-logger-is-callable
  (testing "default *warnings-logger* is callable when warnings are enabled"
    (binding [registry/*warn-on-inconsistency* true]
      (is (= :ok (registry/inconsistency-warning "hello" {:a 1} :ok)))
      (is (= :ok (registry/inconsistency-warning "hello" nil :ok))))))

(deftest new-registry-arities-and-defaulting
  (testing "new-registry supports all constructor arities"
    (let [cur-id->cur       (assoc (into {} (map (fn [i]
                                                   (let [cid (keyword (str "CUR" i))]
                                                     [cid (c/new cid i 2 :iso/fiat :ISO-4217)]))
                                                 (range 1 9)))
                                   :ZZZ (c/new :ZZZ c/no-numeric-id 2 :iso/fiat nil))
          cur-nr->cur       (into {} (keep (fn [[_ ^io.randomseed.bankster.Currency c]]
                                             (let [n (.numeric c)]
                                               (when (pos? n) [n c])))
                                           cur-id->cur))
          ctr-id->cur       {:PL (get cur-id->cur :CUR1)}
          cur-id->ctr-ids   {:CUR1 #{:PL}}
          cur-id->localized {:CUR1 {:* {:name "Cur 1"}}}
          cur-id->traits    {:CUR1 #{:fiat}}
          cur-id->weight    {:CUR1 7}
          hspec             {:domain {:ISO-4217-LEGACY :ISO-4217}}
          v                "test-v"]
      (is (registry/registry? (registry/new-registry)))
      (is (= v (:version (registry/new-registry cur-id->cur
                                                ctr-id->cur
                                                cur-id->localized
                                                cur-id->traits
                                                cur-id->weight
                                                hspec
                                                v))))
      (is (string? (:version (registry/new-registry cur-id->cur
                                                    ctr-id->cur
                                                    cur-id->localized
                                                    cur-id->traits
                                                    cur-id->weight
                                                    hspec))))
      (let [r (registry/new-registry cur-id->cur
                                     ctr-id->cur
                                     cur-id->localized
                                     cur-id->traits
                                     cur-id->weight
                                     hspec
                                     v)]
        (is (= cur-id->cur (:cur-id->cur r)))
        (is (= cur-nr->cur (:cur-nr->cur r)))
        (is (= cur-id->ctr-ids (:cur-id->ctr-ids r))))
      (let [r (registry/new-registry cur-id->cur
                                     ctr-id->cur
                                     cur-id->localized
                                     cur-id->traits
                                     cur-id->weight)]
        (is (string? (:version r)))
        (is (= cur-id->cur (:cur-id->cur r)))
        (is (= cur-nr->cur (:cur-nr->cur r)))
        (is (= cur-id->ctr-ids (:cur-id->ctr-ids r))))))

  (testing "new-registry map arity preserves :ext when provided"
    (let [r (registry/new-registry {:ext {:a 1}})]
      (is (= {:a 1} (:ext r)))
      (is (= 1 (get-in r [:ext :a]))))))

(deftest new-registry-map-arity-defaults-ext
  (testing "new-registry map arity defaults :ext when missing"
    (let [r (registry/new-registry {})]
      (is (map? (:ext r))))))

(deftest private-hierarchy-map-branch-coverage
  (let [hierarchy-map? #'io.randomseed.bankster.registry/hierarchy-map?]
    (testing "hierarchy-map? exercises all short-circuit branches"
      (is (false? (hierarchy-map? nil)))
      (is (false? (hierarchy-map? {})))
      (is (false? (hierarchy-map? {:parents {}})))
      (is (false? (hierarchy-map? {:parents {} :ancestors {}})))
      (is (false? (hierarchy-map? {:parents 1 :ancestors {} :descendants {}})))
      (is (false? (hierarchy-map? {:parents {} :ancestors 1 :descendants {}})))
      (is (false? (hierarchy-map? {:parents {} :ancestors {} :descendants 1})))
      (is (true?  (hierarchy-map? {:parents {} :ancestors {} :descendants {}}))))))

(deftest get-macro-coverage
  (testing "get macroexpansion covers both sentinel and non-sentinel branches"
    (doseq [form '[(io.randomseed.bankster.registry/get)
                   (io.randomseed.bankster.registry/get true)
                   (io.randomseed.bankster.registry/get some-registry)]]
      (is (not= form (macroexpand-1 form))))))

(deftest wrapper-zero-arity-uses-default
  (testing "zero-arity wrapper fns respect registry/with and do not require explicit registry arg"
    (let [r (-> (registry/new)
                (assoc :cur-id->cur {:EUR :eur})
                (assoc :cur-nr->cur {978 :eur})
                (assoc :cur-nr->curs {978 [:eur]})
                (assoc :cur-code->curs {:EUR [:eur]})
                (assoc :ctr-id->cur {:PL :eur})
                (assoc :cur-id->ctr-ids {:EUR #{:PL}})
                (assoc :cur-id->localized {:EUR {:* {:name "Euro"}}})
                (assoc :cur-id->traits {:EUR #{:fiat}})
                (assoc :cur-id->weight {:EUR 7})
                (assoc :ext {:x 1})
                (assoc :version "v"))]
      (registry/with r
        (is (= {:EUR :eur} (registry/currency-id->currency)))
        (is (= {978 :eur} (registry/currency-nr->currency)))
        (is (= {978 [:eur]} (registry/currency-nr->currencies)))
        (is (= {:EUR [:eur]} (registry/currency-code->currencies)))
        (is (= {:PL :eur} (registry/country-id->currency)))
        (is (= {:EUR #{:PL}} (registry/currency-id->country-ids)))
        (is (= {:EUR {:* {:name "Euro"}}} (registry/currency-id->localized)))
        (is (= {:EUR #{:fiat}} (registry/currency-id->traits)))
        (is (= {:EUR 7} (registry/currency-id->weight)))
        (is (= (:hierarchies r) (registry/hierarchies)))
        (is (= (:hierarchies r) (registry/hierarchies nil)))
        (is (= (:hierarchies r) (registry/hierarchies r)))
        (is (= (:kind (:hierarchies r)) (registry/hierarchies :kind nil)))
        (is (= (:kind (:hierarchies r)) (registry/hierarchies :kind r)))
        (is (= (:kind (:hierarchies r)) (registry/hierarchy :kind)))
        (is (= (:kind (:hierarchies r)) (registry/hierarchy :kind nil)))
        (is (= {:x 1} (registry/ext)))
        (is (= "v" (registry/version)))))))

(deftest wrapper-zero-arity-uses-global-when-unbound
  (testing "zero-arity wrapper fns fall back to the global registry when *default* is nil"
    (let [r (-> (registry/new)
                (assoc :cur-id->cur {:EUR :eur})
                (assoc :cur-nr->cur {978 :eur})
                (assoc :cur-nr->curs {978 [:eur]})
                (assoc :cur-code->curs {:EUR [:eur]})
                (assoc :ctr-id->cur {:PL :eur})
                (assoc :cur-id->ctr-ids {:EUR #{:PL}})
                (assoc :cur-id->localized {:EUR {:* {:name "Euro"}}})
                (assoc :cur-id->traits {:EUR #{:fiat}})
                (assoc :cur-id->weight {:EUR 7})
                (assoc :ext {:x 1})
                (assoc :version "v"))
          orig (registry/state)]
      (try
        (registry/set! r)
        (is (= {:EUR :eur} (registry/currency-id->currency)))
        (is (= {978 :eur} (registry/currency-nr->currency)))
        (is (= {978 [:eur]} (registry/currency-nr->currencies)))
        (is (= {:EUR [:eur]} (registry/currency-code->currencies)))
        (is (= {:PL :eur} (registry/country-id->currency)))
        (is (= {:EUR #{:PL}} (registry/currency-id->country-ids)))
        (is (= {:EUR {:* {:name "Euro"}}} (registry/currency-id->localized)))
        (is (= {:EUR #{:fiat}} (registry/currency-id->traits)))
        (is (= {:EUR 7} (registry/currency-id->weight)))
        (is (= (:hierarchies r) (registry/hierarchies)))
        (is (= (:hierarchies r) (registry/hierarchies r)))
        (is (= (:kind (:hierarchies r)) (registry/hierarchies :kind nil)))
        (is (= (:kind (:hierarchies r)) (registry/hierarchies :kind r)))
        (is (= (:kind (:hierarchies r)) (registry/hierarchy :kind)))
        (is (= {:x 1} (registry/ext)))
        (is (= "v" (registry/version)))
        (finally
          (registry/set! orig))))))

(deftest hierarchy-derive-multiple-parents-and-nil-hierarchy
  (testing "hierarchy-derive supports set/vector parents and nil hierarchy values"
    (let [r0 (registry/new)
          r1 (registry/hierarchy-derive :kind :c #{:d :e} r0)
          r2 (registry/hierarchy-derive :kind :x [:y :z] r0)
          r3 (registry/hierarchy-derive :traits :t :p (assoc-in r0 [:hierarchies :traits] nil))]
      (is (isa? (registry/hierarchy :kind r1) :c :d))
      (is (isa? (registry/hierarchy :kind r1) :c :e))
      (is (isa? (registry/hierarchy :kind r2) :x :y))
      (is (isa? (registry/hierarchy :kind r2) :x :z))
      (is (isa? (registry/hierarchy :traits r3) :t :p)))))

(deftest hierarchy-derive-registry-fallback-branches
  (testing "hierarchy-derive uses registry arg, *default*, or global fallback"
    (let [r0 (registry/new)]
      (is (isa? (registry/hierarchy :kind (registry/hierarchy-derive :kind :a :b r0)) :a :b))
      (registry/with r0
        (let [r1 (registry/hierarchy-derive :kind :c :d nil)]
          (is (isa? (registry/hierarchy :kind r1) :c :d))))
      (let [orig (registry/state)]
        (try
          (registry/set! r0)
          (let [r1 (registry/hierarchy-derive :kind :e :f nil)]
            (is (isa? (registry/hierarchy :kind r1) :e :f)))
          (finally
            (registry/set! orig)))))))
