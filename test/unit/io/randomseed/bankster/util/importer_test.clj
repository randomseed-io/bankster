(ns

    ^{:doc    "bankster library, importer tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.util.importer-test

  (:require [clojure.edn                     :as edn]
            [clojure.java.io                 :as io]
            [clojure.string                  :as str]
            [clojure.test                    :refer [deftest testing is]]
            [io.randomseed.bankster          :as bankster]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.util.importer :as importer]))

(defn- tmp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "bankster-importer-test"
            (make-array java.nio.file.attribute.FileAttribute 0))))

(deftest make-currency-legacy-domain-from-comment
  (testing "comment \"Old, now\" causes currency ID to get iso-4217-legacy namespace"
    (let [c (#'io.randomseed.bankster.util.importer/make-currency
             ["ADP" "20" "0" "Old, now EUR"])]
      (is (= :iso-4217-legacy/ADP (:id c)))
      (is (= :ISO-4217-LEGACY (:domain c)))
      (is (= true (currency/iso-legacy? c))))))

(deftest make-currency-funds-kind-from-comment
  (testing "comment \"FundsCode\" causes currency kind to be set to :iso/funds"
    (let [c (#'io.randomseed.bankster.util.importer/make-currency
             ["XUA" "965" "0" "FundsCode"])]
      (is (= :XUA (:id c)))
      (is (= :ISO-4217 (:domain c)))
      (is (= :iso/funds (:kind c))))))

(deftest make-currency-default-kind-is-iso-fiat
  (testing "default ISO currencies imported from CSV are classified as :iso/fiat"
    (let [c (#'io.randomseed.bankster.util.importer/make-currency
             ["EUR" "978" "2" nil])]
      (is (= :EUR (:id c)))
      (is (= :ISO-4217 (:domain c)))
      (is (= :iso/fiat (:kind c))))))

(deftest make-currency-special-kind-mapping
  (testing "some special ISO codes get more specific kinds (aligned with seed/config ontology)"
    (let [mk #'io.randomseed.bankster.util.importer/make-currency]
      (is (= :iso/metal            (:kind (mk ["XAU" "959" "0" nil]))))
      (is (= :iso/test             (:kind (mk ["XTS" "963" "0" nil]))))
      (is (= :iso/null             (:kind (mk ["XXX" "999" "0" nil]))))
      (is (= :iso.funds/settlement (:kind (mk ["USN" "997" "2" nil])))))))

(deftest make-currency-parsing-fallbacks
  (testing "numeric/scale parse fallbacks and normalization"
    (let [mk #'io.randomseed.bankster.util.importer/make-currency]
      (is (nil? (mk [nil "1" "2" nil])))
      (is (= currency/no-numeric-id (:numeric (mk ["EUR" "nope" "2" nil]))))
      (is (= currency/no-numeric-id (:numeric (mk ["EUR" "-10"  "2" nil]))))
      (is (= currency/auto-scaled  (:scale   (mk ["EUR" "978" "nope" nil]))))
      (is (= currency/auto-scaled  (:scale   (mk ["EUR" "978" "-2"   nil])))))))

(deftest joda-csv-importer-defaults-and-path-overrides
  (testing "countries-load returns nil when the resource is missing"
    (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [& _] nil)]
      (is (nil? (importer/countries-load)))
      (is (nil? (importer/countries-load "nope.csv")))))
  (testing "currencies-load returns nil when the resource is missing"
    (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [& _] nil)]
      (is (nil? (importer/currencies-load)))
      (is (nil? (importer/currencies-load "nope.csv")))))
  (testing "currencies-load split-comment branch: with and without '#'"
    (let [dir  (tmp-dir)
          f    (io/file dir "cur.csv")
          path (.getPath f)
          ;; 4 columns: id,numeric,scale,comment
          ;; comment contains an inline '#' to trigger split-comment branch
          _    (spit f (str "AAA,1,2,Hello # Old, now BBB\n"
                            "BBB,2,0,Plain comment\n"
                            "CCC,3,0,Hello #    \n"))]
      (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [& _] path)]
        (let [cs (doall (importer/currencies-load "ignored.csv"))]
          (is (= 3 (count cs)))
          (is (= :AAA (:id (first cs))))
          (is (= :BBB (:id (second cs))))
          (is (= :CCC (:id (nth cs 2))))))))
  (testing "currencies-load split-comment branch: last column can be nil (covers (string? l) false)"
    (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [& _] "/tmp/ignored.csv")
                  io.randomseed.bankster.util.fs/read-csv         (fn [& _]
                                                                  ;; 4 columns expected by make-currency: id,numeric,scale,comment
                                                                  [["AAA" "1" "2" nil]])]
      (let [cs (doall (importer/currencies-load "ignored.csv"))]
        (is (= 1 (count cs)))
        (is (= :AAA (:id (first cs)))))))
  (testing "countries-load reads simple CSV rows via fs/read-csv"
    (let [dir  (tmp-dir)
          f    (io/file dir "countries.csv")
          path (.getPath f)
          _    (spit f (str "PL,PLN\n"
                            "DE,EUR\n"
                            "FR,EUR\n"))]
      (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [& _] path)]
        (let [m (importer/countries-load "ignored.csv")]
          (is (= #{:PL} (get m :PLN)))
          (is (= #{:DE :FR} (get m :EUR))))))))

(deftest joda-import-uses-countries-and-currencies
  (testing "joda-import registers currencies and attaches countries"
    (let [eur (currency/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
      (with-redefs [io.randomseed.bankster.util.importer/countries-load  (fn [& _] {:EUR #{:PL}})
                    io.randomseed.bankster.util.importer/currencies-load (fn [& _] [eur])]
        (let [r (importer/joda-import)]
          (is (true? (currency/defined? :EUR r)))
          (is (= #{:PL} (currency/countries :EUR r)))))))
  (testing "joda-import handles nil loaders (returns empty registry)"
    (with-redefs [io.randomseed.bankster.util.importer/countries-load  (fn [& _] nil)
                  io.randomseed.bankster.util.importer/currencies-load (fn [& _] nil)]
      (let [r (importer/joda-import)]
        (is (true? (registry/registry? r)))
        (is (= {} (:cur-id->cur r)))))))

(deftest edn-import-is-an-alias-for-registry-global
  (testing "edn-import returns the global registry atom"
    (is (= (registry/global) (importer/edn-import)))))

(deftest export-shaping-helpers
  (testing "currency->map includes only meaningful fields"
    (is (= {:scale nil} (importer/currency->map {})))
    (is (= {:scale 2} (importer/currency->map {:scale 2 :numeric -1 :kind nil})))
    (is (= {:numeric 978 :scale 2 :kind :iso/fiat}
           (importer/currency->map {:numeric 978 :scale 2 :kind :iso/fiat :foo "bar"})))
    (is (= {:numeric 978} (importer/currency->map {:numeric 978 :scale -1 :kind nil}))))
  (testing "traits->map and localized->map"
    (is (nil? (importer/traits->map nil)))
    (is (nil? (importer/traits->map [])))
    (is (= [:a :b] (importer/traits->map #{:b :a})))
    (is (= {:en {:name "Euro"} :pl {:name "Euro (PL)"}}
           (importer/localized->map {:en {:name "Euro"} :pl {:name "Euro (PL)"}}))))
  (testing "sort-kw-vec normalizes keywords/strings and removes nils"
    (let [f #'io.randomseed.bankster.util.importer/sort-kw-vec]
      (is (= [:a :b] (f [nil :b "a"]))))))

(deftest map-to-currency-oriented-defaulting-and-weights
  (testing "map->currency-oriented defaults missing branches to {}"
    (let [m  {:version "v"}
          m2 (importer/map->currency-oriented m)]
      (is (= {} (:currencies m2)))
      (is (= {} (:countries m2)))
      (is (= {} (:localized m2)))
      (is (= {} (:traits m2)))
      (is (= {} (:weights m2)))))
  (testing "map->currency-oriented embeds :weight when weights contain the currency ID (including 0)"
    (let [m  {:version    "v"
              :currencies (sorted-map :AAA {:scale 2}
                                      :BBB {:scale 2})
              :weights    (sorted-map :AAA 0
                                      :BBB 5
                                      :CCC 9)
              :hierarchies (sorted-map)}
          m2 (importer/map->currency-oriented m)]
      (is (= 0 (get-in m2 [:currencies :AAA :weight])))
      (is (= 5 (get-in m2 [:currencies :BBB :weight])))
      (is (= {:CCC 9} (:weights m2))))))

(deftest dump-and-export-write-to-a-determined-directory
  (testing "dump/export/export-currency-oriented write to a directory based on fs/resource-pathname"
    (let [dir    (tmp-dir)
          root   (io/file dir "io" "randomseed" "bankster")
          _      (.mkdirs root)
          config (io/file root "config.edn")
          _      (spit config "{}")
          r      (-> (registry/new-registry)
                     (currency/register (currency/new :EUR 978 2 :iso/fiat :ISO-4217 0)))]
      (with-redefs [io.randomseed.bankster.util.fs/resource-pathname
                    (fn [& _] (.getPath config))]
        (importer/dump "dump.edn" r)
        (importer/export "export.edn" r)
        (importer/export-currency-oriented "export-co.edn" r)
        ;; cover additional arities
        (importer/dump r)
        (importer/export r)
        (importer/export-currency-oriented r)
        (registry/with r
          (importer/dump)
          (importer/export)
          (importer/export-currency-oriented))
        (is (true? (.exists (io/file root "dump.edn"))))
        (is (true? (.exists (io/file root "export.edn"))))
        (is (true? (.exists (io/file root "export-co.edn"))))
        (is (true? (.exists (io/file root importer/default-dump-filename))))
        (is (true? (.exists (io/file root importer/default-export-filename))))
        (is (true? (.exists (io/file root importer/default-export-currency-oriented-filename))))
        (is (str/includes? (slurp (io/file root "dump.edn"))
                           "#io.randomseed.bankster.Registry"))))))

(deftest dump-and-export-return-nil-when-resource-dir-cannot-be-resolved
  (testing "dump/export/export-currency-oriented no-op when fs/resource-pathname returns nil"
    (let [r (registry/new-registry)]
      (with-redefs [io.randomseed.bankster.util.fs/resource-pathname (fn [& _] nil)]
        (is (nil? (importer/dump "x.edn" r)))
        (is (nil? (importer/export "x.edn" r)))
        (is (nil? (importer/export-currency-oriented "x.edn" r)))))))

(deftest readers-export-no-op-when-no-namespaced-currency-ids
  (testing "readers-export does nothing when there are no namespaced currency IDs"
    (let [dir (tmp-dir)
          f   (io/file dir "data_readers.clj")
          _   (spit f "{}")]
      (with-redefs [clojure.java.io/resource (fn [_] (.toURL (.toURI f)))]
        (is (nil? (importer/readers-export (registry/new-registry) ["data_readers.clj"] "data_readers_edn.clj"
                                           "io/randomseed/bankster/money/reader_handlers.clj"
                                           "io.randomseed.bankster.money")))))))

(deftest readers-export-wrapper-arities
  (testing "readers-export wrappers call the 5-arity implementation"
    (let [dir      (tmp-dir)
          f        (io/file dir "data_readers.clj")
          _        (spit f "{}")
          handlers (io/file dir "io" "randomseed" "bankster" "money" "reader_handlers.clj")
          _        (.mkdirs (.getParentFile handlers))
          r        (-> (registry/new-registry)
                       (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))]
      (with-redefs [clojure.java.io/resource           (fn [_] (.toURL (.toURI f)))
                    io.randomseed.bankster.registry/state (fn [] r)]
        (importer/readers-export)
        (importer/readers-export r)
        (importer/readers-export r ["data_readers.clj"])
        (importer/readers-export r ["data_readers.clj"] "data_readers_edn.clj")
        (importer/readers-export r ["data_readers.clj"] "data_readers_edn.clj"
                                 "io/randomseed/bankster/money/reader_handlers.clj"
                                 "io.randomseed.bankster.money")
        ;; Non-chunked seq path (doseq macroexpansion differs for lists vs vectors).
        (importer/readers-export r (list "data_readers.clj") "data_readers_edn.clj"
                                 "io/randomseed/bankster/money/reader_handlers.clj"
                                 "io.randomseed.bankster.money")
        (is (true? (.exists (io/file dir "data_readers.clj"))))
        (is (true? (.exists handlers)))))))

(deftest readers-export-skip-data-file-when-data-filename-nil
  (testing "readers-export skips exporting EDN data readers map when data-filename is nil"
    (let [dir        (tmp-dir)
          f          (io/file dir "data_readers.clj")
          _          (spit f "{}")
          handlers   (io/file dir "io" "randomseed" "bankster" "money" "reader_handlers.clj")
          _          (.mkdirs (.getParentFile handlers))
          r          (-> (registry/new-registry)
                         (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))]
      (with-redefs [clojure.java.io/resource (fn [_] (.toURL (.toURI f)))]
        (importer/readers-export r ["data_readers.clj"] nil
                                 "io/randomseed/bankster/money/reader_handlers.clj"
                                 "io.randomseed.bankster.money")
        (is (false? (.exists (io/file dir "data_readers_edn.clj"))))))))

(deftest readers-export-writes-files-and-handlers
  (testing "readers-export writes readers maps and handlers file"
    (let [dir        (tmp-dir)
          f          (io/file dir "data_readers.clj")
          _          (spit f "{}")
          handlers   (io/file dir "io" "randomseed" "bankster" "money" "reader_handlers.clj")
          _          (.mkdirs (.getParentFile handlers))
          r          (-> (registry/new-registry)
                         (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))
          filenames  ["data_readers.clj"]
          data-file  "data_readers_edn.clj"]
      (with-redefs [clojure.java.io/resource (fn [_] (.toURL (.toURI f)))]
        (importer/readers-export r filenames data-file
                                "io/randomseed/bankster/money/reader_handlers.clj"
                                "io.randomseed.bankster.money")
        (is (true? (.exists (io/file dir "data_readers.clj"))))
        (is (true? (.exists (io/file dir "data_readers_edn.clj"))))
        (is (true? (.exists handlers)))
        (is (re-find #"money/crypto" (slurp (io/file dir "data_readers.clj"))))
        (is (re-find #"data-literal-crypto" (slurp handlers)))))))

(deftest readers-export-short-circuits-on-missing-paths
  (testing "when io/resource is nil it short-circuits"
    (let [r (-> (registry/new-registry)
                (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))]
      (with-redefs [clojure.java.io/resource (fn [_] nil)]
        (is (nil? (importer/readers-export r ["data_readers.clj"] "data_readers_edn.clj"
                                           "io/randomseed/bankster/money/reader_handlers.clj"
                                           "io.randomseed.bankster.money"))))))
  (testing "when the resource file has no parent directory, it short-circuits"
    (let [r          (-> (registry/new-registry)
                         (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))
          fake-url   (.toURL (.toURI (java.io.File. "data_readers.clj")))
          orig-file  clojure.java.io/file]
      (with-redefs [clojure.java.io/resource (fn [_] fake-url)
                    clojure.java.io/file     (fn
                                               ([x]
                                                (if (instance? java.net.URL x)
                                                  (java.io.File. "data_readers.clj")
                                                  (orig-file x)))
                                               ([x y]
                                                (orig-file x y))
                                               ([x y & more]
                                                (apply orig-file x y more)))]
        (is (nil? (importer/readers-export r ["data_readers.clj"] "data_readers_edn.clj"
                                           "io/randomseed/bankster/money/reader_handlers.clj"
                                           "io.randomseed.bankster.money"))))))
  (testing "when io/file cannot construct the handlers path, it short-circuits"
    (let [r          (-> (registry/new-registry)
                         (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))
          dir        (tmp-dir)
          f          (io/file dir "data_readers.clj")
          _          (spit f "{}")
          orig-file  clojure.java.io/file]
      (with-redefs [clojure.java.io/resource (fn [_] (.toURL (.toURI f)))
                    clojure.java.io/file     (fn
                                               ([x] (orig-file x))
                                               ([x y]
                                                (if (and (string? x) (string? y))
                                                  nil
                                                  (orig-file x y)))
                                               ([x y & more]
                                                (apply orig-file x y more)))]
        (is (nil? (importer/readers-export r ["data_readers.clj"] "data_readers_edn.clj"
                                           "io/randomseed/bankster/money/reader_handlers.clj"
                                           "io.randomseed.bankster.money")))))))

(deftest readers-export-internal-branches
  (testing "doseq branch: empty filenames list (forced by stubbing io/resource for nil)"
    (let [r        (-> (registry/new-registry)
                       (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))
          dir      (tmp-dir)
          f        (io/file dir "data_readers.clj")
          _        (spit f "{}")
          handlers (io/file dir "io" "randomseed" "bankster" "money" "reader_handlers.clj")
          _        (.mkdirs (.getParentFile handlers))]
      (with-redefs [clojure.java.io/resource (fn [_] (.toURL (.toURI f)))]
        (is (nil? (importer/readers-export r [] nil
                                           "io/randomseed/bankster/money/reader_handlers.clj"
                                           "io.randomseed.bankster.money")))
        (is (true? (.exists handlers))))))
  (testing "when-some branch: data-filename is truthy but io/file returns nil for it"
    (let [r         (-> (registry/new-registry)
                        (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))
          dir       (tmp-dir)
          f         (io/file dir "data_readers.clj")
          _         (spit f "{}")
          handlers  (io/file dir "io" "randomseed" "bankster" "money" "reader_handlers.clj")
          _         (.mkdirs (.getParentFile handlers))
          orig-file clojure.java.io/file]
      (with-redefs [clojure.java.io/resource (fn [_] (.toURL (.toURI f)))
                    clojure.java.io/file     (fn
                                               ([x] (orig-file x))
                                               ([x y]
                                                (if (= "data_readers_edn.clj" (str y))
                                                  nil
                                                  (orig-file x y)))
                                               ([x y & more]
                                                (apply orig-file x y more)))]
        (is (nil? (importer/readers-export r ["data_readers.clj"] "data_readers_edn.clj"
                                           "io/randomseed/bankster/money/reader_handlers.clj"
                                           "io.randomseed.bankster.money"))))))
  (testing "handler-gen returning nil still produces a valid handlers file (preamble only)"
    (let [r        (-> (registry/new-registry)
                       (currency/register (currency/new :crypto/BTC -1 8 :virtual/token :CRYPTO 0)))
          dir      (tmp-dir)
          f        (io/file dir "data_readers.clj")
          _        (spit f "{}")
          handlers (io/file dir "io" "randomseed" "bankster" "money" "reader_handlers.clj")
          _        (.mkdirs (.getParentFile handlers))]
      (with-redefs [clojure.java.io/resource (fn [_] (.toURL (.toURI f)))
                    io.randomseed.bankster.util.importer/handler-gen (fn [_] nil)]
        (is (nil? (importer/readers-export r ["data_readers.clj"] nil
                                           "io/randomseed/bankster/money/reader_handlers.clj"
                                           "io.randomseed.bankster.money")))
        (is (true? (.exists handlers)))))))

(deftest handler-preamble-default-arity
  (testing "handler-preamble 0-arity uses default-handlers-namespace"
    (is (= (importer/handler-preamble importer/default-handlers-namespace)
           (importer/handler-preamble)))))

(deftest registry->map-default-arity
  (testing "registry->map 0-arity uses registry/state"
    (let [r (registry/new-registry)]
      (with-redefs [io.randomseed.bankster.registry/state (fn [] r)]
        (let [m1 (importer/registry->map r)
              m2 (importer/registry->map)]
          ;; :version is time-based (now), so we compare everything else.
          (is (= (dissoc m1 :version) (dissoc m2 :version)))
          (is (string? (:version m1)))
          (is (string? (:version m2))))))))

(deftest registry->map-cur-id->traits-nil-is-treated-as-empty
  (testing "registry->map treats :cur-id->traits nil as empty map"
    (let [r (assoc (registry/new-registry) :cur-id->traits nil)
          m (importer/registry->map r)]
      (is (= {} (:traits m))))))

(deftest registry->map-edn-unreadable-currency-id-guards
  (testing "edn-unreadable-currency-id? handles edge keyword shapes (e.g. empty name) without throwing"
    (let [cid (keyword "crypto" "")
          r   (assoc (registry/new-registry) :cur-id->weight {cid 1})
          m   (importer/registry->map r)]
      (is (= 1 (get-in m [:weights cid]))))))

(deftest registry-to-map-currency-oriented-default-arity
  (testing "registry->map-currency-oriented 0-arity uses registry/state"
    (let [r (-> (registry/new-registry)
                (currency/register (currency/new :EUR 978 2 :iso/fiat :ISO-4217 0)))]
      (with-redefs [io.randomseed.bankster.registry/state (fn [] r)]
        (let [m1 (importer/registry->map-currency-oriented r)
              m2 (importer/registry->map-currency-oriented)]
          (is (= (dissoc m1 :version) (dissoc m2 :version)))
          (is (string? (:version m1)))
          (is (string? (:version m2))))))))

(deftest seed-import-and-high-level-ops
  (testing "seed-import calls currency/config->registry with default resource path"
    (let [seen (atom nil)]
      (with-redefs [io.randomseed.bankster.currency/config->registry
                    (fn [resource-path reg]
                      (reset! seen {:resource resource-path :registry reg})
                      reg)]
        (is (registry/registry? (importer/seed-import)))
        (is (str/includes? (:resource @seen) "seed.edn"))
        (is (registry/registry? (:registry @seen))))))
  (testing "joda->bankster-dump and joda->bankster-export wire together building blocks"
    (let [calls (atom [])]
      (with-redefs [io.randomseed.bankster.util.importer/seed-import (fn [& _] (registry/new-registry))
                    io.randomseed.bankster.util.importer/joda-import (fn [& _] (registry/new-registry))
                    io.randomseed.bankster.util.importer/dump        (fn [& args] (swap! calls conj [:dump args]))
                    io.randomseed.bankster.util.importer/export      (fn [& args] (swap! calls conj [:export args]))
                    io.randomseed.bankster.util.importer/export-currency-oriented
                    (fn [& args] (swap! calls conj [:export-co args]))
                    io.randomseed.bankster.util.importer/merge-registry (fn [& _] (registry/new-registry))]
        (importer/joda->bankster-dump)
        (importer/joda->bankster-export)
        (is (= true (some #(= :dump (first %)) @calls)))
        (is (= true (some #(= :export (first %)) @calls)))
        (is (= true (some #(= :export-co (first %)) @calls)))))))

(deftest invalid-arities-for-dump-and-exporters-throw
  (testing "varargs versions keep the old arity contract (they reject arities > 2)"
    (is (thrown? clojure.lang.ExceptionInfo (importer/dump "a" "b" "c")))
    (is (thrown? clojure.lang.ExceptionInfo (importer/export "a" "b" "c")))
    (is (thrown? clojure.lang.ExceptionInfo (importer/export-currency-oriented "a" "b" "c")))))

(deftest default-registry-prefers-dynamic-over-global
  (testing "default-registry matches registry/get 0-arity semantics (*default* wins over global state)"
    (let [r (registry/new-registry)]
      (binding [registry/*default* r]
        (is (identical? r (#'io.randomseed.bankster.util.importer/default-registry))))))
  (testing "when *default* is nil, default-registry falls back to registry/state"
    (let [r (registry/new-registry)]
      (binding [registry/*default* nil]
        (with-redefs [io.randomseed.bankster.registry/state (fn [] r)]
          (is (identical? r (#'io.randomseed.bankster.util.importer/default-registry))))))))

(deftest hierarchy-map-predicate-covers-false-cases
  (testing "hierarchy-map? returns false for non-hierarchy inputs and true for a real hierarchy map"
    (let [hierarchy-map? #'io.randomseed.bankster.util.importer/hierarchy-map?]
      (is (= false (hierarchy-map? (Object.))))
      (is (= false (hierarchy-map? {})))
      (is (= false (hierarchy-map? {:parents {} :ancestors {}})))          ; missing :descendants
      (is (= false (hierarchy-map? {:parents {} :descendants {}})))        ; missing :ancestors
      (is (= false (hierarchy-map? {:parents {} :ancestors nil :descendants {}})))
      (is (= false (hierarchy-map? {:parents {} :ancestors {} :descendants nil})))
      (is (= false (hierarchy-map? {:parents nil :ancestors {} :descendants {}})))
      (is (= true (hierarchy-map? (make-hierarchy)))))))

(deftest merge-hierarchies-handles-nils
  (testing "merge-hierarchies tolerates nil inputs (treated as empty)"
    (let [merge-hierarchies #'io.randomseed.bankster.util.importer/merge-hierarchies
          h (merge-hierarchies nil nil)]
      (is (map? h))
      (is (= {:domain nil :kind nil :traits nil} (into {} h))))))

(deftest merge-registry-nil-inputs-produce-new-registry
  (testing "merge-registry accepts nil dst/src and falls back to registry/new-registry"
    (let [r (importer/merge-registry nil nil false)]
      (is (true? (registry/registry? r))))))

(deftest merge-registry-iso-like-and-weight-branch-coverage
  (testing "iso-like? param true but non-ISO domain => treated as not iso-like"
    (let [dst    (registry/new-registry)
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :crypto/AAA -1 8 :virtual/token :CRYPTO 0)))
          merged (importer/merge-registry dst src false nil true)]
      (is (true? (currency/defined? :crypto/AAA merged)))))
  (testing "new currency branch tolerates nil :cur-id->weight (treated as empty)"
    (let [dst    (registry/new-registry)
          src0   (-> (registry/new-registry)
                     (currency/register (currency/new :crypto/BBB -1 8 :virtual/token :CRYPTO 0)))
          src    (assoc src0 :cur-id->weight nil)
          merged (importer/merge-registry dst src false nil false)]
      (is (true? (currency/defined? :crypto/BBB merged)))))
  (testing "iso-like mode normalizes legacy-domain currencies to :iso-4217-legacy/* IDs"
    (let [dst    (registry/new-registry)
          src    (-> (registry/new-registry)
                     ;; No namespace in the ID, but legacy ISO domain: should become :iso-4217-legacy/QQQ.
                     (currency/register (currency/new :QQQ 1 2 :iso/fiat :ISO-4217-LEGACY 0)))
          merged (importer/merge-registry dst src false nil true)]
      (is (true? (currency/defined? :iso-4217-legacy/QQQ merged)))
      (is (false? (currency/defined? :QQQ merged)))))
  (testing "iso-like mode can migrate legacy ID -> ISO code when src is ISO and dst has only legacy"
    (let [dst    (-> (registry/new-registry)
                     (currency/register (currency/new :iso-4217-legacy/AAA 1 2 :iso/fiat :ISO-4217-LEGACY 0)))
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :AAA 1 2 :iso/fiat :ISO-4217 0)))
          merged (importer/merge-registry dst src false nil true)]
      (is (nil? (get (:cur-id->cur merged) :iso-4217-legacy/AAA)))
      (is (some? (get (:cur-id->cur merged) :AAA)))))
  (testing "rename path: when existing-countries is nil, it still merges src countries (uses #{})"
    (let [dst    (-> (registry/new-registry)
                     (currency/register (currency/new :iso-4217-legacy/CCC 1 2 :iso/fiat :ISO-4217-LEGACY 0)))
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :CCC 1 2 :iso/fiat :ISO-4217 0) [:PL]))
          merged (importer/merge-registry dst src false nil true)]
      (is (= #{:PL} (currency/countries :CCC merged)))))
  (testing "existing currency: explicit src weight via iso-code-id selects src-exp? branch"
    (let [cid    :iso-4217-legacy/WWW
          dst0   (-> (registry/new-registry)
                     (currency/register (currency/new cid 1 0 :iso/fiat :ISO-4217-LEGACY 0)))
          src0   (-> (registry/new-registry)
                     (currency/register (currency/new cid 1 0 :iso/fiat :ISO-4217-LEGACY 0)))
          ;; Force explicitness through the base map, but only under ISO code (:WWW), not legacy ID.
          src    (assoc src0 :cur-id->weight {:WWW 5})
          dst    (assoc dst0 :cur-id->weight nil)
          merged (importer/merge-registry dst src false nil true)]
      (is (= 5 (currency/weight (currency/unit cid merged))))))
  (testing "existing currency weight base selection: src-exp? false and dst-exp? true uses dst hint weight"
    (let [dst    (-> (registry/new-registry)
                     (currency/register (currency/new :W 1 0 :FIAT :TEST 7)))
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :W 1 0 :FIAT :TEST 0)))
          ;; Make weight base maps absent to force relying on currency meta weight hints.
          dst    (assoc dst :cur-id->weight nil)
          src    (assoc src :cur-id->weight nil)
          merged (importer/merge-registry dst src false nil false)]
      (is (= 7 (currency/weight (currency/unit :W merged))))))
  (testing "existing currency weight base selection: when both sides are non-explicit, base weight is 0"
    (let [dst    (-> (registry/new-registry)
                     (currency/register (currency/new :W2 1 0 :FIAT :TEST 0)))
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :W2 1 0 :FIAT :TEST 0)))
          dst    (assoc dst :cur-id->weight nil)
          src    (assoc src :cur-id->weight nil)
          merged (importer/merge-registry dst src false nil false)]
      (is (= 0 (currency/weight (currency/unit :W2 merged))))))
  (testing "new currency branch: iso-like weight lookup can use iso-code-id when dst-id differs"
    (let [cid    :iso-4217-legacy/ZZZ
          dst    (registry/new-registry)
          src0   (-> (registry/new-registry)
                     (currency/register (currency/new cid 1 0 :iso/fiat :ISO-4217-LEGACY 0)))
          ;; Put weight under ISO code (:ZZZ), not under legacy ID.
          src    (assoc src0 :cur-id->weight {:ZZZ 11})
          merged (importer/merge-registry dst src false nil true)]
      (is (= 11 (currency/weight (currency/unit cid merged)))))))

(deftest merge-registry-existing-branch-covers-src-weight-and-noop-update
  (testing "existing branch: src-id->weight contains dst-id (covers src-w? and src-w first branches)"
    (let [dst-cur (currency/new :NOOPW 1 0 :FIAT :TEST 9)
          src-cur (currency/new :NOOPW 1 0 :FIAT :TEST 9)
          dst0    (-> (registry/new-registry) (currency/register dst-cur))
          src0    (-> (registry/new-registry) (currency/register src-cur))
          ;; Make both weights explicit and equal under the same key to keep weight-changed? false.
          dst     (assoc dst0 :cur-id->weight {:NOOPW 9} :cur-id->traits nil :cur-id->ctr-ids nil :cur-id->localized nil)
          src     (assoc src0 :cur-id->weight {:NOOPW 9} :cur-id->traits nil :cur-id->ctr-ids nil :cur-id->localized nil)
          merged  (importer/merge-registry dst src false nil false)]
      ;; Nothing changes for the currency record itself (updated? is false).
      (is (identical? (get (:cur-id->cur dst) :NOOPW)
                      (get (:cur-id->cur merged) :NOOPW))))))

(deftest merge-registry-existing-branch-src-traits-short-circuit
  (testing "src-traits uses dst-id key when present (second branch of `or` is not evaluated)"
    (let [dst   (-> (registry/new-registry)
                    (currency/register (currency/new :TR 1 0 :FIAT :TEST 0)))
          src0  (-> (registry/new-registry)
                    (currency/register (currency/new :TR 1 0 :FIAT :TEST 0)))
          src   (assoc src0 :cur-id->traits {:TR #{:t/x}})
          merged (importer/merge-registry dst src false nil false)]
      (is (= true (currency/has-trait? :TR :t/x merged))))))

(deftest merge-registry-existing-branch-updated-is-false-when-nothing-changes
  (testing "updated? becomes false when all change detectors are false (exercise full `or` evaluation)"
    (let [c    (currency/new :UNCHANGED -1 0 :virtual/token :CRYPTO 0)
          dst  (-> (registry/new-registry)
                   (assoc :cur-id->cur {:UNCHANGED c}
                          :cur-id->weight nil
                          :cur-id->ctr-ids nil
                          :cur-id->localized nil
                          :cur-id->traits nil))
          src  (-> (registry/new-registry)
                   (assoc :cur-id->cur {:UNCHANGED c}
                          :cur-id->weight nil
                          :cur-id->ctr-ids nil
                          :cur-id->localized nil
                          :cur-id->traits nil))
          merged (importer/merge-registry dst src false nil false)]
      (is (identical? c (get (:cur-id->cur merged) :UNCHANGED)))
      (is (nil? (get (:cur-id->traits merged) :UNCHANGED))))))

(deftest merge-registry-existing-branch-existing-traits-are-used-when-src-has-none
  (testing "traits merge uses existing-traits when src-traits is nil"
    (let [c    (currency/new :TR2 -1 0 :virtual/token :CRYPTO 0)
          dst  (-> (registry/new-registry)
                   (assoc :cur-id->cur {:TR2 c}
                          :cur-id->traits {:TR2 #{:t/existing}}
                          :cur-id->weight nil
                          :cur-id->ctr-ids nil
                          :cur-id->localized nil))
          src  (-> (registry/new-registry)
                   (assoc :cur-id->cur {:TR2 c}
                          :cur-id->traits nil
                          :cur-id->weight nil
                          :cur-id->ctr-ids nil
                          :cur-id->localized nil))
          merged (importer/merge-registry dst src false nil false)]
      (is (= true (currency/has-trait? :TR2 :t/existing merged))))))

(deftest merge-registry-updated-can-be-triggered-by-countries-difference
  (testing "updated? can become true due to countries mismatch (with earlier clauses false)"
    (let [c      (currency/new :COUNTRY -1 0 :virtual/token :CRYPTO 0)
          dst    (-> (registry/new-registry)
                     (assoc :cur-id->cur {:COUNTRY c}
                            :cur-id->ctr-ids {:COUNTRY #{:PL}}
                            :cur-id->weight nil
                            :cur-id->localized nil
                            :cur-id->traits nil))
          src    (-> (registry/new-registry)
                     (assoc :cur-id->cur {:COUNTRY c}
                            :cur-id->ctr-ids {:COUNTRY #{:DE}}
                            :cur-id->weight nil
                            :cur-id->localized nil
                            :cur-id->traits nil))
          merged (importer/merge-registry dst src false nil false)]
      (is (= #{:DE} (currency/countries :COUNTRY merged))))))

(deftest merge-registry-merges-hierarchies-and-ext
  (testing "merges :hierarchies and :ext while keeping existing currency data"
    (let [dst (-> (registry/new-registry)
                  (assoc :hierarchies (bankster/->CurrencyHierarchies
                                       (derive (make-hierarchy) :ISO-4217-LEGACY :ISO-4217)
                                       (derive (make-hierarchy) :COMBANK :FIAT)
                                       (make-hierarchy)))
                  (assoc :ext {:dst 1 :shared :dst}))
          src (-> (registry/new-registry)
                  (assoc :hierarchies (bankster/->CurrencyHierarchies
                                       (derive (make-hierarchy) :ISO-4217-SUPER :ISO-4217)
                                       (derive (make-hierarchy) :CENTRALBANK :FIAT)
                                       (make-hierarchy)))
                  (assoc :ext {:src 2 :shared :src}))
          merged (importer/merge-registry dst src)
          hs     (:hierarchies merged)]
      (is (= {:dst 1 :src 2 :shared :src} (:ext merged)))
      (is (isa? (:domain hs) :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:domain hs) :ISO-4217-SUPER :ISO-4217))
      (is (isa? (:kind hs) :COMBANK :FIAT))
      (is (isa? (:kind hs) :CENTRALBANK :FIAT)))))

(deftest merge-registry-verbose-reports-new-currencies
  (testing "when verbose? is truthy it reports currencies present in src but not in dst"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217))
                  (currency/register (currency/new :ABC 999 2 :FIAT :ISO-4217)))
          out (with-out-str (importer/merge-registry dst src true))]
      (is (re-find #"New currency: ABC" out))
      (is (not (re-find #"New currency: EUR" out))))))

(deftest merge-registry-verbose-reports-updated-currencies
  (testing "when verbose? is truthy it reports only real updates (post-preserve-fields)"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 3 :FIAT :ISO-4217)))
          out (with-out-str (importer/merge-registry dst src true))]
      (is (re-find #"Updated currency: EUR" out))
      (is (not (re-find #"New currency: EUR" out))))))

(deftest merge-registry-preserves-currency-fields-when-replacing
  (testing "preserve-fields keeps selected fields from dst when replacing currency in dst"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)))
          src (-> (registry/new-registry)
                  ;; Same ID, but different domain/kind/scale. We'll preserve :domain and :kind from dst.
                  (currency/register (currency/new :EUR 978 9 :DECENTRALIZED :CRYPTO)))
          merged (importer/merge-registry dst src false [:domain :kind])
          eur    (get (:cur-id->cur merged) :EUR)]
      (is (= :ISO-4217 (:domain eur)))
      (is (= :FIAT (:kind eur)))
      (is (= 9 (:scale eur))))))

(deftest merge-registry-updates-domain-index
  (testing "cur-dom->curs reflects domain add/change during merge"
    (let [dst    (-> (registry/new-registry)
                     (currency/register (currency/new :AAA 1 2 :FIAT :DOM-A)))
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :AAA 1 2 :FIAT :DOM-B))
                     (currency/register (currency/new :BBB 2 2 :FIAT :DOM-C)))
          merged (importer/merge-registry dst src false nil false)
          dom-a  (registry/currency-domain->currencies :DOM-A merged)
          dom-b  (registry/currency-domain->currencies :DOM-B merged)
          dom-c  (registry/currency-domain->currencies :DOM-C merged)]
      (is (not (seq dom-a)))
      (is (= #{:AAA} (set (map currency/id dom-b))))
      (is (= #{:BBB} (set (map currency/id dom-c)))))))

(deftest merge-registry-skips-update-when-equal-after-preserve
  (testing "does not update currency if it becomes equal after preserve-fields (prevents data loss)"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)
                                     nil
                                     {:en {:name "Euro"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :DECENTRALIZED :CRYPTO)))
          preserve [:domain :kind ::importer/localized]
          out (with-out-str (importer/merge-registry dst src true preserve))
          merged (importer/merge-registry dst src false preserve)]
      (is (= "Euro" (currency/localized-property :name :EUR :en merged)))
      (is (not (re-find #"Updated currency: EUR" out))))))

(deftest merge-registry-preserves-localized-and-countries
  (testing "preserve-fields can preserve localized properties and countries with sentinels"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 2 :FIAT :ISO-4217)
                                     [:PL]
                                     {:en {:name "Euro"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :EUR 978 3 :FIAT :ISO-4217)
                                     [:DE]
                                     {:en {:name "Eur0"}}))
          merged (importer/merge-registry dst src false [::importer/localized ::importer/countries])
          eur    (get (:cur-id->cur merged) :EUR)]
      (is (= 3 (:scale eur)))
      (is (= #{:PL} (currency/countries :EUR merged)))
      (is (= "Euro" (currency/localized-property :name :EUR :en merged))))))

(deftest merge-registry-iso-like-renames-iso-to-legacy
  (testing "iso-like? promotes legacy ISO currencies to :iso-4217-legacy/* IDs and migrates dst data"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :ADP 20 0 :FIAT :ISO-4217)
                                     [:AD]
                                     {:en {:name "Andorran peseta"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :iso-4217-legacy/ADP 20 0 :FIAT)))
          preserve [:domain :kind ::importer/localized ::importer/countries]
          merged (importer/merge-registry dst src false preserve true)
          adp    (get (:cur-id->cur merged) :iso-4217-legacy/ADP)]
      (is (nil? (get (:cur-id->cur merged) :ADP)))
      (is (some? adp))
      (is (= :ISO-4217-LEGACY (:domain adp)))
      (is (= (int importer/default-legacy-weight) (currency/weight adp)))
      (is (= #{:AD} (currency/countries :iso-4217-legacy/ADP merged)))
      (is (= "Andorran peseta"
             (currency/localized-property :name :iso-4217-legacy/ADP :en merged))))))

(deftest merge-registry-legacy-weight-preserves-nonzero-existing
  (testing "legacy currencies default to a high weight unless dst already had a non-zero weight"
    (let [dst (-> (registry/new-registry)
                  (currency/register (currency/new :ADP 20 0 :FIAT :ISO-4217 7)
                                     [:AD]
                                     {:en {:name "Andorran peseta"}}))
          src (-> (registry/new-registry)
                  (currency/register (currency/new :iso-4217-legacy/ADP 20 0 :FIAT)))
          preserve [:domain :kind ::importer/localized ::importer/countries]
          merged (importer/merge-registry dst src false preserve true)
          adp    (get (:cur-id->cur merged) :iso-4217-legacy/ADP)]
      (is (some? adp))
      (is (= 7 (currency/weight adp))))))

(deftest merge-registry-legacy-weight-defaults-for-new-currency
  (testing "a newly merged legacy currency gets the default high weight when its weight is 0"
    (let [dst    (registry/new-registry)
          src    (-> (registry/new-registry)
                     (currency/register (currency/new :iso-4217-legacy/AAA 999 2 :FIAT)))
          merged (importer/merge-registry dst src)
          aaa    (get (:cur-id->cur merged) :iso-4217-legacy/AAA)]
      (is (some? aaa))
      (is (= :ISO-4217-LEGACY (:domain aaa)))
      (is (= (int importer/default-legacy-weight) (currency/weight aaa))))))

(deftest merge-registry-legacy-weight-explicit-zero-preserved-from-dst
  (testing "explicit weight 0 in dst means legacy should stay canonical (do not bump to default)"
    (let [dst-cur (currency/with-weight (currency/new :ADP 20 0 :FIAT :ISO-4217 0) 0)
          dst     (-> (registry/new-registry)
                      (currency/register dst-cur [:AD] {:en {:name "Andorran peseta"}}))
          src     (-> (registry/new-registry)
                      (currency/register (currency/new :iso-4217-legacy/ADP 20 0 :FIAT)))
          preserve [:domain :kind ::importer/localized ::importer/countries]
          merged  (importer/merge-registry dst src false preserve true)
          adp     (get (:cur-id->cur merged) :iso-4217-legacy/ADP)]
      (is (some? adp))
      (is (= 0 (currency/weight adp)))
      (is (= true (contains? (:cur-id->weight merged) :iso-4217-legacy/ADP))))))

(deftest merge-registry-legacy-weight-explicit-zero-preserved-from-src
  (testing "explicit weight 0 in src means legacy should stay canonical (do not bump to default)"
    (let [src-cur (currency/with-weight (currency/new :iso-4217-legacy/AAA 999 2 :FIAT) 0)
          src     (-> (registry/new-registry)
                      (currency/register src-cur))
          merged  (importer/merge-registry (registry/new-registry) src)
          aaa     (get (:cur-id->cur merged) :iso-4217-legacy/AAA)]
      (is (some? aaa))
      (is (= :ISO-4217-LEGACY (:domain aaa)))
      (is (= 0 (currency/weight aaa)))
      (is (= true (contains? (:cur-id->weight merged) :iso-4217-legacy/AAA))))))

(deftest merge-registry-merges-extra-hierarchy-keys
  (testing "merges hierarchy values for any keys present in :hierarchies (record may grow)"
    (let [dst (-> (registry/new-registry)
                  (assoc :hierarchies (-> (bankster/->CurrencyHierarchies (make-hierarchy) (make-hierarchy) (make-hierarchy))
                                          (assoc :foo (derive (make-hierarchy) :X :Y))))
                  (assoc :ext {}))
          src (-> (registry/new-registry)
                  (assoc :hierarchies (-> (bankster/->CurrencyHierarchies (make-hierarchy) (make-hierarchy) (make-hierarchy))
                                          (assoc :foo (derive (make-hierarchy) :X :Z)
                                                :bar (derive (make-hierarchy) :A :B))))
                  (assoc :ext {}))
          merged (importer/merge-registry dst src)
          hs     (:hierarchies merged)
          foo-h  (get hs :foo)
          bar-h  (get hs :bar)]
      (is (isa? foo-h :X :Y))
      (is (isa? foo-h :X :Z))
      (is (isa? bar-h :A :B)))))

(deftest config->registry-loads-hierarchies
  (testing "config->registry loads :hierarchies from EDN config"
    (let [r  (currency/config->registry "io/randomseed/bankster/test_config_with_hierarchies.edn")
          hs (:hierarchies r)]
      (is (isa? (:domain hs)     :ISO-4217-LEGACY :ISO-4217))
      (is (isa? (:kind hs)       :child :parent))
      (is (isa? (get hs :traits) :stable :asset))
      (is (isa? (get hs :traits) :stable :fiat)))))

(deftest registry->map-exports-hierarchies-as-parent-maps
  (testing "registry->map exports currency hierarchies as parent-maps"
    (let [r (currency/config->registry "io/randomseed/bankster/test_config_with_hierarchies.edn")
          m (importer/registry->map r)]
      (is (= [] (:propagate-keys m)))
      (is (= :ISO-4217      (get-in m [:hierarchies :domain :ISO-4217-LEGACY])))
      (is (= :parent        (get-in m [:hierarchies :kind :child])))
      (is (= [:asset :fiat] (get-in m [:hierarchies :traits :stable]))))))

(deftest registry->map-always-exports-propagate-keys
  (testing "registry->map always exports :propagate-keys (as a vector)"
    (let [m (importer/registry->map (registry/new-registry))]
      (is (= [] (:propagate-keys m))))

    (let [r (currency/config->registry "io/randomseed/bankster/test_config_propagate_keys.edn")
          m (importer/registry->map r)]
      (is (= [:comment] (:propagate-keys m))))))

(deftest registry->map-exports-explicit-weight-zero
  (testing "registry->map exports :weight 0 only when it was explicitly present in source"
    (let [cur-exp0  (currency/with-weight (currency/new :AAA 1 2 :FIAT :ISO-4217 0) 0)
          cur-impl0 (currency/new :BBB 2 2 :FIAT :ISO-4217 0)
          r   (-> (registry/new-registry)
                  (currency/register cur-exp0)
                  (currency/register cur-impl0))
          m   (importer/registry->map r)
          ws  (:weights m)]
      (is (= 0 (get ws :AAA)))
      (is (= false (contains? ws :BBB))))))

(deftest registry->map-stringifies-unreadable-currency-ids
  (testing "registry->map stringifies currency IDs that EDN cannot read (e.g. :crypto/1INCH)"
    (let [cid (keyword "crypto" "1INCH")
          r   (currency/config->registry "io/randomseed/bankster/test_config_string_currency_ids.edn")
          m   (importer/registry->map r)
          ck  "crypto/1INCH"]
      (is (= true (contains? (:currencies m) ck)))
      (is (= 10 (get-in m [:weights ck])))
      (is (= true (some? (get-in m [:localized ck :en :name]))))
      (is (= #{:PL} (currency/countries cid r)))
      (is (= ck (get-in m [:countries :PL])))
      (is (= true (some #(= % :token/erc20) (get-in m [:traits ck])))))))

(deftest map->currency-oriented-embeds-properties-and-keeps-orphans
  (testing "map->currency-oriented embeds per-currency properties and keeps only orphaned top-level entries"
    (let [m  {:version    "v"
              :currencies (sorted-map :AAA {:scale 2}
                                      :BBB {:scale 2})
              :countries  (sorted-map :PL :AAA
                                      :DE :CCC)
              :localized  (sorted-map :AAA {:* {:name "A"}}
                                      :DDD {:* {:name "D"}})
              :traits     (sorted-map :BBB [:t/b]
                                      :EEE [:t/e])
              :hierarchies (sorted-map)}
          m2 (importer/map->currency-oriented m)]
      (is (= [:PL] (get-in m2 [:currencies :AAA :countries])))
      (is (= false (contains? (get-in m2 [:currencies :BBB]) :countries)))
      (is (= {:* {:name "A"}} (get-in m2 [:currencies :AAA :localized])))
      (is (= [:t/b] (get-in m2 [:currencies :BBB :traits])))
      (is (= {:DE :CCC} (:countries m2)))
      (is (= {:DDD {:* {:name "D"}}} (:localized m2)))
      (is (= {:EEE [:t/e]} (:traits m2))))))

(deftest config->registry-populates-inline-currency-properties
  (testing "currency entries can carry inline :countries/:localized/:traits that are merged into top-level branches"
    (let [r (currency/config->registry "io/randomseed/bankster/test_config_inline_currency_props.edn")]
      ;; countries: from top-level (:DE, :PL) plus inline (:PL, :AD)
      (is (= #{:AD :DE :PL} (currency/countries :crypto/AMLT r)))
      ;; localized: deep merge of locale keys
      (is (= "Token AML" (currency/name :crypto/AMLT :pl r)))
      (is (= "AML Token" (currency/name :crypto/AMLT :en r)))
      ;; traits: merged (not replaced)
      (is (= true (currency/has-trait? :crypto/AMLT :token/costam r)))
      (is (= true (currency/has-trait? :crypto/AMLT :token/erc20 r)))
      ;; Regression: inline keys should not leak into Currency extmap when loading from config.
      (let [cur (currency/unit :crypto/AMLT r)]
        (is (= false (contains? (into {} cur) :countries)))
        (is (= false (contains? (into {} cur) :localized)))
        (is (= false (contains? (into {} cur) :traits)))))))

(deftest config->registry-propagates-selected-extra-currency-keys
  (testing ":propagate-keys controls which extra keys from currency maps are preserved on Currency"
    (let [r (currency/config->registry "io/randomseed/bankster/test_config_propagate_keys.edn")
          a (currency/unit :AAA r)
          b (currency/unit :BBB r)]
      ;; Global propagate-keys allowlist for :AAA: propagate :comment, but not :foo.
      (is (= "A" (get (into {} a) :comment)))
      (is (= false (contains? (into {} a) :foo)))
      ;; Per-currency override for :BBB: propagate :foo, but not :comment.
      (is (= 2 (get (into {} b) :foo)))
      (is (= false (contains? (into {} b) :comment)))
      ;; Directive key never propagates.
      (is (= false (contains? (into {} b) :propagate-keys))))))

(deftest config->registry-accepts-string-currency-ids
  (testing "string currency IDs are accepted in EDN configs (including namespaced digit-start)"
    (let [cid (keyword "crypto" "1INCH")
          r   (currency/config->registry "io/randomseed/bankster/test_config_string_currency_ids.edn")]
      (is (= true (currency/defined? cid r)))
      (is (= :CRYPTO (currency/domain cid r)))
      (is (= :virtual/token (currency/kind cid r)))
      (is (= 18 (currency/sc cid r)))
      (is (= 10 (currency/weight cid r)))
      (is (= #{:PL} (currency/countries cid r)))
      (is (= "1inch" (currency/name cid :en r)))
      (is (= true (currency/has-trait? cid :token/erc20 r))))))

(deftest seed-import-explicit-resource-path-is-passed-through
  (testing "seed-import passes explicit resource path through to currency/config->registry"
    (let [calls (atom [])]
      (with-redefs [io.randomseed.bankster.currency/config->registry (fn [path r]
                                                                       (swap! calls conj [path r])
                                                                       r)]
        (let [r (importer/seed-import "seed-x.edn")]
          (is (instance? io.randomseed.bankster.Registry r))
          (is (= 1 (count @calls)))
          (is (= "seed-x.edn" (ffirst @calls))))))))

(deftest localized->register-input-handles-wildcard-locale
  (testing "localized->register-input keeps :* as-is and converts Locale keys to keyword locale IDs"
    (let [f  #'io.randomseed.bankster.util.importer/localized->register-input
          in {:*                       {:name "Any"}
              java.util.Locale/ENGLISH {:name "English"}}
          out (f in)]
      (is (= #{:* :en} (set (keys out))))
      (is (= {:name "Any"} (get out :*)))
      (is (= {:name "English"} (get out :en))))))

(deftest registry->map-covers-or-branches-and-empty-traits
  (testing "registry->map handles nil per-currency maps and drops empty traits entries"
    (let [r  (-> (registry/new-registry)
                 (currency/register (currency/new :1INCH -1 8 :virtual/token :CRYPTO 0)))
          r2 (-> r
                 ;; Ensure both branches of (when-some [ts (traits->map ts)]) are covered:
                 ;; - empty traits -> dropped
                 ;; - non-empty traits -> kept, and currency-id->edn is invoked for an unnamespaced keyword
                 (assoc :cur-id->traits {:1INCH []
                                         :2INCH [:t/x]})
                 (assoc :cur-id->localized nil)
                 (assoc :cur-id->weight nil)
                 (assoc :cur-id->cur nil)
                 (assoc :ctr-id->cur nil)
                 (update :hierarchies assoc
                         :test-empty {:parents nil}
                         :test-nil   {:parents {:A nil}}))
          m  (importer/registry->map r2)]
      ;; empty traits entry is dropped by (keep ...) branch
      (is (= false (contains? (:traits m) :1INCH)))
      ;; non-empty traits entry is kept and remains a keyword key (no stringification)
      (is (= true (contains? (:traits m) :2INCH)))
      ;; hierarchies normalization: missing/empty parents maps become empty parent maps
      (is (= (sorted-map) (get-in m [:hierarchies :test-empty])))
      (is (= (sorted-map :A nil) (get-in m [:hierarchies :test-nil]))))))

(deftest hierarchy-coercion-branches-in-importer
  (testing "->hierarchy accepts parent-map specs and rejects invalid ones with ex-data"
    (let [->h #'io.randomseed.bankster.util.importer/->hierarchy]
      (is (map? (->h {:a [:b :c]
                      :d :e}
                     :test)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid currency hierarchy specification"
           (->h 1 :test))))))
