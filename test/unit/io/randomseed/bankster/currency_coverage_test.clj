(ns

    ^{:doc    "bankster library, currency coverage tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.currency-coverage-test

  (:require [clojure.test                    :refer [deftest testing is are]]
            [io.randomseed.bankster.currency :as c]
            [io.randomseed.bankster.money    :as m]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.scale    :as scale])

  (:import (io.randomseed.bankster Currency Registry)
           (java.math RoundingMode)
           (java.util Locale)))

(defn- with-temp-global-registry
  [^Registry r f]
  (let [orig (registry/state)]
    (try
      (registry/set! r)
      (f)
      (finally
        (registry/set! orig)))))

(defn- mk-test-registry
  []
  (let [r0   (registry/new)
        eur  (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
        pln  (c/new :PLN 985 2 :iso/fiat :ISO-4217 7)
        usdt (c/new :crypto/USDT c/no-numeric-id 6 :virtual/stable :CRYPTO 3)]
    (-> r0
        (c/register eur :PL {:* {:name "Euro"}})
        (c/register pln [:PL] {:* {:name "Złoty"}})
        (c/register usdt [:US] {:* {:name "Tether"}})
        (c/add-traits eur [:fiat])
        (c/add-traits usdt [:stable]))))

(deftest currency-basic-helpers-coverage
  (testing "val-auto-scaled? and macro helper"
    (is (true? (c/val-auto-scaled? c/auto-scaled)))
    (is (false? (c/val-auto-scaled? 0)))
    (is (true? (c/val-auto-scaled*? c/auto-scaled))))

  (testing "iso-strict-code? exercises short-circuits"
    (are [x ok] (= ok (c/iso-strict-code? x))
      nil          false
      "EUR"        false
      :crypto/EUR  false
      :EURO        false
      :EuR         false
      :EUR         true))

  (testing "normalize-id-input and lookup-id-keys do not intern unexpectedly"
    (let [normalize-id-input #'io.randomseed.bankster.currency/normalize-id-input
          lookup-id-keys     #'io.randomseed.bankster.currency/lookup-id-keys]
      ;; Ensure the candidate keyword exists so Keyword/find can succeed.
      (is (= :crypto/USDT :crypto/USDT))
      (is (= :PLN (normalize-id-input :pln)))
      (is (= :crypto/USDT (normalize-id-input :crypto/usdt)))
      (is (= :CrYpTo/USDT (normalize-id-input "CrYpTo/usdt")))
      (is (= :BTC (normalize-id-input 'btc)))
      (is (= :BTC (normalize-id-input  "btc")))
      (is (= [:crypto/USDT :crypto/usdt] (lookup-id-keys :crypto/usdt)))))

  (testing "weight metadata helpers"
    (let [currency-weight #'io.randomseed.bankster.currency/currency-weight
          with-weight*    #'io.randomseed.bankster.currency/with-weight*
          c0              (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
          c1              (with-weight* c0 7)
          c2              (with-weight* c1 0)]
      (is (= 0 (currency-weight c0)))
      (is (= 7 (currency-weight c1)))
      (is (= 0 (currency-weight c2)))))

  (testing "ISO strict currency predicate"
    (let [iso-strict-currency? #'io.randomseed.bankster.currency/iso-strict-currency?]
      (is (false? (iso-strict-currency? nil)))
      (is (true?  (iso-strict-currency? (c/new :EUR 978 2 :iso/fiat :ISO-4217))))
      (is (false? (iso-strict-currency? (c/new :EUR 0 2 :iso/fiat :ISO-4217))))
      (is (false? (iso-strict-currency? (c/new :crypto/EUR c/no-numeric-id 2 nil :CRYPTO)))))))

(deftest monetary-protocol-and-resolution-coverage
  (let [r (mk-test-registry)]
    (registry/with r
      (testing "Currency resolves against the registry (id + numeric)"
        (let [eur (c/unit :EUR)]
          (is (instance? Currency eur))
          (is (= eur (c/resolve eur)))
          (is (= #{eur} (c/resolve-all eur)))))

      (testing "Keyword resolution covers ISO-4217 namespace special-case"
        (is (= :EUR (.id ^Currency (c/resolve :iso-4217/EUR))))
        (is (= #{#currency EUR} (c/resolve-all :ISO-4217/EUR)))
        (is (= :EUR (c/id :iso-4217/EUR r))))

      (testing "Keyword resolution covers non-namespaced code and namespaced ID"
        (is (= :crypto/USDT (.id ^Currency (c/resolve :USDT))))
        (is (= :crypto/USDT (.id ^Currency (c/resolve :crypto/usdt))))
        (is (some #(= :crypto/USDT (.id ^Currency %)) (c/resolve-all :USDT))))

      (testing "java.util.Currency roundtrip and of-id"
        (let [jc (java.util.Currency/getInstance "EUR")]
          (is (= :EUR (c/to-id jc)))
          (is (= "EUR" (c/to-code-str jc)))
          (is (= 978 (c/to-numeric-id jc)))
          (is (= :EUR (.id ^Currency (c/of-id jc r))))
          (is (= :EUR (.id ^Currency (c/unit jc r))))))

      (testing "Number paths: resolve/resolve-all/defined?/present? and inconsistency fallback"
        (let [n  978
              r' (-> r
                     ;; Make it inconsistent: remove cur-nr->cur but keep cur-nr->curs
                     (assoc :cur-nr->cur {})
                     (assoc :cur-nr->curs {978 [(c/unit :EUR r)]}))]
          (registry/with r'
            (is (= :EUR (.id ^Currency (c/resolve n r'))))
            (is (= #{:EUR} (set (map (fn [^Currency x] (.id x)) (c/resolve-all n r')))))
            (is (true? (c/defined? n r')))
            (is (true? (c/present? n r'))))))

      (testing "Map-based lookup is allowed for resolve/unit and rejected for strict ops"
        (is (= :EUR (.id ^Currency (c/resolve {:id :EUR} r))))
        (is (= :EUR (.id ^Currency (c/unit {:id :EUR} r))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Currency maps cannot be used"
                              (c/id {:id :EUR} r)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Currency maps cannot be used"
                              (c/of-id {:id :EUR} r))))

      (testing "Unknown object representation is not supported by Monetary protocol"
        (let [x (Object.)]
          (is (thrown? IllegalArgumentException (c/to-id x)))
          (is (thrown? IllegalArgumentException (c/definitive? x)))
          (is (thrown? IllegalArgumentException (c/resolve x r)))
          (is (thrown? IllegalArgumentException (c/resolve-all x r))))))))

(deftest macros-and-parsing-coverage
  (let [r (mk-test-registry)]
    (testing "parse-currency-code branches"
      (is (= {:id :EUR} (c/parse-currency-code {:id 'EUR})))
      (is (= :EUR (c/parse-currency-code 'EUR)))
      (is (= 'EUR (c/parse-currency-code 'EUR {'EUR 1}))))

    (testing "attempt, attempt*, and with-attempt cover both definitive and resolving branches"
      (registry/with r
        (is (= :EUR (.id ^Currency (c/attempt :EUR))))
        (is (= :EUR (.id ^Currency (c/attempt* :EUR))))
        (is (= :EUR (.id ^Currency (c/attempt :iso-4217/EUR))))
        (is (nil? (c/attempt :NOPE)))
        (is (= :EUR (.id ^Currency (c/attempt :EUR r))))
        (is (= :EUR (.id ^Currency (c/attempt* :EUR r))))
        (is (= false (c/with-attempt :NOPE r x (.id x))))
        (is (= :EUR (c/with-attempt :EUR r x (.id x))))
        (is (= :EUR (c/with-attempt :EUR r [x] (.id x))))
        (let [with-attempt-macro (var-get #'io.randomseed.bankster.currency/with-attempt)]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"with-attempt expects"
                                (with-attempt-macro nil nil :EUR 'r '[x y] '(.id x)))))))

    (testing "of macro covers map input and registry arg"
      (registry/with r
        (is (= :EUR (.id ^Currency (c/of :EUR))))
        (is (= :EUR (.id ^Currency (c/of {:id :EUR}))))
        (is (= :EUR (.id ^Currency (c/of :EUR r))))))))

(deftest registry-ops-and-predicates-coverage
  (testing "register/update/unregister and ! variants"
    (let [r0  (registry/new)
          eur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
      (is (instance? Registry (c/register r0 eur)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already exists"
                            (c/register (c/register r0 eur) eur)))
      (is (instance? Registry (c/register (c/register r0 eur) eur true)))
      (is (instance? Registry (c/update r0 eur)))
      (is (instance? Registry (c/unregister (c/register r0 eur) eur)))
      (is (instance? Registry (c/remove-countries (c/register r0 eur :PL) [:PL])))
      (is (instance? Registry (c/add-countries (c/register r0 eur) eur :PL)))
      (is (instance? Registry (c/add-localized-properties (c/register r0 eur) eur {:* {:name "Euro"}})))
      (is (instance? Registry (c/remove-localized-properties (c/register r0 eur) eur)))
      (is (instance? Registry (c/add-traits (c/register r0 eur) eur [:fiat])))
      (is (instance? Registry (c/remove-traits (c/register r0 eur) eur [:fiat])))
      (is (instance? Registry (c/set-weight (c/register r0 eur) eur 7)))
      (is (instance? Registry (c/clear-weight (c/set-weight (c/register r0 eur) eur 7) eur)))
      (with-temp-global-registry r0
        #(do
           (is (instance? Registry (c/register! nil)))
           (is (instance? Registry (c/register! eur)))
           (is (instance? Registry (c/add-countries! nil :PL)))
           (is (instance? Registry (c/add-countries! eur :PL)))
           (is (instance? Registry (c/remove-countries! nil)))
           (is (instance? Registry (c/remove-countries! :PL)))
           (is (instance? Registry (c/add-localized-props! nil {:* {:name "Euro"}})))
           (is (instance? Registry (c/add-localized-props! eur {:* {:name "Euro"}})))
           (is (instance? Registry (c/remove-localized-props! nil)))
           (is (instance? Registry (c/remove-localized-props! eur)))
           (is (instance? Registry (c/update! eur)))
           (is (instance? Registry (c/unregister! eur))))))))

  (testing "predicates for kinds/domains/traits"
    (let [r (mk-test-registry)]
      (registry/with r
        (let [eur  (c/unit :EUR)
              usdt (c/unit :crypto/USDT)]
          (is (true? (c/currency? eur)))
          (is (false? (c/currency? :NOPE)))
          (is (true? (c/possible? :EUR)))
          (is (true? (c/iso-possible? :EUR)))
          (is (true? (c/crypto? usdt)))
          (is (true? (c/has-numeric-id? eur)))
          (is (false? (c/has-numeric-id? usdt)))
          (is (true? (c/in-domain? :ISO-4217 eur)))
          (is (true? (c/has-domain? eur)))
          (is (true? (c/has-domain? eur :ISO-4217)))
          (is (true? (c/of-domain? :ISO-4217 eur)))
          (is (true? (c/has-kind? eur :iso/fiat)))
          (is (true? (c/of-kind? :iso/fiat eur)))
          (is (true? (c/has-trait? eur :fiat)))
          (is (true? (c/of-trait? :fiat eur)))
          (is (boolean? (c/iso? eur)))
          (is (boolean? (c/fiat? eur)))
          (is (boolean? (c/stable? usdt)))))
      (is (true? (c/iso? #currency EUR)))
      (is (true? (c/fiat? #currency EUR)))
      (is (boolean? (c/stable? #currency crypto/USDT)))))

(deftest scalable-and-formatting-coverage
  (testing "scale/Scalable integration"
    (let [r (mk-test-registry)]
      (registry/with r
        (is (true? (scale/scalable? :EUR)))
        (is (true? (scale/applied? :EUR)))
        (is (= 2 (scale/of :EUR)))
        (is (= 3 (:scale (scale/apply :EUR 3))))
        (is (= 4 (:scale (scale/apply :EUR 4 RoundingMode/HALF_UP))))
        (let [jc (java.util.Currency/getInstance "EUR")]
          (is (true? (scale/scalable? jc)))
          (is (= 2 (scale/of jc)))
          (is (= 5 (:scale (scale/apply jc 5))))))))

  (testing "localized properties and formatting helpers"
    (let [r (mk-test-registry)]
      (registry/with r
        (is (map? (c/localized-properties :EUR)))
        (is (= "Euro" (#'io.randomseed.bankster.currency/get-localized-property
                        :name
                        Locale/ROOT
                        {Locale/ROOT {:name "Euro"}})))
        (is (= "Euro" (c/localized-property :name :EUR)))
        (is (string? (c/symbol-native :EUR)))
        (is (string? (c/display-name-native :EUR)))
        (is (string? (c/ns-code :crypto/USDT)))
        (is (= "USDT" (c/code :crypto/USDT)))
        (is (= 7 (c/weight :PLN)))
        (is (= :ISO-4217 (c/domain :EUR)))
        (is (= :iso/fiat (c/kind :EUR)))
        (is (map? (c/kinds)))
        (let [f (c/formatter :EUR Locale/ROOT)]
          (is (instance? java.text.NumberFormat f))
          (is (string? (.format ^java.text.NumberFormat f 1.23))))
        (let [s (pr-str (c/unit :EUR))]
          (is (re-find #"^#currency" s)))))))

(deftest currency-monetary-string-and-keyword-coverage
  (let [r (mk-test-registry)]
    (registry/with r
      (testing "String Monetary implementation: resolve / resolve-all"
        (is (= :EUR (.id ^Currency (c/resolve "EUR" r))))
        (is (= :EUR (.id ^Currency (c/resolve "iso-4217/EUR" r))))
        (is (= :crypto/USDT (.id ^Currency (c/resolve "crypto/usdt" r))))
        (is (nil? (c/resolve "" r)))
        (is (nil? (c/resolve "NOPE" r)))
        (is (= #{:EUR} (set (map (fn [^Currency x] (.id x)) (c/resolve-all "EUR" r)))))
        (is (= #{:crypto/USDT} (set (map (fn [^Currency x] (.id x)) (c/resolve-all "crypto/usdt" r)))))
        (is (= #{:EUR} (set (map (fn [^Currency x] (.id x)) (c/resolve-all "iso-4217/EUR" r)))))
        (is (nil? (c/resolve-all "" r)))
        (is (nil? (c/resolve-all "NOPE" r))))

      (testing "String Monetary implementation: to-code-str"
        (is (= :EUR (c/to-id "EUR")))
        (is (= "EUR" (c/to-code-str "EUR")))
        (is (= "EUR" (c/to-code-str "iso-4217/EUR")))
        (is (= "USDT" (c/to-code-str "crypto/usdt")))
        (is (= "crypto/USDT" (c/to-id-str "crypto/usdt")))
        (is (nil? (c/to-code-str ""))))

      (testing "Keyword Monetary implementation: resolve-all and defined?"
        ;; non-ISO namespaced path exercises lookup-id-keys reduction branch
        (is (= #{:crypto/USDT}
               (set (map (fn [^Currency x] (.id x)) (c/resolve-all :crypto/usdt r)))))
        (is (true? (c/defined? :crypto/usdt r)))
        (is (boolean? (c/defined? :USDT r)))
        (is (true? (c/present? :crypto/usdt r)))
        (is (boolean? (c/present? :USDT r)))
        (is (false? (c/defined? :NOPE r))))

      (testing "Currency Monetary implementation: to-map"
        (let [cur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
          (is (= {:id :EUR :nr 978 :sc 2 :do :ISO-4217 :ki :iso/fiat :we 0}
                 (c/to-map cur))))))))

(deftest currency-numeric-and-error-path-coverage
  (testing "Numeric ID paths: of-id inconsistency warning and strict throws"
    (let [r  (mk-test-registry)
          r' (-> r
                 ;; Inconsistent: remove canonical mapping but keep bucket mapping
                 (assoc :cur-nr->cur {})
                 (assoc :cur-nr->curs {978 [(c/unit :EUR r)]}))]
      (binding [registry/*warn-on-inconsistency* true
                registry/*warnings-logger*       (fn [_ _] nil)]
        (registry/with r'
          (is (= :EUR (.id ^Currency (c/of-id 978 r'))))
          (is (= :EUR (c/id 978 r')))
          (is (true? (c/present? 978 r')))))
      (is (thrown? clojure.lang.ExceptionInfo (c/of-id 999 r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/id 999 r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/unit :NOPE r)))))

  (testing "Registry update helpers: invalid weight and missing currency errors"
    (let [r0  (registry/new)
          eur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
          r1  (c/register r0 eur)]
      (is (thrown? clojure.lang.ExceptionInfo (c/set-weight r1 :EUR "not-a-number")))
      (is (thrown? clojure.lang.ExceptionInfo (c/add-countries r0 :NOPE :PL)))
      (is (thrown? clojure.lang.ExceptionInfo (c/add-localized-properties r0 :NOPE {:* {:name "X"}})))
      (is (thrown? clojure.lang.ExceptionInfo (c/add-traits r0 :NOPE [:fiat])))
      (is (thrown? clojure.lang.ExceptionInfo (c/add-weighted-code r0 :NOPE)))))

  (testing "none? exercises seqable/empty/null/resolve branches"
    (let [r (mk-test-registry)]
      (registry/with r
        (is (true? (c/none? nil r)))
        (is (true? (c/none? [] r)))
        (is (false? (c/none? :EUR r)))))))

(deftest currency-symbol-and-formatter-arities-coverage
  (let [r (mk-test-registry)]
    (registry/with r
      (testing "symbol-native/display-name-native arities using explicit registry"
        (is (string? (c/symbol-native :EUR r)))
        (is (string? (c/display-name-native :EUR r)))
        (is (string? (c/display-name-native :EUR Locale/ROOT r))))

      (testing "formatter arities"
        (is (instance? java.text.DecimalFormat (c/formatter :EUR)))
        (is (instance? java.text.DecimalFormat (c/formatter :EUR Locale/ROOT)))
        (is (instance? java.text.DecimalFormat (c/formatter :EUR Locale/ROOT r))))

      (testing "formatter-extended custom currency-symbol-fn"
        (let [f (c/formatter-extended :EUR Locale/ROOT {:currency-symbol-fn (fn [_ _ _] "EUR")} r)]
          (is (instance? java.text.DecimalFormat f))
          (is (= "EUR" (.getCurrencySymbol (.getDecimalFormatSymbols ^java.text.DecimalFormat f)))))))))

(deftest currency-misc-uncovered-branches
  (testing "same-ids? uses numeric -> resolve-all path for numeric strings"
    (let [r (mk-test-registry)]
      (is (true? (c/same-ids? "978" :EUR r)))))

  (testing "Currency/of-id respects explicit nil registry"
    (let [cur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
      (is (identical? cur (c/of-id cur nil)))))

  (testing "java.util.Currency to-map and of-id error branch"
    (let [jc-eur (java.util.Currency/getInstance "EUR")
          jc-usd (java.util.Currency/getInstance "USD")
          r      (mk-test-registry)]
      (is (= {:id :EUR :nr 978 :sc 2 :do :ISO-4217}
             (c/to-map jc-eur)))
      (is (thrown? clojure.lang.ExceptionInfo (c/of-id jc-usd r)))))

  (testing "display-name-native falls back to currency code when no localized properties"
    (let [r0  (registry/new)
          eur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
          r1  (c/register r0 eur)]
      (is (string? (c/display-name-native :EUR Locale/ROOT r1)))))

  (testing "Keyword of-id throws when missing"
    (let [r (mk-test-registry)]
      (is (thrown? clojure.lang.ExceptionInfo (c/of-id :NOPE r))))))

(deftest currency-remaining-small-branches
  (testing "Currency/unit with nil and non-nil registry"
    (let [cur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
          r0  (registry/new)
          r1  (c/register r0 cur)]
      (is (identical? cur (c/unit cur nil)))
      (is (instance? Currency (c/unit cur r1)))))

  (testing "Keyword/to-code strips namespace"
    (is (= :USDT (c/to-code :crypto/USDT))))

  (testing "nr (numeric id) arity with explicit registry"
    (let [r (mk-test-registry)]
      (is (= 978 (c/nr :EUR r)))))

  (testing "set-weight and clear-weight throw when currency missing"
    (let [r (mk-test-registry)]
      (is (thrown? clojure.lang.ExceptionInfo (c/set-weight r :NOPE 1)))
      (is (thrown? clojure.lang.ExceptionInfo (c/clear-weight r :NOPE)))))

  (testing "present? for numeric IDs (arity-1) + inconsistency warning path"
    (let [r0 (mk-test-registry)
          r' (-> r0
                 (assoc :cur-nr->cur {})
                 (assoc :cur-nr->curs {978 [(c/unit :EUR r0)]}))]
      (binding [registry/*warn-on-inconsistency* true
                registry/*warnings-logger*       (fn [_ _] nil)]
        (registry/with r'
          (is (true? (c/present? 978)))
          (is (true? (c/defined? 978)))
          (is (true? (c/defined? 978 r')))))))

  (testing "java.util.Currency resolve-all registry arity"
    (let [r  (mk-test-registry)
          jc (java.util.Currency/getInstance "EUR")]
      (is (= #{:EUR} (set (map (fn [^Currency x] (.id x)) (c/resolve-all jc r)))))))

  (testing "scale/Scalable apply on Currency record"
    (let [cur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
      (is (identical? cur (scale/apply cur)))
      (is (= 3 (:scale (scale/apply cur 3))))
      (is (= 4 (:scale (scale/apply cur 4 RoundingMode/HALF_UP))))))

  (testing "display-name (non-native) ISO fallback branch"
    (let [r0  (registry/new)
          eur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
          r1  (-> (c/register r0 eur)
                  ;; Make the registry instance unique for memoize keying.
                  (assoc :cur-id->traits {:EUR #{:coverage/unique}}))]
      (is (string? (c/display-name :EUR (Locale. "zz" "ZZ") r1)))))

  (testing "map Monetary helpers: to-code and to-numeric-id"
    (is (= :EUR (c/to-code {:id :EUR})))
    (is (= :EUR (c/to-code {:code :EUR})))
    (is (= 978 (c/to-numeric-id {:nr 978})))
    (is (= 978 (c/to-numeric-id {:numeric 978}))))

  (testing "ns-code arity with explicit registry"
    (let [r (mk-test-registry)]
      (is (= "crypto/USDT" (c/ns-code :crypto/USDT r)))))

  (testing "private config helpers: prep-propagate-keys, prep-traits, prep-weights"
    (let [prep-propagate-keys #'io.randomseed.bankster.currency/prep-propagate-keys
          prep-traits         #'io.randomseed.bankster.currency/prep-traits
          prep-weights        #'io.randomseed.bankster.currency/prep-weights
          al                 (doto (java.util.ArrayList.) (.add :foo) (.add :scale) (.add :bar))]
      (is (= #{:foo :bar} (prep-propagate-keys al)))
      (is (= #{:fiat :stable} (prep-traits (doto (java.util.ArrayList.) (.add :fiat) (.add :stable)))))
      (is (thrown? clojure.lang.ExceptionInfo (prep-weights {:EUR "wat"}))))))

(deftest currency-predicates-and-macros-coverage
  (let [r (mk-test-registry)]
    (registry/with r
      (testing "predicate wrappers (arity-2 paths)"
        (is (boolean? (c/possible? :EUR r)))
        (is (boolean? (c/iso-possible? :EUR r)))
        (is (boolean? (c/iso? :EUR r)))
        (is (boolean? (c/has-numeric-id? :EUR r)))
        (is (boolean? (c/has-country? :EUR r)))
        (is (boolean? (c/in-domain? :ISO-4217 :EUR r)))
        (is (boolean? (c/has-trait? :EUR r)))
        (is (boolean? (c/has-domain? :EUR :ISO-4217 r)))
        (is (boolean? (c/big? :EUR r)))
        (is (boolean? (c/iso-strict? :EUR r)))
        (is (thrown? clojure.lang.ExceptionInfo (c/set-traits r :NOPE [:fiat])))
        (is (thrown? clojure.lang.ExceptionInfo (c/remove-traits r :NOPE [:fiat]))))

      (testing "kind-based wrappers"
        (doseq [f [c/virtual?
                   c/asset?
                   c/claim?
                   c/credit?
                   c/fiat?
                   c/real?
                   c/fiduciary?
                   c/funds?
                   c/metal?
                   c/commodity?
                   c/peg?
                   c/stable?
                   c/staked?
                   c/wrapped?
                   c/referenced?
                   c/experimental?
                   c/special?
                   c/test?]]
          (is (boolean? (f :EUR r)))
          (is (boolean? (f :crypto/USDT r)))))

      (testing "currency/with macro binds *default*"
        (c/with :EUR
          (is (= :EUR (.id ^Currency c/*default*))))))))

(deftest currency-last-uncovered-odds-and-ends
  (testing "Currency/unit with explicit registry resolves via unit-resolve!"
    (let [r0  (registry/new)
          cur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
          r1  (c/register r0 cur)]
      (is (= :EUR (.id ^Currency (c/unit cur r1))))))

  (testing "String/Symbol of-id wrappers"
    (let [r (mk-test-registry)]
      (is (= :EUR (.id ^Currency (c/of-id "EUR" r))))
      (is (= :EUR (.id ^Currency (c/of-id 'EUR r))))))

  (testing "Numeric defined? inconsistency warning with explicit registry"
    (let [r0 (mk-test-registry)
          r' (-> r0 (assoc :cur-nr->cur {}) (assoc :cur-nr->curs {978 [(c/unit :EUR r0)]}))]
      (binding [registry/*warn-on-inconsistency* true
                registry/*warnings-logger*       (fn [_ _] nil)]
        (is (true? (c/defined? 978 r'))))))

  (testing "same-ids? branch that checks (some a-ids b-ids)"
    (let [r0  (registry/new)
          c1  (c/new :AAA 999 2 :iso/fiat :ISO-4217 0)
          c2  (c/new :test/AAA 999 2 :iso/fiat :TEST 0)
          r1  (-> r0 (c/register c1) (c/register c2))]
      ;; candidate-ids(\"999\") returns 2 IDs, candidate-ids(:AAA) returns 1.
      (is (true? (c/same-ids? "999" :AAA r1)))))

  (testing "private hint normalizers"
    (let [normalize-id-hint #'io.randomseed.bankster.currency/normalize-id-hint
          normalize-id-input #'io.randomseed.bankster.currency/normalize-id-input]
      (is (= :EUR (normalize-id-hint :ISO-4217/EUR)))
      (is (= :123 (normalize-id-input 123)))))

  (testing "new currency domain mismatch throw path"
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/new :crypto/USDT c/no-numeric-id 6 :virtual/stable 123))))) 
