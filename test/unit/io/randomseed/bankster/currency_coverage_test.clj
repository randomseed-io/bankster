(ns

    ^{:doc    "bankster library, currency coverage tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.currency-coverage-test

  (:require [clojure.test                    :refer [deftest testing is are]]
            [io.randomseed.bankster.config   :as config]
            [io.randomseed.bankster.currency :as c]
            [io.randomseed.bankster.money    :as m]
            [io.randomseed.bankster.registry :as registry]
            [io.randomseed.bankster.scale    :as scale]
            [io.randomseed.bankster.util     :as bu])

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

(defn- mk-dup-num-registry
  []
  (let [r0  (registry/new)
        a   (c/new :AAA 999 2 :iso/fiat :ISO-4217 0)
        b   (c/new :BBB 999 2 :iso/fiat :ISO-4217 0)]
    (-> r0
        (c/register a)
        (c/register b))))

(defn- mk-localized-registry
  []
  (let [r0  (registry/new)
        xxx (c/new :XXX 999 2 :iso/fiat :ISO-4217 0)
        en  (Locale/forLanguageTag "en")
        en-us (Locale/forLanguageTag "en-US")]
    (c/register r0 xxx [:US] {en-us {:symbol "$" :name "Dollar"}
                              en    {:symbol "E$" :name "English Dollar"}
                              :*    {:symbol "DEF" :name "Default"}})))

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
    ;; candidate-ids("999") returns 2 IDs, candidate-ids(:AAA) returns 1.
      (is (true? (c/same-ids? "999" :AAA r1)))))

  (testing "private hint normalizers"
    (let [normalize-id-hint #'io.randomseed.bankster.currency/normalize-id-hint
          normalize-id-input #'io.randomseed.bankster.currency/normalize-id-input]
      (is (= :EUR (normalize-id-hint :ISO-4217/EUR)))
      (is (= :123 (normalize-id-input 123)))))

  (testing "new currency domain mismatch throw path"
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/new :crypto/USDT c/no-numeric-id 6 :virtual/stable 123))))) 

(deftest currency-extra-branch-coverage
  (let [r   (mk-test-registry)
        eur (c/unit :EUR r)
        jc  (java.util.Currency/getInstance "EUR")]
    (testing "Currency of-id returns identity when registry is nil"
      (is (identical? eur (c/of-id eur nil))))
    (testing "java.util.Currency resolve/resolve-all with explicit registry"
      (is (= :EUR (.id ^Currency (c/resolve jc r))))
      (is (= #{:EUR}
             (set (map (fn [^Currency x] (.id x)) (c/resolve-all jc r))))))
    (testing "String and Symbol ISO-4217 resolution branches"
      (is (= :EUR (.id ^Currency (c/resolve "ISO-4217/EUR" r))))
      (is (= #{:EUR}
             (set (map (fn [^Currency x] (.id x)) (c/resolve-all "ISO-4217/EUR" r)))))
      (let [sym (symbol "ISO-4217" "EUR")]
        (is (= :EUR (.id ^Currency (c/resolve sym r))))
        (is (= #{:EUR}
               (set (map (fn [^Currency x] (.id x)) (c/resolve-all sym r)))))))
    (testing "map :code fallback in Monetary protocol"
      (let [m {:code "EUR"}]
        (is (= :EUR (c/to-id m)))
        (is (= :EUR (c/to-code m)))
        (is (= "EUR" (c/to-id-str m)))
        (is (= "EUR" (c/to-code-str m)))))
    (testing "weight locale arity and info weight fallback"
      (let [r0  (registry/new)
            cur (c/new :AAA 111 2 :iso/fiat :ISO-4217 0)
            r1  (c/register r0 cur)
            r2  (assoc r1 :cur-id->weight {})]
        (is (= 0 (c/weight :AAA Locale/ROOT r1)))
        (is (= 0 (:weight (c/info :AAA r2))))))
    (testing "remove-countries returns registry when nil ids"
      (let [r1 (mk-test-registry)]
        (is (identical? r1 (c/remove-countries r1 nil)))))))

(deftest currency-normalization-and-compare-helpers
  (let [candidate-ids         #'io.randomseed.bankster.currency/candidate-ids
        normalize-numeric-hint #'io.randomseed.bankster.currency/normalize-numeric-hint
        normalize-scale-hint   #'io.randomseed.bankster.currency/normalize-scale-hint
        normalize-kind-hint    #'io.randomseed.bankster.currency/normalize-kind-hint
        compare-map-ids        #'io.randomseed.bankster.currency/compare-map-ids
        invalid-map-hint       @#'io.randomseed.bankster.currency/invalid-map-hint
        r                      (mk-test-registry)
        eur                    (c/unit :EUR r)]
    (is (= #{"EUR"} (candidate-ids (java.util.Currency/getInstance "EUR") r)))
    (is (= c/no-numeric-id (normalize-numeric-hint nil)))
    (with-redefs [io.randomseed.bankster.currency/valid-numeric-id? (constantly false)]
      (is (identical? invalid-map-hint (normalize-numeric-hint 5))))
    (is (identical? invalid-map-hint (normalize-numeric-hint "nope")))
    (is (= 5 (normalize-numeric-hint "5")))
    (is (= c/auto-scaled (normalize-scale-hint nil)))
    (is (= :iso/fiat (normalize-kind-hint 'iso/fiat)))
    (is (identical? eur
                    (compare-map-ids eur {:id :EUR :numeric 978 :scale 2 :domain :ISO-4217 :kind :iso/fiat :weight 0})))
    (let [usdt (c/unit :crypto/USDT r)]
      (is (nil? (compare-map-ids usdt {:id :crypto/USDT :domain :ISO-4217}))))))

(deftest currency-resolve-all-branches
  (let [r          (mk-test-registry)
        eur        (c/unit :EUR r)
        missing    (c/new :ZZZ 999 2 :iso/fiat :ISO-4217 0)
        no-num     (c/new :XNN c/no-numeric-id 2 :iso/fiat :ISO-4217 0)
        r2         (c/register r no-num)
        r-num-only (-> r
                       (assoc :cur-id->cur {})
                       (assoc :cur-nr->curs {978 [(c/unit :EUR r)]}))]
    (is (nil? (c/resolve-all missing r)))
    (is (= #{no-num} (c/resolve-all no-num r2)))
    (is (= #{eur} (set (c/resolve-all eur r-num-only))))
    (is (= #{eur} (c/resolve-all eur r)))))

(deftest currency-keyword-iso-case-branches
  (let [r     (mk-test-registry)
        lower "zzq"
        upper (.toUpperCase ^String lower)
        kw    (keyword lower)]
    (is (nil? (clojure.lang.Keyword/find nil upper)))
    (is (= kw (clojure.lang.Keyword/find nil lower)))
    (registry/with r
      (is (nil? (c/resolve (keyword "ISO-4217" lower))))
      (is (nil? (c/resolve-all (keyword "ISO-4217" lower)))))))

(deftest currency-java-and-number-protocols
  (let [r  (mk-test-registry)
        jc (java.util.Currency/getInstance "EUR")]
    (is (= :EUR (c/to-id jc)))
    (is (= :EUR (c/to-code jc)))
    (is (= "EUR" (c/to-id-str jc)))
    (is (= "EUR" (c/to-code-str jc)))
    (is (= 978 (c/to-numeric-id jc)))
    (is (instance? Currency (c/to-currency jc)))
    (is (= {:id :EUR :nr 978 :sc 2 :do :ISO-4217} (c/to-map jc)))
    (is (instance? Currency (c/resolve jc r)))
    (is (set? (c/resolve-all jc r)))
    (is (instance? Currency (c/of-id jc r)))
    (is (instance? Currency (c/unit jc r)))
    (is (keyword? (c/id jc r)))
    (is (boolean? (c/defined? jc r)))
    (is (boolean? (c/present? jc r))))
  (let [r (mk-test-registry)]
    (is (= {:nr 978} (c/to-map 978)))
    (is (instance? Currency (c/resolve 978 r)))
    (is (set? (c/resolve-all 978 r)))
    (is (instance? Currency (c/of-id 978 r)))
    (is (instance? Currency (c/unit 978 r)))
    (is (= :EUR (c/id 978 r)))))

(deftest currency-arity-one-monetary
  (let [r   (mk-test-registry)
        jc  (java.util.Currency/getInstance "EUR")
        m   {:id :EUR}
        r'  (assoc r :cur-code->curs {})]
    (registry/with r
      (is (instance? Currency (c/resolve jc)))
      (is (set? (c/resolve-all jc)))
      (is (instance? Currency (c/of-id jc)))
      (is (instance? Currency (c/resolve 978)))
      (is (set? (c/resolve-all 978)))
      (is (instance? Currency (c/of-id 978)))
      (is (= :EUR (c/id 978)))
      (is (instance? Currency (c/resolve "EUR")))
      (is (set? (c/resolve-all "EUR")))
      (is (instance? Currency (c/of-id "EUR")))
      (is (true? (c/present? "EUR")))
      (is (instance? Currency (c/resolve 'EUR)))
      (is (set? (c/resolve-all 'EUR)))
      (is (instance? Currency (c/of-id 'EUR)))
      (is (true? (c/defined? 'EUR)))
      (is (instance? Currency (c/of-id :EUR)))
      (is (thrown? clojure.lang.ExceptionInfo (c/of-id m)))
      (is (= :EUR (c/id m))))
    (is (true? (c/present? :EUR r')))
    (is (= #{:EUR} (set (map (fn [^Currency x] (.id x)) (c/resolve-all :EUR r')))))
    (is (= :EUR (.id ^Currency (c/unit :EUR r'))))))

(deftest currency-record-protocols
  (let [r   (mk-test-registry)
        cur (c/unit :EUR r)
        cur-ns (c/unit :crypto/USDT r)]
    (is (= :EUR (c/to-id cur)))
    (is (= :EUR (c/to-code cur)))
    (is (= :USDT (c/to-code cur-ns)))
    (is (= "EUR" (c/to-id-str cur)))
    (is (= "EUR" (c/to-code-str cur)))
    (is (= 978 (c/to-numeric-id cur)))
    (is (identical? cur (c/to-currency cur)))
    (is (map? (c/to-map cur)))
    (is (true? (c/definitive? cur)))
    (registry/with r
      (is (instance? Currency (c/of-id cur)))
      (is (true? (c/has-domain? :EUR :ISO-4217)))
      (is (true? (c/iso-strict? :EUR))))
    (is (instance? Currency (c/of-id cur r)))
    (is (instance? Currency (c/unit cur r)))
    (is (keyword? (c/id cur r)))
    (is (boolean? (c/defined? cur r)))
    (is (boolean? (c/present? cur r)))))

(deftest currency-keyword-string-symbol-map-protocols
  (let [r (mk-test-registry)]
    (is (= {:id :EUR} (c/to-map :EUR)))
    (is (= :EUR (.id ^Currency (c/of-id :EUR r))))
    (is (= :EUR (c/id :EUR r)))
    (is (true? (c/present? :EUR r)))
    (is (= :EUR (.id ^Currency (c/resolve :ISO-4217/EUR r))))
    (is (= #{:EUR} (set (map (fn [^Currency x] (.id x)) (c/resolve-all :EUR r)))))
    (is (= #{:EUR} (set (map (fn [^Currency x] (.id x)) (c/resolve-all :ISO-4217/EUR r))))))
  (let [r (mk-test-registry)]
    (is (= :usdt (c/to-code "crypto/usdt")))
    (is (= "USDT" (c/to-code-str "crypto/usdt")))
    (is (instance? Currency (c/to-currency "EUR")))
    (is (= {:id :EUR} (c/to-map "EUR")))
    (is (instance? Currency (c/resolve "EUR" r)))
    (is (set? (c/resolve-all "EUR" r)))
    (is (instance? Currency (c/of-id "EUR" r)))
    (is (= :EUR (c/id "EUR" r)))
    (is (true? (c/defined? "EUR" r)))
    (is (true? (c/present? "EUR" r))))
  (let [r   (mk-test-registry)
        sym (symbol "crypto" "USDT")]
    (is (= :crypto/USDT (c/to-id sym)))
    (is (= :USDT (c/to-code sym)))
    (is (= "crypto/USDT" (c/to-id-str sym)))
    (is (= "USDT" (c/to-code-str sym)))
    (is (nil? (c/to-numeric-id sym)))
    (is (= :crypto/USDT (.id ^Currency (c/to-currency sym))))
    (is (= {:id :crypto/USDT} (c/to-map sym)))
    (is (instance? Currency (c/resolve sym r)))
    (is (set? (c/resolve-all sym r)))
    (is (instance? Currency (c/of-id sym r)))
    (is (instance? Currency (c/unit sym r)))
    (is (= :crypto/USDT (c/id sym r)))
    (is (true? (c/defined? sym r)))
    (is (true? (c/present? sym r))))
  (let [r (mk-test-registry)
        m {:id :EUR :numeric 978 :scale 2 :domain :ISO-4217 :kind :iso/fiat}]
    (is (= :EUR (c/to-id m)))
    (is (= :EUR (c/to-code m)))
    (is (= "EUR" (c/to-id-str m)))
    (is (= "EUR" (c/to-code-str m)))
    (is (= 978 (c/to-numeric-id m)))
    (is (instance? Currency (c/to-currency m)))
    (is (= :EUR (:id (c/to-map m))))
    (is (instance? Currency (c/resolve m r)))
    (is (set? (c/resolve-all m r)))
    (is (thrown? clojure.lang.ExceptionInfo (c/of-id m r)))
    (is (= :EUR (c/id m nil)))
    (is (thrown? clojure.lang.ExceptionInfo (c/id m r)))
    (is (thrown? clojure.lang.ExceptionInfo (c/defined? m)))
    (is (thrown? clojure.lang.ExceptionInfo (c/defined? m r)))
    (is (thrown? clojure.lang.ExceptionInfo (c/present? m)))
    (is (thrown? clojure.lang.ExceptionInfo (c/present? m r)))))

(deftest currency-properties-and-aliases
  (let [r (mk-test-registry)]
    (registry/with r
      (is (= 978 (c/nr :EUR Locale/ROOT r)))
      (is (= 2 (c/sc :EUR Locale/ROOT r)))
      (is (= :ISO-4217 (c/domain :EUR Locale/ROOT r)))
      (is (true? (c/has-domain? :EUR r)))
      (is (= :iso/fiat (c/kind :EUR Locale/ROOT r)))
      (is (= "EUR" (c/ns-code :EUR Locale/ROOT r)))
      (is (= "EUR" (c/code :EUR Locale/ROOT r)))
      (is (= 7 (c/weight :PLN Locale/ROOT r))))))

(deftest currency-countries-and-of-country
  (let [r (mk-test-registry)]
    (registry/with r
      (is (set? (c/countries :PLN Locale/ROOT r)))
      (is (= :PLN (.id ^Currency (c/of-country :PL))))
      (is (= :PLN (.id ^Currency (c/of-country :PL r))))
      (is (= :PLN (.id ^Currency (c/of-country :PL Locale/ROOT r)))))))

(deftest currency-registry-mutations-and-info
  (let [r0  (registry/new)
        eur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
    (let [r1 (c/register r0 eur)]
      (is (instance? Registry (c/set-weight r1 eur 5)))
      (is (instance? Registry (c/clear-weight r1 eur))))
    (let [r1 (c/register r0 eur)
          r2 (c/set-weight r1 :EUR 9)
          r3 (c/update r2 (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))]
      (is (= 9 (c/weight :EUR r3))))
    (with-temp-global-registry (c/register r0 eur)
      #(do
         (is (instance? Registry (c/set-weight! :EUR 3)))
         (is (instance? Registry (c/clear-weight! :EUR))))))
  (let [r (mk-test-registry)]
    (registry/with r
      (let [info (c/info :EUR r)]
        (is (= :EUR (:id info)))
        (is (= 0 (:weight info)))
        (is (map? info)))
      (let [r2   (assoc r :cur-id->weight {:EUR 5})
            info (c/info :EUR r2)]
        (is (= 5 (:weight info))))
      (is (map? (c/info :EUR)))
      (is (map? (c/info :EUR Locale/ROOT r))))))

(deftest currency-config-prep-and-loading
  (let [prep-propagate-keys  #'io.randomseed.bankster.currency/prep-propagate-keys
        prep-currency        #'io.randomseed.bankster.currency/prep-currency
        prep-currencies       #'io.randomseed.bankster.currency/prep-currencies
        prep-all-localized    #'io.randomseed.bankster.currency/prep-all-localized-props
        prep-traits           #'io.randomseed.bankster.currency/prep-traits
        remove-countries-core #'io.randomseed.bankster.currency/remove-countries-core
        r                     (mk-test-registry)]
    (is (= #{:foo} (prep-propagate-keys [nil :foo :scale])))
    (is (= #{:foo} (prep-propagate-keys :foo)))
    (is (instance? Currency (prep-currency [:EUR {:numeric 978 :scale 2 :kind :iso/fiat :domain :ISO-4217}])))
    (is (instance? Currency (prep-currency :EUR {:numeric 978 :scale 2 :kind :iso/fiat :domain :ISO-4217})))
    (is (instance? Currency (prep-currency :EUR {:numeric 978 :scale 2 :kind :iso/fiat :domain :ISO-4217} [:foo])))
    (is (instance? Currency (prep-currency :EUR 978 :iso/fiat 2)))
    (is (instance? Currency (prep-currency :EUR 978 :iso/fiat 2 :ISO-4217 0)))
    (is (seq (doall (prep-currencies {:EUR {:numeric 978 :scale 2}} [:foo]))))
    (is (seq (doall (prep-currencies {:EUR {:numeric 978 :scale 2}}))))
    (is (map? (prep-all-localized {:EUR {:* {:name "Euro"}}})))
    (is (= #{:fiat :stable} (prep-traits [:fiat :stable])))
    (is (= #{:fiat} (prep-traits :fiat)))
    (is (instance? Registry (remove-countries-core r [])))
    (is (instance? Registry (c/remove-countries r [:PL])))
    (is (instance? Registry (c/remove-localized-properties r nil)))
    (is (instance? Registry (c/remove-localized-properties r :EUR)))
    (is (instance? Registry (c/add-weighted-code (assoc r :cur-code->curs {}) :EUR)))
    (is (instance? Registry (c/add-weighted-code r :EUR)))
    (is (instance? Registry (c/register r (c/new :AAA 999 2 :iso/fiat :ISO-4217 0) [:PL] {:* {:name "AAA"}})))
    (is (instance? Registry (c/register r (c/new :DDD 996 2 :iso/fiat :ISO-4217 0) [:PL] true)))
    (is (instance? Registry (c/register r (c/new :CCC 997 2 :iso/fiat :ISO-4217 0) true)))
    (is (instance? Registry (c/update r (c/new :BBB 998 2 :iso/fiat :ISO-4217 0) [:PL])))
    (is (instance? Registry (c/update r (c/new :BBB 998 2 :iso/fiat :ISO-4217 0) nil nil)))
    (is (instance? Registry (c/update r nil nil nil)))
    (is (instance? Registry (c/config->registry config/default-resource-path)))
    (is (instance? Registry (c/config->registry)))))

(deftest currency-global-registry-ops
  (let [r0  (registry/new)
        eur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
    (with-temp-global-registry r0
      (fn []
        (is (instance? Registry (c/register! eur :PL)))
        (is (instance? Registry (c/register! eur :PL {:* {:name "Euro"}} true)))
        (is (instance? Registry (c/update! eur :PL)))
        (is (instance? Registry (c/update! eur :PL {:* {:name "Euro"}})))
        (is (instance? Registry (c/remove-countries! [:PL])))
        (is (instance? Registry (c/add-localized-props! eur {:* {:name "Euro"}})))
        (is (instance? Registry (c/remove-localized-props! eur)))
        (is (instance? Registry (c/set-traits! :EUR [:fiat :stable])))
        (is (instance? Registry (c/add-traits! :EUR [:fiat])))
        (is (instance? Registry (c/remove-traits! :EUR [:fiat])))
        (is (instance? Registry (c/remove-traits! :EUR [:fiat :stable])))))))

(deftest currency-traits-empty-branches
  (let [r (mk-test-registry)]
    (is (instance? Registry (c/set-traits r :EUR nil)))
    (is (instance? Registry (c/set-traits r :EUR [])))
    (is (instance? Registry (c/add-traits r :EUR nil)))
    (is (instance? Registry (c/remove-traits r :EUR nil)))))

(deftest currency-predicate-branches
  (let [r       (mk-test-registry)
        no-kind (c/new :ZZZ 999 2 nil :ISO-4217 0)]
    (is (true? (c/has-domain? :EUR :ISO-4217)))
    (is (boolean? (c/crypto? :crypto/USDT r)))
    (is (boolean? (c/iso-strict? :EUR r)))
    (is (boolean? (c/iso-legacy? :EUR r)))
    (is (boolean? (c/has-kind? :EUR nil)))
    (is (boolean? (c/has-kind? :EUR r)))
    (is (boolean? (c/has-kind? :EUR :iso/fiat)))
    (is (boolean? (c/has-kind? :EUR :iso/fiat r)))
    (is (false? (c/of-kind? :iso no-kind r)))
    (is (boolean? (c/has-trait? :EUR nil)))
    (is (boolean? (c/of-trait? :fiat :EUR r)))
    (doseq [f [c/virtual? c/asset? c/claim? c/credit? c/fiat? c/real? c/fiduciary?
               c/funds? c/metal? c/commodity? c/peg? c/stable? c/staked? c/wrapped?
               c/referenced? c/experimental? c/special? c/test? c/null?]]
      (is (boolean? (f :EUR r))))
    (is (boolean? (c/none? :EUR r)))
    (is (boolean? (c/decentralized? :EUR r)))
    (registry/with r
      (is (boolean? (c/in-domain? :ISO-4217 :EUR)))
      (is (boolean? (c/crypto? :crypto/USDT)))
      (doseq [f [c/virtual? c/asset? c/claim? c/credit? c/fiat? c/real? c/fiduciary?
                 c/funds? c/metal? c/commodity? c/peg? c/stable? c/staked? c/wrapped?
                 c/referenced? c/experimental? c/special? c/test? c/null?]]
        (is (boolean? (f :EUR))))
      (is (boolean? (c/none? :EUR)))
      (is (boolean? (c/decentralized? :EUR))))))

(deftest currency-hierarchy-nil-branches
  (let [r0 (mk-test-registry)
        r1 (assoc r0 :hierarchies {})]
    (is (boolean? (c/of-kind? :iso :EUR r1)))
    (is (boolean? (c/of-trait? :fiat :EUR r1)))))

(deftest currency-localized-and-formatting-branches
  (let [r (mk-test-registry)]
    (registry/with r
      (is (string? (c/symbol-native :EUR)))
      (is (string? (c/symbol-native :PLN)))
      (is (string? (c/symbol-native :EUR r)))
      (is (string? (c/symbol-native :PLN r)))
      (is (string? (c/symbol-native :EUR Locale/ROOT r)))
      (is (string? (c/display-name :EUR)))
      (is (string? (c/display-name-native :EUR)))
      (is (string? (c/display-name-native :PLN)))
      (is (string? (c/display-name-native :EUR r)))
      (is (string? (c/display-name-native :PLN r)))))
  (let [r0  (registry/new)
        eur (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
        r1  (c/register r0 eur)
        r2  (registry/hierarchy-derive :kind :iso/fiat :iso r1)]
    (is (string? (c/display-name :EUR Locale/ROOT r2)))
    (is (string? (c/display-name :EUR (Locale. "en" "US") r2)))))

(deftest currency-scalable-and-formatters
  (let [jc (java.util.Currency/getInstance "EUR")]
    (is (instance? Currency (scale/apply jc)))
    (is (instance? Currency (scale/apply jc 3)))
    (is (instance? Currency (scale/apply jc 3 RoundingMode/HALF_UP))))
  (let [r (mk-test-registry)]
    (registry/with r
      (is (instance? Currency (scale/apply :EUR)))
      (is (instance? Currency (scale/apply :EUR 3)))
      (is (string? (c/symbol-native :EUR r)))
      (is (instance? java.text.DecimalFormat (c/formatter-instance :EUR)))
      (is (instance? java.text.DecimalFormat (c/formatter-instance :EUR Locale/ROOT)))
      (is (instance? java.text.DecimalFormat (c/formatter-instance :EUR Locale/ROOT r)))
      (is (instance? java.text.DecimalFormat (c/formatter-extended :EUR)))
      (is (instance? java.text.DecimalFormat (c/formatter-extended :EUR Locale/ROOT)))
      (is (instance? java.text.DecimalFormat (c/formatter-extended :EUR Locale/ROOT {}))))))

(deftest currency-literals-and-serialization-helpers
  (is (= '(quote nil) (c/data-literal nil)))
  (is (instance? Currency (c/data-literal :EUR)))
  (is (map? (c/to-json-map :EUR)))
  (is (map? (c/to-json-map :EUR {:code-only? true})))
  (is (string? (c/to-json-string :EUR)))
  (is (string? (c/to-json-string :EUR {:code-only? true})))
  (is (instance? clojure.lang.IPersistentMap (c/to-edn-map :EUR)))
  (is (instance? clojure.lang.IPersistentMap (c/to-edn-map :EUR {:code-only? true})))
  (is (string? (c/to-edn-string :EUR)))
  (is (string? (c/to-edn-string :EUR {:code-only? true}))))

(deftest currency-set-default-and-predicates
  (let [r (mk-test-registry)]
    (registry/with r
      (try
        (c/set-default! :EUR)
        (is (instance? Currency c/*default*))
        (finally
          (c/unset-default!))))
    (is (true? (c/currency? (c/of :EUR) r)))))

(deftest currency-final-coverage-holes
  (let [r                  (mk-test-registry)
        id->str            #'io.randomseed.bankster.currency/id->str
        candidate-ids      #'io.randomseed.bankster.currency/candidate-ids
        normalize-code-hint #'io.randomseed.bankster.currency/normalize-code-hint
        normalize-scale-hint #'io.randomseed.bankster.currency/normalize-scale-hint
        normalize-kind-hint #'io.randomseed.bankster.currency/normalize-kind-hint
        normalize-weight-hint #'io.randomseed.bankster.currency/normalize-weight-hint
        invalid-map-hint   @#'io.randomseed.bankster.currency/invalid-map-hint
        prep-currency      #'io.randomseed.bankster.currency/prep-currency]

    (testing "new-currency arities"
      (is (instance? Currency (c/new :EUR 978)))
      (is (instance? Currency (c/new :EUR 978 2)))
      (is (instance? Currency (c/new :EUR 978 2 :iso/fiat))))

    (testing "id->str and candidate-ids branches"
      (is (= "crypto/USDT" (id->str :crypto/USDT)))
      (is (= #{"EUR"} (candidate-ids (c/unit :EUR r) r)))
      (is (= #{"EUR"} (candidate-ids {:id :EUR} r))))

    (testing "same-ids? arity-1"
      (is (true? (c/same-ids? :EUR :EUR))))

    (testing "normalize helpers: parsing branches"
      (is (= :eur (normalize-code-hint :eur)))
      (is (identical? invalid-map-hint (normalize-scale-hint "NaN")))
      (is (= 7 (normalize-scale-hint "7")))
      (is (= :crypto/USDT (normalize-kind-hint "crypto/USDT")))
      (is (= 3 (normalize-weight-hint "3"))))

    (testing "Currency protocol arities and resolve fallback"
      (registry/with r
        (let [cur (c/unit :EUR r)
              r'  (assoc r :cur-id->cur {})]
          (is (= cur (c/resolve cur r')))
          (is (instance? Currency (c/unit cur)))
          (is (= :EUR (c/id cur)))
          (is (true? (c/defined? cur)))
          (is (true? (c/present? cur))))))

    (testing "java.util.Currency protocol arities"
      (let [jc (java.util.Currency/getInstance "EUR")]
        (registry/with r
          (is (= :EUR (c/id jc)))
          (is (true? (c/defined? jc)))
          (is (true? (c/present? jc))))))

    (testing "numeric resolve-all fallback and unit arity-1"
      (let [r' (assoc r :cur-nr->curs {})]
        (is (= #{(c/unit :EUR r')} (c/resolve-all 978 r'))))
      (registry/with r
        (is (instance? Currency (c/unit 978)))))

    (testing "keyword id resolution (non-namespaced)"
      (registry/with r
        (is (= :EUR (c/id :EUR)))
        (is (= :XXX (c/id :XXX)))))

    (testing "String and Symbol Monetary branches"
      (registry/with r
        (is (instance? Currency (c/unit "EUR")))
        (is (instance? Currency (c/unit "EUR" r)))
        (is (= :EUR (c/id "EUR")))
        (is (true? (c/defined? "EUR")))
        (is (= "EUR" (c/to-id-str 'EUR)))
        (is (= "crypto/USDT" (c/to-id-str 'crypto/USDT)))
        (is (instance? Currency (c/unit 'EUR)))
        (is (= :EUR (c/id 'EUR)))))

    (testing "map-based Monetary branches"
      (let [m-ok  {:id :EUR :domain :ISO-4217 :scale 2 :numeric 978}
            m-bad {:id :XXX :domain :ISO-4217 :scale 2 :numeric 999}]
        (registry/with r
          (is (true? (c/definitive? m-ok)))
          (is (instance? Currency (c/resolve m-ok)))
          (is (set? (c/resolve-all m-ok)))
          (is (instance? Currency (c/unit m-ok))))
        (is (thrown? clojure.lang.ExceptionInfo (c/unit m-bad r)))))

    (testing "nr/sc arity-1"
      (registry/with r
        (is (= 978 (c/nr :EUR)))
        (is (= 2 (c/sc :EUR)))))

    (testing "set-weight/clear-weight with countries"
      (let [r-country (-> (registry/new)
                          (c/register (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))
                          (c/add-countries :EUR [:PL]))
            r1        (c/set-weight r-country :EUR 5)
            r2        (c/clear-weight r1 :EUR)]
        (is (= 5 (get-in r1 [:cur-id->weight :EUR])))
        (is (nil? (get-in r2 [:cur-id->weight :EUR])))))

    (testing "countries: arity-1 and missing currency"
      (let [r-country (-> (registry/new)
                          (c/register (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))
                          (c/add-countries :EUR [:PL]))]
        (registry/with r-country
          (is (set? (c/countries :EUR)))))
      (is (thrown? clojure.lang.ExceptionInfo (c/countries (c/new :XXX 999 2 :iso/fiat :ISO-4217 0) r))))

    (testing "java conversion for ISO currency"
      (registry/with r
        (is (instance? java.util.Currency (c/java :EUR))))
      (is (instance? java.util.Currency (c/java :EUR r))))

    (testing "prep-currency propagate-keys override"
      (let [c1 (prep-currency :EUR {:numeric 978 :scale 2 :domain :ISO-4217
                                    :propagate-keys [:foo] :foo "bar"} nil)]
        (is (instance? Currency c1))
        (is (= "bar" (:foo c1)))))

    (testing "register update restores traits"
      (let [r0 (-> (registry/new)
                   (c/register (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))
                   (c/add-traits :EUR [:fiat]))
            r1 (c/register r0 (c/new :EUR 978 2 :iso/fiat :ISO-4217 0) nil nil true)]
        (is (contains? (get-in r1 [:cur-id->traits :EUR]) :fiat))))

    (testing "config->registry hierarchy branch"
      (with-redefs [config/load (fn [_]
                                  {:hierarchies nil
                                   :currencies  {}
                                   :countries   {}
                                   :localized   {}
                                   :traits      {}
                                   :weights     {}
                                   :version     "x"})]
        (let [r1 (c/config->registry "dummy" (registry/new-registry))]
          (is (some? r1)))))

    (testing "domain/country predicates and hierarchy-aware domain checks"
      (let [r1 (registry/hierarchy-derive :domain :ISO-4217 :FIAT r)]
        (let [r-country (-> (registry/new)
                            (c/register (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))
                            (c/add-countries :EUR [:PL]))]
          (registry/with r-country
            (is (true? (c/has-country? :EUR))))
          (registry/with r
            (is (true? (c/has-domain? :EUR nil)))))
        (registry/with r1
          (is (true? (c/of-domain? :FIAT :EUR))))
        (is (true? (c/of-domain? :FIAT :EUR r1)))))

    (testing "auto-scaled and legacy helpers"
      (let [r1 (c/register (registry/new) (c/new :BIG c/no-numeric-id c/auto-scaled :virtual/asset :CRYPTO 0))]
        (registry/with r1
          (is (true? (c/big? :BIG)))))
      (registry/with r
        (is (false? (c/iso-legacy? :EUR)))))

    (testing "has-trait? explicit registry arity"
      (is (true? (c/has-trait? :EUR :fiat r))))

    (testing "localized-properties and localized-property arities"
      (let [missing (c/new :XXX 999 2 :iso/fiat :ISO-4217 0)]
        (is (thrown? clojure.lang.ExceptionInfo (c/localized-properties missing r)))
        (is (thrown? clojure.lang.ExceptionInfo (c/localized-property :symbol missing Locale/ROOT r))))
      (registry/with r
        (is (nil? (c/localized-property :symbol :EUR Locale/ROOT)))))

    (testing "symbol and display-name locale arities and java fallback"
      (let [r1 (registry/hierarchy-derive :kind :iso/fiat :iso
                                          (c/register (registry/new)
                                                      (c/new :USD 840 2 :iso/fiat :ISO-4217 0)))
            r2 (-> (registry/new)
                   (c/register (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))
                   (c/add-localized-properties :EUR {:* {:symbol "€"}}))]
        (registry/with r1
          (is (string? (c/symbol :USD Locale/ROOT)))
          (is (string? (c/display-name :USD Locale/ROOT))))
        (registry/with r2
          (is (= "€" (c/symbol :EUR Locale/ROOT))))))))

(deftest currency-forms-coverage-helpers
  (let [needs-upper-ascii?     #'io.randomseed.bankster.currency/needs-upper-ascii?
        upper-ascii-if-needed  #'io.randomseed.bankster.currency/upper-ascii-if-needed
        normalize-id-input     #'io.randomseed.bankster.currency/normalize-id-input
        lookup-id-keys         #'io.randomseed.bankster.currency/lookup-id-keys
        normalize-id-hint      #'io.randomseed.bankster.currency/normalize-id-hint
        normalize-code-hint    #'io.randomseed.bankster.currency/normalize-code-hint
        normalize-numeric-hint #'io.randomseed.bankster.currency/normalize-numeric-hint
        normalize-scale-hint   #'io.randomseed.bankster.currency/normalize-scale-hint
        normalize-domain-hint  #'io.randomseed.bankster.currency/normalize-domain-hint
        normalize-kind-hint    #'io.randomseed.bankster.currency/normalize-kind-hint
        normalize-weight-hint  #'io.randomseed.bankster.currency/normalize-weight-hint
        try-to-make-iso-domain #'io.randomseed.bankster.currency/try-to-make-iso-domain
        invalid-map-hint       @#'io.randomseed.bankster.currency/invalid-map-hint]

    (testing "needs-upper-ascii? and upper-ascii-if-needed"
      (is (true? (needs-upper-ascii? "aBC")))
      (is (false? (needs-upper-ascii? "ABC")))
      (is (nil? (needs-upper-ascii? nil)))
      (is (= "ABC" (upper-ascii-if-needed "aBC")))
      (is (= "ABC" (upper-ascii-if-needed "ABC"))))

    (testing "normalize-id-input and lookup-id-keys"
      (is (= :EUR (normalize-id-input :eur)))
      (is (= :EUR (normalize-id-input 'eur)))
      (is (= :crypto/USDT (normalize-id-input 'crypto/usdt)))
      (is (= :EUR (normalize-id-input "eur")))
      (is (= :crypto/USDT (normalize-id-input "crypto/usdt")))
      (is (= :123 (normalize-id-input 123)))
      (is (= [:EUR] (lookup-id-keys :EUR)))
      (is (= :crypto/USDT :crypto/USDT))
      (is (= [:crypto/USDT :crypto/usdt] (lookup-id-keys :crypto/usdt))))

    (testing "normalize hints"
      (is (= :EUR (normalize-id-hint :ISO-4217/EUR)))
      (is (= :EUR (normalize-id-hint :EUR)))
      (is (nil? (normalize-id-hint 123)))
      (is (= :eur (normalize-code-hint :eur)))
      (is (= :ISO-4217 (normalize-domain-hint "iso-4217")))
      (is (= :CRYPTO (normalize-domain-hint :crypto)))
      (is (nil? (normalize-domain-hint nil)))
      (is (nil? (normalize-domain-hint "")))
      (is (= :iso/fiat (normalize-kind-hint :iso/fiat)))
      (is (= :iso/fiat (normalize-kind-hint 'iso/fiat)))
      (is (= :iso/fiat (normalize-kind-hint "iso/fiat")))
      (is (nil? (normalize-kind-hint ""))))

    (testing "normalize numeric/scale/weight hints"
      (is (= c/no-numeric-id (normalize-numeric-hint nil)))
      (is (= c/no-numeric-id (normalize-numeric-hint 0)))
      (is (= 978 (normalize-numeric-hint 978)))
      (with-redefs [io.randomseed.bankster.currency/valid-numeric-id? (constantly false)]
        (is (identical? invalid-map-hint (normalize-numeric-hint 2))))
      (is (= 978 (normalize-numeric-hint "978")))
      (is (identical? invalid-map-hint (normalize-numeric-hint "abc")))
      (with-redefs [bu/try-parse-long (fn [_] (throw (RuntimeException. "boom")))]
        (is (identical? invalid-map-hint (normalize-numeric-hint "boom"))))

      (is (= c/auto-scaled (normalize-scale-hint nil)))
      (is (= c/auto-scaled (normalize-scale-hint -1)))
      (is (= 2 (normalize-scale-hint 2)))
      (is (= 2 (normalize-scale-hint "2")))
      (is (identical? invalid-map-hint (normalize-scale-hint "abc")))
      (with-redefs [bu/try-parse-int (fn [_] (throw (RuntimeException. "boom")))]
        (is (identical? invalid-map-hint (normalize-scale-hint "boom"))))

      (is (= 0 (normalize-weight-hint nil)))
      (is (= 3 (normalize-weight-hint 3)))
      (is (= 3 (normalize-weight-hint "3")))
      (is (identical? invalid-map-hint (normalize-weight-hint "abc")))
      (with-redefs [bu/try-parse-int (fn [_] (throw (RuntimeException. "boom")))]
        (is (identical? invalid-map-hint (normalize-weight-hint "boom")))))

    (testing "try-to-make-iso-domain"
      (is (= :ISO-4217 (try-to-make-iso-domain 978 :EUR)))
      (is (nil? (try-to-make-iso-domain 0 :EUR)))
      (is (nil? (try-to-make-iso-domain 978 :EuR))))))

(deftest currency-forms-coverage-candidate-ids-and-same-ids
  (let [r             (mk-test-registry)
        candidate-ids #'io.randomseed.bankster.currency/candidate-ids]
    (testing "candidate-ids branches"
      (is (nil? (candidate-ids nil r)))
      (is (= #{"EUR"} (candidate-ids (c/unit :EUR r) r)))
      (is (nil? (candidate-ids (Currency. nil 0 0 nil nil) r)))
      (is (= #{"EUR"} (candidate-ids {:id :EUR} r)))
      (is (nil? (candidate-ids {:id nil} r)))
      (is (= #{"EUR"} (candidate-ids (java.util.Currency/getInstance "EUR") r)))
      (is (nil? (candidate-ids 0 r)))
      (is (= #{"EUR"} (candidate-ids 978 r)))
      (is (= #{"EUR"} (candidate-ids :EUR r)))
      (with-redefs [c/to-numeric-id (fn [_] (throw (RuntimeException. "boom")))]
        (is (nil? (candidate-ids :EUR r)))))

    (testing "same-ids? branches"
      (is (true? (c/same-ids? :EUR :EUR r)))
      (is (false? (c/same-ids? :EUR :PLN r)))
      (is (false? (c/same-ids? 0 :EUR r)))
      (let [r2 (-> (registry/new)
                   (c/register (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))
                   (c/register (c/new :EU2 978 2 :iso/fiat :ISO-4217 0)))]
        (is (true? (c/same-ids? 978 :EUR r2)))))))

(deftest currency-forms-coverage-compare-map-ids
  (let [compare-map-ids #'io.randomseed.bankster.currency/compare-map-ids
        with-weight*    #'io.randomseed.bankster.currency/with-weight*
        eur             (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
        eur-w           (with-weight* eur 7)]
    (testing "compare-map-ids invalid and guard branches"
      (is (nil? (compare-map-ids nil {:id :EUR})))
      (is (nil? (compare-map-ids eur {})))
      (is (nil? (compare-map-ids eur {:id nil})))
      (is (nil? (compare-map-ids eur {:code nil})))
      (is (nil? (compare-map-ids eur {:nr "x"})))
      (is (nil? (compare-map-ids eur {:nr 1 :numeric 2})))
      (is (nil? (compare-map-ids eur {:sc 1 :scale 2})))
      (is (nil? (compare-map-ids eur {:we 1 :weight 2})))
      (is (nil? (compare-map-ids eur {:do :ISO-4217 :domain :CRYPTO})))
      (is (nil? (compare-map-ids eur {:ki :iso/fiat :kind :virtual/asset})))
      (is (nil? (compare-map-ids eur {:id :crypto/USDT :domain :ISO-4217}))))

    (testing "compare-map-ids matching branches"
      (is (= eur (compare-map-ids eur {:id :EUR})))
      (is (= eur (compare-map-ids eur {:code :EUR})))
      (is (= eur (compare-map-ids eur {:nr 978})))
      (is (= eur (compare-map-ids eur {:numeric 978})))
      (is (= eur (compare-map-ids eur {:sc 2 :scale 2})))
      (is (= eur (compare-map-ids eur {:do :ISO-4217 :domain :ISO-4217})))
      (is (= eur (compare-map-ids eur {:ki :iso/fiat :kind :iso/fiat})))
      (is (= eur-w (compare-map-ids eur-w {:we 7}))))))

(deftest currency-forms-coverage-map-new-and-constructors
  (testing "map->new variations"
    (is (nil? (c/map->new nil)))
    (is (nil? (c/map->new {})))
    (let [c1 (c/map->new {:id :eur})]
      (is (instance? Currency c1))
      (is (= :EUR (.id ^Currency c1))))
    (let [c2 (c/map->new {:code :usd})]
      (is (instance? Currency c2))
      (is (= :USD (.id ^Currency c2))))
    (let [c3 (c/map->new {:id :EUR :nr "978" :sc "2" :we "3"
                          :ki :iso/fiat :domain :ISO-4217 :extra 1})]
      (is (= 978 (.numeric ^Currency c3)))
      (is (= 2 (.scale ^Currency c3)))
      (is (= :ISO-4217 (.domain ^Currency c3)))
      (is (= 1 (:extra c3))))
    (let [c4 (c/map->new {:id :EUR :domain nil})]
      (is (nil? (.domain ^Currency c4))))
    (let [c5 (c/map->new {:id :EUR :do :ISO-4217 :nr 978 :sc 2})]
      (is (= :ISO-4217 (.domain ^Currency c5))))
    (let [c6 (c/map->new {:id :usd :numeric 840 :scale 2})]
      (is (= :ISO-4217 (.domain ^Currency c6))))
    (is (nil? (c/map->new {:id nil :nr 1}))))

  (testing "new-currency branches"
    (let [c1 (c/new :ISO-4217/EUR 978 2 :iso/fiat nil)]
      (is (= :EUR (.id ^Currency c1)))
      (is (= :ISO-4217 (.domain ^Currency c1))))
    (let [c2 (c/new :usd 840 2 :iso/fiat nil)]
      (is (= :USD (.id ^Currency c2)))
      (is (= :ISO-4217 (.domain ^Currency c2))))
    (let [c3 (c/new :crypto/USDT c/no-numeric-id 6 :virtual/stable nil)]
      (is (= :CRYPTO (.domain ^Currency c3))))
    (let [c4 (c/new :USD 840 2 :iso/fiat "iso-4217")]
      (is (= :ISO-4217 (.domain ^Currency c4))))
    (is (nil? (c/new nil)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (c/new :crypto/USDT c/no-numeric-id 6 :virtual/stable :ISO-4217)))))

(deftest currency-forms-coverage-predicates-and-kinds
  (let [r (mk-test-registry)
        r-kind (registry/hierarchy-derive :kind :iso/fiat :iso r)
        r-dom  (registry/hierarchy-derive :domain :ISO-4217 :FIAT r)
        r-tr   (registry/hierarchy-derive :traits :fiat :money
                                          (c/add-traits r :EUR [:fiat]))]
    (testing "possible? and iso-possible?"
      (registry/with r
        (is (false? (c/possible? nil)))
        (is (true? (c/possible? :EUR)))
        (is (true? (c/possible? 978))))
      (is (true? (c/possible? 978 r)))
      (is (false? (c/iso-possible? nil)))
      (registry/with r
        (is (true? (c/iso-possible? :EUR))))
      (is (true? (c/iso-possible? (c/new :EUR 978 2 :iso/fiat :ISO-4217 0))))
      (is (true? (c/iso-possible? :EUR r))))

    (testing "kinds and ns/code"
      (is (map? (c/kinds (registry/new))))
      (is (map? (c/kinds r-kind)))
      (registry/with r
        (is (= "EUR" (c/ns-code :EUR)))
        (is (= "crypto/USDT" (c/ns-code :crypto/USDT)))
        (is (= "USDT" (c/code :crypto/USDT)))))

    (testing "has-domain?/has-kind?/has-country?"
      (registry/with r
        (is (true? (c/has-domain? :EUR)))
        (is (true? (c/has-kind? :EUR)))
        (is (false? (c/has-country? :XXX))))
      (is (true? (c/has-domain? :EUR r)))
      (is (true? (c/has-domain? :EUR :ISO-4217)))
      (is (true? (c/has-domain? :EUR :ISO-4217 r)))
      (is (true? (c/has-kind? :EUR r)))
      (is (true? (c/has-kind? :EUR :iso/fiat)))
      (is (true? (c/has-kind? :EUR :iso/fiat r))))

    (testing "of-domain?/of-trait?/has-trait?"
      (registry/with r-dom
        (is (true? (c/of-domain? :FIAT :EUR))))
      (is (true? (c/of-domain? :FIAT :EUR r-dom)))
      (registry/with r-tr
        (is (true? (c/has-trait? :EUR)))
        (is (true? (c/has-trait? :EUR :fiat)))
        (is (true? (c/of-trait? :fiat :EUR)))
        (is (true? (c/of-trait? :money :EUR))))
      (is (true? (c/has-trait? :EUR :fiat r-tr)))
      (is (true? (c/of-trait? :fiat :EUR r-tr)))))
  )

(deftest currency-forms-coverage-info-and-localized
  (let [cur (assoc (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
                   :sc 2 :nr 978 :do :ISO-4217 :ki :iso/fiat)
        r   (-> (registry/new)
                (c/register cur)
                (c/add-countries :EUR [:PL])
                (c/add-traits :EUR [:fiat])
                (c/add-localized-properties :EUR {:en {:symbol "€"}
                                                   :*  {:symbol "E"}})
                (c/set-weight :EUR 5))]
    (testing "info arities and branches"
      (registry/with r
        (is (map? (c/info :EUR)))
        (is (map? (c/info (c/unit :EUR))))
        (is (nil? (c/info :XXX)))))
    (testing "localized-property paths"
      (registry/with r
        (is (= "€" (c/localized-property :symbol :EUR :en)))
        (is (= "€" (c/localized-property :symbol :EUR :en_US)))
        (is (= "E" (c/localized-property :symbol :EUR :fr_FR)))))
    (testing "symbol-native/display-name-native branches"
      (registry/with r
        (is (string? (c/symbol-native :EUR)))
        (is (string? (c/display-name-native :EUR)))))))

(deftest currency-forms-coverage-formatter-extended
  (let [r   (mk-test-registry)
        f1  (c/formatter-extended :EUR)
        f2  (c/formatter-extended :EUR Locale/ROOT)
        f3  (c/formatter-extended :EUR Locale/ROOT {:scale 2} r)
        f4  (c/formatter-extended :EUR Locale/ROOT
                                  {:rounding-mode RoundingMode/HALF_UP
                                   :grouping true
                                   :grouping-size 4
                                   :negative-prefix "n"
                                   :negative-suffix "x"
                                   :positive-prefix "p"
                                   :positive-suffix "s"
                                   :always-sep-dec true
                                   :currency-symbol-fn (fn [_ _ _] "$")
                                   :min-fraction-digits 1
                                   :max-fraction-digits 3
                                   :min-integer-digits 2
                                   :max-integer-digits 9}
                                  r)]
    (is (instance? java.text.DecimalFormat f1))
    (is (instance? java.text.DecimalFormat f2))
    (is (instance? java.text.DecimalFormat f3))
    (is (instance? java.text.DecimalFormat f4))))

(deftest currency-forms-coverage-extra-branches
  (let [iso-strict-code?   #'io.randomseed.bankster.currency/iso-strict-code?
        with-weight*       #'io.randomseed.bankster.currency/with-weight*
        compare-currency-ids #'io.randomseed.bankster.currency/compare-currency-ids
        candidate-ids       #'io.randomseed.bankster.currency/candidate-ids
        normalize-kind-hint #'io.randomseed.bankster.currency/normalize-kind-hint
        compare-map-ids     #'io.randomseed.bankster.currency/compare-map-ids
        r                  (mk-test-registry)
        eur                (c/unit :EUR r)]
    (testing "iso-strict-code? and with-weight* extra branches"
      (is (false? (iso-strict-code? :EU1)))
      (is (false? (iso-strict-code? :eUR)))
      (let [c0 (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)]
        (is (identical? c0 (with-weight* c0 nil)))
        (is (instance? Currency (with-weight* c0 5)))
        (let [c1 (with-weight* c0 5)
              c2 (with-weight* c1 nil)]
          (is (= c0 c2))
          (is (nil? (::c/weight (meta c2)))))))

    (testing "map->new numeric/scale/weight parsing branches"
      (is (instance? Currency (c/map->new {:id :EUR :nr 978 :sc 2 :we 0})))
      (let [c1 (c/map->new {:id :EUR :nr "0" :sc "abc" :we "abc"})]
        (is (instance? Currency c1))
        (is (= c/no-numeric-id (.numeric ^Currency c1)))
        (is (= c/auto-scaled (.scale ^Currency c1))))
      (let [c1b (c/map->new {:id :EUR :nr "abc" :sc 2})]
        (is (instance? Currency c1b))
        (is (= c/no-numeric-id (.numeric ^Currency c1b))))
      (let [c2 (c/map->new {:id :EUR :nr -1 :sc -1 :we -3})]
        (is (instance? Currency c2))
        (is (= c/no-numeric-id (.numeric ^Currency c2)))
        (is (= c/auto-scaled (.scale ^Currency c2))))
      (let [c3 (c/map->new {:id :EUR :numeric 978 :scale 2 :weight 7})]
        (is (instance? Currency c3))
        (is (= 7 (-> c3 meta ::c/weight))))
      (let [c4 (c/map->new {:id :EUR :numeric 978 :scale 2})]
        (is (instance? Currency c4))))

    (testing "compare-currency-ids domain/kind branches"
      (let [cmp1 (Currency. :EUR 978 2 nil nil)
            cmp2 (Currency. :EUR 978 2 :iso/fiat :ISO-4217)
            cmp3 (Currency. :EUR 978 2 :iso/fiat nil)]
        (is (identical? eur (compare-currency-ids eur cmp1)))
        (is (identical? eur (compare-currency-ids eur cmp2)))
        (is (identical? eur (compare-currency-ids eur cmp3)))
        (is (nil? (compare-currency-ids eur (Currency. :EUR 978 2 :virtual/asset :ISO-4217))))))

    (testing "candidate-ids numeric fallback and definitive"
      (is (= #{"EUR"} (candidate-ids (java.util.Currency/getInstance "EUR") r)))
      (is (= #{"999"} (candidate-ids "999" r)))
      (is (= #{"NOPE"} (candidate-ids "NOPE" r))))

    (testing "same-ids? branch with missing candidate IDs"
      (is (false? (c/same-ids? :EUR :NOPE r)))
      (is (false? (c/same-ids? nil :EUR r))))

    (testing "normalize-kind-hint else branch"
      (is (= :123 (normalize-kind-hint 123)))
      (is (nil? (normalize-kind-hint nil))))

    (testing "compare-map-ids sc/do/ki single-key branches"
      (is (= eur (compare-map-ids eur {:sc 2})))
      (is (= eur (compare-map-ids eur {:scale 2})))
      (is (= eur (compare-map-ids eur {:do :ISO-4217})))
      (is (= eur (compare-map-ids eur {:domain :ISO-4217})))
      (is (= eur (compare-map-ids eur {:ki :iso/fiat})))
      (is (= eur (compare-map-ids eur {:kind :iso/fiat})))
      (let [usdt (c/unit :crypto/USDT r)]
        (is (nil? (compare-map-ids usdt {:id :crypto/USDT :domain :ISO-4217})))))

    (testing "map resolve-all filters candidates via keep-in-sorted-set-where"
      (let [r0 (registry/new)
            c1 (c/new :AAA 999 2 :iso/fiat :ISO-4217 0)
            c2 (c/new :BBB 999 2 :iso/fiat :ISO-4217 0)
            r1 (-> r0 (c/register c1) (c/register c2))]
        (registry/with r1
          (is (= #{c1} (c/resolve-all {:id :AAA :numeric 999} r1))))))

    (testing "keep-in-sorted-set-where coverage"
      (let [keep-where #'io.randomseed.bankster.currency/keep-in-sorted-set-where
            s          (sorted-set 1 2)]
        (is (= s (keep-where (constantly true) s)))
        (is (nil? (keep-where (constantly false) s)))
        (is (nil? (keep-where (constantly true) nil)))))))

(deftest currency-forms-coverage-monetary-arities-extra
  (let [r            (mk-test-registry)
        eur          (c/unit :EUR r)
        jc           (java.util.Currency/getInstance "EUR")
        jc-xxx       (java.util.Currency/getInstance "XXX")
        r-id-miss    (assoc r :cur-id->cur {})
        r-code-miss  (assoc r :cur-code->curs {})
        r-num-miss   (assoc r :cur-nr->curs {})
        r-empty      (registry/new)
        r-inconsistent (-> r
                           (assoc :cur-nr->cur {})
                           (assoc :cur-nr->curs {978 [(c/unit :EUR r)]}))]
    (testing "Currency resolve/resolve-all branch combinations"
      (is (instance? Currency (c/resolve eur r)))
      (is (instance? Currency (c/resolve eur r-id-miss)))
      (is (nil? (c/resolve eur r-empty)))
      (is (= #{eur} (c/resolve-all eur r-num-miss)))
      (is (= #{eur} (c/resolve-all eur r-id-miss)))
      (is (nil? (c/resolve-all eur r-empty))))

    (testing "java.util.Currency monetary arities"
      (registry/with r
        (is (instance? Currency (c/resolve jc)))
        (is (set? (c/resolve-all jc)))
        (is (instance? Currency (c/of-id jc)))
        (is (boolean? (c/defined? jc)))
        (is (boolean? (c/present? jc))))
      (is (instance? Currency (c/resolve jc r)))
      (is (set? (c/resolve-all jc r)))
      (is (instance? Currency (c/of-id jc r)))
      (is (map? (c/to-map jc-xxx)))
      (is (instance? Currency (c/to-currency jc-xxx))))

    (testing "java.util.Currency of-id branch with mismatched scale"
      (let [r-mismatch (-> (registry/new)
                           (c/register (c/new :EUR 978 3 :iso/fiat :ISO-4217 0)))]
        (is (thrown? clojure.lang.ExceptionInfo (c/of-id jc r-mismatch)))))

    (testing "Number monetary branches including inconsistency warnings"
      (binding [registry/*warn-on-inconsistency* true
                registry/*warnings-logger*       (fn [_ _] nil)]
        (registry/with r-inconsistent
          (is (instance? Currency (c/resolve 978)))
          (is (instance? Currency (c/of-id 978)))
          (is (true? (c/defined? 978)))
          (is (true? (c/present? 978)))))
      (is (nil? (c/resolve 999 r-empty)))
      (is (nil? (c/resolve-all 999 r-empty)))
      (is (set? (c/resolve-all 978 r-num-miss)))
      (is (true? (c/present? 978 r-num-miss)))
      (is (thrown? clojure.lang.ExceptionInfo (c/id 999 r-empty))))

    (testing "Keyword/String/Symbol resolve branches"
      (let [kw-iso (keyword "ISO-4217" "EUR")
            kw-iso-miss (keyword "ISO-4217" "UNLIKELY")
            kw-ns (keyword "crypto" "USDT")
            kw-miss :NOPE
            sym-iso (symbol "ISO-4217" "EUR")
            sym-ns (symbol "crypto" "USDT")]
        (registry/with r
          (is (nil? (c/resolve kw-miss)))
          (is (instance? Currency (c/resolve kw-iso)))
          (is (nil? (c/resolve kw-iso-miss)))
          (is (set? (c/resolve-all kw-iso)))
          (is (nil? (c/resolve-all kw-iso-miss)))
          (is (instance? Currency (c/resolve kw-ns)))
          (is (set? (c/resolve-all kw-ns)))
          (is (instance? Currency (c/resolve "ISO-4217/EUR")))
          (is (nil? (c/resolve "ISO-4217/UNLIKELY")))
          (is (set? (c/resolve-all "ISO-4217/EUR")))
          (is (nil? (c/resolve-all "ISO-4217/UNLIKELY")))
          (is (instance? Currency (c/resolve "EUR" r-code-miss)))
          (is (instance? Currency (c/resolve sym-iso)))
          (is (instance? Currency (c/resolve sym-ns))))))

    (testing "Map monetary to-map and definitive? branches"
      (let [m-ok {:id :EUR :nr "978" :sc "2" :ki "iso/fiat" :we "3" :do "ISO-4217"}
            m-bad {:id :EUR :nr "abc" :sc "abc" :we "abc" :domain "ISO-4217"}
            m-code {:code :EUR}
            m-full {:id :EUR :domain :ISO-4217 :scale 2 :numeric 978}]
        (is (map? (c/to-map m-ok)))
        (is (false? (c/definitive? {:id :EUR :scale 2 :domain :ISO-4217})))
        (is (map? (c/to-map m-bad)))
        (is (= :EUR (c/to-id m-code)))
        (is (true? (c/definitive? m-full)))
        (is (set? (c/resolve-all m-code r)))))

    (testing "String/Keyword/Symbol ID helpers"
      (registry/with r
        (is (= :EUR (c/id :EUR)))
        (is (= :EUR (c/id "EUR")))
        (is (= :EUR (c/id 'EUR)))
        (is (true? (c/defined? "EUR")))
        (is (true? (c/present? "EUR"))))
      (is (thrown? clojure.lang.ExceptionInfo (c/defined? {:id :EUR})))
      (is (thrown? clojure.lang.ExceptionInfo (c/present? {:id :EUR})))))
  )

(deftest currency-forms-coverage-extend-protocol-hammer
  (let [r             (mk-test-registry)
        eur           (c/unit :EUR r)
        usdt          (c/unit :crypto/USDT r)
        r-empty       (registry/new)
        r-id-miss     (assoc r :cur-id->cur {})
        r-code-miss   (assoc r :cur-code->curs {})
        r-inconsistent (-> r
                           (assoc :cur-nr->cur {})
                           (assoc :cur-nr->curs {978 [eur]}))
        jc            (java.util.Currency/getInstance "EUR")]

    (testing "Currency arities and negative branches"
      (registry/with r
        (is (instance? Currency (c/resolve eur)))
        (is (set? (c/resolve-all eur)))
        (is (instance? Currency (c/of-id eur)))
        (is (boolean? (c/defined? eur)))
        (is (boolean? (c/present? eur))))
      (is (instance? Currency (c/resolve eur r-id-miss)))
      (is (nil? (c/resolve (c/new :ZZZ 999 2 :iso/fiat :ISO-4217 0) r-empty)))
      (is (false? (c/defined? (c/new :ZZZ 999 2 :iso/fiat :ISO-4217 0) r-empty)))
      (is (false? (c/present? (c/new :ZZZ 999 2 :iso/fiat :ISO-4217 0) r-empty))))

    (testing "java.util.Currency arity-1 paths"
      (registry/with r
        (is (instance? Currency (c/resolve jc)))
        (is (set? (c/resolve-all jc)))
        (is (instance? Currency (c/of-id jc)))
        (is (boolean? (c/defined? jc)))
        (is (boolean? (c/present? jc)))))

    (testing "Number arities and error paths"
      (binding [registry/*warn-on-inconsistency* true
                registry/*warnings-logger*       (fn [_ _] nil)]
        (registry/with r-inconsistent
          (is (instance? Currency (c/resolve 978)))
          (is (coll? (c/resolve-all 978)))
          (is (instance? Currency (c/of-id 978)))
          (is (instance? Currency (c/unit 978)))
          (is (true? (c/defined? 978)))
          (is (true? (c/present? 978)))))
      (is (nil? (c/resolve 999 r-empty)))
      (is (nil? (c/resolve-all 999 r-empty)))
      (is (false? (c/defined? 999 r-empty)))
      (is (false? (c/present? 999 r-empty)))
      (is (thrown? clojure.lang.ExceptionInfo (c/id 999 r-empty)))
      (is (thrown? clojure.lang.ExceptionInfo (c/unit 999 r-empty))))

    (testing "Keyword and String branches with and without code mapping"
      (registry/with r
        (is (instance? Currency (c/resolve :EUR)))
        (is (set? (c/resolve-all :EUR)))
        (is (instance? Currency (c/resolve "EUR")))
        (is (set? (c/resolve-all "EUR"))))
      (is (nil? (c/resolve :NOPE r)))
      (is (nil? (c/resolve-all :NOPE r)))
      (is (instance? Currency (c/resolve "EUR" r-code-miss)))
      (is (set? (c/resolve-all "EUR" r-code-miss)))
      (is (= :EUR (c/id :EUR nil)))
      (is (false? (c/present? :NOPE r)))
      (is (true? (c/present? :EUR r-code-miss))))

    (testing "String namespaced branches"
      (is (instance? Currency (c/resolve "crypto/USDT" r)))
      (is (set? (c/resolve-all "crypto/USDT" r)))
      (is (nil? (c/resolve "crypto/NOPE" r)))
      (is (nil? (c/resolve-all "crypto/NOPE" r))))

    (testing "Symbol branches"
      (registry/with r
        (is (instance? Currency (c/resolve 'EUR)))
        (is (set? (c/resolve-all 'EUR)))))

    (testing "Map coercions and errors"
      (let [m-code {:code "EUR"}
            m-num  {:numeric 978}
            m-bad  {:code :EUR :numeric "abc" :scale "abc" :domain ""}]
        (is (= :EUR (c/to-id m-code)))
        (is (= :EUR (c/to-code m-code)))
        (is (= "EUR" (c/to-id-str m-code)))
        (is (= "EUR" (c/to-code-str m-code)))
        (is (= 978 (c/to-numeric-id m-num)))
        (is (= c/no-numeric-id (c/to-numeric-id {})))
        (is (map? (c/to-map m-bad)))
        (is (true? (c/definitive? {:id :EUR :nr 978 :sc 2 :do :ISO-4217})))
        (is (false? (c/definitive? {:id :EUR}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (c/unit {:id "EUR" :numeric 999 :scale 2 :domain :ISO-4217} r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/defined? {:id :EUR} r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/present? {:id :EUR} r))))

    (testing "candidate-ids nil to-id-str"
      (let [candidate-ids #'io.randomseed.bankster.currency/candidate-ids]
        (with-redefs [c/to-id-str (constantly nil)]
          (is (nil? (candidate-ids {:id :EUR} r))))))))

(deftest currency-forms-coverage-predicates-extra
  (let [r       (mk-test-registry)
        no-kind (Currency. :ZZZ 999 2 nil :ISO-4217)]
    (testing "possible?/iso-possible? branches"
      (registry/with r
        (is (true? (c/possible? 978)))
        (is (true? (c/iso-possible? 978))))
      (is (true? (c/possible? 978 r)))
      (is (true? (c/iso-possible? (c/new :EUR 978 2 :iso/fiat :ISO-4217 0) r))))

    (testing "has-domain?/has-kind? registry-or-tag branches"
      (registry/with r
        (is (true? (c/has-domain? :EUR)))
        (is (true? (c/has-domain? :EUR :ISO-4217))))
      (is (true? (c/has-domain? :EUR r)))
      (is (true? (c/has-domain? :EUR :ISO-4217 r)))
      (is (false? (c/has-kind? no-kind)))
      (is (true? (c/has-kind? :EUR r)))
      (is (true? (c/has-kind? :EUR :iso/fiat)))
      (is (true? (c/has-kind? :EUR :iso/fiat r))))

    (testing "of-kind?/of-trait? hierarchy branches"
      (let [r-kind (registry/hierarchy-derive :kind :iso/fiat :iso r)
            r-tr   (registry/hierarchy-derive :traits :fiat :money
                                              (c/add-traits r :EUR [:fiat]))]
        (is (true? (c/of-kind? :iso :EUR r-kind)))
        (is (false? (c/of-kind? :iso no-kind r)))
        (is (true? (c/of-trait? :money :EUR r-tr)))))

    (testing "none? seqable vs non-seqable"
      (is (true? (c/none? [] r)))
      (is (false? (c/none? :EUR r))))))

(deftest currency-forms-coverage-java-formatters-extra
  (let [r      (mk-test-registry)
        eur    (c/unit :EUR r)
        no-num (Currency. :XNN c/no-numeric-id 2 :iso/fiat :ISO-4217)]
    (testing "java conversion branches"
      (registry/with r
        (is (instance? java.util.Currency (c/java :EUR)))
        (is (nil? (c/java :crypto/USDT))))
      (is (nil? (c/java no-num r)))
      (let [bad (Currency. :EUR 978 c/auto-scaled :iso/fiat :ISO-4217)]
        (is (nil? (c/java bad r)))))

    (testing "formatter non-ISO branch in formatter-instance"
      (registry/with r
        (is (instance? java.text.DecimalFormat (c/formatter :crypto/USDT)))))

    (testing "data-literal empty map branch"
      (is (= '(quote nil) (c/data-literal {}))))

    (testing "print-method includes optional fields"
      (let [with-w (-> (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
                       (with-meta {::c/weight 5}))]
        (is (re-find #":weight 5" (pr-str with-w))))
      (let [no-num (c/new :XNN c/no-numeric-id c/auto-scaled :iso/null :ISO-4217 0)
            s      (pr-str no-num)]
        (is (not (re-find #":numeric" s)))
        (is (not (re-find #":scale" s)))))

    (testing "to-json/to-edn nil branches"
      (is (nil? (c/to-json-map nil)))
      (is (nil? (c/to-json-string nil)))
      (is (nil? (c/to-edn-map nil)))
      (is (nil? (c/to-edn-string nil)))))
  )

(deftest currency-forms-coverage-config-default-extra
  (testing "config->registry nil config branch and nil version"
    (with-redefs [config/load (constantly nil)]
      (is (instance? Registry (c/config->registry "missing" (registry/new)))))
    (with-redefs [config/load (fn [_] {:hierarchies nil :currencies {} :countries {} :localized {} :traits {}
                                       :weights {} :version nil})]
      (is (instance? Registry (c/config->registry "dummy" (registry/new))))))

  (testing "set-default-registry! multiple paths branch"
    (with-redefs [c/config->registry (fn [& _] (registry/new))
                  registry/set!       (fn [_] (registry/new))
                  registry/update!    (fn [_] nil)]
      (c/set-default-registry! "one" "two"))))

(deftest currency-forms-coverage-attempt-and-ids-extra
  (let [r                   (mk-test-registry)
        eur                 (c/unit :EUR r)
        compare-currency-ids #'io.randomseed.bankster.currency/compare-currency-ids
        compare-map-ids      #'io.randomseed.bankster.currency/compare-map-ids
        candidate-ids        #'io.randomseed.bankster.currency/candidate-ids]
    (testing "compare-currency-ids and compare-map-ids branches"
      (let [same      (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
            no-domain (c/new :EUR 978 2 :iso/fiat nil 0)
            no-kind   (Currency. :EUR 978 2 nil :ISO-4217)]
        (is (= eur (compare-currency-ids eur same)))
        (is (= eur (compare-currency-ids eur no-domain)))
        (is (= eur (compare-currency-ids eur no-kind)))
        (is (nil? (compare-currency-ids nil same))))
      (let [m-full {:id :EUR :code :EUR :nr 978 :sc 2 :we 0 :do :ISO-4217 :ki :iso/fiat}
            m-bad  {:id :crypto/USDT :domain :ISO-4217}]
        (is (= eur (compare-map-ids eur m-full)))
        (is (nil? (compare-map-ids eur m-bad)))))

    (testing "candidate-ids definitive? branches"
      (with-redefs [c/definitive? (constantly true)
                    c/to-id-str   (constantly "EUR")]
        (is (= #{"EUR"} (candidate-ids :X r))))
      (with-redefs [c/definitive? (constantly true)
                    c/to-id-str   (constantly nil)]
        (is (nil? (candidate-ids :X r)))))

    (testing "same-ids? count branches"
      (let [r1 (-> (registry/new)
                   (c/register (c/new :AAA 999 2 :iso/fiat :ISO-4217 0))
                   (c/register (c/new :BBB 999 2 :iso/fiat :ISO-4217 0)))]
        (is (true? (c/same-ids? :AAA "999" r1)))
        (is (true? (c/same-ids? "999" :AAA r1)))
        (is (false? (c/same-ids? nil :AAA r1)))))

    (testing "attempt* and attempt branches"
      (is (seq (macroexpand '(c/attempt* :EUR))))
      (is (seq (macroexpand '(c/attempt* :EUR nil))))
      (is (seq (macroexpand '(c/attempt* :EUR registry))))
      (is (seq (macroexpand '(c/with-attempt :EUR r [x] x))))
      (registry/with r
        (is (instance? Currency (c/attempt* eur)))
        (is (instance? Currency (c/attempt* {:id :EUR :domain :ISO-4217 :numeric 978 :scale 2})))
        (is (instance? Currency (c/attempt* (java.util.Currency/getInstance "EUR"))))
        (is (instance? Currency (c/attempt* :EUR)))
        (is (nil? (c/attempt* :NOPE))))
      (is (instance? Currency (c/attempt* :EUR r)))
      (is (nil? (c/attempt* :NOPE r)))
      (with-redefs [c/resolve (constantly nil)]
        (is (nil? (c/attempt* 978 r))))
      (registry/with r
        (is (identical? eur (c/attempt eur)))
        (is (instance? Currency (c/attempt {:id :EUR :domain :ISO-4217 :numeric 978 :scale 2}))))
      (with-redefs [c/definitive? (constantly true)
                    c/to-currency (constantly nil)
                    c/resolve     (constantly eur)]
        (is (identical? eur (c/attempt :X)))
        (is (identical? eur (c/attempt :X r))))
      (with-redefs [c/definitive? (constantly false)
                    c/resolve     (constantly nil)]
        (is (nil? (c/attempt :X)))
        (is (nil? (c/attempt :X r)))))
    ))

(deftest currency-all-selectors-coverage
  (let [r (mk-test-registry)]
    (registry/with r
      (is (seq (c/all)))
      (is (seq (c/ids)))
      (is (seq (c/numerics)))
      (is (seq (c/numeric-ids)))
      (is (seq (c/codes)))
      (is (seq (c/domains)))
      (is (= #{:EUR :PLN}
             (set (map c/id (c/of-domain :ISO-4217))))
          "of-domain uses default registry when bound"))
    (is (seq (c/all r)))
    (is (seq (c/ids r)))
    (is (seq (c/numerics r)))
    (is (seq (c/numeric-ids r)))
    (is (seq (c/codes r)))
    (is (seq (c/domains r)))
    (is (= #{:EUR :PLN}
           (set (map c/id (c/of-domain :ISO-4217 r)))))
    (is (= #{:crypto/USDT}
           (set (map c/id (c/of-domain "crypto" r)))))))

(deftest currency-forms-coverage-monetary-extra-2
  (let [r             (mk-test-registry)
        eur           (c/unit :EUR r)
        jc            (java.util.Currency/getInstance "EUR")
        r-by-id-only  (-> r (assoc :cur-nr->curs {}) (assoc :cur-nr->cur {}))
        r-inconsistent (-> r
                           (assoc :cur-id->cur {})
                           (assoc :cur-nr->curs {978 [eur]})
                           (assoc :cur-nr->cur  {}))
        r-num-inconsistent (-> r
                               (assoc :cur-nr->cur {})
                               (assoc :cur-nr->curs {978 [eur]}))
        r-empty       (registry/new)]
    (testing "Currency resolve/resolve-all branches"
      (registry/with r
        (is (instance? Currency (c/resolve eur)))
        (is (set? (c/resolve-all eur)))
        (is (instance? Currency (c/of-id eur)))
        (is (true? (c/defined? eur)))
        (is (true? (c/present? eur))))
      (is (some #(= eur %) (c/resolve-all eur r-by-id-only)))
      (is (some #(= eur %) (c/resolve-all eur r-inconsistent)))
      (is (nil? (c/resolve-all eur r-empty)))
      (is (instance? Currency (c/resolve eur r-inconsistent)))
      (is (nil? (c/resolve (c/new :EUR 978 2 :iso/fiat :CRYPTO 0) r-empty))))

    (testing "java.util.Currency resolve/of-id branches"
      (registry/with r
        (is (instance? Currency (c/resolve jc)))
        (is (set? (c/resolve-all jc)))
        (is (instance? Currency (c/of-id jc))))
      (is (instance? Currency (c/resolve jc r)))
      (is (set? (c/resolve-all jc r)))
      (let [mismatch (c/new :EUR 978 3 :iso/fiat :ISO-4217 0)]
        (with-redefs [registry/currency-nr->currencies* (fn [& _] [mismatch eur])]
          (is (instance? Currency (c/of-id jc r)))))
      (is (thrown? clojure.lang.ExceptionInfo (c/of-id jc r-empty))))

    (testing "Number protocol branches and warnings"
      (registry/with r-num-inconsistent
        (is (instance? Currency (c/resolve 978)))
        (is (coll? (c/resolve-all 978)))
        (is (instance? Currency (c/of-id 978)))
        (is (instance? Currency (c/unit 978)))
        (is (true? (c/defined? 978)))
        (is (true? (c/present? 978)))
        (is (= :EUR (c/id 978))))
      (is (nil? (c/resolve 999 r-empty)))
      (is (nil? (c/resolve-all 999 r-empty)))
      (is (false? (c/defined? 999 r-empty)))
      (is (false? (c/present? 999 r-empty)))
      (is (thrown? clojure.lang.ExceptionInfo (c/id 999 r-empty))))

    (testing "Keyword/String/Symbol Monetary branches"
      (registry/with r
        (is (instance? Currency (c/resolve :EUR)))
        (is (set? (c/resolve-all :EUR)))
        (is (instance? Currency (c/resolve :ISO-4217/EUR)))
        (is (set? (c/resolve-all :ISO-4217/EUR)))
        (is (instance? Currency (c/resolve :crypto/USDT)))
        (is (set? (c/resolve-all :crypto/USDT)))
        (is (instance? Currency (c/resolve "EUR")))
        (is (set? (c/resolve-all "EUR")))
        (is (instance? Currency (c/resolve "ISO-4217/EUR")))
        (is (set? (c/resolve-all "ISO-4217/EUR")))
        (is (instance? Currency (c/resolve "crypto/USDT")))
        (is (set? (c/resolve-all "crypto/USDT")))
        (is (instance? Currency (c/resolve 'EUR)))
        (is (set? (c/resolve-all 'EUR))))
      (is (instance? Currency (c/resolve :EUR r)))
      (is (instance? Currency (c/resolve "EUR" r)))
      (is (instance? Currency (c/resolve 'EUR r)))
      (is (instance? Currency (c/of-id :EUR r)))
      (is (= :EUR (c/id :EUR r)))
      (is (true? (c/defined? :EUR r)))
      (is (true? (c/present? :EUR r)))
      (is (false? (c/present? :NOPE r))))

    (testing "Map Monetary branches"
      (let [m-code {:code "EUR"}
            m-id   {:id :EUR}
            m-full {:id :EUR :code :EUR :nr "978" :sc "2" :we "0" :do "ISO-4217" :ki "iso/fiat"}
            m-nr   {:nr 978}
            m-num  {:numeric 978}]
        (is (= :EUR (c/to-id m-code)))
        (is (= :EUR (c/to-code m-code)))
        (is (= "EUR" (c/to-id-str m-code)))
        (is (= "EUR" (c/to-code-str m-code)))
        (is (map? (c/to-map m-full)))
        (is (true? (c/definitive? {:id :EUR :domain :ISO-4217 :scale 2 :numeric 978})))
        (is (false? (c/definitive? {:id :EUR})))
        (is (set? (c/resolve-all m-nr r)))
        (is (set? (c/resolve-all m-num r)))
        (is (instance? Currency (c/resolve m-id r)))
        (is (instance? Currency (c/unit m-id r)))
        (is (thrown? clojure.lang.ExceptionInfo (c/defined? m-id r)))
        (is (thrown? clojure.lang.ExceptionInfo (c/present? m-id r))))
      (registry/with r
        (is (instance? Currency (c/resolve {:id :EUR})))
        (is (instance? Currency (c/unit {:id :EUR})))
        (is (thrown? clojure.lang.ExceptionInfo (c/defined? {:id :EUR})))
        (is (thrown? clojure.lang.ExceptionInfo (c/present? {:id :EUR})))))))

(deftest currency-forms-coverage-predicates-helpers-extra-2
  (let [r       (mk-test-registry)
        eur     (c/unit :EUR r)
        usdt    (c/unit :crypto/USDT r)
        auto    (c/new :XNN c/no-numeric-id c/auto-scaled :iso/null :ISO-4217 0)
        no-kind (Currency. :XNN c/no-numeric-id c/auto-scaled nil :ISO-4217)]
    (testing "nr/sc/domain/kind/kinds/ns-code/code/weight helpers"
      (registry/with r
        (is (= 978 (c/nr :EUR)))
        (is (nil? (c/nr :crypto/USDT)))
        (is (= 2 (c/sc :EUR)))
        (is (nil? (c/sc auto)))
        (is (= :ISO-4217 (c/domain :EUR)))
        (is (nil? (c/domain :NOPE)))
        (is (= :iso/fiat (c/kind :EUR)))
        (is (nil? (c/kind no-kind)))
        (is (string? (c/ns-code :EUR)))
        (is (string? (c/code :EUR)))
        (is (nil? (c/ns-code nil)))
        (is (nil? (c/code nil)))
        (is (number? (c/weight :EUR)))
        (is (nil? (c/weight :NOPE))))
      (is (= 978 (c/nr :EUR r)))
      (is (= 2 (c/sc :EUR r)))
      (is (= :ISO-4217 (c/domain :EUR r)))
      (is (= :iso/fiat (c/kind :EUR r)))
      (is (string? (c/ns-code :EUR r)))
      (is (string? (c/code :EUR r)))
      (is (number? (c/weight :EUR r))))

    (testing "kinds branches"
      (is (map? (c/kinds r)))
      (with-redefs [registry/hierarchy* (fn [& _] nil)]
        (is (empty? (c/kinds r)))))

    (testing "with-weight/set-weight/clear-weight branches"
      (let [w0 (c/with-weight eur nil)
            w1 (c/with-weight eur 7)
            r1 (c/set-weight r eur 5)
            r2 (c/set-weight r :crypto/USDT 3)
            r3 (c/clear-weight r2 eur)
            r4 (c/clear-weight r2 :crypto/USDT)]
        (is (= 0 (#'io.randomseed.bankster.currency/currency-weight w0)))
        (is (= 7 (#'io.randomseed.bankster.currency/currency-weight w1)))
        (is (instance? Registry r1))
        (is (instance? Registry r2))
        (is (instance? Registry r3))
        (is (instance? Registry r4))))

    (testing "possible?/iso-possible? branches"
      (is (false? (c/possible? nil)))
      (is (true? (c/possible? :EUR r)))
      (is (true? (c/possible? 978 r)))
      (is (false? (c/possible? 999 r)))
      (is (false? (c/iso-possible? nil)))
      (with-redefs [c/to-currency (constantly eur)]
        (is (true? (c/iso-possible? :EUR r))))
      (is (true? (c/iso-possible? 978 r)))
      (is (false? (c/iso-possible? 999 r))))

    (testing "has-numeric-id?/has-country?/has-domain?/of-domain?"
      (is (true? (c/has-numeric-id? :EUR r)))
      (is (false? (c/has-numeric-id? :crypto/USDT r)))
      (let [r-country (c/add-countries r :EUR [:PL])]
        (is (true? (c/has-country? :EUR r-country)))
        (is (false? (c/has-country? :NOPE r-country))))
      (is (true? (c/has-domain? :EUR r)))
      (is (true? (c/has-domain? :EUR :ISO-4217 r)))
      (is (false? (c/has-domain? :EUR :CRYPTO r)))
      (is (true? (c/has-domain? :EUR nil)))
      (let [r-domain (registry/hierarchy-derive :domain :ISO-4217 :iso r)]
        (is (true? (c/of-domain? :iso :EUR r-domain)))
        (is (false? (c/of-domain? :CRYPTO :EUR r)))))

    (testing "has-kind?/has-trait?/of-trait?/crypto?/old?"
      (is (true? (c/has-kind? :EUR r)))
      (is (true? (c/has-kind? :EUR :iso/fiat r)))
      (is (false? (c/has-kind? :EUR :virtual r)))
      (is (true? (c/has-trait? :EUR r)))
      (is (true? (c/has-trait? :EUR :fiat r)))
      (is (false? (c/has-trait? :EUR :nope r)))
      (let [r-tr (registry/hierarchy-derive :traits :fiat :money r)]
        (is (true? (c/of-trait? :money :EUR r-tr)))
        (is (false? (c/of-trait? :nope :EUR r))))
      (is (true? (c/crypto? :crypto/USDT r)))
      (is (false? (c/old? :EUR r))))))

(deftest currency-forms-coverage-prep-localized-formatter-extra-2
  (let [prep-propagate-keys   #'io.randomseed.bankster.currency/prep-propagate-keys
        prep-currency         #'io.randomseed.bankster.currency/prep-currency
        prep-currencies       #'io.randomseed.bankster.currency/prep-currencies
        prep-cur->ctr         #'io.randomseed.bankster.currency/prep-cur->ctr
        prep-country-ids      #'io.randomseed.bankster.currency/prep-country-ids
        prep-localized-props  #'io.randomseed.bankster.currency/prep-localized-props
        prep-all-localized    #'io.randomseed.bankster.currency/prep-all-localized-props
        prep-traits           #'io.randomseed.bankster.currency/prep-traits
        prep-all-traits       #'io.randomseed.bankster.currency/prep-all-traits
        register-numeric      #'io.randomseed.bankster.currency/register-numeric
        r                     (mk-test-registry)
        eur                   (c/unit :EUR r)]
    (testing "prep-propagate-keys branches"
      (is (nil? (prep-propagate-keys nil)))
      (is (= #{:foo} (prep-propagate-keys [:foo nil])))
      (is (= #{:foo} (prep-propagate-keys #{:foo})))
      (is (= #{:foo} (prep-propagate-keys (list :foo))))
      (is (= #{:foo} (prep-propagate-keys :foo)))
      (is (nil? (prep-propagate-keys [:numeric]))))

    (testing "prep-currency and prep-currencies branches"
      (let [c1 (prep-currency :EUR {:numeric "978" :kind "iso/fiat" :scale "2"
                                    :domain "ISO-4217" :weight "3"
                                    :propagate-keys [:foo] :foo "bar"} nil)
            c2 (prep-currency :USD {:numeric 840 :kind :iso/fiat :scale 2 :domain :ISO-4217}
                              [:bar])
            c3 (prep-currency "EUR" "978" "iso/fiat" "-1" :ISO-4217 "2")]
        (is (instance? Currency c1))
        (is (= "bar" (:foo c1)))
        (is (instance? Currency c2))
        (is (instance? Currency c3)))
      (is (seq (doall (prep-currencies {:EUR {:numeric 978 :scale 2 :kind :iso/fiat :domain :ISO-4217}}
                                        [:foo])))))

    (testing "prep-country-ids/cur->ctr/localized/traits helpers"
      (is (= #{:pl :us} (set (prep-country-ids [:pl :us]))))
      (is (map? (prep-cur->ctr {:PL :EUR :US :USD})))
      (is (map? (prep-localized-props {:en {:name "Euro"} :* {:symbol "€"}})))
      (is (map? (prep-all-localized {:EUR {:en {:name "Euro"}}})))
      (is (= #{:fiat} (prep-traits [:fiat])))
      (is (map? (prep-all-traits {:EUR [:fiat] :USD nil}))))

    (testing "register-numeric invalid numeric and unregister buckets"
      (let [r0 (registry/new)
            c0 (c/new :XNN c/no-numeric-id 2 :iso/null :ISO-4217 0)
            c1 (c/new :AAA 999 2 :iso/fiat :ISO-4217 0)
            c2 (c/new :BBB 999 2 :iso/fiat :ISO-4217 0)
            r1 (-> (registry/new) (c/register c1) (c/register c2))
            r2 (c/unregister r1 :AAA)
            r3 (c/unregister r2 :BBB)]
        (is (identical? r0 (register-numeric r0 c0)))
        (is (instance? Registry r2))
        (is (instance? Registry r3))))

    (testing "remove-countries/remove-localized-properties nil branches"
      (let [r0 (registry/new)]
        (is (identical? r0 (c/remove-countries r0 nil)))
        (is (identical? r0 (c/remove-localized-properties r0 nil)))))

    (testing "countries/java/info/formatter/localized fallback branches"
      (let [r1 (-> (registry/new)
                   (c/register (c/new :EUR 978 2 :iso/fiat :ISO-4217 0) :PL {:en {:symbol "€" :name "Euro"}
                                                                              :*  {:symbol "E" :name "Euro"}}))
            r2 (c/register r1 (c/new :crypto/USDT c/no-numeric-id 6 :virtual/stable :CRYPTO 0))]
        (is (set? (c/countries :EUR r1)))
        (is (thrown? clojure.lang.ExceptionInfo (c/countries :NOPE r1)))
        (is (string? (c/symbol :EUR Locale/ROOT r1)))
        (is (string? (c/display-name :EUR Locale/ROOT r1)))
        (is (string? (c/symbol :crypto/USDT Locale/ROOT r2)))
        (is (string? (c/display-name :crypto/USDT Locale/ROOT r2)))
        (is (string? (c/localized-property :symbol :EUR :en_GB r1)))
        (is (string? (c/localized-property :symbol :EUR :pl_PL r1)))
        (is (instance? java.util.Currency (c/java :EUR r1)))
        (is (nil? (c/java :crypto/USDT r1)))
        (is (nil? (c/java (c/new :XNN c/no-numeric-id c/auto-scaled :iso/null :ISO-4217 0) r1)))
        (is (map? (c/info :EUR r1)))
        (is (nil? (c/info :NOPE r1)))
        (is (instance? java.text.DecimalFormat (c/formatter :EUR Locale/ROOT r1)))
        (is (instance? java.text.DecimalFormat (c/formatter :EUR Locale/ROOT)))
        (is (instance? java.text.DecimalFormat (c/formatter :EUR)))))))

(deftest currency-protocol-wrapper-arities-coverage
  (let [r    (mk-test-registry)
        eur  (c/unit :EUR r)
        jc   (java.util.Currency/getInstance "EUR")
        num  978
        kw   :EUR
        stri "EUR"
        sym  'EUR
        m    {:id :EUR :nr 978 :sc 2 :domain :ISO-4217 :kind :iso/fiat}]
    (testing "arity-1 wrappers use registry/get"
      (registry/with r
        (is (instance? Currency (c/resolve eur)))
        (is (set? (c/resolve-all eur)))
        (is (instance? Currency (c/of-id eur)))
        (is (instance? Currency (c/unit eur)))
        (is (= :EUR (c/id eur)))
        (is (true? (c/defined? eur)))
        (is (true? (c/present? eur)))

        (is (instance? Currency (c/resolve jc)))
        (is (set? (c/resolve-all jc)))
        (is (instance? Currency (c/of-id jc)))
        (is (instance? Currency (c/unit jc)))
        (is (= :EUR (c/id jc)))
        (is (true? (c/defined? jc)))
        (is (true? (c/present? jc)))

        (is (instance? Currency (c/resolve num)))
        (is (set? (c/resolve-all num)))
        (is (instance? Currency (c/of-id num)))
        (is (instance? Currency (c/unit num)))
        (is (= :EUR (c/id num)))
        (is (true? (c/defined? num)))
        (is (true? (c/present? num)))

        (is (instance? Currency (c/resolve kw)))
        (is (set? (c/resolve-all kw)))
        (is (instance? Currency (c/of-id kw)))
        (is (instance? Currency (c/unit kw)))
        (is (= :EUR (c/id kw)))
        (is (true? (c/defined? kw)))
        (is (true? (c/present? kw)))

        (is (instance? Currency (c/resolve stri)))
        (is (set? (c/resolve-all stri)))
        (is (instance? Currency (c/of-id stri)))
        (is (instance? Currency (c/unit stri)))
        (is (= :EUR (c/id stri)))
        (is (true? (c/defined? stri)))
        (is (true? (c/present? stri)))

        (is (instance? Currency (c/resolve sym)))
        (is (set? (c/resolve-all sym)))
        (is (instance? Currency (c/of-id sym)))
        (is (instance? Currency (c/unit sym)))
        (is (= :EUR (c/id sym)))
        (is (true? (c/defined? sym)))
        (is (true? (c/present? sym)))

        (is (instance? Currency (c/resolve m)))
        (is (set? (c/resolve-all m)))
        (is (instance? Currency (c/unit m)))))

    (testing "explicit registry arities"
      (is (instance? Currency (c/resolve eur r)))
      (is (set? (c/resolve-all eur r)))
      (is (instance? Currency (c/of-id eur r)))
      (is (instance? Currency (c/unit eur r)))
      (is (= :EUR (c/id eur r)))
      (is (true? (c/defined? eur r)))
      (is (true? (c/present? eur r)))

      (is (instance? Currency (c/resolve jc r)))
      (is (set? (c/resolve-all jc r)))
      (is (instance? Currency (c/of-id jc r)))
      (is (instance? Currency (c/unit jc r)))
      (is (= :EUR (c/id jc r)))
      (is (true? (c/defined? jc r)))
      (is (true? (c/present? jc r)))

      (is (instance? Currency (c/resolve num r)))
      (is (set? (c/resolve-all num r)))
      (is (instance? Currency (c/of-id num r)))
      (is (instance? Currency (c/unit num r)))
      (is (= :EUR (c/id num r)))
      (is (true? (c/defined? num r)))
      (is (true? (c/present? num r)))

      (is (instance? Currency (c/resolve kw r)))
      (is (set? (c/resolve-all kw r)))
      (is (instance? Currency (c/of-id kw r)))
      (is (instance? Currency (c/unit kw r)))
      (is (= :EUR (c/id kw r)))
      (is (true? (c/defined? kw r)))
      (is (true? (c/present? kw r)))

      (is (instance? Currency (c/resolve stri r)))
      (is (set? (c/resolve-all stri r)))
      (is (instance? Currency (c/of-id stri r)))
      (is (instance? Currency (c/unit stri r)))
      (is (= :EUR (c/id stri r)))
      (is (true? (c/defined? stri r)))
      (is (true? (c/present? stri r)))

      (is (instance? Currency (c/resolve sym r)))
      (is (set? (c/resolve-all sym r)))
      (is (instance? Currency (c/of-id sym r)))
      (is (instance? Currency (c/unit sym r)))
      (is (= :EUR (c/id sym r)))
      (is (true? (c/defined? sym r)))
      (is (true? (c/present? sym r)))

      (is (instance? Currency (c/resolve m r)))
      (is (set? (c/resolve-all m r)))
      (is (instance? Currency (c/unit m r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/id m r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/of-id m r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/defined? m r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/present? m r))))

    (testing "to-* and definitive?"
      (is (= :EUR (c/to-id eur)))
      (is (= :EUR (c/to-code eur)))
      (is (= "EUR" (c/to-id-str eur)))
      (is (= "EUR" (c/to-code-str eur)))
      (is (= 978 (c/to-numeric-id eur)))
      (is (instance? Currency (c/to-currency eur)))
      (is (map? (c/to-map eur)))
      (is (true? (c/definitive? eur)))

      (is (keyword? (c/to-id jc)))
      (is (keyword? (c/to-code jc)))
      (is (string? (c/to-id-str jc)))
      (is (string? (c/to-code-str jc)))
      (is (number? (c/to-numeric-id jc)))
      (is (map? (c/to-map jc)))
      (is (true? (c/definitive? jc)))

      (is (nil? (c/to-id num)))
      (is (nil? (c/to-code num)))
      (is (nil? (c/to-id-str num)))
      (is (nil? (c/to-code-str num)))
      (is (= 978 (c/to-numeric-id num)))
      (is (map? (c/to-map num)))
      (is (false? (c/definitive? num)))

      (is (keyword? (c/to-id kw)))
      (is (keyword? (c/to-code kw)))
      (is (string? (c/to-id-str kw)))
      (is (string? (c/to-code-str kw)))
      (is (= c/no-numeric-id (c/to-numeric-id kw)))
      (is (false? (c/definitive? kw)))

      (is (keyword? (c/to-id stri)))
      (is (keyword? (c/to-code stri)))
      (is (string? (c/to-id-str stri)))
      (is (string? (c/to-code-str stri)))
      (is (nil? (c/to-numeric-id stri)))
      (is (false? (c/definitive? stri)))

      (is (keyword? (c/to-id sym)))
      (is (keyword? (c/to-code sym)))
      (is (string? (c/to-id-str sym)))
      (is (string? (c/to-code-str sym)))
      (is (nil? (c/to-numeric-id sym)))
      (is (false? (c/definitive? sym)))

      (is (keyword? (c/to-id m)))
      (is (keyword? (c/to-code m)))
      (is (string? (c/to-id-str m)))
      (is (string? (c/to-code-str m)))
      (is (number? (c/to-numeric-id m)))
      (is (map? (c/to-map m)))
      (is (true? (c/definitive? m))))))

(deftest currency-compare-map-ids-and-same-ids-coverage-extra
  (let [compare-map-ids #'io.randomseed.bankster.currency/compare-map-ids
        eur             (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
        r1              (mk-dup-num-registry)]
    (testing "compare-map-ids invalid hint branches"
      (is (nil? (compare-map-ids eur {:id nil})))
      (is (nil? (compare-map-ids eur {:code nil})))
      (is (nil? (compare-map-ids eur {:nr 1 :numeric 2})))
      (is (nil? (compare-map-ids eur {:sc 1 :scale 2})))
      (is (nil? (compare-map-ids eur {:we 1 :weight 2})))
      (is (nil? (compare-map-ids eur {:do :ISO-4217 :domain :CRYPTO})))
      (is (nil? (compare-map-ids eur {:ki :iso/fiat :kind :virtual})))
      (is (nil? (compare-map-ids eur {:id :crypto/USDT :domain :ISO-4217}))))
    (testing "compare-map-ids with full match"
      (is (= eur (compare-map-ids eur {:id :EUR :code :EUR :nr 978 :sc 2 :we 0 :do :ISO-4217 :ki :iso/fiat}))))
    (testing "same-ids? count branches"
      (is (true? (c/same-ids? :AAA "999" r1)))
      (is (true? (c/same-ids? "999" :AAA r1))))))

(deftest currency-traits-helper-coverage
  (let [r0     (mk-test-registry)
        h      (-> (make-hierarchy)
                   (derive :stable :fiat)
                   (derive :fiat :asset))
        r      (assoc r0 :hierarchies (assoc (:hierarchies r0) :traits h))
        r-no-h (assoc r :hierarchies nil)
        ad-hoc (c/new :XTT 999 2 :iso/fiat :ISO-4217 0)]
    (registry/with r
      (is (set? (c/traits :EUR)))
      (is (contains? (c/traits :EUR) :fiat)))
    (is (set? (c/traits :EUR r)))
    (is (contains? (c/traits :EUR r) :fiat))
    (is (set? (c/traits :EUR Locale/ROOT r)))
    (is (nil? (c/traits :PLN r)))
    (is (nil? (c/traits ad-hoc r)))

    (is (set? (c/traits-expanded :EUR r)))
    (is (contains? (c/traits-expanded :EUR r) :fiat))
    (is (contains? (c/traits-expanded :EUR r) :asset))
    (is (set? (c/traits-expanded :crypto/USDT r)))
    (is (contains? (c/traits-expanded :crypto/USDT r) :stable))
    (is (contains? (c/traits-expanded :crypto/USDT r) :fiat))
    (is (contains? (c/traits-expanded :crypto/USDT r) :asset))
    (registry/with r
      (is (set? (c/traits-expanded :EUR)))
      (is (contains? (c/traits-expanded :EUR) :asset)))
    (is (set? (c/traits-expanded :EUR Locale/ROOT r)))
    (is (contains? (c/traits-expanded :EUR Locale/ROOT r) :asset))
    (is (set? (c/traits-expanded :EUR r-no-h)))
    (is (contains? (c/traits-expanded :EUR r-no-h) :fiat))
    (is (nil? (c/traits-expanded ad-hoc r)))))

(deftest currency-domain-kind-trait-predicates-coverage-extra
  (let [r0      (mk-test-registry)
        r       (c/add-countries r0 :EUR [:PL])
        r-no-h  (assoc r :hierarchies nil)
        eur     (c/unit :EUR r)
        usdt    (c/unit :crypto/USDT r)]
    (testing "possible?/iso-possible? arities and guards"
      (registry/with r
        (is (false? (c/possible? nil)))
        (is (true? (c/possible? :EUR)))
        (is (true? (c/iso-possible? :EUR)))
        (is (false? (c/iso-possible? :crypto/USDT))))
      (is (true? (c/possible? :EUR r)))
      (is (false? (c/possible? nil r)))
      (is (true? (c/iso-possible? :EUR r)))
      (is (false? (c/iso-possible? :crypto/USDT r))))

    (testing "domain and country predicates"
      (is (true? (c/has-country? :EUR r)))
      (registry/with r
        (is (true? (c/has-country? :EUR))))
      (is (true? (c/has-domain? :EUR r)))
      (is (true? (c/has-domain? :EUR :ISO-4217)))
      (is (true? (c/has-domain? :EUR :ISO-4217 r)))
      (is (true? (c/of-domain? :ISO-4217 :EUR r)))
      (is (true? (c/of-domain? :ISO-4217 :EUR r-no-h))))

    (testing "kind predicates"
      (is (true? (c/has-kind? :EUR r)))
      (is (true? (c/has-kind? :EUR :iso/fiat)))
      (is (true? (c/has-kind? :EUR :iso/fiat r)))
      (is (true? (c/of-kind? :iso/fiat :EUR r)))
      (is (true? (c/of-kind? :iso/fiat :EUR r-no-h))))

    (testing "trait predicates"
      (is (true? (c/has-trait? :EUR r)))
      (registry/with r
        (is (true? (c/has-trait? :EUR :fiat))))
      (is (true? (c/has-trait? :EUR :fiat r)))
      (is (true? (c/of-trait? :fiat :EUR r)))
      (is (true? (c/of-trait? :fiat :EUR r-no-h)))
      (is (true? (c/has-trait? usdt :stable r))))))

(deftest currency-localization-formatting-and-config-coverage-extra
  (let [r       (mk-localized-registry)
        eur     (c/new :EUR 978 2 :iso/fiat :ISO-4217 0)
        bogus   (c/new :ZZZ 999 2 :iso/fiat :ISO-4217 0)]
    (testing "localized-properties and localized-property fallbacks"
      (registry/with r
        (is (map? (c/localized-properties :XXX)))
        (is (string? (c/localized-property :symbol :XXX :en_US)))
        (is (string? (c/localized-property :symbol :XXX :en_GB)))
        (is (string? (c/localized-property :symbol :XXX :pl_PL))))
      (is (thrown? clojure.lang.ExceptionInfo (c/localized-properties :NOPE r)))
      (is (thrown? clojure.lang.ExceptionInfo (c/localized-property :symbol :NOPE :en r))))

    (testing "symbol and display-name wrappers"
      (registry/with r
        (is (string? (c/symbol :XXX)))
        (is (string? (c/display-name :XXX)))
        (is (string? (c/symbol :XXX Locale/ROOT)))
        (is (string? (c/display-name :XXX Locale/ROOT))))
      (is (string? (c/symbol :XXX Locale/ROOT r)))
      (is (string? (c/display-name :XXX Locale/ROOT r)))
      (is (string? (c/symbol-native :XXX r)))
      (is (string? (c/display-name-native :XXX r))))

    (testing "formatter-extended branches"
      (let [f1 (c/formatter-extended :XXX Locale/ROOT {:grouping true
                                                       :grouping-size 4
                                                       :negative-prefix "-"
                                                       :negative-suffix ">"
                                                       :positive-prefix "+"
                                                       :positive-suffix "<"
                                                       :always-sep-dec true
                                                       :min-integer-digits 2
                                                       :max-integer-digits 6
                                                       :min-fraction-digits 1
                                                       :max-fraction-digits 3
                                                       :currency-symbol-fn (fn [_ _ _] "¤")}
                                      r)
            f2 (c/formatter-extended :XXX Locale/ROOT {:scale 2} r)]
        (is (instance? java.text.DecimalFormat f1))
        (is (instance? java.text.DecimalFormat f2))))

    (testing "scale/Scalable arities"
      (registry/with r
        (is (true? (scale/scalable? :XXX)))
        (is (true? (scale/applied? :XXX)))
        (is (number? (scale/of :XXX)))
        (is (instance? Currency (scale/apply :XXX 2)))
        (is (instance? Currency (scale/apply :XXX 2 RoundingMode/HALF_UP)))
        (is (nil? (scale/amount :XXX)))
        (is (nil? (scale/amount :XXX 2)))
        (is (nil? (scale/amount :XXX 2 RoundingMode/HALF_UP))))
      (is (true? (scale/scalable? eur)))
      (is (true? (scale/applied? eur)))
      (is (number? (scale/of eur)))
      (is (instance? Currency (scale/apply eur 2)))
      (is (instance? Currency (scale/apply eur 2 RoundingMode/HALF_UP)))

      (let [jc (java.util.Currency/getInstance "EUR")]
        (is (true? (scale/scalable? jc)))
        (is (true? (scale/applied? jc)))
        (is (number? (scale/of jc)))
        (is (instance? Currency (scale/apply jc 2)))
        (is (instance? Currency (scale/apply jc 2 RoundingMode/HALF_UP)))))

    (testing "traits errors with Currency id"
      (let [r0 (registry/new)]
        (is (thrown? clojure.lang.ExceptionInfo (c/set-traits r0 bogus [:x])))
        (is (thrown? clojure.lang.ExceptionInfo (c/add-traits r0 bogus [:x])))
        (is (thrown? clojure.lang.ExceptionInfo (c/remove-traits r0 bogus [:x])))))

    (testing "config->registry and set-default-registry!"
      (with-redefs [config/load            (fn [_] {:hierarchies {:domain (make-hierarchy)}
                                                    :propagate-keys [:x]})
                    config/propagate-keys  (fn [_] [:x])
                    config/currencies      (fn [_] {:EUR {:numeric 978 :scale 2 :kind :iso/fiat :domain :ISO-4217}})
                    config/countries       (fn [_] {:PL :EUR})
                    config/localized       (fn [_] {:EUR {:* {:name "Euro"}}})
                    config/traits          (fn [_] {:EUR #{:fiat}})
                    config/weights         (fn [_] {:EUR 1})
                    config/version         (fn [_] "0.0.1")]
        (is (instance? Registry (c/config->registry "dummy" (registry/new))))
        (is (instance? Registry (c/config->registry "dummy"))))
      (is (instance? Registry (c/config->registry "nope" (registry/new))))
      (with-redefs [c/config->registry (fn [& _] (registry/new))
                    registry/set!     (fn [_] (registry/new))
                    registry/update!  (fn [& _] (registry/new))]
        (is (instance? Registry (c/set-default-registry! "a")))
        (is (nil? (c/set-default-registry! "a" "b" "c")))))

    (testing "global registry helpers"
      (with-temp-global-registry
        r
        (fn []
          (is (instance? Registry (c/add-countries! (c/unit :XXX r) [:US])))
          (is (instance? Registry (c/add-countries! nil [:US])))
          (is (instance? Registry (c/remove-countries! [:US])))
          (is (instance? Registry (c/remove-countries! nil)))
          (is (instance? Registry (c/add-localized-props! (c/unit :XXX r) {:* {:name "X"}})))
          (is (instance? Registry (c/add-localized-props! nil {:* {:name "X"}})))
          (is (instance? Registry (c/remove-localized-props! (c/unit :XXX r))))
          (is (instance? Registry (c/remove-localized-props! nil))))))))
