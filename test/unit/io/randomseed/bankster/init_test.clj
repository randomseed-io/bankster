(ns

    ^{:doc    "bankster library, init helpers tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.init-test

  (:require [clojure.test                  :refer [deftest testing is]]
            [io.randomseed.bankster.config :as config]
            [io.randomseed.bankster.init   :as init]
            [io.randomseed.bankster.currency :as currency]
            [io.randomseed.bankster.registry :as registry])

  (:import (io.randomseed.bankster Registry)))

(deftest load-registry-defaults-to-user-only
  (testing "load-registry defaults to user-only config (no dist overlay)"
    (let [r (init/load-registry "io/randomseed/bankster/test_config_init_overlay.edn")]
      (is (= true  (currency/defined? :init/AAA r)))
      (is (= false (currency/defined? :EUR r))))))

(deftest load-registry-keep-dist-overlay
  (testing "load-registry with keep-dist? overlays user config over dist config"
    (let [r (init/load-registry "io/randomseed/bankster/test_config_init_overlay.edn"
                                {:keep-dist? true})]
      (is (= true (currency/defined? :init/AAA r)))
      (is (= true (currency/defined? :EUR r))))))

(deftest load-registry-merge-opts-and-dist-path
  (testing "load-registry honors :merge-opts and :dist-resource-path"
    (let [r (init/load-registry "io/randomseed/bankster/test_config_init_overlay.edn"
                                {:keep-dist? true
                                 :dist-resource-path config/default-resource-path
                                 :merge-opts {:preserve-fields [:domain :kind]
                                              :iso-like? true}})]
      (is (= true (currency/defined? :init/AAA r)))
      (is (= true (currency/defined? :EUR r))))))

(deftest load-registry-missing-and-optional
  (testing "missing resource with optional? true returns empty registry when no dist"
    (let [r (init/load-registry "io/randomseed/bankster/does_not_exist.edn"
                                {:optional? true})]
      (is (instance? Registry r))
      (is (= false (currency/defined? :EUR r)))))
  (testing "missing resource with keep-dist? true returns dist registry"
    (let [r (init/load-registry "io/randomseed/bankster/does_not_exist.edn"
                                {:keep-dist? true
                                 :optional?  true})]
      (is (instance? Registry r))
      (is (= true (currency/defined? :EUR r))))))

(deftest ensure-resource-errors
  (testing "ensure-resource! throws when resource is missing and optional? is false"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Registry config resource not found"
         (#'init/ensure-resource! "io/randomseed/bankster/does_not_exist.edn" false)))))

(deftest load-registry-uses-user-registry-when-resource-exists
  (testing "load-registry uses user registry when resource is resolvable"
    (let [reg (registry/new-registry)
          url (java.net.URL. "file:/tmp/user-reg.edn")]
      (with-redefs [io.randomseed.bankster.util.fs/paths->resource (fn [_] url)
                    io.randomseed.bankster.currency/config->registry (fn [_ _] reg)]
        (is (= reg (init/load-registry "user.edn")))))))

(deftest load-registry-bang-sets-global
  (testing "load-registry! installs registry into global state"
    (let [orig (registry/state)]
      (try
        (init/load-registry! "io/randomseed/bankster/test_config_init_overlay.edn"
                             {:optional? true})
        (is (= true (currency/defined? :init/AAA (registry/state))))
        (finally
          (registry/set! orig))))))

(deftest load-registry-bang-default-arity
  (testing "load-registry! 1-arity delegates to 2-arity"
    (let [orig (registry/state)]
      (try
        (init/load-registry! "io/randomseed/bankster/test_config_init_overlay.edn")
        (is (= true (currency/defined? :init/AAA (registry/state))))
        (finally
          (registry/set! orig))))))
