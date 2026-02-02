(ns

    ^{:doc    "bankster library, init helpers tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.init-test

  (:require [clojure.test               :refer [deftest testing is]]
            [io.randomseed.bankster.init :as init]
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

(deftest load-registry-bang-sets-global
  (testing "load-registry! installs registry into global state"
    (let [orig (registry/state)]
      (try
        (init/load-registry! "io/randomseed/bankster/test_config_init_overlay.edn"
                             {:optional? true})
        (is (= true (currency/defined? :init/AAA (registry/state))))
        (finally
          (registry/set! orig))))))
