(ns

    ^{:doc    "bankster library, init helpers tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.init-test

  (:require [clojure.test               :refer [deftest testing is]]
            [io.randomseed.bankster.init :as init]
            [io.randomseed.bankster.currency :as currency]))

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

