(ns io.randomseed.bankster.deploy.main

  (:require [deps-deploy.deps-deploy :as dd]))

(defn deploy
  "Usage:
    clj -T`:deploy` deploy `:artifact` '\"target/bankster-1.0.1.jar\"'
  Optional:
    `:sign?` true|false
    `:sign-key-id` '\"<fingerprint>\"'
  Env:
    CLOJARS_USERNAME, CLOJARS_PASSWORD (already in bin/deploy)"
  [{:keys [artifact sign? sign-key-id installer pom-file]
    :or   {sign?     true
           installer :remote}}]
  (when-not artifact
    (throw (ex-info "Missing :artifact (path to jar)." {})))
  (dd/deploy (cond-> {:installer      installer
                      :artifact       artifact
                      :pom-file       pom-file
                      :sign-releases? (boolean sign?)}
               sign-key-id (assoc :sign-key-id sign-key-id))))
