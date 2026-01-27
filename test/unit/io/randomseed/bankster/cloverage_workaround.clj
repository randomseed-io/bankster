(ns io.randomseed.bankster.cloverage-workaround
  (:require [cloverage.coverage :as cov]))

(defonce ^:private installed? (atom false))
(defonce ^:private patched-do-wrap? (atom false))
(defonce ^:private patched-wrap? (atom false))

(defn- unbound-instrumented-ns?
  []
  (instance? clojure.lang.Var$Unbound cov/*instrumented-ns*))

(defn- infer-ns
  [form]
  (or (when (and (seq? form) (= 'ns (first form)) (symbol? (second form)))
        (second form))
      (some-> *ns* ns-name)
      'user))

(defn- ns-form?
  [form]
  (and (seq? form)
       (symbol? (first form))
       (or (= 'ns (first form))
           (= 'clojure.core/ns (first form)))))

(defn- patch-do-wrap!
  []
  (when (compare-and-set! patched-do-wrap? false true)
    (require 'cloverage.instrument)
    (let [orig (get-method cloverage.instrument/do-wrap :atomic)]
      (when orig
        (remove-method cloverage.instrument/do-wrap :atomic)
        (defmethod cloverage.instrument/do-wrap :atomic [f line form env]
          (if (ns-form? form)
            form
            (orig f line form env)))))))

(defn- patch-wrap!
  []
  (when (compare-and-set! patched-wrap? false true)
    (require 'cloverage.instrument)
    (alter-var-root
     #'cloverage.instrument/wrap
     (fn [orig]
       (fn [f-var line-hint form]
         (if (ns-form? form)
           form
           (orig f-var line-hint form)))))))

(defn install!
  "Installs a safe wrapper around `cloverage.coverage/track-coverage` that binds
  `*instrumented-ns*` when it is unbound. Safe to call multiple times."
  []
  (when (compare-and-set! installed? false true)
    (patch-do-wrap!)
    (patch-wrap!)
    (alter-var-root
     #'cov/track-coverage
     (fn [orig]
       (fn [line-hint form]
         (if (unbound-instrumented-ns?)
           (binding [cov/*instrumented-ns* (infer-ns form)]
             (orig line-hint form))
           (orig line-hint form))))))
  :installed)
