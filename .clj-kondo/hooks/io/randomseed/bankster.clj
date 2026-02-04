(ns hooks.io.randomseed.bankster)

(defmacro with-attempt
  [c registry binding & body]
  (let [sym (cond
              (symbol? binding)
              binding

              (and (vector? binding)
                   (= 1 (count binding))
                   (symbol? (nth binding 0)))
              (nth binding 0)

              :else
              (gensym "cur"))]
    `(if-some [~sym (io.randomseed.bankster.currency/attempt* ~c ~registry)]
       (do ~@body)
       false)))
