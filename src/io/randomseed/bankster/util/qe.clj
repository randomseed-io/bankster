;; SPDX-License-Identifier: LGPL-3.0-or-later OR Apache-2.0

(ns

    ^{:doc    "Quick equality macro."
      :author "Paweł Wilk"
      :added  "2.2.4"}

    io.randomseed.bankster.util.qe)

(defmacro q=
  "Compares 2 forms by calling `clojure.core/identical?`
  and if that returns falsy value tries `clojure.core/=`."
  {:added "2.2.4"}
  [a b]
  (cond
    (and (keyword? a) (keyword? b))         (= a b)
    (and (string?  a) (string?  b) (= a b)) true
    (and (number?  a) (number?  b) (= a b)) true
    (and (nil? a) (nil? b))                 true

    :else
    `(let [a# ~a b# ~b]
       (or (clojure.core/identical? a# b#)
           (clojure.core/= a# b#)))))
