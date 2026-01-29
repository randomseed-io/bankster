(ns

    ^{:doc    "bankster library, filesystem helpers tests."
      :author "Paweł Wilk"
      :added  "2.0.0"
      :no-doc true}

    io.randomseed.bankster.util.fs-test

  (:require [clojure.test               :refer [deftest testing is]]
            [io.randomseed.bankster.util.fs :as fs]))

(deftest read-csv-comments
  (testing "inline # comments are preserved as part of the last column when comments? is truthy"
    (let [^java.io.File f (java.io.File/createTempFile "bankster-csv-" ".csv")]
      (try
        (spit f (str "#Code,Numeric,DecPlaces\n"
                     "ADP,20,0 # Old, now EUR\n"
                     "AED,784,2\n"))
        (is (= [["ADP" "20" "0 # Old, now EUR"]
                ["AED" "784" "2"]]
               (mapv vec (fs/read-csv (.getPath f) true))))
        (is (= [["ADP" "20" "0"]
                ["AED" "784" "2"]]
               (mapv vec (fs/read-csv (.getPath f)))))
        (finally
          (.delete f))))))

