(ns

    ^{:doc    "bankster library, util.fs tests."
      :author "Paweł Wilk"
      :added  "2.1.0"
      :no-doc true}

    io.randomseed.bankster.util.fs-test

  (:require [clojure.java.io            :as io]
            [clojure.test               :refer [deftest testing is]]
            [io.randomseed.bankster.util.fs :as fs])

  (:import (java.nio.file Files Path)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^Path []
  (Files/createTempDirectory "bankster-fs-test" (make-array FileAttribute 0)))

(deftest pathnames-and-classpath
  (testing "absolute-path?/relative-path? work on typical paths"
    (is (true? (fs/absolute-path? "/tmp")))
    (is (true? (fs/relative-path? "x/y"))))
  (testing "get-java-classpath-folders returns a seq in a normal dev/test classpath"
    (is (seq (fs/get-java-classpath-folders)))))

(deftest property-based-pathnames
  (testing "prop-pathname arities"
    (is (string? (fs/prop-pathname "user.dir")))
    (is (string? (fs/prop-pathname "user.dir" ["a" nil "b"]))))
  (testing "user-dir-pathname and home-dir-pathname accept extra path segments"
    (is (string? (fs/user-dir-pathname)))
    (is (string? (fs/user-dir-pathname "a" "b")))
    (is (string? (fs/home-dir-pathname)))
    (is (string? (fs/home-dir-pathname "a" "b")))))

(deftest resources-and-resource-pathnames
  (testing "resource-pathname and paths->resource resolve resources on filesystem"
    (is (string? (fs/resource-pathname)))
    (is (some? (fs/paths->resource)))
    (is (string? (fs/resource-pathname "io" "randomseed" "bankster" "config.edn")))
    (is (some? (fs/paths->resource "io" "randomseed" "bankster" "config.edn"))))
  (testing "get-resource returns a URL when pathname exists and nil otherwise"
    (is (some? (fs/get-resource "io/randomseed/bankster/config.edn")))
    (is (nil?  (fs/get-resource "nope/nope/nope.edn")))))

(deftest fs-negative-cases-and-partial-branches
  (testing "prop-pathname returns nil when property is missing or paths are empty"
    (is (nil? (fs/prop-pathname "no.such.property")))
    (is (= "a" (fs/prop-pathname "no.such.property" ["a"])))
    (is (nil? (fs/prop-pathname "user.dir" nil)))
    (is (nil? (fs/prop-pathname "user.dir" [nil nil])))
    (is (nil? (fs/prop-pathname "user.dir" []))))
  (testing "resource-pathname / paths->resource return nil for missing resources"
    (is (nil? (fs/resource-pathname "nope" "nope.edn")))
    (is (nil? (fs/paths->resource "nope" "nope.edn")))
    (is (nil? (fs/resource-pathname nil)))
    (is (nil? (fs/paths->resource nil)))))

(deftest integer-string-parsing
  (testing "integer-string? validates integer-only strings"
    (is (true?  (fs/integer-string? "12")))
    (is (false? (fs/integer-string? "12.0")))
    (is (false? (fs/integer-string? "nope")))
    (is (false? (fs/integer-string? nil)))))

(deftest read-preferences-supports-relative-paths-and-opts
  (testing "read-preferences returns nil when filename is nil"
    (is (nil? (fs/read-preferences nil))))
  (testing "read-preferences supports absolute paths (with and without opts)"
    (let [dir (tmp-dir)
          f   (.resolve dir "prefs.edn")
          p   (.toString f)]
      (spit p "{:a 1}")
      (is (= {:a 1} (fs/read-preferences p)))
      (is (= {:a 1} (fs/read-preferences p {:readers {}})))))
  (testing "read-preferences resolves relative paths against user.home"
    (let [old-home (System/getProperty "user.home")
          dir      (tmp-dir)]
      (try
        (System/setProperty "user.home" (.toString dir))
        (spit (.toString (.resolve dir "prefs.edn")) "{:b 2}")
        (is (= {:b 2} (fs/read-preferences "prefs.edn")))
        (finally
          (System/setProperty "user.home" old-home))))))

(deftest read-preferences-relative-path-with-nil-home-dir-pathname
  (testing "read-preferences returns nil when it cannot resolve an absolute filename (unreachable in normal runtime)"
    (with-redefs [io.randomseed.bankster.util.fs/home-dir-pathname (fn [& _] nil)]
      (is (nil? (fs/read-preferences "prefs.edn"))))))

(deftest resource-pathname-and-paths-to-resource-zero-arity-nil-branches
  (testing "resource-pathname/paths->resource handle nil io/resource result (unreachable in normal classpath)"
    (with-redefs [clojure.java.io/resource (fn [_] nil)]
      (is (nil? (fs/resource-pathname)))
      (is (nil? (fs/paths->resource))))))

(deftest resource-pathname-and-paths-to-resource-varargs-nil-io-file-branches
  (testing "resource-pathname/paths->resource short-circuit when io/file yields nil (unreachable in normal runtime)"
    (with-redefs [clojure.java.io/file (fn [& _] nil)]
      (is (nil? (fs/resource-pathname "io" "randomseed")))
      (is (nil? (fs/paths->resource "io" "randomseed"))))))

(deftest resource-pathname-nil-io-file-on-url-branch
  (testing "resource-pathname short-circuits when io/file returns nil for a URL (unreachable in normal runtime)"
    (let [orig-io-file clojure.java.io/file]
      (with-redefs [clojure.java.io/file (fn
                                           ([x]
                                            (if (instance? java.net.URL x)
                                              nil
                                              (orig-io-file x)))
                                           ([x y]
                                            (orig-io-file x y))
                                           ([x y & more]
                                            (apply orig-io-file x y more)))]
        (is (nil? (fs/resource-pathname "io" "randomseed" "bankster" "config.edn")))))))

(deftest resource-pathname-nil-str-on-second-file-branch
  (testing "resource-pathname short-circuits when str returns nil for a later step (unreachable in normal runtime)"
    (let [orig-str clojure.core/str
          calls    (atom 0)]
      (with-redefs [clojure.core/str (fn [& xs]
                                       (if (instance? java.io.File (first xs))
                                         (let [n (swap! calls inc)]
                                           (if (= 2 n) nil (apply orig-str xs)))
                                         (apply orig-str xs)))]
        (is (nil? (fs/resource-pathname "io" "randomseed" "bankster" "config.edn")))))))

(deftest csv-reading-and-comment-handling
  (testing "read-csv with comments?=false strips trailing # comments"
    (let [dir (tmp-dir)
          f   (.toString (.resolve dir "simple.csv"))]
      (spit f (str "# full line comment\n"
                   "a,b,c # trailing\n"
                   "a,b,hello # foo,bar\n"
                   "\n"))
      (is (= [["a" "b" "c"]
              ["a" "b" "hello"]]
             (fs/read-csv f)))
      (is (= [["a" "b" "c"]
              ["a" "b" "hello"]]
             (fs/read-csv f false)))))
  (testing "read-csv with comments?=true preserves inline comments as part of the last field"
    (let [dir (tmp-dir)
          f   (.toString (.resolve dir "comments.csv"))]
      (spit f (str "# full line comment\n"
                   "a,b,hello # foo,bar\n"
                   "a,b,\"hello # foo,bar\"\n"
                   "onlyfield # foo,bar\n"
                   "a,b,\n"))
      (is (= [["a" "b" "hello # foo,bar"]
              ["a" "b" "hello # foo,bar"]
              ["onlyfield # foo" "bar"]
             ["a" "b" ""]]
             (fs/read-csv f true))))))

(deftest csv-bom-handling
  (testing "read-csv detects BOM and takes the BOM-based encoding branch"
    (let [dir (tmp-dir)
          f   (.toString (.resolve dir "bom.csv"))
          ;; UTF-8 BOM: EF BB BF (as signed bytes: -17 -69 -65)
          bom (byte-array [(byte -17) (byte -69) (byte -65)])
          s   (.getBytes "a,b\n1,2\n" "UTF-8")]
      (with-open [out (io/output-stream f)]
        (.write out bom)
        (.write out s))
      (is (= [["a" "b"]
              ["1" "2"]]
             (fs/read-csv f))))))
