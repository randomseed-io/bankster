(ns io.randomseed.bankster.util.fs

  ^{:doc    "Support functions and macros, filesystem operations."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:require [clojure.string              :as      str]
            [clojure.java.io             :as       io]
            [clojure.java.classpath      :as       cp]
            [clojure.data.csv            :as      csv]
            [clojure.edn                 :as      edn]
            [io.randomseed.bankster.util :refer  :all])

  (:import [org.apache.commons.io.input BOMInputStream]
           [org.apache.commons.io        ByteOrderMark]))

(def ^String ^private ^const default-encoding
  "Default encoding for input files."
  "UTF-8")

(defn ^Boolean relative-path?
  "Returns true if the given path is relative. False otherwise."
  [^String pathname]
  (try (io/as-relative-path pathname)
       (catch IllegalArgumentException e
         false))
  true)

(def
  ^{:tag Boolean :arglists '([^String pathname])}
  absolute-path?
  "Returns true if the given path is absolute. False otherwise."
  (complement relative-path?))

(defn ^clojure.lang.LazySeq get-java-classpath-folders
  "Lists all directories which exist in Java classpath as a sequence of
  strings. Returns nil if there are none."
  []
  (seq (map str (filter #(.isDirectory (io/file %)) (cp/classpath)))))

(defn prop-pathname
  "For the given Java property name and optional path names creates a path name
  expressed as a string by prefixing them with the directory obtained from a
  property with all parts joined using pathname separator."
  ([prop]       (some-> (System/getProperty (str prop)) str))
  ([prop paths] (some->> paths (remove nil?) seq
                         (apply io/file (System/getProperty (str prop)))
                         str)))

(defn user-dir-pathname
  "For the given pathnames creates a pathname expressed as a string by prefixing them
  with user's directory (typically a project directory) obtained from the Java
  property `user.dir` with all parts joined using current path name separator."
  ([]        (prop-pathname "user.dir"))
  ([& paths] (prop-pathname "user.dir" paths)))


(defn home-dir-pathname
  "For the given pathnames creates a pathname expressed as a string by prefixing them
  with user's home directory obtained from the Java property `user.dir` with all parts
  joined using current path name separator."
  ([]        (prop-pathname "user.home"))
  ([& paths] (prop-pathname "user.home" paths)))

(defn ^String resource-pathname
  "For the given pathnames creates a pathname expressed as a string that resides within
  one of the Java resource directories. The path must exist to be returned. WARNING:
  it will only work for resources on a filesystem, giving you the regular pathname,
  not URI."
  ([]        (some-> (io/resource "") io/file str))
  ([& paths] (some->> paths (remove nil?) seq
                      (apply io/file) str
                      io/resource
                      io/file
                      str)))

(defn ^java.net.URL paths->resource
  "For the given pathnames creates a resource object that resides within one of the
  Java resource directories. The resource must exist for the URI to be returned."
  ([] (when-some [r (io/resource "")] r))
  ([& paths] (some->> paths (remove nil?) seq
                      (apply io/file) str
                      io/resource)))

(defn ^java.net.URL get-resource
  "For the given pathname, returns the resource URL if the path exists. If the path
  does not exist, returns nil."
  {:added "1.2.4"}
  [pname]
  (.getResource
   (.. Thread currentThread getContextClassLoader)
   pname))


(defn ^Boolean integer-string?
  "Returns true if string contains valid integer number."
  [^String s]
  (try
    (boolean (and (some? s) (Integer/parseInt s)))
    (catch NumberFormatException e false)))

(def ^:private bom-utf-ary
  "Array of BOM encodings."
  (into-array [ByteOrderMark/UTF_16LE
               ByteOrderMark/UTF_16BE
               ByteOrderMark/UTF_8
               ByteOrderMark/UTF_32BE
               ByteOrderMark/UTF_32LE]))

(defn read-preferences
  "Reads the given preference file. If the path is relative it will be relative to
  user's home directory."
  [^String filename]
  (when (some? filename)
    (when-some [abs-filename (if (relative-path? filename) (home-dir-pathname filename) filename)]
      (with-open [r (io/reader abs-filename)]
        (edn/read (java.io.PushbackReader. r))))))

(defn rtrim-comments
  [^String s]
  (str/trimr
   (str/join
    (first (take 1 (partition-by #{\#} s))))))

(defn ^clojure.lang.LazySeq read-csv
  "Reads CSV file and returns a lazy sequence of rows."
  [resource]
  (let [stream   (io/input-stream resource)
        bstream  (BOMInputStream. stream true ^longs bom-utf-ary)
        bomenc   (.getBOM bstream)
        encoding (if (some? bomenc) (.getCharsetName bomenc) default-encoding)]
    (with-open [reader (io/reader bstream :encoding encoding)]
      (doall
       (->> reader line-seq
            (map rtrim-comments)
            (remove (comp #{\#} first))
            (remove empty?)
            (str/join "\n")
            csv/read-csv)))))
