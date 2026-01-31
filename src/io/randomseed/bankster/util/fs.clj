(ns

    ^{:doc    "Support functions and macros, filesystem operations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.bankster.util.fs

  (:require [clojure.string              :as      str]
            [clojure.java.io             :as       io]
            [clojure.java.classpath      :as       cp]
            [clojure.data.csv            :as      csv]
            [clojure.edn                 :as      edn]
            [io.randomseed.bankster.util :refer  :all])

  (:import [org.apache.commons.io.input BOMInputStream]
           [org.apache.commons.io        ByteOrderMark]
           [java.nio.file                   Path Paths]))

(def ^String ^:const default-encoding
  "Default encoding for input files."
  "UTF-8")

(def ^"[Ljava.lang.String;"
  empty-str-ary
  (into-array [""]))

(defn ^Boolean absolute-path?
  [pathname]
  (.isAbsolute ^Path (Paths/get ^String (str pathname)
                                ^"[Ljava.lang.String;" empty-str-ary)))

(def ^Boolean relative-path?
  (complement absolute-path?))

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
  with user's home directory obtained from the Java property `user.home` with all parts
  joined using current path name separator."
  ([]        (prop-pathname "user.home"))
  ([& paths] (prop-pathname "user.home" paths)))

(defn ^String resource-pathname
  "For the given pathnames creates a pathname expressed as a string that resides within
  one of the Java resource directories. The path must exist to be returned. WARNING:
  it will only work for resources on a filesystem, giving you the regular pathname,
  not a URI."
  ([]        (some-> (io/resource "") io/file str))
  ([& paths] (some->> paths (remove nil?) seq
                      (apply io/file) str
                      io/resource
                      io/file
                      str)))

(defn paths->resource
  "For the given pathnames creates a resource object that resides within one of the
  Java resource directories. The resource must exist for the URI to be returned."
  {:tag java.net.URL :added "1.0.0"}
  ([] (when-some [r (io/resource "")] r))
  ([& paths] (some->> paths (remove nil?) seq
                      (apply io/file) str
                      io/resource)))

(defn get-resource
  "For the given pathname, returns the resource URL if the path exists. If the path
  does not exist, returns nil."
  {:tag java.net.URL :added "1.2.4"}
  [pname]
  (.getResource
   (.. Thread currentThread getContextClassLoader)
   pname))

(defn ^Boolean integer-string?
  "Returns true if the string contains a valid integer."
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
  user's home directory. Optional options map will be passed to EDN reader."
  ([^String filename]
   (read-preferences filename nil))
  ([^String filename opts]
   (when (some? filename)
     (when-some [abs-filename (if (relative-path? filename) (home-dir-pathname filename) filename)]
       (with-open [r (io/reader abs-filename)]
         (if (nil? opts)
           (edn/read (java.io.PushbackReader. r))
           (edn/read opts (java.io.PushbackReader. r))))))))

(defn rtrim-comments
  [^String s]
  (str/trimr
   (str/join
    (first (take 1 (partition-by #{\#} s))))))

(defn- comment-line?
  {:tag Boolean :private true :added "2.0.0"}
  [^String s]
  (let [s (str/triml (str s))]
    (and (pos? (count s))
         (identical? \# (first s)))))

(defn- quote-last-field
  "If the last CSV field contains a # comment then quote the whole field, so commas in
  the comment do not create extra columns. Keeps the comment (including the #)."
  {:tag String :private true :added "2.0.0"}
  [^String s]
  (let [s (str s)
        h (.indexOf s "#")
        i (if (neg? h)
            (.lastIndexOf s ",")
            (.lastIndexOf (subs s 0 h) ","))]
    (if (neg? i)
      s
      (let [p (subs s 0 (inc i))
            v (subs s (inc i))]
        (if (and (not (str/blank? v))
                 (not (and (str/starts-with? (str/triml v) "\"")
                           (str/ends-with? (str/trimr v) "\"")))
                 (<= 0 (.indexOf v "#")))
          (let [v (str/replace v "\"" "\"\"")]
            (str p "\"" v "\""))
          s)))))

(defn read-csv
  "Reads CSV file and returns a sequence of rows.

  When `comments?` is truthy, lines beginning with # are ignored, but inline comments
  (after #) are preserved and treated as part of the last column."
  (^clojure.lang.LazySeq [resource]
   (read-csv resource false))
	  (^clojure.lang.LazySeq [resource comments?]
	   (let [stream   (io/input-stream resource)
	         ^org.apache.commons.io.input.BOMInputStream$Builder builder (BOMInputStream/builder)
	         builder  (doto builder
	                    (.setInputStream stream)
	                    (.setInclude true)
	                    (.setByteOrderMarks bom-utf-ary))
	         ^BOMInputStream bstream (.get builder)
	         bomenc   (.getBOM bstream)
	         encoding (if (some? bomenc) (.getCharsetName bomenc) default-encoding)]
	     (with-open [reader (io/reader bstream :encoding encoding)]
	       (doall
        (->> reader
             line-seq
             (remove comment-line?)
             (remove (comp str/blank? str))
             (map (if comments? quote-last-field rtrim-comments))
             (remove (comp str/blank? str))
             (str/join "\n")
             csv/read-csv))))))
