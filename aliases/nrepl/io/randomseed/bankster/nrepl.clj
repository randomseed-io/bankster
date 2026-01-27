(ns ^{:clojure.tools.namespace.repl/load false} io.randomseed.bankster.nrepl
  (:require
   [clojure.java.io :as io]
   [clojure.string  :as str]
   [nrepl.server    :as nrepl.server :refer [start-server stop-server]]
   [cider.nrepl]))

(def ^:private default-port 5610)
(def ^:private default-port-file ".nrepl-port")

(defn- parse-long*
  ^Long [x]
  (try
    (cond
      (nil? x) nil
      (int? x) (long x)
      (number? x) (long x)
      :else (Long/parseLong (str/trim (str x))))
    (catch Exception _ nil)))

(defn- configured-port
  ^Long []
  (or (parse-long* (System/getProperty "bankster.nrepl.port"))
      (parse-long* (System/getProperty "nrepl.port"))
      (parse-long* (System/getenv "BANKSTER_NREPL_PORT"))
      (parse-long* (System/getenv "NREPL_PORT"))
      default-port))

(defn- configured-port-file
  ^String []
  (or (not-empty (System/getProperty "bankster.nrepl.port-file"))
      (not-empty (System/getProperty "nrepl.port-file"))
      default-port-file))

(defn- middleware-var
  "Best-effort middleware resolver.

  Returns a middleware var or nil when it can't be resolved (missing dep etc.)."
  [ns-sym var-sym]
  (try
    (require ns-sym)
    (ns-resolve ns-sym var-sym)
    (catch Throwable _ nil)))

(defn nrepl-handler
  "Returns the nREPL handler with CIDER middleware and optional extras."
  []
  (let [kaocha-mw     (middleware-var 'kaocha-nrepl.middleware   'wrap-kaocha)
        refactor-mw   (middleware-var 'refactor-nrepl.middleware 'wrap-refactor)
        piggieback-mw (middleware-var 'cider.piggieback          'wrap-cljs-repl)
        mw-symbol     (fn [mw]
                        (cond
                          (symbol? mw) mw
                          (var? mw)    (symbol (-> mw meta :ns ns-name)
                                               (-> mw meta :name))
                          (instance? clojure.lang.Named mw)
                          (symbol (namespace mw) (name mw))
                          :else nil))
        base-mw       (if piggieback-mw
                        cider.nrepl/cider-middleware
                        (remove #(= 'cider.nrepl/wrap-complete (mw-symbol %))
                                cider.nrepl/cider-middleware))]
    (apply nrepl.server/default-handler
           (cond-> (mapv #'cider.nrepl/resolve-or-fail base-mw)
             piggieback-mw (conj piggieback-mw)
             kaocha-mw     (conj kaocha-mw)
             refactor-mw   (conj refactor-mw)))))

(defonce ^:private !server (atom nil))

(defn start!
  "Starts an nREPL server (if not started yet) and writes `.nrepl-port` to CWD.

  Options:
  - `:port`      integer port (defaults to env/sysprops/5610)
  - `:port-file` path to write port file (defaults to `.nrepl-port`)"
  ([] (start! {}))
  ([{:keys [port port-file]
     :or   {port      (configured-port)
            port-file (configured-port-file)}
     :as   _opts}]
   (or @!server
       (let [server (start-server :port (long port) :handler (nrepl-handler))
             pf     (io/file port-file)]
         (spit pf (str (:port server)))
         (println (str "[bankster] nREPL client can be connected to port " (:port server)
                       " (" (.getPath pf) ")"))
         (reset! !server server)))))

(defn stop!
  "Stops the nREPL server (if running) and tries to delete `.nrepl-port`."
  []
  (when-let [server @!server]
    (stop-server server)
    (reset! !server nil))
  (let [pf (io/file (configured-port-file))]
    (when (.exists pf)
      (try
        (.delete pf)
        (catch Throwable _ nil)))))

(println "[bankster] Starting nREPL server")

(start! {})
