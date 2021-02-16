(ns ^{:clojure.tools.namespace.repl/load false} nrepl
  (:require
   [nrepl.server :refer [start-server stop-server]]
   [cider.nrepl]
   ;;[refactor-nrepl.middleware :as refactor.nrepl]
   [io.aviso.ansi]
   [puget.printer :refer [cprint]]))

(defn start-nrepl-old
  [opts]
  (let [server
        (start-server
         :port (:port opts)
         :handler
         (apply nrepl.server/default-handler
                (conj (map #'cider.nrepl/resolve-or-fail cider.nrepl/cider-middleware)
                      ;; #'refactor.nrepl/wrap-refactor
                      )))]
    (spit ".nrepl-port" (:port server))
    (println (io.aviso.ansi/yellow (str "[bankster] nREPL client can be connected to port " (:port server))))
    server))

(defn nrepl-handler []
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defn start-nrepl
  [opts]
  (let [server (start-server :port (:port opts) :handler (nrepl-handler))]
    (println (io.aviso.ansi/yellow (str "[bankster] nREPL client can be connected to port " (:port server))))
    server))

(println "[bankster] Starting nREPL server")

(def server (start-nrepl {:port 5610}))
