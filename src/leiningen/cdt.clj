(ns leiningen.cdt
  (:use [robert.hooke :only [add-hook]])
  (:require leiningen.compile
            leiningen.repl))

(defn cdt-debug-port
  [project]
  (or (:cdt-debug-port project) 8021))

(defn repl-client-send-cdt-init-hook
  [port]
  (fn [f reader writer & args]
      (.write writer (str "(do (in-ns 'user) (use 'com.georgejahad.cdt) (cdt-attach 8021))\n"))
      (.flush writer)
      ;; clear out the first prompt and return value of the init code
      (.read reader (make-array Character/TYPE 1000))
      (.read reader (make-array Character/TYPE 1000))
      (Thread/sleep 100)
      (apply f reader writer args)))

(defn cdt
  [project]
  (add-hook #'leiningen.repl/repl-client (repl-client-send-cdt-init-hook (cdt-debug-port project)))
  (leiningen.repl/repl
     (merge project
            {:force-no-debug true})))
