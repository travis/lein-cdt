(ns leiningen.cdt
  "Start a CDT debug REPL"
  (:use [robert.hooke :only [add-hook]])
  (:require leiningen.compile
            leiningen.repl))

(def default-cdt-debug-port 8021)

(defn cdt-debug-port
  [project]
  (or (:cdt-debug-port project) default-cdt-debug-port))

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
  "Starts a CDT REPL that connects to a running debug JVM. Assumes the debug JVM
is already running.

It is recommended that users add :hooks [leiningen.hooks.cdt] to project.clj
to ensure all lein-launched JVMs will be run in debug mode on the appropriate
port."
  [project]
  (add-hook #'leiningen.repl/repl-client (repl-client-send-cdt-init-hook (cdt-debug-port project)))
  (leiningen.repl/repl
     (merge project
            {:force-no-debug true})))
