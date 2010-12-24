(ns leiningen.hooks.cdt
  (:use [leiningen.cdt :only [cdt-debug-port]]
        [robert.hooke :only [add-hook]]))

(defn add-debug-options-hook [f project & args]
  (apply f (if (:force-no-debug project)
             project
             (assoc project :jvm-opts
                    (conj (:jvm-opts project)
                          (str "-agentlib:jdwp=transport=dt_socket,address=" (cdt-debug-port project) ",server=y,suspend=n"))))
         args))

(add-hook #'leiningen.compile/eval-in-project add-debug-options-hook)


