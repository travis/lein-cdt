(ns leiningen.cdt-emacs
  "Download CDT dependencies and print instructions for setting up emacs integration"
  (:use [clojure.java.io :only [as-url copy file]]
        [leiningen.cdt :only [default-cdt-debug-port]]
        [leiningen.core :only [home-dir]]
        [leiningen.deps :only [deps]]
        [leiningen.classpath :only [classpath]]))

(defn cdt-home-dir
  []
  (.getAbsolutePath
   (doto (file (home-dir) "cdt")
     .mkdirs)))

(defn cdt-lib-dir
  []
  (.getAbsolutePath
   (doto (file (cdt-home-dir) "lib")
     .mkdirs)))

(defn cdt-src-dir
  []
  (.getAbsolutePath
   (doto (file (cdt-home-dir) "src")
     .mkdirs)))

(defn cdt-config
  [project]
  (merge
   {:remote-el "https://github.com/GeorgeJahad/cdt/raw/master/ide/emacs/cdt.el"
    :local-el (.getAbsolutePath (file (cdt-home-dir) "cdt.el"))
    :cdt-dir (cdt-home-dir)
    :lib-dir (cdt-lib-dir)
    :clj-src "http://cloud.github.com/downloads/clojure/clojure/clojure-1.2.0.zip"
    :contrib-src "http://cloud.github.com/downloads/clojure/clojure-contrib/clojure-contrib-1.2.0.zip"
    :src-dir (cdt-src-dir)}
   (:cdt project)))

(defn download
  [remote local]
  (let [i (.openStream (as-url remote))]
    (copy i (file local))
    (.close i))
  local)

(defn unzip
  [filename out-dir]
  (let [zipfile (java.util.zip.ZipFile. filename)]
    (doseq [entry (enumeration-seq (.entries zipfile))]
      (let [outfile (file out-dir (.getName entry))]
        (if (.isDirectory entry)
          (.mkdirs outfile)
          (copy (.getInputStream zipfile entry)
                outfile))))))

(defn cdt-emacs
  "Download CDT dependencies to a central location and print instructions
for setting up emacs integration.

Users can specify the following config values within the :cdt hash in project.clj:

:remote-el - a URL from which to download cdt.el
:local-el - the local filesystem file to save cdt.el in
:cdt-dir - the central 'cdt directory' to use for downloading cdt.el and libs by default
:lib-dir - the directory to use for downloading dependencies

Example:

:cdt {:remote-el \"https://github.com/GeorgeJahad/cdt/raw/master/ide/emacs/cdt.el\"
      :local-el \"~/.emacs.d/cdt.el\"
      :cdt-dir \"~/.cdt\"
      :lib-dir \"~/.cdt/lib\"
"
  [project]
  (let [c (cdt-config project)
        dummy-project {:root (:root project)
                       :library-path (:lib-dir c)
                       :dependencies [['cdt "1.2.4"]]}]
    (download (:remote-el c) (:local-el c))
    (println "Downloading" (:clj-src c) "to" (:src-dir c))
    (unzip (download (:clj-src c)
                     (java.io.File/createTempFile "clojure" "zip"))
           (:src-dir c))
    (println "Downloading" (:contrib-src c) "to" (:src-dir c))
    (unzip (download (:contrib-src c)
                     (java.io.File/createTempFile "clojure-contrib" "zip"))
           (:src-dir c))
    (deps dummy-project)
    (println (str "
Thanks for using CDT!

I've downloaded the jars you'll need to " (:lib-dir c) ". To use the emacs frontend
add the following elisp to your emacs init files:

"
"(setq cdt-dir \"" (:cdt-dir c) "\")
"
"(setq cdt-source-path (let ((src-root \"" (:src-dir c) "\")) (format \"%s/clojure-1.2.0/src/jvm:%s/clojure-1.2.0/src/clj:%s/clojure-contrib-1.2.0/src/main/clojure:\" src-root src-root src-root)))
"
"(load-file \"" (:local-el c) "\")

"
"(defun cdt-query-cmdline ()
  (let ((path (strip-trail cdt-dir)))
    (format \"java -classpath%s clojure.main --repl\"
            (mapconcat 'identity (directory-files (format \"%s/lib\" cdt-dir) t \".jar$\") \":\"))))

and then reload your init files or restart emacs, and then connect to a debug
JVM in CDT mode using 'M-x cdt'. The default CDT debug port is " default-cdt-debug-port ".

For more information on CDT emacs integration, see:

http://georgejahad.com/clojure/emacs-cdt.html
"))))
