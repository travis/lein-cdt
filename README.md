# lein-cdt

A leiningen plugin to make it easy to use the Clojure Debugging Toolkit.

## Usage

Includes a hook that will ensure JVMs are started in debug mode. To
use it, add

:hooks [leiningen.hooks.cdt]

to project.clj.

To connect to any running debug JVM (ie, hooked and started via lein
repl, lein swank, etc), run "lein cdt".

"lein cdt" starts a REPL, 'uses' com.georgejahad.cdt and runs
attach-cdt.

"lein cdt-emacs" downloads the Clojure source to a central location
and prints instructions for configuring emacs to work with CDT.

Projects can set :cdt-debug-port to change the JVM debug port (which
is used by CDT).

More info on CDT here:

[http://georgejahad.com/clojure/cdt.html](http://georgejahad.com/clojure/cdt.html)

TODO:
 - upgrading CDT versions is currently tricky - it requires updates in
   project.clj and cdt_emacs.clj
 - cdt-emacs is currently hardcoded to download Clojure 1.2.0 - this
   should be configurable or something
 - cdt-emacs currently hardcoded to a particular cdt version - is
   there a better way?
 - downloading other sources?
 - top-to-bottom tutorial would be nice

## License

Copyright (C) 2010 Travis Vachon

Distributed under the Eclipse Public License, the same as Clojure.
