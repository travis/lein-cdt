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

Projects can set :cdt-debug-port to change the JVM debug port (which
is used by CDT).

More info on CDT here:

[http://georgejahad.com/clojure/cdt.html](http://georgejahad.com/clojure/cdt.html)

## License

Copyright (C) 2010 Travis Vachon

Distributed under the Eclipse Public License, the same as Clojure.
