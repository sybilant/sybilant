(defproject sybilant "0.0.1-SNAPSHOT"
  :description "A \"full stack\" Lisp programming language for the x86-64
  architecture."
  :url "http://github.com/sybilant/sybilant/"
  :license {:name "Mozilla Public License, v. 2.0"
            :url "http://mozilla.org/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.8.4"]]
  :source-paths ["clojure/src"]
  :test-paths ["clojure/test"]
  :aot [sybilant.compile]
  :main sybilant.compile
  :aliases {"test!" ["do" "clean," "test"]}
  :profiles {:dev {:dependencies [[pjstadig/humane-test-output "0.6.0"]
                                  [robert/hooke "1.3.0"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]}})
