(defproject sybilant "0.0.1-SNAPSHOT"
  :description "A \"full stack\" Lisp programming language for the x86-64
  architecture."
  :url "http://github.com/sybilant/sybilant/"
  :license {:name "Mozilla Public License, v. 2.0"
            :url "http://mozilla.org/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :source-paths ["clojure/src"]
  :test-paths ["clojure/test"]
  :aot [sybilant.compile]
  :profiles {:dev {:dependencies [[pjstadig/humane-test-output "0.2.0"]]}})
