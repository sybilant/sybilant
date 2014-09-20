(defproject sybilant "0.0.1-SNAPSHOT"
  :description "A hybridly typed, functional, full stack, Lisp programming
  language."
  :url "http://github.com/sybilant/sybilant/"
  :license {:name "Mozilla Public License, v. 2.0"
            :url "http://mozilla.org/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.reader "0.8.8"]
                 [slingshot "0.11.0"]]
  :source-paths ["clojure/src"]
  :test-paths ["clojure/test"]
  :main ^:skip-aot sybilant.compile
  :target-path "target/%s"
  :clean-targets ["target"]
  :profiles {:uberjar {:aot :all}}
  :global-vars {*warn-on-reflection* true})
