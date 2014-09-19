;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile
  (:require [clojure.stacktrace :refer [print-stack-trace]]
            [slingshot.slingshot :refer [try+]])
  (:gen-class))

(defn exit [exit-code]
  (System/exit exit-code))

(defn -main [& args]
  (try+
    (apply println "Hello, Sybilant!" args)
    (exit 0)
    (catch Throwable t
      (binding [*out* *err*]
        (print "Unexpected error: ")
        (print-stack-trace t)
        (flush))
      (exit 1))
    (catch :exit-code {:keys [exit-code exit-message]}
      (when exit-message
        (if (pos? exit-code)
          (binding [*out* *err*]
            (println exit-message))
          (println exit-message)))
      (exit exit-code))
    (catch Object o
      (binding [*out* *err*]
        (println "Unexpected error:" (pr-str o)))
      (exit 1))))
