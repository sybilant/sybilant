;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.util
  (:require [sybilant.analyzer :refer [*globals*]]))

(defn clear-file [file]
  (fn [f]
    (if (.exists file)
      (.delete file))
    (try
      (f)
      (finally
        (if (.exists file)
          (.delete file))))))

(defn reset-globals [f]
  (binding [*globals* (atom {})]
    (f)))

(defmacro with-output-strs [& body]
  `(let [out# (new java.io.StringWriter)
         err# (new java.io.StringWriter)]
     (binding [*out* (new java.io.PrintWriter out#)
               *err* (new java.io.PrintWriter err#)]
       ~@body
       [(str out#) (str err#)])))

(defmacro with-empty-env [& body]
  `(binding [*globals* (atom {})]
     ~@body))
