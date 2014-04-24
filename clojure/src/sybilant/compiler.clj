;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compiler
  (:refer-clojure :exclude [compile read])
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :refer [read]]
            [clojure.tools.reader.reader-types :refer
             [indexing-push-back-reader]]
            [sybilant.analyzer :refer [analyze]]
            [sybilant.emitter :refer [emit]]
            [sybilant.optimizer :refer [optimize]]
            [sybilant.parser :refer [parse]])
  (:import (java.io FileInputStream InputStreamReader PushbackReader)))

(defn read-all
  [infile options]
  (with-open [in (-> infile
                     FileInputStream.
                     (InputStreamReader. "UTF-8")
                     PushbackReader.)]
    (let [in (indexing-push-back-reader in 1 infile)]
      (doall (take-while (complement (partial = ::eof))
                         (repeatedly #(read in false ::eof)))))))

(defn compile
  [form options]
  (-> form
      (parse options)
      (analyze options)
      (optimize options)))

(defn compile-all
  [forms options]
  (for [form forms]
    (compile form options)))

(defn emit-all
  [exps options]
  (for [exp exps]
    (emit exp options)))

(defn compile-files
  ([infiles outfile]
     (compile-files infiles outfile {}))
  ([infiles outfile options]
     (let [forms (mapcat #(read-all % options) infiles)
           exps (doall (compile-all forms options))]
       (with-open [out (io/writer outfile :encoding "UTF-8")]
         (doseq [str (emit-all exps options)]
           (.write out str))))))
