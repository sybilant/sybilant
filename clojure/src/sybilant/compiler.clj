;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [sybilant.analyzer :refer [analyze]]
            [sybilant.emitter :refer [emit]]
            [sybilant.parser :refer [defdata? parse-top-level]]
            [sybilant.util :refer [die]])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io Writer)))

(defn reader ^java.io.Reader [in]
  (LineNumberingPushbackReader. (io/reader in :encoding "utf-8")))

(defn read-file [in]
  (take-while (partial not= ::eof)
              (repeatedly #(read in false ::eof))))

(defn read-files [infiles]
  (if (seq infiles)
    (mapcat (fn [infile]
              (let [in (io/file infile)]
                (when-not (.exists in)
                  (die 1 "input file" infile "does not exist"))
                (with-open [in (reader in)]
                  (doall (read-file in)))))
            infiles)
    (read-file *in*)))

(defn data-exp? [exp]
  (defdata? exp))

(defn compile-files [infiles ^Writer out]
  (let [forms (read-files infiles)
        exps (->> forms
                  (map parse-top-level)
                  (map analyze))
        data-exps (filter data-exp? exps)
        code-exps (filter (complement data-exp?) exps)]
    (.write out "bits 64\n")
    (.write out "default rel\n")
    (when (seq data-exps)
      (.write out "\nsection .data\n")
      (doseq [exp data-exps]
        (emit exp out)))
    (when (seq code-exps)
      (.write out "\nsection .text\n")
      (doseq [exp code-exps]
        (emit exp out)))))

(defn compile [infiles outfile force? debug?]
  (if-let [outfile (io/file outfile)]
    (do
      (when (and (.exists outfile) (not force?))
        (die 1 "output file" outfile "exists"))
      (try
        (with-open [out (io/writer outfile :encoding "utf-8")]
          (compile-files infiles out))
        (catch Throwable t
          (when-not debug?
            (.delete outfile))
          (throw t))))
    (compile-files infiles *out*)))
