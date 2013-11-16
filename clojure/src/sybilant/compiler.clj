;;;; Copyright © 2013 Paul Stadig. All rights reserved.
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
            [sybilant.parser :refer [parse-top-level]]
            [sybilant.util :refer [die]])
  (:import (clojure.lang LineNumberingPushbackReader)))

(defn compile-file [in out]
  (doseq [exp (take-while (partial not= ::eof)
                          (repeatedly #(read in false ::eof)))]
    (emit (analyze (parse-top-level exp)) out)))

(defn compile-files [infiles out]
  (.write out "bits 64\n")
  (.write out "default rel\n")
  (if (seq infiles)
    (doseq [infile infiles
            :let [in (io/file infile)]]
      (when-not (.exists in)
        (die 1 "input file" infile "does not exist"))
      (with-open [in (-> in
                         (io/reader :encoding "utf-8")
                         LineNumberingPushbackReader.)]
        (compile-file in out)))
    (compile-file *in* out)))

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
