;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compiler
  (:refer-clojure :exclude [compile read read-string symbol?])
  (:require [clojure.java.io :as io]
            [clojure.tools.reader.edn :refer [read read-string]]
            [clojure.tools.reader.reader-types :refer
             [indexing-push-back-reader]]
            [sybilant.analyzer :refer [analyze]]
            [sybilant.emitter :refer [emit]]
            [sybilant.optimizer :refer [optimize]]
            [sybilant.parser :refer [parse]]
            [sybilant.types :refer :all]
            [sybilant.utils :refer :all])
  (:import (java.io FileInputStream InputStreamReader PushbackReader Writer)))

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
  (let [data-exps (filter defdata? exps)
        text-exps (filter deftext? exps)]
    (concat
     (when (seq data-exps)
       (cons "section .data"
             (apply concat (for [exp data-exps]
                             (emit exp options)))))
     ["section .text"]
     (apply concat (for [exp text-exps]
                     (emit exp options))))))

(defn compile-and-emit-all
  [forms options]
  (let [exps (doall (compile-all forms options))]
    (emit-all exps options)))

(def ^:const EOF (symbol (str (char 65535))))

(defn read-all
  [in]
  (take-while (complement (partial = EOF))
              (repeatedly #(read {:eof EOF :readers *data-readers*} in))))

(defn read-file
  [^String infile options]
  (with-open [in (FileInputStream. infile)
              in (InputStreamReader. in "UTF-8")
              in (PushbackReader. in)]
    (doall (read-all (indexing-push-back-reader in 1 infile)))))

(defn read-files
  [infiles options]
  (doall (mapcat #(read-file % options) infiles)))

(defn write-to-stream
  [strs ^Writer out]
  (doseq [^String str strs]
    (.write out str)
    (.write out "\n")))

(defn write-to-file
  [strs outfile force?]
  (if (and (.exists (io/file outfile)) (not force?))
    (die 1 "%s already exists" outfile)
    (with-open [out (io/writer outfile :encoding "UTF-8")]
      (write-to-stream strs out))))

(defn compile-files
  [infiles outfile {:keys [force?] :as options}]
  (if (seq infiles)
    (let [strs (-> infiles
                   (read-files options)
                   (compile-and-emit-all options))]
      (if outfile
        (write-to-file strs outfile force?)
        (write-to-stream strs *out*)))
    (die 1 "expected at least one input file")))
