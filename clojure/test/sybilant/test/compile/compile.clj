;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.compile.compile
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [sybilant.analyzer :refer [*globals*]]
            [sybilant.compile.compile :refer :all]
            [sybilant.test.util :refer [reset-globals]])
  (:import (java.io File Writer)))

(use-fixtures :once (fn redef-exit* [f]
                      (with-redefs [exit* (fn [exit-code] exit-code)]
                        (f))))

(defonce outfile (delay (-> (File/createTempFile "sybilant" ".asm")
                            .getCanonicalPath)))

(use-fixtures :each
  (fn clear-outfile [f]
    (.delete (io/file @outfile))
    (f))
  reset-globals)

(def ^:dynamic *out-writer*)
(def ^:dynamic *err-writer*)

(defmacro with-output
  [out-sym err-sym & body]
  `(let [out-sw# (java.io.StringWriter.)
         err-sw# (java.io.StringWriter.)
         ~out-sym out-sw#
         ~err-sym err-sw#]
     (binding [*out-writer* out-sw#
               *err-writer* err-sw#]
       ~@body)))

(defmacro with-output-capture
  [& body]
  `(binding [*out* *out-writer*
             *err* *err-writer*]
     (try
       ~@body
       (finally
         (.flush *out*)
         (.flush ^java.io.Writer *err*)))))

(def input-file "sybilant/test/exit0.syb")

(defn read-input-file
  []
  (slurp input-file))

(defn split-lines
  [string]
  (map str/trim (str/split-lines (str string))))

(defn slurp-lines
  [file]
  (split-lines (slurp file)))

(def expected-output
  ["bits 64"
   "default rel"
   "section .text"
   "extern exit"
   "global _start"
   "_start:"
   "mov rdi, 0"
   "jmp exit"])

(deftest test-main
  (let [outfile @outfile]
    (with-output out err
      (is (= 0 (with-output-capture (main [input-file "--outfile" outfile]))))
      (is (= expected-output (slurp-lines outfile)))
      (is (empty? (str out)))
      (is (empty? (str err))))
    (testing "should fail with existing outfile"
      (with-output out err
        (is (= 1 (with-output-capture (main [input-file "-o" outfile]))))
        (is (empty? (str out)))
        (is (= (format "%s already exists\n" outfile) (str err)))))
    (reset! *globals* {})
    (doto (io/file outfile)
      .delete
      .createNewFile)
    (testing "should overwrite existing outfile when given force option"
      (with-output out err
        (is (= 0 (with-output-capture (main [input-file "-o" outfile "-f"]))))
        (is (= expected-output (slurp-lines outfile)))
        (is (empty? (str out)))
        (is (empty? (str err)))))))

(deftest test-main-given-no-infile
  (let [outfile @outfile]
    (with-in-str (read-input-file)
      (with-output out err
        (is (= 0 (with-output-capture (main ["-o" outfile]))))
        (is (= "> " (str out)))
        (is (empty? (str err)))
        (is (= expected-output (slurp-lines outfile)))))))

(deftest test-main-given-no-outfile
  (with-output out err
    (is (= 0 (with-output-capture (main [input-file]))))
    (is (= expected-output (split-lines out)))
    (is (empty? (str err)))))

(deftest test-main-given-no-infile-and-no-outfile
  (with-in-str (read-input-file)
    (with-output out err
      (is (= 0 (with-output-capture (main []))))
      (is (= (update-in expected-output [0] #(str "> " %)) (split-lines out)))
      (is (empty? (str err))))))
