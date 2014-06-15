;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [slingshot.slingshot :refer [try+]]
            [sybilant.compiler :refer :all]
            [sybilant.test.util :refer [clear-file reset-globals
                                        with-output-strs]]))

(def inpath "clojure/test/sybilant/test/input.syb")
(def infile (io/file inpath))

(def output "bits 64\ndefault rel\nCOMPILE'D!\n")
(def outpath "clojure/test/sybilant/test/output.asm")
(def outfile (io/file outpath))

(defn mocked-compile-files [infiles]
  (if (seq infiles)
    (doall (for [infile infiles
                 :let [in (io/file infile)]
                 line (with-open [in (io/reader in)]
                        (doall (line-seq in)))]
             line))
    (doall (line-seq (io/reader *in*)))))

(use-fixtures :once
  (fn reset-infile [f]
    (if (.exists infile)
      (.delete infile))
    (with-open [in (io/writer infile :encoding "utf-8")]
      (.write in output))
    (try
      (f)
      (finally
        (if (.exists infile)
          (.delete infile))))))

(use-fixtures :each
  reset-globals
  (clear-file outfile))

(deftest test-compile-files
  (is (= ["bits 64"
          "default rel"
          "section .data"
          "global foo"
          "foo:"
          "db 1"
          "section .text"
          "global bar"
          "bar:"
          "jmp bar"]
         (with-in-str "(defdata foo #int8 1) (defasm bar (%jmp bar))"
           (compile-files [])))))

(deftest test-compile-will-default-to-standard-in
  (with-redefs [compile-files mocked-compile-files]
    (with-in-str output
      (compile [] outfile false false)))
  (is (= output (slurp outfile))))

(deftest test-compile-will-default-to-standard-out
  (with-redefs [compile-files mocked-compile-files]
    (is (= output (with-out-str (compile [inpath] nil false false))))))

(deftest test-compile-will-not-overwrite-existing-output-file
  (with-open [out (io/writer outfile :encoding "utf-8")]
    (.write out "don't overwrite me, bro!"))
  (is (thrown? Throwable (compile [inpath] outpath false false)))
  (is (= "don't overwrite me, bro!" (slurp outfile))))

(deftest test-compile-force-will-overwrite-existing-output-file
  (.createNewFile outfile)
  (with-redefs [compile-files mocked-compile-files]
    (compile [inpath] outpath true false))
  (is (= output (slurp outfile))))

(deftest test-compile-cleans-up-output-file-on-exception
  (with-redefs [compile-files (fn [& _] (throw (Exception. "ex")))]
    (is (thrown? Throwable (compile [inpath] outpath false false))))
  (is (not (.exists outfile))))

(deftest test-compile-debug-flag-leaves-output-file
  (with-redefs [read-file (fn [& _] (throw (Exception. "ex")))]
    (is (thrown? Throwable (compile [inpath] outpath false true))))
  (is (.exists outfile)))

(deftest test-compile-will-check-that-infile-exists
  (try+
    (compile ["nonexistent"] outpath false false)
    (is false "should throw exception")
    (catch Object e
      (is (= "input file nonexistent does not exist" (:message e))))))
