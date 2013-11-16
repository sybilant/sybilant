;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.compile
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [sybilant.compile :refer :all]
            [sybilant.compiler]))

(defmacro with-output-strs [& body]
  `(let [out# (new java.io.StringWriter)
         err# (new java.io.StringWriter)]
     (binding [*out* (new java.io.PrintWriter out#)
               *err* (new java.io.PrintWriter err#)]
       ~@body
       [(str out#) (str err#)])))

(def inpath "clojure/test/sybilant/test/input.syb")
(def infile (io/file inpath))

(def output "COMPILE'D!")
(def outpath "clojure/test/sybilant/test/output.asm")
(def outfile (io/file outpath))

(def exit-code (atom nil))

(use-fixtures :once
  (fn mock-exit [f]
    (with-redefs [exit (fn [x] (flush) (reset! exit-code x))]
      (f)))
  (fn mock-compile [f]
    (with-redefs [sybilant.compiler/compile-file (fn [in out] (io/copy in out))]
      (f)))
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
  (fn reset-exit-code [f]
    (reset! exit-code nil)
    (f))
  (fn clear-outfile [f]
    (if (.exists outfile)
      (.delete outfile))
    (try
      (f)
      (finally
        (if (.exists outfile)
          (.delete outfile))))))

(deftest test-prep-args
  (is (= ["-o" "foo" "--outfile" "bar" "--outfile" "baz" "quux"]
         (prep-args ["-o" "foo" "--outfile=bar" "--outfile" "baz" "quux"]))))

(deftest test-parse-args
  (is (= {:debug? true :force? true :outfile "foo" :infiles ["bar" "baz"]}
         (parse-args ["-d" "-f" "-o" "foo" "bar" "baz"]))))

(deftest test-main-will-default-to-standard-in
  (is (= ["" ""]
         (with-in-str output (with-output-strs (-main "-o" outpath)))))
  (is (= (str "bits 64\ndefault rel\n" output) (slurp outfile)))
  (is (= 0 @exit-code)))

(deftest test-main-will-default-to-standard-out
  (is (= [(str "bits 64\ndefault rel\n" output) ""]
         (with-output-strs (-main inpath))))
  (is (= 0 @exit-code)))

(deftest test-main-will-not-overwrite-existing-output-file
  (with-open [out (io/writer outfile :encoding "utf-8")]
    (.write out "don't overwrite me, bro!"))
  (is (= ["" (str "An error occurred: output file " outpath " exists\n")]
         (with-output-strs (-main "-o" outpath inpath))))
  (is (= "don't overwrite me, bro!" (slurp outfile)))
  (is (= 1 @exit-code)))

(deftest test-main-force-will-overwrite-existing-output-file
  (.createNewFile outfile)
  (is (= ["" ""]
         (with-output-strs (-main "-f" "-o" outpath inpath))))
  (is (= (str "bits 64\ndefault rel\n" output) (slurp outfile)))
  (is (= 0 @exit-code)))

(deftest test-main-cleans-up-output-file-on-exception
  (with-redefs [sybilant.compiler/compile-file (fn [& _]
                                                 (throw (Exception. "ex")))]
    (with-output-strs (-main "-o" outpath inpath)))
  (is (not (.exists outfile)))
  (is (= 1 @exit-code)))

(deftest test-main-debug-flag-leaves-output-file
  (with-redefs [sybilant.compiler/compile-file (fn [& _]
                                                 (throw (Exception. "ex")))]
    (with-output-strs (-main "-d" "-o" outpath inpath)))
  (is (.exists outfile))
  (is (= 1 @exit-code)))

(deftest test-main-will-check-that-infile-exists
  (is (= ["" (str "An error occurred: input file nonexistent does not exist\n")]
         (with-output-strs (-main "-o" outpath "nonexistent"))))
  (is (= 1 @exit-code)))

(deftest test-main-prints-exception-string
  (with-redefs [sybilant.compiler/compile-file (fn [& _]
                                                 (throw (Exception. "ex")))]
    (is (= "An error occurred: ex\n"
           (second (with-output-strs (-main "-o" outpath inpath))))))
  (is (= 1 @exit-code)))

(deftest test-main-debug-flag-prints-stack-trace
  (with-redefs [sybilant.compiler/compile-file (fn [& _]
                                                 (throw (Exception. "ex")))]
    (let [err (second (with-output-strs (-main "-d" "-o" outpath inpath)))]
      (is (.startsWith err (str "An error occurred: java.lang.Exception: ex\n"
                                "\tat sybilant.test.compile"))
          err)))
  (is (= 1 @exit-code)))
