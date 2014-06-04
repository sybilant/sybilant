;;;; Copyright © Paul Stadig. All rights reserved.
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
            [robert.hooke :refer [prepend with-scope]]
            [sybilant.compile :refer :all]
            [sybilant.test.utils :refer :all])
  (:import (java.io ByteArrayOutputStream File PrintStream PrintWriter)))

(defn main-with-fresh-env
  [f]
  (with-scope
    (prepend -main (reset-global-env))
    (f)))

(defn redef-exit
  [f]
  (with-redefs [exit (fn [exit-code] exit-code)]
    (f)))

(use-fixtures :each
  main-with-fresh-env
  redef-exit)

(def expected-output (slurp-lines "sybilant/test/exit0.syb.asm"))

(defmacro with-output
  [result out err exp & body]
  `(let [old-out# System/out
         old-err# System/err]
     (try
       (let [out-str# (ByteArrayOutputStream.)
             err-str# (ByteArrayOutputStream.)]
         (System/setOut (PrintStream. out-str#))
         (System/setErr (PrintStream. err-str#))
         (let [~result (binding [*out* (PrintWriter. out-str#)
                                 *err* (PrintWriter. err-str#)]
                         (let [r# ~exp]
                           (.flush *out*)
                           (.flush *err*)
                           (.flush System/out)
                           (.flush System/err)
                           r#))
               ~out (str out-str#)
               ~err (str err-str#)]
           ~@body))
       (finally
         (System/setOut old-out#)
         (System/setErr old-err#)))))

(defonce outfile (.getCanonicalPath (File/createTempFile "sybilant" ".asm")))

(deftest test-main
  (testing "given no infiles"
    (with-output result out err
      (-main)
      (is (= 1 result))
      (is (= "An error occurred: expected at least one input file\n" err))))
  (testing "given infiles"
    (with-output result out err
      (-main "sybilant/test/exit0.syb")
      (is (= 0 result))
      (is (= expected-output (split-lines out)))))
  (testing "given infiles and outfile"
    (.delete (io/file outfile))
    (with-output result out err
      (-main "sybilant/test/exit0.syb" "-o" outfile)
      (is (= 0 result))
      (is (= expected-output (slurp-lines outfile)))))
  (testing "given infiles, outfile, and force"
    (with-output result out err
      (-main "sybilant/test/exit0.syb" "-o" outfile)
      (is (= 1 result))
      (is (= (str "An error occurred: " outfile " already exists\n") err)))
    (with-output result out err
      (-main "sybilant/test/exit0.syb" "-o" outfile "-f")
      (is (= 0 result))
      (is (= expected-output (slurp-lines outfile))))))
