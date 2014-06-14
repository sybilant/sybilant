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
            [sybilant.compile :refer :all]
            [sybilant.compiler :refer [compile-files]]
            [sybilant.test.util :refer [clear-file with-output-strs]]))

(def inpath "clojure/test/sybilant/test/input.syb")

(def outpath "clojure/test/sybilant/test/output.asm")
(def outfile (io/file outpath))

(def exit-code (atom nil))

(use-fixtures :once
  (fn mock-exit [f]
    (with-redefs [exit (fn [x] (flush) (reset! exit-code x))]
      (f))))

(use-fixtures :each
  (fn reset-exit-code [f]
    (reset! exit-code nil)
    (f))
  (clear-file outfile))

(deftest test-prep-args
  (is (= ["-o" "foo" "--outfile" "bar" "--outfile" "baz" "quux"]
         (prep-args ["-o" "foo" "--outfile=bar" "--outfile" "baz" "quux"]))))

(deftest test-parse-args
  (is (= {:debug? true :force? true :outfile "foo" :infiles ["bar" "baz"]}
         (parse-args ["-d" "-f" "-o" "foo" "bar" "baz"]))))

(deftest test-main-prints-exception-string
  (with-redefs [compile-files (fn [& _] (throw (Exception. "ex")))]
    (is (= "An error occurred: ex\n"
           (second (with-output-strs (-main "-o" outpath inpath))))))
  (is (= 1 @exit-code)))

(deftest test-main-debug-flag-prints-stack-trace
  (with-redefs [compile-files (fn [& _] (throw (Exception. "ex")))]
    (let [err (second (with-output-strs (-main "-d" "-o" outpath inpath)))]
      (is (.startsWith err (str "An error occurred: java.lang.Exception: ex\n"
                                "\tat sybilant.test.compile"))
          err)))
  (is (= 1 @exit-code)))
