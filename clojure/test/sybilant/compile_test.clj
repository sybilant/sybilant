;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile-test
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [sybilant.compile :refer :all]
            [sybilant.utils :refer [file-exists? uuid-str with-err-str
                                    with-tmp-paths]]
            [sybilant.test-utils :refer [test-path]]))

(defn redef-exit [f]
  (with-redefs [exit (fn [exit-code] exit-code)]
    (f)))

(use-fixtures :once redef-exit)

(def infile (test-path "foo.syb"))

(deftest test-main
  (is (seq
       (with-out-str
         (with-in-str
           (slurp infile)
           (is (zero? (-main))))))))

(deftest test-main-given-infile
  (is (seq
       (with-out-str
         (is (zero? (-main infile)))))))

(deftest test-main-given-outfile
  (with-tmp-paths [outfile]
    (with-in-str
      (slurp infile)
      (is (zero? (-main "-o" (str outfile)))))
    (when (is (file-exists? outfile))
      (is (seq (slurp (str outfile)))))))

(defmacro with-existing-file [[file content] & exprs]
  `(with-tmp-paths [~file]
     (let [~content (uuid-str)]
       (spit ~file ~content)
       ~@exprs)))

(deftest test-main-given-infile-and-outfile
  (with-tmp-paths [outfile]
    (is (zero? (-main "-o" outfile infile)))
    (when (is (file-exists? outfile))
      (is (seq (slurp outfile)))))
  (testing "should not overwrite existing file"
    (with-existing-file [outfile content]
      (is (seq
           (with-err-str
             (is (pos? (-main "-o" outfile infile))))))
      (when (is (file-exists? outfile))
        (is (= content (slurp outfile))))))
  (testing "should overwrite existing file with force"
    (with-existing-file [outfile content]
      (is (zero? (-main "-o" outfile "--force" infile)))
      (when (is (file-exists? outfile))
        (let [outfile-content (slurp outfile)]
          (is (seq outfile-content))
          (is (not= content (read-string outfile-content))))))))
