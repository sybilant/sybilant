;;;; Copyright © 2014 Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile-test
  (:require [clojure.test :refer :all]
            [sybilant.compile :refer :all]))

(defn redef-exit [f]
  (with-redefs [exit (fn [exit-code] exit-code)]
    (f)))

(use-fixtures :once redef-exit)

(deftest test-main
  (is (= "Hello, Sybilant!\n" (with-out-str (-main))))
  (is (= "Hello, Sybilant! foo\n" (with-out-str (-main "foo")))))
