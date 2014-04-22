;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.compile
  (:require [clojure.test :refer :all]
            [sybilant.compile :refer :all]))

(deftest test-main
  (is (= "Hello, Sybilant!\n" (with-out-str (-main))))
  (is (= "Hello, Sybilant! foo\n" (with-out-str (-main "foo")))))
