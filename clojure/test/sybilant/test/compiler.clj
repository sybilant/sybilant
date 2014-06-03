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
            [clojure.string :as str]
            [clojure.test :refer :all]
            [sybilant.compiler :refer :all]
            [sybilant.test.utils :refer :all]))

(use-fixtures :each reset-global-env-fixture)

(defn sybilant-test-files
  []
  (sort (for [f (file-seq (io/file "sybilant/test"))
              :let [file-name (.getName f)]
              :when (.endsWith file-name ".syb")]
          file-name)))

(deftest test-sybilant-test-files
  (doseq [file-name (sybilant-test-files)]
    (reset-global-env)
    (is (assembles? file-name) (str "Failed to compile " file-name))))
