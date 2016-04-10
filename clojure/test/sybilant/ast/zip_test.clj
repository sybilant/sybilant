;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.ast.zip-test
  (:require
   [clojure.test :refer [are deftest use-fixtures]]
   [schema.test :refer [validate-schemas]]
   [sybilant.ast.zip :refer :all]
   [sybilant.parser :as parser]))

(use-fixtures :once validate-schemas)

(deftest t-dfs-visit
  (are [exp]
      (= exp (dfs-visit exp identity))
    (parser/parse-defimport
     '(%defimport (%label foo)))
    (parser/parse-defconst
     '(%defconst (%label foo) 5))
    (parser/parse-defdata
     '(%defdata (%label foo [(%sint8 0 1)]) [1]))
    (parser/parse-deftext
     '(%deftext (%label foo)
                (%add %rax 1)
                (%sub %rax (%addr %rbx))))))
