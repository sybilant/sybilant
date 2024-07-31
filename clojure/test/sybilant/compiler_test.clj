;; Copyright © 2024 Paul Stadig
;;
;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
;; of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public
;; License, v. 2.0.
(ns sybilant.compiler-test
  (:require
   [clojure.test :refer [deftest is]]
   [sybilant.compiler :as compiler]))

(deftest t-compile-forms-throws-unknown-symbol
  (try
    (compiler/compile-forms '[(sybilant.x86-64/deftext (label foo) (mov bar eax))])
    (is false "should throw exception")
    (catch Exception e
      (is (= "Unknown symbol" (.getMessage e)))
      (is (= {:symbol 'bar} (ex-data e))))))
