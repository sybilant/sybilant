;; Copyright © 2024 Paul Stadig
;;
;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
;; of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public
;; License, v. 2.0.
(ns sybilant.compile-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [sybilant.compile :as compile]))

(defn- read-instructions
  "Read instructions from file while trimming whitespace and ignoring comments and blank lines."
  [file]
  (into []
    (comp
      (map str/trim)
      (remove str/blank?))
    (-> file
      slurp
      (str/replace #"#.*" "")
      (str/split #"\n"))))

(deftest t-compile
  (let [lines (read-instructions "sybilant/test/exit0.syb.asm")]
    (is (= lines (compile/compile-files ["sybilant/test/exit0.syb"])))))
