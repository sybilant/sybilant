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
   [clojure.test :refer [deftest is testing]]
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
  (doseq [file ["exit0" "syscall"]
          :let [syb-file (format "sybilant/test/%s.syb" file)
                asm-file (format "sybilant/test/%s.syb.asm" file)]]
    (testing syb-file
      (let [lines (read-instructions asm-file)]
        (is (= lines (compile/compile-files [syb-file])))))))
