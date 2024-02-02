;; Copyright © 2024 Paul Stadig
;;
;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
;; of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public
;; License, v. 2.0.
(ns sybilant.compiler-test
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [sybilant.compiler :as compiler]))

(defn- read-lines
  [f]
  (let [comment? #(str/starts-with? % ";")]
    (into []
      (comp
        (map str/trim)
        (remove str/blank?)
        (remove comment?))
      (line-seq (io/reader (io/file f))))))

(deftest t-compile
  (let [lines (read-lines "sybilant/test/exit0.syb.asm")]
    (is (= lines (compiler/compile "sybilant/test/exit0.syb")))))
