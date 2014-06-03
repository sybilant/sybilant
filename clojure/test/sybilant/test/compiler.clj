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

(deftest test-compile-and-emit-all
  (let [forms [(%deftext (%label malloc))
               (%defdata (%label PI))
               (%deftext (%label foo)
                 (%mov %rax (%mem64 1))
                 (%jmp bar)
                 (%label bar)
                 (%add %rax 1))
               (%defdata (%label bar) #sint8 1 #uint8 2)]]
    (is (= "extern malloc
extern PI
global foo
foo:
mov rax, qword [1]
jmp bar
.bar:
add rax, 1
global bar
bar:
db 1
db 2"
           (str/join "\n" (compile-and-emit-all forms {}))))))

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
