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
  (:require [clojure.test :refer :all]
            [sybilant.compiler :refer :all]))

(defn compile-and-emit-all
  [forms]
  (-> forms
      (compile-all {})
      (emit-all {})))

(defmacro defasm [& body]
  `(list '~'defasm
         ~@(for [exp body]
             `'~exp)))

(deftest test-defasm-macro
  (is (= '(defasm foo
            (%add %rax 1)
            (%sub %rax 1))
         (defasm foo
           (%add %rax 1)
           (%sub %rax 1)))))

(deftest test-compile-and-emit-all
  (let [forms [(defasm foo
                 (%mov %rax (%mem64 1))
                 (%jmp bar)
                 (%label bar)
                 (%add %rax 1))]]
    (is (= (map pr-str forms)
           (compile-and-emit-all forms)))))
