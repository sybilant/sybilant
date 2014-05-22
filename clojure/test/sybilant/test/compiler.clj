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
            [sybilant.compiler :refer :all]
            [sybilant.parser :refer [parse-top-level]]))

(defn compile-and-emit-all
  [forms]
  (-> forms
      (compile-all {})
      (emit-all {})))

(defmacro %deftext [& body]
  `(list '~'%deftext
         ~@(for [exp body]
             `'~exp)))

(deftest test-deftext-macro
  (is (= '(%deftext (%label foo)
            (%mov %rax (%mem64 1))
            (%jmp bar)
            (%label bar)
            (%add %rax 1))
         (%deftext (%label foo)
           (%mov %rax (%mem64 1))
           (%jmp bar)
           (%label bar)
           (%add %rax 1)))))

(defmacro %defdata [name & values]
  `(list '~'%defdata '~name ~@(for [value values] `'~value)))

(deftest test-defdata-macro
  (is (= '(%defdata (%label foo) #sint8 1 #sint8 2)
         (%defdata (%label foo) #sint8 1 #sint8 2))))

(deftest test-compile-and-emit-all
  (let [forms [(%deftext (%label malloc))
               (%defdata (%label PI))
               (%deftext (%label foo)
                 (%mov %rax (%mem64 1))
                 (%jmp bar)
                 (%label bar)
                 (%add %rax 1))
               (%defdata (%label foo) #sint8 1 #sint8 2)]]
    (is (= (map (comp pr-str parse-top-level) forms)
           (compile-and-emit-all forms)))))
