;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.visitor
  (:refer-clojure :exclude [number? symbol?])
  (:require [clojure.test :refer :all]
            [sybilant.parser :refer :all]
            [sybilant.visitor :refer :all]))

(defn visitor [n]
  (assoc n :visited? true))

(deftest test-visit-mem
  (let [visited (visit (parse-mem '(%mem8 %rax %rbx 4 17)) visitor)]
    (is (get-in visited [:visited?]))
    (is (get-in visited [:base :visited?]))
    (is (get-in visited [:index :visited?]))
    (is (get-in visited [:scale :visited?]))
    (is (get-in visited [:disp :visited?]))))

(deftest test-visit-instruction
  (let [visited (visit (parse-instruction '(%add %rax 1)) visitor)]
    (is (get-in visited [:visited?]))
    (is (get-in visited [:operator :visited?]))
    (doseq [operand (:operands visited)]
      (get-in operand [:visited?]))))

(deftest test-visit-defasm
  (let [visited (visit (parse-defasm '(defasm foo (%add %rax 1))) visitor)]
    (is (get-in visited [:visited?]))
    (is (get-in visited [:name :visited?]))
    (doseq [statement (:statements visited)]
      (is (get-in statement [:visited?])))))

(deftest test-visit-defextern
  (let [visited (visit (parse-defextern '(defextern foo)) visitor)]
    (is (get-in visited [:visited?]))
    (is (get-in visited [:name :visited?]))))
