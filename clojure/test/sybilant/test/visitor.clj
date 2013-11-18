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
  (vary-meta n assoc :visited? true))

(defn visited? [obj]
  (:visited? (meta obj)))

(deftest test-visit-mem
  (let [exp (parse-mem '(%mem8 17))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:disp visited))))
  (let [exp (parse-mem '(%mem8 %rax))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:base visited))))
  (let [exp (parse-mem '(%mem8 %rax 17))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:base visited)))
    (is (visited? (:disp visited))))
  (let [exp (parse-mem '(%mem8 %rbx 4 17))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:index visited)))
    (is (visited? (:scale visited)))
    (is (visited? (:disp visited))))
  (let [exp (parse-mem '(%mem8 %rax %rbx 17))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:base visited)))
    (is (visited? (:index visited)))
    (is (visited? (:disp visited))))
  (let [exp (parse-mem '(%mem8 %rax %rbx 4 17))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:base visited)))
    (is (visited? (:index visited)))
    (is (visited? (:scale visited)))
    (is (visited? (:disp visited)))))

(deftest test-visit-instruction
  (let [exp (parse-instruction '(%add %rax 1))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:operator visited)))
    (doseq [operand (:operands visited)]
      (visited? operand))))

(deftest test-visit-label
  (let [exp (parse-label '(%label foo))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:name visited)))))

(deftest test-visit-defasm
  (let [exp (parse-defasm '(defasm foo (%add %rax 1)))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:name visited)))
    (doseq [statement (:statements visited)]
      (is (visited? statement)))))

(deftest test-visit-defimport
  (let [exp (parse-defimport '(defimport foo))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:name visited)))))

(deftest test-visit-defdata
  (let [exp (parse-defdata '(defdata foo #int8 1 #uint64 2))
        visited (visit exp visitor)]
    (is (= exp visited))
    (is (visited? visited))
    (is (visited? (:name visited)))
    (doseq [value (:values visited)]
      (is (visited? value)))))
