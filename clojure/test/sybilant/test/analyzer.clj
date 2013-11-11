;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.analyzer
  (:refer-clojure :exclude [number? symbol?])
  (:require [clojure.test :refer :all]
            [sybilant.analyzer :refer :all]
            [sybilant.parser :refer :all]))

(use-fixtures :each
  (fn reset-symbol-table [f]
    (binding [*symbol-table* (atom {})]
      (f))))

(deftest test-undefined-symbol-reference
  (is (thrown? Exception (analyze (parse-symbol 'foo))))
  (binding [*symbol-table* (atom {})]
    (analyze (parse-defasm '(defasm foo (%add %rax 1))))
    (is (= (parse-symbol 'foo) (analyze (parse-symbol 'foo)))))
  (binding [*symbol-table* (atom {})]
    (analyze (parse-defextern '(defextern foo)))
    (is (= (parse-symbol 'foo) (analyze (parse-symbol 'foo))))))

(deftest test-check-double-symbol-definition
  (analyze (parse-defasm '(defasm foo (%add %rax 1))))
  (is (thrown? Exception (analyze (parse-defextern '(defextern foo))))))

(deftest test-check-symbol-format
  (analyze (parse-defextern '(defextern foo)))
  (testing "valid format"
    (is (= (parse-symbol 'foo) (analyze (parse-symbol 'foo)))))
  (testing "invalid format"
    (is (thrown? Exception (analyze (parse-symbol 'foo-bar))))))
