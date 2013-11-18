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
            [sybilant.parser :refer :all]
            [sybilant.test.util :refer [reset-globals]]))

(use-fixtures :each reset-globals)

(deftest test-undefined-symbol-reference
  (is (thrown? Exception (analyze (parse-defasm '(defasm foo (%jmp bar))))))
  (binding [*globals* (atom {})]
    (analyze (parse-defasm '(defasm bar (%add %rax 1))))
    (is (= (parse-defasm '(defasm foo (%jmp bar)))
           (analyze (parse-defasm '(defasm foo (%jmp bar)))))))
  (binding [*globals* (atom {})]
    (analyze (parse-defimport '(defimport bar)))
    (is (= (parse-defasm '(defasm foo (%jmp bar)))
           (analyze (parse-defasm '(defasm foo (%jmp bar))))))))

(deftest test-check-double-symbol-definition
  (analyze (parse-defasm '(defasm foo (%add %rax 1))))
  (is (thrown? Exception (analyze (parse-defimport '(defimport foo))))))

(deftest test-check-symbol-format
  (is (thrown? Exception (analyze (parse-defimport '(defimport foo-bar)))))
  (is (thrown? Exception (analyze (parse-defasm '(defasm ^:export foo-bar
                                                   (%add %rax 1))))))
  (binding [*globals* (atom {})]
    (analyze (parse-defasm '(defasm foo-bar (%add %rax 1)))))
  (is (thrown? Exception (analyze (parse-defdata
                                   '(defdata ^:export foo-bar 1)))))
  (binding [*globals* (atom {})]
    (analyze (parse-defdata '(defdata foo-bar #int8 1)))))

(deftest test-check-labels
  (binding [*globals* (atom {})]
    (analyze (parse-defasm '(defasm foo
                              (%jmp bar-baz)
                              (%label bar-baz)
                              (%add %rbx 1)))))
  (binding [*globals* (atom {})]
    (is (thrown? Exception (analyze (parse-defasm '(defasm foo
                                                     (%add %rax 1)
                                                     (%label foo)
                                                     (%add %rbx 1)))))))
  (binding [*globals* (atom {})]
    (is (thrown? Exception (analyze (parse-defasm '(defasm foo
                                                     (%add %rax 1)
                                                     (%label bar)
                                                     (%add %rbx 1)
                                                     (%label bar)
                                                     (%add %rcx 1))))))))
