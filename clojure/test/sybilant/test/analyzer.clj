;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.analyzer
  (:refer-clojure :exclude [number? string? symbol?])
  (:require [clojure.test :refer :all]
            [sybilant.analyzer :refer :all]
            [sybilant.parser :refer :all]
            [sybilant.test.util :refer [reset-globals with-empty-env]]))

(use-fixtures :each reset-globals)

(deftest test-undefined-symbol-reference
  (is (thrown? Exception (analyze (parse-defasm '(defasm foo (%jmp bar))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm bar (%add %rax 1))))
    (is (= (parse-defasm '(defasm foo (%jmp bar)))
           (analyze (parse-defasm '(defasm foo (%jmp bar)))))))
  (with-empty-env
    (analyze (parse-defimport '(defimport bar)))
    (is (= (parse-defasm '(defasm foo (%jmp bar)))
           (analyze (parse-defasm '(defasm foo (%jmp bar))))))))

(deftest test-check-double-symbol-definition
  (analyze (parse-defasm '(defasm foo (%add %rax 1))))
  (is (thrown? Exception (analyze (parse-defimport '(defimport foo)))))
  (is (thrown? Exception (analyze (parse-defdata '(defdata foo #int8 1)))))
  (is (thrown? Exception (analyze (parse-defconst '(defconst foo 1))))))

(deftest test-check-symbol-format
  (is (thrown? Exception (analyze (parse-defimport '(defimport foo-bar)))))
  (is (thrown? Exception (analyze (parse-defasm '(defasm ^:export foo-bar
                                                   (%add %rax 1))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo-bar (%add %rax 1)))))
  (is (thrown? Exception (analyze (parse-defdata
                                   '(defdata ^:export foo-bar 1)))))
  (with-empty-env
    (analyze (parse-defdata '(defdata foo-bar #int8 1)))))

(deftest test-check-labels
  (with-empty-env
    (analyze (parse-defasm '(defasm foo
                              (%jmp bar-baz)
                              (%label bar-baz)
                              (%add %rbx 1)))))
  (with-empty-env
    (is (thrown? Exception (analyze (parse-defasm '(defasm foo
                                                     (%add %rax 1)
                                                     (%label foo)
                                                     (%add %rbx 1)))))))
  (with-empty-env
    (is (thrown? Exception (analyze (parse-defasm '(defasm foo
                                                     (%add %rax 1)
                                                     (%label bar)
                                                     (%add %rbx 1)
                                                     (%label bar)
                                                     (%add %rcx 1))))))))

(deftest test-replace-constant-values
  (with-empty-env
    (analyze (parse-defconst '(defconst baz 1)))
    (analyze (parse-defconst '(defconst bar baz)))
    (analyze (parse-defdata '(defdata quux #int8 1)))
    (is (= (parse-defasm '(defasm foo1
                            (%add (%mem8 1) 1)))
           (analyze (parse-defasm '(defasm foo1
                                     (%add (%mem8 baz) bar))))))
    (is (= (parse-defasm '(defasm foo2
                            (%add (%mem8 quux) quux)))
           (analyze (parse-defasm '(defasm foo2
                                     (%add (%mem8 quux) quux)))))))
  (with-empty-env
    (analyze (parse-defconst '(defconst baz #int8 1)))
    (analyze (parse-defconst '(defconst bar baz)))
    (analyze (parse-defdata '(defdata quux #int8 1)))
    (is (= (parse-defdata '(defdata foo1 #int8 1))
           (analyze (parse-defdata '(defdata foo1 baz)))))
    (is (= (parse-defdata '(defdata foo2 quux))
           (analyze (parse-defdata '(defdata foo2 quux)))))))

(deftest test-syntax-check
  (with-empty-env
    (is (thrown? Exception (analyze (parse-defasm '(defasm foo
                                                     (%add %rax)))))))
  (with-empty-env
    (is (thrown? Exception (analyze (parse-defasm '(defasm foo
                                                     (%add %rax #int16 1)))))))
  (with-empty-env
    (analyze (parse-defconst '(defconst bar #int16 1)))
    (is (thrown? Exception (analyze (parse-defasm '(defasm foo
                                                     (%add %rax bar)))))))
  (with-empty-env
    (analyze (parse-defconst '(defconst bar #int16 1)))
    (is (= {:type :defasm
            :name (parse-symbol 'foo)
            :statements [(parse-instruction '(%mov %rax 0xfffffffffffffff8))]}
           (analyze (parse-defasm
                     '(defasm foo
                        (%mov %rax 0xfffffffffffffff8))))))))

(deftest test-basic-blocks
  (let [label-foo (parse-label '(%label foo {%rax uint64}))
        symbol-foo (:name label-foo)
        add1 (parse-instruction '(%add %rax 1))
        add2 (parse-instruction '(%add %rax 2))
        jmp-foo (parse-instruction '(%jmp foo))]
    (with-empty-env
      (let [block0 {:index 0
                    :label label-foo
                    :instructions [add1 add2]}]
        (is (= {symbol-foo block0 0 block0}
               (:basic-blocks (meta (analyze (parse-defasm
                                              '(defasm foo {%rax uint64}
                                                 (%add %rax 1)
                                                 (%add %rax 2))))))))))
    (with-empty-env
      (let [block0 {:index 0
                    :label label-foo
                    :instructions [add1 jmp-foo]}]
        (is (= {symbol-foo block0 0 block0}
               (:basic-blocks (meta (analyze (parse-defasm
                                              '(defasm foo {%rax uint64}
                                                 (%add %rax 1)
                                                 (%jmp foo))))))))))
    (with-empty-env
      (let [block0 {:index 0
                    :label label-foo
                    :instructions [add1 jmp-foo]}
            block1 {:index 1
                    :instructions [add2]}]
        (is (= {symbol-foo block0 0 block0 1 block1}
               (:basic-blocks (meta (analyze (parse-defasm
                                              '(defasm foo {%rax uint64}
                                                 (%add %rax 1)
                                                 (%jmp foo)
                                                 (%add %rax 2))))))))))
    (let [label-bar (parse-label '(%label bar))
          symbol-bar (:name label-bar)
          block1 {:index 1
                  :label label-bar
                  :instructions [add2]}]
      (with-empty-env
        (let [block0 {:index 0
                      :label label-foo
                      :instructions [add1]}]
          (is (= {symbol-foo block0 0 block0
                  symbol-bar block1 1 block1}
                 (:basic-blocks (meta (analyze (parse-defasm
                                                '(defasm foo {%rax uint64}
                                                   (%add %rax 1)
                                                   (%label bar)
                                                   (%add %rax 2))))))))))
      (with-empty-env
        (let [block0 {:index 0
                      :label label-foo
                      :instructions [add1 jmp-foo]}]
          (is (= {symbol-foo block0 0 block0
                  symbol-bar block1 1 block1}
                 (:basic-blocks (meta (analyze (parse-defasm
                                                '(defasm foo {%rax uint64}
                                                   (%add %rax 1)
                                                   (%jmp foo)
                                                   (%label bar)
                                                   (%add %rax 2)))))))))))))

(deftest test-check-tags
  (with-empty-env
    (analyze (parse-defasm '(defasm foo {%rax uint64 %rbx uint64}
                              (%add %rax %rbx)))))
  (is (thrown? Exception
               (analyze (parse-defasm '(defasm foo {%rax uint64 %rbx int64}
                                         (%add %rax %rbx))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo {%eax uint32}
                              (%add %eax #uint32 1)))))
  (is (thrown? Exception
               (analyze (parse-defasm '(defasm foo {%eax uint32}
                                         (%add %eax #int32 1))))))
  (with-empty-env
    (is (thrown? Exception
                 (analyze (parse-defasm '(defasm foo {%rax uint64}
                                           (%add %rax %rbx))))))))
