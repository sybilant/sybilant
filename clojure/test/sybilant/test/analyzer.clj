;;;; Copyright © Paul Stadig. All rights reserved.
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
            [sybilant.test.utils :refer [reset-globals with-empty-env]]))

(use-fixtures :each reset-globals)

(deftest test-undefined-symbol-reference
  (is (error?
       "bar is undefined"
       (analyze (parse-defasm '(defasm foo (%jmp bar))))))
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
  (is (error? "foo is already defined"
              (analyze (parse-defimport '(defimport foo)))))
  (is (error? "foo is already defined"
              (analyze (parse-defdata '(defdata foo #int8 1)))))
  (is (error? "foo is already defined"
              (analyze (parse-defconst '(defconst foo 1))))))

(deftest test-check-symbol-format
  (is (error? "foo-bar is an invalid symbol"
              (analyze (parse-defimport '(defimport foo-bar)))))
  (is (error? "foo-bar is an invalid symbol"
              (analyze (parse-defasm '(defasm ^:export foo-bar
                                        (%add %rax 1))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo-bar (%add %rax 1)))))
  (is (error? "foo-bar is an invalid symbol"
              (analyze (parse-defdata
                        '(defdata ^:export foo-bar #int8 1)))))
  (with-empty-env
    (analyze (parse-defdata '(defdata foo-bar #int8 1)))))

(deftest test-check-labels
  (analyze (parse-defasm '(defasm foo0
                            (%jmp bar-baz)
                            (%label bar-baz)
                            (%add %rbx 1))))
  (is (error? "foo1 is already defined"
              (analyze (parse-defasm '(defasm foo1
                                        (%add %rax 1)
                                        (%label foo1)
                                        (%add %rbx 1))))))
  (is (error? "bar is already defined"
              (analyze (parse-defasm '(defasm foo2
                                        (%add %rax 1)
                                        (%label bar)
                                        (%add %rbx 1)
                                        (%label bar)
                                        (%add %rcx 1)))))))

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
    (is (error? "invalid syntax for \\(%add %rax\\)"
                (analyze (parse-defasm '(defasm foo
                                          (%add %rax)))))))
  (with-empty-env
    (is (error? "invalid syntax for \\(%add %rax #int16 1\\)"
                (analyze (parse-defasm '(defasm foo
                                          (%add %rax #int16 1)))))))
  (with-empty-env
    (analyze (parse-defconst '(defconst bar #int16 1)))
    (is (error? "invalid syntax for \\(%add %rax bar\\)"
                (analyze (parse-defasm '(defasm foo
                                          (%add %rax bar)))))))
  (with-empty-env
    (analyze (parse-defconst '(defconst bar #int16 1)))
    (is (= {:type :defasm
            :name (parse-symbol 'foo)
            :statements [(parse-instruction '(%mov %rax 0xfffffffffffffff8))]}
           (analyze (parse-defasm
                     '(defasm foo
                        (%mov %rax 0xfffffffffffffff8))))))))

(deftest test-label-tag
  (with-empty-env
    (is (error? "duplicate register %rax"
                (analyze (parse-defasm '(defasm foo {%rax int64 %ax int16}
                                          (%add %rax 1)))))))
  (with-empty-env
    (is (error? "label tag expects register and tag to be same width, but got"
                " %rax with int8"
                (analyze (parse-defasm '(defasm foo {%rax int8}
                                          (%add %rax 1))))))))

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
  (is (error? "incompatible types: uint64 int64"
              (analyze (parse-defasm '(defasm foo {%rax uint64 %rbx int64}
                                        (%add %rax %rbx))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo {%eax uint32}
                              (%add %eax #uint32 1)))))
  (is (error? "incompatible types: uint32 int32"
              (analyze (parse-defasm '(defasm foo {%eax uint32}
                                        (%add %eax #int32 1))))))
  (with-empty-env
    (is (error? "missing tag for %rbx"
                (analyze (parse-defasm '(defasm foo {%rax uint64}
                                          (%add %rax %rbx)))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo {%rax uint64}
                              (%mov %rbx #uint64 1)
                              (%add %rax %rbx)))))
  (is (error? "incompatible types: uint64 int64"
              (analyze (parse-defasm '(defasm foo {%rbx uint64 %rcx int64}
                                        (%mov %rax 1)
                                        (%add %rax %rbx)
                                        (%add %rax %rcx))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo {%rax int64}
                              (%add %rax #int32 1)))))
  (is (error? "incompatible types: uint64 int32"
              (analyze (parse-defasm '(defasm foo {%rax uint64}
                                        (%add %rax #int32 1))))))
  (is (error? "%eax not compatible with tag: int64"
              (analyze (parse-defasm '(defasm foo {%rax int64}
                                        (%add %eax #int32 1))))))
  (analyze (parse-defasm '(defasm foo {%rax int64}
                            (%ret))))
  (with-empty-env
    (is (error? "incompatible types: int32 uint32"
                (analyze (parse-defasm '(defasm foo {%eax int32}
                                          (%cmp %eax #int32 1)
                                          (%jne bar)
                                          (%add %eax #int32 1)
                                          (%label bar)
                                          (%sub %eax #uint32 1)))))))
  (with-empty-env
    (is (error? "incompatible types for %eax: uint32 int32"
                (analyze (parse-defasm '(defasm foo {%eax int32}
                                          (%cmp %eax #int32 1)
                                          (%jne bar)
                                          (%add %eax #int32 1)
                                          (%label bar {%eax uint32})
                                          (%sub %eax #uint32 1)))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo {%eax int32}
                              (%cmp %eax #int32 1)
                              (%jne end)
                              (%add %eax #int32 1)
                              (%jmp end)
                              (%label bar {%eax uint32})
                              (%sub %eax #uint32 1)
                              (%label end)
                              (%ret)))))
  (with-empty-env
    (is (error? "incompatible types: int32 uint32"
                (analyze (parse-defasm '(defasm foo {%eax int32}
                                          (%cmp %eax #int32 1)
                                          (%jne bar)
                                          (%add %eax #int32 1)
                                          (%jmp end)
                                          (%label bar)
                                          (%sub %eax #uint32 1)
                                          (%label end)
                                          (%ret)))))))
  (with-empty-env
    (is (error? "bar requires a tag"
                (analyze (parse-defasm '(defasm foo {%eax int32}
                                          (%cmp %eax #int32 1)
                                          (%label bar)
                                          (%jne bar)
                                          (%ret)))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm bar
                              (%ret))))
    (analyze (parse-defasm '(defasm foo {%eax int32}
                              (%cmp %eax #int32 1)
                              (%jne bar)))))
  (with-empty-env
    (analyze (parse-defasm '(defasm bar {%eax uint32}
                              (%ret))))
    (is (error? "incompatible types for %eax: uint32 int32"
                (analyze (parse-defasm '(defasm foo {%eax int32}
                                          (%cmp %eax #int32 1)
                                          (%jne bar)))))))
  (with-empty-env
    (analyze (parse-defdata '(defdata bar #int8 1)))
    (is (error? "target of jump instruction must be a label or defasm: bar"
                (analyze (parse-defasm '(defasm foo {%eax int32}
                                          (%cmp %eax #int32 1)
                                          (%jne bar)))))))
  (is (error? "\\(%mem64 %rbx\\) not allowed in checked block"
              (analyze (parse-defasm '(defasm foo {%rax uint64 %rbx int64}
                                        (%add %rax (%mem64 %rbx)))))))
  (with-empty-env
    (analyze (parse-defasm '(defasm foo ^:unchecked {%rax uint64}
                              (%add %rax (%mem64 %rbx)))))
    (is (error? "incompatible types for %rax: uint64 int64"
                (analyze (parse-defasm '(defasm bar {%rax int64}
                                          (%add %rax 1)
                                          (%jmp foo)))))))
  (with-empty-env
    (is (error? "missing tag for %rbx"
                (analyze (parse-defasm '(defasm foo {%rax uint64}
                                          (%mov %rax %rbx))))))
    (analyze (parse-defasm '(defasm foo {%rax int64}
                              (%mov %rbx #int32 1)
                              (%add %rax %rbx))))))

(deftest test-check-movsx
  (analyze (parse-defasm '(defasm foo {%ax int16}
                            (%movsx %rbx %ax)
                            (%label bar {%rbx int64})
                            (%ret)))))

(deftest test-check-movsxd
  (analyze (parse-defasm '(defasm foo {%eax int32}
                            (%movsxd %rbx %eax)
                            (%label bar {%rbx int64})
                            (%ret)))))

(deftest test-check-movzx
  (analyze (parse-defasm '(defasm foo {%ax uint16}
                            (%movzx %rbx %ax)
                            (%label bar {%rbx uint64})
                            (%ret)))))

(deftest test-check-bsf
  (analyze (parse-defasm '(defasm foo {%rbx int64}
                            (%bsf %rax %rbx)
                            (%add %rax #int32 1)
                            (%ret)))))
