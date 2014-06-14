;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.emitter
  (:refer-clojure :exclude [munge number? string? symbol?])
  (:require [clojure.test :refer :all]
            [sybilant.analyzer :refer [*globals* analyze]]
            [sybilant.emitter :refer :all]
            [sybilant.parser :refer :all])
  (:import (java.io StringWriter)))

(defn emit* [exp]
  (with-open [sw (StringWriter.)]
    (emit exp sw)
    (str sw)))

(deftest test-emit-symbol
  (is (= "foo" (emit* (parse-symbol 'foo)))))

(deftest test-emit-string
  (is (= "102, 111, 111" (emit* (parse-string "foo"))))
  (is (= "-30, -104, -125" (emit* (parse-string "☃")))))

(deftest test-emit-number
  (is (= "1" (emit* (parse-number 1)))))

(deftest test-emit-signed-integers
  (is (= "1" (emit* (parse-int8 (byte 1)))))
  (is (= "1" (emit* (parse-int16 (short 1)))))
  (is (= "1" (emit* (parse-int32 (int 1)))))
  (is (= "1" (emit* (parse-int64 (->Int64 1))))))

(deftest test-emit-unsigned-integers
  (is (= "1" (emit* (parse-uint8 (->Uint8 1)))))
  (is (= "1" (emit* (parse-uint16 (->Uint16 1)))))
  (is (= "1" (emit* (parse-uint32 (->Uint32 1)))))
  (is (= "1" (emit* (parse-uint64 (->Uint64 1))))))

(deftest test-emit-register
  (is (= "rax" (emit* (parse-register '%rax)))))

(deftest test-emit-mem
  (is (= "[17]" (emit* (parse-mem '(%mem8 17)))))
  (is (= "[rax]" (emit* (parse-mem '(%mem8 %rax)))))
  (is (= "[rax+17]" (emit* (parse-mem '(%mem8 %rax 17)))))
  (is (= "[(rbx*4)+17]" (emit* (parse-mem '(%mem8 %rbx 4 17)))))
  (is (= "[rax+rbx+17]" (emit* (parse-mem '(%mem8 %rax %rbx 17)))))
  (is (= "[rax+(rbx*4)+17]" (emit* (parse-mem '(%mem8 %rax %rbx 4 17))))))

(deftest test-emit-operator
  (is (= "add" (emit* (parse-operator '%add)))))

(deftest test-emit-instruction
  (is (= "add rax, byte 1\n" (emit* (parse-instruction '(%add %rax #int8 1))))))

(deftest test-emit-label
  (binding [*globals* (atom {})]
    (is (= "\nglobal foo\nfoo:\njmp .bar\n.bar:\nadd rax, 1\n"
           (emit* (analyze (parse-defasm '(defasm foo
                                            (%jmp bar)
                                            (%label bar)
                                            (%add %rax 1))))))))
  (binding [*globals* (atom {})]
    (is (= "\nglobal bar\nbar:\njmp ._u2603\n._u2603:\nadd rax, 1\n"
           (emit* (analyze (parse-defasm '(defasm bar
                                            (%jmp ☃)
                                            (%label ☃)
                                            (%add %rax 1)))))))))

(deftest test-emit-defasm
  (is (= "\nglobal foo\nfoo:\nadd rax, 1\n"
         (emit* (parse-defasm '(defasm foo (%add %rax 1))))))
  (is (= "\nglobal _u2603\n_u2603:\nadd rax, 1\n"
         (emit* (parse-defasm '(defasm ☃ (%add %rax 1)))))))

(deftest test-emit-defimport
  (is (= "\nextern foo\n"
         (emit* (parse-defimport '(defimport foo))))))

(deftest test-emit-defdata
  (is (= "\nglobal foo\nfoo:\ndb 1\ndq 2\n"
         (emit* (parse-defdata '(defdata foo #int8 1 #uint64 2)))))
  (is (= "\nglobal _u2603\n_u2603:\ndb 1\ndq 2\n"
         (emit* (parse-defdata '(defdata ☃ #int8 1 #uint64 2))))))
