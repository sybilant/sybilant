;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.emitter
  (:refer-clojure :exclude [number? symbol?])
  (:require [clojure.test :refer :all]
            [sybilant.parser :refer :all]
            [sybilant.emitter :refer :all])
  (:import (java.io StringWriter)))

(defn emit* [exp]
  (with-open [sw (StringWriter.)]
    (emit exp sw)
    (str sw)))

(deftest test-emit-symbol
  (is (= "foo" (emit* (parse-symbol 'foo)))))

(deftest test-emit-number
  (is (= "1" (emit* (parse-number 1)))))

(deftest test-emit-signed-integers
  (is (= "byte 1" (emit* (parse-int8 (byte 1)))))
  (is (= "word 1" (emit* (parse-int16 (short 1)))))
  (is (= "dword 1" (emit* (parse-int32 (int 1)))))
  (is (= "qword 1" (emit* (parse-int64 (->Int64 1))))))

(deftest test-emit-unsigned-integers
  (is (= "byte 1" (emit* (parse-uint8 (->Uint8 1)))))
  (is (= "word 1" (emit* (parse-uint16 (->Uint16 1)))))
  (is (= "dword 1" (emit* (parse-uint32 (->Uint32 1)))))
  (is (= "qword 1" (emit* (parse-uint64 (->Uint64 1))))))

(deftest test-emit-register
  (is (= "rax" (emit* (parse-register '%rax)))))

(deftest test-emit-operator
  (is (= "add" (emit* (parse-operator '%add)))))

(deftest test-emit-instruction
  (is (= "add rax, 1\n" (emit* (parse-instruction '(%add %rax 1))))))

(deftest test-emit-defasm
  (is (= "\nglobal foo\nfoo:\nadd rax, 1\n"
         (emit* (parse-defasm '(defasm foo (%add %rax 1)))))))

(deftest test-emit-defextern
  (is (= "\nextern foo\n"
         (emit* (parse-defextern '(defextern foo))))))
