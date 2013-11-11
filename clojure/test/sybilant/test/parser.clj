;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.parser
  (:refer-clojure :exclude [number? symbol?])
  (:require [clojure.test :refer :all]
            [sybilant.parser :refer :all]))

(deftest test-read-signed-integers
  (is (= (byte 1) (read-string "#int8 1")))
  (is (= (short 1) (read-string "#int16 1")))
  (is (= (int 1) (read-string "#int32 1")))
  (is (= (->Int64 1) (read-string "#int64 1"))))

(deftest test-print-signed-integers
  (is (= "#int8 1" (pr-str (byte 1))))
  (is (= "#int16 1" (pr-str (short 1))))
  (is (= "#int32 1" (pr-str (int 1))))
  (is (= "#int64 1" (pr-str (->Int64 1)))))

(deftest test-parse-signed-integers
  (is (= {:type :int :width 8 :form 1} (parse-int8 (byte 1))))
  (is (= {:type :int :width 16 :form 1} (parse-int16 (short 1))))
  (is (= {:type :int :width 32 :form 1} (parse-int32 (int 1))))
  (is (= {:type :int :width 64 :form 1} (parse-int64 (->Int64 1)))))

(deftest test-read-unsigned-integers
  (is (= (->Uint8 1) (read-string "#uint8 1")))
  (is (= (->Uint16 1) (read-string "#uint16 1")))
  (is (= (->Uint32 1) (read-string "#uint32 1")))
  (is (= (->Uint64 1) (read-string "#uint64 1"))))

(deftest test-print-unsigned-integers
  (is (= "#uint8 1" (pr-str (->Uint8 1))))
  (is (= "#uint16 1" (pr-str (->Uint16 1))))
  (is (= "#uint32 1" (pr-str (->Uint32 1))))
  (is (= "#uint64 1" (pr-str (->Uint64 1)))))

(deftest test-parse-unsigned-integers
  (is (= {:type :uint :width 8 :form 1} (parse-uint8 (->Uint8 1))))
  (is (= {:type :uint :width 16 :form 1} (parse-uint16 (->Uint16 1))))
  (is (= {:type :uint :width 32 :form 1} (parse-uint32 (->Uint32 1))))
  (is (= {:type :uint :width 64 :form 1} (parse-uint64 (->Uint64 1)))))

(deftest test-parse-symbol
  (is (symbol-form? 'foo))
  (is (= {:type :symbol :form 'foo} (parse-symbol 'foo)))
  (is (symbol? (parse-symbol 'foo)))
  (is (nil? (meta (parse-symbol 'foo)))))

(deftest test-parse-number
  (is (number-form? 1))
  (is (= {:type :number :form 1} (parse-number 1)))
  (is (number? (parse-number 1))))

(deftest test-parse-register
  (is (register-form? '%rax))
  (is (= {:type :register :name 'a :width 64} (parse-register '%rax)))
  (is (register? (parse-register '%rax)))
  (is (= {:form '%rax} (meta (parse-register '%rax)))))

(deftest test-parse-operator
  (is (operator-form? '%add))
  (is (= {:type :operator :form '%add} (parse-operator '%add)))
  (is (operator? (parse-operator '%add)))
  (is (nil? (meta (parse-operator '%add)))))

(deftest test-parse-operand
  (testing "number"
    (is (operand-form? 1))
    (is (= (parse-number 1) (parse-operand 1)))
    (is (operand? (parse-operand 1))))
  (testing "signed integers"
    (doseq [i [(byte 1) (short 1) (int 1) (->Int64 1)]]
      (is (operand-form? i))
      (is (= (parse-int i) (parse-operand i)))
      (is (operand? (parse-operand i)))))
  (testing "unsigned integers"
    (doseq [i [(->Uint8 1) (->Uint16 1) (->Uint32 1) (->Uint64 1)]]
      (is (operand-form? i))
      (is (= (parse-uint i) (parse-operand i)))
      (is (operand? (parse-operand i)))))
  (testing "register"
    (is (operand-form? '%rax))
    (is (= (parse-register '%rax) (parse-operand '%rax)))
    (is (operand? (parse-operand '%rax))))
  (testing "symbol"
    (is (operand-form? 'foo))
    (is (= (parse-symbol 'foo) (parse-operand 'foo)))
    (is (operand? (parse-operand 'foo)))))

(deftest test-parse-instruction
  (let [form '(%add %rax 1)]
    (is (instruction-form? form))
    (let [instruction (parse-instruction form)
          meta (meta instruction)]
      (is (= {:type :instruction
              :operator (parse-operator '%add)
              :operands [(parse-register '%rax) (parse-number 1)]}
             instruction))
      (is (instruction? instruction))
      (is (= form (:form meta)))
      (is (:line meta))
      (is (:column meta))))
  (is (thrown? Exception (parse-instruction '(%add (%add %rax 1) 1)))))

(deftest test-parse-defasm
  (let [form '(defasm foo (%add %rax 1))]
    (is (defasm-form? form))
    (let [defasm (parse-defasm form)
          meta (meta defasm)]
      (is (= {:type :defasm
              :name (parse-symbol 'foo)
              :statements [(parse-instruction '(%add %rax 1))]}
             defasm))
      (is (defasm? defasm))
      (is (= form (:form meta)))
      (is (:line meta))
      (is (:column meta))))
  (is (thrown? Exception (parse-defasm '(defasm))))
  (is (thrown? Exception (parse-defasm '(defasm foo))))
  (is (thrown? Exception (parse-defasm '(defasm 1))))
  (is (thrown? Exception (parse-defasm '(defasm foo 1)))))

(deftest test-parse-defextern
  (let [form '(defextern foo)]
    (is (defextern-form? form))
    (let [defextern (parse-defextern form)
          meta (meta defextern)]
      (is (= {:type :defextern :name (parse-symbol 'foo)}
             defextern))
      (is (defextern? defextern))
      (is (= form (:form meta)))
      (is (:line meta))
      (is (:column meta))))
  (is (thrown? Exception (parse-defextern '(defextern))))
  (is (thrown? Exception (parse-defextern '(defextern 1))))
  (is (thrown? Exception (parse-defextern '(defextern foo bar)))))
