;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.parser
  (:refer-clojure :exclude [number? symbol?])
  (:require [clojure.core :as clj]
            [clojure.java.io :as io]
            [slingshot.slingshot :refer [throw+]]
            [sybilant.util :refer [error]]))

(defn read-int8 [form]
  (unchecked-byte form))

(defmethod print-method Byte [form out]
  (.write out "#int8 ")
  (.write out (str form)))

(defn read-int16 [form]
  (unchecked-short form))

(defmethod print-method Short [form out]
  (.write out "#int16 ")
  (.write out (str form)))

(defn read-int32 [form]
  (unchecked-int form))

(defmethod print-method Integer [form out]
  (.write out "#int32 ")
  (.write out (str form)))

(defrecord Int64 [form])

(defn read-int64 [form]
  (->Int64 (long form)))

(defmethod print-method Int64 [form out]
  (.write out "#int64 ")
  (.write out (str (:form form))))

(defrecord Uint8 [form])

(defn read-uint8 [form]
  (->Uint8 (long form)))

(defmethod print-method Uint8 [form out]
  (.write out "#uint8 ")
  (.write out (str (:form form))))

(defrecord Uint16 [form])

(defn read-uint16 [form]
  (->Uint16 (long form)))

(defmethod print-method Uint16 [form out]
  (.write out "#uint16 ")
  (.write out (str (:form form))))

(defrecord Uint32 [form])

(defn read-uint32 [form]
  (->Uint32 (long form)))

(defmethod print-method Uint32 [form out]
  (.write out "#uint32 ")
  (.write out (str (:form form))))

(defrecord Uint64 [form])

(defn read-uint64 [form]
  (->Uint64 (long form)))

(defmethod print-method Uint64 [form out]
  (.write out "#uint64 ")
  (.write out (str (:form form))))

(defn typed-map? [exp t]
  (and (map? exp) (= t (:type exp))))

(defn symbol? [exp]
  (typed-map? exp :symbol))

(defn symbol-form? [form]
  (clj/symbol? form))

(defn make-symbol [form]
  {:pre [(symbol-form? form)]}
  (with-meta {:type :symbol :form form} (meta form)))

(defn parse-symbol [form]
  (when-not (symbol-form? form)
    (error "expected symbol, but was" form))
  (make-symbol form))

(defn number? [exp]
  (typed-map? exp :number))

(defn number-form? [form]
  (instance? Long form))

(defn make-number [form]
  {:pre [(number-form? form)]}
  {:type :number :form form})

(defn parse-number [form]
  (when-not (number-form? form)
    (error "expected number, but was" form))
  (make-number form))

(defn int? [exp]
  (typed-map? exp :int))

(defn int8? [exp]
  (and (int? exp) (= 8 (:width exp))))

(defn int8-form? [form]
  (instance? Byte form))

(defn make-int8 [form]
  {:pre [(int8-form? form)]}
  {:type :int :width 8 :form form})

(defn parse-int8 [form]
  (when-not (int8-form? form)
    (error "expected int8, but was" form))
  (make-int8 form))

(defn int16? [exp]
  (and (int? exp) (= 16 (:width exp))))

(defn int16-form? [form]
  (instance? Short form))

(defn make-int16 [form]
  {:pre [(int16-form? form)]}
  {:type :int :width 16 :form form})

(defn parse-int16 [form]
  (when-not (int16-form? form)
    (error "expected int16, but was" form))
  (make-int16 form))

(defn int32? [exp]
  (and (int? exp) (= 32 (:width exp))))

(defn int32-form? [form]
  (instance? Integer form))

(defn make-int32 [form]
  {:pre [(int32-form? form)]}
  {:type :int :width 32 :form form})

(defn parse-int32 [form]
  (when-not (int32-form? form)
    (error "expected int32, but was" form))
  (make-int32 form))

(defn int64? [exp]
  (and (int? exp) (= 64 (:width exp))))

(defn int64-form? [form]
  (instance? Int64 form))

(defn make-int64 [form]
  {:pre [(int64-form? form)]}
  {:type :int :width 64 :form (:form form)})

(defn parse-int64 [form]
  (when-not (int64-form? form)
    (error "expected int64, but was" form))
  (make-int64 form))

(defn int-form? [form]
  (or (int8-form? form) (int16-form? form) (int32-form? form)
      (int64-form? form)))

(defn parse-int [form]
  (when-not (int-form? form)
    (error "expected int, but was" form))
  (cond
   (int8-form? form) (parse-int8 form)
   (int16-form? form) (parse-int16 form)
   (int32-form? form) (parse-int32 form)
   (int64-form? form) (parse-int64 form)))

(defn uint? [exp]
  (typed-map? exp :uint))

(defn uint8? [exp]
  (and (uint? exp) (= 8 (:width exp))))

(defn uint8-form? [form]
  (instance? Uint8 form))

(defn make-uint8 [form]
  {:pre [(uint8-form? form)]}
  {:type :uint :width 8 :form (:form form)})

(defn parse-uint8 [form]
  (when-not (uint8-form? form)
    (error "expected uint8, but was" form))
  (make-uint8 form))

(defn uint16? [exp]
  (and (uint? exp) (= 16 (:width exp))))

(defn uint16-form? [form]
  (instance? Uint16 form))

(defn make-uint16 [form]
  {:pre [(uint16-form? form)]}
  {:type :uint :width 16 :form (:form form)})

(defn parse-uint16 [form]
  (when-not (uint16-form? form)
    (error "expected uint16, but was" form))
  (make-uint16 form))

(defn uint32? [exp]
  (and (uint? exp) (= 32 (:width exp))))

(defn uint32-form? [form]
  (instance? Uint32 form))

(defn make-uint32 [form]
  {:pre [(uint32-form? form)]}
  {:type :uint :width 32 :form (:form form)})

(defn parse-uint32 [form]
  (when-not (uint32-form? form)
    (error "expected uint32, but was" form))
  (make-uint32 form))

(defn uint64? [exp]
  (and (uint? exp) (= 64 (:width exp))))

(defn uint64-form? [form]
  (instance? Uint64 form))

(defn make-uint64 [form]
  {:pre [(uint64-form? form)]}
  {:type :uint :width 64 :form (:form form)})

(defn parse-uint64 [form]
  (when-not (uint64-form? form)
    (error "expected uint64, but was" form))
  (make-uint64 form))

(defn uint-form? [form]
  (or (uint8-form? form) (uint16-form? form) (uint32-form? form)
      (uint64-form? form)))

(defn parse-uint [form]
  (when-not (uint-form? form)
    (error "expected uint, but was" form))
  (cond
   (uint8-form? form) (parse-uint8 form)
   (uint16-form? form) (parse-uint16 form)
   (uint32-form? form) (parse-uint32 form)
   (uint64-form? form) (parse-uint64 form)))

(def registers
  (-> "sybilant/registers.clj"
      io/resource
      slurp
      read-string))

(assert (do (doseq [[r v] registers]
              (assert (= r (:form (meta v))) r))
            true))

(defn register? [exp]
  (typed-map? exp :register))

(defn register-form? [form]
  (contains? registers form))

(defn make-register [form]
  {:pre [(register-form? form)]}
  (vary-meta (get registers form) merge (meta form)))

(defn parse-register [form]
  (when-not (register-form? form)
    (error "expected register, but was" form))
  (make-register form))

(def operators
  (-> "sybilant/operators.clj"
      io/resource
      slurp
      read-string))

(assert (do (doseq [[r v] operators]
              (assert (= r (:form v)) r))
            true))

(defn operator? [exp]
  (typed-map? exp :operator))

(defn operator-form? [form]
  (contains? operators form))

(defn make-operator [form]
  {:pre [(operator-form? form)]}
  (with-meta (get operators form) (meta form)))

(defn parse-operator [form]
  (when-not (operator-form? form)
    (error "expected operator, but was" form))
  (make-operator form))

(defn operand? [exp]
  (or (number? exp) (int? exp) (uint? exp) (register? exp) (symbol? exp)))

(defn operand-form? [form]
  (or (number-form? form) (int-form? form) (uint-form? form)
      (register-form? form) (symbol-form? form)))

(defn parse-operand [form]
  (when-not (operand-form? form)
    (error "expected operand, but was" form))
  (cond
   (number-form? form) (parse-number form)
   (int-form? form) (parse-int form)
   (uint-form? form) (parse-uint form)
   (register-form? form) (parse-register form)
   (symbol-form? form) (parse-symbol form)))

(defn instruction? [exp]
  (typed-map? exp :instruction))

(defn instruction-form? [form]
  (and (list? form) (operator-form? (first form))))

(defn make-instruction
  ([operator operands]
     {:pre [(operator? operator) (every? operand? operands)]}
     {:type :instruction :operator operator :operands operands})
  ([operator operands form]
     {:pre [(instruction-form? form)]}
     (with-meta (make-instruction operator operands)
       (assoc (meta form) :form (with-meta form {})))))

(defn parse-instruction [form]
  (when-not (instruction-form? form)
    (error "expected instruction, but was" form))
  (let [[operator & operands] form]
    (doseq [operand operands]
      (when-not (operand-form? operand)
        (error "instruction expects an operand, but was" operand)))
    (make-instruction (parse-operator operator)
                      (map parse-operand operands)
                      form)))

(defn tagged-list? [form t]
  (and (list? form) (= t (first form))))

(defn defasm? [exp]
  (typed-map? exp :defasm))

(defn defasm-form? [form]
  (tagged-list? form 'defasm))

(defn make-defasm
  ([name statements]
     {:pre [(symbol? name) (every? instruction? statements)]}
     (with-meta {:type :defasm :name name :statements statements}
       {:definition? true}))
  ([name statements form]
     {:pre [(defasm-form? form)]}
     (-> (make-defasm name statements)
         (vary-meta merge (meta form))
         (vary-meta assoc :form (with-meta form {})))))

(defn parse-defasm [form]
  (when-not (defasm-form? form)
    (error "expected defasm, but was" form))
  (let [form-count (dec (count form))]
    (when (< form-count 2)
      (error "defasm expects at least 2 arguments, but got" form-count)))
  (let [[_ name & statements] form]
    (when-not (symbol-form? name)
      (error "defasm expects a symbol for name, but got" name))
    (doseq [statement statements]
      (when-not (instruction-form? statement)
        (error "defasm expects a statement, but got" statement)))
    (make-defasm (parse-symbol name) (map parse-instruction statements) form)))

(defn defextern? [exp]
  (typed-map? exp :defextern))

(defn defextern-form? [form]
  (tagged-list? form 'defextern))

(defn make-defextern
  ([name]
     {:pre [(symbol? name)]}
     (with-meta {:type :defextern :name name}
       {:definition? true}))
  ([name form]
     {:pre [(defextern-form? form)]}
     (-> (make-defextern name)
         (vary-meta merge (meta form))
         (vary-meta assoc :form (with-meta form {})))))

(defn parse-defextern [form]
  (when-not (defextern-form? form)
    (error "expected defextern, but was" form))
  (let [form-count (dec (count form))]
    (when (not= form-count 1)
      (error "defextern expects exactly 1 argument, but got" form-count)))
  (let [name (second form)]
    (when-not (symbol-form? name)
      (error "defextern expect a symbol for name, but got" name))
    (make-defextern (parse-symbol name) form)))

(defn top-level? [exp]
  (or (defasm? exp) (defextern? exp)))

(defn top-level-form? [form]
  (or (defasm-form? form) (defextern-form? form)))

(defn parse-top-level [form]
  (when-not (top-level-form? form)
    (error "expected top level form, but was" form))
  (cond
   (defasm-form? form) (parse-defasm form)
   (defextern-form? form) (parse-defextern form)))
