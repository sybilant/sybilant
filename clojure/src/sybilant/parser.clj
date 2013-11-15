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
            [sybilant.util :refer [error maybe]]))

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

(defn mem? [exp]
  (typed-map? exp :mem))

(def mem-width '{%mem8 8 %mem16 16 %mem32 32 %mem64 64})

(defn mem-base? [exp]
  (register? exp))

(defn mem-index? [exp]
  (register? exp))

(defn mem-scale-form? [form]
  (and (number-form? form) (contains? #{2 4 8} form)))

(defn mem-scale? [exp]
  (and (number? exp) (mem-scale-form? (:form exp))))

(defn mem-disp-form? [form]
  (and (number-form? form) (<= Integer/MIN_VALUE form Integer/MAX_VALUE)))

(defn mem-disp? [exp]
  (and (number? exp) (mem-disp-form? (:form exp))))

(declare mem-form?)

(defn make-mem
  ([name base index scale disp]
     {:pre [(contains? mem-width name)
            ((maybe mem-base?) base)
            ((maybe mem-index?) index)
            ((maybe mem-scale?) scale)
            ((maybe mem-disp?) disp)
            (or (nil? scale) index)
            (or (nil? index) disp)]}
     (merge {:type :mem :width (mem-width name)}
            (when base
              {:base base})
            (when index
              {:index index})
            (when scale
              {:scale scale})
            (when disp
              {:disp disp})))
  ([name base index scale disp form]
     {:pre [(mem-form? form)]}
     (-> (make-mem name base index scale disp)
         (vary-meta merge (meta form))
         (vary-meta assoc :form (with-meta form {})))))

(defn mem-args? [& args]
  (let [preds (drop-last args)
        args (last args)]
    (and (= (count preds) (count args))
         (every? (fn [[pred arg]] (pred arg)) (map vector preds args)))))

(defn parse-mem-args [[a b c d :as args]]
  (cond
   (mem-args? mem-disp-form? args)
   [nil nil nil a]
   (mem-args? register-form? args)
   [a nil nil nil]
   (mem-args? register-form? number-form? args)
   [a nil nil b]
   (mem-args? register-form? mem-scale-form? mem-disp-form? args)
   [nil a b c]
   (mem-args? register-form? register-form? mem-disp-form? args)
   [a b nil c]
   (mem-args? register-form? register-form? mem-scale-form? mem-disp-form? args)
   [a b c d]))

(defn parse-mem [form]
  (when-not (mem-form? form)
    (error "expected %mem, but was" form))
  (let [arg-count (dec (count form))]
    (when-not (< 0 arg-count)
      (error (first form) "expects at least 1 argument, but got" arg-count))
    (when-not (< arg-count 5)
      (error (first form) "expects at most 4 argument, but got" arg-count))
    (let [[name & args] form
          [base index scale disp :as args] (parse-mem-args args)]
      (if (seq args)
        (make-mem name
                  (when base
                    (parse-register base))
                  (when index
                    (parse-register index))
                  (when scale
                    (parse-number scale))
                  (when disp
                    (parse-number disp))
                  form)
        (error "invalid arguments for" form)))))

(defn tagged-list? [form t]
  (and (list? form) (= t (first form))))

(defn mem8? [exp]
  (and (mem? exp) (= 8 (:width exp))))

(defn mem8-form? [form]
  (tagged-list? form '%mem8))

(defn parse-mem8 [form]
  (when-not (mem8-form? form)
    (error "expected %mem8, but was" form))
  (parse-mem form))

(defn mem16? [exp]
  (and (mem? exp) (= 16 (:width exp))))

(defn mem16-form? [form]
  (tagged-list? form '%mem16))

(defn parse-mem16 [form]
  (when-not (mem16-form? form)
    (error "expected %mem16, but was" form))
  (parse-mem form))

(defn mem32? [exp]
  (and (mem? exp) (= 32 (:width exp))))

(defn mem32-form? [form]
  (tagged-list? form '%mem32))

(defn parse-mem32 [form]
  (when-not (mem32-form? form)
    (error "expected %mem32, but was" form))
  (parse-mem form))

(defn mem64? [exp]
  (and (mem? exp) (= 64 (:width exp))))

(defn mem64-form? [form]
  (tagged-list? form '%mem64))

(defn parse-mem64 [form]
  (when-not (mem64-form? form)
    (error "expected %mem64, but was" form))
  (parse-mem form))

(defn mem-form? [form]
  (or (mem8-form? form) (mem16-form? form) (mem32-form? form)
      (mem64-form? form)))

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
  (or (number? exp) (int? exp) (uint? exp) (register? exp) (symbol? exp)
      (mem? exp)))

(defn operand-form? [form]
  (or (number-form? form) (int-form? form) (uint-form? form)
      (register-form? form) (symbol-form? form) (mem-form? form)))

(defn parse-operand [form]
  (when-not (operand-form? form)
    (error "expected operand, but was" form))
  (cond
   (number-form? form) (parse-number form)
   (int-form? form) (parse-int form)
   (uint-form? form) (parse-uint form)
   (register-form? form) (parse-register form)
   (symbol-form? form) (parse-symbol form)
   (mem-form? form) (parse-mem form)))

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

(defn label? [exp]
  (typed-map? exp :label))

(defn label-form? [form]
  (tagged-list? form '%label))

(defn make-label
  ([name]
     {:pre [(symbol? name)]}
     (with-meta {:type :label :name name}
       {:definition? true}))
  ([name form]
     {:pre [(label-form? form)]}
     (-> (make-label name)
         (vary-meta merge (meta form))
         (vary-meta assoc :form (with-meta form {})))))

(defn parse-label [form]
  (when-not (label-form? form)
    (error "expected %label, but was" form))
  (let [arg-count (dec (count form))]
    (when-not (= 1 arg-count)
      (error "%label expects exactly 1 argument, but got" arg-count)))
  (let [name (second form)]
    (when-not (symbol-form? name)
      (error "%label expects a symbol for name, but got" name))
    (make-label (parse-symbol name) form)))

(defn statement? [exp]
  (or (instruction? exp) (label? exp)))

(defn statement-form? [form]
  (or (instruction-form? form) (label-form? form)))

(defn parse-statement [form]
  (when-not (statement-form? form)
    (error "expected statement, but was" form))
  (cond
   (instruction-form? form) (parse-instruction form)
   (label-form? form) (parse-label form)))

(defn defasm? [exp]
  (typed-map? exp :defasm))

(defn defasm-form? [form]
  (tagged-list? form 'defasm))

(defn make-defasm
  ([name statements]
     {:pre [(symbol? name) (every? statement? statements)]}
     (with-meta {:type :defasm :name name :statements statements}
       (merge {:definition? true}
              (when (:extern (meta name))
                {:extern? true}))))
  ([name statements form]
     {:pre [(defasm-form? form)]}
     (-> (make-defasm name statements)
         (vary-meta merge (meta form))
         (vary-meta assoc :form (with-meta form {})))))

(defn parse-defasm [form]
  (when-not (defasm-form? form)
    (error "expected defasm, but was" form))
  (let [arg-count (dec (count form))]
    (when (< arg-count 2)
      (error "defasm expects at least 2 arguments, but got" arg-count)))
  (let [[_ name & statements] form]
    (when-not (symbol-form? name)
      (error "defasm expects a symbol for name, but got" name))
    (when-not (instruction-form? (first statements))
      (error "defasm expects an instruction as its first statement, but got"
             (first statements)))
    (loop [prev (first statements)
           [statement & statements] (rest statements)]
      (when statement
        (when-not (statement-form? statement)
          (error "defasm expects a statement, but got" statement))
        (when (and (label-form? prev) (label-form? statement))
          (error "%label expects to be followed by an instruction, but got"
                 statement))
        (recur statement statements)))
    (make-defasm (parse-symbol name) (map parse-statement statements) form)))

(defn defimport? [exp]
  (typed-map? exp :defimport))

(defn defimport-form? [form]
  (tagged-list? form 'defimport))

(defn make-defimport
  ([name]
     {:pre [(symbol? name)]}
     (with-meta {:type :defimport :name name}
       {:definition? true
        :extern? true}))
  ([name form]
     {:pre [(defimport-form? form)]}
     (-> (make-defimport name)
         (vary-meta merge (meta form))
         (vary-meta assoc :form (with-meta form {})))))

(defn parse-defimport [form]
  (when-not (defimport-form? form)
    (error "expected defimport, but was" form))
  (let [arg-count (dec (count form))]
    (when (not= arg-count 1)
      (error "defimport expects exactly 1 argument, but got" arg-count)))
  (let [name (second form)]
    (when-not (symbol-form? name)
      (error "defimport expect a symbol for name, but got" name))
    (make-defimport (parse-symbol name) form)))

(defn top-level? [exp]
  (or (defasm? exp) (defimport? exp)))

(defn top-level-form? [form]
  (or (defasm-form? form) (defimport-form? form)))

(defn parse-top-level [form]
  (when-not (top-level-form? form)
    (error "expected top level form, but was" form))
  (cond
   (defasm-form? form) (parse-defasm form)
   (defimport-form? form) (parse-defimport form)))
