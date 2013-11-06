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
            [sybilant.compile :refer [die]]))

(defn form [obj]
  (if (string? obj)
    obj
    (pr-str (or (:form (meta obj)) (:form obj) obj))))

(defn error [& msg]
  (apply die 2 (map form msg)))

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
  (clj/integer? form))

(defn make-number [form]
  {:pre [(number-form? form)]}
  {:type :number :form form})

(defn parse-number [form]
  (when-not (number-form? form)
    (error "expected number, but was" form))
  (make-number form))

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
  (or (number? exp) (register? exp) (symbol? exp)))

(defn operand-form? [form]
  (or (number-form? form) (register-form? form) (symbol-form? form)))

(defn parse-operand [form]
  (when-not (operand-form? form)
    (error "expected operand, but was" form))
  (cond
   (number-form? form) (parse-number form)
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
     {:type :defasm :name name :statements statements})
  ([name statements form]
     {:pre [(defasm-form? form)]}
     (with-meta (make-defasm name statements)
       (assoc (meta form) :form (with-meta form {})))))

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
     {:type :defextern :name name})
  ([name form]
     {:pre [(defextern-form? form)]}
     (with-meta (make-defextern name)
       (assoc (meta form) :form (with-meta form {})))))

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
