;; Copyright © 2024 Paul Stadig
;;
;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
;; of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public
;; License, v. 2.0.
(ns sybilant.compiler
  "A whole program compiler for Sybilant.  It compiles one or more files into a single assembly file
  that can be assembled and linked with external programs."
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]))

(defn register?
  [operand]
  (contains? '#{rdi eax ebx} operand))

(defn emit-operand
  [[env operands] operand]
  [env
   (cond
     (number? operand)
     (conj operands (format "$%s" operand))
     (register? operand)
     (conj operands (format "%%%s" operand))
     (symbol? operand)
     (let [o (get env operand)]
       (if-let [cv (:const-value o)]
         ;; const should not modify env
         (second (emit-operand [env operands] cv))
         (if o
           (conj operands (format "%s" operand))
           (throw (ex-info "Unknown symbol" {:symbol operand}))))))])

(defn emit-operands
  [[env instruction] operands]
  (when (seq operands)
    (let [[env' o] (reduce emit-operand [env []] operands)]
      [env' (format "%s %s" instruction (str/join ", " o))])))

(defn emit-statement
  [[env instructions] [op & operands]]
  (let [[env' instruction] (emit-operands [env (str op)] operands)]
    [env' (conj instructions instruction)]))

(defn exp-type
  [exp]
  (first exp))

(defmulti emit-exp
  "Emit exp as a sequence of assembly statements."
  (fn [_ exp] (exp-type exp)))

(defmethod emit-exp 'sybilant/defconst
  [[env instructions] [_ [_ sym] value]]
  [(assoc env sym {:type :defconst :const-value value})
   ;; TODO: emit something here
   instructions])

(defmethod emit-exp 'sybilant.x86-64/deftext
  [[env instructions] [_ [_ sym] & statements]]
  (let [env' (assoc env sym {:type :deftext})]
    (if-let [statements (seq statements)]
      (reduce
        emit-statement
        [env'
         (conj instructions
           ".text"
           (format ".global %s" sym)
           (format "%s:" sym))]
        statements)
      [env'
       (conj instructions
         ".text"
         (format ".extern %s" sym))])))

(defn compile-forms
  "Compile forms into a sequence of assembly statements."
  [forms]
  (second (reduce emit-exp [{} []] forms)))
