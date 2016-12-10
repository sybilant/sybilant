;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer.syntax
  (:refer-clojure :exclude [defn])
  (:require
   [schema.core :refer [Bool defn]]
   [sybilant.ast]
   [sybilant.ast.zip :refer [dfs-visit]])
  (:import
   (clojure.lang Symbol)
   (sybilant.ast AddressNode DefconstNode DefdataNode DefimportNode DeftextNode
                 IntNode InstructionNode IntTagNode LabelNode OperatorNode
                 RegisterNode TextTagNode TupleTagNode)))

(defn long-symbol? :- Bool
  [sym :- Symbol]
  (> (count (name sym)) 31))

(defn symbol-too-long
  [symbol :- Symbol]
  (ex-info (str "Symbol is too long '" symbol "'")
           (merge {:sybilant/error :long-symbol
                   :sybilant/symbol symbol}
                  (select-keys (meta symbol) [:file :line :column]))))

(defn valid-symbol? :- Bool
  [sym :- Symbol]
  (and (nil? (namespace sym))
       (boolean (re-matches #"[_a-z][_a-zA-Z0-9]*" (name sym)))))

(defn symbol-invalid
  [symbol :- Symbol]
  (ex-info (str "Symbol is invalid '" symbol "'")
           (merge {:sybilant/error :invalid-symbol
                   :sybilant/symbol symbol}
                  (select-keys (meta symbol) [:file :line :column]))))

(defn invalid-tag
  [reg tag]
  (ex-info (str tag " is incompatible with " reg)
           (merge {:sybilant/error :invalid-tag
                   :sybilant/register reg
                   :sybilant/tag tag}
                  (select-keys (meta tag) [:file :line :column]))))

(defn validate-symbol
  [exp :- Symbol]
  (when (long-symbol? exp)
    (throw (symbol-too-long exp)))
  (when-not (valid-symbol? exp)
    (throw (symbol-invalid exp))))

(defprotocol ICheckExpSyntax
  (check-exp-syntax [exp env]))

(extend-protocol ICheckExpSyntax
  AddressNode
  (check-exp-syntax [exp env]
    exp)
  DefconstNode
  (check-exp-syntax [exp env]
    exp)
  DefdataNode
  (check-exp-syntax [exp env]
    exp)
  DefimportNode
  (check-exp-syntax [exp env]
    exp)
  DeftextNode
  (check-exp-syntax [exp env]
    exp)
  IntNode
  (check-exp-syntax [exp env]
    exp)
  InstructionNode
  (check-exp-syntax [exp env]
    exp)
  IntTagNode
  (check-exp-syntax [exp env]
    exp)
  LabelNode
  (check-exp-syntax [exp env]
    (validate-symbol (:name exp))
    (when (:tag exp)
      (check-exp-syntax (:tag exp) env))
    exp)
  OperatorNode
  (check-exp-syntax [exp env]
    exp)
  RegisterNode
  (check-exp-syntax [exp env]
    exp)
  TextTagNode
  (check-exp-syntax [exp env]
    (doseq [[reg tag] (:tags exp)]
      (if (not= (:width reg) (:width tag))
        (throw (invalid-tag reg tag))))
    exp)
  TupleTagNode
  (check-exp-syntax [exp env]
    exp)
  Symbol
  (check-exp-syntax [exp env]
    (validate-symbol exp)
    exp))

(defprotocol ICheckSyntax
  (check-syntax [exp env]))

(extend-protocol ICheckSyntax
  DefconstNode
  (check-syntax [exp env]
    (dfs-visit exp check-exp-syntax env))
  DefdataNode
  (check-syntax [exp env]
    (dfs-visit exp check-exp-syntax env))
  DefimportNode
  (check-syntax [exp env]
    (dfs-visit exp check-exp-syntax env))
  DeftextNode
  (check-syntax [exp env]
    (dfs-visit exp check-exp-syntax env)))
