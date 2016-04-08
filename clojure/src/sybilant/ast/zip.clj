;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.ast.zip
  (:refer-clojure :exclude [defmethod defn])
  (:require
   [clojure.zip :as zip]
   [schema.core :refer [Bool defmethod defn Keyword maybe]]
   [sybilant.ast :as ast]))

(defn node-type :- (maybe Keyword)
  [exp]
  (cond
    (ast/defimport? exp) :defimport
    (ast/defconst? exp) :defconst
    (ast/defdata? exp) :defdata
    (ast/deftext? exp) :deftext
    (ast/instruction? exp) :instruction
    (ast/address? exp) :address))

(defn branch? :- Bool
  [exp]
  (some? (node-type exp)))

(defmulti children
  node-type)

(defmethod children :defimport
  [{:keys [label]} :- ast/Defimport]
  [label])

(defmethod children :defconst
  [{:keys [name value]} :- ast/Defconst]
  [name value])

(defmethod children :defdata
  [{:keys [label value]} :- ast/Defdata]
  (cons label value))

(defmethod children :deftext
  [{:keys [label statements]} :- ast/Deftext]
  (cons label statements))

(defmethod children :instruction
  [{:keys [operands]} :- ast/Instruction]
  operands)

(defmethod children :address
  [{:keys [base index scale disp]} :- ast/Address]
  [base index scale disp])

(defmulti make-node
  (fn [node children]
    (node-type node)))

(defmethod make-node :defimport :- ast/Label
  [node :- ast/Label [label]]
  (assoc node :label label))

(defmethod make-node :defconst :- ast/Defconst
  [node :- ast/Defconst [name value]]
  (cond-> (assoc (dissoc node :value) :name name)
    value (assoc :value value)))

(defmethod make-node :defdata :- ast/Defdata
  [node :- ast/Defdata [label & value]]
  (cond-> (assoc (dissoc node :value) :label label)
    value (assoc :value value)))

(defmethod make-node :deftext :- ast/Deftext
  [node :- ast/Deftext [label & statements]]
  (cond-> (assoc (dissoc node :statements) :label label)
    (seq statements) (assoc :statements statements)))

(defmethod make-node :instruction :- ast/Instruction
  [node :- ast/Instruction operands]
  (if (seq operands)
    (assoc node :operands operands)
    (dissoc node :operands)))

(defmethod make-node :address :- ast/Address
  [node :- ast/Address [base index scale disp]]
  (cond-> (dissoc node :base :index :scale :disp)
    base (assoc :base base)
    index (assoc :index index)
    scale (assoc :scale scale)
    disp (assoc :disp disp)))

(defn zipper
  [exp]
  (zip/zipper branch? children make-node exp))

(defn dfs-visit
  [exp f & args]
  (-> (loop [loc (zipper exp)]
        (if (zip/end? loc)
          loc
          (recur (zip/next (apply zip/edit loc f args)))))
      zip/root))
