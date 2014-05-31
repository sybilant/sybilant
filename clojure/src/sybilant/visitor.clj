;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.visitor
  (:require [clojure.zip :as zip]))

(def branch-types #{:deftext :defdata :instruction})

(def branch? (comp branch-types #(get-in % [:type :name])))

(defn dispatch-fn [exp] (get-in exp [:type :name]))

(defmulti children dispatch-fn)
(defmethod children :deftext
  [exp]
  (cons (:label exp) (:statements exp)))

(defmethod children :defdata
  [exp]
  (cons (:label exp) (:values exp)))

(defmethod children :instruction
  [exp]
  (cons (:operator exp) (:operands exp)))

(defmulti make-node (comp dispatch-fn first list))
(defmethod make-node :deftext
  [exp [label & statements]]
  (assoc exp :label label :statements statements))

(defmethod make-node :defdata
  [exp [label & values]]
  (assoc exp :label label :values values))

(defmethod make-node :instruction
  [exp [operator & operands]]
  (assoc exp :operator operator :operands operands))

(defn dfs-visit
  [exp f & args]
  (loop [loc (zip/zipper branch? children make-node exp)]
    (if-not (zip/end? loc)
      (recur (zip/next (apply zip/edit loc f args)))
      (zip/node loc))))
