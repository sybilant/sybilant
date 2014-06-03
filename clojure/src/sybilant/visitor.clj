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

(def branch-types #{:deftext :defdata :label :instruction :mem})

(def branch? (comp branch-types #(get-in % [:type :name])))

(defn dispatch-fn [exp] (get-in exp [:type :name]))

(defmulti children dispatch-fn)
(defmethod children :deftext
  [exp]
  (cons (:label exp) (:statements exp)))

(defmethod children :defdata
  [exp]
  (cons (:label exp) (:values exp)))

(defmethod children :label
  [exp]
  [(:name exp)])

(defmethod children :instruction
  [exp]
  (cons (:operator exp) (:operands exp)))

(defmethod children :mem
  [exp]
  ((juxt :base :index :scale :disp) exp))

(defmulti make-node (comp dispatch-fn first list))
(defmethod make-node :deftext
  [exp [label & statements]]
  (assoc exp :label label :statements statements))

(defmethod make-node :defdata
  [exp [label & values]]
  (assoc exp :label label :values values))

(defmethod make-node :label
  [exp [name]]
  (assoc exp :name name))

(defmethod make-node :instruction
  [exp [operator & operands]]
  (assoc exp :operator operator :operands operands))

(defmethod make-node :mem
  [exp [base index scale disp]]
  (merge exp
         (when base
           {:base base})
         (when index
           {:index index})
         (when scale
           {:scale scale})
         (when disp
           {:disp disp})))

(defn dfs-visit
  [exp f & args]
  (loop [loc (zip/zipper branch? children make-node exp)]
    (if-not (zip/end? loc)
      (recur (zip/next (apply zip/edit
                              loc
                              (fn [node & args]
                                (when node
                                  (apply f node args)))
                              args)))
      (zip/node loc))))

(defn visit-property
  ([exp key mf]
     (dfs-visit exp (fn [exp] (vary-meta exp assoc key (mf exp)))))
  ([exp key mf rf]
     (if (branch? exp)
       (let [exp (make-node exp (for [child (children exp)]
                                  (visit-property child key mf rf)))]
         (vary-meta exp assoc key (reduce (fn [result child]
                                            (rf result (get (meta child) key)))
                                          (mf exp)
                                          (children exp))))
       (when exp
         (vary-meta exp assoc key (mf exp))))))
