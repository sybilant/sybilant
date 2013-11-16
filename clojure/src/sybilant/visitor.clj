;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.visitor
  (:require [clojure.zip :as z]
            [sybilant.util :refer [error]]))

(defn branch? [exp]
  (contains? #{:mem :instruction :label :defasm :defimport :defdata}
             (:type exp)))

(defmulti children (comp :type first list))
(defmethod children :mem [{:keys [base index scale disp]}]
  [base index scale disp])
(defmethod children :instruction [{:keys [operator operands]}]
  (cons operator operands))
(defmethod children :label [{:keys [name]}]
  [name])
(defmethod children :defasm [{:keys [name statements]}]
  (cons name statements))
(defmethod children :defimport [{:keys [name]}]
  [name])
(defmethod children :defdata [{:keys [name values]}]
  (cons name values))
(defmethod children :default [exp]
  (error "not a branch node" exp))

(defmulti make-node (comp :type first list))
(defmethod make-node :mem [exp [base index scale disp]]
  (merge exp
         (when base
           {:base base})
         (when index
           {:index index})
         (when scale
           {:scale scale})
         (when disp
           {:disp disp})))
(defmethod make-node :instruction [exp [operator & operands]]
  (assoc exp :operator operator :operands operands))
(defmethod make-node :label [exp [name]]
  (assoc exp :name name))
(defmethod make-node :defasm [exp [name & statements]]
  (assoc exp :name name :statements statements))
(defmethod make-node :defimport [exp [name]]
  (assoc exp :name name))
(defmethod make-node :defdata [exp [name & values]]
  (assoc exp :name name :values values))
(defmethod make-node :default [exp children]
  (error "not a branch node" exp))

(defn visit [exp visitor]
  (let [zip (z/zipper branch? children make-node exp)]
    (loop [loc zip]
      (if-not (z/end? loc)
        (recur (z/next (z/edit loc visitor)))
        (z/root loc)))))
