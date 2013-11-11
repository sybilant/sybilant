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
  (contains? #{:instruction :defasm :defextern} (:type exp)))

(defmulti children (comp :type first list))
(defmethod children :instruction [{:keys [operator operands]}]
  (cons operator operands))
(defmethod children :defasm [{:keys [name statements]}]
  (cons name statements))
(defmethod children :defextern [{:keys [name]}]
  [name])
(defmethod children :default [exp]
  (error "not a branch node" exp))

(defmulti make-node (comp :type first list))
(defmethod make-node :instruction [exp [operator & operands]]
  (assoc exp :operator operator :operands operands))
(defmethod make-node :defasm [exp [name & statements]]
  (assoc exp :name name :statements statements))
(defmethod make-node :defextern [exp [name]]
  (assoc exp :name name))
(defmethod make-node :default [exp children]
  (error "not a branch node" exp))

(defn visit [exp visitor]
  (let [zip (z/zipper branch? children make-node exp)]
    (loop [loc zip]
      (if-not (z/end? loc)
        (recur (z/next (z/edit loc visitor)))
        (z/root loc)))))
