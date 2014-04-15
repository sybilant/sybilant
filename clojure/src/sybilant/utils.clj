;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.utils
  (:require [clojure.string :as str]))

(defn error
  [& msg]
  (throw (Exception. (str/join " " msg))))

(defn form
  [obj]
  (or (:form (meta obj))
      (:form obj)
      obj))

(defn width
  [obj]
  (or (:width (:type obj))
      (:width obj)))

(defn typed-map?
  [t obj]
  (and (map? obj) (= t (:type obj))))

(defn type?
  ([obj]
     (typed-map? :type obj))
  ([name obj]
     (and (type? obj)
          (= name (:name obj)))))

(defn tagged-list?
  [t obj]
  (and (list? obj) (= t (first obj))))

(defn maybe
  [pred]
  (fn [x]
    (or (nil? x) (pred x))))

(defn implies
  [p q]
  (or (not p) q))

(defn oxford
  ([l]
     (oxford l "or"))
  ([l c]
     (case (count l)
       0 ""
       1 (str (first l))
       2 (str (first l) " " c " " (second l))
       (str (str/join ", " (butlast l)) ", " c " " (last l)))))
