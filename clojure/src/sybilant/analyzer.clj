;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer
  (:refer-clojure :exclude [number? symbol?])
  (:require [sybilant.visitor :refer [visit]]
            [sybilant.parser :refer :all]
            [sybilant.util :refer [error]]))

(def ^:dynamic *globals* (atom {}))

(defn check-symbol-reference [exp]
  (when (symbol? exp)
    (when-not (contains? (merge (:globals (meta exp))
                                (:locals (meta exp)))
                         exp)
      (error exp "is undefined")))
  exp)

(defn check-symbol-format [exp]
  (when (symbol? exp)
    (when (not (re-matches #"^[a-zA-Z_][a-zA-Z0-9_]*$" (str (:form exp))))
      (error exp "is an invalid symbol")))
  exp)

(defn add-symbol-table-entry [atom exp]
  (swap! atom assoc (:name exp) (select-keys exp [:type :name])))

(defn definition? [exp]
  (:definition? (meta exp)))

(defn populate-symbol-table [exp]
  (let [locals (atom {})]
    (-> exp
        (visit (fn [exp]
                 (when (definition? exp)
                   (when (contains? @locals (:name exp))
                     (error (:name exp) "is already defined"))
                   (add-symbol-table-entry locals exp))
                 (vary-meta exp assoc :globals @*globals*)))
        (visit (fn [exp]
                 (vary-meta exp assoc :locals @locals))))))

(defn global-defined? [exp]
  (contains? @*globals* exp))

(defn define-global [exp]
  {:pre [(definition? exp)]}
  (when (global-defined? (:name exp))
    (error (:name exp) "is already defined"))
  (add-symbol-table-entry *globals* exp))

(defn analyze [exp]
  {:pre [(top-level? exp)]}
  (let [exp (-> (populate-symbol-table exp)
                (visit (comp check-symbol-format
                             check-symbol-reference)))]
    (when (definition? exp)
      (define-global exp))
    exp))
