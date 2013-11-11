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
            [sybilant.parser :refer :all]))

(def ^:dynamic *symbol-table* (atom {}))

(defn check-symbol-reference [exp]
  (when (symbol? exp)
    (when (not (contains? @*symbol-table* exp))
      (error exp "is undefined")))
  exp)

(defn check-symbol-format [exp]
  (when (symbol? exp)
    (when (not (re-matches #"^[a-zA-Z_][a-zA-Z0-9_]*$" (str (:form exp))))
      (error exp "is an invalid symbol")))
  exp)

(defn definition? [exp]
  (:definition? (meta exp)))

(defn defined? [exp]
  (contains? @*symbol-table* exp))

(defn define [exp]
  {:pre [(definition? exp)]}
  (when (defined? (:name exp))
    (error (:name exp) "is already defined"))
  (swap! *symbol-table*
         assoc (:name exp) (select-keys exp [:type :name])))

(defn analyze [exp]
  (when (definition? exp)
    (define exp))
  (visit exp (comp check-symbol-format
                   check-symbol-reference)))
