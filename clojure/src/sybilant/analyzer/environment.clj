;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer.environment
  (:refer-clojure :exclude [defn resolve])
  (:require
   [schema.core :refer [defn Symbol]]))

(defn new
  []
  {})

(defn get-local
  [env name :- Symbol]
  (get-in env [:locals name]))

(defn assoc-locals
  [env locals]
  (cond-> env
    (not-empty locals) (assoc :locals locals)))

(defn locals
  [env]
  (:locals env))

(defn assoc-global
  [env name :- Symbol exp]
  (assoc-in env [:globals name] exp))

(defn get-global
  [env name :- Symbol]
  (get-in env [:globals name]))

(defn resolve
  [env name :- Symbol]
  (or (get-local env name)
      (get-global env name)))
