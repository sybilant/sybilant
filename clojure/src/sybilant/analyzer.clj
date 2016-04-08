;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer
  (:refer-clojure :exclude [defn])
  (:require
   [schema.core :refer [Bool defn defschema pred]]
   [sybilant.ast :as ast]
   [sybilant.ast.zip :as zip]
   [sybilant.analyzer.environment :as env]))

(defn atom? :- Bool
  [obj]
  (instance? clojure.lang.IAtom obj))

(defschema Atom
  (pred atom? 'atom?))

(defn collect-locals
  [exp env :- Atom]
  (let [locals (atom {})]
    (letfn
        [(collect-locals
           [exp]
           (when (ast/label? exp)
             (swap! locals assoc (:name exp) exp))
           exp)]
      (zip/dfs-visit exp collect-locals))
    (vary-meta exp assoc :locals @locals)))

(defn check-symbols
  [exp env :- Atom]
  (let [env (atom (env/assoc-locals @env (:locals (meta exp))))]
    (letfn
        [(check-symbols
           [exp]
           (when (symbol? exp)
             (when-not (env/resolve @env exp)
               (throw (ex-info (str "Could not resolve '" exp "'")
                               {:error :undefined-symbol :symbol exp}))))
           exp)]
      (zip/dfs-visit exp check-symbols)))
  exp)

(defn analyze
  [exp env :- Atom]
  (let [exp (collect-locals exp env)
        exp (check-symbols exp env)]
    exp))
