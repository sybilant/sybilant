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
   [schema.core :refer [Bool defn defschema pred Symbol]]
   [sybilant.ast :as ast]
   [sybilant.ast.zip :as zip]
   [sybilant.analyzer.environment :as env]))

(defn atom? :- Bool
  [obj]
  (instance? clojure.lang.IAtom obj))

(defschema Atom
  (pred atom? 'atom?))

(defn long-symbol? :- Bool
  [sym :- Symbol]
  (> (count (name sym)) 31))

(defn valid-symbol? :- Bool
  [sym :- Symbol]
  (and (nil? (namespace sym))
       (boolean (re-matches #"[_a-z][_a-zA-Z0-9]{0,30}" (name sym)))))

(defn duplicate-definition
  [symbol previous-definition]
  (ex-info (str "Duplicate symbol '" symbol "'")
           {:sybilant/error :duplicate-definition
            :sybilant/symbol symbol
            :sybilant/previous-definition previous-definition}))

(defn symbol-too-long
  [symbol]
  (ex-info (str "Symbol is too long '" symbol "'")
           {:sybilant/error :long-symbol
            :sybilant/symbol symbol}))

(defn symbol-invalid
  [symbol]
  (ex-info (str "Symbol is invalid '" symbol "'")
           {:sybilant/error :invalid-symbol
            :sybilant/symbol symbol}))

(defn collect-locals
  [exp env :- Atom]
  (let [locals (atom {})]
    (letfn
        [(collect-locals
           [exp]
           (when (ast/label? exp)
             (let [label-name (:name exp)]
               (when-let [previous-definition (get @locals label-name)]
                 (throw (duplicate-definition label-name previous-definition)))
               (when (long-symbol? label-name)
                 (throw (symbol-too-long label-name)))
               (when-not (valid-symbol? label-name)
                 (throw (symbol-invalid label-name)))
               (swap! locals assoc (:name exp) exp)))
           exp)]
      (zip/dfs-visit exp collect-locals))
    (vary-meta exp assoc :locals @locals)))

(defn undefined-symbol
  [symbol]
  (ex-info (str "Could not resolve '" symbol "'")
           {:sybilant/error :undefined-symbol
            :sybilant/symbol symbol}))

(defn check-symbols
  [exp env :- Atom]
  (let [env (atom (env/assoc-locals @env (:locals (meta exp))))]
    (letfn
        [(check-symbols
           [exp]
           (when (symbol? exp)
             (when-not (env/resolve @env exp)
               (throw (undefined-symbol exp))))
           exp)]
      (zip/dfs-visit exp check-symbols))))

(defn definition? :- Bool
  [exp]
  (or (ast/defimport? exp)
      (ast/defconst? exp)
      (ast/defdata? exp)
      (ast/deftext? exp)))

(defn definition-name :- Symbol
  [exp]
  (get-in exp [:label :name]))

(defn define-globals
  [exp env :- Atom]
  (letfn
      [(define-globals
         [exp]
         (when (definition? exp)
           (let [name (definition-name exp)]
             (if-let [previous-definition (env/get-global @env name)]
               (throw (duplicate-definition name previous-definition))
               (swap! env env/assoc-global name exp))))
         exp)]
    (zip/dfs-visit exp define-globals)))

(defn analyze
  [exp env :- Atom]
  (let [exp (collect-locals exp env)
        exp (check-symbols exp env)
        exp (define-globals exp env)]
    exp))
