(ns sybilant.ast
  (:refer-clojure :exclude [int? symbol?])
  (:require
   [sybilant.ast.defconst :as ast.defconst]
   [sybilant.ast.defextern :as ast.defextern]
   [sybilant.ast.int :as ast.int]
   [sybilant.ast.label :as ast.label]
   [sybilant.ast.symbol :as ast.symbol]))

(defn int?
  "True if o has type int."
  [o]
  (and (map? o) (ast.int/type? (:type o))))

(defn symbol?
  "True if o has type symbol."
  [o]
  (and (map? o) (ast.symbol/type? (:type o))))

(defn label?
  "True if o has type label."
  [o]
  (and (map? o) (ast.label/type? (:type o))))

(defn defconst?
  "True if o has defconst type."
  [o]
  (and (map? o) (ast.defconst/type? (:type o))))

(defn defextern?
  "True if o has defextern type."
  [o]
  (and (map? o) (ast.defextern/type? (:type o))))
