(ns sybilant.ast
  (:refer-clojure :exclude [symbol type])
  (:require
   [sybilant.ast.common :refer [kinded-map typed-map]]
   [sybilant.ast.symbol :as ast.symbol]
   [sybilant.ast.type :as ast.type]
   [sybilant.kind :as-alias kind]))

(defn type
  "nil if o is not a type, otherwise o"
  [o]
  (kinded-map ::kind/type o))

(defn symbol
  "nil if o is not a symbol, otherwise o"
  [o]
  (typed-map ast.symbol/+type+ o))

(defn valid
  "nil if exp is not a valid expression, otherwise o"
  [exp]
  (cond
    (type exp) (ast.type/valid exp)
    :else      (ast.symbol/valid exp)))
