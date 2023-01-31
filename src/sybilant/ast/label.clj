(ns sybilant.ast.label
  (:require
   [sybilant.ast :as-alias ast]
   [sybilant.ast.symbol :as ast.symbol]))

(def ^:const +type+
  {:kind ::ast/type :name ::ast/label})

(defn make
  "Make label with name, or nil if name is not a valid symbol."
  [name]
  (when (ast.symbol/valid? name)
    {:type +type+ :name name}))

(defn type?
  "True if o is the label type."
  [o]
  (= +type+ o))

(defn valid?
  "True if o has the label type and a valid name."
  [o]
  (and (map? o)
       (type? (:type o))
       (ast.symbol/valid? (:name o))))
