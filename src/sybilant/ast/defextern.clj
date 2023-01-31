(ns sybilant.ast.defextern
  (:require
   [sybilant.ast :as-alias ast]
   [sybilant.ast.label :as ast.label]))

(def ^:const +type+
  {:kind ::ast/type :name ::ast/defextern})

(defn make
  "Make defextern with name, or nil if name is not a valid label."
  [name]
  (when (ast.label/valid? name)
    {:type +type+ :name name}))

(defn type?
  "True if o is the defextern type."
  [o]
  (= +type+ o))

(defn valid?
  "True if o has the defextern type and  name is a valid label."
  [o]
  (and (map? o)
       (type? (:type o))
       (ast.label/valid? (:name o))))
