(ns sybilant.ast.defconst
  (:require
   [sybilant.ast :as-alias ast]
   [sybilant.ast.int :as ast.int]
   [sybilant.ast.label :as ast.label]))

(def ^:const +type+
  {:kind ::ast/type :name ::ast/defconst})

(defn make
  "Make defconst with name and value, or nil if name is not a valid label."
  [name value]
  (when (ast.label/valid? name)
    {:type +type+ :name name :value value}))

(defn type?
  "True if o is the defconst type."
  [o]
  (= +type+ o))

(defn valid?
  "True if o has the defconst type, name is a valid label, and value is a valid
  int."
  [o]
  (and (map? o)
       (type? (:type o))
       (ast.label/valid? (:name o))
       (ast.int/valid? (:value o))))
