(ns sybilant.ast.int
  (:require
   [sybilant.ast :as-alias ast]
   [sybilant.ast.int.type :as ast.int.type]))

(defn make
  "Make int from value and type, or nil if value is not a valid int value. If type
  is not given, it is inferred."
  ([value]
   (make value nil))
  ([value type]
   (when (ast.int.type/value? value)
     (let [type (or type (ast.int.type/infer value))]
       {:value value :type type}))))

(defn type?
  "True if o is an int type."
  [o]
  (and (map? o) (= ::ast/int (:kind o))))

(defn valid?
  "True if o has a valid int type and a valid value for its type."
  [o]
  (and (map? o)
       (type? (:type o))
       (ast.int.type/valid? (:type o))
       (ast.int.type/value? (:value o))
       (<= (-> o :type :min) (:value o) (-> o :type :max))))
