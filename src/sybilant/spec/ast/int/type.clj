(ns sybilant.spec.ast.int.type
  (:refer-clojure :exclude [min max])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [sybilant.ast :as-alias ast]
   [sybilant.ast.int.type :as ast.int.type]))

(defn value
  ([]
   (value ast.int.type/+value-min+ ast.int.type/+value-max+))
  ([min max]
   (let [hmin (quot min 2)
         hmax (quot max 2)]
     (s/with-gen ast.int.type/value?
       (fn int-value-gen
         []
         (gen/fmap
          (fn [[a b]] (+' a b))
          (gen/tuple
           (gen/large-integer*
            {:min hmin :max hmax})
           (gen/large-integer*
            {:min (if (odd? min) ((if (neg? min) dec' inc') hmin) hmin)
             :max (if (odd? max) ((if (neg? max) dec' inc') hmax) hmax)}))))))))

(s/def ::ast.int.type/kind
  #{::ast/int})

(s/def ::ast.int.type/signedness
  ast.int.type/+signednesses+)

(s/def ::ast.int.type/width
  ast.int.type/+widths+)

(s/def ::ast.int.type/min
  (value))

(s/def ::ast.int.type/max
  (value))
