(ns sybilant.spec.ast.int
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [sybilant.ast :as-alias ast]
   [sybilant.ast.int :as ast.int]
   [sybilant.ast.int.type :as ast.int.type]
   [sybilant.spec.ast.int.type :as spec.ast.int.type]))

(s/def ::ast.int/type
  (s/with-gen
    (s/and (s/keys :req-un [::ast.int.type/kind
                            ::ast.int.type/signedness
                            ::ast.int.type/width
                            ::ast.int.type/min
                            ::ast.int.type/max])
           ast.int.type/valid?)
    (fn int-type-gen
      []
      (gen/bind
       (gen/tuple (s/gen ::ast.int.type/signedness)
                  (s/gen ::ast.int.type/width))
       (fn int-type-gen-signedness-width
         [[signedness width]]
         (let [tmin (ast.int.type/min signedness width)
               tmax (ast.int.type/max signedness width)]
           (gen/fmap
            (fn int-type-gen-min-max
              [[a b]]
              (let [[min max] (if (< a b) [a b] [b a])]
                {:kind ::ast/int
                 :signedness signedness
                 :width width
                 :min min
                 :max max}))
            (gen/tuple (s/gen (spec.ast.int.type/value tmin tmax))
                       (s/gen (spec.ast.int.type/value tmin tmax))))))))))
