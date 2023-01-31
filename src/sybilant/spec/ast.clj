(ns sybilant.spec.ast
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [sybilant.ast :as ast]
   [sybilant.ast.defconst :as ast.defconst]
   [sybilant.ast.defextern :as ast.defextern]
   [sybilant.ast.int :as ast.int]
   [sybilant.ast.label :as ast.label]
   [sybilant.ast.symbol :as ast.symbol]
   [sybilant.spec.ast.int]
   [sybilant.spec.ast.int.type :as spec.ast.int.type]
   [sybilant.spec.ast.symbol]))

(s/def ::ast/int
  (s/with-gen ast.int/valid?
    (fn int-gen
      []
      (gen/bind
       (gen/one-of [(gen/return nil)
                    (s/gen ::ast.int/type)])
       (fn int-gen-type
         [type]
         (gen/fmap
          (fn int-gen-value-type
            [value]
            (ast.int/make value type))
          (s/gen (if (and (:min type) (:max type))
                   (spec.ast.int.type/value (:min type) (:max type))
                   (spec.ast.int.type/value)))))))))

(s/def ::ast/symbol
  (s/with-gen ast.symbol/valid?
    (fn symbol-gen
      []
      (gen/fmap
       (fn symbol-gen-value
         [value]
         (ast.symbol/make value))
       (s/gen ::ast.symbol/value)))))

(s/def ::ast/label
  (s/with-gen ast.label/valid?
    (fn label-gen
      []
      (gen/fmap
       (fn label-gen-name
         [name]
         (ast.label/make name))
       (s/gen ::ast/symbol)))))

(s/def ::ast/defconst
  (s/with-gen ast.defconst/valid?
    (fn defconst-gen
      []
      (gen/fmap
       (fn defconst-gen-name-and-value
         [[name value]]
         (ast.defconst/make name value))
       (gen/tuple (s/gen ::ast/label)
                  (s/gen ::ast/int))))))

(s/def ::ast/defextern
  (s/with-gen ast.defextern/valid?
    (fn defextern-gen
      []
      (gen/fmap
       (fn defextern-gen-name
         [name]
         (ast.defextern/make name))
       (s/gen ::ast/label)))))
