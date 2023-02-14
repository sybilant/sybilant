(ns sybilant.generators.ast
  (:require
   [clojure.test.check.generators :as gen]
   [sybilant.ast.symbol :as symbol]
   [sybilant.kind :as-alias kind]))

(def type-gen
  (gen/hash-map
   :kind (gen/return ::kind/type)
   :name gen/symbol-ns))

(defn string-gen
  [min max]
  (gen/fmap
   (fn string-gen
     [code-points]
     (let [sb (StringBuffer.)]
       (doseq [code-point code-points]
         (.appendCodePoint sb code-point))
       (.toString sb)))
   (gen/vector (gen/choose Character/MIN_CODE_POINT Character/MAX_CODE_POINT)
               min max)))

(def symbol-value-gen
  (gen/such-that symbol/valid-value (string-gen 1 100)))

(def qualified-symbol-gen
  (gen/hash-map
   :type (gen/return symbol/+type+)
   :namespace symbol-value-gen
   :name symbol-value-gen))

(def unqualified-symbol-gen
  (gen/hash-map
   :type (gen/return symbol/+type+)
   :name symbol-value-gen))

(def symbol-gen
  (gen/one-of [unqualified-symbol-gen qualified-symbol-gen]))

(def exp-gen
  (gen/one-of
   [type-gen
    symbol-gen]))
