(ns sybilant.spec.ast.symbol
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [sybilant.ast.symbol :as ast.symbol]))

(s/def ::ast.symbol/code-point
  (s/with-gen ast.symbol/code-point?
    (fn code-point-gen
      []
      (gen/choose Character/MIN_CODE_POINT Character/MAX_CODE_POINT))))

(s/def ::ast.symbol/value-start
  (s/with-gen ast.symbol/value-start?
    (fn symbol-start-gen
      []
      (s/gen ::ast.symbol/code-point))))

(s/def ::ast.symbol/code-points
  (s/coll-of ::ast.symbol/code-point
             :min-count 0
             :max-count (dec ast.symbol/+value-length-max+)))

(s/def ::ast.symbol/value
  (s/with-gen ast.symbol/value?
    (fn symbol-gen
      []
      (gen/fmap
       (fn symbol-gen-start-code-points
         [[start code-points]]
         (let [sb (StringBuffer. (inc (count code-points)))]
           (.appendCodePoint sb start)
           (doseq [cp code-points]
             (.appendCodePoint sb cp))
           (.toString sb)))
       (gen/tuple (s/gen ::ast.symbol/value-start)
                  (s/gen ::ast.symbol/code-points))))))
