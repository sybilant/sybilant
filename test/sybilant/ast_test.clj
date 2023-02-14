(ns sybilant.ast-test
  (:require
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :refer [for-all]]
   [sybilant.generators.ast :as ast.gen]
   [sybilant.ast :as ast]))

(defspec t-type-is-valid
  (for-all
   [type ast.gen/type-gen]
   (ast/valid type)))

(defspec t-symbol-is-valid
  (for-all
   [type ast.gen/symbol-gen]
   (ast/valid type)))

(defspec t-exp-is-valid
  (for-all
   [exp ast.gen/exp-gen]
   (ast/valid exp)))
