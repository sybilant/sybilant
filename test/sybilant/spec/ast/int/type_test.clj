(ns sybilant.spec.ast.int.type-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :refer [for-all]]
   [sybilant.spec.ast.int.type :as spec.ast.int.type]))

(defspec t-value
  (for-all
   [i (s/gen (spec.ast.int.type/value))]
   (some? i)))

(defspec t-value-range
  (for-all
   [i (s/gen (spec.ast.int.type/value -1 100))]
   (<= -1 i 100)))
