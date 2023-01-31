(ns sybilant.spec.ast.int-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :refer [for-all]]
   [sybilant.ast.int :as ast.int]
   [sybilant.spec.ast.int]))

(defspec t-type
  (for-all
   [i (s/gen ::ast.int/type)]
   (some? i)))
