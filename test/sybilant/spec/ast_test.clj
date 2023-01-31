(ns sybilant.spec.ast-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :refer [for-all]]
   [sybilant.ast :as ast]
   [sybilant.spec.ast]))

(defspec t-int
  (for-all
   [i (s/gen ::ast/int)]
   (ast/int? i)))

(defspec t-symbol
  (for-all
   [i (s/gen ::ast/symbol)]
   (ast/symbol? i)))

(defspec t-label
  (for-all
   [i (s/gen ::ast/label)]
   (ast/label? i)))

(defspec t-defconst
  (for-all
   [i (s/gen ::ast/defconst)]
   (ast/defconst? i)))

(defspec t-defextern
  (for-all
   [i (s/gen ::ast/defextern)]
   (ast/defextern? i)))
