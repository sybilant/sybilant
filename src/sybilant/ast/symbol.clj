(ns sybilant.ast.symbol
  (:require
   [sybilant.ast.common :refer [typed-map]]
   [sybilant.ast.type :as ast.type]
   [sybilant.type :as-alias type]))

(set! *warn-on-reflection* true)

(def +type+
  (ast.type/make `type/symbol))

(defn make
  ([name]
   {:type +type+ :name name})
  ([namespace name]
   {:type +type+ :namespace namespace :name name}))

(def +value-regex+
  #"(?U)[^\p{Digit}\p{Space}\p{Cntrl}][^\p{Space}&&[^\p{Cntrl}]]{0,99}")

(defn valid-value
  [v]
  (and (string? v)
       (re-matches +value-regex+ ^String v)))

(defn qualified
  [o]
  (when-let [{:keys [namespace name]} (typed-map +type+ o)]
    (when (and (valid-value namespace)
               (valid-value name))
      o)))

(defn unqualified
  [o]
  (when-let [{:keys [namespace name]} (typed-map +type+ o)]
    (when (and (nil? namespace)
               (valid-value name))
      o)))

(defn valid
  [o]
  (or (unqualified o) (qualified o)))
