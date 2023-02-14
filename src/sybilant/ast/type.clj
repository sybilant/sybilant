(ns sybilant.ast.type
  (:require
   [sybilant.ast.common :refer [kinded-map]]
   [sybilant.kind :as-alias kind]))

(defn make
  [name]
  {:kind ::kind/type :name name})

(defn valid
  [o]
  (when-let [{:keys [name]} (kinded-map ::kind/type o)]
    (when (qualified-symbol? name)
      o)))
