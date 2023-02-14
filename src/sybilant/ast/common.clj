(ns sybilant.ast.common)

(defn kinded-map
  "nil if m is not a kinded map of kind k, otherwise m"
  [k m]
  (when (and (map? m) (= k (:kind m)))
    m))

(defn typed-map
  "nil if m is not a typed map of type t, otherwise m"
  [t m]
  (when (and (map? m) (= t (:type m)))
    m))
