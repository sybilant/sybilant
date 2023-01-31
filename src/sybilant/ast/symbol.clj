(ns sybilant.ast.symbol
  (:require
   [sybilant.ast :as-alias ast]))

(defn code-point?
  "True if o is a valid code point."
  [o]
  (and (integer? o)
       (Character/isValidCodePoint o)))

(defn value-start?
  "True if o is a valid code point to start a symbol value."
  [o]
  (and (code-point? o)
       (not (or (Character/isISOControl o)
                (Character/isDigit o)
                (Character/isWhitespace o)))))

(def ^:const +value-length-max+
  "Maximum length for a symbol value."
  100)

(defn value?
  "True if o is a valid symbol value."
  [o]
  (and (string? o)
       (<= 1 (.codePointCount o 0 (count o)) +value-length-max+)
       (value-start? (.codePointAt o 0))))

(def ^:const +type+
  {:kind ::ast/type :name ::ast/symbol})

(defn make
  "Make symbol from value, or nil if value is not a valid symbol value."
  [value]
  (when (value? value)
    {:type +type+ :value value}))

(defn type?
  "True if o is the symbol type."
  [o]
  (= +type+ o))

(defn valid?
  "True if o has the symbol type and a valid value."
  [o]
  (and (map? o)
       (type? (:type o))
       (value? (:value o))))
