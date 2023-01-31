(ns sybilant.ast.int.type
  (:refer-clojure :exclude [min max])
  (:require
   [sybilant.ast :as-alias ast]))

(def ^:const +value-min+
  "Minimum fixed int value."
  -9223372036854775808)

(def ^:const +value-max+
  "Maximum fixed int value."
  18446744073709551615N)

(defn value?
  "True if o is a valid integer value."
  [o]
  (and (integer? o)
       (<= +value-min+ o +value-max+)))

(def ^:const +signednesses+
  "Set of valid int type signednesses."
  #{:none :unsigned :signed})

(def ^:const +widths+
  "Set of valid int type widths."
  #{8 16 32 64})

(defn signedness?
  "True if o is a valid int type signedness."
  [o]
  (contains? +signednesses+ o))

(defn width?
  "True if o is a valid int type width."
  [o]
  (contains? +widths+ o))

(defn min
  "Minimum value for signedness and width."
  [signedness width]
  (case signedness
    :signed (unchecked-long (.pow (biginteger -2) (dec width)))
    (:none :unsigned) 0))

(defn max
  "Maximum value for signedness and width."
  [signedness width]
  (case signedness
    (:none :signed) (unchecked-long (dec (.pow (biginteger 2) (dec width))))
    :unsigned (cond-> (dec (.pow (biginteger 2) width))
                (not= width 64) unchecked-long)))

(defn make
  "Make int type with signedness, width, min, and max. If min and max are not
  given, they default to the min and max for signedness and width."
  ([signedness width]
   (make signedness width (min signedness width) (max signedness width)))
  ([signedness width min max]
   {:kind ::ast/int
    :signedness signedness
    :width width
    :min min
    :max max}))

(def ^{:arglists '([] [min max])} make-int8
  "Make an 8-bit no signedness int type with min and max. If min and max are not
  given, they default to the min and max for an 8-bit no signedness type."
  (partial make :none 8))
(def ^{:arglists '([] [min max])} make-int16
  "Make an 16-bit no signedness int type with min and max. If min and max are not
  given, they default to the min and max for an 16-bit no signedness type."
  (partial make :none 16))
(def ^{:arglists '([] [min max])} make-int32
  "Make an 32-bit no signedness int type with min and max. If min and max are not
  given, they default to the min and max for an 32-bit no signedness type."
  (partial make :none 32))
(def ^{:arglists '([] [min max])} make-int64
  "Make an 64-bit no signedness int type with min and max. If min and max are not
  given, they default to the min and max for an 64-bit no signedness type."
  (partial make :none 64))

(def ^{:arglists '([] [min max])} make-uint8
  "Make an unsigned 8-bit int type with min and max. If min and max are not given,
  they default to the min and max for an unsigned 8-bit type."
  (partial make :unsigned 8))
(def ^{:arglists '([] [min max])} make-uint16
  "Make an unsigned 16-bit int type with min and max. If min and max are not
  given, they default to the min and max for an unsigned 16-bit type."
  (partial make :unsigned 16))
(def ^{:arglists '([] [min max])} make-uint32
  "Make an unsigned 32-bit int type with min and max. If min and max are not
  given, they default to the min and max for an unsigned 32-bit type."
  (partial make :unsigned 32))
(def ^{:arglists '([] [min max])} make-uint64
  "Make an unsigned 64-bit int type with min and max. If min and max are not
  given, they default to the min and max for an unsigned 64-bit type."
  (partial make :unsigned 64))

(def ^{:arglists '([] [min max])} make-sint8
  "Make a signed 8-bit int type with min and max. If min and max are not given,
  they default to the min and max for a signed 8-bit type."
  (partial make :signed 8))
(def ^{:arglists '([] [min max])} make-sint16
  "Make a signed 16-bit int type with min and max. If min and max are not given,
  they default to the min and max for a signed 16-bit type."
  (partial make :signed 16))
(def ^{:arglists '([] [min max])} make-sint32
  "Make a signed 32-bit int type with min and max. If min and max are not given,
  they default to the min and max for a signed 32-bit type."
  (partial make :signed 32))
(def ^{:arglists '([] [min max])} make-sint64
  "Make a signed 64-bit int type with min and max. If min and max are not given,
  they default to the min and max for a signed 64-bit type."
  (partial make :signed 64))

(def ^:const +int8+
  "8-bit no signedness type with default min and max."
  (make-int8))
(def ^:const +int16+
  "16-bit no signedness type with default min and max."
  (make-int16))
(def ^:const +int32+
  "32-bit no signedness type with default min and max."
  (make-int32))
(def ^:const +int64+
  "64-bit no signedness type with default min and max."
  (make-int64))

(def ^:const +uint8+
  "8-bit unsigned type with default min and max."
  (make-uint8))
(def ^:const +uint16+
  "16-bit unsigned type with default min and max."
  (make-uint16))
(def ^:const +uint32+
  "32-bit unsigned type with default min and max."
  (make-uint32))
(def ^:const +uint64+
  "64-bit unsigned type with default min and max."
  (make-uint64))

(def ^:const +sint8+
  "8-bit signed type with default min and max."
  (make-sint8))
(def ^:const +sint16+
  "16-bit signed type with default min and max."
  (make-sint16))
(def ^:const +sint32+
  "32-bit signed type with default min and max."
  (make-sint32))
(def ^:const +sint64+
  "64-bit signed type with default min and max."
  (make-sint64))

(defn valid?
  "True if o has int-type type and valid min and max for its signedness and
  width."
  [o]
  (and (map? o)
       (= ::ast/int (:kind o))
       (and (signedness? (:signedness o))
            (width? (:width o))
            (value? (:min o))
            (value? (:max o))
            (let [tmin (min (:signedness o) (:width o))
                  tmax (max (:signedness o) (:width o))]
              (<= tmin (:min o) (:max o) tmax)))))

(defn infer
  "Inferred int type for value, or nil, if value is not a valid int
  value. Inferred type has min and max equal to value and the smallest width
  whose default range is valid for value. If value is negative, inferred type
  will be signed."
  [value]
  (when (value? value)
    (-> (if (neg? value)
          (condp > value
            (:min +sint32+) +sint64+
            (:min +sint16+) +sint32+
            (:min +sint8+) +sint16+
            +sint8+)
          (condp < value
            (:max +int64+) +uint64+
            (:max +int32+) +int64+
            (:max +int16+) +int32+
            (:max +int8+) +int16+
            +int8+))
        (assoc :min value :max value))))
