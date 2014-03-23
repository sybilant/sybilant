;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.parser
  (:refer-clojure :exclude [< <= >= >])
  (:require [clojure.core :as clj]
            [clojure.string :as str]
            [sybilant.numbers :refer [< <= >= >]]
            [sybilant.utils :as u]))

(defn syntax-error
  [expected actual]
  (u/error "expected" (str (name expected) ",") "but was"
           (pr-str (u/form actual))))

(defn arg-type-error
  [type-name arg-name expected-type actual]
  (u/error (name type-name) "expects" (name arg-name) "to be"
           (str (name expected-type) ",") "but was" (pr-str (u/form actual))))

(defn assoc-form
  [exp form]
  (-> exp
      (vary-meta assoc :form (if (instance? clojure.lang.IObj form)
                               (with-meta form {})
                               form))
      (vary-meta merge (meta form))))

(defn check-value-range
  ([type value-name value]
     (check-value-range (u/form type) (:min type) (:max type) value-name value))
  ([type-name min max value-name value]
     (let [min (u/form min)
           max (u/form max)
           value (u/form value)]
       (when-not (<= min value max)
         (u/error (name type-name) "expects" (name value-name) "to be between"
                  min "and" (str max ",") "but was" value)))))

(defn make-type
  [name]
  {:type :type :name name})

(defn type?
  [obj]
  (u/typed-map? :type obj))

(def int-type (make-type :int))

(def ^:const +uint64-max-value+ (inc' (*' 2 Long/MAX_VALUE)))

(defn int-form?
  [form]
  (or (instance? Long form)
      (instance? java.math.BigInteger form)
      (instance? clojure.lang.BigInt form)))

(defn coerce-int-form
  [form]
  (let [form (u/form form)]
    (cond
     (<= Long/MIN_VALUE form Long/MAX_VALUE)
     (long form)
     (<= (inc' Long/MAX_VALUE) form +uint64-max-value+)
     (bigint form)
     :else
     (u/error "value out of range for int:" form))))

(defn make-int
  [form]
  {:pre [(int-form? form)]}
  {:type int-type :form form})

(defn parse-int
  [form]
  (when-not (int-form? form)
    (syntax-error :int form))
  (make-int (coerce-int-form form)))

(defn coerce-int
  [form]
  (parse-int (coerce-int-form form)))

(defn int?
  [exp]
  (u/typed-map? int-type exp))

(defn int-type-form?
  [form]
  (or (= 'int form)
      (int-form? form)
      (u/tagged-list? 'int form)))

(defn check-int-type-form*
  ([type-name type form]
     (check-int-type-form* type-name (:min type) (:max type) form))
  ([type-name type-min type-max form]
     (let [form-count (dec (count form))
           [_ min max] form]
       (when-not (= 2 form-count)
         (u/error (name type-name) "expects 2 arguments, but got" form-count))
       (when-not (int-form? min)
         (arg-type-error type-name :min :int min))
       (when-not (int-form? max)
         (arg-type-error type-name :max :int max))
       (check-value-range type-name type-min type-max "min" min)
       (check-value-range type-name type-min type-max "max" max)
       (when-not (< min max)
         (u/error (name type-name) "expects min to be less than max, but was"
                  (u/form min)))
       [min max])))

(def check-int-type-form
  (partial check-int-type-form* :int-type Long/MIN_VALUE +uint64-max-value+))

(defn make-int-type
  [min max]
  {:pre [(int? min) (int? max)]}
  (assoc int-type :min min :max max))

(defn parse-int-type
  [form]
  (when-not (int-type-form? form)
    (syntax-error :int-type form))
  (cond
   (= 'int form)
   int-type
   (int-form? form)
   (let [exp (parse-int form)]
     (assoc-form (make-int-type exp exp) form))
   :else
   (let [[min max] (check-int-type-form form)]
     (assoc-form (make-int-type (parse-int min) (parse-int max)) form))))

(def int8-type (-> (make-int-type (coerce-int Byte/MIN_VALUE)
                                  (coerce-int Byte/MAX_VALUE))
                   (assoc-form 'int8)
                   (assoc :width 8)))

(defn read-int8
  [form]
  (byte form))

(defmethod print-method Byte
  [obj writer]
  (.write writer "#int8 ")
  (.write writer (str obj)))

(defn int8-form?
  [form]
  (instance? Byte form))

(defn make-int8
  [form]
  {:pre [(int8-form? form)]}
  {:type int8-type :form form})

(defn parse-int8
  [form]
  (when-not (int8-form? form)
    (syntax-error :int8 form))
  (make-int8 form))

(defn int8?
  [exp]
  (u/typed-map? int8-type exp))

(defn int8-type-form?
  [form]
  (or (= 'int8 form)
      (int8-form? form)
      (u/tagged-list? 'int8 form)))

(def check-int8-type-form
  (partial check-int-type-form* :int8-type int8-type))

(defn make-int8-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min int8-type) min max (:max int8-type))]}
     (assoc int8-type :min min :max max))
  ([min max form]
     {:pre [(int8-type-form? form)]}
     (when-not (int8-form? form)
       (check-int8-type-form form))
     (assoc-form (make-int8-type min max) form)))

(defn parse-int8-type
  [form]
  (when-not (int8-type-form? form)
    (syntax-error :int8-type form))
  (cond
   (= 'int8 form)
   int8-type
   (int8-form? form)
   (let [exp (coerce-int form)]
     (make-int8-type exp exp form))
   :else
   (let [[min max] (check-int8-type-form form)]
     (make-int8-type (parse-int min) (parse-int max) form))))

(def int16-type (-> (make-int-type (coerce-int Short/MIN_VALUE)
                                   (coerce-int Short/MAX_VALUE))
                    (assoc :width 16)
                    (assoc-form 'int16)))

(defn read-int16
  [form]
  (short form))

(defmethod print-method Short
  [obj writer]
  (.write writer "#int16 ")
  (.write writer (str obj)))

(defn int16-form?
  [form]
  (instance? Short form))

(defn make-int16
  [form]
  {:pre [(int16-form? form)]}
  {:type int16-type :form form})

(defn parse-int16
  [form]
  (when-not (int16-form? form)
    (syntax-error :int16 form))
  (make-int16 form))

(defn int16?
  [exp]
  (u/typed-map? int16-type exp))

(defn int16-type-form?
  [form]
  (or (= 'int16 form)
      (int16-form? form)
      (u/tagged-list? 'int16 form)))

(def check-int16-type-form
  (partial check-int-type-form* :int16-type int16-type))

(defn make-int16-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min int16-type) min max (:max int16-type))]}
     (assoc int16-type :min min :max max))
  ([min max form]
     {:pre [(int16-type-form? form)]}
     (when-not (int16-form? form)
       (check-int16-type-form form))
     (assoc-form (make-int16-type min max) form)))

(defn parse-int16-type
  [form]
  (when-not (int16-type-form? form)
    (syntax-error :int16-type form))
  (cond
   (= 'int16 form)
   int16-type
   (int16-form? form)
   (let [exp (coerce-int form)]
     (make-int16-type exp exp form))
   :else
   (let [[min max] (check-int16-type-form form)]
     (make-int16-type (parse-int min) (parse-int max) form))))

(def int32-type (-> (make-int-type (coerce-int Integer/MIN_VALUE)
                                   (coerce-int Integer/MAX_VALUE))
                    (assoc :width 32)
                    (assoc-form 'int32)))

(defn read-int32
  [form]
  (int form))

(defmethod print-method Integer
  [obj writer]
  (.write writer "#int32 ")
  (.write writer (str obj)))

(defn int32-form?
  [form]
  (instance? Integer form))

(defn make-int32
  [form]
  {:pre [(int32-form? form)]}
  {:type int32-type :form form})

(defn parse-int32
  [form]
  (when-not (int32-form? form)
    (syntax-error :int32 form))
  (make-int32 form))

(defn int32?
  [exp]
  (u/typed-map? int32-type exp))

(defn int32-type-form?
  [form]
  (or (= 'int32 form)
      (int32-form? form)
      (u/tagged-list? 'int32 form)))

(def check-int32-type-form
  (partial check-int-type-form* :int32-type int32-type))

(defn make-int32-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min int32-type) min max (:max int32-type))]}
     (assoc int32-type :min min :max max))
  ([min max form]
     {:pre [(int32-type-form? form)]}
     (when-not (int32-form? form)
       (check-int32-type-form form))
     (assoc-form (make-int32-type min max) form)))

(defn parse-int32-type
  [form]
  (when-not (int32-type-form? form)
    (syntax-error :int32-type form))
  (cond
   (= 'int32 form)
   int32-type
   (int32-form? form)
   (let [exp (coerce-int form)]
     (make-int32-type exp exp form))
   :else
   (let [[min max] (check-int32-type-form form)]
     (make-int32-type (parse-int min) (parse-int max) form))))

(defrecord Int64
    [form]
  Object
  (toString
    [this]
    (str "#int64 " form)))

(def int64-type (-> (make-int-type (coerce-int Long/MIN_VALUE)
                                   (coerce-int Long/MAX_VALUE))
                    (assoc :width 64)
                    (assoc-form 'int64)))

(defn read-int64
  [form]
  (Int64. (long form)))

(defmethod print-method Int64
  [obj writer]
  (.write writer "#int64 ")
  (.write writer (str (u/form obj))))

(defn int64-form?
  [form]
  (instance? Int64 form))

(defn make-int64
  [form]
  {:pre [(int64-form? form)]}
  {:type int64-type :form form})

(defn parse-int64
  [form]
  (when-not (int64-form? form)
    (syntax-error :int64 form))
  (make-int64 form))

(defn int64?
  [exp]
  (u/typed-map? int64-type exp))

(defn int64-type-form?
  [form]
  (or (= 'int64 form)
      (int64-form? form)
      (u/tagged-list? 'int64 form)))

(def check-int64-type-form
  (partial check-int-type-form* :int64-type int64-type))

(defn make-int64-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min int64-type) min max (:max int64-type))]}
     (assoc int64-type :min min :max max))
  ([min max form]
     {:pre [(int64-type-form? form)]}
     (when-not (int64-form? form)
       (check-int64-type-form form))
     (assoc-form (make-int64-type min max) form)))

(defn parse-int64-type
  [form]
  (when-not (int64-type-form? form)
    (syntax-error :int64-type form))
  (cond
   (= 'int64 form)
   int64-type
   (int64-form? form)
   (let [exp (coerce-int form)]
     (make-int64-type exp exp form))
   :else
   (let [[min max] (check-int64-type-form form)]
     (make-int64-type (parse-int min) (parse-int max) form))))

(def uint-type (assoc int-type :unsigned true))

(defn make-uint-type
  [min max]
  {:pre [(int? min) (int? max)]}
  (assoc uint-type :min min :max max))

(defrecord Uint8
    [form]
  Object
  (toString
    [this]
    (str "#uint8 " form)))

(def ^:const +zero+ (parse-int 0))

(def ^:const +uint8-max-value+ (inc (* 2 Byte/MAX_VALUE)))

(def uint8-type (-> (make-uint-type +zero+ (parse-int +uint8-max-value+))
                    (assoc :width 8)
                    (assoc-form 'uint8)))

(defn read-uint8
  [form]
  (check-value-range uint8-type :value form)
  (Uint8. (long form)))

(defmethod print-method Uint8
  [obj writer]
  (.write writer (str obj)))

(defn uint8-form?
  [form]
  (instance? Uint8 form))

(defn make-uint8
  [form]
  {:pre [(uint8-form? form)]}
  {:type uint8-type :form form})

(defn parse-uint8
  [form]
  (when-not (uint8-form? form)
    (syntax-error :uint8 form))
  (make-uint8 form))

(defn uint8?
  [exp]
  (u/typed-map? uint8-type exp))

(defn uint8-type-form?
  [form]
  (or (= 'uint8 form)
      (uint8-form? form)
      (u/tagged-list? 'uint8 form)))

(def check-uint8-type-form
  (partial check-int-type-form* :uint8-type uint8-type))

(defn make-uint8-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min uint8-type) min max (:max uint8-type))]}
     (assoc uint8-type :min min :max max))
  ([min max form]
     {:pre [(uint8-type-form? form)]}
     (when-not (uint8-form? form)
       (check-uint8-type-form form))
     (assoc-form (make-uint8-type min max) form)))

(defn parse-uint8-type
  [form]
  (when-not (uint8-type-form? form)
    (syntax-error :uint8-type form))
  (cond
   (= 'uint8 form)
   uint8-type
   (uint8-form? form)
   (let [exp (coerce-int form)]
     (make-uint8-type exp exp form))
   :else
   (let [[min max] (check-uint8-type-form form)]
     (make-uint8-type (parse-int min) (parse-int max) form))))

(defrecord Uint16
    [form]
  Object
  (toString
    [this]
    (str "#uint16 " form)))

(def ^:const +uint16-max-value+ (inc (* 2 Short/MAX_VALUE)))

(def uint16-type (-> (make-uint-type +zero+ (parse-int +uint16-max-value+))
                     (assoc :width 16)
                     (assoc-form 'uint16)))

(defn read-uint16
  [form]
  (check-value-range uint16-type :value form)
  (Uint16. (long form)))

(defmethod print-method Uint16
  [obj writer]
  (.write writer (str obj)))

(defn uint16-form?
  [form]
  (instance? Uint16 form))

(defn make-uint16
  [form]
  {:pre [(uint16-form? form)]}
  {:type uint16-type :form form})

(defn parse-uint16
  [form]
  (when-not (uint16-form? form)
    (syntax-error :uint16 form))
  (make-uint16 form))

(defn uint16?
  [exp]
  (u/typed-map? uint16-type exp))

(defn uint16-type-form?
  [form]
  (or (= 'uint16 form)
      (uint16-form? form)
      (u/tagged-list? 'uint16 form)))

(def check-uint16-type-form
  (partial check-int-type-form* :uint16-type uint16-type))

(defn make-uint16-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min uint16-type) min max (:max uint16-type))]}
     (assoc uint16-type :min min :max max))
  ([min max form]
     {:pre [(uint16-type-form? form)]}
     (when-not (uint16-form? form)
       (check-uint16-type-form form))
     (assoc-form (make-uint16-type min max) form)))

(defn parse-uint16-type
  [form]
  (when-not (uint16-type-form? form)
    (syntax-error :uint16-type form))
  (cond
   (= 'uint16 form)
   uint16-type
   (uint16-form? form)
   (let [exp (coerce-int form)]
     (make-uint16-type exp exp form))
   :else
   (let [[min max] (check-uint16-type-form form)]
     (make-uint16-type (parse-int min) (parse-int max) form))))

(defrecord Uint32
    [form]
  Object
  (toString
    [this]
    (str "#uint32 " form)))

(def ^:const +uint32-max-value+ (inc (* 2 Integer/MAX_VALUE)))

(def uint32-type (-> (make-uint-type +zero+ (parse-int +uint32-max-value+))
                     (assoc :width 32)
                     (assoc-form 'uint32)))

(defn read-uint32
  [form]
  (check-value-range uint32-type :value form)
  (Uint32. (long form)))

(defmethod print-method Uint32
  [obj writer]
  (.write writer (str obj)))

(defn uint32-form?
  [form]
  (instance? Uint32 form))

(defn make-uint32
  [form]
  {:pre [(uint32-form? form)]}
  {:type uint32-type :form form})

(defn parse-uint32
  [form]
  (when-not (uint32-form? form)
    (syntax-error :uint32 form))
  (make-uint32 form))

(defn uint32?
  [exp]
  (u/typed-map? uint32-type exp))

(defn uint32-type-form?
  [form]
  (or (= 'uint32 form)
      (uint32-form? form)
      (u/tagged-list? 'uint32 form)))

(def check-uint32-type-form
  (partial check-int-type-form* :uint32-type uint32-type))

(defn make-uint32-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min uint32-type) min max (:max uint32-type))]}
     (assoc uint32-type :min min :max max))
  ([min max form]
     {:pre [(uint32-type-form? form)]}
     (when-not (uint32-form? form)
       (check-uint32-type-form form))
     (assoc-form (make-uint32-type min max) form)))

(defn parse-uint32-type
  [form]
  (when-not (uint32-type-form? form)
    (syntax-error :uint32-type form))
  (cond
   (= 'uint32 form)
   uint32-type
   (uint32-form? form)
   (let [exp (coerce-int form)]
     (make-uint32-type exp exp form))
   :else
   (let [[min max] (check-uint32-type-form form)]
     (make-uint32-type (parse-int min) (parse-int max) form))))

(defrecord Uint64
    [form]
  Object
  (toString
    [this]
    (str "#uint64 " form)))

(def uint64-type (-> (make-uint-type +zero+ (parse-int +uint64-max-value+))
                     (assoc :width 64)
                     (assoc-form 'uint64)))

(defn read-uint64
  [form]
  (check-value-range uint64-type :value form)
  (Uint64. (bigint form)))

(defmethod print-method Uint64
  [obj writer]
  (.write writer (str obj)))

(defn uint64-form?
  [form]
  (instance? Uint64 form))

(defn make-uint64
  [form]
  {:pre [(uint64-form? form)]}
  {:type uint64-type :form form})

(defn parse-uint64
  [form]
  (when-not (uint64-form? form)
    (syntax-error :uint64 form))
  (make-uint64 form))

(defn uint64?
  [exp]
  (u/typed-map? uint64-type exp))

(defn uint64-type-form?
  [form]
  (or (= 'uint64 form)
      (uint64-form? form)
      (u/tagged-list? 'uint64 form)))

(def check-uint64-type-form
  (partial check-int-type-form* :uint64-type uint64-type))

(defn make-uint64-type
  ([min max]
     {:pre [(int? min) (int? max)
            (<= (:min uint64-type) min max (:max uint64-type))]}
     (assoc uint64-type :min min :max max))
  ([min max form]
     {:pre [(uint64-type-form? form)]}
     (when-not (uint64-form? form)
       (check-uint64-type-form form))
     (assoc-form (make-uint64-type min max) form)))

(defn parse-uint64-type
  [form]
  (when-not (uint64-type-form? form)
    (syntax-error :uint64-type form))
  (cond
   (= 'uint64 form)
   uint64-type
   (uint64-form? form)
   (let [exp (coerce-int form)]
     (make-uint64-type exp exp form))
   :else
   (let [[min max] (check-uint64-type-form form)]
     (make-uint64-type (parse-int min) (parse-int max) form))))
