;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.parser
  (:refer-clojure :exclude [< <= >= > integer? symbol?])
  (:require [clojure.core :as clj]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sybilant.numbers :refer [< <= >= >]]
            [sybilant.utils :as u]
            [sybilant.x86db :as db]))

(defn syntax-error
  [expected actual]
  (u/error "expected" (str (name expected) ",") "but was"
           (pr-str (u/form actual))))

(defn arity-error
  [type-name expected-count actual-count form]
  (u/error (name type-name) "expects" expected-count "arguments, but got"
           (str actual-count ":") (pr-str (u/form form))))

(defn arity-error-at-least
  [type-name expected-count actual-count form]
  (u/error (name type-name) "expects at least" expected-count "arguments, but"
           "got" (str actual-count ":") (pr-str (u/form form))))

(defn arg-type-error
  ([type-name arg-name expected-type actual]
     (u/error (name type-name) "expects" (name arg-name) "to be"
              (str (name expected-type) ",") "but was"
              (pr-str (u/form actual))))
  ([type-name arg-name expected-type actual form]
     (u/error (name type-name) "expects" (name arg-name) "to be"
              (str (name expected-type) ",") "but was"
              (str (pr-str (u/form actual)) ":") (pr-str (u/form form)))))

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

(defn integer-form?
  [form]
  (or (int-form? form)
      (int8-form? form)
      (uint8-form? form)
      (int16-form? form)
      (uint16-form? form)
      (int32-form? form)
      (uint32-form? form)
      (int64-form? form)
      (uint64-form? form)))

(defn parse-integer
  [form]
  (when-not (integer-form? form)
    (syntax-error :integer form))
  (cond
   (int-form? form) (parse-int form)
   (int8-form? form) (parse-int8 form)
   (int16-form? form) (parse-int16 form)
   (int32-form? form) (parse-int32 form)
   (int64-form? form) (parse-int64 form)
   (uint8-form? form) (parse-uint8 form)
   (uint16-form? form) (parse-uint16 form)
   (uint32-form? form) (parse-uint32 form)
   (uint64-form? form) (parse-uint64 form)))

(defn integer?
  [exp]
  (or (int? exp)
      (int8? exp)
      (uint8? exp)
      (int16? exp)
      (uint16? exp)
      (int32? exp)
      (uint32? exp)
      (int64? exp)
      (uint64? exp)))

(def register-type (make-type :register))

(defn reg-type?
  [exp]
  (and (type? exp)
       (= :register (:name exp))))

(def reg8-type (assoc register-type :width 8))

(def reg16-type (assoc register-type :width 16))

(def reg32-type (assoc register-type :width 32))

(def reg64-type (assoc register-type :width 64))

(def registers (-> (io/resource "sybilant/registers.clj")
                   slurp
                   read-string
                   eval))

(defn reg8-form?
  [form]
  (= 8 (get-in registers [form :type :width])))

(defn make-reg8
  [form]
  {:pre [(reg8-form? form)]}
  (assoc-form (get registers form) form))

(defn parse-reg8
  [form]
  (when-not (reg8-form? form)
    (syntax-error :reg8 form))
  (make-reg8 form))

(defn reg8?
  [exp]
  (u/typed-map? reg8-type exp))

(defn reg16-form?
  [form]
  (= 16 (get-in registers [form :type :width])))

(defn make-reg16
  [form]
  {:pre [(reg16-form? form)]}
  (assoc-form (get registers form) form))

(defn parse-reg16
  [form]
  (when-not (reg16-form? form)
    (syntax-error :reg16 form))
  (make-reg16 form))

(defn reg16?
  [exp]
  (u/typed-map? reg16-type exp))

(defn reg32-form?
  [form]
  (= 32 (get-in registers [form :type :width])))

(defn make-reg32
  [form]
  {:pre [(reg32-form? form)]}
  (assoc-form (get registers form) form))

(defn parse-reg32
  [form]
  (when-not (reg32-form? form)
    (syntax-error :reg32 form))
  (make-reg32 form))

(defn reg32?
  [exp]
  (u/typed-map? reg32-type exp))

(defn reg64-form?
  [form]
  (= 64 (get-in registers [form :type :width])))

(defn make-reg64
  [form]
  {:pre [(reg64-form? form)]}
  (assoc-form (get registers form) form))

(defn parse-reg64
  [form]
  (when-not (reg64-form? form)
    (syntax-error :reg64 form))
  (make-reg64 form))

(defn reg64?
  [exp]
  (u/typed-map? reg64-type exp))

(defn register-form?
  [form]
  (contains? registers form))

(defn make-register
  [form]
  {:pre [(register-form? form)]}
  (assoc-form (get registers form) form))

(defn parse-register
  [form]
  (when-not (register-form? form)
    (syntax-error :register form))
  (make-register form))

(defn register?
  [exp]
  (reg-type? (:type exp)))

(def symbol-type (make-type :symbol))

(defn symbol-form?
  [form]
  (and (clj/symbol? form) (not (register-form? form))))

(defn make-symbol
  [form]
  {:pre [(symbol-form? form)]}
  (vary-meta {:type symbol-type :form form} merge (meta form)))

(defn parse-symbol
  [form]
  (when-not (symbol-form? form)
    (syntax-error :symbol form))
  (make-symbol form))

(defn symbol?
  [exp]
  (u/typed-map? symbol-type exp))

(def mem-type (make-type :mem))

(defn mem-type?
  [exp]
  (and (type? exp)
       (= :mem (:name exp))))

(defn make-mem
  [type base index scale disp]
  {:pre [(mem-type? type)
         ((u/maybe register?) base)
         ((u/maybe register?) index)
         ((u/maybe int?) scale)
         ((u/maybe int?) disp)
         (or base index scale disp)
         (u/implies scale index)
         (u/implies index disp)]}
  (merge {:type type}
         (when base
           {:base base})
         (when index
           {:index index})
         (when scale
           {:scale scale})
         (when disp
           {:disp disp})))

(defn disp-form?
  [form]
  (and (integer-form? form)
       (<= (:min int32-type) (u/form form) (:max int32-type))))

(defn scale-form?
  [form]
  (contains? #{1 2 4 8} (u/form form)))

(defn parse-mem-args*
  [form]
  {:pre [(seq form)]}
  (let [[mem & [a b c d :as args]] form
        arg-count (count args)]
    (case arg-count
      1 (cond
         (disp-form? a) [mem nil nil nil a]
         (register-form? a) [mem a nil nil nil]
         :else (u/error mem "expects register or int, but was"
                        (str (pr-str a) ":") (pr-str form)))
      2 [mem a nil nil b]
      3 (cond
         (scale-form? b) [mem nil a b c]
         (register-form? b) [mem a b nil c]
         :else (u/error mem "expects register or scale, but was"
                        (str (pr-str b) ":") (pr-str form)))
      4 [mem a b c d]
      (u/error (str mem) "expects between 1 and 4 arguments, but got"
               (str arg-count ":") (pr-str form)))))

(defn parse-mem-args
  [form]
  (let [[mem base index scale disp] (parse-mem-args* form)]
    (letfn
        [(arg-type-error [arg-name expected-type actual]
           (u/error mem "expects" (name arg-name) "to be"
                    (str (name expected-type) ",") "but was"
                    (str (pr-str (u/form actual)) ":") (pr-str form)))]
      [(when base
         (when-not (register-form? base)
           (arg-type-error :base :register base))
         (parse-register base))
       (when index
         (when-not (register-form? index)
           (arg-type-error :index :register index))
         (parse-register index))
       (when scale
         (when-not (scale-form? scale)
           (arg-type-error :scale :scale scale))
         (coerce-int scale))
       (when disp
         (when-not (disp-form? disp)
           (arg-type-error :displacement :int disp))
         (coerce-int disp))])))

(def mem8-type (assoc mem-type :width 8))

(defn mem8-form?
  [form]
  (u/tagged-list? '%mem8 form))

(defn make-mem8
  ([base index scale disp]
     (make-mem mem8-type base index scale disp))
  ([base index scale disp form]
     {:pre [(mem8-form? form)]}
     (assoc-form (make-mem8 base index scale disp) form)))

(defn parse-mem8
  [form]
  (when-not (mem8-form? form)
    (syntax-error :mem8 form))
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem8 base index scale disp form)))

(defn mem8?
  [exp]
  (u/typed-map? mem8-type exp))

(def mem16-type (assoc mem-type :width 16))

(defn mem16-form?
  [form]
  (u/tagged-list? '%mem16 form))

(defn make-mem16
  ([base index scale disp]
     (make-mem mem16-type base index scale disp))
  ([base index scale disp form]
     {:pre [(mem16-form? form)]}
     (assoc-form (make-mem16 base index scale disp) form)))

(defn parse-mem16
  [form]
  (when-not (mem16-form? form)
    (syntax-error :mem16 form))
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem16 base index scale disp form)))

(defn mem16?
  [exp]
  (u/typed-map? mem16-type exp))

(def mem32-type (assoc mem-type :width 32))

(defn mem32-form?
  [form]
  (u/tagged-list? '%mem32 form))

(defn make-mem32
  ([base index scale disp]
     (make-mem mem32-type base index scale disp))
  ([base index scale disp form]
     {:pre [(mem32-form? form)]}
     (assoc-form (make-mem32 base index scale disp) form)))

(defn parse-mem32
  [form]
  (when-not (mem32-form? form)
    (syntax-error :mem32 form))
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem32 base index scale disp form)))

(defn mem32?
  [exp]
  (u/typed-map? mem32-type exp))

(def mem64-type (assoc mem-type :width 64))

(defn mem64-form?
  [form]
  (u/tagged-list? '%mem64 form))

(defn make-mem64
  ([base index scale disp]
     (make-mem mem64-type base index scale disp))
  ([base index scale disp form]
     {:pre [(mem64-form? form)]}
     (assoc-form (make-mem64 base index scale disp) form)))

(defn parse-mem64
  [form]
  (when-not (mem64-form? form)
    (syntax-error :mem64 form))
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem64 base index scale disp form)))

(defn mem64?
  [exp]
  (u/typed-map? mem64-type exp))

(defn mem-form?
  [form]
  (or (mem8-form? form)
      (mem16-form? form)
      (mem32-form? form)
      (mem64-form? form)))

(defn parse-mem
  [form]
  (when-not (mem-form? form)
    (syntax-error :mem form))
  (cond
   (mem8-form? form) (parse-mem8 form)
   (mem16-form? form) (parse-mem16 form)
   (mem32-form? form) (parse-mem32 form)
   (mem64-form? form) (parse-mem64 form)))

(defn mem?
  [exp]
  (or (mem8? exp)
      (mem16? exp)
      (mem32? exp)
      (mem64? exp)))

(defn all-mnemonics
  []
  (set (db/mnemonics db/db)))

(alter-var-root #'all-mnemonics memoize)

(defn mnemonic
  [form]
  (if (symbol-form? form)
    (str/upper-case (subs (name form) 1))))

(def operator-type (make-type :operator))

(defn operator-form?
  [form]
  (and (symbol-form? form)
       (contains? (all-mnemonics) (mnemonic form))))

(defn make-operator
  [form]
  {:pre [(operator-form? form)]}
  (assoc-form {:type operator-type :mnemonic (mnemonic form)} form))

(defn parse-operator
  [form]
  (when-not (operator-form? form)
    (syntax-error :operator form))
  (make-operator form))

(defn operator?
  [exp]
  (u/typed-map? operator-type exp))

(defn operand-form?
  [form]
  (or (integer-form? form)
      (register-form? form)
      (mem-form? form)))

(defn parse-operand
  [form]
  (when-not (operand-form? form)
    (syntax-error :operand form))
  (cond
   (integer-form? form) (parse-integer form)
   (register-form? form) (parse-register form)
   (mem-form? form) (parse-mem form)))

(defn operand?
  [exp]
  (or (integer? exp)
      (register? exp)
      (mem? exp)))

(def instruction-type (make-type :instruction))

(defn instruction-form?
  [form]
  (and (list? form)
       (operator-form? (first form))))

(defn operand-addressing?
  [operand {:keys [group nr address]}]
  (if (and group nr)
    (and (= group (:group operand))
         (= nr (:nr operand)))
    (case address
      "C" nil ; control register
      "D" nil ; debug register
      "E" (or (register? operand) (mem? operand))
      ("G" "R" "H" "Z") (register? operand)
      "I" (integer? operand)
      "M" (mem? operand)
      "O" (and (mem? operand)
               (:disp operand)
               (not (or (:base operand)
                        (:index operand)
                        (:scale operand))))
      "S" nil ; segment register
      "T" nil ; test register
      nil true)))

(defn byte?
  [operand]
  (or (reg8? operand) (mem8? operand) (int8? operand) (uint8? operand)
      (and (int? operand)
           (<= Byte/MIN_VALUE (u/form operand) +uint8-max-value+))))

(defn signed-byte?
  [operand]
  (or (reg8? operand) (mem8? operand) (int8? operand)
      (and (int? operand)
           (<= Byte/MIN_VALUE (u/form operand) Byte/MAX_VALUE))))

(defn word?
  [operand]
  (or (reg16? operand) (mem16? operand) (int16? operand) (uint16? operand)
      (and (int? operand)
           (<= Short/MIN_VALUE (u/form operand) +uint16-max-value+))))

(defn signed-word?
  [operand]
  (or (reg16? operand) (mem16? operand) (int16? operand)
      (and (int? operand)
           (<= Short/MIN_VALUE (u/form operand) Short/MAX_VALUE))))

(defn doubleword?
  [operand]
  (or (reg32? operand) (mem32? operand) (int32? operand) (uint32? operand)
      (and (int? operand)
           (<= Integer/MIN_VALUE (u/form operand) +uint32-max-value+))))

(defn signed-doubleword?
  [operand]
  (or (reg32? operand) (mem32? operand) (int32? operand)
      (and (int? operand)
           (<= Integer/MIN_VALUE (u/form operand) Integer/MAX_VALUE))))

(defn quadword?
  [operand]
  (or (reg64? operand) (mem64? operand) (int64? operand) (uint64? operand)
      (and (int? operand)
           (<= Long/MIN_VALUE (u/form operand) +uint64-max-value+))))

(defn signed?
  [operand]
  (not (get-in operand [:type :unsigned])))

(defn operand-type?
  [operand {:keys [type width]}]
  (and (u/implies width (or (integer? operand)
                            (= width (u/width operand))))
       (case type
         "b" (byte? operand)
         "bs" (signed-byte? operand)
         "w" (word? operand)
         "d" (doubleword? operand)
         "q" (quadword? operand)
         "vqp" (and (or (word? operand) (doubleword? operand)
                        (quadword? operand))
                    (u/implies (not= width (u/width operand))
                               (signed? operand)))
         "vds" (and (or (word? operand) (doubleword? operand))
                    (u/implies (not= width (u/width operand))
                               (signed? operand)))
         nil true)))

(defn operand-syntax?
  [operand operand-syntax]
  (and (operand-addressing? operand operand-syntax)
       (operand-type? operand operand-syntax)))

(defn infer-width
  [operands syntax-schema]
  (let [[[_ dst-index]] (filter #(= "dst" (:mode (first %)))
                                (map vector
                                     (:operands syntax-schema)
                                     (range)))]
    (if dst-index
      (let [operand (nth operands dst-index nil)
            operand-syntax (nth (:operands syntax-schema) dst-index nil)]
        (if (operand-syntax? operand operand-syntax)
          (if-let [width (u/width operand)]
            (update-in syntax-schema [:operands]
                       #(mapv (fn [o] (assoc o :width width)) %))
            syntax-schema)
          syntax-schema))
      syntax-schema)))

(defn filter-syntaxes
  [arg operands syntaxes]
  (let [operand (nth operands arg)]
    (filter (fn [syntax]
              (and (= (count operands) (count (:operands syntax)))
                   (operand-syntax? operand (nth (:operands syntax) arg nil))))
            syntaxes)))

(defn operator-syntax
  [operator]
  (set (db/mnemonic-syntax (:mnemonic operator) db/db)))

(defn validate-instruction
  [operator operands]
  (let [syntaxes (operator-syntax operator)
        syntaxes (map (partial infer-width operands) syntaxes)]
    (loop [arg 0
           syntaxes syntaxes]
      (if (< arg (count operands))
        (let [new-syntaxes (filter-syntaxes arg operands syntaxes)]
          (if (seq new-syntaxes)
            (recur (inc arg) new-syntaxes)
            false))
        true))))

(defn make-instruction
  ([operator operands]
     {:pre [(operator? operator) (every? operand? operands)]}
     (when-not (validate-instruction operator operands)
       (u/error "invalid syntax"))
     {:type instruction-type :operator operator :operands operands})
  ([operator operands form]
     (when-not (validate-instruction operator operands)
       (u/error "invalid syntax for" (u/form form)))
     (assoc-form (make-instruction operator operands) form)))

(defn parse-instruction
  [form]
  (when-not (instruction-form? form)
    (syntax-error :instruction form))
  (let [[operator-form & operand-forms] form]
    (when-not (operator-form? operator-form)
      (arg-type-error :instruction :operator operator-form form))
    (doseq [operand-form operand-forms]
      (when-not (operand-form? operand-form)
        (arg-type-error :instruction :operand
                        "integer, register, or memory reference"
                        operand-form form)))
    (make-instruction (parse-operator operator-form)
                      (map parse-operand operand-forms)
                      form)))

(defn instruction?
  [exp]
  (u/typed-map? instruction-type exp))

(def label-type (make-type :label))

(defn label-form?
  [form]
  (and (list? form)
       (= '%label (first form))))

(defn make-label
  ([name]
     {:pre [(symbol? name)]}
     {:type label-type :name name})
  ([name form]
     {:pre [(label-form? form)]}
     (assoc-form (make-label name) form)))

(defn parse-label
  [form]
  (when-not (label-form? form)
    (syntax-error :label form))
  (let [form-count (dec (count form))]
    (when (not= 1 form-count)
      (arity-error :label 1 form-count form)))
  (let [name-form (second form)]
    (when-not (symbol-form? name-form)
      (arg-type-error :label :name :symbol name-form form))
    (make-label (parse-symbol name-form) form)))

(defn label?
  [exp]
  (u/typed-map? label-type exp))

(defn statement-form?
  [form]
  (or (instruction-form? form) (label-form? form)))

(defn parse-statement
  [form]
  (when-not (statement-form? form)
    (syntax-error :statement form))
  (cond
   (instruction-form? form) (parse-instruction form)
   (label-form? form) (parse-label form)))

(defn statement?
  [exp]
  (or (instruction? exp)
      (label? exp)))

(def defasm-type (make-type :defasm))

(defn defasm-form?
  [form]
  (and (list? form)
       (= 'defasm (first form))))

(defn make-defasm
  ([name instruction statements]
     {:pre [(symbol? name) (instruction? instruction)
            (every? statement? statements)]}
     (merge {:type defasm-type :name name :instruction instruction}
            (when (seq statements)
              {:statements statements})))
  ([name instruction statements form]
     {:pre [(defasm-form? form)]}
     (assoc-form (make-defasm name instruction statements) form)))

(defn parse-defasm
  [form]
  (when-not (defasm-form? form)
    (syntax-error :defasm form))
  (let [form-count (dec (count form))]
    (when-not (<= 2 form-count)
      (arity-error-at-least :defasm 2 form-count form)))
  (let [[_ name-form instruction-form & statement-forms] form]
    (when-not (symbol-form? name-form)
      (arg-type-error :defasm :name :symbol name-form))
    (when-not (instruction-form? instruction-form)
      (u/error "defasm" name-form "expects first statement to be instruction,"
               "but was" (pr-str instruction-form)))
    (loop [label-form nil
           [statement-form & statement-forms] statement-forms]
      (when statement-form
        (when-not (statement-form? statement-form)
          (u/error "defasm" name-form "expects statement, but was"
                   (pr-str statement-form)))
        (when (and label-form (label-form? statement-form))
          (u/error "defasm" name-form "expects label to be followed by"
                   "instruction, but was" (pr-str label-form) "followed by"
                   (pr-str statement-form)))
        (recur (when (label-form? statement-form)
                 statement-form)
               statement-forms)))
    (make-defasm (parse-symbol name-form)
                 (parse-instruction instruction-form)
                 (map parse-statement statement-forms))))

(defn defasm?
  [exp]
  (u/typed-map? defasm-type exp))
