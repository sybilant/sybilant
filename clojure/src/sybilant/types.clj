;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.types
  (:refer-clojure :exclude [symbol?])
  (:require [sybilant.utils :refer :all]))

(defn type?
  ([obj]
     (= :type (:type obj)))
  ([obj name]
     (and (type? obj)
          (= name (:name obj)))))

(defn typed-map?
  [obj t]
  (and (type? t)
       (= t (:type obj))))

(defn make-type
  [name]
  {:type :type :name name})

(def label-type (make-type :label))

(defn label?
  [exp]
  (typed-map? exp label-type))

(def operator-type (make-type :operator))

(defn operator?
  [exp]
  (typed-map? exp operator-type))

(def ^:const +uint64-max-value+ (inc' (*' Long/MAX_VALUE 2)))

(def int-type (assoc (make-type :int)
                :min Long/MIN_VALUE
                :max +uint64-max-value+))

(defn int?
  [exp]
  (= :int (get-in exp [:type :name])))

(def sint-type (make-type :sint))

(def sint8-type (assoc sint-type :min Byte/MIN_VALUE :max Byte/MAX_VALUE))

(defn sint8?
  [exp]
  (typed-map? exp sint8-type))

(def sint16-type (assoc sint-type :min Short/MIN_VALUE :max Short/MAX_VALUE))

(defn sint16?
  [exp]
  (typed-map? exp sint16-type))

(def sint32-type (assoc sint-type
                   :min Integer/MIN_VALUE
                   :max Integer/MAX_VALUE))

(defn sint32?
  [exp]
  (typed-map? exp sint32-type))

(def sint64-type (assoc sint-type :min Long/MIN_VALUE :max Long/MAX_VALUE))

(defn sint64?
  [exp]
  (typed-map? exp sint64-type))

(defn sint?
  [exp]
  (= :sint (get-in exp [:type :name])))

(def uint-type (assoc (make-type :uint) :min 0))

(def ^:const +uint8-max-value+ (inc' (*' Byte/MAX_VALUE 2)))

(def uint8-type (assoc uint-type :max +uint8-max-value+))

(defn uint8?
  [exp]
  (typed-map? exp uint8-type))

(def ^:const +uint16-max-value+ (inc' (*' Short/MAX_VALUE 2)))

(def uint16-type (assoc uint-type :max +uint16-max-value+))

(defn uint16?
  [exp]
  (typed-map? exp uint16-type))

(def ^:const +uint32-max-value+ (inc' (*' Integer/MAX_VALUE 2)))

(def uint32-type (assoc uint-type :max +uint32-max-value+))

(defn uint32?
  [exp]
  (typed-map? exp uint32-type))

(def uint64-type (assoc uint-type :max +uint64-max-value+))

(defn uint64?
  [exp]
  (typed-map? exp uint64-type))

(defn uint?
  [exp]
  (= :uint (get-in exp [:type :name])))

(def symbol-type (make-type :symbol))

(defn symbol?
  [exp]
  (typed-map? exp symbol-type))

(defn precise-literal?
  [exp]
  (or (sint? exp) (uint? exp) (symbol? exp)))

(defn literal?
  [exp]
  (or (int? exp) (precise-literal? exp)))

(defn int-form?
  [form]
  (and (or (instance? Long form)
           (instance? clojure.lang.BigInt form)
           (instance? BigInteger form))
       (<= Long/MIN_VALUE form +uint64-max-value+)))

(defn int-type?
  [exp]
  (type? exp :int))

(defn make-int-type
  [min max]
  {:pre [(int-form? (long min)) (int-form? (long max))]
   :post [(int-type? %)]}
  (assoc int-type :min min :max max))

(def reg-type (make-type :reg))

(def reg8-type (merge reg-type {:width 8
                                :sint sint8-type :uint uint8-type
                                :int (make-int-type 0 (:max sint8-type))}))

(defn reg8?
  [exp]
  (typed-map? exp reg8-type))

(def reg16-type (merge reg-type {:width 16
                                 :sint sint16-type :uint uint16-type
                                 :int (make-int-type 0 (:max sint16-type))}))

(defn reg16?
  [exp]
  (typed-map? exp reg16-type))

(def reg32-type (merge reg-type {:width 32
                                 :sint sint32-type :uint uint32-type
                                 :int (make-int-type 0 (:max sint32-type))}))

(defn reg32?
  [exp]
  (typed-map? exp reg32-type))

(def reg64-type (merge reg-type {:width 64
                                 :sint sint64-type :uint uint64-type
                                 :int (make-int-type 0 (:max sint64-type))}))

(defn reg64?
  [exp]
  (typed-map? exp reg64-type))

(defn reg?
  [exp]
  (or (reg8? exp) (reg16? exp) (reg32? exp) (reg64? exp)))

(defn base?
  [exp]
  (reg? exp))

(defn index?
  [exp]
  (reg? exp))

(defn scale-form?
  [form]
  (contains? #{1 2 4 8} form))

(defn scale?
  [exp]
  (scale-form? (form exp)))

(defn disp?
  [exp]
  (or (literal? exp) (symbol? exp)))

(def mem-type (make-type :mem))

(defn mem-type?
  [exp]
  (type? exp :mem))

(def mem8-type (merge mem-type {:width 8
                                :sint sint8-type :uint uint8-type
                                :int (make-int-type 0 (:max sint8-type))}))

(defn mem8?
  [exp]
  (typed-map? exp mem8-type))

(def mem16-type (merge mem-type {:width 16
                                 :sint sint16-type :uint uint16-type
                                 :int (make-int-type 0 (:max sint16-type))}))

(defn mem16?
  [exp]
  (typed-map? exp mem16-type))

(def mem32-type (merge mem-type {:width 32
                                 :sint sint32-type :uint uint32-type
                                 :int (make-int-type 0 (:max sint32-type))}))

(defn mem32?
  [exp]
  (typed-map? exp mem32-type))

(def mem64-type (merge mem-type {:width 64
                                 :sint sint64-type :uint uint64-type
                                 :int (make-int-type 0 (:max sint64-type))}))

(defn mem64?
  [exp]
  (typed-map? exp mem64-type))

(defn mem?
  [exp]
  (or (mem8? exp) (mem16? exp) (mem32? exp) (mem64? exp)))

(defn operand?
  [exp]
  (or (literal? exp) (symbol? exp) (reg? exp) (mem? exp)))

(def instruction-type (make-type :instruction))

(defn instruction?
  [exp]
  (typed-map? exp instruction-type))

(defn statement?
  [exp]
  (or (label? exp) (instruction? exp)))

(def deftext-type (make-type :deftext))

(defn deftext?
  [exp]
  (typed-map? exp deftext-type))

(def defdata-type (make-type :defdata))

(defn defdata?
  [exp]
  (typed-map? exp defdata-type))

(def defconst-type (make-type :defconst))

(defn defconst?
  [exp]
  (typed-map? exp defconst-type))

(defn top-level?
  [exp]
  (or (deftext? exp) (defdata? exp) (defconst? exp)))
