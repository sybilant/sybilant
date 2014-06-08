;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer.syntax
  (:refer-clojure :exclude [symbol?])
  (:require [clojure.core :as clj]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [sybilant.environment :refer :all]
            [sybilant.types :refer :all]
            [sybilant.utils :refer :all]
            [sybilant.visitor :refer [dfs-visit visit-property]]
            [sybilant.x86db :as x86db]))

(defn width
  [operand]
  (get-in operand [:type :width]))

(defn int-byte?
  [exp]
  (or (sint8? exp)
      (uint8? exp)
      (and (int? exp)
           (let [exp-type (:type exp)]
             (<= (int-min sint8-type) (int-min exp-type)
                 (int-max exp-type) (int-max uint8-type))))))

(defn byte?
  [exp]
  (or (symbol? exp) (reg8? exp) (mem8? exp) (int-byte? exp)))

(defn int-sbyte?
  [exp]
  (or (sint8? exp)
      (and (int? exp)
           (let [exp-type (:type exp)]
             (<= (int-min sint8-type) (int-min exp-type)
                 (int-max exp-type) (int-max sint8-type))))))

(defn sbyte?
  [exp]
  (or (reg8? exp) (mem8? exp) (int-sbyte? exp)))

(defn int-word?
  [exp]
  (or (sint16? exp)
      (uint16? exp)
      (and (int? exp)
           (let [exp-type (:type exp)]
             (<= (int-min sint16-type) (int-min exp-type)
                 (int-max exp-type) (int-max uint16-type))))))

(defn word?
  [exp]
  (or (symbol? exp) (reg16? exp) (mem16? exp) (int-word? exp)))

(defn int-doubleword?
  [exp]
  (or (sint32? exp)
      (uint32? exp)
      (and (int? exp)
           (let [exp-type (:type exp)]
             (<= (int-min sint32-type) (int-min exp-type)
                 (int-max exp-type) (int-max uint32-type))))))

(defn doubleword?
  [exp]
  (or (symbol? exp) (reg32? exp) (mem32? exp) (int-doubleword? exp)))

(defn int-sdoubleword?
  [exp]
  (or (sint32? exp)
      (and (int? exp)
           (let [exp-type (:type exp)]
             (<= (int-min sint32-type) (int-min exp-type)
                 (int-max exp-type) (int-max sint32-type))))))

(defn sdoubleword?
  [exp]
  (or (reg32? exp) (mem32? exp) (int-sdoubleword? exp)))

(defn int-quadword?
  [exp]
  (or (sint64? exp)
      (uint64? exp)
      (and (int? exp)
           (let [exp-type (:type exp)]
             (<= (int-min sint64-type) (int-min exp-type)
                 (int-max exp-type) (int-max uint64-type))))))

(defn quadword?
  [exp]
  (or (symbol? exp) (reg64? exp) (mem64? exp) (int-quadword? exp)))

(defn schema-arg-size-match?
  [schema-arg operand]
  {:pre [(operand? operand)]}
  (or (= (:size schema-arg) (width operand))
      (when-let [arg-type (get schema-arg (get-in operand [:type :name]))]
        (let [op-type (:type operand)]
          (<= (int-min arg-type) (int-min op-type)
              (int-max op-type) (int-max arg-type))))))

(defn schema-arg-type-match?
  [{:keys [type size] :as schema-arg} operand]
  {:pre [(operand? operand)]}
  (case type
    "b" (byte? operand)
    "bs" (sbyte? operand)
    "d" (doubleword? operand)
    "q" (quadword? operand)
    "v" nil
    "vds" (and (implies size (schema-arg-size-match? schema-arg operand))
               (if (= size 64)
                 (sdoubleword? operand)
                 (or (word? operand) (doubleword? operand))))
    "vqp" (and (implies size (schema-arg-size-match? schema-arg operand))
               (or (word? operand) (doubleword? operand) (quadword? operand)))
    "w" (word? operand)))

(defn segment-memory?
  [exp]
  (mem? exp))

(defn control-register?
  [exp]
  nil)

(defn debug-register?
  [exp]
  nil)

(defn general-register?
  [exp]
  (reg? exp))

(defn segment-register?
  [exp]
  nil)

(defn immediate?
  [exp]
  (literal? exp))

(defn offset?
  [exp]
  (or (symbol? exp) (int-word? exp) (int-doubleword? exp) (int-quadword? exp)))

(defn relative-offset?
  [exp]
  (or (symbol? exp) (int-word? exp) (int-doubleword? exp) (int-quadword? exp)))

(defn schema-arg-address-match?
  [{:keys [address group nr] :as schema-arg} operand]
  {:pre [(operand? operand)]}
  (if (and group nr)
    (and (reg? operand)
         (= group (:group operand))
         (= nr (:nr operand)))
    (case address
      "C" (control-register? operand)
      "D" (debug-register? operand)
      "E" (or (general-register? operand)
              (segment-memory? operand))
      "G" (general-register? operand)
      "H" (general-register? operand)
      "I" (immediate? operand)
      "J" (relative-offset? operand)
      "O" (offset? operand)
      "R" (general-register? operand)
      "S" (segment-register? operand)
      "Z" (general-register? operand))))

(defn schema-arg-match?
  [schema-arg operand]
  {:pre [(operand? operand)]}
  (if-let [c (constant operand)]
    (recur schema-arg (:value c))
    (and (schema-arg-type-match? schema-arg operand)
         (schema-arg-address-match? schema-arg operand))))

(defn filter-by-arity
  [schemata operands]
  {:pre [(every? operand? operands)]}
  (let [operand-count (count operands)]
    (for [schema schemata
          :when (= (count schema) operand-count)]
      schema)))

(defn filter-by-arg
  [schemata operands n]
  {:pre [(every? operand? operands) (< n (count operands))]}
  (for [schema schemata
        :let [schema-arg (nth schema n)
              operand (nth operands n)]
        :when (schema-arg-match? schema-arg operand)]
    schema))

(defn invalid-syntax
  ([schemata exp]
     {:pre [(instruction? exp)]}
     (error "invalid syntax %s%s" (form exp) (compiling exp)))
  ([schemata exp n]
     {:pre [(instruction? exp) (< n (count (:operands exp)))]}
     (error "invalid syntax %s%s" (form exp) (compiling exp))))

(defonce mnemonics (into #{} (x86db/mnemonics x86db/db)))

(defn mnemonic->operator
  [mnemonic]
  (symbol (str \% (str/lower-case mnemonic))))

(defonce operators (into #{} (map mnemonic->operator mnemonics)))

(defonce operator-schematas
  (into {} (for [m mnemonics
                 :let [operator (mnemonic->operator m)]]
             [operator
              (for [{:keys [operands]} (x86db/mnemonic-syntax m x86db/db)]
                (for [operand operands]
                  (if (:nr operand)
                    (update-in operand [:nr] #(Integer/parseInt %))
                    operand)))])))

(defn operator-schemata
  [exp]
  {:pre [(operator? exp)]}
  (get operator-schematas (form exp)))

(defn find-dst-index
  [schema]
  (reduce (fn [index [i schema-arg]]
            (if (= (:mode schema-arg) "dst")
              i
              index))
          0
          (zipmap (range) schema)))

(defn infer-size
  [schemata operands]
  (for [schema schemata
        :let [n (find-dst-index schema)
              schema-arg (nth schema n)
              operand (nth operands n)]
        :when (schema-arg-match? schema-arg operand)]
    (for [schema-arg schema]
      (assoc schema-arg
        :size (width operand)
        :sint (get-in operand [:type :sint])
        :uint (get-in operand [:type :uint])
        :int (get-in operand [:type :int])))))

(defn check-operator
  [{:keys [operator] :as exp}]
  {:pre [(instruction? exp) (operator? operator)]}
  (when-not (contains? operators (form operator))
    (error "invalid operator %s%s" (form operator) (compiling operator)))
  exp)

(defn check-instruction
  [{:keys [operator operands] :as exp}]
  {:pre [(instruction? exp)
         (operator? operator) (contains? operators (form operator))
         (every? operand? operands)]}
  (let [operand-count (count operands)
        schemata (filter-by-arity (operator-schemata operator) operands)]
    (when-not (seq schemata)
      (error "wrong number of arguments for %s%s" (form operator)
             (compiling exp)))
    (if-let [schemata (seq (infer-size schemata operands))]
      (loop [n 0 schemata schemata]
        (let [schemata* (filter-by-arg schemata operands n)]
          (cond
           (and (< (inc n) operand-count) (seq schemata*))
           (recur (inc n) schemata*)
           (< (inc n) operand-count)
           (invalid-syntax schemata exp n)
           (seq schemata*)
           exp
           :else
           (invalid-syntax schemata exp n))))
      (invalid-syntax schemata exp))))

(defn check-syntax
  [exp]
  {:pre [(top-level? exp)]}
  (dfs-visit exp (fn [exp]
                   (if (instruction? exp)
                     (-> exp
                         check-operator
                         check-instruction)
                     exp))))
