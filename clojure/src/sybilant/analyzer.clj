;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer
  (:refer-clojure :exclude [number? string? symbol?])
  (:require [clojure.java.io :as io]
            [sybilant.parser :refer :all]
            [sybilant.util :refer [error]]
            [sybilant.visitor :refer [visit]]))

(def ^:dynamic *globals* (atom {}))

(defn check-symbol-reference [exp]
  (when (symbol? exp)
    (when-not (contains? (:symbol-table (meta exp)) exp)
      (error exp "is undefined")))
  exp)

(defn check-symbol-format [exp]
  (when (symbol? exp)
    (when (and (get-in (:symbol-table (meta exp)) [exp :extern?])
               (not (re-matches #"^[a-zA-Z_][a-zA-Z0-9_]*$" (str (:form exp)))))
      (error exp "is an invalid symbol")))
  exp)

(defn add-symbol-table-entry [atom exp]
  (swap! atom assoc
         (:name exp) (merge (select-keys exp [:type :name :value])
                            (meta exp))))

(defn get-constant-value [exp]
  (if (symbol? exp)
    (let [globals (:globals (meta exp))]
      (if (= :defconst (get-in globals [exp :type]))
        (get-in globals [exp :value])
        exp))
    exp))

(defn replace-constants [exp]
  (get-constant-value exp))

(defn definition? [exp]
  (:definition? (meta exp)))

(defn populate-symbol-table [exp]
  (let [locals (atom {})]
    (-> exp
        (visit (fn [exp]
                 (when (definition? exp)
                   (when (contains? @locals (:name exp))
                     (error (:name exp) "is already defined"))
                   (add-symbol-table-entry locals exp))
                 (vary-meta exp assoc :globals @*globals*)))
        (visit (fn [exp]
                 (let [exp (vary-meta exp assoc :locals @locals)]
                   (vary-meta exp assoc
                              :symbol-table
                              (merge (:globals (meta exp))
                                     (:locals (meta exp))))))))))

(defn reg8? [exp]
  (and (register? exp) (= 8 (:width exp))))

(defn reg16? [exp]
  (and (register? exp) (= 16 (:width exp))))

(defn reg32? [exp]
  (and (register? exp) (= 32 (:width exp))))

(defn reg64? [exp]
  (and (register? exp) (= 64 (:width exp))))

(def preds {:imm8 #(or (uint8? %)
                       (int8? %)
                       (symbol? %)
                       (and (number? %)
                            (<= Byte/MIN_VALUE
                                (:form %)
                                (inc (* Byte/MAX_VALUE 2)))))
            :imm16 #(or (uint16? %)
                        (int16? %)
                        (symbol? %)
                        (and (number? %)
                             (<= Short/MIN_VALUE
                                 (:form %)
                                 (inc (* Short/MAX_VALUE 2)))))
            :imm32 #(or (uint32? %)
                        (int32? %)
                        (symbol? %)
                        (and (number? %)
                             (<= Integer/MIN_VALUE
                                 (:form %)
                                 (inc (* Integer/MAX_VALUE 2)))))
            :imm64 #(or (uint64? %)
                        (int64? %)
                        (symbol? %)
                        (and (number? %)
                             (<= Long/MIN_VALUE
                                 (:form %)
                                 (inc' (*' Long/MAX_VALUE 2)))))
            :int8 #(or (int8? %)
                       (symbol? %)
                       (and (number? %)
                            (<= Byte/MIN_VALUE
                                (:form %)
                                Byte/MAX_VALUE)))
            :int16 #(or (int16? %)
                        (symbol? %)
                        (and (number? %)
                             (<= Short/MIN_VALUE
                                 (:form %)
                                 Short/MAX_VALUE)))
            :int32 #(or (int32? %)
                        (symbol? %)
                        (and (number? %)
                             (<= Integer/MIN_VALUE
                                 (:form %)
                                 Integer/MAX_VALUE)))
            :uint8 #(or (uint8? %)
                        (symbol? %)
                        (and (number? %)
                             (<= 0
                                 (:form %)
                                 (inc (* Byte/MAX_VALUE 2)))))
            :rel8 #(or (int8? %)
                       (symbol? %))
            :rel32 #(or (int32? %)
                        (symbol? %))
            :rm8 #(or (reg8? %) (mem8? %))
            :rm16 #(or (reg16? %) (mem16? %))
            :rm32 #(or (reg32? %) (mem32? %))
            :rm64 #(or (reg64? %) (mem64? %))
            :cl #(and (reg8? %) (= 'c (:name %)))
            :r8 reg8?
            :r16 reg16?
            :r32 reg32?
            :r64 reg64?
            :m16 mem16?
            :m32 mem32?
            :m64 mem64?})

(assert (do (doseq [t (->> operators
                           vals
                           (map (comp :schemata meta))
                           flatten
                           set)]
              (assert (preds t) t))
            true))

(defn check-pred [[operand pred]]
  {:pre [(operand? operand)]}
  (when-not (preds pred)
    (error "Unknown predicate" pred))
  ((preds pred) (get-constant-value operand)))

(defn check-schema [exp schema]
  {:pre [(instruction? exp)]}
  (when (= (count (:operands exp)) (count schema))
    (if (seq schema)
      (every? check-pred (zipmap (:operands exp) schema))
      true)))

(defn check-schemata [exp schemata]
  {:pre [(instruction? exp)]}
  (when-not (some (partial check-schema exp) schemata)
    (error "invalid syntax for" exp)))

(defn check-syntax [exp]
  (when (instruction? exp)
    (let [operator (:operator exp)]
      (when-let [schemata (:schemata (meta operator))]
        (check-schemata exp schemata))))
  exp)

(defn branch? [{:keys [operator]}]
  (:branch? (meta operator)))

(defn parse-basic-blocks [exp]
  (if (defasm? exp)
    (letfn [(assoc-block [exp {:keys [index label instructions] :as block}]
              (let [exp (vary-meta exp assoc-in [:basic-blocks index] block)]
                (if label
                  (vary-meta exp assoc-in [:basic-blocks (:name label)] block)
                  exp)))]
      (loop [exp exp
             [statement & statements] (:statements exp)
             block {:index 0
                    :label (vary-meta (make-label (:name exp))
                                      merge
                                      (select-keys (meta exp)
                                                   [:tag :line :column]))
                    :instructions []}]
        (if (label? statement)
          (if (empty? (:instructions block))
            (recur exp statements (assoc-in block [:label] statement))
            (recur (assoc-block exp block)
                   statements
                   {:index (inc (:index block))
                    :label statement
                    :instructions []}))
          (let [block (update-in block [:instructions] conj statement)]
            (if (seq statements)
              (let [exp (if (branch? statement)
                          (assoc-block exp block)
                          exp)
                    block (if (branch? statement)
                            {:index (inc (:index block))
                             :instructions []}
                            block)]
                (recur exp statements block))
              (assoc-block exp block))))))
    exp))

(defn global-defined? [exp]
  (contains? @*globals* exp))

(defn define-global [exp]
  {:pre [(definition? exp)]}
  (when (global-defined? (:name exp))
    (error (:name exp) "is already defined"))
  (add-symbol-table-entry *globals* exp))

(defn analyze [exp]
  {:pre [(top-level? exp)]}
  (let [exp (-> (populate-symbol-table exp)
                (visit (comp replace-constants
                             parse-basic-blocks
                             check-syntax
                             check-symbol-reference
                             check-symbol-format)))]
    (when (definition? exp)
      (define-global exp))
    exp))
