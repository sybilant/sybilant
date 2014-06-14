;;;; Copyright © Paul Stadig. All rights reserved.
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
            [sybilant.util :refer [error form]]
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
  (when (or (defasm? exp) (label? exp))
    (when-let [tag (:tag (meta exp))]
      (reduce-kv (fn [seen {:keys [name] :as reg} tag]
                   (when (contains? seen name)
                     (error "duplicate register" reg))
                   (when (not= (:width reg) (:width tag))
                     (error "label tag expects register and tag to be same"
                            "width, but got" reg "with" tag))
                   (conj seen name))
                 #{}
                 (:tags tag))))
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
              (let [block (assoc block
                            :instructions
                            (map #(vary-meta % assoc :block-index index)
                                 instructions))
                    exp (vary-meta exp assoc-in [:basic-blocks index] block)]
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

(defn set-tag
  ([env [reg tag]]
     (set-tag env reg tag))
  ([env reg tag]
     (assoc-in env [:tags (:name reg)] tag)))

(defn get-tag [env exp]
  (let [tag (if (register? exp)
              (get-in env [:tags (:name exp)])
              (:tag (meta exp)))]
    (when-not tag
      (error "missing tag for" exp))
    tag))

(defn make-env [label-tag]
  {:pre [(label-tag? label-tag)]}
  (reduce set-tag {} (:tags label-tag)))

(defn tag=
  ([tag] true)
  ([tag0 tag1]
     (cond
      (and (or (int-tag? tag0) (uint-tag? tag0)) (number-tag? tag1))
      (<= (:min tag0) (:form tag1) (:max tag0))
      (and (number-tag? tag0) (or (int-tag? tag1) (uint-tag? tag1)))
      (<= (:min tag1) (:form tag0) (:max tag1))
      :else
      (= (:type tag0) (:type tag1))))
  ([tag0 tag1 & tags]
     (if (tag= tag0 tag1)
       (if (next tags)
         (recur tag1 (first tags) (next tags))
         (tag= tag1 (first tags)))
       false)))

(defn check-label-tag [label-tag env]
  (every? identity (for [[k v] (:tags label-tag)
                         :let [t (get-tag env k)]]
                     (when-not (= v t)
                       (error "incompatible types for" (str (form k) ":")
                              v t)))))

(declare check-basic-block)

(defn check-branch-instruction [env {:keys [operands] :as exp}]
  (let [blocks (:basic-blocks env)
        label-name (first operands)]
    (if-let [block (get blocks label-name)]
      (if-let [tag (:tag (meta (:label block)))]
        (check-label-tag tag env)
        (if (> (:index block) (:block-index (meta exp)))
          (check-basic-block env block)
          (error label-name "requires a tag")))
      (let [defasm (get (:symbol-table (meta exp)) label-name)]
        (assert defasm)
        (when-not (defasm? defasm)
          (error "target of jump instruction must be a label or defasm:"
                 label-name))
        (when-let [tag (:tag (meta defasm))]
          (check-label-tag tag env)))))
  env)

(def literal-cast
  {:int-tag {8 int8-tag
             16 int16-tag
             32 int32-tag
             64 int64-tag}
   :uint-tag {8 uint8-tag
              16 uint16-tag
              32 uint32-tag
              64 uint64-tag}})

(defmulti check-instruction-tag (comp :form :operator second list))
(defmethod check-instruction-tag '%mov [env {:keys [operands] :as exp}]
  (let [[dst src] operands
        tag (get-tag env src)
        dst-tag (if (int? src)
                  (get-in literal-cast [(:type tag) (:width dst)])
                  tag)]
    (when (and (not (number-tag? dst-tag))
               (not= (:width dst-tag) (:width dst)))
      (error dst "not compatible with tag:" dst-tag))
    (set-tag env dst dst-tag)))
(defmethod check-instruction-tag '%movsx [env {:keys [operands] :as exp}]
  (let [[dst src] operands
        tag (get-tag env src)]
    (when-not (int-tag? tag)
      (error tag "is incompatible with %movsx"))
    (set-tag env dst (get-in literal-cast [(:type tag) (:width dst)]))))
(defmethod check-instruction-tag '%movsxd [env {:keys [operands] :as exp}]
  (let [[dst src] operands
        tag (get-tag env src)]
    (when-not (int-tag? tag)
      (error tag "is incompatible with %movsxd"))
    (set-tag env dst (get-in literal-cast [(:type tag) (:width dst)]))))
(defmethod check-instruction-tag '%movzx [env {:keys [operands] :as exp}]
  (let [[dst src] operands
        tag (get-tag env src)]
    (when-not (uint-tag? tag)
      (error tag "is incompatible with %movzx"))
    (set-tag env dst (get-in literal-cast [(:type tag) (:width dst)]))))
(defmethod check-instruction-tag :default
  [env {:keys [operator operands] :as exp}]
  (if (:branch? (meta operator))
    (check-branch-instruction env exp)
    (let [tags (map (fn [operand]
                      (when (mem? operand)
                        (error operand "not allowed in checked block"))
                      (let [tag (get-tag env operand)]
                        (when (and (not (number-tag? tag))
                                   (not= (:width tag) (:width operand)))
                          (error operand "not compatible with tag:" tag))
                        tag))
                    operands)]
      (when (and (seq tags) (not (apply tag= tags)))
        (apply error "incompatible types:" (map form tags)))
      (if (number-tag? (first tags))
        (set-tag env (first operands) (second tags))
        env))))

(defn jmp? [exp]
  (and (instruction? exp)
       (= '%jmp (get-in exp [:operator :form]))))

(defn check-basic-block [env block]
  (let [blocks (:basic-blocks env)
        env (reduce check-instruction-tag env (:instructions block))]
    (when-let [block (get blocks (inc (:index block)))]
      (when-not (jmp? (last (:instructions block)))
        (if-let [tag (:tag (meta (:label block)))]
          (check-label-tag tag env)
          (recur env block))))))

(defn check-exp-tag [exp]
  (when (defasm? exp)
    (let [blocks (:basic-blocks (meta exp))]
      (doseq [block (set (vals blocks))
              :let [label-tag (:tag (meta (:label block)))]
              :when (and label-tag (nil? (:unchecked (meta label-tag))))]
        (check-basic-block (assoc (make-env label-tag) :basic-blocks blocks)
                           block))))
  exp)

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
                (visit check-symbol-format)
                (visit check-symbol-reference)
                (visit check-syntax)
                (visit parse-basic-blocks)
                (visit check-exp-tag)
                (visit replace-constants))]
    (when (definition? exp)
      (define-global exp))
    exp))
