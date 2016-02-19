;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.parser
  (:refer-clojure :exclude [defn])
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [schema.core :refer [Bool defn maybe one Str Symbol]]
   [sybilant.ast :as ast]))

(defmacro try-parse
  [body clause]
  `(try
     ~body
     (catch Exception e#
       (if (= :syntax-error (:error (ex-data e#)))
         ~clause
         (throw e#)))))

(defn syntax-error
  [& msg]
  (throw (ex-info (str/join " " msg) {:error :syntax-error})))

(defn example :- Str
  [form]
  (str "'" (pr-str form) "'"))

(defn validate-tagged-list
  ([form tag-sym min-args]
   (validate-tagged-list form tag-sym min-args nil))
  ([form tag-sym min-args max-args]
   (when-not (list? form)
     (syntax-error "Expected list, but was" (example form)))
   (let [tag (first form)]
     (cond
       (symbol? tag-sym) (when-not (= tag-sym tag)
                           (syntax-error "Expected" (example tag-sym) "as first"
                                         "element of list, but was"
                                         (example tag)))
       :else (do (assert (set? tag-sym))
                 (when-not (contains? tag-sym tag)
                   (syntax-error "Expected first element to be one of"
                                 (str (example tag-sym) ",") "but was"
                                 (example tag)))))
     (let [arg-count (count (rest form))]
       (when-not (<= min-args arg-count)
         (syntax-error tag "expects at least" min-args "arguments, but got"
                       arg-count))
       (when-not (or (nil? max-args) (<= arg-count max-args))
         (syntax-error tag "expects at most" max-args "arguments, but got"
                       arg-count))))))

(defn with-form-meta
  [form exp]
  (with-meta exp (select-keys (meta form) [:file :line :column])))

(defn parse-int-value :- ast/IntValue
  [form]
  (when-not (integer? form)
    (syntax-error "Expected an integer value, but got" (example form)))
  (when-not (<= ast/+sint64-min-value+ form)
    (syntax-error form "is below the minimum value for an int"
                  (example ast/+sint64-min-value+)))
  (when-not (<= form ast/+uint64-max-value+)
    (syntax-error form "is above the maximum value for an int"
                  (example ast/+uint64-max-value+)))
  form)

(def ^:const sint-tag-sym
  '%sint)

(def ^:const uint-tag-sym
  '%uint)

(def ^:const int-tag-syms
  #{'%int sint-tag-sym uint-tag-sym})

(defn int-tag-form? :- Bool
  [obj]
  (and (list? obj)
       (contains? int-tag-syms (first obj))))

(defn parse-int-tag :- ast/IntTag
  [[tag-sym min max & rest :as form]]
  {:pre [(list? form)]}
  (validate-tagged-list form int-tag-syms 2 2)
  (let [signedness (condp = tag-sym
                     sint-tag-sym :signed
                     uint-tag-sym :unsigned
                     nil)
        min (parse-int-value min)
        max (parse-int-value max)]
    (with-form-meta form (ast/int-tag min max signedness))))

(declare parse-tag)

(defn parse-tuple-tag :- ast/TupleTag
  [form]
  {:pre [(vector? form)]}
  (when-not (pos? (count form))
    (syntax-error "Expected at least one tag"))
  (with-form-meta form (ast/tuple-tag (mapv parse-tag form))))

(defn symbol-form? :- Bool
  [form]
  (symbol? form))

(defn parse-symbol :- Symbol
  [form]
  (when-not (symbol-form? form)
    (syntax-error "Expected a symbol, but got" (example form)))
  form)

;; The register and address parsing are admittedly x86 specific at the moment.
(def registers
  (edn/read {:readers {'sybilant.ast.RegisterNode ast/map->RegisterNode}}
            (java.io.PushbackReader. (io/reader (io/resource "registers.edn")))))

(defn parse-register :- ast/Register
  [form]
  (if-let [reg (get registers form)]
    (with-form-meta form reg)
    (syntax-error "Expected register, but got" (example form))))

(defn register-form? :- Bool
  [form]
  (contains? registers form))

(defn parse-text-tag :- ast/TextTag
  [form]
  {:pre [(map? form)]}
  (when-not (seq form)
    (syntax-error "Expected at least one tag"))
  (with-form-meta form (ast/text-tag (zipmap (map parse-register (keys form))
                                             (map parse-tag (vals form))))))

(defn tag-form? :- Bool
  [obj]
  (or (int-tag-form? obj) (vector? obj) (map? obj)))

(defn parse-tag :- ast/Tag
  [form]
  (cond
    (int-tag-form? form) (parse-int-tag form)
    (vector? form) (parse-tuple-tag form)
    (map? form) (parse-text-tag form)
    :else (syntax-error "Expected a tag, but got" (example form))))

(defn int-form? :- Bool
  [form]
  (integer? form))

(defn parse-int :- ast/Int
  [form]
  (ast/int (parse-int-value form)))

(def ^:const label-sym
  '%label)

(defn label-form? :- Bool
  [form]
  (and (list? form)
       (= label-sym (first form))))

(defn parse-label :- ast/Label
  [[_ sym tag? :as form]]
  (validate-tagged-list form label-sym 1 2)
  (when-not (symbol-form? sym)
    (syntax-error label-sym "expects a symbol as its first argument, but got"
                  (example sym)))
  (when-not (or (nil? tag?) (tag-form? tag?))
    (syntax-error label-sym "expects a tag as its second argument, but got"
                  (example tag?)))
  (with-form-meta form
    (ast/label (parse-symbol sym)
               (when tag? (parse-tag tag?)))))

(def ^:const defimport-sym
  '%defimport)

(defn parse-defimport :- ast/Defimport
  [[_ label :as form]]
  (validate-tagged-list form defimport-sym 1 1)
  (when-not (label-form? label)
    (syntax-error defimport-sym "expects a label as its argument,"
                  "but got" (example label)))
  (with-form-meta form (ast/defimport (parse-label label))))

(defn const-value-form? :- Bool
  [obj]
  (or (int-form? obj) (symbol-form? obj)))

(defn parse-const-value :- ast/ConstValue
  [form]
  (cond
    (int-form? form) (parse-int form)
    (symbol-form? form) (parse-symbol form)
    :else (syntax-error "Expected a constant value, but got" (example form))))

(def ^:const defconst-sym
  '%defconst)

(defn parse-defconst :- ast/Defconst
  [[_ name value :as form]]
  (validate-tagged-list form defconst-sym 2 2)
  (when-not (symbol-form? name)
    (syntax-error defconst-sym "expects a symbol as its first argument, but got"
                  (example name)))
  (when-not (const-value-form? value)
    (syntax-error defconst-sym "expects a constant value as its second"
                  "argument, but got" (example value)))
  (with-form-meta form
    (ast/defconst (parse-symbol name)
      (parse-const-value value))))

(defn parse-data-label :- ast/DataLabel
  [[_ name tag? :as form]]
  (let [label (parse-label form)]
    (when-not (or (nil? (:tag label)) (ast/tuple-tag? (:tag label)))
      (syntax-error "Data label expects a tuple tag, but got" (example tag?)))
    label))

(defn parse-data-value :- ast/DataValue
  [form]
  (cond
    (int-form? form) (parse-int form)
    (symbol-form? form) (parse-symbol form)
    :else (syntax-error "Expected a data value, but got" (example form))))

(def ^:const defdata-sym
  '%defdata)

(defn parse-defdata :- ast/Defdata
  [[_ data-label data-value? :as form]]
  (validate-tagged-list form defdata-sym 1 2)
  (when-not (label-form? data-label)
    (syntax-error defdata-sym "expects a label as its first argument, but got"
                  (example data-label)))
  (let [data-label (parse-data-label data-label)
        data-value? (mapv parse-data-value data-value?)]
    (with-form-meta form (ast/defdata data-label data-value?))))

(defn parse-operator :- ast/Operator
  [form]
  (when-not (symbol-form? form)
    (syntax-error "Expected operator, but got" (example form)))
  (with-form-meta form (ast/operator (parse-symbol form))))

(defn parse-base :- ast/BaseRegister
  [form]
  (let [reg (parse-register form)]
    (when-not (ast/base-register? reg)
      (syntax-error "Expected base register, but got" (example form)))
    reg))

(defn parse-index :- ast/BaseRegister
  [form]
  (let [reg (parse-register form)]
    (when-not (ast/index-register? reg)
      (syntax-error "Expected index register, but got" (example form)))
    reg))

(defn disp-int-form? :- Bool
  [obj]
  (and (integer? obj)
       (ast/disp-value? obj)))

(defn parse-disp :- ast/Displacement
  [form]
  (cond
    (symbol-form? form) (parse-symbol form)
    (disp-int-form? form) (parse-int form)
    :else (syntax-error "Expected displacement, but got" (example form))))

(defn scale-form? :- Bool
  [obj]
  (and (integer? obj)
       (ast/scale-value? obj)))

(defn parse-scale :- ast/Scale
  [form]
  (when-not (scale-form? form)
    (syntax-error "Expected scale, but got" (example form)))
  (parse-int form))

(defn parse-address-args :- [(one (maybe ast/BaseRegister) 'base-register)
                             (one (maybe ast/IndexRegister) 'index-register)
                             (one (maybe ast/Scale) 'scale)
                             (one (maybe ast/Displacement) 'displacement)]
  [sym [a b c d :as args]]
  (case (count args)
    1
    (try-parse
     (if (register-form? a)
       [(parse-base a) nil nil nil]
       [nil nil nil (parse-disp a)])
     (syntax-error sym "expected either a base register or displacement,"
                   "but got" (example a)))
    2
    [(parse-base a)
     nil
     nil
     (parse-disp b)]
    3
    (if (scale-form? b)
      [nil (parse-index a) (parse-scale b) (parse-disp c)]
      (let [index (try-parse
                   (parse-index b)
                   (syntax-error sym "expected either a scale or an index"
                                 "register, but got" (example b)))]
        [(parse-base a) index nil (parse-disp c)]))
    4
    [(parse-base a)
     (parse-index b)
     (parse-scale c)
     (parse-disp d)]))

(def ^:const addr-sym
  '%addr)

(defn address-form? :- Bool
  [form]
  (and (list? form)
       (= addr-sym (first form))))

(defn parse-address :- ast/Address
  [[sym & args :as form]]
  (let [[base index scale disp] (parse-address-args sym args)]
    (with-form-meta form (ast/address base index scale disp))))

(defn parse-operand :- ast/Operand
  [form]
  (cond
    (int-form? form) (parse-int form)
    (register-form? form) (parse-register form)
    (address-form? form) (parse-address form)
    (symbol-form? form) (parse-symbol form)
    :else (syntax-error "Expected an operand, but got" (example form))))

(defn instruction-form? :- Bool
  [form]
  (and (list? form)
       (not= label-sym (first form))))

(defn parse-instruction :- ast/Instruction
  [[operator-sym & operands? :as form]]
  (when-not (symbol-form? operator-sym)
    (syntax-error "Instruction expects an operator as its first argument, but"
                  "got" (example operator-sym)))
  (with-form-meta form
    (ast/instruction (parse-operator operator-sym)
                     (mapv parse-operand operands?))))

(defn parse-statement :- ast/Statement
  [form]
  (cond
    (label-form? form) (parse-label form)
    (instruction-form? form) (parse-instruction form)
    :else (syntax-error "Expected statement, but got" (example form))))

(def ^:const deftext-sym
  '%deftext)

(defn parse-deftext :- ast/Deftext
  [[_ label & statements? :as form]]
  (validate-tagged-list form deftext-sym 1)
  (when-not (label-form? label)
    (syntax-error deftext-sym "expects a label as its first argument, but got"
                  (example label)))
  (with-form-meta form
    (ast/deftext (parse-label label)
      (mapv parse-statement statements?))))
