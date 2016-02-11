;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.ast
  (:refer-clojure :exclude [defn defrecord int])
  (:require
   [schema.core :refer [Bool cond-pre constrained defn defrecord defschema enum
                        optional-key recursive Symbol]]))

(defschema Signedness
  (enum :unsigned :signed))

(defschema Width
  (enum 8 16 32 64))

(def ^:const +sint64-min-value+
  -9223372036854775808)
(def ^:const +uint64-max-value+
  18446744073709551615N)

(defn int-value? :- Bool
  [obj]
  (and (integer? obj)
       (<= +sint64-min-value+ obj +uint64-max-value+)))

(defschema IntValue
  (constrained schema.core/Int int-value? 'int-value?))

(def ^:const +sint8-min-value+
  -128)
(def ^:const +sint16-min-value+
  -32768)
(def ^:const +sint32-min-value+
  -2147483648)

(defn signed-int-min-value :- IntValue
  [width :- Width]
  (case width
    8 +sint8-min-value+
    16 +sint16-min-value+
    32 +sint32-min-value+
    64 +sint64-min-value+))

(defn int-min-value :- IntValue
  [width :- Width
   signedness :- Signedness]
  (case signedness
    :unsigned 0
    :signed (signed-int-min-value width)))

(def ^:const +uint8-max-value+
  255)
(def ^:const +uint16-max-value+
  65535)
(def ^:const +uint32-max-value+
  4294967295)

(defn unsigned-int-max-value :- IntValue
  [width :- Width]
  (case width
    8 +uint8-max-value+
    16 +uint16-max-value+
    32 +uint32-max-value+
    64 +uint64-max-value+))

(def ^:const +sint8-max-value+
  127)
(def ^:const +sint16-max-value+
  32767)
(def ^:const +sint32-max-value+
  2147483647)
(def ^:const +sint64-max-value+
  9223372036854775807)

(defn signed-int-max-value :- IntValue
  [width :- Width]
  (case width
    8 +sint8-max-value+
    16 +sint16-max-value+
    32 +sint32-max-value+
    64 +sint64-max-value+))

(defn int-max-value :- IntValue
  [width :- Width
   signedness :- Signedness]
  (case signedness
    :unsigned (unsigned-int-max-value width)
    :signed (signed-int-max-value width)))

(defrecord IntTagNode
    [min :- IntValue
     max :- IntValue]
  {(optional-key :width) Width
   (optional-key :signedness) Signedness})

(defn min-less-than-or-equal-to-max? :- Bool
  [{:keys [min max]} :- IntTagNode]
  (<= min max))

(defn min-greater-than-or-equal-to-type-min? :- Bool
  [{:keys [min width signedness]} :- IntTagNode]
  (let [width (or width 64)]
    (<= (int-min-value width (or signedness :signed)) min)))

(defn max-less-than-or-equal-to-type-max? :- Bool
  [{:keys [max width signedness]} :- IntTagNode]
  (let [width (or width 64)]
    (<= max (int-max-value width (or signedness :unsigned)))))

(defschema IntTag
  (-> IntTagNode
      (constrained min-less-than-or-equal-to-max?
                   'min-less-than-or-equal-to-max?)
      (constrained min-greater-than-or-equal-to-type-min?
                   'min-greater-than-or-equal-to-type-min?)
      (constrained max-less-than-or-equal-to-type-max?
                   'max-less-than-or-equal-to-type-max?)))

(declare Tag*)

(defschema Tag
  (recursive #'Tag*))

(defschema Tags
  (constrained [Tag] seq 'tags?))

(defrecord TupleTagNode
    [tags :- Tags])

(def TupleTag
  TupleTagNode)

(defrecord RegisterNode
    [name :- Symbol
     width :- Width])

(def Register
  RegisterNode)

(defrecord TextTagNode
    [tags :- {Register Tag}])

(def TextTag
  TextTagNode)

(defschema Tag*
  (cond-pre IntTag TupleTag TextTag))

(defrecord IntNode
    [value :- IntValue
     tag :- IntTag])

(defn value-greater-than-or-equal-to-min? :- Bool
  [{{:keys [min]} :tag value :value} :- IntNode]
  (<= min value))

(defn value-less-than-or-equal-to-max? :- Bool
  [{{:keys [max]} :tag value :value} :- IntNode]
  (<= value max))

(defschema Int
  (-> IntNode
      (constrained value-greater-than-or-equal-to-min?
                   'value-greater-than-or-equal-to-min?)
      (constrained value-less-than-or-equal-to-max?
                   'value-less-than-or-equal-to-max?)))

(defrecord LabelNode
    [name :- Symbol]
  {(optional-key :tag) Tag})

(def Label
  LabelNode)

(defrecord DefimportNode
    [label :- Label])

(def Defimport
  DefimportNode)

(defschema ConstValue
  (cond-pre Int Symbol))

(defrecord DefconstNode
    [name :- Symbol
     value :- ConstValue])

(def Defconst
  DefconstNode)

(def DataTag
  (cond-pre TupleTag))

(defn data-label? :- Bool
  [obj]
  (and (instance? Label obj)
       (let [tag (:tag obj)]
         (or (nil? tag)
             (instance? DataTag tag)))))

(defschema DataLabel
  (constrained Label data-label? 'data-label?))

(defschema DataValue
  (cond-pre Int Symbol))

(defrecord DefdataNode
    [label :- DataLabel
     ;; an empty value indicates a declaration
     value :- [DataValue]])

(def Defdata
  DefdataNode)

(defrecord OperatorNode
    [name :- Symbol])

(def Operator
  OperatorNode)

(defschema PointerArgument
  (cond-pre Int Register Symbol))

(defschema PointerArguments
  (constrained [PointerArgument] seq 'pointer-arguments?))

(defrecord PointerNode
    [name :- Symbol
     arguments :- PointerArguments])

(def Pointer
  PointerNode)

(defschema Operand
  (cond-pre Int Register Pointer Symbol))

(defrecord InstructionNode
    [operator :- Operator
     operands :- [Operand]])

(def Instruction
  InstructionNode)

(defschema Statement
  (cond-pre Label Instruction))

(defn text-label? :- Bool
  [obj]
  (and (instance? Label obj)
       (let [tag (:tag obj)]
         (or (nil? tag)
             (instance? TextTag tag)))))

(defschema TextLabel
  (constrained Label text-label? 'text-label?))

(defrecord DeftextNode
    [label :- TextLabel
     ;; an empty statements indicates a declaration
     statements :- [Statement]])

(def Deftext
  DeftextNode)

(defn int-tag :- IntTag
  ([min :- IntValue
    max :- IntValue]
   (->IntTagNode min max))
  ([min :- IntValue
    max :- IntValue
    width :- Width]
   (assoc (int-tag min max) :width width))
  ([min :- IntValue
    max :- IntValue
    width :- Width
    signedness :- Signedness]
   (assoc (int-tag min max width) :signedness signedness)))

(defn tuple-tag :- TupleTag
  [tags :- Tags]
  (->TupleTagNode tags))

(defn register :- Register
  [name :- Symbol
   width :- Width]
  (->RegisterNode name width))

(defn text-tag :- TextTag
  [tags :- {Register Tag}]
  (->TextTagNode tags))

(defn int :- Int
  ([value :- IntValue]
   (int value (int-tag value value)))
  ([value :- IntValue
    tag :- IntTag]
   (->IntNode value tag)))

(defn label :- Label
  ([name :- Symbol]
   (->LabelNode name))
  ([name :- Symbol
    tag :- Tag]
   (assoc (label name) :tag tag)))

(defn defimport :- Defimport
  [label :- Label]
  (->DefimportNode label))

(defn defconst :- Defconst
  [name :- Symbol
   value :- ConstValue]
  (->DefconstNode name value))

(defn defdata :- Defdata
  ([label :- Label]
   (defdata label []))
  ([label :- Label
    value :- [DataValue]]
   (->DefdataNode label value)))

(defn operator :- Operator
  [name :- Symbol]
  (->OperatorNode name))

(defn pointer :- Pointer
  ([name :- Symbol]
   (pointer name []))
  ([name :- Symbol
    arguments :- [PointerArgument]]
   (->PointerNode name arguments)))

(defn instruction :- Instruction
  ([operator :- Operator]
   (instruction operator []))
  ([operator :- Operator
    operands :- [Operand]]
   (->InstructionNode operator operands)))

(defn deftext :- Deftext
  ([label :- Label]
   (deftext label []))
  ([label :- Label
    statements :- [Statement]]
   (->DeftextNode label statements)))
