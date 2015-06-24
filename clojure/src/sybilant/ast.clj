;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.ast
  (:refer-clojure :exclude [deftype special-symbol? symbol symbol?])
  (:require
   [clojure.core :as clj]
   [schema.core :as s]
   [sybilant.utils :refer [all?]]))

(s/defschema Width
  (s/enum 8 16 32 64))

(def ^:const +uint64-max-value+
  18446744073709551615N)

(s/defn valid-uint-value? :- s/Bool
  [obj]
  (and (integer? obj)
       (<= 0 obj +uint64-max-value+)))

(s/defschema UintValue
  (s/pred valid-uint-value? 'valid-uint-value?))

(s/defrecord UintTypeNode
    [width :- Width
     min :- UintValue
     max :- UintValue]
  (fn valid-uint-type?
    [{:keys [min max]}]
    (<= min max)))

(def UintType UintTypeNode)

(s/defn uint-type :- UintType
  [width :- Width
   min :- UintValue
   max :- UintValue]
  (->UintTypeNode width min max))

(s/defn uint-type? :- s/Bool
  [obj]
  (nil? (s/check UintType obj)))

(def ^:const +uint8-max-value+
  255)

(s/defn valid-uint8-value? :- s/Bool
  [value :- UintValue]
  (<= 0 value +uint8-max-value+))

(s/defschema Uint8Value
  (s/both UintValue
          (s/pred valid-uint8-value? 'valid-uint8-value?)))

(s/defschema Uint8Type
  (s/both UintType
          {:width (s/eq 8)
           :min Uint8Value
           :max Uint8Value}))

(s/defn uint8-type :- Uint8Type
  [min :- Uint8Value
   max :- Uint8Value]
  (uint-type 8 min max))

(s/defn uint8-type? :- s/Bool
  [obj]
  (nil? (s/check Uint8Type obj)))

(def ^:const +uint8-type+
  (uint8-type 0 +uint8-max-value+))

(s/defrecord Uint8Node
    [type :- Uint8Type
     value :- Uint8Value]
  (fn valid-uint8?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Uint8 Uint8Node)

(s/defn uint8 :- Uint8
  ([value :- Uint8Value]
   (uint8 +uint8-type+ value))
  ([type :- Uint8Type
    value :- Uint8Value]
   (->Uint8Node type value)))

(s/defn uint8? :- s/Bool
  [obj]
  (nil? (s/check Uint8 obj)))

(def ^:const +uint16-max-value+
  65535)

(s/defn valid-uint16-value? :- s/Bool
  [value :- UintValue]
  (<= 0 value +uint16-max-value+))

(s/defschema Uint16Value
  (s/both UintValue
          (s/pred valid-uint16-value? 'valid-uint16-value?)))

(s/defschema Uint16Type
  (s/both UintType
          {:width (s/eq 16)
           :min Uint16Value
           :max Uint16Value}))

(s/defn uint16-type :- Uint16Type
  [min :- Uint16Value
   max :- Uint16Value]
  (uint-type 16 min max))

(s/defn uint16-type? :- s/Bool
  [obj]
  (nil? (s/check Uint16Type obj)))

(def ^:const +uint16-type+
  (uint16-type 0 +uint16-max-value+))

(s/defrecord Uint16Node
    [type :- Uint16Type
     value :- Uint16Value]
  (fn valid-uint16?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Uint16 Uint16Node)

(s/defn uint16 :- Uint16
  ([value :- Uint16Value]
   (uint16 +uint16-type+ value))
  ([type :- Uint16Type
    value :- Uint16Value]
   (->Uint16Node type value)))

(s/defn uint16? :- s/Bool
  [obj]
  (nil? (s/check Uint16 obj)))

(def ^:const +uint32-max-value+
  4294967295)

(s/defn valid-uint32-value? :- s/Bool
  [value :- UintValue]
  (<= 0 value +uint32-max-value+))

(s/defschema Uint32Value
  (s/both UintValue
          (s/pred valid-uint32-value? 'valid-uint32-value?)))

(s/defschema Uint32Type
  (s/both UintType
          {:width (s/eq 32)
           :min Uint32Value
           :max Uint32Value}))

(s/defn uint32-type :- Uint32Type
  [min :- Uint32Value
   max :- Uint32Value]
  (uint-type 32 min max))

(s/defn uint32-type? :- s/Bool
  [obj]
  (nil? (s/check Uint32Type obj)))

(def ^:const +uint32-type+
  (uint32-type 0 +uint32-max-value+))

(s/defrecord Uint32Node
    [type :- Uint32Type
     value :- Uint32Value]
  (fn valid-uint32?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Uint32 Uint32Node)

(s/defn uint32 :- Uint32
  ([value :- Uint32Value]
   (uint32 +uint32-type+ value))
  ([type :- Uint32Type
    value :- Uint32Value]
   (->Uint32Node type value)))

(s/defn uint32? :- s/Bool
  [obj]
  (nil? (s/check Uint32 obj)))

(s/defn valid-uint64-value? :- s/Bool
  [value :- UintValue]
  (<= 0 value +uint64-max-value+))

(s/defschema Uint64Value
  (s/both UintValue
          (s/pred valid-uint64-value? 'valid-uint64-value?)))

(s/defschema Uint64Type
  (s/both UintType
          {:width (s/eq 64)
           :min Uint64Value
           :max Uint64Value}))

(s/defn uint64-type :- Uint64Type
  [min :- Uint64Value
   max :- Uint64Value]
  (uint-type 64 min max))

(s/defn uint64-type? :- s/Bool
  [obj]
  (nil? (s/check Uint64Type obj)))

(def ^:const +uint64-type+
  (uint64-type 0 +uint64-max-value+))

(s/defrecord Uint64Node
    [type :- Uint64Type
     value :- Uint64Value]
  (fn valid-uint64?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Uint64 Uint64Node)

(s/defn uint64 :- Uint64
  ([value :- Uint64Value]
   (uint64 +uint64-type+ value))
  ([type :- Uint64Type
    value :- Uint64Value]
   (->Uint64Node type value)))

(s/defn uint64? :- s/Bool
  [obj]
  (nil? (s/check Uint64 obj)))

(def ^:const +int64-min-value+
  Long/MIN_VALUE)
(def ^:const +int64-max-value+
  Long/MAX_VALUE)

(s/defn valid-int-value? :- s/Bool
  [obj]
  (and (integer? obj)
       (<= +int64-min-value+ obj +int64-max-value+)))

(s/defschema IntValue
  (s/pred valid-int-value? 'valid-int-value?))

(s/defrecord IntTypeNode
    [width :- Width
     min :- IntValue
     max :- IntValue]
  (fn valid-int8-type?
    [{:keys [min max]}]
    (<= min max)))

(def IntType IntTypeNode)

(s/defn int-type :- IntType
  [width :- Width
   min :- IntValue
   max :- IntValue]
  (->IntTypeNode width min max))

(s/defn int-type? :- s/Bool
  [obj]
  (nil? (s/check IntType obj)))

(def ^:const +int8-min-value+
  Byte/MIN_VALUE)
(def ^:const +int8-max-value+
  Byte/MAX_VALUE)

(s/defn valid-int8-value? :- s/Bool
  [value :- IntValue]
  (<= +int8-min-value+ value +int8-max-value+))

(s/defschema Int8Value
  (s/both IntValue
          (s/pred valid-int8-value? 'valid-int8-value?)))

(s/defschema Int8Type
  (s/both IntType
          {:width (s/eq 8)
           :min Int8Value
           :max Int8Value}))

(s/defn int8-type :- Int8Type
  [min :- Int8Value
   max :- Int8Value]
  (int-type 8 min max))

(s/defn int8-type? :- s/Bool
  [obj]
  (nil? (s/check Int8Type obj)))

(def ^:const +int8-type+
  (int8-type +int8-min-value+ +int8-max-value+))

(s/defrecord Int8Node
    [type :- Int8Type
     value :- Int8Value]
  (fn valid-int8?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Int8 Int8Node)

(s/defn int8 :- Int8
  ([value :- Int8Value]
   (int8 +int8-type+ value))
  ([type :- Int8Type
    value :- Int8Value]
   (->Int8Node type value)))

(s/defn int8? :- s/Bool
  [obj]
  (nil? (s/check Int8 obj)))

(def ^:const +int16-min-value+
  Short/MIN_VALUE)
(def ^:const +int16-max-value+
  Short/MAX_VALUE)

(s/defn valid-int16-value? :- s/Bool
  [value :- IntValue]
  (<= +int16-min-value+ value +int16-max-value+))

(s/defschema Int16Value
  (s/both IntValue
          (s/pred valid-int16-value? 'valid-int16-value?)))

(s/defschema Int16Type
  (s/both IntType
          {:width (s/eq 16)
           :min Int16Value
           :max Int16Value}))

(s/defn int16-type :- Int16Type
  [min :- Int16Value
   max :- Int16Value]
  (int-type 16 min max))

(s/defn int16-type? :- s/Bool
  [obj]
  (nil? (s/check Int16Type obj)))

(def ^:const +int16-type+
  (int16-type +int16-min-value+ +int16-max-value+))

(s/defrecord Int16Node
    [type :- Int16Type
     value :- Int16Value]
  (fn valid-int16?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Int16 Int16Node)

(s/defn int16 :- Int16
  ([value :- Int16Value]
   (int16 +int16-type+ value))
  ([type :- Int16Type
    value :- Int16Value]
   (->Int16Node type value)))

(s/defn int16? :- s/Bool
  [obj]
  (nil? (s/check Int16 obj)))

(def ^:const +int32-min-value+
  Integer/MIN_VALUE)
(def ^:const +int32-max-value+
  Integer/MAX_VALUE)

(s/defn valid-int32-value? :- s/Bool
  [value :- IntValue]
  (<= +int32-min-value+ value +int32-max-value+))

(s/defschema Int32Value
  (s/both IntValue
          (s/pred valid-int32-value? 'valid-int32-value?)))

(s/defschema Int32Type
  (s/both IntType
          {:width (s/eq 32)
           :min Int32Value
           :max Int32Value}))

(s/defn int32-type :- Int32Type
  [min :- Int32Value
   max :- Int32Value]
  (int-type 32 min max))

(s/defn int32-type? :- s/Bool
  [obj]
  (nil? (s/check Int32Type obj)))

(def ^:const +int32-type+
  (int32-type +int32-min-value+ +int32-max-value+))

(s/defrecord Int32Node
    [type :- Int32Type
     value :- Int32Value]
  (fn valid-int32?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Int32 Int32Node)

(s/defn int32 :- Int32
  ([value :- Int32Value]
   (int32 +int32-type+ value))
  ([type :- Int32Type
    value :- Int32Value]
   (->Int32Node type value)))

(s/defn int32? :- s/Bool
  [obj]
  (nil? (s/check Int32 obj)))

(s/defn valid-int64-value? :- s/Bool
  [value :- IntValue]
  (<= +int64-min-value+ value +int64-max-value+))

(s/defschema Int64Value
  (s/both IntValue
          (s/pred valid-int64-value? 'valid-int64-value?)))

(s/defschema Int64Type
  (s/both IntType
          {:width (s/eq 64)
           :min Int64Value
           :max Int64Value}))

(s/defn int64-type :- Int64Type
  [min :- Int64Value
   max :- Int64Value]
  (int-type 64 min max))

(s/defn int64-type? :- s/Bool
  [obj]
  (nil? (s/check Int64Type obj)))

(def ^:const +int64-type+
  (int64-type +int64-min-value+ +int64-max-value+))

(s/defrecord Int64Node
    [type :- Int64Type
     value :- Int64Value]
  (fn valid-int64?
    [{{min :min max :max} :type value :value}]
    (<= min value max)))

(def Int64 Int64Node)

(s/defn int64 :- Int64
  ([value :- Int64Value]
   (int64 +int64-type+ value))
  ([type :- Int64Type
    value :- Int64Value]
   (->Int64Node type value)))

(s/defn int64? :- s/Bool
  [obj]
  (nil? (s/check Int64 obj)))

(s/defrecord SymbolNode
    [name :- s/Symbol])

(def Symbol SymbolNode)

(s/defn symbol :- Symbol
  [name :- s/Symbol]
  (->SymbolNode name))

(s/defn symbol? :- s/Bool
  [obj]
  (nil? (s/check Symbol obj)))

(s/defn valid-special-symbol-value? :- s/Bool
  [obj]
  (= \% (first (name obj))))

(s/defschema SpecialSymbol
  (s/both Symbol
          {:name (s/pred valid-special-symbol-value?
                         'valid-special-symbol-value?)
           s/Any s/Any}))

(s/defn special-symbol? :- s/Bool
  [obj]
  (nil? (s/check SpecialSymbol obj)))

(s/defrecord RegisterNode
    [name :- SpecialSymbol])

(def Register RegisterNode)

(s/defn register :- Register
  [name :- SpecialSymbol]
  (->RegisterNode name))

(s/defn register? :- s/Bool
  [obj]
  (nil? (s/check Register obj)))

(s/defschema PreciseIntegerType
  (s/either Uint8Type Uint16Type Uint32Type Uint64Type
            Int8Type Int16Type Int32Type Int64Type))

(s/defn precise-integer-type? :- s/Bool
  [obj]
  (nil? (s/check PreciseIntegerType obj)))

(declare Type*)

(s/defschema Type
  (s/recursive #'Type*))

(s/defrecord TupleTypeNode
    [types :- [Type]])

(def TupleType TupleTypeNode)

(s/defrecord TextTypeNode
    [types :- {Register Type}])

(def TextType TextTypeNode)

(s/defschema Type*
  (s/either PreciseIntegerType TupleType TextType))

(s/defn type? :- s/Bool
  [obj]
  (nil? (s/check Type obj)))

(s/defn tuple-type :- TupleType
  [types :- [Type]]
  (->TupleTypeNode types))

(s/defn tuple-type? :- s/Bool
  [obj]
  (nil? (s/check TupleType obj)))

(s/defn text-type :- TextType
  [types :- {Register Type}]
  (->TextTypeNode types))

(s/defn text-type? :- s/Bool
  [obj]
  (nil? (s/check TextType obj)))

(s/defschema LabelType
  (s/either TupleType TextType))

(s/defn label-type? :- s/Bool
  [obj]
  (nil? (s/check LabelType obj)))

(s/defrecord LabelNode
    [name :- Symbol]
  {(s/optional-key :tag) (s/maybe LabelType)})

(def Label LabelNode)

(s/defn label :- Label
  ([name :- Symbol]
   (->LabelNode name))
  ([name :- Symbol
    tag :- (s/maybe LabelType)]
   (let [obj (label name)]
     (if tag
       (assoc obj :tag tag)
       obj))))

(s/defn label? :- s/Bool
  [obj]
  (nil? (s/check Label obj)))

(s/defschema TaggedLabel
  (s/both Label
          {:tag Type
           s/Any s/Any}))

(s/defn tagged-label? :- s/Bool
  [obj]
  (nil? (s/check TaggedLabel obj)))

(s/defschema UntaggedLabel
  (s/both Label
          {(s/optional-key :tag) (s/eq nil)
           s/Any s/Any}))

(s/defn untagged-label? :- s/Bool
  [obj]
  (nil? (s/check UntaggedLabel obj)))

(s/defrecord DeftypeNode
    [name :- UntaggedLabel
     value :- Type])

(def Deftype DeftypeNode)

(s/defn deftype :- Deftype
  [name :- UntaggedLabel
   value :- Type]
  (->DeftypeNode name value))

(s/defn deftype? :- s/Bool
  [obj]
  (nil? (s/check Deftype obj)))

(s/defschema PreciseInteger
  (s/either Uint8 Uint16 Uint32 Uint64 Int8 Int16 Int32 Int64))

(s/defn precise-integer? :- s/Bool
  [obj]
  (nil? (s/check PreciseInteger obj)))

(s/defschema Const
  (s/either PreciseInteger))

(s/defrecord DefconstNode
    [name :- UntaggedLabel
     value :- Const])

(def Defconst DefconstNode)

(s/defn defconst :- Defconst
  [name :- UntaggedLabel
   value :- Type]
  (->DefconstNode name value))

(s/defn defconst? :- s/Bool
  [obj]
  (nil? (s/check Defconst obj)))

(s/defrecord DefimportNode
    [name :- Label])

(def Defimport DefimportNode)

(s/defn defimport :- Defimport
  [name :- Label]
  (->DefimportNode name))

(s/defn defimport? :- s/Bool
  [obj]
  (nil? (s/check Defimport obj)))

(s/defschema DataTupleType
  (s/both TupleType
          {:types [PreciseIntegerType]}))

(s/defn data-tuple-type :- DataTupleType
  [types :- [PreciseIntegerType]]
  (->TupleTypeNode types))

(s/defn data-tuple-type? :- s/Bool
  [obj]
  (nil? (s/check DataTupleType obj)))

(s/defschema Data
  [PreciseInteger])

(s/defn data? :- s/Bool
  [obj]
  (nil? (s/check Data obj)))

(s/defschema DataLabel
  (s/both TaggedLabel
          {(s/optional-key :tag) (s/maybe DataTupleType)
           s/Any s/Any}))

(s/defn data-label? :- s/Bool
  [obj]
  (nil? (s/check DataLabel obj)))

(declare subtype?)

(s/defrecord DefdataNode
    [name :- DataLabel
     data :- Data]
  (fn valid-data?
    [{:keys [name data]}]
    (or (nil? (:tag name))
        (let [types (get-in name [:tag :types])]
          (and (= (count data) (count types))
               (all? subtype? (map :type data) types))))))

(def Defdata DefdataNode)

(s/defn defdata :- Defdata
  ([name :- DataLabel
    data :- Data]
   (defdata name (map :type data) data))
  ([name :- DataLabel
    type :- DataTupleType
    data :- Data]
   (->DefdataNode name type data)))

(s/defn defdata? :- s/Bool
  [obj]
  (nil? (s/check Defdata obj)))

(s/defrecord OperatorNode
    [name :- SpecialSymbol])

(def Operator OperatorNode)

(s/defn operator :- Operator
  [name :- SpecialSymbol]
  (->OperatorNode name))

(s/defn operator? :- s/Bool
  [obj]
  (nil? (s/check Operator obj)))

(s/defrecord TupleReferenceNode
    [tuple :- Symbol
     slot :- Uint64])

(def TupleReference TupleReferenceNode)

(s/defn tuple-reference :- TupleReference
  [tuple :- Symbol
   slot :- Uint64]
  (->TupleReferenceNode tuple slot))

(s/defn tuple-reference? :- s/Bool
  [obj]
  (nil? (s/check TupleReference obj)))

(s/defschema MemOperand
  (s/either Register Symbol))

(s/defn mem-operand? :- s/Bool
  [obj]
  (nil? (s/check MemOperand obj)))

(s/defschema MemOperands
  [MemOperand])

(s/defn mem-operands? :- s/Bool
  [obj]
  (nil? (s/check MemOperands obj)))

(s/defrecord MemNode
    [operands :- MemOperands]
  {(s/optional-key :width) (s/maybe Width)})

(def Mem MemNode)

(s/defn mem :- Mem
  ([operands :- MemOperands]
   (->MemNode operands))
  ([operands :- MemOperands
    width :- (s/maybe Width)]
   (let [obj (mem operands)]
     (if width
       (assoc obj :width width)
       obj))))

(s/defn mem? :- s/Bool
  [obj]
  (nil? (s/check Mem obj)))

(s/defschema Mem8
  (s/both Mem
          {:width (s/eq 8)
           s/Any s/Any}))

(s/defn mem8 :- Mem8
  [operands :- MemOperands]
  (mem operands 8))

(s/defn mem8? :- s/Bool
  [obj]
  (nil? (s/check Mem8 obj)))

(s/defschema Mem16
  (s/both Mem
          {:width (s/eq 16)
           s/Any s/Any}))

(s/defn mem16 :- Mem16
  [operands :- MemOperands]
  (mem operands 16))

(s/defn mem16? :- s/Bool
  [obj]
  (nil? (s/check Mem16 obj)))

(s/defschema Mem32
  (s/both Mem
          {:width (s/eq 32)
           s/Any s/Any}))

(s/defn mem32 :- Mem32
  [operands :- MemOperands]
  (mem operands 32))

(s/defn mem32? :- s/Bool
  [obj]
  (nil? (s/check Mem32 obj)))

(s/defschema Mem64
  (s/both Mem
          {:width (s/eq 64)
           s/Any s/Any}))

(s/defn mem64 :- Mem64
  [operands :- MemOperands]
  (mem operands 64))

(s/defn mem64? :- s/Bool
  [obj]
  (nil? (s/check Mem64 obj)))

(s/defschema UntypedMemoryReference
  (s/either Mem Mem8 Mem16 Mem32 Mem64))

(s/defn untyped-memory-reference? :- s/Bool
  [obj]
  (nil? (s/check UntypedMemoryReference obj)))

(s/defschema MemoryReference
  (s/either TupleReference UntypedMemoryReference))

(s/defn memory-reference? :- s/Bool
  [obj]
  (nil? (s/check MemoryReference obj)))

(s/defschema Operand
  (s/either PreciseInteger Register MemoryReference Symbol))

(s/defn operand? :- s/Bool
  [obj]
  (nil? (s/check Operand obj)))

(s/defschema Operands
  [Operand])

(s/defn operands? :- s/Bool
  [obj]
  (nil? (s/check Operands obj)))

(s/defrecord InstructionNode
    [operator :- Operator
     operands :- Operands])

(def Instruction InstructionNode)

(s/defn instruction :- Instruction
  ([operator :- Operator]
   (instruction operator []))
  ([operator :- Operator
    operands :- Operands]
   (->InstructionNode operator operands)))

(s/defn instruction? :- s/Bool
  [obj]
  (nil? (s/check Instruction obj)))

(s/defschema Statement
  (s/either Label Instruction))

(s/defn statement? :- s/Bool
  [obj]
  (nil? (s/check Statement obj)))

(s/defschema Statements
  [(s/one Instruction 'Instruction) Statement])

(s/defn statements? :- s/Bool
  [obj]
  (nil? (s/check Statements obj)))

(s/defrecord DeftextNode
    [name :- Label
     statements :- Statements])

(def Deftext DeftextNode)

(s/defn deftext :- Deftext
  [name :- Label
   statements :- Statements]
  (->DeftextNode name statements))

(s/defn deftext? :- s/Bool
  [obj]
  (nil? (s/check Deftext obj)))

(defmulti subtype? (fn [t0 t1] [(class t0) (class t1)]))
(defmethod subtype? [UintType UintType]
  [{width0 :width min0 :min max0 :max} {width1 :width min1 :min max1 :max}]
  (and (= width0 width1)
       (<= min1 min0 max0 max1)))
(defmethod subtype? [IntType IntType]
  [{width0 :width min0 :min max0 :max} {width1 :width min1 :min max1 :max}]
  (and (= width0 width1)
       (<= min1 min0 max0 max1)))
(defmethod subtype? [TupleType TupleType]
  [{types0 :types} {types1 :types}]
  (and (<= (count types0) (count types1))
       (all? subtype? types0 types1)))
(defmethod subtype? :default
  [t0 t1]
  false)
