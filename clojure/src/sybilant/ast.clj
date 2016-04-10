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
                        maybe optional-key pred recursive Symbol]]))

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
  {(optional-key :signedness) Signedness
   (optional-key :width) Width})

(defn int-tag? :- Bool
  [obj]
  (instance? IntTagNode obj))

(defschema IntTag*
  (pred #'int-tag? 'int-tag?))

(defn min-less-than-or-equal-to-max? :- Bool
  [{:keys [min max]} :- IntTag*]
  (<= min max))

(defn min-greater-than-or-equal-to-type-min? :- Bool
  [{:keys [min width signedness]} :- IntTag*]
  (let [width (or width 64)]
    (<= (int-min-value width (or signedness :signed)) min)))

(defn max-less-than-or-equal-to-type-max? :- Bool
  [{:keys [max width signedness]} :- IntTag*]
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

(defn tuple-tag? :- Bool
  [obj]
  (instance? TupleTagNode obj))

(def TupleTag
  TupleTagNode)

(defrecord RegisterNode
    [name :- Symbol
     width :- Width])

(defn register? :- Bool
  [obj]
  (instance? RegisterNode obj))

(def Register
  RegisterNode)

(defrecord TextTagNode
    [tags :- {Register Tag}])

(defn text-tag? :- Bool
  [obj]
  (instance? TextTagNode obj))

(def TextTag
  TextTagNode)

(defschema Tag*
  (cond-pre IntTag TupleTag TextTag))

(defrecord IntNode
    [value :- IntValue
     tag :- IntTag])

(defn int? :- Bool
  [obj]
  (instance? IntNode obj))

(defschema Int*
  (pred #'int? 'int?))

(defn value-greater-than-or-equal-to-min? :- Bool
  [{{:keys [min]} :tag value :value} :- Int*]
  (<= min value))

(defn value-less-than-or-equal-to-max? :- Bool
  [{{:keys [max]} :tag value :value} :- Int*]
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

(defn label? :- Bool
  [obj]
  (instance? LabelNode obj))

(def Label
  LabelNode)

(defrecord DefimportNode
    [label :- Label])

(defn defimport? :- Bool
  [obj]
  (instance? DefimportNode obj))

(def Defimport
  DefimportNode)

(defschema ConstValue
  (cond-pre Int Symbol))

(defrecord DefconstNode
    [name :- Symbol
     value :- ConstValue])

(defn defconst? :- Bool
  [obj]
  (instance? DefconstNode obj))

(def Defconst
  DefconstNode)

(defn data-label? :- Bool
  [obj]
  (and (label? obj)
       (let [tag (:tag obj)]
         (or (nil? tag)
             (tuple-tag? tag)))))

(defschema DataLabel
  (constrained Label data-label? 'data-label?))

(defschema DataValue
  (cond-pre Int Symbol))

(defrecord DefdataNode
    [label :- DataLabel]
  ;; an empty value indicates a declaration
  {(optional-key :value) [DataValue]})

(defn defdata? :- Bool
  [obj]
  (instance? DefdataNode obj))

(def Defdata
  DefdataNode)

(defrecord OperatorNode
    [name :- Symbol])

(defn operator? :- Bool
  [obj]
  (instance? OperatorNode obj))

(def Operator
  OperatorNode)

(defn base-register-name? :- Bool
  [obj]
  (and (symbol? obj)
       (contains? '#{a b c d sp bp si di} obj)))

(defn base-register? :- Bool
  [{name :name :as reg}]
  (base-register-name? name))

(defschema BaseRegister
  (constrained Register base-register? 'base-register?))

(defn index-register-name? :- Bool
  [obj]
  (and (symbol? obj)
       (contains? '#{a b c d bp si di} obj)))

(defn index-register? :- Bool
  [{name :name :as reg}]
  (index-register-name? name))

(defschema IndexRegister
  (constrained Register index-register? 'index-register?))

(defn scale-value? :- Bool
  [obj]
  (and (integer? obj)
       (contains? #{2 4 8} obj)))

(defn valid-scale? :- Bool
  [obj]
  (scale-value? (:value obj)))

(defschema Scale
  (constrained Int valid-scale? 'valid-scale?))

(defn disp-value? :- Bool
  [obj]
  (and (integer? obj)
       (<= +sint32-min-value+ obj +sint32-max-value+)))

(defn valid-displacement? :- Bool
  [obj]
  (disp-value? (:value obj)))

(defschema Displacement
  (cond-pre (constrained Int valid-displacement? 'valid-displacement?) Symbol))

(defrecord AddressNode
    []
  {(optional-key :base) BaseRegister
   (optional-key :index) IndexRegister
   (optional-key :scale) Scale
   (optional-key :disp) Displacement})

(defn address? :- Bool
  [obj]
  (instance? AddressNode obj))

(defschema Address*
  (pred #'address? 'address?))

(defn valid-address? :- Bool
  [{:keys [base index scale disp]} :- Address*]
  (boolean (and (or base index scale disp)
                (or (nil? scale) index)
                (or (nil? index) disp))))

(defschema Address
  (constrained AddressNode valid-address? 'valid-address?))

(defschema Operand
  (cond-pre Int Register Address Symbol))

(defrecord InstructionNode
    [operator :- Operator]
  {(optional-key :operands) [Operand]})

(defn instruction? :- Bool
  [obj]
  (instance? InstructionNode obj))

(def Instruction
  InstructionNode)

(defschema Statement
  (cond-pre Label Instruction))

(defn text-label? :- Bool
  [obj]
  (and (label? obj)
       (let [tag (:tag obj)]
         (or (nil? tag)
             (text-tag? tag)))))

(defschema TextLabel
  (constrained Label text-label? 'text-label?))

(defrecord DeftextNode
    [label :- TextLabel]
  ;; an empty statements indicates a declaration
  {(optional-key :statements) [Statement]})

(defn deftext? :- Bool
  [obj]
  (instance? DeftextNode obj))

(def Deftext
  DeftextNode)

(defn top-level? :- Bool
  [obj]
  (or (defimport? obj) (defconst? obj) (defdata? obj) (deftext? obj)))

(defschema TopLevel
  (pred #'top-level? 'top-level?))

(defn int-tag :- IntTag
  ([min :- IntValue
    max :- IntValue]
   (->IntTagNode min max))
  ([min :- IntValue
    max :- IntValue
    signedness :- (maybe Signedness)]
   (cond-> (int-tag min max)
     signedness (assoc :signedness signedness)))
  ([min :- IntValue
    max :- IntValue
    signedness :- (maybe Signedness)
    width :- (maybe Width)]
   (cond-> (int-tag min max signedness)
     width (assoc :width width))))

(defn sint-tag? :- Bool
  [obj]
  (and (int-tag? obj)
       (= :signed (:signedness obj))))

(defn uint-tag? :- Bool
  [obj]
  (and (int-tag? obj)
       (= :unsigned (:signedness obj))))

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

(defn tag? :- Bool
  [obj]
  (or (text-tag? obj)
      (tuple-tag? obj)
      (int-tag? obj)))

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
    tag :- (maybe Tag)]
   (cond-> (label name)
     tag (assoc :tag tag))))

(defn defimport :- Defimport
  [label :- Label]
  (->DefimportNode label))

(defn defconst :- Defconst
  [name :- Symbol
   value :- ConstValue]
  (->DefconstNode name value))

(defn defdata :- Defdata
  ([label :- Label]
   (->DefdataNode label))
  ([label :- Label
    value :- (maybe [DataValue])]
   (cond-> (defdata label)
     (seq value) (assoc :value value))))

(defn operator :- Operator
  [name :- Symbol]
  (->OperatorNode name))

(defn address :- Address
  [base :- (maybe Register)
   index :- (maybe Register)
   scale :- (maybe Scale)
   disp :- (maybe Displacement)]
  (cond-> (->AddressNode)
    base (assoc :base base)
    index (assoc :index index)
    scale (assoc :scale scale)
    disp (assoc :disp disp)))

(defn instruction :- Instruction
  ([operator :- Operator]
   (->InstructionNode operator))
  ([operator :- Operator
    operands :- (maybe [Operand])]
   (cond-> (instruction operator)
     (seq operands) (assoc :operands operands))))

(defn deftext :- Deftext
  ([label :- Label]
   (->DeftextNode label))
  ([label :- Label
    statements :- (maybe [Statement])]
   (cond-> (deftext label)
     (seq statements) (assoc :statements statements))))
