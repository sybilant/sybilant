;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.parser
  (:refer-clojure :exclude [symbol?])
  (:require [clojure.core :as clj]
            [clojure.java.io :as io]))

(defn maybe [pred]
  (fn [x] (or (nil? x) (pred x))))

(defn implies [p q]
  (or (not p) q))

(defn merge-form
  [exp form]
  (vary-meta exp merge (meta form)))

(defn assoc-form
  [exp form]
  (vary-meta (merge-form exp form) assoc
             :form (if (instance? clojure.lang.IObj form)
                     (with-meta form {})
                     form)))

(defn tagged-list?
  [obj tag]
  (and (list? obj)
       (= tag (first obj))))

(defn make-type
  [name]
  {:type :type :name name})

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

(defn form
  [obj]
  (or (:form (meta obj))
      (:form obj)
      obj))

(defn form= [f exp]
  (= f (form exp)))

(defn error
  [msg & args]
  (throw (Exception. (apply format msg args))))

(defn loc
  ([form]
     (let [{:keys [file line column]} (meta form)]
       (loc file line column)))
  ([file line column]
     (if (and file line)
       (if column
         (format " (compiling %s at %s:%s)" file line column)
         (format " (compiling %s at %s)" file line))
       "")))

(defn syntax-error
  [expected actual]
  (error "Expected %s, but was %s%s" (name expected) (pr-str (form actual))
         (loc actual)))

(def symbol-type (make-type :symbol))

(defn symbol-form?
  [form]
  (and (clj/symbol? form) (not= \% (first (name form)))))

(defn symbol?
  [exp]
  (typed-map? exp symbol-type))

(defn make-symbol
  [form]
  {:pre [(symbol-form? form)]
   :post [(symbol? %) (form= form %)]}
  (merge-form {:type symbol-type :form form} form))

(defn parse-symbol
  [form]
  {:pre [(symbol-form? form)]
   :post [(symbol? %) (form= form %)]}
  (make-symbol form))

(def label-type (make-type :label))

(defn label-form?
  [form]
  (tagged-list? form '%label))

(defn label?
  [exp]
  (typed-map? exp label-type))

(defn make-label
  ([name]
     {:pre [(symbol? name)]
      :post [(label? %)]}
     {:type label-type :name name})
  ([name form]
     {:pre [(label-form? form)]
      :post [(label? %) (form= form %)]}
     (assoc-form (make-label name) form)))

(defn parse-label
  [form]
  {:pre [(label-form? form)]
   :post [(label? %) (form= form %)]}
  (let [[_ symbol-form] form
        form-count (dec (count form))]
    (when (not= form-count 1)
      (error "%%label expects 1 argument, but got %s%s" form-count (loc form)))
    (when-not (symbol-form? symbol-form)
      (syntax-error :symbol symbol-form))
    (make-label (parse-symbol symbol-form) form)))

(defn asm-symbol-form?
  [form]
  (and (clj/symbol? form) (= \% (first (name form)))))

(declare reg-form?)

(def operator-type (make-type :operator))

(defn operator-form?
  [form]
  (and (asm-symbol-form? form)
       (not (reg-form? form))
       (not= '%label form)
       (not= '%deftext form)
       (not= '%defdata form)))

(defn operator?
  [exp]
  (typed-map? exp operator-type))

(defn make-operator
  [form]
  {:pre [(operator-form? form)]
   :post [(operator? %) (form= form %)]}
  (merge-form {:type operator-type :form form} form))

(defn parse-operator
  [form]
  {:pre [(operator-form? form)]
   :post [(operator? %) (form= form %)]}
  (make-operator form))

(def ^:const +uint64-max-value+ (inc' (*' Long/MAX_VALUE 2)))

(def int-type (assoc (make-type :int)
                :min Long/MIN_VALUE
                :max +uint64-max-value+))

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

(defn int?
  [exp]
  (typed-map? exp int-type))

(defn parse-int
  [form]
  {:pre [(int-form? form)]
   :post [(int? %)]}
  {:type int-type :form form})

(def sint-type (make-type :sint))

(defn read-sint8
  [form]
  (unchecked-byte form))

(defmethod print-method Byte [exp writer]
  (.write writer "#sint8 ")
  (.write writer (str exp)))

(defn sint8-form?
  [form]
  (instance? Byte form))

(def sint8-type (assoc sint-type :min Byte/MIN_VALUE :max Byte/MAX_VALUE))

(defn sint8?
  [exp]
  (typed-map? exp sint8-type))

(defn parse-sint8
  [form]
  {:pre [(sint8-form? form)]
   :post [(sint8? %)]}
  {:type sint8-type :form form})

(defn read-sint16
  [form]
  (unchecked-short form))

(defmethod print-method Short [exp writer]
  (.write writer "#sint16 ")
  (.write writer (str exp)))

(defn sint16-form?
  [form]
  (instance? Short form))

(def sint16-type (assoc sint-type :min Short/MIN_VALUE :max Short/MAX_VALUE))

(defn sint16?
  [exp]
  (typed-map? exp sint16-type))

(defn parse-sint16
  [form]
  {:pre [(sint16-form? form)]
   :post [(sint16? %)]}
  {:type sint16-type :form form})

(defn read-sint32
  [form]
  (unchecked-int form))

(defmethod print-method Integer [exp writer]
  (.write writer "#sint32 ")
  (.write writer (str exp)))

(defn sint32-form?
  [form]
  (instance? Integer form))

(def sint32-type (assoc sint-type
                   :min Integer/MIN_VALUE
                   :max Integer/MAX_VALUE))

(defn sint32?
  [exp]
  (typed-map? exp sint32-type))

(defn parse-sint32
  [form]
  {:pre [(sint32-form? form)]
   :post [(sint32? %)]}
  {:type sint32-type :form form})

(defrecord Sint64 [form]
  Object
  (toString [this]
    (str form)))

(defn read-sint64
  [form]
  (Sint64. (unchecked-long form)))

(defmethod print-method Sint64 [exp writer]
  (.write writer "#sint64 ")
  (.write writer (str exp)))

(defn sint64-form?
  [form]
  (instance? Sint64 form))

(def sint64-type (assoc sint-type :min Long/MIN_VALUE :max Long/MAX_VALUE))

(defn sint64?
  [exp]
  (typed-map? exp sint64-type))

(defn parse-sint64
  [form]
  {:pre [(sint64-form? form)]
   :post [(sint64? %)]}
  {:type sint64-type :form (:form form)})

(defn sint-form?
  [form]
  (or (sint8-form? form) (sint16-form? form) (sint32-form? form)
      (sint64-form? form)))

(defn sint?
  [exp]
  (or (sint8? exp) (sint16? exp) (sint32? exp) (sint64? exp)))

(defn parse-sint
  [form]
  {:pre [(sint-form? form)]
   :post [(sint? %)]}
  (cond
   (sint8-form? form) (parse-sint8 form)
   (sint16-form? form) (parse-sint16 form)
   (sint32-form? form) (parse-sint32 form)
   (sint64-form? form) (parse-sint64 form)))

(def uint-type (assoc (make-type :uint) :min 0))

(def ^:const +uint8-max-value+ (inc' (*' Byte/MAX_VALUE 2)))

(defrecord Uint8 [form]
  Object
  (toString [this]
    (str form)))

(defn read-uint8
  [form]
  (Uint8. (mod form (inc' +uint8-max-value+))))

(defmethod print-method Uint8 [exp writer]
  (.write writer "#uint8 ")
  (.write writer (str exp)))

(defn uint8-form?
  [form]
  (instance? Uint8 form))

(def uint8-type (assoc uint-type :max +uint8-max-value+))

(defn uint8?
  [exp]
  (typed-map? exp uint8-type))

(defn parse-uint8
  [form]
  {:pre [(uint8-form? form)]
   :post [(uint8? %)]}
  {:type uint8-type :form (:form form)})

(def ^:const +uint16-max-value+ (inc' (*' Short/MAX_VALUE 2)))

(defrecord Uint16 [form]
  Object
  (toString [this]
    (str form)))

(defn read-uint16
  [form]
  (Uint16. (mod form (inc' +uint16-max-value+))))

(defmethod print-method Uint16 [exp writer]
  (.write writer "#uint16 ")
  (.write writer (str exp)))

(defn uint16-form?
  [form]
  (instance? Uint16 form))

(def uint16-type (assoc uint-type :max +uint16-max-value+))

(defn uint16?
  [exp]
  (typed-map? exp uint16-type))

(defn parse-uint16
  [form]
  {:pre [(uint16-form? form)]
   :post [(uint16? %)]}
  {:type uint16-type :form (:form form)})

(def ^:const +uint32-max-value+ (inc' (*' Integer/MAX_VALUE 2)))

(defrecord Uint32 [form]
  Object
  (toString [this]
    (str form)))

(defn read-uint32
  [form]
  (Uint32. (mod form (inc' +uint32-max-value+))))

(defmethod print-method Uint32 [exp writer]
  (.write writer "#uint32 ")
  (.write writer (str exp)))

(defn uint32-form?
  [form]
  (instance? Uint32 form))

(def uint32-type (assoc uint-type :max +uint32-max-value+))

(defn uint32?
  [exp]
  (typed-map? exp uint32-type))

(defn parse-uint32
  [form]
  {:pre [(uint32-form? form)]
   :post [(uint32? %)]}
  {:type uint32-type :form (:form form)})

(defrecord Uint64 [form]
  Object
  (toString [this]
    (str form)))

(defn read-uint64
  [form]
  (Uint64. (mod form (inc' +uint64-max-value+))))

(defmethod print-method Uint64 [exp writer]
  (.write writer "#uint64 ")
  (.write writer (str exp)))

(defn uint64-form?
  [form]
  (instance? Uint64 form))

(def uint64-type (assoc uint-type :max +uint64-max-value+))

(defn uint64?
  [exp]
  (typed-map? exp uint64-type))

(defn parse-uint64
  [form]
  {:pre [(uint64-form? form)]
   :post [(uint64? %)]}
  {:type uint64-type :form (:form form)})

(defn uint-form?
  [form]
  (or (uint8-form? form) (uint16-form? form) (uint32-form? form)
      (uint64-form? form)))

(defn uint?
  [exp]
  (or (uint8? exp) (uint16? exp) (uint32? exp) (uint64? exp)))

(defn parse-uint
  [form]
  {:pre [(uint-form? form)]
   :post [(uint? %)]}
  (cond
   (uint8-form? form) (parse-uint8 form)
   (uint16-form? form) (parse-uint16 form)
   (uint32-form? form) (parse-uint32 form)
   (uint64-form? form) (parse-uint64 form)))

(defn precise-literal-form?
  [form]
  (or (sint-form? form) (uint-form? form)))

(defn precise-literal?
  [exp]
  (or (sint? exp) (uint? exp)))

(defn parse-precise-literal
  [form]
  {:pre [(precise-literal-form? form)]
   :post [(precise-literal? %)]}
  (if (sint-form? form)
    (parse-sint form)
    (parse-uint form)))

(defn literal-form?
  [form]
  (or (int-form? form) (precise-literal-form? form)))

(defn literal?
  [exp]
  (or (int? exp) (precise-literal? exp)))

(defn parse-literal
  [form]
  {:pre [(literal-form? form)]
   :post [(literal? %)]}
  (cond
   (int-form? form) (parse-int form)
   (precise-literal-form? form) (parse-precise-literal form)))

(def reg-type (make-type :reg))

(def reg8-type (merge reg-type {:sint sint8-type :uint uint8-type
                                :int (make-int-type 0 (:max sint8-type))}))

(defn reg8?
  [exp]
  (typed-map? exp reg8-type))

(def reg16-type (merge reg-type {:sint sint16-type :uint uint16-type
                                 :int (make-int-type 0 (:max sint16-type))}))

(defn reg16?
  [exp]
  (typed-map? exp reg16-type))

(def reg32-type (merge reg-type {:sint sint32-type :uint uint32-type
                                 :int (make-int-type 0 (:max sint32-type))}))

(defn reg32?
  [exp]
  (typed-map? exp reg32-type))

(def reg64-type (merge reg-type {:sint sint64-type :uint uint64-type
                                 :int (make-int-type 0 (:max sint64-type))}))

(defn reg64?
  [exp]
  (typed-map? exp reg64-type))

(def registers (-> (io/resource "sybilant/registers.clj")
                   slurp
                   read-string
                   eval))

(defn reg-form?
  [form]
  (contains? registers form))

(defn reg?
  [exp]
  (or (reg8? exp) (reg16? exp) (reg32? exp) (reg64? exp)))

(defn parse-reg
  [form]
  {:pre [(reg-form? form)]
   :post [(reg? %)]}
  (assoc-form (get registers form) form))

(def mem-type (make-type :mem))

(defn mem-type?
  [exp]
  (type? exp :mem))

(defn base-form?
  [form]
  (reg-form? form))

(defn base?
  [exp]
  (reg? exp))

(defn parse-base
  [form]
  {:pre [(base-form? form)]
   :post [(base? %)]}
  (parse-reg form))

(defn index-form?
  [form]
  (reg-form? form))

(defn index?
  [exp]
  (reg? exp))

(defn parse-index
  [form]
  {:pre [(index-form? form)]
   :post [(index? %)]}
  (parse-reg form))

(defn scale-form?
  [form]
  (contains? #{1 2 4 8} form))

(defn scale?
  [exp]
  (scale-form? (form exp)))

(defn parse-scale
  [form]
  {:pre [(scale-form? form)]
   :post [(scale? %)]}
  (parse-literal form))

(defn disp-form?
  [form]
  (literal-form? form))

(defn disp?
  [exp]
  (literal? exp))

(defn parse-disp
  [form]
  {:pre [(disp-form? form)]
   :post [(disp? %)]}
  (parse-literal form))

(declare mem-form? mem?)

(defn verify-args
  [base index scale disp]
  (and ((maybe base?) base) ((maybe index?) index) ((maybe scale?) scale)
       ((maybe disp?) disp)
       (or base index scale disp)
       (implies scale index)
       (implies index disp)))

(defn make-mem
  [mem-type base index scale disp form]
  {:pre [(mem-type? mem-type) (verify-args base index scale disp)
         (mem-form? form)]
   :post [(mem? %) (form= form %)]}
  (assoc-form (merge {:type mem-type}
                     (when base
                       {:base base})
                     (when index
                       {:index index})
                     (when scale
                       {:scale scale})
                     (when disp
                       {:disp disp}))
              form))

(defn mem8-form?
  [form]
  (tagged-list? form '%mem8))

(def mem8-type (merge mem-type {:sint sint8-type :uint uint8-type
                                :int (make-int-type 0 (:max sint8-type))}))

(defn mem8?
  [exp]
  (typed-map? exp mem8-type))

(defn make-mem8
  [base index scale disp form]
  {:pre [(verify-args base index scale disp)
         (mem8-form? form)]
   :post [(mem8? %) (form= form %)]}
  (make-mem mem8-type base index scale disp form))

(defn parse-mem-args*
  [[form-name a b c d :as form]]
  {:pre [(mem-form? form)]
   :post [(let [[base index scale disp] %]
            (and ((maybe base-form?) base) ((maybe index-form?) index)
                 ((maybe scale-form?) scale) ((maybe disp-form?) disp)
                 (or base index scale disp)
                 (implies scale index)
                 (implies index disp)))]}
  (let [form-count (dec (count form))]
    (cond
     (and (= 1 form-count) (base-form? a))
     [a nil nil nil]
     (= 1 form-count)
     (do (when-not (disp-form? a)
           (syntax-error :disp a))
         [nil nil nil a])
     (= 2 form-count)
     (do (when-not (base-form? a)
           (syntax-error :base a))
         (when-not (disp-form? b)
           (syntax-error :disp b))
         [a nil nil b])
     (and (= 3 form-count) (index-form? a) (scale-form? b) (disp-form? c))
     [nil a b c]
     (= 3 form-count)
     (do (when-not (base-form? a)
           (syntax-error :base a))
         (when-not (index-form? b)
           (syntax-error :index b))
         (when-not (disp-form? c)
           (syntax-error :disp c))
         [a b nil c])
     (= 4 form-count)
     (do (when-not (base-form? a)
           (syntax-error :base a))
         (when-not (index-form? b)
           (syntax-error :index b))
         (when-not (scale-form? c)
           (syntax-error :scale c))
         (when-not (disp-form? d)
           (syntax-error :disp d))
         [a b c d])
     :else
     (error "%s expects between 1 to 4 arguments, but got %s%s" (name form-name)
            form-count (loc form)))))

(defn parse-mem-args
  [form]
  {:pre [(mem-form? form)]
   :post [(apply verify-args %)]}
  (let [[base index scale disp] (parse-mem-args* form)]
    [(when base
       (parse-base base))
     (when index
       (parse-index index))
     (when scale
       (parse-scale scale))
     (when disp
       (parse-disp disp))]))

(defn parse-mem8
  [form]
  {:pre [(mem8-form? form)]
   :post [(mem8? %) (form= form %)]}
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem8 base index scale disp form)))

(defn mem16-form?
  [form]
  (tagged-list? form '%mem16))

(def mem16-type (merge mem-type {:sint sint16-type :uint uint16-type
                                 :int (make-int-type 0 (:max sint16-type))}))

(defn mem16?
  [exp]
  (typed-map? exp mem16-type))

(defn make-mem16
  [base index scale disp form]
  {:pre [(verify-args base index scale disp) (mem16-form? form)]
   :post [(mem16? %) (form= form %)]}
  (make-mem mem16-type base index scale disp form))

(defn parse-mem16
  [form]
  {:pre [(mem16-form? form)]
   :post [(mem16? %) (form= form %)]}
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem16 base index scale disp form)))

(defn mem32-form?
  [form]
  (tagged-list? form '%mem32))

(def mem32-type (merge mem-type {:sint sint32-type :uint uint32-type
                                 :int (make-int-type 0 (:max sint32-type))}))

(defn mem32?
  [exp]
  (typed-map? exp mem32-type))

(defn make-mem32
  [base index scale disp form]
  {:pre [(verify-args base index scale disp) (mem32-form? form)]
   :post [(mem32? %) (form= form %)]}
  (make-mem mem32-type base index scale disp form))

(defn parse-mem32
  [form]
  {:pre [(mem32-form? form)]
   :post [(mem32? %) (form= form %)]}
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem32 base index scale disp form)))

(defn mem64-form?
  [form]
  (tagged-list? form '%mem64))

(def mem64-type (merge mem-type {:sint sint64-type :uint uint64-type
                                 :int (make-int-type 0 (:max sint64-type))}))

(defn mem64?
  [exp]
  (typed-map? exp mem64-type))

(defn make-mem64
  [base index scale disp form]
  {:pre [(verify-args base index scale disp) (mem64-form? form)]
   :post [(mem64? %) (form= form %)]}
  (make-mem mem64-type base index scale disp form))

(defn parse-mem64
  [form]
  {:pre [(mem64-form? form)]
   :post [(mem64? %) (form= form %)]}
  (let [[base index scale disp] (parse-mem-args form)]
    (make-mem64 base index scale disp form)))

(defn mem-form?
  [form]
  (or (mem8-form? form) (mem16-form? form) (mem32-form? form)
      (mem64-form? form)))

(defn mem?
  [exp]
  (or (mem8? exp) (mem16? exp) (mem32? exp) (mem64? exp)))

(defn parse-mem
  [form]
  {:pre [(mem-form? form)]
   :post [(mem? %) (form= form %)]}
  (cond
   (mem8-form? form) (parse-mem8 form)
   (mem16-form? form) (parse-mem16 form)
   (mem32-form? form) (parse-mem32 form)
   (mem64-form? form) (parse-mem64 form)))

(defn operand-form?
  [form]
  (or (literal-form? form) (symbol-form? form) (reg-form? form)
      (mem-form? form)))

(defn operand?
  [exp]
  (or (literal? exp) (symbol? exp) (reg? exp) (mem? exp)))

(defn parse-operand
  [form]
  {:pre [(operand-form? form)]
   :post [(operand? %)]}
  (cond
   (literal-form? form) (parse-literal form)
   (symbol-form? form) (parse-symbol form)
   (reg-form? form) (parse-reg form)
   (mem-form? form) (parse-mem form)))

(declare deftext-form? defdata-form?)

(def instruction-type (make-type :instruction))

(defn instruction-form?
  [form]
  (and (list? form)
       (not (label-form? form))
       (not (mem-form? form))
       (not (deftext-form? form))
       (not (defdata-form? form))))

(defn instruction?
  [exp]
  (typed-map? exp instruction-type))

(defn make-instruction
  [operator operands form]
  {:pre [(operator? operator) (every? operand? operands)
         (instruction-form? form)]
   :post [(instruction? %) (form= form %)]}
  (assoc-form {:type instruction-type :operator operator :operands operands}
              form))

(defn parse-instruction
  [form]
  {:pre [(instruction-form? form)]
   :post [(instruction? %) (form= form %)]}
  (let [[operator-form & operand-forms] form
        form-count (dec (count form))]
    (when-not (>= form-count 1)
      (error "instruction expects at least 1 argument, but got %s%s" form-count
             (loc form)))
    (when-not (operator-form? operator-form)
      (syntax-error :operator operator-form))
    (doseq [operand-form operand-forms]
      (when-not (operand-form? operand-form)
        (syntax-error :operand operand-form)))
    (make-instruction (parse-operator operator-form)
                      (map parse-operand operand-forms)
                      form)))

(defn statement-form?
  [form]
  (or (label-form? form) (instruction-form? form)))

(defn statement?
  [exp]
  (or (label? exp) (instruction? exp)))

(defn parse-statement
  [form]
  {:pre [(statement-form? form)]
   :post [(statement? %)]}
  (if (label-form? form)
    (parse-label form)
    (parse-instruction form)))

(def deftext-type (make-type :deftext))

(defn deftext-form?
  [form]
  (tagged-list? form '%deftext))

(defn deftext?
  [exp]
  (typed-map? exp deftext-type))

(defn valid-statements
  [statements]
  (or (not (seq statements))
      (loop [[statement & statements] statements
             label true]
        (when (statement? statement)
          (when-not (and label (label? statement))
            (if (seq statements)
              (recur statements (label? statement))
              (instruction? statement)))))))

(defn make-deftext
  [label statements form]
  {:pre [(label? label) (valid-statements statements) (deftext-form? form)]
   :post [(deftext? %) (form= form %)]}
  (assoc-form (merge {:type deftext-type :label label}
                     (when (seq statements)
                       {:statements statements}))
              form))

(defn parse-deftext
  [form]
  {:pre [(deftext-form? form)]
   :post [(deftext? %) (form= form %)]}
  (let [[_ label-form & statement-forms] form
        form-count (dec (count form))]
    (when-not (>= form-count 1)
      (error "%%deftext expects at least 1 argument, but got %s%s" form-count
             (loc form)))
    (when-not (label-form? label-form)
      (syntax-error :label label-form))
    (when (seq statement-forms)
      (loop [[statement-form & statement-forms] statement-forms
             label true]
        (when-not (statement-form? statement-form)
          (syntax-error :statement statement-form))
        (when (and label (label-form? statement-form))
          (error (str "%%deftext expects an instruction to follow a label,"
                      " but got %s%s")
                 (pr-str statement-form)
                 (loc statement-form)))
        (if (seq statement-forms)
          (recur statement-forms (label-form? statement-form))
          (when-not (instruction-form? statement-form)
            (error (str "%%deftext expects instruction as last statement,"
                        " but got %s%s")
                   (pr-str statement-form)
                   (loc statement-form))))))
    (make-deftext (parse-label label-form)
                  (map parse-statement statement-forms)
                  form)))

(defn defdata-form?
  [form]
  (tagged-list? form '%defdata))

(def defdata-type (make-type :defdata))

(defn defdata?
  [exp]
  (typed-map? exp defdata-type))

(defn make-defdata
  [label values form]
  {:pre [(label? label) (every? precise-literal? values) (defdata-form? form)]
   :post [(defdata? %) (form= form %)]}
  (assoc-form (merge {:type defdata-type :label label}
                     (when (seq values)
                       {:values values}))
              form))

(defn parse-defdata
  [form]
  {:pre [(defdata-form? form)]
   :post [(defdata? %) (form= form %)]}
  (let [[_ label-form & value-forms] form
        form-count (dec (count form))]
    (when-not (>= form-count 1)
      (error "%%defdata expects at least 1 argument, but got %s%s" form-count
             (loc form)))
    (when-not (label-form? label-form)
      (syntax-error :label label-form))
    (doseq [value-form value-forms]
      (when-not (precise-literal-form? value-form)
        (syntax-error "precise literal" value-form)))
    (make-defdata (parse-label label-form)
                  (map parse-precise-literal value-forms)
                  form)))

(defn top-level-form?
  [form]
  (or (deftext-form? form) (defdata-form? form)))

(defn top-level?
  [exp]
  (or (deftext? exp) (defdata? exp)))

(defn parse-top-level
  [form]
  {:pre [(top-level-form? form)]
   :post [(top-level? %)]}
  (cond
   (deftext-form? form) (parse-deftext form)
   (defdata-form? form) (parse-defdata form)))

(defn parse
  [form options]
  (when-not (top-level-form? form)
    (syntax-error "top level" form))
  (parse-top-level form))
