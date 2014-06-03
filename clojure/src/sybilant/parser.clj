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
            [clojure.java.io :as io]
            [sybilant.types :refer :all]
            [sybilant.utils :refer :all]))

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

(defn form= [f exp]
  (= f (form exp)))

(defn symbol-form?
  [form]
  (and (clj/symbol? form) (not= \% (first (name form)))))

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

(defn label-form?
  [form]
  (tagged-list? form '%label))

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
      (error "%%label expects 1 argument, but got %s%s" form-count
             (compiling form)))
    (when-not (symbol-form? symbol-form)
      (syntax-error :symbol symbol-form))
    (when (:extern (meta form))
      (error "%s may not be marked external%s" symbol-form (compiling form)))
    (make-label (parse-symbol symbol-form) form)))

(defn asm-symbol-form?
  [form]
  (and (clj/symbol? form) (= \% (first (name form)))))

(declare reg-form?)

(defn operator-form?
  [form]
  (and (asm-symbol-form? form)
       (not (reg-form? form))
       (not= '%label form)
       (not= '%deftext form)
       (not= '%defdata form)))

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

(defn parse-int
  [form]
  {:pre [(int-form? form)]
   :post [(int? %)]}
  {:type int-type :form form})

(defn read-sint8
  [form]
  (unchecked-byte form))

(defmethod print-method Byte [exp ^java.io.Writer writer]
  (.write writer "#sint8 ")
  (.write writer (str exp)))

(defn sint8-form?
  [form]
  (instance? Byte form))

(defn parse-sint8
  [form]
  {:pre [(sint8-form? form)]
   :post [(sint8? %)]}
  {:type sint8-type :form form})

(defn read-sint16
  [form]
  (unchecked-short form))

(defmethod print-method Short [exp ^java.io.Writer writer]
  (.write writer "#sint16 ")
  (.write writer (str exp)))

(defn sint16-form?
  [form]
  (instance? Short form))

(defn parse-sint16
  [form]
  {:pre [(sint16-form? form)]
   :post [(sint16? %)]}
  {:type sint16-type :form form})

(defn read-sint32
  [form]
  (unchecked-int form))

(defmethod print-method Integer [exp ^java.io.Writer writer]
  (.write writer "#sint32 ")
  (.write writer (str exp)))

(defn sint32-form?
  [form]
  (instance? Integer form))

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

(defmethod print-method Sint64 [exp ^java.io.Writer writer]
  (.write writer "#sint64 ")
  (.write writer (str exp)))

(defn sint64-form?
  [form]
  (instance? Sint64 form))

(defn parse-sint64
  [form]
  {:pre [(sint64-form? form)]
   :post [(sint64? %)]}
  {:type sint64-type :form (:form form)})

(defn sint-form?
  [form]
  (or (sint8-form? form) (sint16-form? form) (sint32-form? form)
      (sint64-form? form)))

(defn parse-sint
  [form]
  {:pre [(sint-form? form)]
   :post [(sint? %)]}
  (cond
   (sint8-form? form) (parse-sint8 form)
   (sint16-form? form) (parse-sint16 form)
   (sint32-form? form) (parse-sint32 form)
   (sint64-form? form) (parse-sint64 form)))

(defrecord Uint8 [form]
  Object
  (toString [this]
    (str form)))

(defn read-uint8
  [form]
  (Uint8. (mod form (inc' +uint8-max-value+))))

(defmethod print-method Uint8 [exp ^java.io.Writer writer]
  (.write writer "#uint8 ")
  (.write writer (str exp)))

(defn uint8-form?
  [form]
  (instance? Uint8 form))

(defn parse-uint8
  [form]
  {:pre [(uint8-form? form)]
   :post [(uint8? %)]}
  {:type uint8-type :form (:form form)})

(defrecord Uint16 [form]
  Object
  (toString [this]
    (str form)))

(defn read-uint16
  [form]
  (Uint16. (mod form (inc' +uint16-max-value+))))

(defmethod print-method Uint16 [exp ^java.io.Writer writer]
  (.write writer "#uint16 ")
  (.write writer (str exp)))

(defn uint16-form?
  [form]
  (instance? Uint16 form))

(defn parse-uint16
  [form]
  {:pre [(uint16-form? form)]
   :post [(uint16? %)]}
  {:type uint16-type :form (:form form)})

(defrecord Uint32 [form]
  Object
  (toString [this]
    (str form)))

(defn read-uint32
  [form]
  (Uint32. (mod form (inc' +uint32-max-value+))))

(defmethod print-method Uint32 [exp ^java.io.Writer writer]
  (.write writer "#uint32 ")
  (.write writer (str exp)))

(defn uint32-form?
  [form]
  (instance? Uint32 form))

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

(defmethod print-method Uint64 [exp ^java.io.Writer writer]
  (.write writer "#uint64 ")
  (.write writer (str exp)))

(defn uint64-form?
  [form]
  (instance? Uint64 form))

(defn parse-uint64
  [form]
  {:pre [(uint64-form? form)]
   :post [(uint64? %)]}
  {:type uint64-type :form (:form form)})

(defn uint-form?
  [form]
  (or (uint8-form? form) (uint16-form? form) (uint32-form? form)
      (uint64-form? form)))

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

(defn parse-literal
  [form]
  {:pre [(literal-form? form)]
   :post [(literal? %)]}
  (cond
   (int-form? form) (parse-int form)
   (precise-literal-form? form) (parse-precise-literal form)))

(def registers (-> (io/resource "sybilant/registers.clj")
                   slurp
                   read-string
                   eval))

(defn reg-form?
  [form]
  (contains? registers form))

(defn parse-reg
  [form]
  {:pre [(reg-form? form)]
   :post [(reg? %)]}
  (assoc-form (get registers form) form))

(defn base-form?
  [form]
  (reg-form? form))

(defn parse-base
  [form]
  {:pre [(base-form? form)]
   :post [(base? %)]}
  (parse-reg form))

(defn index-form?
  [form]
  (reg-form? form))

(defn parse-index
  [form]
  {:pre [(index-form? form)]
   :post [(index? %)]}
  (parse-reg form))

(defn parse-scale
  [form]
  {:pre [(scale-form? form)]
   :post [(scale? %)]}
  (parse-literal form))

(defn disp-form?
  [form]
  (literal-form? form))

(defn parse-disp
  [form]
  {:pre [(disp-form? form)]
   :post [(disp? %)]}
  (parse-literal form))

(declare mem-form?)

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
            form-count (compiling form)))))

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

(defn instruction-form?
  [form]
  (and (list? form)
       (not (label-form? form))
       (not (mem-form? form))
       (not (deftext-form? form))
       (not (defdata-form? form))))

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
             (compiling form)))
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

(defn parse-statement
  [form]
  {:pre [(statement-form? form)]
   :post [(statement? %)]}
  (if (label-form? form)
    (parse-label form)
    (parse-instruction form)))

(defn deftext-form?
  [form]
  (tagged-list? form '%deftext))

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

(defn parse-extern-label
  [form]
  {:pre [(label-form? form)]
   :post [(label? %) (form= form %)]}
  (let [label (parse-label (vary-meta form dissoc :extern))]
    (if (:extern (meta form))
      (vary-meta label assoc :extern? true)
      label)))

(defn parse-deftext
  [form]
  {:pre [(deftext-form? form)]
   :post [(deftext? %) (form= form %)]}
  (let [[_ label-form & statement-forms] form
        form-count (dec (count form))]
    (when-not (>= form-count 1)
      (error "%%deftext expects at least 1 argument, but got %s%s" form-count
             (compiling form)))
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
                 (compiling statement-form)))
        (if (seq statement-forms)
          (recur statement-forms (label-form? statement-form))
          (when-not (instruction-form? statement-form)
            (error (str "%%deftext expects instruction as last statement,"
                        " but got %s%s")
                   (pr-str statement-form)
                   (compiling statement-form))))))
    (make-deftext (parse-extern-label label-form)
                  (map parse-statement statement-forms)
                  form)))

(defn defdata-form?
  [form]
  (tagged-list? form '%defdata))

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
             (compiling form)))
    (when-not (label-form? label-form)
      (syntax-error :label label-form))
    (doseq [value-form value-forms]
      (when-not (precise-literal-form? value-form)
        (syntax-error "precise literal" value-form)))
    (make-defdata (parse-extern-label label-form)
                  (map parse-precise-literal value-forms)
                  form)))

(defn top-level-form?
  [form]
  (or (deftext-form? form) (defdata-form? form)))

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
