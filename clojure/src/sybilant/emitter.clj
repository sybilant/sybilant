;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.emitter
  (:refer-clojure :exclude [symbol?])
  (:require [sybilant.types :refer :all]
            [sybilant.utils :refer :all]))

(defmulti width (fn [exp] (get-in exp [:type :name])))

(defmethod width :reg
  [exp]
  (get-in exp [:type :width]))

(defmethod width :mem
  [exp]
  (get-in exp [:type :width]))

(defmethod width :sint
  [{{min :min max :max} :type}]
  (cond
   (<= (:min sint8-type) min max (:max sint8-type)) 8
   (<= (:min sint16-type) min max (:max sint16-type)) 16
   (<= (:min sint32-type) min max (:max sint32-type)) 32
   (<= (:min sint64-type) min max (:max sint64-type)) 64))

(defmethod width :uint
  [{{min :min max :max} :type}]
  (cond
   (<= (:min uint8-type) min max (:max uint8-type)) 8
   (<= (:min uint16-type) min max (:max uint16-type)) 16
   (<= (:min uint32-type) min max (:max uint32-type)) 32
   (<= (:min uint64-type) min max (:max uint64-type)) 64))

(defmethod width :default
  [exp]
  nil)

(defmulti emit* (fn [exp] (get-in exp [:type :name])))

(def width->prefix
  {8 "byte "
   16 "word "
   32 "dword "
   64 "qword "
   nil nil})

(defn emit-with-prefix [exp]
  (str (when (or (precise-literal? exp) (mem? exp))
         (width->prefix (width exp)))
       (emit* exp)))

(def width->data-instruction
  {8 "db "
   16 "dw "
   32 "dd "
   64 "dq "
   nil nil})

(defn emit-as-data [exp]
  (str (when (precise-literal? exp)
         (width->data-instruction (width exp)))
       (emit* exp)))

(defmethod emit* :label
  [exp]
  (str "." (emit* (:name exp)) ":"))

(defmethod emit* :operator
  [exp]
  (subs (str (form exp)) 1))

(defmethod emit* :reg
  [exp]
  (subs (str (form exp)) 1))

(defmethod emit* :mem
  [{:keys [base index scale disp]}]
  (str "["
       (when base (emit* base))
       (when index
         (str (when base "+")
              (emit* index)
              (when scale (str "*" (emit* scale)))))
       (when disp
         (str (when (or base index) "+")
              (emit* disp))) "]"))

(defmethod emit* :instruction
  [{:keys [operator operands]}]
  (apply str
         (emit* operator)
         " "
         (interpose ", " (map emit-with-prefix operands))))

(defmethod emit* :deftext
  [{:keys [label statements]}]
  (let [label-name (:name label)]
    (if (seq statements)
      (apply str
             "global " (form label-name) "\n"
             (emit* label-name)
             ":\n"
             (interpose "\n" (map emit* statements)))
      (str "extern " (form label-name)))))

(defmethod emit* :defdata
  [{:keys [label values]}]
  (let [label-name (:name label)]
    (if (seq values)
      (apply str
             "global " (form label-name) "\n"
             (emit* label-name)
             ":\n"
             (interpose "\n" (map emit-as-data values)))
      (str "extern " (form label-name)))))

(defmethod emit* :default
  [exp]
  (str (form exp)))

(defn emit
  [exp options]
  (emit* exp))
