;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.emitter
  (:refer-clojure :exclude [munge symbol?])
  (:require [clojure.string :as str]
            [sybilant.types :refer :all]
            [sybilant.utils :refer :all]))

(defn munge ^String [value]
  (let [chars (seq (if (namespace value)
                     (str (namespace value) "/" (name value))
                     (name value)))
        sb (StringBuilder.)
        chars (condp = (first chars)
                \$ (do (.append sb "_A")
                       (rest chars))
                \. (do (.append sb "_O")
                       (rest chars))
                chars)]
    (when (seq chars)
      (loop [[^Character c & chars] chars]
        (let [chars (cond
                     (or (and (not (pos? (.compareTo \a c)))
                              (not (pos? (.compareTo c \z))))
                         (and (not (pos? (.compareTo \A c)))
                              (not (pos? (.compareTo c \Z))))
                         (and (not (pos? (.compareTo \0 c)))
                              (not (pos? (.compareTo c \9))))
                         (= \$ c)
                         (= \# c)
                         (= \. c)
                         (= \? c))
                     (do (.append sb c)
                         chars)
                     :else
                     (condp = c
                       \_ (do (.append sb "__") chars)
                       \! (do (.append sb "_N") chars)
                       \% (do (.append sb "_E") chars)
                       \& (do (.append sb "_M") chars)
                       \* (do (.append sb "_S") chars)
                       \- (do (.append sb "_D") chars)
                       \= (do (.append sb "_Q") chars)
                       \+ (do (.append sb "_P") chars)
                       \| (do (.append sb "_R") chars)
                       \: (do (.append sb "_C") chars)
                       \< (do (.append sb "_L") chars)
                       \> (do (.append sb "_G") chars)
                       \/ (do (.append sb "_H") chars)
                       (if (Character/isHighSurrogate c)
                         (if (and (seq chars)
                                  (Character/isLowSurrogate (first chars)))
                           (let [p (first chars)
                                 cp (Character/toCodePoint c p)]
                             (.append sb (if (> cp 0xffff)
                                           (format "_U%08x" cp)
                                           (format "_u%04x" cp)))
                             (rest chars))
                           (error "Invalid symbol" value))
                         (do (.append sb (format "_u%04x" (int c)))
                             chars))))]
          (when (seq chars)
            (recur chars)))))
    (.toString sb)))

(defn width
  [exp]
  (get-in exp [:type :width]))

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
  (str (emit* (:name exp)) ":"))

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

(defmethod emit* :symbol
  [exp]
  (let [sym (form exp)
        name (if (:munge? (meta exp))
               (munge sym)
               (str sym))]
    (if (:local? (meta exp))
      (str "." name)
      name)))

(defmethod emit* :instruction
  [{:keys [operator operands]}]
  (apply str
         (emit* operator)
         " "
         (str/join ", " (map emit-with-prefix operands))))

(defmethod emit* :deftext
  [{:keys [label statements]}]
  (let [label-name (:name label)]
    (if (seq statements)
      (concat [(str "global " (emit* label-name))]
              [(str (emit* label-name) ":")]
              (map emit* statements))
      [(str "extern " (emit* label-name))])))

(defmethod emit* :defdata
  [{:keys [label values]}]
  (let [label-name (:name label)]
    (if (seq values)
      (concat [(str "global " (emit* label-name))
               (str (emit* label-name) ":")]
              (map emit-as-data values))
      [(str "extern " (emit* label-name))])))

(defmethod emit* :defconst
  [exp]
  [])

(defmethod emit* :default
  [exp]
  (str (form exp)))

(defn emit
  [exp options]
  (emit* exp))
