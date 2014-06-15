;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.emitter
  (:refer-clojure :exclude [munge])
  (:require [clojure.string :as str]
            [sybilant.parser :refer [int? mem? uint?]]
            [sybilant.utils :refer [error]]))

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

(defmulti emit (comp :type first list))
(defmethod emit :symbol [exp]
  (let [symbol-info (get-in (meta exp) [:symbol-table exp])]
    (str (when (= :label (:type symbol-info))
           ".")
         (cond
          (not (:extern? symbol-info))
          (munge (:form exp))
          :else
          (str (:form exp))))))
(defmethod emit :string [exp]
  (let [chars (seq (.getBytes ^String (:form exp) "utf-8"))]
    (str (when (seq chars)
           (str/join ", "
                     (for [char chars]
                       (str char)))))))
(defmethod emit :number [exp]
  (str (:form exp)))
(defmethod emit :int [exp]
  (str (:form exp)))
(defmethod emit :uint [exp]
  (str (:form exp)))
(defmethod emit :register [exp]
  (subs (str (:form (meta exp))) 1))
(defmethod emit :mem [{:keys [base index scale disp] :as exp}]
  (str "["
       (when base
         (emit base))
       (when index
         (str (when base
                "+")
              (when scale
                "(")
              (emit index)
              (when scale
                (str "*" (emit scale) ")"))))
       (when disp
         (str (when (or base index)
                "+")
              (emit disp)))
       "]"))
(defmethod emit :operator [exp]
  (subs (str (:form exp)) 1))
(defn emit-prefix? [exp]
  (or (int? exp) (mem? exp) (uint? exp)))
(defn emit-width-prefix [exp]
  (case (long (:width exp))
    8 "byte "
    16 "word "
    32 "dword "
    64 "qword "))
(defmethod emit :instruction [exp]
  (str (emit (:operator exp))
       " "
       (when-let [operands (seq (:operands exp))]
         " "
         (when (emit-prefix? (first operands))
           (emit-width-prefix (first operands)))
         (str/join ", "
                   (for [operand operands]
                     (str (when (emit-prefix? operand)
                            (emit-width-prefix operand))
                          (emit operand)))))))
(defmethod emit :label [exp]
  (str (emit (:name exp)) ":"))
(defmethod emit :defasm [exp]
  (list* (str "global " (emit (:name exp)))
         (str (emit (:name exp)) ":")
         (for [statement (:statements exp)]
           (emit statement))))
(defmethod emit :defimport [exp]
  [(str "extern " (emit (:name exp)))])
(defn emit-data-instruction [exp]
  (case (long (:width exp))
    8 "db "
    16 "dw "
    32 "dd "
    64 "dq "))
(defmethod emit :defdata [exp]
  (list* (str "global " (emit (:name exp)))
         (str (emit (:name exp)) ":")
         (for [value (:values exp)]
           (str (emit-data-instruction value)
                (emit value)))))
(defmethod emit :defconst [exp]
  (comment intentionally left blank))
