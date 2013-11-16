;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.emitter
  (:refer-clojure :exclude [munge])
  (:require [sybilant.util :refer [error]]))

(defn munge [value]
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
      (loop [[c & chars] chars]
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
(defmethod emit :symbol [exp out]
  (let [symbol-info (get-in (meta exp) [:symbol-table exp])]
    (when (= :label (:type symbol-info))
      (.write out "."))
    (cond
     (not (:extern? symbol-info))
     (.write out (munge (:form exp)))
     :else
     (.write out (str (:form exp))))))
(defmethod emit :number [exp out]
  (.write out (str (:form exp))))
(defn emit-width-prefix [exp out]
  (.write out (case (:width exp)
                8 "byte "
                16 "word "
                32 "dword "
                64 "qword ")))
(defmethod emit :int [exp out]
  (emit-width-prefix exp out)
  (.write out (str (:form exp))))
(defmethod emit :uint [exp out]
  (emit-width-prefix exp out)
  (.write out (str (:form exp))))
(defmethod emit :register [exp out]
  (.write out (subs (str (:form (meta exp))) 1)))
(defmethod emit :mem [{:keys [base index scale disp] :as exp} out]
  (emit-width-prefix exp out)
  (.write out "[")
  (when base
    (emit base out))
  (when index
    (when base
      (.write out "+"))
    (when scale
      (.write out "("))
    (emit index out)
    (when scale
      (.write out "*")
      (emit scale out)
      (.write out ")")))
  (when disp
    (when (or base index)
      (.write out "+"))
    (emit disp out))
  (.write out "]"))
(defmethod emit :operator [exp out]
  (.write out (subs (str (:form exp)) 1)))
(defmethod emit :instruction [exp out]
  (emit (:operator exp) out)
  (when-let [operands (seq (:operands exp))]
    (.write out " ")
    (emit (first operands) out)
    (doseq [operand (rest operands)]
      (.write out ", ")
      (emit operand out)))
  (.write out "\n"))
(defmethod emit :label [exp out]
  (emit (:name exp) out)
  (.write out ":\n"))
(defmethod emit :defasm [exp out]
  (.write out "\nglobal ")
  (emit (:name exp) out)
  (.write out "\n")
  (emit (:name exp) out)
  (.write out ":\n")
  (doseq [statement (:statements exp)]
    (emit statement out)))
(defmethod emit :defimport [exp out]
  (.write out "\nextern ")
  (emit (:name exp) out)
  (.write out "\n"))
(defmethod emit :defdata [exp out]
  (.write out "\nglobal ")
  (emit (:name exp) out)
  (.write out "\n")
  (emit (:name exp) out)
  (.write out ":\n")
  (emit (first (:values exp)) out)
  (doseq [value (rest (:values exp))]
    (.write out " ")
    (emit value out))
  (.write out "\n"))
