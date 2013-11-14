;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.emitter)

(defmulti emit (comp :type first list))
(defmethod emit :symbol [exp out]
  (.write out (str (:form exp))))
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
(defmethod emit :defextern [exp out]
  (.write out "\nextern ")
  (emit (:name exp) out)
  (.write out "\n"))
