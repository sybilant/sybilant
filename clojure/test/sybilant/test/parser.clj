;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.parser
  (:require [clojure.test :refer :all]
            [sybilant.parser :refer :all]))

(defmethod assert-expr 'out-of-range? [msg [_ form]]
  `(is (~'thrown-with-msg? Exception #"out of range" ~form) ~msg))

(defmethod assert-expr 'min-out-of-range? [msg [_ form]]
  `(is (~'thrown-with-msg? Exception #"expects min to be between" ~form) ~msg))

(defmethod assert-expr 'max-out-of-range? [msg [_ form]]
  `(is (~'thrown-with-msg? Exception #"expects max to be between" ~form) ~msg))

(deftest test-parse-int
  (is (= {:type int-type :form Long/MIN_VALUE} (parse-int Long/MIN_VALUE)))
  (is (= {:type int-type :form +uint64-max-value+}
         (parse-int +uint64-max-value+)))
  (is (out-of-range? (parse-int (dec' Long/MIN_VALUE))))
  (is (out-of-range? (parse-int (inc' +uint64-max-value+)))))

(deftest test-parse-int-type
  (is (= (assoc int-type
           :min (parse-int Long/MIN_VALUE)
           :max (parse-int +uint64-max-value+))
         (parse-int-type (list 'int Long/MIN_VALUE +uint64-max-value+))))
  (is (min-out-of-range? (parse-int-type (list 'int (dec' Long/MIN_VALUE)
                                               +uint64-max-value+))))
  (is (max-out-of-range? (parse-int-type (list 'int Long/MIN_VALUE
                                               (inc' +uint64-max-value+))))))

(deftest test-parse-int8-type
  (let [min (long Byte/MIN_VALUE)
        max (long Byte/MAX_VALUE)]
    (is (= (assoc int8-type :min (parse-int min) :max (parse-int max))
           (parse-int8-type (list 'int8 min max))))
    (is (min-out-of-range? (parse-int8-type (list 'int8 (dec min) max))))
    (is (max-out-of-range? (parse-int8-type (list 'int8 min (inc max)))))))

(deftest test-parse-int16-type
  (let [min (long Short/MIN_VALUE)
        max (long Short/MAX_VALUE)]
    (is (= (assoc int16-type :min (parse-int min) :max (parse-int max))
           (parse-int16-type (list 'int16 min max))))
    (is (min-out-of-range? (parse-int16-type (list 'int16 (dec min) max))))
    (is (max-out-of-range? (parse-int16-type (list 'int16 min (inc max)))))))

(deftest test-parse-int32-type
  (let [min (long Integer/MIN_VALUE)
        max (long Integer/MAX_VALUE)]
    (is (= (assoc int32-type :min (parse-int min) :max (parse-int max))
           (parse-int32-type (list 'int32 min max))))
    (is (min-out-of-range? (parse-int32-type (list 'int32 (dec min) max))))
    (is (max-out-of-range? (parse-int32-type (list 'int32 min (inc max)))))))

(deftest test-parse-int64-type
  (let [min Long/MIN_VALUE
        max Long/MAX_VALUE]
    (is (= (assoc int64-type :min (parse-int min) :max (parse-int max))
           (parse-int64-type (list 'int64 min max))))
    (is (min-out-of-range? (parse-int64-type (list 'int64 (dec' min) max))))
    (is (max-out-of-range? (parse-int64-type (list 'int64 min (inc' max)))))))

(deftest test-parse-uint8-type
  (is (= (assoc uint8-type :min +zero+ :max (parse-int +uint8-max-value+))
         (parse-uint8-type (list 'uint8 0 +uint8-max-value+))))
  (is (min-out-of-range? (parse-uint8-type (list 'uint8 -1 +uint8-max-value+))))
  (is (max-out-of-range?
       (parse-uint8-type (list 'uint8 0 (inc +uint8-max-value+))))))

(deftest test-parse-uint16-type
  (is (= (assoc uint16-type :min +zero+ :max (parse-int +uint16-max-value+))
         (parse-uint16-type (list 'uint16 0 +uint16-max-value+))))
  (is (min-out-of-range?
       (parse-uint16-type (list 'uint16 -1 +uint16-max-value+))))
  (is (max-out-of-range?
       (parse-uint16-type (list 'uint16 0 (inc +uint16-max-value+))))))

(deftest test-parse-uint32-type
  (is (= (assoc uint32-type :min +zero+ :max (parse-int +uint32-max-value+))
         (parse-uint32-type (list 'uint32 0 +uint32-max-value+))))
  (is (min-out-of-range?
       (parse-uint32-type (list 'uint32 -1 +uint32-max-value+))))
  (is (max-out-of-range?
       (parse-uint32-type (list 'uint32 0 (inc +uint32-max-value+))))))

(deftest test-parse-uint64-type
  (is (= (assoc uint64-type :min +zero+ :max (parse-int +uint64-max-value+))
         (parse-uint64-type (list 'uint64 0 +uint64-max-value+))))
  (is (min-out-of-range?
       (parse-uint64-type (list 'uint64 -1 +uint64-max-value+))))
  (is (max-out-of-range?
       (parse-uint64-type (list 'uint64 0 (inc' +uint64-max-value+))))))
