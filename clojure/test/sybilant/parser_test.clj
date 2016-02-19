;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.parser-test
  (:require
   [clojure.test :refer :all]
   [schema.test :refer [validate-schemas]]
   [sybilant.ast :as ast]
   [sybilant.parser :refer :all]))

(use-fixtures :once validate-schemas)

(defmethod assert-expr 'syntax-error?
  [msg [_ form]]
  `(let [msg# ~msg]
     (try
       ~form
       (do-report {:type :fail :message msg#
                   :expected '~form :actual nil})
       (catch Exception e#
         (if (= :syntax-error (:error (ex-data e#)))
           (do-report {:type :pass :message msg#
                       :expected '~form :actual e#})
           (throw e#))))))

(defmethod assert-expr 'meta?
  [msg [_ expected exp]]
  `(let [{file# :file line# :line column# :column} ~expected
         m# (meta ~exp)]
     (is (~'= file# (:file m#)) "file does not match")
     (is (~'= line# (:line m#)) "line does not match")
     (is (~'= column# (:column m#)) "column does not match")))

(deftest t-parse-int-value
  (is (= 12 (parse-int-value 12)))
  (is (syntax-error? (parse-int-value (dec' ast/+sint64-min-value+))))
  (is (syntax-error? (parse-int-value (inc' ast/+uint64-max-value+)))))

(deftest t-parse-int-tag
  (let [m {:file "foo.syb" :line 1 :column 5}
        form (with-meta '(%int 1 2) m)
        exp (parse-int-tag form)]
    (is (ast/int-tag? exp))
    (is (meta? m exp))))

(deftest t-parse-int-tag-with-wrong-tagged-list
  (is (syntax-error? (parse-int-tag '(%foo 1 2)))))

(deftest t-parse-int-tag-with-one-arg
  (is (syntax-error? (parse-int-tag '(%int 1)))))

(deftest t-parse-int-tag-with-three-args
  (is (syntax-error? (parse-int-tag '(%int 1 2 3)))))

(deftest t-parse-sint-tag
  (let [m {:file "foo.syb" :line 1 :column 5}
        form (with-meta '(%sint 1 2) m)
        exp (parse-int-tag form)]
    (is (ast/sint-tag? exp))
    (is (meta? m exp))))

(deftest t-parse-uint-tag
  (let [m {:file "foo.syb" :line 1 :column 5}
        form (with-meta '(%uint 1 2) m)
        exp (parse-int-tag form)]
    (is (ast/uint-tag? exp))
    (is (meta? m exp))))

(deftest t-parse-tuple-tag
  (let [m {:file "foo.syb" :line 1 :column 5}
        form (with-meta '[(%int 1 2)] m)
        exp (parse-tuple-tag form)]
    (is (ast/tuple-tag? exp))
    (is (meta? m exp))))

(deftest t-parse-tuple-tag-with-no-tags
  (is (syntax-error? (parse-tuple-tag []))))

(deftest t-parse-text-tag
  (let [m {:file "foo.syb" :line 1 :column 5}
        form (with-meta '{%rax (%int 1 2)} m)
        exp (parse-text-tag form)]
    (is (ast/text-tag? exp))
    (is (meta? m exp))))

(deftest t-parse-text-tag-with-no-tags
  (is (syntax-error? (parse-text-tag {}))))

(deftest t-parse-int
  (let [int (parse-int 3)]
    (is (ast/int? int))))

(deftest t-parse-label
  (let [m {:file "foo.syb" :line 2 :column 4}]
    (let [form (with-meta '(%label foo) m)
          exp (parse-label form)]
      (is (ast/label? exp))
      (is (meta? m exp)))
    (testing "with tag"
      (let [form (with-meta '(%label foo (%int 1 2)) m)
            exp (parse-label form)]
        (is (ast/label? exp))
        (is (ast/tag? (:tag exp)))
        (is (meta? m exp))))))

(deftest t-parse-defimport
  (let [m {:file "foo.syb" :line 1 :column 5}
        form (with-meta '(%defimport (%label foo)) m)
        exp (parse-defimport form)]
    (is (ast/defimport? exp))
    (is (meta? m exp))))

(deftest t-parse-defconst
  (let [m {:file "foo.syb" :line 1 :column 1}
        form (with-meta '(%defconst PI 3) m)
        exp (parse-defconst form)]
    (is (ast/defconst? exp))
    (is (meta? m exp))))

(deftest t-parse-defdata
  (let [m {:file "foo.syb" :line 1 :column 1}]
    (let [form (with-meta '(%defdata (%label foo [(%int 1 2)]) [1]) m)
          exp (parse-defdata form)]
      (is (ast/defdata? exp))
      (is (meta? m exp)))
    (testing "without value"
      (let [m {:file "foo.syb" :line 1 :column 1}
            form (with-meta '(%defdata (%label foo [(%int 1 2)])) m)
            exp (parse-defdata form)]
        (is (ast/defdata? exp))
        (is (meta? m exp))))))

(deftest t-parse-deftext
  (let [m {:file "foo.syb" :line 1 :column 1}]
    (let [form (with-meta '(%deftext (%label foo {%rax (%int 1 2)})
                                     (%add (%addr %rbx 5) 1)
                                     (%label quux)
                                     (%jmp quux))
                 m)
          exp (parse-deftext form)]
      (is (ast/deftext? exp))
      (is (meta? m exp)))
    (testing "without body"
      (let [m {:file "foo.syb" :line 1 :column 1}
            form (with-meta '(%deftext (%label foo {%rax (%int 1 2)})
                                       (%add (%addr %rbx 5) 1)
                                       (%label quux)
                                       (%jmp quux))
                   m)
            exp (parse-deftext form)]
        (is (ast/deftext? exp))
        (is (meta? m exp))))))
