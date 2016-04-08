;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer-test
  (:require
   [clojure.test :refer [are assert-expr deftest do-report is testing
                         use-fixtures]]
   [schema.test :refer [validate-schemas]]
   [sybilant.analyzer :refer :all]
   [sybilant.analyzer.environment :as env]
   [sybilant.parser :as parser])
  (:import
   clojure.lang.ExceptionInfo))

(defmethod assert-expr 'ex-info?
  [msg [_ expected form]]
  `(let [msg# ~msg]
     (try
       ~form
       (do-report {:type :fail :message msg#
                   :expected '~expected :actual nil})
       (catch Exception e#
         (if-let [data# (ex-data e#)]
           (let [expected# ~expected]
             (if-not (= expected# (select-keys data# (keys expected#)))
               (do-report {:type :fail :message msg#
                           :expected expected# :actual data#})
               (do-report {:type :pass :message msg#
                           :expected '~expected :actual data#})))
           (do-report {:type :fail :message msg#
                       :expected '~expected :actual e#}))))))

(use-fixtures :once validate-schemas)

(deftest t-analyze-checks-symbols
  (are [exp]
      (ex-info?
       {:error :undefined-symbol
        :symbol 'bar}
       (analyze (parser/parse-top-level exp)
                (atom (env/new))))
    '(%deftext (%label foo)
               (%add bar 1)
               (%ret))
    '(%defdata (%label foo [(%sint8 0 1)]) [bar]))
  (testing "with %deftext"
    (is (analyze (parser/parse-deftext
                  '(%deftext (%label foo)
                             (%jmp bar)
                             (%label bar)
                             (%ret)))
                 (atom (env/new)))
        "should find label")
    (is (analyze (parser/parse-deftext
                  '(%deftext (%label foo)
                             (%jmp foo)
                             (%ret)))
                 (atom (env/new)))
        "should find %deftext's label"))
  (testing "with %defdata"
    (is (analyze (parser/parse-defdata
                  '(%defdata (%label foo [(%sint8 0 1)]) [foo]))
                 (atom (env/new)))
        "should find %defdata's label")))

(deftest t-analyze-defines-globals
  (let [env (atom (env/new))]
    (is (analyze (parser/parse-deftext
                  '(%deftext (%label foo)
                             (%ret)))
                 env))
    (is (analyze (parser/parse-deftext
                  '(%deftext (%label bar)
                             (%call foo)
                             (%ret)))
                 env))))
