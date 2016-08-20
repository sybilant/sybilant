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
   [sybilant.parser :as parser]
   [sybilant.test]))

(use-fixtures :once validate-schemas)

(deftest t-analyze-checks-for-undefined-symbols
  (are [exp]
      (ex-info?
       {:sybilant/error :undefined-symbol
        :sybilant/symbol 'bar}
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
    (analyze (parser/parse-defimport
              '(%defimport (%label foo)))
             env)
    (is (analyze (parser/parse-deftext
                  '(%deftext (%label bar)
                             (%call foo)
                             (%ret)))
                 env))))

(deftest t-analyze-checks-for-duplicate-locals
  (is (ex-info?
       {:sybilant/error :duplicate-definition
        :sybilant/symbol 'foo
        :sybilant/previous-definition (parser/parse-label '(%label foo))}
       (analyze (parser/parse-top-level
                 '(%deftext (%label foo)
                            (%add %rax 1)
                            (%label foo)
                            (%ret)))
                (atom (env/new))))))

(deftest t-analyze-checks-for-duplicate-globals
  (let [env (atom (env/new))
        exp (parser/parse-deftext
             '(%deftext (%label foo)
                        (%ret)))]
    (analyze exp env)
    (is (ex-info?
         {:sybilant/error :duplicate-definition
          :sybilant/symbol 'foo
          :sybilant/previous-definition exp}
         (analyze (parser/parse-deftext
                   '(%deftext (%label foo)
                              (%ret)))
                  env)))))

(deftest t-analyze-checks-for-invalid-symbols
  (is (ex-info?
       {:sybilant/error :invalid-symbol
        :sybilant/symbol 'foo-bar}
       (analyze (parser/parse-deftext
                 '(%deftext (%label foo-bar)
                            (%ret)))
                (atom (env/new))))
      "should not allow invalid characters")
  (is (ex-info?
       {:sybilant/error :invalid-symbol
        :sybilant/symbol 'foo/bar}
       (analyze (parser/parse-deftext
                 '(%deftext (%label foo/bar)
                            (%ret)))
                (atom (env/new))))
      "should not allow qualified symbols"))

(deftest t-analyze-checks-for-long-symbols
  (is (ex-info?
       {:sybilant/error :long-symbol
        :sybilant/symbol 'f1234567890123456789012345678901}
       (analyze (parser/parse-deftext
                 '(%deftext (%label f1234567890123456789012345678901)
                            (%ret)))
                (atom (env/new))))
      "should not allow symbols longer than 31 characters"))
