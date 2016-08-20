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

(defn analyze-top-level
  ([form]
   (analyze-top-level form nil))
  ([form env]
   (analyze (parser/parse-top-level form) (or env (atom (env/new))))))

(defmethod assert-expr 'analyze-error?
  [msg [_ expected form env]]
  (let [env (or env '(atom (env/new)))]
    `(is (~'ex-info? ~expected (analyze-top-level ~form ~env)))))

(deftest t-analyze-checks-for-undefined-symbols
  (are [exp]
      (analyze-error?
       {:sybilant/error :undefined-symbol
        :sybilant/symbol 'bar}
       exp)
    '(%deftext (%label foo)
               (%add bar 1)
               (%ret))
    '(%defdata (%label foo [(%sint8 0 1)]) [bar]))
  (testing "with %deftext"
    (is (analyze-top-level '(%deftext (%label foo)
                                      (%jmp bar)
                                      (%label bar)
                                      (%ret)))
        "should find label")
    (is (analyze-top-level '(%deftext (%label foo)
                                      (%jmp foo)
                                      (%ret)))
        "should find %deftext's label"))
  (testing "with %defdata"
    (is (analyze-top-level '(%defdata (%label foo [(%sint8 0 1)]) [foo]))
        "should find %defdata's label")))

(deftest t-analyze-defines-globals
  (let [env (atom (env/new))]
    (analyze-top-level '(%defimport (%label foo))
                       env)
    (is (analyze-top-level '(%deftext (%label bar)
                                      (%call foo)
                                      (%ret))
                           env))))

(deftest t-analyze-checks-for-duplicate-locals
  (is (analyze-error?
       {:sybilant/error :duplicate-definition
        :sybilant/symbol 'foo
        :sybilant/previous-definition (parser/parse-label '(%label foo))}
       '(%deftext (%label foo)
                  (%add %rax 1)
                  (%label foo)
                  (%ret)))))

(deftest t-analyze-checks-for-duplicate-globals
  (let [env (atom (env/new))
        exp (parser/parse-deftext
             '(%deftext (%label foo)
                        (%ret)))]
    (analyze exp env)
    (is (analyze-error?
         {:sybilant/error :duplicate-definition
          :sybilant/symbol 'foo
          :sybilant/previous-definition exp}
         '(%deftext (%label foo)
                    (%ret))
         env))))

(deftest t-analyze-checks-for-invalid-symbols
  (is (analyze-error?
       {:sybilant/error :invalid-symbol
        :sybilant/symbol 'foo-bar}
       '(%deftext (%label foo-bar)
                  (%ret)))
      "should not allow invalid characters")
  (is (analyze-error?
       {:sybilant/error :invalid-symbol
        :sybilant/symbol 'foo/bar}
       '(%deftext (%label foo/bar)
                  (%ret)))
      "should not allow qualified symbols"))

(deftest t-analyze-checks-for-long-symbols
  (is (analyze-error?
       {:sybilant/error :long-symbol
        :sybilant/symbol 'f1234567890123456789012345678901}
       '(%deftext (%label f1234567890123456789012345678901)
                  (%ret)))
      "should not allow symbols longer than 31 characters"))
