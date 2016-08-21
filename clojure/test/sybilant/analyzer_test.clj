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
        :sybilant/symbol 'undefined}
       exp)
    '(%deftext (%label foo)
               (%add undefined 1)
               (%ret))
    '(%defdata (%label foo [(%sint8 0 1)]) [undefined])
    ;; %defconst cannot refer to itself
    '(%defconst (%label undefined) undefined))
  (testing "with %deftext"
    (is (analyze-top-level '(%deftext (%label foo)
                                      (%jmp foo)
                                      (%ret)))
        "should find %deftext's label")
    (is (analyze-top-level '(%deftext (%label foo)
                                      (%jmp bar)
                                      (%label bar)
                                      (%ret)))
        "should find label statement"))
  (testing "with %defdata"
    ;; this would really only work with a recursive type
    (is (analyze-top-level '(%defdata (%label foo [(%sint8 0 1)]) [foo]))
        "should find %defdata's label")))

(deftest t-analyze-defines-globals
  (testing "%defimport"
    (let [env (atom (env/new))]
      (analyze-top-level '(%defimport (%label foo))
                         env)
      (is (env/get-global @env 'foo) "should define global")))
  (testing "%defconst"
    (let [env (atom (env/new))]
      (analyze-top-level '(%defconst (%label foo) 1)
                         env)
      (is (env/get-global @env 'foo) "should define global")))
  (testing "%defdata"
    (let [env (atom (env/new))]
      (analyze-top-level '(%defdata (%label foo [(%sint8 0 1)]) [1])
                         env)
      (is (env/get-global @env 'foo) "should define global")))
  (testing "%deftext"
    (let [env (atom (env/new))]
      (analyze-top-level '(%deftext (%label foo)
                                    (%ret))
                         env)
      (is (env/get-global @env 'foo) "should define global"))))

(deftest t-analyze-checks-for-duplicate-locals
  (is (analyze-error?
       {:sybilant/error :duplicate-definition
        :sybilant/symbol 'foo
        :sybilant/previous-definition (parser/parse-label '(%label foo))}
       '(%deftext (%label foo)
                  (%add %rax 1)
                  (%label foo)
                  (%ret)))
      "should check %deftext label")
  (is (analyze-error?
       {:sybilant/error :duplicate-definition
        :sybilant/symbol 'bar
        :sybilant/previous-definition (parser/parse-label '(%label bar))}
       '(%deftext (%label foo)
                  (%add %rax 1)
                  (%label bar)
                  (%add %rax 2)
                  (%label bar)
                  (%ret)))
      "should check %deftext statements"))

(deftest t-analyze-checks-for-duplicate-globals
  (let [env (atom (env/new))
        exp (parser/parse-defimport
             '(%defimport (%label foo)))
        error {:sybilant/error :duplicate-definition
               :sybilant/symbol 'foo
               :sybilant/previous-definition exp}]
    (analyze exp env)
    (testing "%defimport"
      (is (analyze-error?
           error
           '(%defimport (%label foo))
           env)))
    (testing "%defconst"
      (is (analyze-error?
           error
           '(%defconst (%label foo) 1)
           env)))
    (testing "%defdata"
      (is (analyze-error?
           error
           '(%defdata (%label foo [(%sint8 0 1)]) [1])
           env)))
    (testing "%deftext"
      (is (analyze-error?
           error
           '(%deftext (%label foo)
                      (%ret))
           env)))))

(deftest t-analyze-checks-for-invalid-characters
  (let [error {:sybilant/error :invalid-symbol
               :sybilant/symbol 'foo-bar}]
    (testing "%defimport"
      (is (analyze-error?
           error
           '(%defimport (%label foo-bar)))
          "should not allow invalid characters in label"))
    (testing "%defconst"
      (is (analyze-error?
           error
           '(%defconst (%label foo-bar) 1))
          "should not allow invalid characters in label")
      (is (analyze-error?
           error
           '(%defconst (%label foo) foo-bar))
          "should not allow invalid characters in value"))
    (testing "%defdata"
      (is (analyze-error?
           error
           '(%defdata (%label foo-bar [(%sint8 0 1)]) [1]))
          "should not allow invalid characters in label")
      (is (analyze-error?
           error
           '(%defdata (%label foo [(%sint8 0 1)]) [foo-bar]))
          "should not allow invalid characters in value"))
    (testing "%deftext"
      (is (analyze-error?
           error
           '(%deftext (%label foo-bar)
                      (%ret)))
          "should not allow invalid characters in label")
      (is (analyze-error?
           error
           '(%deftext (%label foo)
                      (%label foo-bar)
                      (%ret)))
          "should not allow invalid characters in statements")
      (is (analyze-error?
           error
           '(%deftext (%label foo)
                      (%mov %rax foo-bar)
                      (%ret)))
          "should not allow invalid characters in statements"))))

(deftest t-analyze-checks-for-qualified-symbols
  (let [error {:sybilant/error :invalid-symbol
               :sybilant/symbol 'foo/bar}]
    (testing "%defimport"
      (is (analyze-error?
           error
           '(%defimport (%label foo/bar)))
          "should not allow qualified symbols in label"))
    (testing "%defconst"
      (is (analyze-error?
           error
           '(%defconst (%label foo/bar) 1))
          "should not allow qualified symbols in label")
      (is (analyze-error?
           error
           '(%defconst (%label foo) foo/bar))
          "should not allow qualified symbols in value"))
    (testing "%defdata"
      (is (analyze-error?
           error
           '(%defdata (%label foo/bar [(%sint8 0 1)]) [1]))
          "should not allow qualified symbols in label")
      (is (analyze-error?
           error
           '(%defdata (%label foo [(%sint8 0 1)]) [foo/bar]))
          "should not allow qualified symbols in value"))
    (testing "%deftext"
      (is (analyze-error?
           error
           '(%deftext (%label foo/bar)
                      (%ret)))
          "should not allow qualified symbols in label")
      (is (analyze-error?
           error
           '(%deftext (%label foo)
                      (%label foo/bar)
                      (%ret)))
          "should not allow qualified symbols in statements")
      (is (analyze-error?
           error
           '(%deftext (%label foo)
                      (%mov %rax foo/bar)
                      (%ret)))
          "should not allow qualified symbols in statements"))))

(deftest t-analyze-checks-for-long-symbols
  (let [error {:sybilant/error :long-symbol
               :sybilant/symbol 'f1234567890123456789012345678901}]
    (testing "%defimport"
      (is (analyze-error?
           error
           '(%defimport (%label f1234567890123456789012345678901)))
          "should not allow long symbols in label"))
    (testing "%defconst"
      (is (analyze-error?
           error
           '(%defconst (%label f1234567890123456789012345678901) 1))
          "should not allow long symbols in label")
      (is (analyze-error?
           error
           '(%defconst (%label foo) f1234567890123456789012345678901))
          "should not allow long symbols in value"))
    (testing "%defdata"
      (is (analyze-error?
           error
           '(%defdata (%label f1234567890123456789012345678901 [(%sint8 0 1)])
                      [1]))
          "should not allow long symbols in label")
      (is (analyze-error?
           error
           '(%defdata (%label foo [(%sint8 0 1)])
                      [f1234567890123456789012345678901]))
          "should not allow long symbols in value"))
    (testing "%deftext"
      (is (analyze-error?
           error
           '(%deftext (%label f1234567890123456789012345678901)
                      (%ret)))
          "should not allow long symbols in label")
      (is (analyze-error?
           error
           '(%deftext (%label foo)
                      (%label f1234567890123456789012345678901)
                      (%ret)))
          "should not allow long symbols in statements")
      (is (analyze-error?
           error
           '(%deftext (%label foo)
                      (%mov %rax f1234567890123456789012345678901)
                      (%ret)))
          "should not allow long symbols in statements"))))
