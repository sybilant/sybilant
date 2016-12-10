;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer.syntax-test
  (:require
   [clojure.test :refer [are assert-expr deftest do-report is testing
                         use-fixtures]]
   [schema.test :refer [validate-schemas]]
   [sybilant.analyzer :refer :all]
   [sybilant.analyzer.environment :as env]
   [sybilant.analyzer-test]
   [sybilant.parser :as parser]
   [sybilant.test]))

(use-fixtures :once validate-schemas)

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

(deftest t-analyze-checks-text-tag-width
  (is (analyze-error?
       {:sybilant/error :invalid-tag
        :sybilant/register (parser/parse-register '%rax)
        :sybilant/tag (parser/parse-int-tag '(%sint8 0 1))}
       '(%deftext (%label foo {%rax (%sint8 0 1)
                               %rbx (%sint64 0 1)})
                  (%add %rax %rbx)))))
