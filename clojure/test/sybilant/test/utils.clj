;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test.utils
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [sybilant.compile :refer [read-file]]
            [sybilant.compiler :refer [compile-and-emit-all]]
            [sybilant.environment :refer [global-env]]))

(defn reset-global-env
  []
  (reset! global-env {}))

(defn reset-global-env-fixture
  [f]
  (reset-global-env)
  (f))

(defmacro %deftext
  [& body]
  `(list '~'%deftext
         ~@(for [exp body]
             `'~exp)))

(defmacro %defdata
  [name & values]
  `(list '~'%defdata '~name ~@(for [value values] `'~value)))

(deftest test-deftext-macro
  (is (= '(%deftext (%label foo)
            (%mov %rax (%mem64 1))
            (%jmp bar)
            (%label bar)
            (%add %rax 1))
         (%deftext (%label foo)
           (%mov %rax (%mem64 1))
           (%jmp bar)
           (%label bar)
           (%add %rax 1)))))

(deftest test-defdata-macro
  (is (= '(%defdata (%label foo) #sint8 1 #sint8 2)
         (%defdata (%label foo) #sint8 1 #sint8 2))))

(defmethod assert-expr 'assembles?
  [msg [_ file-name]]
  `(let [file-name# (str "sybilant/test/" (str ~file-name))]
     (is (~'= (str/trim (slurp (str file-name# ".asm")))
              (str/join "\n" (compile-and-emit-all (read-file file-name# {}) {})))
         ~msg)))
