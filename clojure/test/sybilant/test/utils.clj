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

(defmethod assert-expr 'assembles?
  [msg [_ file-name]]
  `(let [file-name# (str "sybilant/test/" (str ~file-name))]
     (is (~'= (str/split (slurp (str file-name# ".asm")) #"\n")
              (compile-and-emit-all (read-file file-name# {}) {}))
         ~msg)))
