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
            [sybilant.compiler :refer [compile-and-emit-all read-file]]
            [sybilant.environment :refer [global-env]]))

(defn reset-global-env
  []
  (reset! global-env {}))

(defn reset-global-env-fixture
  [f]
  (reset-global-env)
  (f))

(defn split-lines
  [string]
  (map str/trim (str/split-lines string)))

(defn slurp-lines
  [file-name]
  (split-lines (slurp file-name)))

(defmethod assert-expr 'assembles?
  [msg [_ file-name]]
  `(let [file-name# (str "sybilant/test/" (str ~file-name))]
     (is (~'= (slurp-lines (str file-name# ".asm"))
              (compile-and-emit-all (read-file file-name# {}) {}))
         ~msg)))
