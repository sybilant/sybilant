;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compiler
  (:refer-clojure :exclude [compile])
  (:require [sybilant.analyzer :refer [analyze]]
            [sybilant.emitter :refer [emit]]
            [sybilant.parser :refer [parse-top-level]]))

(defn compile [form options]
  (-> form
      parse-top-level
      analyze))
