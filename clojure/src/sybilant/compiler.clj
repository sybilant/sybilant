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
            [sybilant.optimizer :refer [optimize]]
            [sybilant.parser :refer [parse]]))

(defn compile
  [form options]
  (-> form
      (parse options)
      (analyze options)
      (optimize options)))

(defn compile-all
  [forms options]
  (for [form forms]
    (compile form options)))

(defn emit-all
  [exps options]
  (apply concat (for [exp exps]
                  (emit exp options))))

(defn compile-and-emit-all
  [forms options]
  (let [exps (doall (compile-all forms options))]
    (emit-all exps options)))
