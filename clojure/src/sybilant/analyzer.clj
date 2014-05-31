;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.analyzer
  (:refer-clojure :exclude [symbol?])
  (:require [sybilant.environment :refer :all]
            [sybilant.types :refer :all]))

(defn define-local-labels
  [exp]
  {:pre [(top-level? exp)]}
  (if (deftext? exp)
    (let [exp-label (:label exp)
          exp-label-name (:name exp-label)
          local-env (atom {exp-label-name exp-label})]
      (doseq [statement (:statements exp)
              :when (label? statement)]
        (define-label local-env statement))
      (if-let [local-labels (not-empty (dissoc @local-env exp-label-name))]
        (assoc exp :local-labels local-labels)
        exp))
    exp))

(defn analyze
  [exp options]
  {:pre [(top-level? exp)]}
  (let [exp (define-local-labels exp)]
    (define-label global-env (:label exp))
    exp))
