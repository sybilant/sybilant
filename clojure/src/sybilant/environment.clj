;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.environment
  (:refer-clojure :exclude [symbol?])
  (:require [sybilant.types :refer :all]
            [sybilant.utils :refer :all]))

(defonce global-env (atom {}))

(defn redef-error
  [form definition-loc compiling-loc]
  (if (seq definition-loc)
    (error "%s was defined at %s and cannot be redefined%s"
           form
           definition-loc
           compiling-loc)
    (error "%s cannot be redefined%s" form compiling-loc)))

(defn define-label
  [env label]
  {:pre [(atom? env) (label? label)]}
  (let [label-name (:name label)]
    (letfn [(define
              [env]
              (when-let [existing-label (get env label-name)]
                (redef-error (form label-name)
                             (loc existing-label)
                             (compiling label)))
              (assoc env label-name label))]
      (swap! env define)))
  nil)
