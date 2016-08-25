;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.test
  (:require
   [clojure.test :refer [assert-expr do-report is]]))

(defmethod assert-expr 'ex-info?
  [msg [_ expected form]]
  `(let [msg# ~msg]
     (try
       ~form
       (do-report {:type :fail :message msg#
                   :expected '~expected :actual nil})
       (catch Exception e#
         (if-let [data# (ex-data e#)]
           (let [expected# ~expected]
             (if-not (= expected# (select-keys data# (keys expected#)))
               (do-report {:type :fail :message msg#
                           :expected expected# :actual data#})
               (do-report {:type :pass :message msg#
                           :expected '~expected :actual data#})))
           (throw e#))))))
