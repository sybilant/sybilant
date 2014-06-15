;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.utils
  (:require [clojure.string :as str]
            [slingshot.slingshot :refer [throw+]]))

(defn die [exit-code & message]
  (throw+ {:message (str/join " " (map str message))
           :exit-code exit-code}))

(defn form [obj]
  (if (string? obj)
    obj
    (pr-str (or (:form (meta obj)) (:form obj) obj))))

(defn error [& msg]
  (apply die 2 (map form msg)))

(defn maybe [pred]
  (fn [x] (or (nil? x) (pred x))))
