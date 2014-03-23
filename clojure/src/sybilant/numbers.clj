;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.numbers
  (:refer-clojure :exclude [< <= >= >])
  (:require [clojure.core :as clj]
            [sybilant.utils :as u]))

(defn <
  [a b & more]
  (every? #(apply clj/< %) (partition 2 1 (map u/form (list* a b more)))))

(defn <=
  [a b & more]
  (every? #(apply clj/<= %) (partition 2 1 (map u/form (list* a b more)))))

(defn >=
  [a b & more]
  (every? #(apply clj/>= %) (partition 2 1 (map u/form (list* a b more)))))

(defn >
  [a b & more]
  (every? #(apply clj/> %) (partition 2 1 (map u/form (list* a b more)))))
