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
  (:require [clojure.set :as set]
            [sybilant.environment :refer :all]
            [sybilant.types :refer :all]
            [sybilant.utils :refer :all]
            [sybilant.visitor :refer [dfs-visit]]))

(defn define-local-env
  [exp]
  {:pre [(top-level? exp)]}
  (if (deftext? exp)
    (let [exp-label (:label exp)
          exp-label-name (:name exp-label)
          local-env (atom {exp-label-name exp-label})]
      (doseq [statement (:statements exp)
              :when (label? statement)]
        (define-label local-env statement))
      (if-let [local-env (not-empty @local-env)]
        (vary-meta exp assoc :local-env local-env)
        exp))
    exp))

(defn free-symbols
  [exp]
  {:pre [(top-level? exp)]}
  (let [local-symbols (set (keys (:local-env (meta exp))))
        global-symbols (set (keys @global-env))
        free-symbol? (complement (set/union #{(:label exp)}
                                            local-symbols
                                            global-symbols))
        free-symbols (atom [])]
    (dfs-visit exp (fn [exp]
                     (when (and (symbol? exp) (free-symbol? exp))
                       (swap! free-symbols conj exp))
                     exp))
    (if-let [free-symbols (not-empty (distinct @free-symbols))]
      (assoc exp :free-symbols free-symbols)
      exp)))

(defn verify-deftext-closed
  [exp]
  {:pre [(top-level? exp)]}
  (when (deftext? exp)
    (when-let [free-symbols (seq (:free-symbols exp))]
      (error "undefined symbol%s: %s%s"
             (str (when (> (count free-symbols) 1) "s"))
             (apply oxford (map form free-symbols))
             (compiling (first free-symbols)))))
  exp)

(defn analyze
  [exp options]
  {:pre [(top-level? exp)]}
  (let [exp (-> exp
                define-local-env
                free-symbols
                verify-deftext-closed)]
    (define-label global-env (:label exp))
    exp))
