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
            [sybilant.visitor :refer [dfs-visit visit-property]]))

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
  (letfn
      [(symbol-refs
         [exp]
         (if (symbol? exp)
           #{exp}
           #{}))
       (symbol-defs
         [exp]
         (if (label? exp)
           #{(:name exp)}
           #{}))
       (free-symbols
         [exp]
         (let [{:keys [symbol-refs symbol-defs]} (meta exp)]
           (set/difference symbol-refs symbol-defs)))]
    (-> exp
        (visit-property :symbol-refs symbol-refs set/union)
        (visit-property :symbol-defs symbol-defs set/union)
        (visit-property :free-symbols free-symbols))))

(defn verify-deftext-closed
  [exp]
  {:pre [(top-level? exp)]}
  (when (deftext? exp)
    (let [global-symbols (set (keys @global-env))
          free-symbols (seq (:free-symbols (meta exp)))]
      (when-let [undefined-symbols (seq (remove global-symbols free-symbols))]
        (error "undefined symbol%s: %s%s"
               (str (when (> (count undefined-symbols) 1) "s"))
               (apply oxford (map form undefined-symbols))
               (compiling (first undefined-symbols))))))
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
