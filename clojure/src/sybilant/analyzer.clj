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

(defn define-global-env
  [exp]
  {:pre [(top-level? exp)]}
  (let [exp-label (:label exp)
        global-env (define-label @global-env exp-label exp)]
    (dfs-visit exp vary-meta assoc :global-env global-env)))

(defn define-local-env
  [exp]
  {:pre [(top-level? exp)]}
  (if (deftext? exp)
    (let [exp-label (:label exp)
          exp-label-name (:name exp-label)
          local-env (atom {})]
      (doseq [statement (:statements exp)
              :when (label? statement)]
        (swap! local-env define-label statement))
      (if-let [local-env (not-empty @local-env)]
        (dfs-visit exp vary-meta assoc :local-env local-env)
        exp))
    exp))

(defn mark-local-symbols
  [exp]
  {:pre [(top-level? exp)]}
  (dfs-visit exp (fn [exp]
                   (if (contains? (local-symbol-table exp) exp)
                     (vary-meta exp assoc :local? true)
                     exp))))

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
    (let [global-symbols (set (keys (global-symbol-table exp)))
          free-symbols (seq (:free-symbols (meta exp)))]
      (when-let [undefined-symbols (seq (remove global-symbols free-symbols))]
        (error "undefined symbol%s: %s%s"
               (str (when (> (count undefined-symbols) 1) "s"))
               (apply oxford (map form undefined-symbols))
               (compiling (first undefined-symbols))))))
  exp)

(defn verify-symbol-format
  [exp]
  {:pre [(symbol? exp)]}
  (let [exp-form (form exp)]
    (when-not (re-matches #"^[a-zA-Z_][a-zA-Z0-9_]*$" (str exp-form))
      (error "invalid external symbol format %s%s" exp-form (compiling exp))))
  exp)

(defn munge-symbols
  [exp]
  {:pre [(top-level? exp)]}
  (dfs-visit exp
             (fn [exp]
               (if (symbol? exp)
                 (if (:extern? (meta (get-in (global-symbol-table exp)
                                             [exp :label])))
                   (verify-symbol-format exp)
                   (vary-meta exp assoc :munge? true))
                 exp))))

(defn analyze
  [exp options]
  {:pre [(top-level? exp)]}
  (let [exp (-> exp
                define-global-env
                define-local-env
                mark-local-symbols
                free-symbols
                verify-deftext-closed
                munge-symbols)]
    (reset! global-env (:global-env (meta exp)))
    exp))
