;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.utils
  (:require [clojure.string :as str]))

(defn atom?
  [obj]
  (instance? clojure.lang.Atom obj))

(defn maybe
  [pred]
  (fn [x] (or (nil? x) (pred x))))

(defmacro implies
  [p q]
  `(or (not ~p) ~q))

(defn die
  [exit-code format-str & args]
  (throw (ex-info (apply format format-str args) {:exit-code exit-code})))

(defn error
  [msg & args]
  (apply die 1 msg args))

(defn form
  [obj]
  (or (:form (meta obj))
      (:form obj)
      obj))

(defn loc
  ([form]
     (let [{:keys [file line column]} (meta form)]
       (loc file line column)))
  ([file line column]
     (if (and file line)
       (if column
         (format "%s:%s:%s" file line column)
         (format "%s:%s" file line))
       "")))

(defn compiling [form]
  (if-let [loc (not-empty (loc form))]
    (format " (compiling %s)" loc)
    ""))

(defn syntax-error
  [expected actual]
  (error "Expected %s, but was %s%s" (name expected) (pr-str (form actual))
         (compiling actual)))

(defn oxford
  ([] "")
  ([a] (str a))
  ([a b] (format "%s and %s" (str a) (str b)))
  ([a b & more]
     (format "%s, and %s"
             (str/join ", " (map str (list* a b (butlast more))))
             (str (last more)))))
