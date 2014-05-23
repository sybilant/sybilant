;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.utils)

(defn maybe [pred]
  (fn [x] (or (nil? x) (pred x))))

(defn implies [p q]
  (or (not p) q))

(defn error
  [msg & args]
  (throw (Exception. (apply format msg args))))

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
         (format "%s at %s:%s" file line column)
         (format "%s at %s" file line))
       "")))

(defn compiling [form]
  (format " (compiling %s)" (loc form)))

(defn syntax-error
  [expected actual]
  (error "Expected %s, but was %s%s" (name expected) (pr-str (form actual))
         (compiling actual)))
