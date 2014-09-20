;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.utils
  (:require [clojure.java.io :as io]
            [slingshot.slingshot :refer [throw+]]))

(defn die
  "Exits with exit-code and (optionally) prints exit-message."
  ([exit-code]
     (die exit-code nil))
  ([exit-code exit-message]
     (throw+ (merge {:exit-code exit-code}
                    (when exit-message
                      {:exit-message exit-message})))))

(defn file-exists?
  "True if file-path exists."
  [file-path]
  (.exists (io/file file-path)))

(defn tmp-file
  "Creates a temporary file using name for its prefix."
  ^java.io.File [name]
  (java.io.File/createTempFile name nil))

(defmacro with-tmp-files
  "Evaluates exprs in a context where bindings have been bound to temporary
  files.  After evaluating exprs the temporary paths will be deleted, if they
  exist."
  [bindings & exprs]
  (when-not (and (vector? bindings)
                 (seq bindings)
                 (every? symbol? bindings))
    (throw (Exception. "bindings must be a non-empty vector of symbols")))
  (let [[binding & bindings] bindings]
    `(let [~binding (tmp-file ~(str binding))]
       (try
         ~@(if (seq bindings)
             `((with-tmp-files ~bindings ~@exprs))
             exprs)
         (finally
           (.delete ~binding))))))

(defn tmp-path
  "Creates a temporary path using name for its prefix."
  ^String [name]
  (let [file (tmp-file name)]
    (.delete file)
    (.getAbsolutePath file)))

(defmacro with-tmp-paths
  "Evaluates exprs in a context where bindings have been bound to temporary
  paths.  The temporary paths will be unique, but non-existent filesystem paths.
  After evaluating exprs the temporary paths will be deleted, if they exist."
  [bindings & exprs]
  (when-not (and (vector? bindings)
                 (seq bindings)
                 (every? symbol? bindings))
    (throw (Exception. "bindings must be a non-empty vector of symbols")))
  (let [[binding & bindings] bindings]
    `(let [~binding (tmp-path ~(str binding))]
       (try
         ~@(if (seq bindings)
             `((with-tmp-paths ~bindings ~@exprs))
             exprs)
         (finally
           (.delete (io/file ~binding)))))))

(defmacro with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh StringWriter.
  Returns the string created."
  [& exprs]
  `(let [err# (java.io.StringWriter.)]
     (binding [*err* err#]
       ~@exprs
       (str err#))))

(defn uuid
  "Creates a new java.util.UUID object."
  ^java.util.UUID []
  (java.util.UUID/randomUUID))

(defn uuid-str
  "Creates a new java.util.UUID object and returns its string representation."
  ^String []
  (str (uuid)))

(defn println-err [& strs]
  (binding [*out* *err*]
    (apply println strs)))
