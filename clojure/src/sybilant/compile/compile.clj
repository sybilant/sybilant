;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile.compile
  (:refer-clojure :exclude [compile *data-readers* read])
  (:require [clojure.core :as clj]
            [clojure.java.io :as io]
            [clojure.tools.reader :refer [read *data-readers*]]
            [clojure.tools.reader.reader-types
             :refer [indexing-push-back-reader]]
            [slingshot.slingshot :refer [try+]]
            [sybilant.compiler :refer [compile]]
            [sybilant.emitter :refer [emit]]
            [sybilant.parser :refer [defasm? defdata? defimport?]]
            [sybilant.util :refer :all])
  (:import (java.io File PrintWriter PushbackReader)))

(defn option?
  [^String arg]
  (and arg (.startsWith arg "-")))

(defn long-option?
  [^String arg]
  (and arg (.startsWith arg "--")))

(defn prep-args
  ([args]
     (prep-args args []))
  ([[^String arg & args] result]
     (if arg
       (if (and (long-option? arg) (pos? (.indexOf arg "=")))
         (recur args (into result (.split arg "=")))
         (recur args (conj result arg)))
       result)))

(defn parse-outfile
  [arg [file & args] result]
  (when (option? file)
    (die 1 "expected value for" arg))
  [args (assoc result :outfile (io/file file))])

(defn parse-flag
  [key]
  (fn
    [arg args result]
    (if (seq args)
      (cond
       (option? (first args)) [args (assoc result key true)]
       (= "true" (first args)) [(rest args) (assoc result key true)]
       (= "false" (first args)) [(rest args) (assoc result key false)]
       :else (die 1 "invalid value for" arg))
      [args (assoc result key true)])))

(def option-specs
  {"-o" parse-outfile
   "--outfile" parse-outfile
   "-d" (parse-flag :debug?)
   "--debug" (parse-flag :debug?)
   "-f" (parse-flag :force?)
   "--force" (parse-flag :force?)})

(defn parse-args
  ([args]
     (parse-args args {}))
  ([[arg & args] result]
     (if arg
       (if (option? arg)
         (if-let [f (option-specs arg)]
           (let [[args result] (f arg args result)]
             (recur args result))
           (die 1 "invalid option" arg))
         (recur args (update-in result [:infiles] (fnil conj []) arg)))
       result)))

(defn parse-options
  [args]
  (parse-args (prep-args args)))

(defn read-forms
  ([]
     (read-forms (indexing-push-back-reader *in*)))
  ([in]
     (binding [*data-readers* clj/*data-readers*]
       (doall (take-while (partial not= ::eof)
                          (repeatedly #(read in false ::eof)))))))

(defn read-forms-from-file
  [f]
  (with-open [r (io/reader f :encoding "UTF-8")
              r (PushbackReader. r)]
    (read-forms (indexing-push-back-reader r 1 f))))

(defn data-exp? [exp]
  (defdata? exp))

(defn code-exp? [exp]
  (or (defasm? exp) (defimport? exp)))

(defn compile-and-emit-forms
  [forms options]
  (let [exps (for [form forms] (compile form options))
        data-exps (mapcat emit (doall (filter data-exp? exps)))
        code-exps (mapcat emit (doall (filter code-exp? exps)))
        lines (concat
               (when (seq data-exps)
                 (cons "section .data" data-exps))
               (when (seq code-exps)
                 (cons "section .text" code-exps)))]
    (when (seq lines)
      (list* "bits 64" "default rel" lines))))

(defn read-files
  [infiles]
  (if (seq infiles)
    (mapcat read-forms-from-file infiles)
    (do (print "> ")
        (flush)
        (read-forms))))

(defn compile-files
  [infiles options]
  (let [forms (read-files infiles)]
    (compile-and-emit-forms forms options)))

(defn print-lines
  [lines]
  (doseq [line lines]
    (println line)))

(defn print-lines-to-file
  [lines outfile]
  (with-open [w (io/writer outfile :encoding "utf-8")
              out (PrintWriter. w)]
    (binding [*out* out]
      (print-lines lines))))

(def debug? (atom false))

(defn print-error
  ([message]
     (binding [*out* *err*]
       (println message)))
  ([message ^Throwable throwable]
     (binding [*out* *err*]
       (println message)
       (when @debug?
         (let [w (PrintWriter. *out*)]
           (try
             (.printStackTrace throwable w)
             (finally
               (.flush w))))))))

(defn exit*
  [exit-code]
  (System/exit exit-code))

(defn main
  [args]
  (try+
    (let [{:keys [infiles ^File outfile] :as cli-options} (parse-options args)
          options (select-keys cli-options [:debug? :force?])]
      (reset! debug? (:debug? options))
      (if (and outfile (.exists outfile) (not (:force? options)))
        (die 1 outfile "already exists")
        (let [lines (compile-files infiles options)]
          (if outfile
            (print-lines-to-file lines outfile)
            (print-lines lines)))))
    (exit* 0)
    (catch :exit-code {:keys [exit-code message]}
      (when message
        (if (pos? exit-code)
          (print-error message)
          (println message)))
      (exit* exit-code))
    (catch Throwable t
      (print-error (format "Unexpected exception: %s"
                           (.getMessage ^Throwable t))
                   t)
      (exit* 1))
    (catch Object o
      (print-error (format "Unexpectedly thrown object: %s" (pr-str o))
                   (:throwable &throw-context))
      (exit* 1))))
