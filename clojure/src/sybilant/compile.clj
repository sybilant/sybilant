;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :refer [read]]
            [clojure.tools.reader.reader-types :refer
             [indexing-push-back-reader]]
            [slingshot.slingshot :refer [throw+ try+]])
  (:import (java.io FileInputStream InputStreamReader PushbackReader))
  (:gen-class))

(defn die
  [exit-code format-str & args]
  (throw+ {:exit-code exit-code
           :message (apply format format-str args)}))

(def usage
  "Compiles Sybilant source into x86-64 assembly source. If no source files
are specified, then the source is read from standard input.


Usage: [OPTIONS] [FILE]...

Option        Default  Description
------        -------  -----------
-h --help              Display this usage message
-d --debug             Print detailed error messages, and leave output
                       file for inspection
-o --outfile  stdout   Path to the output file
-f --force             Overwrite output file, if it exists
")

(defn option?
  [arg]
  (and arg (.startsWith arg "-")))

(defn long-option?
  [arg]
  (and arg (.startsWith arg "--")))

(defn parse-long-option
  [arg]
  (and arg (seq (.split arg "="))))

(defn prep-args
  [args]
  (loop [[arg & args] args
         result []]
    (if arg
      (if (long-option? arg)
        (let [[arg value] (parse-long-option arg)]
          (if value
            (recur args (conj result arg value))
            (recur (rest args) (conj result arg (first args)))))
        (recur args (conj result arg)))
      result)))

(defn parse-args
  [args]
  (loop [[arg & args] args
         options {}
         infiles []]
    (if arg
      (case arg
        ("-h" "--help")
        (die 0 usage)
        ("-d" "--debug")
        (recur args (assoc options :debug? true) infiles)
        ("-o" "--outfile")
        (let [outfile (first args)]
          (when (or (nil? outfile) (option? outfile))
            (die 1 "expected value for --outfile"))
          (recur (rest args) (assoc options :outfile (first args)) infiles))
        ("-f" "--force")
        (recur args (assoc options :force? true) infiles)
        (if (option? arg)
          (die 1 "unknown option: %s" arg)
          (recur args options (conj infiles arg))))
      (assoc options :infiles infiles))))

(defn exit
  [exit-code]
  (flush)
  (System/exit exit-code))

(def ^:const EOF (symbol (str (char 65535))))

(defn read-all
  [in]
  (take-while (complement (partial = EOF))
              (repeatedly #(read in false EOF))))

(defn read-file
  [infile options]
  (with-open [in (FileInputStream. infile)
              in (InputStreamReader. in "UTF-8")
              in (PushbackReader. in)]
    (doall (read-all (indexing-push-back-reader in 1 infile)))))

(defn read-files
  [infiles options]
  (doall (mapcat #(read-file % options) infiles)))

(defn write-strs
  [strs out]
  (doseq [str strs]
    (.write out str)
    (.write out "\n")))

(defn -main [& args]
  (require 'sybilant.compiler)
  (try+
    (let [{:keys [infiles outfile force? debug?]} (-> args prep-args parse-args)
          options {:force? force? :debug? debug?}]
      (if (seq infiles)
        (let [forms (read-files infiles options)
              compile-and-emit-all (resolve
                                    'sybilant.compiler/compile-and-emit-all)
              strs (compile-and-emit-all forms options)]
          (try+
            (if outfile
              (if (and (.exists (io/file outfile)) (not force?))
                (die 1 "%s already exists" outfile)
                (with-open [out (io/writer outfile :encoding "UTF-8")]
                  (write-strs strs out)))
              (write-strs strs *out*))
            (exit 0)
            (catch Throwable t
              (binding [*out* *err*]
                (print "An error occurred: ")
                (if debug?
                  (.printStackTrace t *out*)
                  (println (.getMessage t)))
                (flush))
              (exit 1))))
        (die 1 "expected at least one input file")))
    (catch :exit-code {:keys [exit-code message]}
      (if (pos? exit-code)
        (binding [*out* *err*]
          (print "An error occurred: ")
          (println message)
          (flush))
        (println message))
      (exit exit-code))
    (catch Throwable t
      (binding [*out* *err*]
        (print "An error occurred: ")
        (.printStackTrace t *out*)
        (flush))
      (exit 1))
    (catch Object o
      (binding [*out* *err*]
        (print "Unexpectedly thrown object: ")
        (prn o)
        (flush))
      (exit 1))))
