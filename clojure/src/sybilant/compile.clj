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
            [slingshot.slingshot :refer [try+]]
            [sybilant.utils :refer :all])
  (:import (java.io FileInputStream InputStreamReader PrintWriter PushbackReader
                    Writer))
  (:gen-class))

(def usage
  "Compiles Sybilant source into x86-64 assembly source.


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
  [^String arg]
  (and arg (.startsWith arg "-")))

(defn long-option?
  [^String arg]
  (and arg (.startsWith arg "--")))

(defn parse-long-option
  [^String arg]
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
  [^String infile options]
  (with-open [in (FileInputStream. infile)
              in (InputStreamReader. in "UTF-8")
              in (PushbackReader. in)]
    (doall (read-all (indexing-push-back-reader in 1 infile)))))

(defn read-files
  [infiles options]
  (doall (mapcat #(read-file % options) infiles)))

(defn write-to-stream
  [strs ^Writer out]
  (doseq [^String str strs]
    (.write out str)
    (.write out "\n")))

(defn write-to-file
  [strs outfile force?]
  (if (and (.exists (io/file outfile)) (not force?))
    (die 1 "%s already exists" outfile)
    (with-open [out (io/writer outfile :encoding "UTF-8")]
      (write-to-stream strs out))))

(defn print-error
  [message ^Throwable t print-stack-trace?]
  (binding [*out* *err*]
    (print "An error occurred: ")
    (if print-stack-trace?
      (.printStackTrace t ^PrintWriter *out*)
      (println message))
    (flush)))

(defn -main
  [& args]
  (require 'sybilant.compiler)
  (let [compile-and-emit-all (resolve 'sybilant.compiler/compile-and-emit-all)
        print-stack-trace? (atom false)]
    (try+
      (let [{:keys [infiles outfile force? debug?]} (-> args
                                                        prep-args
                                                        parse-args)
            options {:force? force? :debug? debug?}]
        (reset! print-stack-trace? debug?)
        (if (seq infiles)
          (let [strs (-> infiles
                         (read-files options)
                         (compile-and-emit-all options))]
            (if outfile
              (write-to-file strs outfile force?)
              (write-to-stream strs *out*))
            (exit 0))
          (die 1 "expected at least one input file")))
      (catch :exit-code {:keys [exit-code]}
        (let [{:keys [message wrapper]} &throw-context]
          (if (pos? exit-code)
            (print-error message wrapper @print-stack-trace?)
            (println message)))
        (exit exit-code))
      (catch Throwable t
        (print-error (.getMessage ^Throwable t) t @print-stack-trace?)
        (exit 1))
      (catch Object o
        (binding [*out* *err*]
          (print "Unexpectedly thrown object: ")
          (prn o)
          (flush))
        (exit 1)))))
