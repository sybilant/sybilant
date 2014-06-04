;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile
  (:gen-class))

(defn die
  [exit-code format-str & args]
  (throw (ex-info (apply format format-str args) {:exit-code exit-code})))

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
          (die 1 "unknown option: %s\n\n%s" arg usage)
          (recur args options (conj infiles arg))))
      (assoc options :infiles infiles))))

(defn exit
  [exit-code]
  (flush)
  (System/exit exit-code))

(def ^:dynamic *debug?* false)

(defn print-error
  [message ^Throwable t]
  (binding [*out* *err*]
    (print "An error occurred: ")
    (if *debug?*
      (.printStackTrace t ^java.io.PrintWriter *out*)
      (println message))
    (flush)))

(defn parse-and-compile
  [args]
  (try
    (let [compile-files (do (require 'sybilant.compiler)
                            (resolve 'sybilant.compiler/compile-files))
          {:keys [infiles outfile force? debug?]} (-> args
                                                      prep-args
                                                      parse-args)]
      (set! *debug?* debug?)
      (compile-files infiles outfile {:force? force? :debug? debug?}))
    0
    (catch clojure.lang.ExceptionInfo e
      (let [exit-code (:exit-code (ex-data e))]
        (when-not exit-code
          (throw e))
        (let [message (.getMessage e)]
          (if (pos? exit-code)
            (print-error message e)
            (println message)))
        exit-code))))

(defn -main
  [& args]
  (exit (binding [*debug?* *debug?*]
          (try
            (parse-and-compile args)
            (catch Throwable t
              (print-error (.getMessage t) t)
              1)))))
