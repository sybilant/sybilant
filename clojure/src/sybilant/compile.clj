;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import (clojure.lang LineNumberingPushbackReader))
  (:gen-class))

(defn die [exit-code & message]
  (throw+ {:message (str/join " " (map str message))
           :exit-code exit-code}))

(def usage
  (str
   "Compiles Sybilant source into x86-64 assembly source.  If no source files\n"
   "are specified, then the source is read from standard input.\n"
   "\n"
   "Usage: [OPTIONS] [FILE]...\n"
   "\n"
   "Option        Default  Description\n"
   "------        -------  -----------\n"
   "-h --help              Display this usage message\n"
   "-d --debug             Print detailed error messages, and leave output\n"
   "                       file for inspection\n"
   "-o --outfile  stdout   Path to the output file\n"
   "-f --force             Overwrite output file, if it exists\n"))

(defn option? [arg]
  (and arg (.startsWith arg "-")))

(defn long-option? [arg]
  (and arg (.startsWith arg "--")))

(defn parse-long-option [arg]
  (and arg (seq (.split arg "="))))

(defn prep-args [args]
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

(defn parse-args [args]
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
          (die 1 "unknown option" arg)
          (recur args options (conj infiles arg))))
      (assoc options :infiles infiles))))

(defn exit [exit-code]
  (flush)
  (System/exit exit-code))

(defn compile-files [{:keys [infiles out] :as options}]
  (require 'sybilant.compiler)
  (let [compile (resolve 'sybilant.compiler/compile)]
    (if (seq infiles)
      (doseq [infile infiles
              :let [in (io/file infile)]]
        (when-not (.exists in)
          (die 1 "input file" infile "does not exist"))
        (with-open [in (-> in
                           (io/reader :encoding "utf-8")
                           LineNumberingPushbackReader.)]
          (compile in out)))
      (compile *in* out))))

(defn -main [& args]
  (try+
    (let [{:keys [outfile force? debug?] :as options} (-> args
                                                          prep-args
                                                          parse-args)]
      (try+
        (if-let [outfile (io/file outfile)]
          (do
            (when (and (.exists outfile) (not force?))
              (die 1 "output file" outfile "exists"))
            (try
              (with-open [out (io/writer outfile :encoding "utf-8")]
                (.write out "bits 64\n")
                (.write out "default rel\n")
                (compile-files (assoc (dissoc options :outfile) :out out)))
              (catch Throwable t
                (when-not debug?
                  (.delete outfile))
                (throw t))))
          (compile-files (assoc options :out *out*)))
        (exit 0)
        (catch Throwable t
          (binding [*out* *err*]
            (print "An error occurred: ")
            (if debug?
              (.printStackTrace t *out*)
              (println (.getMessage t)))
            (flush))
          (exit 1))))
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
