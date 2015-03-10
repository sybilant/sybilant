;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.compile
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.string :as str]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types
             :refer [indexing-push-back-reader]]
            [slingshot.slingshot :refer [try+]]
            [sybilant.utils :refer [die file-exists? println-err]])
  (:import (java.io PushbackReader))
  (:gen-class))

(defn option?
  "True if arg is an option."
  [^String arg]
  (and arg (.startsWith arg "-")))

(defn long-option?
  "True if arg is a long option."
  [^String arg]
  (and arg (.startsWith arg "--")))

(defn parse-long-option
  "Parses a long option into its option and value (if specified) components."
  [arg]
  (let [[option value :as pair] (str/split arg #"=" 2)]
    (if (seq value)
      pair
      [option])))

(defn expect-value
  "Returns arg if arg is not an option.  If arg is an option, then an
  exception is thrown."
  [arg]
  (when (option? arg)
    (throw (Exception. (str "Expected value, but was" arg))))
  arg)

(defn expand-args
  "Expands args that are not simple values.  Long options with a specified
  value (e.g. '--foo=bar') are parsed into an option and value (e.g. '--foo'
  'bar').  Short options that are combined (e.g. '-fo') are parsed into separate
  options (e.g. '-f' '-o')."
  [args result]
  (if-let [[arg & args] (seq args)]
    (cond
      (long-option? arg) (let [pair (parse-long-option arg)]
                           (recur args (into result pair)))
      (option? arg) (if-let [arg (next arg)]
                      (recur args (into result
                                        (for [option arg]
                                          (str "-" option))))
                      (recur args (conj result arg)))
      :else (recur args (conj result arg)))
    result))

(def usage
  "Usage: [OPTION]... [FILE]...
  Compiles Sybilant source code into x86-64 assembly source code.

  -h, --help                  display this usage message
  -o, --output=FILE           path to the output file; defaults to stdout
  -f, --force                 overwrite output file, if it exists")

(defn parse-args
  "Parses args and associates them into options."
  [args options]
  (if-let [[arg & args] (seq args)]
    (if (option? arg)
      (case arg
        ("-h" "--help") (die 0 usage)
        ("-o" "--output") (recur (next args)
                                 (assoc options
                                        :outfile (expect-value (first args))))
        ("-f" "--force") (recur args (assoc options :force? true))
        (throw (Exception. (str "Invalid option: " arg))))
      (recur args (update-in options [:infiles] (fnil conj []) arg)))
    options))

(def default-options {})

(defn read-all
  "Reads (and fully realizes) all the forms from reader.  Expects reader to be a
  java.io.PushbackReader and file-name to be a String.  If file-name is not
  specified, it defaults to \"NO_SOURCE_FILE\"."
  ([reader]
   (read-all reader "NO_SOURCE_FILE"))
  ([reader file-name]
   (let [reader (indexing-push-back-reader reader 1 file-name)
         forms (take-while (partial not= ::eof)
                           (repeatedly #(r/read reader false ::eof)))]
     forms)))

(defn read-file
  "Reads the forms from file-name."
  [file-name]
  (let [file (io/file file-name)]
    (with-open [reader (PushbackReader. (io/reader file :encoding "UTF-8"))]
      (let [forms (read-all reader (.getName file))]
        (when-not (seq forms)
          (println-err "WARNING: no code read from" file-name))
        (doall forms)))))

(defn read-files
  "Reads the forms from the files in file-paths, or, if file-paths is empty,
  reads forms from stdin."
  [file-paths]
  (if (seq file-paths)
    (mapcat read-file file-paths)
    (let [forms (read-all *in*)]
      (when-not (seq forms)
        (println-err "WARNING: no code read from stdin"))
      forms)))

(defn compile
  "Parses, analyzes, and compiles form into Sybilant special forms for assembly
  code."
  [options form]
  form)

(defn emit
  "Returns a list of strings, one for each line of assembly code for exp."
  [options exp]
  [(pr-str exp)])

(defn emit-section
  "Returns a list of strings for the specified section name and exps."
  [options section-name exps]
  (cons (str "section " section-name) (mapcat (partial emit options) exps)))

(defn section
  "Returns the assembly file section into which exp should be assembled."
  [exp]
  ".text")

(defn compile-and-emit-all
  "Compiles forms, groups them by section, emits the lines of assembly code for
  each exp, and returns a flat sequence of the lines of assembly code."
  [options forms]
  (let [exps (map (partial compile options) forms)
        sections (group-by section exps)]
    (mapcat (partial emit-section options) (keys sections) (vals sections))))

(defn validate-infiles
  "Validates that each infile exists.  If any infile does not exist, then an
  exception is thrown."
  [infiles]
  (doseq [infile infiles
          :when (not (file-exists? infile))]
    (die 1 (str "Error: " infile " does not exist"))))

(defn validate-outfile
  "Validates that outfile (if specified) does not exist, or if it does exists,
  then force? must be true."
  [outfile force?]
  (when (and outfile (file-exists? outfile) (not force?))
    (die 1 (str "Error: " outfile " already exists"))))

(defn write-lines
  "Writes lines (and a newline for each line) to outfile, or if outfile is nil,
  then write the lines to stdout."
  [outfile lines]
  (letfn
      [(write-lines [^java.io.Writer out lines]
         (doseq [^String line lines]
           (.write out line)
           (.write out "\n")))]
    (when (seq lines)
      (if outfile
        (with-open [out (io/writer (io/file outfile) :encoding "UTF-8")]
          (write-lines out lines))
        (write-lines *out* lines)))))

(defn exit
  "Exit the with exit-code."
  [exit-code]
  (System/exit exit-code))

(defn -main
  [& args]
  (try+
   (let [options (parse-args (expand-args args []) default-options)
         {:keys [outfile infiles force?]} options]
     (validate-infiles infiles)
     (validate-outfile outfile force?)
     (->> (read-files infiles)
          (compile-and-emit-all options)
          (write-lines outfile)))
   (exit 0)
   (catch Throwable t
     (binding [*out* *err*]
       (print "Unexpected error: ")
       (print-stack-trace t)
       (flush))
     (exit 1))
   (catch :exit-code {:keys [exit-code exit-message]}
     (when exit-message
       (if (pos? exit-code)
         (println-err exit-message)
         (println exit-message)))
     (exit exit-code))
   (catch Object o
     (println-err "Unexpected error:" (pr-str o))
     (exit 1))))
