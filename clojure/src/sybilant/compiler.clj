;; Copyright © 2024 Paul Stadig
;;
;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
;; of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public
;; License, v. 2.0.
(ns sybilant.compiler
  "A whole program compiler for Sybilant.  It compiles one or more files into a single assembly file
  that can be assembled and linked with external programs."
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]))

(defn register?
  [operand]
  (contains?
   '#{rdi eax ebx}
   operand))

(defn emit-operand
  [operand]
  (cond
    (number? operand)
    (format "$%s" operand)
    (register? operand)
    (format "%%%s" operand)
    (symbol? operand)
    (format "%s" operand)))

(defn emit-operands
  [operands]
  (when (seq operands)
    (format " %s" (str/join ", " (map emit-operand operands)))))

(defn emit-statement
  [[op & operands]]
  (str op (emit-operands operands)))

(defn emit-exp
  "Emit exp as a sequence of assembly statements."
  [[_ [_ sym] & statements]]
  (if (seq statements)
    (into [".text"
           (format ".global %s" sym)
           (format "%s:" sym)]
          (map emit-statement)
          statements)
    [".text"
     (format ".extern %s" sym)]))

(defn compile-forms
  "Compile forms into a sequence of assembly statements."
  [forms]
  (mapcat emit-exp forms))
