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
  (:refer-clojure :exclude [compile]))

(defn emit-exp
  "Emit exp as a sequence of assembly instructions."
  [_exp]
  [".text"
   ".global _start"
   "_start:"
   "movl $1, %eax"
   "movl $0, %ebx"
   "int $0x80"])

(defn compile-forms
  "Compile forms into a sequence of assembly instructions."
  [forms]
  (mapcat emit-exp forms))
