;; Copyright © 2024 Paul Stadig
;;
;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
;; of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public
;; License, v. 2.0.
(ns sybilant.compile
  (:require
   [clojure.java.io :as io]
   [sybilant.compiler :as compiler]))

(defn read-file
  [file]
  (with-open [f (-> file io/file io/reader java.io.PushbackReader.)]
    (into []
      (take-while (complement #{::eof}))
      (repeatedly #(read f false ::eof)))))

(defn compile-files
  [files]
  (compiler/compile-forms (mapcat read-file files)))

(defn -main
  [& files]
  (doseq [line (compile-files files)]
    (println line))
  (System/exit 0))
