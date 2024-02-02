;; Copyright © 2024 Paul Stadig
;;
;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
;; of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public
;; License, v. 2.0.
(ns sybilant.compile
  (:require
   [sybilant.compiler :as compiler]))

(defn -main
  [& files]
  (doseq [line (compiler/compile files)]
    (println line))
  (System/exit 0))
