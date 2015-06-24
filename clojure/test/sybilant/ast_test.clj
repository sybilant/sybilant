;;;; Copyright © Paul Stadig.  All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0.  If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns sybilant.ast-test
  (:require
   [clojure.test :refer [are deftest is]]
   [sybilant.ast :as ast]))

(deftest t-subtype?
  (are [t0 t1]
    (ast/subtype? t0 t1)
    (ast/uint8-type 1 2) (ast/uint8-type 0 3)
    (ast/uint16-type 1 2) (ast/uint16-type 0 3)
    (ast/uint32-type 1 2) (ast/uint32-type 0 3)
    (ast/uint64-type 1 2) (ast/uint64-type 0 3)
    (ast/int8-type 1 2) (ast/int8-type 0 3)
    (ast/int16-type 1 2) (ast/int16-type 0 3)
    (ast/int32-type 1 2) (ast/int32-type 0 3)
    (ast/int64-type 1 2) (ast/int64-type 0 3)))
