(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage :com.informatimago.games.stonedge.generator
  (:use
   :common-lisp
   :split-sequence
   :com.informatimago.common-lisp.cesarum.a-star)
  (:import-from :com.informatimago.common-lisp.cesarum.array
                #:copy-array)
  (:shadow #:copy)
  (:export
   "GENERATE"
   "MAIN"))

