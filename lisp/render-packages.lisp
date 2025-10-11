(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage :com.informatimago.games.stonedge.render
  (:use
   :common-lisp
   :com.informatimago.games.stonedge)
  ;; (:shadowing-import-from :com.informatimago.games.stonedge
  ;;                         "STONE" "GAME" "CELL")
  (:export
   "D" "H" "ALPHA"
   "CELL-CORNER-RADIUS" "SQUARE-SIZE" "INSET"
   ;;--
   "CELL-PATHS" "STONE-PATHS" "RENDER-GAME"
   "POINT" "X" "Y" "COPY-POINT"
   ))


(defpackage :com.informatimago.games.stonedge.render-png
  (:use
   :common-lisp
   :vecto
   :com.informatimago.games.stonedge.render
   :com.informatimago.games.stonedge)
  (:shadowing-import-from :vecto :rotate)
  (:export))

(defpackage :com.informatimago.games.stonedge.render-svg
  (:use
   :common-lisp
   :cl-svg
   :com.informatimago.games.stonedge.render
   :com.informatimago.games.stonedge)
  (:shadowing-import-from :cl-svg :rotate)
  (:export))
