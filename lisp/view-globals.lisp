(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package :com.informatimago.games.stonedge.render)

(defconstant d (* 50.0 2) "Side of the cell square")
(defconstant h (* 10.0 2) "Height of the cell slab")
(defconstant alpha (/ pi 6) "Projection angle")
(defconstant cell-corner-radius (* 6.0 2))
(defconstant square-size 40.0 "Assuming each square on the board has a fixed size, e.g., 40x40 points.")
(defconstant inset 10.0 "on each side.")



(defstruct (point (:type list) (:conc-name nil) (:constructor point (x y))) x y)



(defgeneric stone-paths (stone d h alpha corner-radius))
(defgeneric cell-paths  (cell  d h alpha corner-radius))
(defgeneric render-game (game pathname file-type))
