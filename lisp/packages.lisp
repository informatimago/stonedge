(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage :com.informatimago.games.stonedge
  (:use
   :common-lisp
   :split-sequence
   :com.informatimago.common-lisp.cesarum.graph
   :com.informatimago.common-lisp.graphviz.graph-dot)
  (:import-from :com.informatimago.common-lisp.cesarum.array
                #:copy-array)
  (:shadow #:copy)

  (:export
   #:stonedge
   #:solve-problem
   #:*simple*
   #:*level-36*
   #:*level-37*
   #:*level-38*
   #:*level-39*
   #:*level-52*)

  (:export
   #:stone
   #:stone-x
   #:stone-y
   #:stone-direction
   #:rotate
   #:negate
   #:normalize
   #:move
   #:verticalp
   #:lateralp
   #:frontp
   #:cell
   #:cell-x
   #:cell-y
   #:game-won
   #:game-lost
   #:stone-moved-over-cell
   #:stone-left-cell
   #:game-status
   #:game
   #:game-stone
   #:game-cells
   #:solid-cell
   #:target-cell
   #:empty-cell
   #:button-cell
   #:button-cell-switches
   #:red-button-cell
   #:blue-button-cell
   #:pathway-cell
   #:pathway-cell-state
   #:crumble-cell
   #:crumble-cell-state
   #:ice-cell
   #:text-icon
   #:stone-coverage))

(defpackage :com.informatimago.games.stonedge.generator
  (:use
   :common-lisp
   :split-sequence
   :com.informatimago.common-lisp.cesarum.a-star)
  (:import-from :com.informatimago.common-lisp.cesarum.array
                #:copy-array)
  (:shadow #:copy)
  (:export
   "GENERATE"))

(defpackage :com.informatimago.games.stonedge.render
  (:use
   :common-lisp)
  (:export
   "D" "H" "ALPHA"
   "CELL-CORNER-RADIUS" "SQUARE-SIZE" "INSET"
   ;;--
   "CELL-PATHS" "STONE-PATHS" "RENDER-GAME"
   "POINT" "POINT-X" "POINT-Y" "COPY-POINT"
   ))


(defpackage :com.informatimago.games.stonedge.render-png
  (:use
   :common-lisp
   :vecto
   :com.informatimago.games.stonedge.render
   :com.informatimago.games.stonedge)
  (:export))
