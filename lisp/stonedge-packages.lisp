(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage :com.informatimago.games.stonedge
  (:use
   :common-lisp
   :split-sequence)

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
   #:level
   #:level-title
   #:level-description
   #:level-cells
   #:level-start-cell
   #:game-status
   #:game
   #:game-stone
   #:game-cells
   #:make-game-from-level
   #:solid-cell
   #:start-cell
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
   #:stone-coverage
   ;;
   #:display
   #:*levels*
   #:named-cell
   #:cell-name
   #:definition
   #:definition-name
   #:definition-link
   #:definition-connected
   #:definition-state
   #:level-named-cells
   #:level-definitions
   #:*reserved-names*
   #:*button-cell-names*
   #:*pathway-cell-names*
   ))

