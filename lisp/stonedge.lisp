;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               playtomo-stonedge.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements the Playtomo Stonedge Game, and its solver.
;;;;    See http://www.playtomo.com/ (not much here when I went);
;;;;    Download the playtomo games on BlackBerry.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-07-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package :com.informatimago.games.stonedge)

(defgeneric display (object stream &key &allow-other-keys))

;;;-----------------------------------------------------------------------------
;;;
;;; STONE
;;;


(defclass stone ()
  ((x :initform 0
      :initarg :x
      :reader stone-x
      :documentation "Ordinate of the first cube of the stone.")
   (y :initform 0
      :initarg :y
      :reader stone-y
      :documentation "Coordinate of the first cube of the stone.")
   (direction :initform (vector 0 0 1)
              :initarg :direction
              :reader stone-direction
              :documentation "A unit vector indicating the direction of the stone.
The coordinate of the other cube of the stone is given by adding this vector to
the coordinates of the first cube.
Note: The stone is normalized so that the vertical coordinate of the direction is either 0 or 1."))
  (:documentation "A stone is made of two cubes of size equal to the cells.
To move, it rotates 1/4 turn on one of its edges that is in contact with the cells."))


(defconstant +right+ 0)
(defconstant +left+  1)
(defconstant +front+ 2)
(defconstant +back+  3)


;;
;;              ^
;;             y|front
;;              |
;;              |
;; left         |                 right
;; -------------+-------------------->
;;             0|                   x
;;              |
;;              |
;;              |back
;;

(defparameter *rotations*
  ;;                   x               x                  x                  x
  ;;                   y               y                  y                  y
  ;;          right    z      left     z         front    z         back     z
  ;;        -------   ---   --------  ---      --------  ---      -------   ---
  ;;          0 0 1    z      0 0 -1  -z        1  0  0   x        1  0  0   x
  ;;          0 1 0    y      0 1 0    y        0  0  1   z        0  0 -1  -z
  ;;         -1 0 0   -x      1 0 0    x        0 -1  0  -y        0  1  0   y
  #(
    ;; +right+
    #2a((0 0 1)
        (0 1 0)
        (-1 0 0))
    ;; +left+
    #2a((0 0 -1)
        (0 1 0)
        (1 0 0))
    ;; +front+
    #2a((1 0 0)
        (0 0 1)
        (0 -1 0))
    ;; +back+
    #2a((1 0 0)
        (0 0 -1)
        (0 1 0)))
  "A vector of 3D rotation matrices for right, left, front and back rotations.")


(defun rotate (matrix vector)
  "Returns matrix * vector"
  (coerce
   (loop
      :for i :from 0 :to 2
      :collect (loop
                  :for j :from 0 :to 2
                  :sum (* (aref vector j) (aref matrix i j))))
   'vector))


(defun test-rotate ()
  (assert
   (equalp
    (let ((result '()))
      (dolist (direction (list #(1 0 0) #(-1 0 0) #(0 1 0) #(0 -1 0) #(0 0 1) #(0 0 -1))
                         result)
        (dotimes (rotation 4)
          (push (list direction (aref #(left--> right-> front-> back-->) rotation)
                      (rotate (aref *rotations* rotation)  direction))
                result))))
    '((#(0 0 -1) back--> #(0 1 0))
      (#(0 0 -1) front-> #(0 -1 0))
      (#(0 0 -1) right-> #(1 0 0))
      (#(0 0 -1) left--> #(-1 0 0))
      (#(0 0 1) back--> #(0 -1 0))
      (#(0 0 1) front-> #(0 1 0))
      (#(0 0 1) right-> #(-1 0 0))
      (#(0 0 1) left--> #(1 0 0))
      (#(0 -1 0) back--> #(0 0 -1))
      (#(0 -1 0) front-> #(0 0 1))
      (#(0 -1 0) right-> #(0 -1 0))
      (#(0 -1 0) left--> #(0 -1 0))
      (#(0 1 0) back--> #(0 0 1))
      (#(0 1 0) front-> #(0 0 -1))
      (#(0 1 0) right-> #(0 1 0))
      (#(0 1 0) left--> #(0 1 0))
      (#(-1 0 0) back--> #(-1 0 0))
      (#(-1 0 0) front-> #(-1 0 0))
      (#(-1 0 0) right-> #(0 0 -1))
      (#(-1 0 0) left--> #(0 0 1))
      (#(1 0 0) back--> #(1 0 0))
      (#(1 0 0) front-> #(1 0 0))
      (#(1 0 0) right-> #(0 0 1))
      (#(1 0 0) left--> #(0 0 -1)))))
  :success)


(test-rotate)


(defun negate (vector)
  "Returns   - vector"
  (map 'vector (lambda (x) (- x)) vector))


(defgeneric normalize (stone direction)
  (:documentation "
Normalize the stone for a rotation in the given direction.
DIRECTION: (member :left :right :front :back)
")
  (:method ((self stone) (direction (eql :right)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (when (plusp (aref direction 0))
        (progn
          (incf x)
          (setf direction (negate direction)))))
    self)
  (:method ((self stone) (direction (eql :left)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (when (minusp (aref direction 0))
        (progn
          (decf x)
          (setf direction (negate direction)))))
    self)
  (:method ((self stone) (direction (eql :front)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (when (plusp (aref direction 1))
        (progn
          (incf y)
          (setf direction (negate direction)))))
    self)
  (:method ((self stone) (direction (eql :back)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (when (minusp (aref direction 1))
        (progn
          (decf y)
          (setf direction (negate direction)))))
    self))


(defgeneric move (stone direction)
  (:documentation "Move the stone in the given direction.")
  (:method ((self stone) (direction (eql :right)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (incf x)
      (setf direction  (rotate (aref *rotations* +right+) direction)))
    self)
  (:method ((self stone) (direction (eql :left)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (decf x)
      (setf direction  (rotate (aref *rotations* +left+) direction)))
    self)
  (:method ((self stone) (direction (eql :front)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (incf y)
      (setf direction  (rotate (aref *rotations* +front+) direction)))
    self)
  (:method ((self stone) (direction (eql :back)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (decf y)
      (setf direction  (rotate (aref *rotations* +back+) direction)))
    self))

(defmethod move :before ((self stone) direction) (normalize self direction))



(defun verticalp (direction)
  (not (zerop (aref direction 2))))

(defun lateralp (direction)
  (not (zerop (aref direction 0))))

(defun frontp (direction)
  (not (zerop (aref direction 1))))


(defmethod print-object ((self stone) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (x y direction) self
      (format stream "~A,~A ~S~%" x y direction)
      (cond
        ((verticalp direction)
         (format stream "\"
     +---+
    /   /|
   +---+ |
   |   |/|
   |---| +
   |   |/
   +---+  ~A,~A
\"" x y))

        ((lateralp direction)
         (apply (function format) stream "\"
     +-------+
    /   /   /|
   +-------+ +
   |   |   |/
   +-------+ ~A,~A
\""
                (if (minusp (aref direction 0))
                    (list x y)
                    (list (1+ x) y))))

        ((frontp direction)
         (apply (function format) stream "\"
       +---+
      /   /|
     /---/ +
    /   /|/
   +---+ /
   |   |/
   +---+ ~A,~A
\""
                (if (plusp (aref direction 1))
                    (list x y)
                    (list x (1- y))))))))
  self)



;;;-----------------------------------------------------------------------------
;;;
;;; CELLS
;;;

(defclass cell ()
  ((x :initform 0
      :initarg :x
      :reader cell-x
      :documentation "Lateral coordinate.")
   (y :initform 0
      :initarg :y
      :reader cell-y
      :documentation "Front coordinate."))
  (:documentation "This is an abstract cell. Cells are square, and all of the same size."))

(define-condition game-won  () ())
(define-condition game-lost () ())

(defgeneric stone-moved-over-cell (stone cell)
  (:documentation "Do cell specific behavior when the stone moves over the cell.
May signal a game-won or game-lost condition.")
  (:method (stone cell)
    (declare (ignorable stone cell))
    (values)))

(defgeneric stone-left-cell (stone cell)
  (:documentation "Do cell specific behavior when the stone moves from the cell.
May signal a game-won or game-lost condition.")
  (:method (stone cell)
    (declare (ignorable stone cell))
    (values)))

(defgeneric game-status (stone cell)
  (:documentation "Returns nil :win or :lose depending on what would happen if the stone was on the cell.")
  (:method (stone cell)
    (declare (ignorable stone cell))
    nil))




(defclass solid-cell (cell)
  ()
  (:documentation "The stone may remain securely on a solid cell."))

(defclass start-cell (solid-cell)
  ()
  (:documentation "The start position in the grid. There must be only one start cell."))

(defclass target-cell (solid-cell)
  ()
  (:documentation "Once the stone is in vertical position on a target cell,
the game is won."))

(defmethod stone-moved-over-cell (stone (cell target-cell))
  (declare (ignorable stone cell))
  (when (verticalp (stone-direction stone))
    (signal 'game-won)))

(defmethod game-status (stone (cell target-cell))
  (declare (ignorable cell))
  (when (verticalp (stone-direction stone))
    :win))



(defclass empty-cell (cell)
  ()
  (:documentation "When the stone is over an empty cell,
the game is lost."))

(defmethod stone-moved-over-cell (stone (cell empty-cell))
  (declare (ignorable stone cell))
  (signal 'game-lost))

(defmethod game-status (stone (cell empty-cell))
  (declare (ignorable stone cell))
  :lose)

(defclass crumble-cell (cell)
  ((state :initform :open
          :initarg :state
          :reader crumble-cell-state
          :type (member :closed :open)
          :documentation "A crumble cell goes from :open to :closed the first time
it's walked over, and stays :closed thereafter."))
  (:documentation "When a crumble cell is :open, it supports a stone;
when it's :closed the stone falls down and
the game is lost."))

(defmethod stone-moved-over-cell ((s stone) (cell crumble-cell))
  (declare (ignorable s))
  (when (eql :closed (crumble-cell-state cell))
    (signal 'game-lost)))

(defmethod stone-left-cell (stone (cell crumble-cell))
  (declare (ignorable stone))
  (setf (slot-value cell 'state) :closed))

(defmethod game-status (stone (cell crumble-cell))
  (declare (ignorable stone))
  (when (eql :closed (crumble-cell-state cell))
    :lose))


(defclass ice-cell (cell)
  ()
  (:documentation "An ice cell supports an horizontal stone, but
when the stone is over it in vertical position, it breaks, the stone falls down, and
the game is lost."))

(defmethod stone-moved-over-cell ((s stone) (cell ice-cell))
  (declare (ignorable cell))
  (when (verticalp (stone-direction s))
    (signal 'game-lost)))

(defmethod game-status (stone (cell ice-cell))
  (declare (ignorable cell))
  (when (verticalp (stone-direction stone))
    :lose))


(defgeneric cell-name (cell)
  (:method ((cell empty-cell))   ".")
  (:method ((cell solid-cell))   "O")
  (:method ((cell start-cell))   "S")
  (:method ((cell target-cell))  "T")
  (:method ((cell ice-cell))     "I")
  (:method ((cell crumble-cell)) "C"))

(defparameter *reserved-names* " .OSTICXYZ")
(defparameter *button-cell-names* "ABDEFGHJKLMNPQRUVW")
(defparameter *pathway-cell-names* "0123456789!@#$%^&*()_+-={}[]")

(defclass named-cell (cell)
  ((name :initarg :name
         :reader cell-name
         :type string
         :documentation "A one-char string containing the name of the cell.")))

(defclass button-cell (named-cell)
  ((switches :initform '()
             :initarg :switches
             :accessor button-cell-switches
             :documentation "A list of cells that may be switched when the stone is over the button cell."))
  (:documentation "This is an abstract button cell.
Button cells may switch the state of pathway-cells."))

(defgeneric switch-pathway-cells (button-cell)
  (:documentation "Switches the associated pathway-cells.")
  (:method ((self button-cell))
    (map nil (function switch-cell) (button-cell-switches self))
    self))



(defclass red-button-cell (button-cell)
  ()
  (:documentation "A red button cell switches its pathway cells
as soon as the stone is over it."))

(defmethod stone-moved-over-cell ((s stone) (cell red-button-cell))
  (declare (ignorable s))
  (switch-pathway-cells cell))



(defclass blue-button-cell (button-cell)
  ()
  (:documentation "A blue button cell switches its pathway cells
only when the stone is over it in vertical position."))

(defmethod stone-moved-over-cell ((s stone) (cell blue-button-cell))
  (when (verticalp (stone-direction s))
    (switch-pathway-cells cell)))



(defclass pathway-cell (named-cell)
  ((state :initform :closed
          :initarg :state
          :reader pathway-cell-state
          :type (member :closed :open)
          :documentation "A pathway cell may be :open or :closed."))
  (:documentation "When a pathway cell is :open, it supports a stone;
when it's :closed the stone falls down and
the game is lost."))

(defmethod stone-moved-over-cell ((s stone) (cell pathway-cell))
  (declare (ignorable s))
  (when (eql :closed (pathway-cell-state cell))
    (signal 'game-lost)))

(defmethod game-status (stone (cell pathway-cell))
  (declare (ignorable stone))
  (when (eql :closed (pathway-cell-state cell))
    :lose))

(defmethod switch-cell ((self pathway-cell))
  (setf (slot-value self 'state) (ecase (pathway-cell-state self)
                                   ((:open)   :closed)
                                   ((:closed) :open)))
  self)





;;;-----------------------------------------------------------------------------
;;;
;;; LEVEL
;;;
;;; We reify the level class to be able to manipulate and edit them
;;; easily in the level explorer.

(defclass definition ()
  ((name      :initarg :name
              :type string
              :reader definition-name)
   (link      :initarg :link
              :initform :pathway
              :type (member :red :blue :pathway)
              :reader definition-link)
   (connected :initarg :connected
              :initform '()
              :type list ; list of cell-name strings
              :reader definition-connected)
   (state     :initarg :state
              :initform :closed
              :type (member :open :closed)
              :reader definition-state))
  (:documentation "Definition of a red-button, blue-button or pathway cell."))

(defclass level ()
  ((title       :initarg :title
                :initform ""
                :accessor level-title
                :documentation "The title of the level (single line).")
   (description :initarg :description
                :initform ""
                :accessor level-description
                :documentation "The description of the level (multiline).")
   (cells       :initarg :cells
                :initform #2a()
                :reader level-cells
                :documentation "The cells.")
   (named-cells :initarg :named-cells
                :initform (make-hash-table :test (function equal))
                :reader level-named-cells
                :type hash-table
                :documentation "A map from cell names (strings) to cells.")
   (definitions :initarg :definitions
                :initform (make-hash-table :test (function equal))
                :reader level-definitions
                :type hash-table
                :documentation "A map from cell names (string) to cell definition.")))


(defmethod level-start-cell ((level level))
  (let ((cells (level-cells level)))
    (loop :for y :below (array-dimension cells 0)
          :do (loop :for x :below (array-dimension cells 1)
                    :if (typep (aref cells y x) 'start-cell)
                    :do (return-from level-start-cell (values (aref cells y x) y x))))
    (cerror "Continue with no start cell" "Level ~S is missing a start cell"
            (level-title level))
    nil))


(defmethod print-object ((level level) stream)
  (print-unreadable-object (level stream :identity t :type t)
    (format stream ":TITLE ~S~% :DESCRIPTION ~S~% :CELLS ~%"
            (level-title level)
            (level-description level))
    (display (level-cells level) stream))
  level)

;;;-----------------------------------------------------------------------------
;;;
;;; GAME
;;;


(defclass game ()
  ((stone :initform (make-instance 'stone)
          :initarg :stone
          :reader game-stone
          :documentation "The stone.")
   (cells :initform #2a()
          :initarg :cells
          :reader game-cells
          :documentation "The cells.")
   (level-title :initarg :level-title
                :initform ""
                :accessor level-title
                :documentation "The title of the current level (single line).")
   (level-description :initarg :level-description
                      :initform ""
                      :accessor level-description
                      :documentation "The description of the current level (multiline).")))


(defmethod make-game-from-level ((level level))
  (multiple-value-bind (start-cell x y) (level-start-cell level)
    (make-instance 'game
                   :stone (make-instance 'stone :x x :y y :direction #(0 0 1))
                   :level-title (level-title level)
                   :level-description (level-description level)
                   :cells (level-cells level))))


(defgeneric text-icon (cell)
  (:documentation "Returns a three-character strings denoting graphically the cell.")
  (:method ((cell empty-cell))       (declare (ignorable cell)) "   ")
  (:method ((cell solid-cell))       (declare (ignorable cell)) "SSS")
  (:method ((cell start-cell))       (declare (ignorable cell)) "BGN")
  (:method ((cell red-button-cell))  (declare (ignorable cell)) "[R]")
  (:method ((cell blue-button-cell)) (declare (ignorable cell)) "[B]")
  (:method ((cell ice-cell))         (declare (ignorable cell)) ",,,")
  (:method ((cell target-cell))      (declare (ignorable cell)) "TTT")
  (:method ((cell crumble-cell))
    (declare (ignorable cell))
    (if (eql :open (crumble-cell-state cell))
        "CCC"
        "   "))
  (:method ((cell pathway-cell))
    (declare (ignorable cell))
    (if (eql :open (pathway-cell-state cell))
        "---"
        " / ")))


(defun stone-coverage (stone)
  "
Returns:   direction; left; back; right; front
DIRECTION: (member :vertical :lateral :front)
"
  (let ((direction (cond
                     ((verticalp (stone-direction stone)) :vertical)
                     ((lateralp  (stone-direction stone)) :lateral)
                     (t                                   :front))))
    (if (eql direction :vertical)
        (values direction  (stone-x stone) (stone-y stone) (stone-x stone) (stone-y stone))
        (let* ((x0  (stone-x stone))
               (x1  (+ x0 (aref (stone-direction stone) 0)))
               (y0  (stone-y stone))
               (y1  (+ y0 (aref (stone-direction stone) 1))))
          (values direction (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1))))))


(defmethod display ((cells array) stream &key stone)
  "
Prints an ASCII-art representation of the cells onto the STREAM.
"
  (let* ((line  (with-output-to-string (out)
                  (loop
                    :initially (princ "+" out)
                    :repeat (array-dimension cells 0)
                    :do (princ "---+" out)))))
    (if stone
        (multiple-value-bind (direction stone-left stone-back stone-right stone-front)
            (stone-coverage stone)
          (loop
            :for j :from (1-  (array-dimension cells 1)) :downto 0
            :initially (princ line stream) (terpri stream)
            :do (loop
                  :for i :from 0 :below (array-dimension cells 0)
                  :initially (princ "|" stream)
                  :do (unless (ecase direction
                                ((:vertical)
                                 (when (and (= stone-left i) (= stone-back j))
                                   (princ "BBB" stream) (princ "|" stream) t))
                                ((:lateral)
                                 (cond
                                   ((and (= stone-left i) (= stone-back j))
                                    (princ "BBBB" stream) t)
                                   ((and (= stone-right i) (= stone-back j))
                                    (princ "BBB" stream) (princ "|" stream) t)))
                                ((:front)
                                 (when (and (= stone-left i) (or (= stone-back j)
                                                                 (= stone-front j)))
                                   (princ "BBB" stream) (princ "|" stream) t)))
                        (princ (text-icon (aref cells i j)) stream) (princ "|" stream))
                  :finally (progn
                             (terpri stream)
                             (if (and (eql direction :front) (= stone-front j))
                                 (let ((line (copy-seq line)))
                                   (replace line "BBB" :start1 (+ 1 (* 4 stone-left)))
                                   (princ line stream))
                                 (princ line stream))
                             (terpri stream)))))
        (loop
          :for j :from (1-  (array-dimension cells 1)) :downto 0
          :initially (princ line stream) (terpri stream)
          :do (loop
                :for i :from 0 :below (array-dimension cells 0)
                :initially (princ "|" stream)
                :do (princ (text-icon (aref cells i j)) stream)
                    (princ "|" stream)
                :finally (progn
                           (terpri stream)
                           (princ line stream)
                           (terpri stream)))))))

(defmethod display ((game game) stream &key)
  "
Prints an ASCII-art representation of the GAME onto the STREAM.
"
  (display (game-cells game) stream :stone (game-stone game)))


(defmethod print-object ((self game) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "\"~%")
    (display self stream :stone (game-stone self))
    (format stream "~%\""))
  self)








;; (defparameter *game* (make-game-from-level (parse-level *problem*))
;; (stonedge *level-39*)
;; (solve-problem *problem*) (solve-problem *simple*)
;; (time (progn (reset) (explore (make-game-from-level (parse-level )) '()) (find-win)))
;;
;; (solve-problem *level-37*)
;; (solve-problem *level-38*)
;; (solve-problem *level-39*)

;; (solve-problem *level-52*)
;;
;; (let ((name "g"))
;;   (with-open-file (dot (format nil "~A.dot" name)
;;                        :direction :output
;;                        :if-does-not-exist :create
;;                        :if-exists :supersede)
;;     (princ (generate-dot (setf *g* (make-graph-from-states *states*))) dot))
;;   (ext:shell (format nil "dot -Tpng -o ~A.png  ~:*~A.dot" name)))
