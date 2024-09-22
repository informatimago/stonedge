;;; Stonedge Game Generator in Common Lisp

(defpackage :com.informatimago.games.stonedge.generator
  (:use
   :common-lisp
   :split-sequence))
(in-package :com.informatimago.games.stonedge.generator)

;;; Stonedge Game Generator in Common Lisp

;; Define cell types
(defconstant +cell-types+
  '(:empty :solid :origin :target :ice :crumble :pathway :button))

;; Define directions
(defparameter *directions*
  '(:left :right :up :down))

;; Define a structure for cells
(defstruct cell
  type          ; Cell type
  state         ; For pathway cells (:open or :closed)
  button-type   ; For button cells (:red or :blue)
  name          ; Name of the cell (for buttons and pathways)
  linked-cells) ; List of linked pathway cells (for buttons)

;; Define the board as a hash table mapping positions to cells
(defparameter *board* (make-hash-table :test 'equal))

;; Utility function to set a cell on the board
(defun set-cell (x y cell)
  (setf (gethash (cons x y) *board*) cell))

;; Utility function to get a cell from the board
(defun get-cell (x y)
  (gethash (cons x y) *board*))

;; Utility function to check if a cell exists and is solid or special
(defun solid-cell-p (pos)
  (let ((cell (get-cell (car pos) (cdr pos))))
    (and cell (not (member (cell-type cell) '(:empty))))))

;; Generate a random integer between min and max (inclusive)
(defun random-between (min max)
  (+ min (random (1+ (- max min)))))

;; Initialize the board with empty cells
(defun initialize-board (width height)
  (clrhash *board*)
  (loop for x from -2 to (+ width 2)
        do (loop for y from -2 to (+ height 2)
                 do (set-cell x y (make-cell :type :empty)))))

;; Place the origin and target cells on the board
(defun place-origin-and-target (width height)
  (let ((origin-x (random-between 1 (- width 2)))
        (origin-y (random-between 1 (- height 2)))
        (target-x (random-between 1 (- width 2)))
        (target-y (random-between 1 (- height 2))))
    ;; Ensure that the origin and target are not at the same position
    (when (and (= origin-x target-x) (= origin-y target-y))
      (setf target-x (mod (+ target-x 1) width)))
    (set-cell origin-x origin-y (make-cell :type :origin))
    (set-cell target-x target-y (make-cell :type :target))
    (values (cons origin-x origin-y) (cons target-x target-y))))

;; Define a structure for the menhir's state
(defstruct menhir
  position     ; (x . y)
  orientation) ; :vertical or :horizontal-<direction>

;; Get the cells occupied by the menhir based on its state
(defun menhir-cells (menhir)
  (let ((x (car (menhir-position menhir)))
        (y (cdr (menhir-position menhir))))
    (case (menhir-orientation menhir)
      (:vertical
       (list (cons x y)))
      (:horizontal-left
       (list (cons x y) (cons (- x 1) y)))
      (:horizontal-right
       (list (cons x y) (cons (+ x 1) y)))
      (:horizontal-up
       (list (cons x y) (cons x (- y 1))))
      (:horizontal-down
       (list (cons x y) (cons x (+ y 1))))
      (otherwise
       (list (cons x y))))))

;; Check if the menhir can move in a given direction
(defun menhir-can-move-p (menhir direction)
  (let ((new-menhir (compute-new-menhir menhir direction)))
    (every #'solid-cell-p (mapcar #'identity (menhir-cells new-menhir)))))

;; Compute the new menhir state after moving in a direction
(defun compute-new-menhir (menhir direction)
  (let ((x (car (menhir-position menhir)))
        (y (cdr (menhir-position menhir)))
        (orientation (menhir-orientation menhir)))
    (case direction
      (:left
       (case orientation
         (:vertical
          (make-menhir :position (cons (- x 1) y) :orientation :horizontal-left))
         (:horizontal-left
          (make-menhir :position (cons (- x 2) y) :orientation :vertical))
         (:horizontal-right
          (make-menhir :position (cons x y) :orientation :vertical))
         (otherwise menhir)))
      (:right
       (case orientation
         (:vertical
          (make-menhir :position (cons (+ x 1) y) :orientation :horizontal-right))
         (:horizontal-right
          (make-menhir :position (cons (+ x 2) y) :orientation :vertical))
         (:horizontal-left
          (make-menhir :position (cons x y) :orientation :vertical))
         (otherwise menhir)))
      (:up
       (case orientation
         (:vertical
          (make-menhir :position (cons x (- y 1)) :orientation :horizontal-up))
         (:horizontal-up
          (make-menhir :position (cons x (- y 2)) :orientation :vertical))
         (:horizontal-down
          (make-menhir :position (cons x y) :orientation :vertical))
         (otherwise menhir)))
      (:down
       (case orientation
         (:vertical
          (make-menhir :position (cons x (+ y 1)) :orientation :horizontal-down))
         (:horizontal-down
          (make-menhir :position (cons x (+ y 2)) :orientation :vertical))
         (:horizontal-up
          (make-menhir :position (cons x y) :orientation :vertical))
         (otherwise menhir)))
      (otherwise menhir))))

;; Generate a path from start to end positions, considering menhir's movement
(defun generate-path (start-menhir end-pos)
  (let ((open-list (list start-menhir))
        (closed-list '())
        (came-from (make-hash-table :test 'equal))
        (found nil))
    (loop while open-list do
          (let ((current (pop open-list)))
            (push current closed-list)
            ;; Check if we have reached the end position in vertical orientation
            (when (and (equal (menhir-position current) end-pos)
                       (eql (menhir-orientation current) :vertical))
              (setf found current)
              (return))
            ;; Generate possible moves
            (dolist (direction *directions*)
              (let ((new-menhir (compute-new-menhir current direction)))
                ;; Check if new position is valid and not already visited
                (when (and (not (member new-menhir closed-list :test #'equal))
                           (every #'solid-cell-p (menhir-cells new-menhir)))
                  (push new-menhir open-list)
                  (setf (gethash new-menhir came-from) current)))))
          )
    ;; Reconstruct path and set solid cells
    (when found
      (let ((path '())
            (current found))
        (loop while current do
              (push current path)
              (setf current (gethash current came-from)))
        ;; For each menhir state in the path, ensure that the cells it occupies are solid
        (dolist (menhir path)
          (dolist (pos (menhir-cells menhir))
            (let ((cell (get-cell (car pos) (cdr pos))))
              (when (eql (cell-type cell) :empty)
                (set-cell (car pos) (cdr pos) (make-cell :type :solid))))))
        (values t path))
      )
    ))

;; Generate random special cells (ice, crumble, pathway, buttons)
(defun generate-special-cells (width height)
  ;; Similar to before but ensure cells are solid before modifying
  (let ((num-special (random-between 5 10)))
    (loop repeat num-special
          do (let ((x (random-between 0 (- width 1)))
                   (y (random-between 0 (- height 1))))
               (let ((cell (get-cell x y)))
                 (when (and cell (member (cell-type cell) '(:solid)))
                   (let ((rand (random 100)))
                     (cond
                       ((< rand 25)
                        ;; Place an ice cell
                        (set-cell x y (make-cell :type :ice)))
                       ((< rand 50)
                        ;; Place a crumble cell
                        (set-cell x y (make-cell :type :crumble)))
                       ((< rand 75)
                        ;; Place a pathway cell
                        (set-cell x y (make-cell :type :pathway
                                                 :state (if (< (random 2) 1)
                                                            :open
                                                            :closed)
                                                 :name (code-char (+ 65 (random 26))))))
                       (t
                        ;; Place a button cell
                        (set-cell x y (make-cell :type :button
                                                 :button-type (if (< (random 2) 1)
                                                                  :red
                                                                  :blue)
                                                 :name (code-char (+ 65 (random 26))))
                                  ))))))))))

;; Format the board into the game description string
(defun format-board (width height)
  (with-output-to-string (out)
    (loop for y from 0 below height
          do (loop for x from 0 below width
                   do (let ((cell (get-cell x y)))
                        (princ (case (cell-type cell)
                                 (:empty ".")
                                 (:solid "O")
                                 (:origin "S")
                                 (:target "T")
                                 (:ice "I")
                                 (:crumble "C")
                                 (:pathway (string (cell-name cell)))
                                 (:button (string (cell-name cell)))
                                 (t ".")) out)))
             (princ #\Newline out))))

;; Generate the game description clauses for buttons and pathways
(defun format-clauses ()
  (let ((clauses '()))
    (maphash (lambda (pos cell)
               (when (or (eql (cell-type cell) :button)
                         (eql (cell-type cell) :pathway))
                 (let ((name (intern (string (cell-name cell)))))
                   (cond
                     ((eql (cell-type cell) :button)
                      ;; For buttons, specify type and linked pathways
                      (push `(,name ,(cell-button-type cell)
                                    ,@(cell-linked-cells cell))
                            clauses))
                     ((eql (cell-type cell) :pathway)
                      ;; For pathways, specify initial state
                      (push `(,name :pathway ,(cell-state cell))
                            clauses))))))
             *board*)
    (nreverse clauses)))

;; Main function to generate a Stonedge game
(defun generate-stonedge-game (&key (width 10) (height 10))
  (initialize-board width height)
  (multiple-value-bind (origin-pos target-pos)
      (place-origin-and-target width height)
    ;; Initialize menhir at origin position in vertical orientation
    (let ((start-menhir (make-menhir :position origin-pos :orientation :vertical)))
      ;; Generate path to target position
      (multiple-value-bind (found path)
          (generate-path start-menhir target-pos)
        (if found
            (progn
              ;; Generate additional random paths and special cells
              (generate-special-cells width height)
              ;; Format the board and clauses
              (let ((board-string (format-board width height))
                    (clauses (format-clauses)))
                ;; Return the game description
                (list board-string clauses)))
            (error "Failed to generate a solvable game."))))))

;;; Example usage
(defun gen ()
  (let ((game (generate-stonedge-game)))
    (format t "Generated Stonedge Game:~%")
    (format t "~A~%" (first game))
    (dolist (clause (second game))
      (format t "~S~%" clause))))
