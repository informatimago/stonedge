(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage :com.informatimago.games.stonedge.player
  (:use :common-lisp
        :com.informatimago.games.stonedge
        :com.informatimago.games.stonedge.parser)
  (:export "STONEDGE" "MOVE"))

(in-package :com.informatimago.games.stonedge.player)

(defmethod move ((game game) direction)
  "
Moves the stone of the game in the given direction.
"
  (let ((stone (game-stone game))
        (cells (game-cells game)))
    (multiple-value-bind (direction stone-left stone-back stone-right stone-front) (stone-coverage stone)
      (if (eql direction :vertical)
          (stone-left-cell stone (aref cells stone-back stone-left))
          (progn
            (stone-left-cell stone (aref cells stone-back stone-left))
            (stone-left-cell stone (aref cells stone-front stone-right)))))
    (move (game-stone game) direction)
    (multiple-value-bind (direction stone-left stone-back stone-right stone-front) (stone-coverage stone)
      (if (eql direction :vertical)
          (stone-moved-over-cell stone (aref cells stone-back stone-left))
          (progn
            (stone-moved-over-cell stone (aref cells stone-back stone-left))
            (stone-moved-over-cell stone (aref cells stone-front stone-right))))))
  game)

(defun stonedge (level)
  "
Play the playtomo stonedge game for the given LEVEL.
See PARSE-LEVEL for the description of LEVEL.
"
  (let ((game (make-game-from-level level)))
    (handler-case
        (loop
         (display game *query-io*)
         (format *query-io* "Your move: ")
         (block :abort
           (let ((input (string-trim #(#\space #\tab) (read-line *query-io*))))
             (unless (zerop (length input))
               (move game
                     (case (char input 0)
                       ((#\j #\4) :left)
                       ((#\l #\6) :right)
                       ((#\i #\8) :front)
                       ((#\k #\2) :back)
                       (otherwise (return-from :abort))))))))
      (game-won  () (display game *query-io*) (format t "~%You win!~2%"))
      (game-lost () (display game *query-io*) (format t "~%You lose!~2%")))
    (values)))

;;;; THE END ;;;; 
