;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generator.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a generator of boards for the Playtomo Stonedge Game.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2024-09-30 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2024 - 2024
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

(in-package :com.informatimago.games.stonedge.generator)


(progn
 (defconstant +empty+   #\.)
 (defconstant +start+   #\S)
 (defconstant +solid+   #\O)
 (defconstant +target+  #\T)
 (defconstant +ice+     #\I)
 (defconstant +crumble+ #\C)
 ;; pseudo codes, to be replaced with identifying letters.
 (defconstant +red+     #\r)
 (defconstant +blue+    #\b)
 (defconstant +pathway+ #\P)
 )

(defun format-level (cells buttons pathways)
  (with-output-to-string (*standard-output*)
    (loop :for r :below (array-dimension cells 0)
          :initially (write-line (make-string (+ 4 (array-dimension cells 1))
                                              :initial-element +empty+))
                     (write-line (make-string (+ 4 (array-dimension cells 1))
                                              :initial-element +empty+))
          :do (progn
                (loop :for c :below (array-dimension cells 1)
                      :initially (write-char +empty+) (write-char +empty+)
                      :do (write-char (aref cells r c))
                      :finally   (write-char +empty+) (write-char +empty+))
                (terpri))
          :finally   (write-line (make-string (+ 4 (array-dimension cells 1))
                                              :initial-element +empty+))
                     (write-line (make-string (+ 4 (array-dimension cells 1))
                                              :initial-element +empty+)))
    (terpri)
    (format t "~{~A~%~}" buttons)
    (format t "~{~A~%~}" pathways)))

(defun position-of-nth (n what where)
  (loop
    :for i :below (length where)
    :until (and (eql what (aref where i)) (zerop n))
    :when (eql what (aref where i))
    :do (decf n)
    :finally (return i)))

(defun random-position (cells &optional (amongst +solid+))
  (let* ((flat-cells  (make-array (* (array-dimension cells 0)
                                     (array-dimension cells 1))
                                  :displaced-to cells))
         (count-free (count amongst flat-cells)))
    (when (plusp count-free)
      (let* ((random-free (random count-free))
             (random-flat (position-of-nth random-free amongst flat-cells)))
        (multiple-value-list (truncate random-flat (array-dimension cells 1)))))))


(defun generate-level-1 (&key (width 5) (height width))
  (let ((cells (make-array (list height width) :initial-element +solid+))
        (m     (* width height)))
    (let ((position (random-position cells)))
      (setf (aref cells (first position) (second position)) +start+))
    (let ((position (random-position cells)))
      (setf (aref cells (first position) (second position)) +target+))
    (loop :repeat (truncate (* 1/4 m))
          :do (let ((position (random-position cells)))
                (setf (aref cells (first position) (second position)) +ice+)))
    (loop :repeat (truncate (* 1/8 m))
          :do (let ((position (random-position cells)))
                (setf (aref cells (first position) (second position)) +crumble+)))
    (format-level cells '() '())))

(generate-level-1)
".........
.........
..OOOOO..
..OIOCT..
..ICOCI..
..IOOOI..
..OSOIO..
.........
.........h

"

;;      ^
;;      |
;;    front
;;      |
;;      v
;; <-lateral->


(defun move-stone (x y orientation direction)
  (ecase orientation
    ((:vertical) ;; x y
     (let ((nx (min (+ x (* 2 (first direction)))
                    (+ x      (first direction))))
           (ny (min (+ y (* 2 (second direction)))
                    (+ y      (second direction)))))
       (if (zerop (second direction))
           (list nx ny :lateral)
           (list nx ny :front))))

    ((:front) ;; x y x+1 y
     (cond
       ((zerop (second direction))
        (list (+ x (first direction))
              y
              :front))
       ((plusp (second direction))
        (list x
              (+ y 2)
              :vertical))
       (t
        (list x
              (- y 1)
              :vertical))))

    ((:lateral) ;; x y x+1 y
     (cond
       ((zerop (first direction))
        (list x
              (+ y (second direction))
              :lateral))
       ((plusp (first direction))
        (list (+ x 2)
              y
              :vertical))
       (t
        (list (- x 1)
              y
              :vertical))))))

(assert (equalp (list
                 (move-stone  10 10 :vertical '(1 0))
                 (move-stone  10 10 :vertical '(-1 0))
                 (move-stone  10 10 :vertical '(0 1))
                 (move-stone  10 10 :vertical '(0 -1))

                 (move-stone  10 10 :lateral '(1 0))
                 (move-stone  10 10 :lateral '(-1 0))
                 (move-stone  10 10 :lateral '(0 1))
                 (move-stone  10 10 :lateral '(0 -1))

                 (move-stone  10 10 :front '(1 0))
                 (move-stone  10 10 :front '(-1 0))
                 (move-stone  10 10 :front '(0 1))
                 (move-stone  10 10 :front '(0 -1)))

                '((11 10 :lateral)
                  (8 10 :lateral)
                  (10 11 :front)
                  (10 8 :front)

                  (12 10 :vertical)
                  (9 10 :vertical)
                  (10 11 :lateral)
                  (10 9 :lateral)

                  (11 10 :front)
                  (9 10 :front)
                  (10 12 :vertical)
                  (10 9 :vertical))))

(defun stone-minx (nx ny no)
  (declare (ignore ny no))
  nx)

(defun stone-miny (nx ny no)
  (declare (ignore nx no))
  ny)

(defun stone-maxx (nx ny no)
  (declare (ignore ny))
  (case no
    (:vertical nx)
    (:front    nx)
    (:lateral  (+ 1 nx))))

(defun stone-maxy (nx ny no)
  (declare (ignore nx))
  (case no
    (:vertical ny)
    (:lateral  ny)
    (:front    (+ 1 ny))))


(defun validate-position (nx ny no cells)
  (and (<= 2 (stone-minx nx ny no))
       (<= 2 (stone-miny nx ny no))
       (<= (stone-maxx nx ny no) (- (array-dimension cells 0) 2))
       (<= (stone-maxy nx ny no) (- (array-dimension cells 1) 2))))


(defstruct node
  (x 0)
  (y 0)
  (orientation '(0 0 1))
  (previous nil)
  (cost +infinity+)
  (cells))

(defmethod node-equal (a b)
  (declare (ignorable a b))
  nil)
(defmethod node-equal ((a node) (b node))
  (and (= (node-x a) (node-x b))
       (= (node-y a) (node-y b))
       (eql (node-orientation a) (node-orientation b))))

(defun solidify (x y orientation cells)
  (if (or (equal +empty+ (aref cells
                               (stone-minx x y orientation)
                               (stone-miny x y orientation)))
          (equal +empty+ (aref cells
                               (stone-maxx x y orientation)
                               (stone-maxy x y orientation))))
      (let ((cells (copy-array cells)))
        (when (equal +empty+ (aref cells
                                   (stone-minx x y orientation)
                                   (stone-miny x y orientation)))
          (setf (aref cells
                      (stone-minx x y orientation)
                      (stone-miny x y orientation))
                +solid+))
        (when (equal +empty+ (aref cells
                                   (stone-maxx x y orientation)
                                   (stone-maxy x y orientation)))
          (setf (aref cells
                      (stone-maxx x y orientation)
                      (stone-maxy x y orientation))
                +solid+))
        cells)
      cells))

(defmethod successors ((node node))
  (let ((cells  (node-cells node)))
    (mapcan (lambda (direction)
              (destructuring-bind (new-x new-y new-orientation)
                  (move-stone (node-x node) (node-y node) (node-orientation node) direction)
                (when (validate-position new-x new-y new-orientation cells)
                  (list (make-node :x new-x
                                   :y new-y
                                   :orientation new-orientation
                                   :previous node
                                   :cells (solidify new-x new-y new-orientation cells))))))
            '((1 0) (-1 0) (0 1) (0 -1)))))

#-(and)
(let ((x 2)
      (y 2)
      (orientation :vertical)
      (cells  (make-array '(9 9) :initial-element +empty+)))
  (setf (aref cells x y) +solid+)
  (successors (make-node :x x :y y :orientation orientation :cells cells)))

(defmethod previous ((node node))
  (node-previous node))
(defmethod set-previous (new-previous (node node))
  (setf (node-previous node) new-previous))
(defmethod cost ((node node))
  (node-cost node))
(defmethod set-cost (new-cost (node node))
  (setf (node-cost node) new-cost))

(defun square (x) (* x x))
(defmethod estimate-distance ((a node) (b node))
  (+ (square (- (node-x a) (node-x b)))
     (square (- (node-y a) (node-y b)))))

(defun make-goalp (x y orientation)
  (case orientation
    ((:any)
     (lambda (node)
       (or (and (= x (stone-minx (node-x node) (node-y node) (node-orientation node)))
                (= y (stone-miny (node-x node) (node-y node) (node-orientation node))))
           (and (= x (stone-maxx (node-x node) (node-y node) (node-orientation node)))
                (= y (stone-maxy (node-x node) (node-y node) (node-orientation node)))))))
    (otherwise
     (lambda (node)
       (and (eql orientation (node-orientation node))
            (= (stone-minx x y orientation)
               (stone-minx (node-x node) (node-y node) (node-orientation node)))
            (= (stone-miny x y orientation)
               (stone-miny (node-x node) (node-y node) (node-orientation node)))
            (= (stone-maxx x y orientation)
               (stone-maxx (node-x node) (node-y node) (node-orientation node)))
            (= (stone-maxy x y orientation)
               (stone-maxy (node-x node) (node-y node) (node-orientation node))))))))


 "
DO:                 Implement the A* algorithm.

SUCCESSORS:         a function giving for each node, the list of its
                    successors.

PREVIOUS:           a getter giving the current previous node for the
                    given node.

SET-PREVIOUS:       a setter to set the current previous node for the
                    given node. (lambda (new-previous node))

COST:               a getter giving the current cost of the given
                    node. The default should be +INFINITY+.

SET-COST:           a setter setting the current cost of the given
                    node. (lambda (new-cost node))

ESTIMATE-DISTANCE:  a function of two nodes returning a cost estimate
                    of the distance between them.

GOALP:              a predicate indicating whether the node is the
                    GOAL-NODE (or close enough).

START-NODE:         the start node of the searched path.

GOAL-NODE:          the end node of the searched path.

RETURN:             a path, ie. a list of nodes from START-NODE to
                    GOAL-NODE.
"


(defun build-path (start start-orientation end end-orientation cells)
  (if (equalp start end)
      '(())
      (find-path (function successors)
                 (function previous) (function set-previous)
                 (function cost) (function set-cost)
                 (function estimate-distance)
                 (make-goalp (first end) (second end) end-orientation)
                 (make-node :x (first start)
                            :y (second start)
                            :orientation start-orientation
                            :cells cells)
                 (make-node :x (first end)
                            :y (second end)
                            :orientation end-orientation
                            :cells cells)
                 :node-equal (function node-equal))))

#-(and)
(map nil 'print
     (let ((*standard-output* (make-broadcast-stream)))
       (build-path '(2 2) :vertical '(4 5) :any (make-array '(9 9) :initial-element +empty+))))

#-(and)
(format-level
 (node-cells (first (last (build-path '(2 2) :vertical '(4 5) :any (make-array '(9 9) :initial-element +empty+)))))
 nil nil)

#-(and)
(loop
  :with pos   := '((2 2 :vertical) (7 7 :vertical) (7 2 :vertical) (2 7 :vertical) (5 7 :vertical))
  :with start := (pop pos)
  :with cells := (make-array '(9 9) :initial-element +empty+)
  :for next := (pop pos)
  :for path := (and next
                    (build-path (list (first start) (second start)) (third start)
                                (list (first next)  (second next))  (third next)
                                cells))
  :while next
  :append path :into paths
  :do (setf start next)
      (setf cells (node-cells (first (last path))))
  :finally (return (values paths (format-level cells nil nil))))





(defun generate-path (start end cells start-orientation)
  (let ((end-orientation :vertical)
        (intermediates '()))
    (when (eql +red+ (aref cells (first end) (second end)))
      (setf end-orientation :any))
    (when (member (aref cells (first start) (second start)) (list +red+ +blue+))
      (setf intermediates (loop :repeat (random 3)
                                :collect (let ((position (random-position cells +empty+)))
                                           (when position
                                             (setf (aref cells (first position) (second position))
                                                   +pathway+)
                                             position)))))
    `(path ,@
      (loop
        :for start := start :then end
        :for start-orientation := start-orientation :then :any
        :for step := (append intermediates end)
        :for step-orientation := (if (equal step end)
                                    end-orientation
                                    :any)
        :append (build-path start start-orientation step step-orientation cells)))))

(defun generate-level-2 (&key (width 5) (height width)
                         (goals (1+ (random (ceiling (* 0.2 width height))))))
  (let ((cells (make-array (list (+ 4 height) (+ 4 width)) :initial-element +empty+))
        (m     (* width height)))
    (let ((goal-positions
            (loop :with ngoals := goals
                  :for position := (random-position cells +empty+)
                  :while (plusp ngoals)
                  :do (assert position)
                  :if (eql +empty+ (aref cells (first position) (second position)))
                  :do (setf (aref cells (first position) (second position))
                            (cond
                              ((= goals ngoals)   +start+)
                              ((= 1 ngoals)       +target+)
                              ((zerop (random 2)) +red+)
                              (t                  +blue+)))
                      (decf ngoals)
                  :and
                  :collect (print (list position (aref cells (first position) (second position)))))))
      (print goal-positions)

      (print (loop :for start :in goal-positions
                   :for end :in (rest goal-positions)
                   :collect (generate-path cells start end)))

      )


    ;; (loop :repeat (truncate (* 1/4 m))
    ;;       :do (let ((position (random-position cells)))
    ;;             (setf (aref cells (first position) (second position)) +ice+)))
    ;; (loop :repeat (truncate (* 1/8 m))
    ;;       :do (let ((position (random-position cells)))
    ;;             (setf (aref cells (first position) (second position)) +crumble+)))

    (format-level cells '() '())))

#-(and)
(let (board)
  (concatenate 'string
               (with-output-to-string (*standard-output*)
                  (setf board (generate-level-2 :goals 3)))
               ;; (com.informatimago.common-lisp.cesarum.string:string-justify-left
               ;;
               ;;  nil nil nil :line-prefix ";; ")
               #(#\newline #\newline)
               board))

#-(and)
(loop
  :with pos   := '((2 2 :vertical) (7 7 :vertical) (7 2 :vertical) (2 7 :vertical) (5 7 :vertical))
  :with start := (pop pos)
  :with cells := (make-array '(9 9) :initial-element +empty+)
  :for next := (pop pos)
  :for path := (and next
                    (build-path (list (first start) (second start)) (third start)
                                (list (first next)  (second next))  (third next)
                                cells))
  :while next
  :append path :into paths
  :do (setf start next)
      (setf cells (node-cells (first (last path))))
  :finally (return (values paths (format-level cells nil nil))))
