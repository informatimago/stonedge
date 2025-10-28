(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage :com.informatimago.games.stonedge.solver
  (:use
   :common-lisp
   :com.informatimago.games.stonedge
   :com.informatimago.games.stonedge.parser
   :com.informatimago.common-lisp.cesarum.graph
   :com.informatimago.common-lisp.graphviz.graph-dot)
  (:import-from :com.informatimago.common-lisp.cesarum.array
                #:copy-array)
  (:export #:solve-problem))

(in-package :com.informatimago.games.stonedge.solver)

;;;-----------------------------------------------------------------------------
;;;
;;; Solver
;;;

(defgeneric cell-state (cell)
  (:documentation "Return NIL or the state of the cell.")
  (:method (cell)                (declare (ignorable cell)) nil)
  (:method ((cell crumble-cell)) (declare (ignorable cell)) (crumble-cell-state cell))
  (:method ((cell pathway-cell)) (declare (ignorable cell)) (pathway-cell-state cell)))


(defgeneric game-state (game)
  (:documentation "Return a list containing in a concise form the full state of the game.")
  (:method ((game game))
    (let ((cells (game-cells game))
          (stone (game-stone game)))
      (multiple-value-bind (direction stone-left stone-back stone-right stone-front)
          (stone-coverage stone)
        (declare (ignore direction))
        (list* (or (game-status stone (aref cells stone-left  stone-back))
                   (game-status stone (aref cells stone-right stone-front)))
               stone-left stone-back stone-right stone-front
               (loop
                  :for i :from 0 :below (array-total-size cells)
                  :for state = (cell-state (row-major-aref cells i))
                  :when state :collect state))))))



(defgeneric copy (object &key &allow-other-keys)

  (:documentation "Copy the game objects.  Stateless cells are returned uncopied.")

  (:method ((stone stone) &key &allow-other-keys)
    (make-instance 'stone
        :x (stone-x stone)
        :y (stone-y stone)
        :direction (stone-direction stone)))

  (:method ((cell cell) &key &allow-other-keys)
    cell)

  (:method ((cell button-cell) &key &allow-other-keys)
    (make-instance (class-of cell)
        :x (cell-x cell)
        :y (cell-y cell)
        :switches (button-cell-switches cell)))

  (:method ((cell pathway-cell) &key &allow-other-keys)
    (make-instance (class-of cell)
        :x (cell-x cell)
        :y (cell-y cell)
        :state (pathway-cell-state cell)))

  (:method ((cell crumble-cell) &key &allow-other-keys)
    (make-instance (class-of cell)
        :x (cell-x cell)
        :y (cell-y cell)
        :state (crumble-cell-state cell)))

  (:method ((game game) &key &allow-other-keys)
    (make-instance 'game
        :stone (copy (game-stone game))
        :cells (loop
                  :with cells    = (copy-array (game-cells game))
                  :with pathways = '()
                  :with buttons  = '()
                  :for i :from 0 :below (array-total-size cells)
                  :for original = (row-major-aref cells i)
                  :for copy     = (copy original)
                  :do (setf (row-major-aref cells i) copy)
                  :do (typecase original
                        (pathway-cell (push (cons original copy) pathways))
                        (button-cell  (push copy                 buttons)))
                  :finally (progn
                             (dolist (button buttons)
                               (setf (button-cell-switches button)
                                     (mapcar (lambda (old) (cdr (assoc old pathways)))
                                             (button-cell-switches button))))
                             (return cells))))))



(defstruct node
  "A node of the graph of the stonedge game."
  state
  game
  path
  visitedp
  startp
  ;; neighbors is nil or a vector of neighbor nodes in (:right :left :front :back) order.
  neighbors)


(defvar *states* (make-hash-table :test (function equal))
  "A hash-table mapping game states to nodes.")


(defun make-graph-from-states (states)
  (let* ((ne (make-hash-table))
         (en (make-hash-table))
         (elements
          (let ((elements '()))
            (maphash
             (lambda (state node)
               (let ((element (make-instance 'element-class :ident state)))
                 (set-property element :path (reverse (node-path node)))
                 (set-property element :dot-label
                               #- (and) (with-output-to-string (out)
                                                     (dolist (line (split-sequence:split-sequence
                                                                    #\newline
                                                                    (with-output-to-string (out)
                                                                      (display (node-game node) out))))
                                                       (princ line out)
                                                       (princ "\\n" out)))
                               #+ (and) "x")
                 (set-property element :dot-fill-color (if (node-startp node)
                                                           "Yellow"
                                                           (ecase (first state)
                                                             ((:win)  "Green")
                                                             ((:lose) "Red")
                                                             ((nil)   "White"))))
                 (setf (gethash element en) node)
                 (setf (gethash node ne) element)
                 (push element elements)))
             states)
            elements))
         (graph (make-instance 'graph-class
                    :nodes (make-instance 'set-class :elements elements)
                    :edge-class 'directed-edge)))
    (print (list :number-of-elements (length elements)))
    (dolist (element elements)
      (let ((node (gethash element en)))
        (when (node-neighbors node)
          (loop
             :for successor :across (node-neighbors node)
             :for direction :in '(:right :left :front :back)
             :do (when successor
                   (let ((edge (make-instance 'directed-edge-class
                                   :from element
                                   :to (gethash successor ne))))
                     (set-property edge :dot-label direction)
                     (add-edge graph edge)))))))
    graph))

(defun reset ()
  "Cleans up the *STATES* hash-table."
  (setf  *states* (make-hash-table :test (function equal))))


(defun explore (game path)
  "Walks the game graph, recoding each node in the *STATES* hash-table."
  (let* ((state (game-state game))
         (node  (gethash state *states*)))
    (unless node
      (setf node (setf (gethash state *states*)
                       (make-node :state state
                                  :game game
                                  :path path
                                  :visitedp nil))))
    (unless (node-visitedp node)
      (setf (node-visitedp node) t)
      (unless (first state)
        (setf (node-neighbors node)
              (coerce
               (loop
                  :for move    :in '(:right :left :front :back)
                 ;; :for reverse :in '(:left :right :back :front)
                  :collect (let ((child (copy game)))
                             (move child move)
                             (explore child (cons move path))))
               'vector))))
    node))


(defun print-wins ()
  "Find all the WIN nodes from the *STATES* hash-table, and print thei state and path."
  (maphash (lambda (state node)
             (when (eq :win (first state))
               (print (list state (reverse (node-path node))))))
           *states*))


(defun solve-problem (problem)
  "Solves the playtomo-stonedge game level PROBLEM,
printing the number of states and the win states."
  (time (progn
          (reset)
          (setf (node-startp (explore (make-game-from-level (parse-level problem)) '())) t)
          (print `(:number-of-states ,(hash-table-count *states*)))
          (print-wins))))

;;;; THE END ;;;;
