(defpackage :com.informatimago.games.stonedge.level-manager
  (:use :common-lisp
        :com.informatimago.games.stonedge
        :com.informatimago.games.stonedge.parser
        :com.informatimago.common-lisp.cesarum.file)
  (:export
   #:level-list
   #:levels
   #:selected-level
   #:delete-level
   #:insert-level-before
   #:select-level
   #:unselect-level
   #:selected-level-index
   #:*level-list*
   #:query-filename
   #:manage-levels
   #:query-integer
   #:query-string
   #:query-multi-line-string
   #:edit-level
   #:size-to-fit
   #:extend-level-cells))

(in-package :com.informatimago.games.stonedge.level-manager)

(defclass level-list ()
  ((levels :initarg :levels
           :initform '()
           :reader levels)
   (selected-level :initarg :selected-level
                   :initform nil
                   :reader selected-level)))

(defmethod display ((level-list level-list) stream &key)
  (loop
    :for i :from 1
    :for e :in (levels level-list)
    :initially (terpri stream)
    :do (format stream "~A~3D: ~A~%"
                (if (eql e (selected-level level-list))
                    "*" " ")
                i
                (level-title e))
    :finally (terpri stream)))


(defmethod delete-level ((level-list level-list) (level level))
  (when (eql level (selected-level level-list))
    (unselect-level level-list))
  (setf (slot-value level-list 'levels)
        (delete level (levels level-list))))

(defmethod delete-level ((level-list level-list) (index integer))
  (cond ((< index 1)
         (error "Invalid level index ~A" index))
        ((< (length (levels level-list)) index)
         (error "Invalid level index ~A (max ~A)" index (length (levels level-list))))
        (t
         (when (eql (elt (levels level-list) (1- index)) (selected-level level-list))
           (unselect-level level-list))
         (setf (slot-value level-list 'levels)
               (nconc (subseq (levels level-list) 0 (1- index))
                      (subseq (levels level-list) index))))))




(defmethod insert-level-before ((level-list level-list) (new-level level) (old-level level))
  (assert (not (eql new-level old-level)))
  (assert (member old-level (levels level-list)))
  (let ((selected (eql new-level (selected-level level-list))))
    (delete-level level-list new-level)
    (insert-level-before level-list new-level (1+ (position old-level (levels level-list))))
    (when selected (select-level level-list new-level))
    (levels level-list)))

(defmethod insert-level-before ((level-list level-list) (new-level level) (index integer))
  (let ((selected (eql new-level (selected-level level-list))))
    (setf (slot-value level-list 'levels) (delete new-level (levels level-list)))
    (setf (slot-value level-list 'levels)
          (cond
            ((< index 1)
             (cons new-level (levels level-list)))
            ((<= index (length (levels level-list)))
             (nconc (subseq (levels level-list) 0 (1- index))
                    (list new-level)
                    (subseq (levels level-list) (1- index))))
            (t
             (nconc (levels level-list) (list new-level)))))
    (when selected (select-level level-list new-level))
    (levels level-list)))



(defmethod select-level ((level-list level-list) (level level))
  (assert (member level (levels level-list)))
  (setf (slot-value level-list 'selected-level) level))

(defmethod select-level ((level-list level-list) (index integer))
  (setf (slot-value level-list 'selected-level)
        (cond ((< index 1) nil)
              ((< (length (levels level-list)) index) nil)
              (t (elt (levels level-list) (1- index))))))

(defmethod unselect-level ((level-list level-list))
  (setf (slot-value level-list 'selected-level) nil))

(defmethod selected-level-index ((level-list level-list))
  (when (selected-level level-list)
    (1+ (position (selected-level level-list) (levels level-list)))))



(defvar *level-list*  (make-instance 'level-list))
(defvar *unsaved*     nil)

(defun query-filename (prompt)
  (loop
   (handler-case
       (progn
         (format *query-io* "~%~A" prompt)
         (finish-output *query-io*)
         (let ((filename (string-trim #(#\space #\tab) (read-line *query-io*))))
           (terpri *query-io*)
           (finish-output *query-io*)
           (return-from query-filename (pathname filename))))
     (error (err)
       (format *error-output* "~%~A~%" err)
       (finish-output  *error-output*)))))

(defun query-integer (prompt)
  (loop
   (handler-case
       (progn
         (format *query-io* "~%~A" prompt)
         (finish-output *query-io*)
         (let* ((line (read-line *query-io*))
                (value (read-from-string line)))
           (terpri *query-io*)
           (finish-output *query-io*)
           (etypecase value
             (integer (return-from query-integer value)))))
     (error (err)
       (format *error-output* "~%~A~%" err)
       (finish-output  *error-output*)))))

(defun query-string (prompt)
  (loop
   (handler-case
       (progn
         (format *query-io* "~%~A" prompt)
         (finish-output *query-io*)
         (let* ((string (read-line *query-io*)))
           (terpri *query-io*)
           (finish-output *query-io*)
           (return-from query-string string)))
     (error (err)
       (format *error-output* "~%~A~%" err)
       (finish-output  *error-output*)))))

(defun query-multi-line-string (prompt)
  (loop
    :with lines := '()
    :do (handler-case
            (progn
              (format *query-io* "~%~A" prompt)
              (loop
               (format *query-io* "~&> ")
               (finish-output *query-io*)
               (let* ((line (read-line *query-io*)))
                 (if (zerop (length line))
                     (progn
                       (terpri *query-io*)
                       (finish-output *query-io*)
                       (return-from query-multi-line-string (nreverse lines)))
                     (push line lines)))))
          (error (err)
            (format *error-output* "~%~A~%" err)
            (finish-output  *error-output*)))))

(defgeneric editor-text-icon (cell)
  (:documentation "Returns a three-character strings denoting graphically the cell.")
  (:method ((cell cell)) (text-icon cell))
  (:method ((cell red-button-cell))
    (format nil "~A[R" (cell-name cell)))
  (:method ((cell blue-button-cell))
    (format nil "~A[B" (cell-name cell)))
  (:method ((cell pathway-cell))
    (format nil (if (eql :open (pathway-cell-state cell))
                    "~A--"
                    "~A/\\")
            (cell-name cell))))

(defun editor-display-cells (cells stream selected-x selected-y)
  "
Prints an ASCII-art representation of the cells onto the STREAM.
The cell at selected-x selected-y coordinates is highlighed.
"
  (assert (<= 2 selected-x (- (array-dimension cells 1) 3)))
  (assert (<= 2 selected-y (- (array-dimension cells 0) 3)))
  (flet ((print-line (j)
           (loop
             :initially (princ "+" stream)
             :for i :below (array-dimension cells 1)
             :do (if (or (= (- j 1) selected-y)
                         (=    j    selected-y))
                     (cond
                       ((= (1+ i) selected-x) (princ "---*" stream))
                       ((= i selected-x)      (princ "****" stream))
                       (t                     (princ "---+" stream)))
                     (princ "---+" stream)))))
    (loop
      :for j :from (- (array-dimension cells 0) 1) :downto 0
      :initially (print-line j) (terpri stream)
      :do (loop
            :for i :below (array-dimension cells 1)
            :initially (princ "|" stream)
            :do (princ (editor-text-icon (aref cells j i)) stream)
                (if (= j selected-y)
                    (cond
                      ((= (1+ i) selected-x) (princ "*" stream))
                      ((= i selected-x)      (princ "*" stream))
                      (t                     (princ "|" stream)))
                    (princ "|" stream))
            :finally (progn
                       (terpri stream)
                       (print-line j)
                       (terpri stream))))))

(defun move-cells (src dst dx dy)
  (loop :for y :below (array-dimension src 0)
        :do (loop :for x :below (array-dimension src 1)
                  :for cell := (aref src y x)
                  :do (setf (cell-x cell) (+ x dx)
                            (cell-y cell) (+ y dy))
                      (setf (aref dst (+ y dy) (+ x dx)) cell))))

(defmethod size-to-fit ((level level))
  (let* ((grid (level-cells level))
         (minx (array-dimension grid 1))
         (miny (array-dimension grid 0))
         (maxx 0)
         (maxy 0))
    (loop :for j :below (array-dimension grid 0)
          :do (loop :for i :below (array-dimension grid 1)
                    :do (unless (typep (aref grid j i) 'empty-cell)
                          (setf minx (min i minx)
                                miny (min j miny)
                                maxx (max i maxx)
                                maxy (max j maxy)))))
    (unless (and (= (+ 4 (- maxx minx -1)) (array-dimension grid 1))
                 (= (+ 4 (- maxy miny -1)) (array-dimension grid 0)))
      (let ((ngrid (make-array (list (+ 4 (- maxy miny -1))
                                     (+ 4 (- maxx minx -1))))))
        (loop :with top  := (- miny 2)
              :with left := (- minx 2)
              :for j :from top :to (+ maxy 2)
              :do (loop :for i :from left :to (+ maxx 2)
                        :for cell := (aref grid j i)
                        :do (setf (cell-x cell) i
                                  (cell-y cell) j)
                            (setf (aref ngrid (- j top) (- i left)) cell)))
        (setf (level-cells level) ngrid))))
  level)

(defmethod extend-level-cells ((level level) side)
  (let* ((grid (level-cells level))
         (ngrid (ecase side
                  ((:left :right) (make-array (list (array-dimension grid 0)
                                                    (1+ (array-dimension grid 1)))))
                  ((:top :bottom) (make-array (list (1+ (array-dimension grid 0))
                                                    (array-dimension grid 1)))))))
    (move-cells grid ngrid
                (if (eq side :left) 1 0)
                (if (eq side :top)  1 0))
    (case side
      (:right  (loop :with x := (- (array-dimension ngrid 1) 1)
                     :for y :below (array-dimension ngrid 0)
                     :do (setf (aref ngrid y x) (make-instance 'empty-cell :x x :y y))))
      (:left   (loop :with x := 0
                     :for y :below (array-dimension ngrid 0)
                     :do (setf (aref ngrid y x) (make-instance 'empty-cell :x x :y y))))
      (:bottom (loop :with y := (- (array-dimension ngrid 0) 1)
                     :for x :below (array-dimension ngrid 1)
                     :do (setf (aref ngrid y x) (make-instance 'empty-cell :x x :y y))))
      (:top    (loop :with y := 0
                     :for x :below (array-dimension ngrid 1)
                     :do (setf (aref ngrid y x) (make-instance 'empty-cell :x x :y y)))))
    (setf (level-cells level) ngrid))
  level)

(defun move-left (level x y)
  (if (< x 3)
      (progn
        (extend-level-cells level :left)
        (values x y))
      (values (- x 1) y)))

(defun move-top (level x y)
  (if (< y 3)
      (progn
        (extend-level-cells level :top)
        (values x y))
      (values x (- y 1))))

(defun move-right (level x y)
  (if (< x (- (array-dimension (level-cells level) 1) 3))
      (values (+ x 1) y)
      (progn
        (extend-level-cells level :right)
        (values (+ x 1) y))))

(defun move-bottom (level x y)
  (if (< y (- (array-dimension (level-cells level) 0) 3))
      (values x (+ y 1))
      (progn
        (extend-level-cells level :bottom)
        (values x (+ y 1)))))


(defun mirror-vertically (level selected-x selected-y)
  (let* ((cells  (level-cells level))
         (height (array-dimension cells 0))
         (width  (array-dimension cells 1))
         (new    (make-array (list height width))))
    (loop
      :for y :below height
      :do (loop :for x :below width
                :do (setf (aref new y x) (aref cells (- height y 1) x)
                          (cell-y (aref new y x)) y)))
    (setf (level-cells level) new)
    (values selected-x (- height selected-y 1))))

(defun mirror-horizontally (level selected-x selected-y)
  (let* ((cells  (level-cells level))
         (height (array-dimension cells 0))
         (width  (array-dimension cells 1))
         (new    (make-array (list height width))))
    (loop
      :for y :below height
      :do (loop :for x :below width
                :do (setf (aref new y x) (aref cells y (- width x 1))
                          (cell-x (aref new y x)) x)))
    (setf (level-cells level) new)
    (values (- width selected-x 1) selected-y)))

(defun rotate-clockwise (level selected-x selected-y)
  ;; (((0 0)  (0 1) (0 2))
  ;;  ((1 0)  (1 1) (1 2)))
  ;; -->
  ;; (((0 2)  (1 2))
  ;;  ((0 1)  (1 1))
  ;;  ((0 0)  (1 0)))
  (let* ((cells  (level-cells level))
         (height (array-dimension cells 0))
         (width  (array-dimension cells 1))
         (new    (make-array (list width height))))
    (loop
      :for y :below height
      :do (loop :for x :below width
                :for new-y := (- width x 1)
                :for new-x := y
                :do (setf (aref new new-y new-x) (aref cells y x)
                          (cell-x (aref new new-y new-x)) new-x
                          (cell-y (aref new new-y new-x)) new-y)))
    (setf (level-cells level) new)
    (values selected-y (- width selected-x 1))))

(defun rotate-counter-clockwise (level selected-x selected-y)
  ;; (((0 0)  (0 1) (0 2))
  ;;  ((1 0)  (1 1) (1 2)))
  ;; -->
  ;; (((1 0)  (0 0))
  ;;  ((1 1)  (0 1))
  ;;  ((1 2)  (0 2)))
  (let* ((cells  (level-cells level))
         (height (array-dimension cells 0))
         (width  (array-dimension cells 1))
         (new    (make-array (list width height))))
    (loop
      :for y :below height
      :do (loop :for x :below width
                :for new-y := x
                :for new-x := (- height y 1)
                :do (setf (aref new new-y new-x) (aref cells y x)
                          (cell-x (aref new new-y new-x)) new-x
                          (cell-y (aref new new-y new-x)) new-y)))
    (setf (level-cells level) new)
    (values (- height selected-y 1) selected-x)))

(defvar *editor-commands*
  '(("." "Change to an empty cell")
    ("o" "Change to a solid cell")
    ("z" "Set the start cell")
    ("t" "Change to a target cell")
    ("i" "Change to an ice cell")
    ("c" "Change to a crumble cell")
    ("b" "Change to a blue button cell")
    ("r" "Change to a red button cell")
    ("p" "Change to a pathway cell")
    ("k" "Connect the button to pathway cells")
    ("Y" "Swap open/close on pathway cell")
    ("-" "Mirror the board vertically")
    ("|" "Mirror the board horizontally")
    (">" "Rotate the board clockwise")
    ("<" "Rotate the board counter-clockwise")
    ("a" "Move the cursor left")
    ("s" "Move the cursor down")
    ("d" "Move the cursor right")
    ("w" "Move the cursor up")
    ("f" "Size to fit the level.")
    ("n" "Display the named cells (buttons and pathways)")
    ("h" "Display this help")
    ("x" "Exit from the level editor")))

;; (defparameter *button-cell-names* "ABDEFGHJKLMNPQRUVW")
;; (defparameter *pathway-cell-names* "0123456789!@#$%^&*()_+-={}[]")

(defmethod find-available-name ((level level) valid-names)
  (loop :with named-cells := (level-named-cells level)
        :for name :across valid-names
        :for cell := (gethash (string name) named-cells)
        :while cell
        :finally (return (if cell nil (string-upcase name)))))

(defun validate-cell-name (cell expected-cell-kind valid-names named-cells)
  (unless (= 1 (length (cell-name cell)))
    (error "~S has an invalid-length name ~S (expected 1 character)"
           cell (cell-name cell)))
  (when (find (character (cell-name cell)) *reserved-names* :test (function equalp))
    (error "~S has a reserved named ~S" cell (cell-name cell)))
  (let ((other-cell (gethash (cell-name cell) named-cells)))
    (unless (or (null other-cell) (eql other-cell cell))
        (error "~S has a duplicate name ~S with ~S"
               cell (cell-name cell) (gethash (cell-name cell) named-cells))))
  (unless (find (character (cell-name cell)) valid-names :test (function equalp))
    (error "~S has a non-~A name ~S"
           cell expected-cell-kind (cell-name cell))))

(defgeneric update-definitions (level)
  (:documentation "Update the definitions and named-cells slots of the level from the cells in the grid.")
  (:method ((level level))
    (let ((cells       (level-cells level))
          (named-cells (make-hash-table :test 'equal))
          (definitions (make-hash-table :test 'equal)))
      (flet ((connected-cell-names (cell)
               (map 'list
                    (lambda (name) (string name))
                    (sort (with-output-to-string (out)
                            (dolist (pathway (button-cell-switches cell))
                              (unless (typep pathway 'pathway-cell)
                                (error "Button cell ~S is connected to a non-pathway cell ~S"
                                       cell pathway))
                              (princ (cell-name pathway) out)))
                          (function char<)))))
        (loop
          :for cell :across (make-array (array-total-size cells)
                                        :displaced-to cells)
          :do (typecase cell
                (blue-button-cell
                 (validate-cell-name cell "blue button cell" *button-cell-names* named-cells)
                 (setf (gethash (string-upcase (cell-name cell)) named-cells) cell
                       (gethash (string-upcase (cell-name cell)) definitions)
                       (make-instance 'definition
                                      :name (string-upcase (cell-name cell))
                                      :link :blue
                                      :connected (connected-cell-names cell))))
                (red-button-cell
                 (validate-cell-name cell "red button cell" *button-cell-names* named-cells)
                 (setf (gethash (string-upcase (cell-name cell)) named-cells) cell
                       (gethash (string-upcase (cell-name cell)) definitions)
                       (make-instance 'definition
                                      :name (string-upcase (cell-name cell))
                                      :link :red
                                      :connected (connected-cell-names cell))))
                (pathway-cell
                 (validate-cell-name cell "pathway cell" *pathway-cell-names* named-cells)
                 (setf (gethash (string-upcase (cell-name cell)) named-cells) cell
                       (gethash (string-upcase (cell-name cell)) definitions)
                       (make-instance 'definition
                                      :name (string-upcase (cell-name cell))
                                      :link :pathway
                                      :state (pathway-cell-state cell)))))))
      (setf (slot-value level 'com.informatimago.games.stonedge::named-cells) named-cells
            (slot-value level 'com.informatimago.games.stonedge::definitions) definitions)
      level)))

;; (update-definitions (first (levels *level-list*)))
;; (find-available-name (first (levels *level-list*)) *button-cell-names*)

(defun pathway-cell-names (level)
  (sort (string-upcase (with-output-to-string (out)
                         (dolist (pathway (level-pathway-cells level))
                           (princ (cell-name pathway) out))))
        (function char<)))

(defun display-available-pathway-cells (level)
  (format t "~&Available pathway cells: ~A~%" (pathway-cell-names level)))

(defmethod unregister-cell ((level level) (cell cell))
  cell)

(defmethod unregister-cell ((level level) (cell named-cell))
  (remhash (string-upcase (cell-name cell)) (level-named-cells level))
  (update-definitions level)
  cell)

(defmethod register-cell ((level level) (cell cell))
  cell)

(defmethod register-cell ((level level) (cell named-cell))
  (setf (gethash (string-upcase (cell-name cell)) (level-named-cells level)) cell)
  (update-definitions level)
  cell)

(defun interpret-command (cmd level selected-x selected-y)
  (let ((cells (level-cells level)))
    (let ((entry (find (string cmd) *editor-commands* :key (function first) :test (function string-equal))))
      (when entry
        (format t "~&~A~%" (second entry))))
    (case cmd
      ((#\.)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'empty-cell
                            :x selected-x
                            :y selected-y)))
      ((#\o #\O)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'solid-cell
                            :x selected-x
                            :y selected-y)))
      ((#\z #\Z)
       (multiple-value-bind (old-cell old-y old-x)
           (level-start-cell level)
         (when old-cell
           (setf (aref cells old-y old-x)
                 (make-instance 'solid-cell
                                :x selected-x
                                :y selected-y)))
         (setf (aref cells selected-y selected-x)
               (make-instance 'start-cell
                              :x selected-x
                              :y selected-y))))
      ((#\t #\T)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'target-cell
                            :x selected-x
                            :y selected-y)))
      ((#\i #\I)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'ice-cell
                            :x selected-x
                            :y selected-y)))
      ((#\c #\C)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'crumble-cell
                            :x selected-x
                            :y selected-y)))
      ((#\b #\B)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'blue-button-cell
                            :x selected-x
                            :y selected-y
                            :name (find-available-name level *button-cell-names*)))
       (register-cell level (aref cells selected-y selected-x)))
      ((#\r #\R)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'red-button-cell
                            :x selected-x
                            :y selected-y
                            :name (find-available-name level *button-cell-names*)))
       (register-cell level (aref cells selected-y selected-x)))
      ((#\p #\P)
       (unregister-cell level (aref cells selected-y selected-x))
       (setf (aref cells selected-y selected-x)
             (make-instance 'pathway-cell
                            :x selected-x
                            :y selected-y
                            :name (find-available-name level *pathway-cell-names*)))
       (register-cell level (aref cells selected-y selected-x)))

      ((#\k #\K)
       ;; connect button cell to pathway-cell(s)
       (let ((button (aref cells selected-y selected-x)))
         (when (typep button 'button-cell)
           (display-available-pathway-cells level)
           (format *query-io* "Enter list of connected pathways: ")
           (finish-output *query-io*)
           (let* ((new-connected (coerce (string-upcase
                                          (remove-if (lambda (ch) (find ch #(#\space #\tab)))
                                                     (read-line *query-io*)))
                                         'list))
                  (invalid (set-difference
                            new-connected
                            (coerce (pathway-cell-names level) 'list))))
             (when invalid
               (error "You specified invalid pathway cell names: ~A" invalid))
             (setf (button-cell-switches button)
                   (mapcar (lambda (name)
                             (print (list '(setf button-cell-switches) (string-upcase name)
                                          (gethash (string-upcase name) (level-named-cells level))))
                             (gethash (string-upcase name) (level-named-cells level)))
                           new-connected))
             (update-definitions level)))))

      ((#\y #\Y)
       ;; swap open/close on pathway-cell
       (let ((cell (aref cells selected-y selected-x)))
         (when (typep cell 'pathway-cell)
           (switch-cell cell))))

      ((#\-)   (multiple-value-setq (selected-x selected-y)
                 (mirror-vertically level selected-x selected-y)))
      ((#\|)   (multiple-value-setq (selected-x selected-y)
                 (mirror-horizontally level selected-x selected-y)))
      ((#\<)   (multiple-value-setq (selected-x selected-y)
                 (rotate-counter-clockwise level selected-x selected-y)))
      ((#\>)   (multiple-value-setq (selected-x selected-y)
                 (rotate-clockwise level selected-x selected-y)))

      ;; TODO: editor-display-cell and s/d directions seem to be opposite.
      ;; TODO: check also the consistency of the axis orientation between editor-display-cell and the display-level for the player, and in swift code. This is a mess.
      ((#\a #\A) (multiple-value-setq (selected-x selected-y)
                   (move-left level selected-x selected-y)))
      ((#\s #\S) (multiple-value-setq (selected-x selected-y)
                   (move-top level selected-x selected-y)))
      ((#\d #\D) (multiple-value-setq (selected-x selected-y)
                   (move-right level selected-x selected-y)))
      ((#\w #\W) (multiple-value-setq (selected-x selected-y)
                   (move-bottom level selected-x selected-y)))

      ((#\f #\F)
       (level-cells (size-to-fit level))
       (setf selected-x (max 2 (min selected-x (- (array-dimension cells 1) 3)))
             selected-y (max 2 (min selected-y (- (array-dimension cells 0) 3)))))
      ((#\n #\N) ;; display named cells
       (maphash (lambda (name cell)
                  (format t "~&~A: ~A~%" name cell))
                (level-named-cells level)))
      ((#\h #\H)
       (format t "~&Help:~%~:{  ~A  ~A~%~}~%" *editor-commands*))
      ((#\x #\X) (throw 'petites-gazongues level)))
    (values selected-x selected-y)))

(defmethod edit-level ((level level))
  (catch 'petites-gazongues
    (let ((selected-x 2)
          (selected-y 2))
      (loop
       (with-simple-restart (continue-edit "Continue editing")
         (editor-display-cells (level-cells level) t selected-x selected-y)
         (let ((cell (aref (level-cells level) selected-y selected-x)))
           (typecase cell
             (button-cell
              (format t "~&Button connected to pathway cells: ~{~A~}~%"
                      (definition-connected (gethash (cell-name cell) (level-definitions level))))
              (display-available-pathway-cells level))))
         (format *query-io* "~& .ozticbrp ky -|<> asdw fnh x : ")
         (finish-output *query-io*)
         (let ((input (read-line *query-io*)))
           (loop :for cmd :across input
                 :do (multiple-value-setq (selected-x selected-y)
                       (interpret-command cmd level selected-x selected-y))))))
      level)))

(defun cmd-load ()
  (unless (and *unsaved*
               (not (y-or-n-p "~&Changes are unsaved, confirm overriding them? ")))
    (let* ((filename     (query-filename "Level file: "))
           (descriptions (let ((*default-pathname-defaults*
                                 (merge-pathnames #P".sexp" *default-pathname-defaults*)))
                           (sexp-file-contents filename))))
      (setf (slot-value *level-list* 'levels)
            (mapcar (function parse-level) descriptions))
      (setf *unsaved* nil))))

(defun cmd-append ()
  (unless (and *unsaved*
               (not (y-or-n-p "~&Changes are unsaved, confirm overriding them? ")))
    (let* ((filename     (query-filename "Level file: "))
           (descriptions (let ((*default-pathname-defaults*
                                 (merge-pathnames #P".sexp" *default-pathname-defaults*)))
                           (sexp-file-contents filename))))
      (setf (slot-value *level-list* 'levels)
            (nconc (slot-value *level-list* 'levels)
                   (mapcar (function parse-level) descriptions)))
      (setf *unsaved* t))))

(defun cmd-save ()
  (let* ((filename     (query-filename "Level file: ")))
    (let ((*default-pathname-defaults*
            (merge-pathnames #P".sexp" *default-pathname-defaults*)))
      (setf (sexp-file-contents
             filename
             :if-exists (if (and (probe-file filename)
                                 (y-or-n-p "File exist, should we override it? "))
                            :supersede
                            :error)
             :if-does-not-exist :create)
            (mapcar (function unparse-level) (levels *level-list*))))
    (setf *unsaved* nil)))

(defun cmd-select ()
  (when (levels *level-list*)
    (let ((index (query-integer "Level index: ")))
      (select-level *level-list* index))))

(defun cmd-select-next ()
  (when (levels *level-list*)
    (select-level *level-list*
                  (if (selected-level *level-list*)
                      (1+ (mod (1+ (1- (selected-level-index *level-list*)))
                               (length (levels *level-list*))))
                      1))))

(defun cmd-select-previous ()
  (when (levels *level-list*)
    (select-level *level-list*
                  (if (selected-level *level-list*)
                      (1+ (mod (1- (1- (selected-level-index *level-list*)))
                               (length (levels *level-list*))))
                      (length (levels *level-list*))))))

(defun cmd-delete ()
  (when (levels *level-list*)
    (if (selected-level *level-list*)
        (delete-level *level-list* (selected-level *level-list*))
        (let ((index (query-integer "Level index: ")))
          (delete-level *level-list* index)))
    (setf *unsaved* t)))

(defun cmd-move ()
  (when (levels *level-list*)
    (unless (selected-level *level-list*)
      (let ((index (query-integer "Level index: ")))
        (select-level *level-list* index)))
    (let ((index (query-integer "Destination index: ")))
      (insert-level-before *level-list* (selected-level *level-list*) index))
    (setf *unsaved* t)))

(defun make-cells (width &optional (height width))
  (let* ((height (+ height 4))
         (width  (+ width  4))
         (cells  (make-array (list height width))))
    (loop :for y :below height
          :do (loop :for x :below width
                    :do (setf (aref cells y x)
                              (make-instance 'empty-cell :x x :y y))))
    (setf (aref cells 2 2)
          (make-instance 'start-cell :x 2 :y 2)
          (aref cells (- height 3) (- width 3))
          (make-instance 'target-cell :x (- width 3) :y (- height 3)))
    cells))

(defun cmd-new ()
  (let* ((title       (query-string "Title: "))
         (description (query-multi-line-string "Description: "))
         (size        (let ((size (query-integer "Size (min 3): ")))
                        (if (<= 3 size 30)
                            size
                            (error "Invalid grid size ~A" size))))
         (new         (make-instance
                       'level
                       :title title
                       :description description
                       :cells (make-cells size))))
    (insert-level-before *level-list* new (or (selected-level *level-list*)
                                              (length (levels *level-list*))))
    (select-level *level-list* new)
    (setf *unsaved* t)))

(defgeneric copy-cell (cell)
  (:method ((cell empty-cell))       (make-instance 'empty-cell       :x (cell-x cell) :y (cell-y cell)))
  (:method ((cell solid-cell))       (make-instance 'solid-cell       :x (cell-x cell) :y (cell-y cell)))
  (:method ((cell start-cell))       (make-instance 'start-cell       :x (cell-x cell) :y (cell-y cell)))
  (:method ((cell target-cell))      (make-instance 'target-cell      :x (cell-x cell) :y (cell-y cell)))
  (:method ((cell ice-cell))         (make-instance 'ice-cell         :x (cell-x cell) :y (cell-y cell)))
  (:method ((cell crumble-cell))     (make-instance 'crumble-cell     :x (cell-x cell) :y (cell-y cell) :state (crumble-cell-state cell)))
  (:method ((cell pathway-cell))     (make-instance 'pathway-cell     :x (cell-x cell) :y (cell-y cell) :name (cell-name cell) :state (pathway-cell-state cell)))
  (:method ((cell red-button-cell))  (make-instance 'red-button-cell  :x (cell-x cell) :y (cell-y cell) :name (cell-name cell)))
  (:method ((cell blue-button-cell)) (make-instance 'blue-button-cell :x (cell-x cell) :y (cell-y cell) :name (cell-name cell))))

(defun copy-cells (original)
  (let* ((height (array-dimension original 0))
         (width  (array-dimension original 1))
         (cells  (make-array (list height width))))
    (loop :for y :below height
          :do (loop :for x :below width
                    :do (setf (aref cells y x)
                              (copy-cell (aref original y x)))))
    ;; Update the buttons switches links:
    (loop :for y :below height
          :do (loop :for x :below width
                    :with old := (aref original y x)
                    :with new := (aref cells y x)
                    :do (typecase new
                          (button-cell
                           (setf (button-cell-switches new)
                                 (mapcar (lambda (old-pathway)
                                           (aref cells (cell-y old-pathway) (cell-x old-pathway)))
                                         (button-cell-switches old)))))))
    cells))

(defun cmd-duplicate ()
  (when (levels *level-list*)
    (unless (selected-level *level-list*)
      (let ((index (query-integer "Level index: ")))
        (select-level *level-list* index)))
    (let ((original (selected-level *level-list*)))
      (format *query-io* "~&Old Title: ~A~%" (level-title original))
      (let* ((title       (query-string "New Title: "))
             (new         (make-instance
                           'level
                           :title title
                           :description (level-description original)
                           :cells (copy-cells (level-cells original)))))
        (update-definitions new)
        (insert-level-before *level-list* new
                              (1+ (selected-level-index *level-list*)))
        (select-level *level-list* new)
        (setf *unsaved* t)))))


(defun cmd-edit ()
  (when (levels *level-list*)
    (unless (selected-level *level-list*)
      (let ((index (query-integer "Level index: ")))
        (select-level *level-list* index)))
    (edit-level (selected-level *level-list*))
    (setf *unsaved* t)))

(defun play-level (level)
  (com.informatimago.games.stonedge.player:stonedge level))

(defun solve-level (level)
  (com.informatimago.games.stonedge.solver:solve-level level))

(defun cmd-play ()
  (when (levels *level-list*)
    (unless (selected-level *level-list*)
      (let ((index (query-integer "Level index: ")))
        (select-level *level-list* index)))
    (play-level (selected-level *level-list*))))

(defun cmd-solve ()
  (when (levels *level-list*)
    (unless (selected-level *level-list*)
      (let ((index (query-integer "Level index: ")))
        (select-level *level-list* index)))
    (solve-level (selected-level *level-list*))))


(defvar *manage-commands*
  '((load       "Parse and Load a level file, replacing the current level list.")
    (append     "Parse and Load a level file, appending it to the current level list.")
    (save       "Unparse and save the current level list into a level file.")
    (select     "Select a level (by index).")
    (next       "Select the next level.")
    (prev       "select the previous level.")
    (new        "Create a new level.")
    (duplicate  "Duplicate a level.")
    (delete     "Delete a level.")
    (move       "Move the selected level before the destination index.")
    (edit       "Edit the current level.")
    (play       "Play the current level.")
    (solve      "Solve the current level")
    (help       "Display this help.")
    (debug      "Toggle debugging.")
    (quit       "Quit the level manager.")))

(defun cmd-help ()
  (format t "~&Level Manager Help:~%~:{  ~10A  ~A~%~}~%"
          *manage-commands*))

(defvar *debug* nil
  "When an error occurs in the level manager, if true then invoke the debugger else display an error message and continue.")
(setf *debug* t)


(defun manage-levels ()
  (let ((*level-list*  (make-instance 'level-list))
        (*unsaved*     t))
    (with-simple-restart (abort-manage "Abort managing levels")
      (loop
       (with-simple-restart (continue-manage "Continue managing levels")
         (format t "~%Levels:")
         (display *level-list* t)
         (format t "~{~A~^ ~}?~%"
                 (mapcar (function first) *manage-commands*))
         (handler-bind
             ((error (lambda (condi)
                       (if *debug*
                           (invoke-debugger condi)
                           (progn
                             (format t "~&~A~%" condi)
                             nil)))))
           (let ((command (let ((*package* (find-package #.(package-name *package*))))
                            (read))))
             (case command
               ((load)          (cmd-load))
               ((append)        (cmd-append))
               ((save)          (cmd-save))
               ((select)        (cmd-select))
               ((next)          (cmd-select-next))
               ((prev previous) (cmd-select-previous))
               ((new)           (cmd-new))
               ((duplicate)     (cmd-duplicate))
               ((delete)        (cmd-delete))
               ((move)          (cmd-move))
               ((edit)          (cmd-edit))
               ((play)          (cmd-play))
               ((solve)         (cmd-solve))
               ((help)          (cmd-help))
               ((debug)
                (setf *debug* (not *debug*))
                (format t "~&Debugging ~:[disabled~;activated~]~%" *debug*))
               ((quit)
                (if *unsaved*
                    (when (y-or-n-p "~&Changes are unsaved, confirm quit without saving? ")
                      (return-from manage-levels))
                    (return-from manage-levels)))))))))))




;; (defparameter *level-list*
;;   (let ((level-list (make-instance 'level-list
;;                                    :levels (mapcar (function parse-level)
;;                                                    com.informatimago.games.stonedge.parser::*levels*))))
;;     (select-level level-list 1)
;;     level-list))
;; (display *level-list* t)


;; 1  : Tutorial 1
;; 2  : Tutorial 1.1 - Moves
;; 3  : Tutorial 2
;; 4  : Tutorial 3
;; 5  : Tutorial 4
;; 6  : Level 34 of the original Playtomo Stonedge Game
;; 7  : Level 37 of the original Playtomo Stonedge Game
;; 8  : Level 38 of the original Playtomo Stonedge Game
;; 9  : Level 39 of the original Playtomo Stonedge Game
;; 10 : Level 52 of the original Playtomo Stonedge Game
;;
;; nil
