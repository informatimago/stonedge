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
   #:edit-level))

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


(defun display-cells (cells stream selected-x selected-y)
  "
Prints an ASCII-art representation of the cells onto the STREAM.
The cell at selected-x selected-y coordinates is highlighed.
"
  (assert (<= 2 selected-x (- (array-dimension cells 1) 2)))
  (assert (<= 2 selected-y (- (array-dimension cells 0) 2)))
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
            :do (princ (text-icon (aref cells j i)) stream)
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


(defmethod edit-level ((level level))
  (let ((selected-x 2)
        (selected-y 2)
        (cells (level-cells level)))
    (loop
     (display-cells (level-cells level) t selected-x selected-y)
     (format *query-io* "~& .ozticbrp ky asdw x > ")
     (finish-output *query-io*)
     (let ((input (read-line *query-io*)))
       (loop :for cmd :across input
             :do (case cmd
                   ((#\.)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'empty-cell
                                         :x selected-x
                                         :y selected-y)))
                   ((#\o #\O)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'solid-cell
                                         :x selected-x
                                         :y selected-y)))
                   ((#\z #\Z)
                    (multiple-value-bind (old-cell old-y old-x)
                        (level-start-cell level)
                      (declare (ignore old-cell))
                      (setf (aref cells old-y old-x)
                            (make-instance 'solid-cell
                                           :x selected-x
                                           :y selected-y)
                            (aref cells selected-y selected-x)
                            (make-instance 'start-cell
                                           :x selected-x
                                           :y selected-y))))
                   ((#\t #\T)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'target-cell
                                         :x selected-x
                                         :y selected-y)))
                   ((#\i #\I)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'ice-cell
                                         :x selected-x
                                         :y selected-y)))
                   ((#\c #\C)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'crumble-cell
                                         :x selected-x
                                         :y selected-y)))
                   ((#\b #\B)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'blue-button-cell
                                         :x selected-x
                                         :y selected-y)))
                   ((#\r #\R)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'red-button-cell
                                         :x selected-x
                                         :y selected-y)))
                   ((#\p #\P)
                    (setf (aref cells selected-y selected-x)
                          (make-instance 'pathway-cell
                                         :x selected-x
                                         :y selected-y)))
                   
                   ((#\y #\Y)
                    ;; swap open/close on pathway-cell
                    )
                   ((#\k #\K)
                    ;; connect button cell to pathway-cell(s)
                    )

                   ((#\a #\A) (setf selected-x (max 2 (- selected-x 1))))
                   ((#\s #\S) (setf selected-y (max 2 (- selected-y 1))))
                   ((#\d #\D) (setf selected-x (min (- (array-dimension cells 1) 2)
                                                    (+ selected-x 1))))
                   ((#\w #\W) (setf selected-y (min (- (array-dimension cells 0) 2)
                                                    (+ selected-y 1))))
                   ((#\x #\X) (return-from edit-level level))))))
    level))

(defun cmd-load ()
  (unless (and *unsaved*
               (not (y-or-n-p "~&Changes are unsaved, confirm overriding them? ")))
    (let* ((filename     (query-filename "Level file: "))
           (descriptions (sexp-file-contents filename)))
      (setf (slot-value *level-list* 'levels)
            (mapcar (function parse-level) descriptions))
      (setf *unsaved* nil))))

(defun cmd-append ()
  (unless (and *unsaved*
               (not (y-or-n-p "~&Changes are unsaved, confirm overriding them? ")))
    (let* ((filename     (query-filename "Level file: "))
           (descriptions (sexp-file-contents filename)))
      (setf (slot-value *level-list* 'levels)
            (nconc (slot-value *level-list* 'levels)
                   (mapcar (function parse-level) descriptions)))
      (setf *unsaved* t))))

(defun cmd-save ()
  (let* ((filename     (query-filename "Level file: ")))
    (setf (sexp-file-contents
           filename
           :if-exists (if (and (probe-file filename)
                               (y-or-n-p "File exist, should we override it? "))
                          :supersede
                          :error)
           :if-does-not-exist :create)
          (mapcar (function unparse-level) (levels *level-list*)))
    (setf *unsaved* nil)))

(defun cmd-select ()
  (let ((index (query-integer "Level index: ")))
    (select-level *level-list* index)))

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
                       :cells (make-cells size)
                       :definitions )))
    (insert-level-before *level-list* new (or (selected-level *level-list*)
                                              (length (levels *level-list*))))
    (select-level *level-list* new)
    (setf *unsaved* t)))

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

(defun manage-levels ()
  (let ((*level-list*  (make-instance 'level-list))
        (*unsaved*     t))
    (loop
      :do (format t "~%Levels:")
          (display *level-list* t)
          (format t "~{~A~^ ~}?~%"
                  '(load append save select delete move new edit play solve quit))
          (handler-case
              (let ((command (let ((*package* (find-package #.(package-name *package*))))
                                (read))))
                (case command
                  ((load)   (cmd-load))
                  ((append) (cmd-append))
                  ((save)   (cmd-save))
                  ((select) (cmd-select))
                  ((delete) (cmd-delete))
                  ((move)   (cmd-move))
                  ((new)    (cmd-new))
                  ((edit)   (cmd-edit))
                  ((play)   (cmd-play))
                  ((solve)  (cmd-solve))
                  ((quit)
                   (if *unsaved*
                       (when (y-or-n-p "~&Changes are unsaved, confirm quit without saving? ")
                         (return-from manage-levels))
                       (return-from manage-levels)))))
            (error (err)
              (format t "~&~A~%" err))))))




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
