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



(defvar *level-list*)

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

(defmethod edit-level ((level level))
  level)

(defun manage-levels ()
  (let ((*level-list*  (make-instance 'level-list))
        (unsaved       t))
    (loop
      :do (format t "~%Levels:")
          (display *level-list* t)
          (format t "~{~A~^ ~}?~%"
                  '(load save select delete move new edit quit))
          (handler-case
              (let ((command (read)))
                (case command
                  ((load)
                   (unless (and unsaved
                                (not (y-or-n-p "~&Changes are unsaved, confirm overriding them? ")))
                     (let* ((filename     (query-filename "Level file: "))
                            (descriptions (sexp-file-contents filename)))
                       (setf (slot-value *level-list* 'levels)
                             (mapcar (function parse-level) descriptions))
                       (setf unsaved nil))))
                  ((save)
                   (let* ((filename     (query-filename "Level file: ")))
                     (setf (sexp-file-contents
                            filename
                            :if-exists (if (and (probe-file filename)
                                                (y-or-n-p "File exist, should we override it? "))
                                           :supersede
                                           :error)
                            :if-does-not-exist :create)
                           (mapcar (function unparse-level) (levels *level-list*))))
                   (setf unsaved nil))
                  ((select)
                   (let ((index (query-integer "Level index: ")))
                     (select-level *level-list* index)))
                  ((delete)
                   (when (levels *level-list*)
                     (if (selected-level *level-list*)
                         (delete-level *level-list* (selected-level *level-list*))
                         (let ((index (query-integer "Level index: ")))
                           (delete-level *level-list* index)))
                     (setf unsaved t)))
                  ((move)
                   (when (levels *level-list*)
                     (unless (selected-level *level-list*)
                       (let ((index (query-integer "Level index: ")))
                         (select-level *level-list* index)))
                     (let ((index (query-integer "Destination index: ")))
                       (insert-level-before *level-list* (selected-level *level-list*) index))
                     (setf unsaved t)))
                  ((new)
                   (let* ((title (query-string "Title: "))
                          (description (query-multi-line-string "Description: "))
                          (level (make-instance 'level :title title :description description)))
                     (insert-level-before *level-list* new (or (selected-level *level-list*)
                                                             (length (levels *level-list*))))
                     (select-level *level-list* new)
                     (setf unsaved t)))
                  ((edit)
                   (when (levels *level-list*)
                     (unless (selected-level *level-list*)
                       (let ((index (query-integer "Level index: ")))
                         (select-level *level-list* index)))
                     (edit-level (selected-level *level-list*))
                     (setf unsaved t)))
                  ((quit)
                   (if unsaved
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
