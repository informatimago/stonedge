(defpackage :com.informatimago.games.stonedge.level-manager.tests
  (:use :common-lisp
        :com.informatimago.games.stonedge.level-manager
        :com.informatimago.games.stonedge
        :com.informatimago.games.stonedge.parser
        :com.informatimago.common-lisp.cesarum.file)
  (:export
   "TEST/ALL"
   "TEST/DELETE-LEVEL/LEVEL"
   "TEST/DELETE-LEVEL/INDEX"
   "TEST/INSERT-LEVEL-BEFORE/LEVEL"
   "TEST/INSERT-LEVEL-BEFORE/INDEX"))

(in-package :com.informatimago.games.stonedge.level-manager.tests)

(defun test/delete-level/level ()
  (let* ((one (make-instance 'level :title "One"))
         (two (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (four  (make-instance 'level :title "Four"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (delete-level level-list four)
    (assert (equal (levels level-list) (list one two three))))
  (let* ((one (make-instance 'level :title "One"))
         (two (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (delete-level level-list one)
    (assert (equal (levels level-list) (list two three))))
  (let* ((one (make-instance 'level :title "One"))
         (two (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (delete-level level-list two)
    (assert (equal (levels level-list) (list one three))))
  (let* ((one (make-instance 'level :title "One"))
         (two (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (delete-level level-list three)
    (assert (equal (levels level-list) (list one two))))
    (let* ((one (make-instance 'level :title "One"))
         (two (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (delete-level level-list one)
    (delete-level level-list two)
    (delete-level level-list three)
    (assert (equal (levels level-list) '())))
  :success)

(defun test/delete-level/index ()
  (let ((level-list (make-instance 'level-list
                                   :levels (list (make-instance 'level :title "One")
                                                 (make-instance 'level :title "Two")
                                                 (make-instance 'level :title "Three")))))
    (handler-case (delete-level level-list 0)
      (:no-error (&rest results) (declare (ignore results)) (error "Expected an error"))
      (error () :success)))

  (let ((level-list (make-instance 'level-list
                                   :levels (list (make-instance 'level :title "One")
                                                 (make-instance 'level :title "Two")
                                                 (make-instance 'level :title "Three")))))
    (delete-level level-list 1)
    (assert (string= (with-output-to-string (out)
                       (display level-list out))
                     "
   1: Two
   2: Three

")))

  (let ((level-list (make-instance 'level-list
                                   :levels (list (make-instance 'level :title "One")
                                                 (make-instance 'level :title "Two")
                                                 (make-instance 'level :title "Three")))))
    (delete-level level-list 2)
    (assert (string= (with-output-to-string (out)
                       (display level-list out))
                     "
   1: One
   2: Three

")))

  (let ((level-list (make-instance 'level-list
                                   :levels (list (make-instance 'level :title "One")
                                                 (make-instance 'level :title "Two")
                                                 (make-instance 'level :title "Three")))))
    (delete-level level-list 3)
    (assert (string= (with-output-to-string (out)
                       (display level-list out))
                     "
   1: One
   2: Two

")))

  (let ((level-list (make-instance 'level-list
                                   :levels (list (make-instance 'level :title "One")
                                                 (make-instance 'level :title "Two")
                                                 (make-instance 'level :title "Three")))))
    (handler-case (delete-level level-list 42)
      (:no-error (&rest results) (declare (ignore results)) (error "Expected an error"))
      (error () :success)))

  :success)

(defun test/insert-level-before/level ()

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new one)
    (assert (equal (levels level-list) (list new one two three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new two)
    (assert (equal (levels level-list) (list one new two three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new three)
    (assert (equal (levels level-list) (list one two new three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list three one)
    (assert (equal (levels level-list) (list three one two))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (select-level level-list three)
    (insert-level-before level-list three one)
    (assert (equal (levels level-list) (list three one two)))
    (assert (eql three (selected-level level-list))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list two one)
    (assert (equal (levels level-list) (list two one three))))


  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (select-level level-list two)
    (insert-level-before level-list two one)
    (assert (equal (levels level-list) (list two one three)))
    (assert (eql two (selected-level level-list))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list two three)
    (assert (equal (levels level-list) (list one two three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (select-level level-list one)
    (insert-level-before level-list two three)
    (assert (equal (levels level-list) (list one two three)))
    (assert (eql one (selected-level level-list))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (handler-case (insert-level-before level-list one one)
      (:no-error (&rest ignored) (declare (ignore ignored))
        (error "Expected an error"))
      (error () :success)))

  :success)

(defun test/insert-level-before/index ()

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new 0)
    (assert (equal (levels level-list) (list new one two three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new 1)
    (assert (equal (levels level-list) (list new one two three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new 2)
    (assert (equal (levels level-list) (list one new two three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new 3)
    (assert (equal (levels level-list) (list one two new three))))

  (let* ((one   (make-instance 'level :title "One"))
         (two   (make-instance 'level :title "Two"))
         (three (make-instance 'level :title "Three"))
         (new   (make-instance 'level :title "Inserted"))
         (level-list (make-instance 'level-list :levels (list one two three))))
    (insert-level-before level-list new 4)
    (assert (equal (levels level-list) (list one two three new))))

  :success)



(defun valid-grid-p (grid)
  (loop :for j :below (array-dimension grid 0)
        :always (loop :for i :below (array-dimension grid 1)
                      :always (and (or (typep (aref grid j i) 'cell)
                                       (error "Not a cell in the grid at ~A ~A -> ~S"
                                              j i (aref grid j i)))
                                   (or (= (cell-x (aref grid j i)) i)
                                       (error "Bad coordinate for cell at ~A ~A -> ~A ~A"
                                              j i
                                              (cell-y (aref grid j i))
                                              (cell-x (aref grid j i))))
                                   (or (= (cell-y (aref grid j i)) j)
                                       (error "Bad coordinate for cell at ~A ~A -> ~A ~A"
                                              j i
                                              (cell-y (aref grid j i))
                                              (cell-x (aref grid j i))))))))

(defun test/move-left ()
  (let ((level (make-instance 'level
                              :title "Test"
                              :cells (com.informatimago.games.stonedge.level-manager::make-cells 3))))
    (assert (valid-grid-p (level-cells level)))
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-left level 3 3)
      (assert (= 2 x))
      (assert (= 3 y))
      (assert (= 7 (array-dimension (level-cells level) 0)))
      (assert (= 7 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO cells didn't change
      )
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-left level 2 2)
      (assert (= 2 x))
      (assert (= 2 y))
      (assert (= 7 (array-dimension (level-cells level) 0)))
      (assert (= 8 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO compare cells
     ))
  :success)

(defun test/move-right ()
  (let ((level (make-instance 'level
                              :title "Test"
                              :cells (com.informatimago.games.stonedge.level-manager::make-cells 3))))
    (assert (valid-grid-p (level-cells level)))
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-right level 3 3)
      (assert (= 4 x))
      (assert (= 3 y))
      (assert (= 7 (array-dimension (level-cells level) 0)))
      (assert (= 7 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO cells didn't change
      )
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-right level 4 4)
      (assert (= 5 x))
      (assert (= 4 y))
      (assert (= 7 (array-dimension (level-cells level) 0)))
      (assert (= 8 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO compare cells
      ))
  :success)

(defun test/move-top ()
  (let ((level (make-instance 'level
                              :title "Test"
                              :cells (com.informatimago.games.stonedge.level-manager::make-cells 3))))
    (assert (valid-grid-p (level-cells level)))
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-top level 3 3)
      (assert (= 3 x))
      (assert (= 2 y))
      (assert (= 7 (array-dimension (level-cells level) 0)))
      (assert (= 7 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO cells didn't change
      )
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-top level 2 2)
      (assert (= 2 x))
      (assert (= 2 y))
      (assert (= 8 (array-dimension (level-cells level) 0)))
      (assert (= 7 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO compare cells
      ))
  :success)

(defun test/move-bottom ()
  (let ((level (make-instance 'level
                              :title "Test"
                              :cells (com.informatimago.games.stonedge.level-manager::make-cells 3))))
    (assert (valid-grid-p (level-cells level)))
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-bottom level 3 3)
      (assert (= 3 x))
      (assert (= 4 y))
      (assert (= 7 (array-dimension (level-cells level) 0)))
      (assert (= 7 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO cells didn't change
      )
    (multiple-value-bind (x y) (com.informatimago.games.stonedge.level-manager::move-bottom level 4 4)
      (assert (= 4 x))
      (assert (= 5 y))
      (assert (= 8 (array-dimension (level-cells level) 0)))
      (assert (= 7 (array-dimension (level-cells level) 1)))
      (assert (valid-grid-p (level-cells level)))
      ;; TODO compare cells
      ))
  :success)


(defun test/all ()
  (test/delete-level/level)
  (test/delete-level/index)
  (test/insert-level-before/level)
  (test/insert-level-before/index)
  (test/move-left)
  (test/move-right)
  (test/move-top)
  (test/move-bottom))

(test/all)
