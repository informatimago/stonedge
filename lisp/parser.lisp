;;;; com.informatimago.games.stonedge.parser
;;;; Translation of GameParser.swift to Common Lisp
;;;; Pascal Bourguignon, 2024 — Swift original referenced.

(defpackage :com.informatimago.games.stonedge.parser
  (:use :cl
        :com.informatimago.games.stonedge)
  (:export
   #:parse-level
   #:create-cell
   ;; exported types & helpers
   #:make-definition
   #:definition-name #:definition-link #:definition-connected #:definition-state
   #:*empty-name* #:*empty-name2* #:*start-name* #:*solid-name*
   #:*target-name* #:*ice-name* #:*crumble-name*))

(in-package :com.informatimago.games.stonedge.parser)

;;; --- Assumptions about the game model --------------------------------------
;;; You already have CLOS classes like:
;;;   cell, empty-cell, solid-cell, target-cell, ice-cell, crumble-cell,
;;;   pathway-cell, red-button-cell, blue-button-cell
;;; and classes/ctors for STONE and GAME.
;;; We define a few generic setters so this file does not need to know
;;; your exact slot names.

(defgeneric set-button-switches (button switches)
  (:documentation "Attach a list of PATHWAY-CELL instances to BUTTON."))

(defgeneric set-pathway-state (pathway state)
  (:documentation "Set pathway state to one of :OPEN or :CLOSED."))


(defmethod set-button-switches ((button button-cell) switches)
  (setf (button-cell-switches button) (copy-seq switches)))
(defmethod set-pathway-state ((pathway pathway-cell) state)
  (setf (slot-value pathway 'com.informatimago.games.stonedge::state) state))

(defun make-level (title description cells #|connections|#)
  (make-instance 'level
                 :title (or title "Untitled Level")
                 :description (or description "")
                 :cells (cond
                          ((listp cells)
                           (make-array (list (length cells) (length (first cells)))
                                       :initial-contents cells))
                          ((not (arrayp cells))
                           (error "~S: cells must be an array"))
                          (t (ecase (array-rank cells) 
                               ((2) cells)
                               ((1) (make-array (list (length cells) (length (aref cells 0)))
                                                :initial-contents cells)))))
                 ;; connections
                 ))

;;; OPTIONAL: If your STONE and GAME have different initialization APIs,
;;; adapt these two helpers:
(defun make-stone (&key x y orientation)
  (make-instance 'stone :x x :y y
                        :direction (ecase orientation
                                     (:vertical #(0 0 1))
                                     (:front    #(0 1 0))
                                     (:lateral  #(1 0 0)))))

(defun make-game (&key stone cells title description)
  (let ((g (make-instance 'game :stone stone :cells cells)))
    (setf (level-title g) (or title ""))
    (setf (level-description g) (or description ""))
    g))

;;; --- Data structures mirrored from Swift -----------------------------------

;; Swift enum LinkType { case red, blue, pathway }
;; Lisp uses keywords:
;;   :RED, :BLUE, :PATHWAY

(defstruct definition
  (name      "" :type string)
  (link      :pathway :type keyword)  ; one of :RED :BLUE :PATHWAY
  (connected '() :type list)          ; list of cell-name strings
  (state     :closed :type keyword))  ; :OPEN or :CLOSED

(defparameter *empty-name* ".")
(defparameter *empty-name2* " ")
(defparameter *start-name* "S")
(defparameter *solid-name* "O")
(defparameter *target-name* "T")
(defparameter *ice-name* "I")
(defparameter *crumble-name* "C")

;;; --- Small string helpers ---------------------------------------------------

(defun string-all-p (pred string)
  "Return T if PRED holds for every character of STRING."
  (loop for ch across string always (funcall pred ch)))

(defun trim-left-bars (line)
  "Imitates the Swift logic: if LINE starts with |, drop leading spaces and |."
  (if (and (> (length line) 0) (char= (aref line 0) #\|))
      (let* ((n (length line))
             (i (loop for i from 0 below n
                      for ch = (aref line i)
                      while (or (char= ch #\|) (char= ch #\Space))
                      finally (return i))))
        (subseq line i))
      line))

(defun split-lines (spec)
  "Split SPEC on #\Newline, keep empty lines."
  (let ((start 0) (res '()))
    (loop for i from 0 below (length spec)
          do (when (char= (aref spec i) #\Newline)
               (push (subseq spec start i) res)
               (setf start (1+ i))))
    (push (subseq spec start) res)
    (nreverse res)))

(defun uppercase-lines (lines)
  (mapcar #'string-upcase lines))

;;; --- Core parsing -----------------------------------------------------------

(defun find-first-index (lines predicate)
  "Return index of first line satisfying PREDICATE or NIL."
  (loop for line in lines
        for idx from 0
        when (funcall predicate line) do (return idx)
        finally (return nil)))

(defun all-dots-non-empty-p (line)
  "True if LINE is not empty and all characters are dots."
  (and (> (length line) 0)
       (string-all-p (lambda (ch) (char= ch #\.)) line)))

(defun non-empty-and-not-all-dots-p (line)
  (and (> (length line) 0)
       (not (all-dots-non-empty-p line))))

(defun parse-definitions (cell-def-lines)
  "Return (hash-table-of name->definition)."
  (let ((defs (make-hash-table :test 'equal)))
    (dolist (line cell-def-lines)
      ;; Swift did: components = line.split(" ")
      (let* ((components (remove "" (uiop:split-string line :separator " ") :test #'string=)))
        (when (and components (second components))
          (let* ((cell-name-first (aref (first components) 0))
                 (name (string cell-name-first))
                 (directive (second components))
                 (rest (subseq components 2))
                 ;; Swift joins the rest, then splits every character as a name.
                 (connected-names (map 'list #'string
                                       (coerce (apply #'concatenate 'string rest) 'list)))
                 (link :pathway)
                 (state :closed))
            (ecase (intern (string-upcase directive) :keyword)
              (:PATHWAY
               (setf link :pathway
                     state (if (and rest (string-equal (first rest) "OPEN"))
                               :open :closed)))
              (:RED (setf link :red))
              (:BLUE (setf link :blue)))
            (setf (gethash name defs)
                  (make-definition :name name :link link
                                   :connected connected-names :state state))))))
    defs))

(defun width-of-grid (grid-lines)
  (reduce #'max grid-lines :initial-value 0 :key #'length))

(defun make-cell (class x y)
  (make-instance class :x x :y y))

(defun create-cell (cell-name x y definitions)
  "Mirrors Swift createCell(from: x:y:definitions:)."
  (let ((def (gethash cell-name definitions)))
    (cond
      (def
          (ecase (definition-link def)
            (:red     (make-cell 'red-button-cell  x y))
            (:blue    (make-cell 'blue-button-cell x y))
            (:pathway (make-cell 'pathway-cell     x y))))
      (t
       (cond
         ((or (string= cell-name *empty-name*)
              (string= cell-name *empty-name2*))
          (make-cell 'empty-cell x y))
         ((string= cell-name *solid-name*)
          (make-cell 'solid-cell x y))
         ((string= cell-name *start-name*)
          (make-cell 'start-cell x y))
         ((string= cell-name *target-name*)
          (make-cell 'target-cell x y))
         ((string= cell-name *ice-name*)
          (make-cell 'ice-cell x y))
         ((string= cell-name *crumble-name*)
          (make-cell 'crumble-cell x y))
         (t
          (make-cell 'solid-cell x y)))))))

(defun remove-prefix-whitespaces (string)
  (let ((start (position #\space string :test-not (function eql))))
    (if start
        (subseq string start)
        string)))

(defun parse-level (specification)
  "Parse SPECIFICATION (string) and build a GAME or return NIL on failure.
Follows the same steps as the Swift version."
  (let* ((lines          (mapcar (function remove-prefix-whitespaces) (split-lines specification)))
         (title-index    (find-first-index lines #'non-empty-and-not-all-dots-p))
         (first-grid-idx (find-first-index lines #'all-dots-non-empty-p))
         (title "")
         (description '()))
    ;; Title + description (before the first dotted grid)
    (when (and title-index
               (or (null first-grid-idx) (< title-index first-grid-idx)))
      (setf title (nth title-index lines))
      (let* ((from (1+ title-index))
             (to   (or first-grid-idx (length lines))))
        (setf description
              (loop for i from from below to
                    for line = (nth i lines)
                    collect (if (string= line "")
                                line
                                (trim-left-bars line))))))

    ;; Uppercase all lines for parsing grid & defs (matches Swift)
    (let* ((ulines      (uppercase-lines lines))
           ;; First definition line: doesn't start with "|" and contains PATHWAY/RED/BLUE
           (first-def-idx (or (find-first-index
                               ulines
                               (lambda (ln)
                                 (and (plusp (length ln))
                                      (not (char= (aref ln 0) #\|))
                                      (or (search "PATHWAY" ln)
                                          (search "RED" ln)
                                          (search "BLUE" ln)))))
                              (length ulines)))
           (grid-lines   (if first-grid-idx
                             (subseq ulines first-grid-idx first-def-idx)
                             '()))
           (cell-defines (subseq ulines first-def-idx))
           (height       (length grid-lines))
           (width        (width-of-grid grid-lines))
           (definitions  (parse-definitions cell-defines))
           (named-cells  (make-hash-table :test 'equal))
           (grid         (make-array height))
           (stone        nil))

      ;; Fill grid and track coordinates
      (loop for y from 0 below height
            for line = (nth y grid-lines) do
            ;; (terpri)
            (let ((row '()))
              (loop for x from 0 below (length line)
                    for ch = (aref line x)
                    for name = (string ch)
                    for cell = (create-cell name x y definitions) do
                    ;; (print (list x y name cell))
                    (setf row (nconc row (list cell)))
                    (setf (gethash name named-cells) cell))
              ;; pad up to WIDTH with EmptyCell
              (loop for x from (length row) below width
                    do (setf row (nconc row (list (make-cell 'empty-cell x y)))))
              (setf (aref grid y) row)))

      ;; Validate borders and apply definitions
      (labels ((near-border-p (x y)
                 (or (< x 2) (< y 2) (< (- width 2) x) (< (- height 2) y))))
        (loop for y from 0 below height
              for line across grid do
              (loop for x from 0 below width
                    for cell = (nth x line)
                    for ch   = (if (< x (length (nth y grid-lines)))
                                   (aref (nth y grid-lines) x)
                                   #\.) do
                    ;; Border rule: only EmptyCell allowed near border
                    (when (and (near-border-p x y)
                               (not (typep cell 'empty-cell)))
                      (cerror  "Continue returning NIL"
                               "Cells must be 2 step away from a border (~A ~A ~A)" (type-of cell) x y)
                      (return-from parse-level nil))
                    ;; Apply definitions for exact (x,y) of the named cell
                    (let* ((cell-name (string ch))
                           (named (gethash cell-name named-cells)))
                      (when (and named
                                 (eql (cell-x named) x)
                                 (eql (cell-y named) y))
                        (let ((def (gethash cell-name definitions)))
                          (when def
                            (ecase (definition-link def)
                              (:red
                               (let ((button cell))
                                 (set-button-switches
                                  button
                                  (mapcar (lambda (nm) (gethash nm named-cells))
                                          (definition-connected def)))))
                              (:blue
                               (let ((button cell))
                                 (set-button-switches
                                  button
                                  (mapcar (lambda (nm) (gethash nm named-cells))
                                          (definition-connected def)))))
                              (:pathway
                               (set-pathway-state cell (definition-state def)))))))))))
      (make-level title
                  description
                  grid
                  ;; connections
                  )
      ;; ;; Build GAME
      ;; (when stone
      ;;   (return-from parse-level
      ;;     (make-game :stone stone
      ;;                :cells (make-array (list (length grid) (length (aref grid 0)))
      ;;                                   :initial-contents grid)
      ;;                :title title
      ;;                :description description)))
      ;; ;; no STONE found
      ;; (cerror "Continue returning NIL" "Missing a start cell")
      ;; nil
      )))
