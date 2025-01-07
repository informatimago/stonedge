(in-package "COMMON-LISP-USER")

;;; --------------------------------------------------------------------
;;; Load the generator

(load (make-pathname :name "generate" :type "lisp" :version nil
                     :defaults (or *load-pathname* #P"./")))

;;; --------------------------------------------------------------------
;;; generate the program
;;;

(defparameter *source-directory*  (make-pathname :name nil :type nil :version nil
                                                 :defaults (or *load-pathname* (truename (first (directory #P"./*.lisp"))))))
(defparameter *asdf-directories*
  (append (mapcar (lambda (path) (make-pathname :name nil :type nil :version nil :defaults path))
                  (append (directory (merge-pathnames "**/*.asd"
                                                      *source-directory*
                                                      nil))
                          (directory (merge-pathnames "**/*.asd"
                                                      (translate-logical-pathname #P"HOME:src;public;lisp;")
                                                      nil))
                          (list *source-directory* )))))

(defparameter *release-directory* *source-directory* #|#P"HOME:bin;"|# "Where the executable will be stored." )


(say "*source-directory*  = ~S~%" *source-directory*)
(say "*asdf-directories*  = ~S~%" *asdf-directories*)
(say "*release-directory* = ~S~%" *release-directory*)

(generate-program :program-name "generator"
                  :main-function "COM.INFORMATIMAGO.GAMES.STONEDGE.GENERATOR:MAIN"
                  :system-name "com.informatimago.games.stonedge.generator"
                  :system-list '()
                  :init-file "~/.stonedge-generator.lisp"
                  :version "1.0.0"
                  :copyright (format nil "Copyright Pascal J. Bourguignon 2024 - 2025~%License: Proprietary")
                  :source-directory  *source-directory*
                  :asdf-directories  *asdf-directories*
                  :release-directory *release-directory*)

