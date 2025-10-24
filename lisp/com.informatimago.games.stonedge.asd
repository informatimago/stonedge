;;;; -*- mode:lisp;coding:utf-8 -*-

(asdf:defsystem :com.informatimago.games.stonedge
  :name "Stonedge Game"
  :description  "Stonedge Game"
  :author "<PJB> Pascal Bourguignon <pjb@informatimago.com"
  :version "0.0.5"
  :licence "AGPL-3"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Automn 2025")
               ((#:albert #:output-dir)          . "../documentation/com.informatimago.game.stonedge/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("split-sequence"
               "com.informatimago.common-lisp")
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :components ((:file "stonedge-packages")
               (:file "stonedge"     :depends-on   ("stonedge-packages"))
               (:file "parser"       :depends-on   ("stonedge-packages" "stonedge"))
               (:file "levels"       :depends-on   ("stonedge-packages" "stonedge" "parser"))
               (:file "player"       :depends-on   ("stonedge-packages" "stonedge" "parser"))
               (:file "solver"       :depends-on   ("stonedge-packages" "stonedge" "parser"))))


;;;; THE END ;;;;
