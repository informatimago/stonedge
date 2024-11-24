;;;; -*- mode:lisp;coding:utf-8 -*-

(asdf:defsystem :com.informatimago.games.stonedge
  :name "Stonedge Game"
  :description  "Stonedge Game"
  :author "<PJB> Pascal Bourguignon <pjb@informatimago.com"
  :version "0.0.4"
  :licence "AGPL-3"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Spring 2024")
               ((#:albert #:output-dir)          . "../documentation/com.informatimago.game.stonedge/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("split-sequence"
               "vecto"
               "com.informatimago.common-lisp")
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :components ((:file "packages")
               (:file "stonedge"     :depends-on   ("packages"))
               (:file "generator"    :depends-on   ("packages" "stonedge"))
               (:file "view-globals" :depends-on   ("packages"))
               (:file "render-png"   :depends-on   ("packages" "stonedge" "view-globals"))))


;;;; THE END ;;;;
