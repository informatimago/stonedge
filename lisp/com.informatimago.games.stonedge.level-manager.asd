;;;; -*- mode:lisp;coding:utf-8 -*-

(asdf:defsystem :com.informatimago.games.stonedge.level-manager
  :name "Stonedge Game Level Manager"
  :description  "Stonedge Game Renderers"
  :author "<PJB> Pascal Bourguignon <informatimago@gmail.com>"
  :version "0.0.5"
  :licence "AGPL-3"
  :properties ((#:author-email                   . "informatimago@gmail.com")
               (#:date                           . "Winter 2025")
               ((#:albert #:output-dir)          . "../documentation/com.informatimago.game.stonedge/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("com.informatimago.games.stonedge"
               "com.informatimago.common-lisp")
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :components ((:file "level-manager")))


;;;; THE END ;;;;

