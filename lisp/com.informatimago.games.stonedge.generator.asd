;;;; -*- mode:lisp;coding:utf-8 -*-

(asdf:defsystem :com.informatimago.games.stonedge.generator
  :name "Stonedge Game Generator"
  :description  "Stonedge Game Generator"
  :author "<PJB> Pascal Bourguignon <pjb@informatimago.com"
  :version "0.0.5"
  :licence "Proprietary"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2024")
               ((#:albert #:output-dir)          . "../documentation/com.informatimago.game.stonedge/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("split-sequence"
               "com.informatimago.common-lisp"
               "com.informatimago.games.stonedge")
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :components ((:file "generator-packages")
               (:file "generator"    :depends-on   ("generator-packages"))))


;;;; THE END ;;;;
