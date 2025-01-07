;;;; -*- mode:lisp;coding:utf-8 -*-

(asdf:defsystem :com.informatimago.games.stonedge.render
  :name "Stonedge Game Renderers"
  :description  "Stonedge Game Renderers"
  :author "<PJB> Pascal Bourguignon <pjb@informatimago.com"
  :version "0.0.5"
  :licence "AGPL-3"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2024")
               ((#:albert #:output-dir)          . "../documentation/com.informatimago.game.stonedge/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("com.informatimago.games.stonedge"
               "vecto" "cl-svg"
               "com.informatimago.common-lisp")
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :components ((:file "render-packages")
               (:file "view-globals" :depends-on   ("render-packages"))
               (:file "render-png"   :depends-on   ("render-packages" "view-globals"))
               (:file "render-svg"   :depends-on   ("render-packages" "view-globals"))))


;;;; THE END ;;;;

