;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               render.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Render a stonedge board game to SVG/PNG.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2024-10-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2024 - 2024
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package :com.informatimago.games.stonedge.render-png)

(defvar *geometry-size* (point 1024 1024))

(defmethod stone-paths ((stone stone) d h alpha corner-radius)
  (multiple-value-bind (orientation x0 y0 x1 y1) (stone-coverage stone)
    ;; Calculate trigonometric values
    (let* ((cos-alpha (cos alpha))
           (sin-alpha (sin alpha))
           ;; Compute relative points similarly to the SwiftUI example
           (h (point 0 h))
           (b (point (* d cos-alpha)     (+ h (* d sin-alpha))))
           (d (point (- (* d cos-alpha)) (+ h (* d sin-alpha))))
           (hu (point 0 (+ d h)))
           (bu (point (* d cos-alpha) (+ d h (* d sin-alpha))))
           (cu (point 0 (+ d h (* 2 d sin-alpha))))
           (du (point (- (* d cos-alpha)) (+ d h (* d sin-alpha))))
           ;; Calculate cell origin (adjusted for Vecto capabilities)
           ;; Assuming `*geometry-size*` is a point of (width height) for the drawing surface
           (oxy (point (+ (* (- x0 y0) (* d cos-alpha)) (/ (x *geometry-size*) 2))
                       (+ (* (+ x0 y0) (* d sin-alpha)) (/ (y *geometry-size*) 2))))
           (pxy (point (+ (* (- x1 y1) (* d cos-alpha)) (/ (x *geometry-size*) 2))
                       (+ (* (+ x1 y1) (* d sin-alpha)) (/ (y *geometry-size*) 2))))
           (dx (- (x pxy) (x oxy)))
           (dy (- (y pxy) (y oxy))))
      
      (when (eq orientation :vertical)
        (setf dx 0)
        (setf dy d))


      ;; print( "stone Oxy(\(stone.x),\(stone.y)) = \(Oxy)  orientation = \(orientation) x0=\(x0) y0=\(y0) z0=\(z0) x1=\(x1) y1=\(y1) z1=\(z1) dx=\(dx) dy=\(dy)")

      ;; view the cell slab

      (lambda ()
        (vecto:move-to (+ (x Oxy) (x H))
                       (+ (y Oxy) (y H)))
        (vecto:arc))
      ;; var path1 = Path()
      ;; path1.move(to: CGPoint(x: Oxy.x + H.x, y: Oxy.y + H.y))
      ;; path1.addArc(tangent1End: CGPoint(x: Oxy.x + H.x, y: Oxy.y + H.y), tangent2End: CGPoint(x: Oxy.x + B.x, y: Oxy.y + B.y), radius: cornerRadius)
      ;; path1.addArc(tangent1End: CGPoint(x: Oxy.x + B.x, y: Oxy.y + B.y), tangent2End: CGPoint(x: Oxy.x + Bu.x, y: Oxy.y + Bu.y), radius: cornerRadius)
      ;; path1.addArc(tangent1End: CGPoint(x: Oxy.x + Bu.x, y: Oxy.y + Bu.y), tang
      ;;                           ent2End: CGPoint(x: Oxy.x + Cu.x, y: Oxy.y + Cu.y), radius: cornerRadius)
      ;; path1.addArc(tangent1End: CGPoint(x: Oxy.x + Cu.x, y: Oxy.y + Cu.y), tangent2End: CGPoint(x: Oxy.x + Du.x, y: Oxy.y + Du.y), radius: cornerRadius)
      ;; path1.addArc(tangent1End: CGPoint(x: Oxy.x + Du.x, y: Oxy.y + Du.y), tangent2End: CGPoint(x: Oxy.x + D.x, y: Oxy.y + D.y), radius: cornerRadius)
      ;; path1.addArc(tangent1End: CGPoint(x: Oxy.x + D.x, y: Oxy.y + D.y), tangent2End: CGPoint(x: Oxy.x + H.x, y: Oxy.y + H.y), radius: cornerRadius)
      ;; path1.closeSubpath()
      ;; 
      ;; let transform = CGAffineTransform(translationX: dx, y: dy)
      ;; let path2 = path1.applying(transform)
      ;; 
      ;; var pathF = path1
      ;; var pathB = path2
      ;; 
      ;; switch(orientation){
      ;;                     case .vertical:
      ;;                     pathF = path1
      ;;                     pathB = path2
      ;;                     case .lateral:
      ;;                     pathF = path2
      ;;                     pathB = path1
      ;;                     case .front:
      ;;                     pathF = path2
      ;;                     pathB = path1
      ;;                     }
      ;; 
      ;; let ticks = Path()

      
      (values pathf pathb ticks) ;; Return simulated paths (placeholders in this example)
      )))


(defmethod render-game ((game game) pathname (file-type (eql :png)))
  "
Writes to the file at PATHNAME a PNG representaiton of the GAME.
"
  (vecto:with-canvas (1024 1024)
    ;; Set the background to white
    (vecto:set-rgb-fill 1 1 1)  ;; RGB for white
    ;; (vecto:paint-background)


  
  #-(and) (let* ((cells (game-cells game))
              (line  (with-output-to-string (out)
                       (loop
                         :initially (princ "+" out)
                         :repeat (array-dimension cells 0)
                         :do (princ "---+" out)))))
         (multiple-value-bind (direction stone-left stone-back stone-right stone-front)
             (stone-coverage  (game-stone game))
           (loop
             :for j :from (1-  (array-dimension cells 1)) :downto 0
             :initially (princ line stream) (terpri stream)
             :do (loop
                   :for i :from 0 :below (array-dimension cells 0)
                   :initially (princ "|" stream)
                   :do (unless (ecase direction
                                 ((:vertical)
                                  (when (and (= stone-left i) (= stone-back j))
                                    (princ "BBB" stream) (princ "|" stream) t))
                                 ((:lateral)
                                  (cond
                                    ((and (= stone-left i) (= stone-back j))
                                     (princ "BBBB" stream) t)
                                    ((and (= stone-right i) (= stone-back j))
                                     (princ "BBB" stream) (princ "|" stream) t)))
                                 ((:front)
                                  (when (and (= stone-left i) (or (= stone-back j)
                                                                  (= stone-front j)))
                                    (princ "BBB" stream) (princ "|" stream) t)))
                         (princ (text-icon (aref cells i j)) stream) (princ "|" stream))
                   :finally (progn
                              (terpri stream)
                              (if (and (eql direction :front) (= stone-front j))
                                  (let ((line (copy-seq line)))
                                    (replace line "BBB" :start1 (+ 1 (* 4 stone-left)))
                                    (princ line stream))
                                  (princ line stream))
                              (terpri stream))))))
    ;; ;; Set the fill color to black
    ;; (vecto:set-rgb-fill 0 0 0)  ;; RGB for red
    ;; 
    ;; ;; Draw a circle
    ;; (vecto:circle 150 150 100)  ;; (x y radius)
    ;; (vecto:fill)


    
    ;; Save the image to a PNG file
    (vecto:save-png pathname)))

(defun arc-ends (x y radius angle1 angle2)
  (list (list (+ x (* radius (cos angle1)))
              (+ y (* radius (sin angle1))))
        (list (+ x (* radius (cos angle2)))
              (+ y (* radius (sin angle2))))))

(defun cross (point &optional (size 5))
  (vecto:move-to (- (x point) size) (- (y point) size))
  (vecto:line-to (+ (x point) size) (+ (y point) size))
  (vecto:move-to (- (x point) size) (+ (y point) size))
  (vecto:line-to (+ (x point) size) (- (y point) size)))

(defun plus (point &optional (size 5))
  (vecto:move-to (- (x point) size) (y point))
  (vecto:line-to (+ (x point) size) (y point))
  (vecto:move-to (x point) (+ (y point) size))
  (vecto:line-to (x point) (- (y point) size)))

(defun essai-pie-wedge (file)
  (vecto:with-canvas (:width 200 :height 200)
    (let* ((x 10)
           (y 10)
           (radius 50)
           (angle1 0)
           (angle2 (/ pi 2))
           (ends   (arc-ends x y radius angle1 angle2)))
      ;; (vecto:translate 50 50)
      ;; (vecto:set-rgb-fill 1 1 1)
      ;; 
      ;; (plus '(0 0))
      ;; (cross (first  ends))
      ;; (cross (second ends))
      ;; 
      ;; (vecto:move-to (x (first ends))  -50)
      ;; (vecto:line-to (x (first ends))  (y (first ends)))
      ;; (vecto:arc x y radius angle1 angle2)
      ;; (vecto:line-to -50               (y (second ends)))
      ;; 
      ;; ;; (vecto:arc x y (/ radius 2) (- angle2) angle1)
      ;; ;; (vecto:arc x y (/ radius 4) angle1 angle2)
      ;; (vecto:stroke)
      ;; ;; (vecto:fill-and-stroke)
      (vecto:save-png file))))

#-(and)
(progn (time (essai-pie-wedge "/tmp/a.png"))
       (com.informatimago.pjb:shell "open /tmp/a.png")
       (com.informatimago.games.stonedge.render-game (make-instance 'game) "/tmp/stonedge-game.png" :png))

