(cl:defpackage :hello-background-framebuffer
  (:use :cl :cl-bodge.engine :cl-bodge.appkit :cl-bodge.graphics :cl-bodge.canvas :cl-bodge.shading)
  (:export #:run-it))
(cl:in-package :hello-background-framebuffer)

(defcanvas background (width height (radius 10))
  (draw-rect (vec2 0 0) width height :fill-paint (vec4 1 1 1 1))
  (loop repeat 100000
        for x = (random width)
        for y = (random height)
        for r = (random radius)
        do (draw-circle (vec2 x y) r
                        :fill-paint (vec4 (/ x width) (/ y height) 0 1)
                        :stroke-paint (vec4 (- (/ x width) 0.1) (- (/ y height) 0.1) 0 1))))


(defvar *viewport-width* 800)
(defvar *viewport-height* 600)
(defvar *viewport-half* (vec2 (/ *viewport-width* 2) (/ *viewport-height* 2)))


(defapp hello-framebuffers ()
  ((background-canvas :initform nil)
   (background-banner :initform nil)
   (ping-pong-pair :initform nil))
  (:viewport-title "Hello Background Framebuffer")
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*))


(defun render-background (canvas texture)
  (render texture canvas :width *viewport-width* :height *viewport-height*))


(defun refresh-background-in-loop (this)
  (with-slots (ping-pong-pair background-canvas) this
    (with-ping-pong-back (texture ping-pong-pair)
      (render-background background-canvas texture)
      (finish-rendering-output))
    (ping-pong-swap ping-pong-pair)
    (inject-flow
     (for-shared-graphics ()
       (refresh-background-in-loop this)))))


(defmethod configuration-flow ((this hello-framebuffers))
  (with-slots (background-texture-front
               background-texture-back
               background-banner
               background-canvas
               ping-pong-pair)
      this
    (for-graphics ()
      (setf background-banner (make-2d-banner -1 -1 2 2))
      (run (for-shared-graphics ()
             (let ((front (make-empty-2d-texture *viewport-width* *viewport-height* :rgba))
                   (back (make-empty-2d-texture *viewport-width* *viewport-height* :rgba))
                   (canvas (make-canvas 'background *viewport-width* *viewport-height*)))
               (render-background canvas front)
               (finish-rendering-output)
               (setf background-canvas canvas
                     ping-pong-pair (make-ping-pong-pair front back)))
             (refresh-background-in-loop this))))))


(defmethod sweeping-flow ((this hello-framebuffers))
  (with-slots (background-banner background-canvas ping-pong-pair) this
    (>> (for-graphics ()
          (dispose background-banner))
        (for-shared-graphics ()
          (loop for value in (list background-canvas
                                   (with-ping-pong-front (front ping-pong-pair) front)
                                   (with-ping-pong-back (back ping-pong-pair) back))
                do (dispose value))))))


(defun render-foreground ()
  (let* ((time (bodge-util:real-time-seconds))
         (r 150)
         (x (+ (* (cos time) r) (x *viewport-half*)))
         (y (+ (* (sin time) r) (y *viewport-half*))))
    (draw-circle (vec2 x y) 100
                 :fill-paint (vec4 1 1 1 0.2))))

(defmethod draw ((this hello-framebuffers))
  (with-slots (background-banner ping-pong-pair) this
    (if ping-pong-pair
        (with-ping-pong-front (front ping-pong-pair)
          (render-banner background-banner front))
        (draw-text (vec2 270 400) "Wait a bit for the background texture"))
    (render-foreground)))

(defun run-it ()
  (ge.app:start 'hello-framebuffers))

(defun stop-it ()
  (ge.app:stop))
