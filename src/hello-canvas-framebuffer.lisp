(cl:defpackage :hello-canvas-framebuffer
  (:use :cl :cl-bodge.engine :cl-bodge.appkit :cl-bodge.graphics :cl-bodge.canvas :cl-bodge.shading)
  (:export #:run-it))

(cl:in-package :hello-canvas-framebuffer)

(defcanvas background (width height (radius 10))
  (draw-rect (vec2 0 0) width height :fill-paint (vec4 1 1 1 1))
  (loop repeat 100000
        for x = (random width)
        for y = (random height)
        for r = (random radius)
        do (draw-circle (vec2 x y) r
                        :fill-paint (vec4 (/ x width) (/ y height) 0 1)
                        :stroke-paint (vec4 (- (/ x width) 0.1) (- (/ y height) 0.1) 0 1))))

(cl:in-package :hello-canvas-framebuffer)

(defun render-background ()
  ;; Temporary canvas object to draw our background
  (let ((canvas (make-canvas 'background *viewport-width* *viewport-height*))
        (texture (make-empty-2d-texture *viewport-width* *viewport-height* :rgba)))
    ;; Using texture as a target we render canvas we defined earlier
    (render texture canvas :width *viewport-width* :height *viewport-height*)
    (dispose canvas)
    texture))

(cl:in-package :hello-canvas-framebuffer)

(defvar *viewport-width* 800)
(defvar *viewport-height* 600)
(defvar *viewport-half* (vec2 (/ *viewport-width* 2) (/ *viewport-height* 2)))

;; Definition of our application
(defapp hello-framebuffer ()
  ((background-texture :initform nil)
   (background-banner :initform nil)
   (shared-context :initform nil))
  (:viewport-title "Hello Framebuffers")
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*))

(cl:in-package :hello-canvas-framebuffer)

(defmethod configuration-flow ((this hello-framebuffer))
  (with-slots (background-texture background-banner shared-context) this
    (>> (graphics-context-assembly-flow)
        (for-graphics (context)
          ;; cl-bodge's banner object for blitting our texture onto the screen:
          ;; these arguments tell it fill the whole viewport
          (setf background-banner (make-2d-banner -1 -1 2 2)
                shared-context context)
          ;; And here we invoke a function we defined earlier to render background scene onto a
          ;; texture in the background thread!
          (run (for-graphics :context shared-context ()
                 ;; Texture with our rendered background
                 (setf background-texture (render-background))))))))


(defmethod sweeping-flow ((this hello-framebuffer))
  (with-slots (background-texture background-banner) this
    (for-graphics ()
      (when background-texture
        (dispose background-texture))
      (dispose background-banner))))

(cl:in-package :hello-canvas-framebuffer)

;; Just a circle circling through circles
(defun render-foreground ()
  (let* ((time (bodge-util:real-time-seconds))
         (r 150)
         (x (+ (* (cos time) r) (x *viewport-half*)))
         (y (+ (* (sin time) r) (y *viewport-half*))))
    (draw-circle (vec2 x y) 100
                 :fill-paint (vec4 1 1 1 0.2))))

(defmethod draw ((this hello-framebuffer))
  (with-slots (background-banner background-texture) this
    ;; First, blit our background when texture is ready
    (if background-texture
        (render-banner background-banner background-texture)
        (draw-text (vec2 270 400) "Wait a bit for the background texture"))
    ;; Second, our foreground
    (render-foreground)))

(cl:in-package :hello-canvas-framebuffer)

(defun run-it ()
  (ge.app:start 'hello-framebuffer))

(defun stop-it ()
  (ge.app:stop))
