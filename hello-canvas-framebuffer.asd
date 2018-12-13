(asdf:defsystem :hello-canvas-framebuffer
  :description "Example of simple framebuffer usage"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (cl-bodge/appkit cl-bodge/graphics cl-bodge/canvas cl-bodge/shading)
  :pathname "src/"
  :serial t
  :components ((:file "hello-canvas-framebuffer")))
