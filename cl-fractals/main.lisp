;;; Load tk bindings
(ql:quickload "ltk")
(use-package :ltk)
;;;
(with-ltk ()
  (let ((button (make-instance 'button :text "A Button")))
    ;; Place BUTTON at x=50, y=40
    (place button 50 40 :width 100 :height 50) ))
