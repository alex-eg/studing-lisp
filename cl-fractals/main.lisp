;;; Define package
(defpackage :cl-fractals
  (:use :ltk :cl))

(in-package :cl-fractals)

(defun create-window ()
  (with-ltk ()
    (let* ((c (make-instance 'canvas :borderwidth 2 :relief :sunken))
	   (th (make-instance 'entry :text "theorem"))
	   (f (make-instance 'frame :borderwidth 2 :relief :sunken))
	   (scrll (make-instance 'scrollbar :master f))
	   (axi (make-instance 'entry  :text "axiom" :master f))
	   (add (make-instance 'button :text "Add" :master f))
	   (del (make-instance 'button :text "Delete" :master f))
	   (dpth (make-instance 'entry :text "Depth"))
	   (plot (make-instance 'button :text "Plot"))
	   (quit (make-instance 'button :text "Quit")))
      (grid c 0 0 :rowspan 5)
      (grid th 0 1 :columnspan 2)
;;      (pack f)
;;      (pack axi)
;;      (pack scrll :side :right)
;;      (pack add :side :left)
;;      (pack del :side :left)
      (grid f 1 1 :rowspan 3 :columnspan 2 :sticky "n")
      (grid axi 1 1 :columnspan 2)
      (grid add 2 1)
      (grid del 2 2)
      (grid dpth 3 1 :columnspan 2 :sticky "n")
      (grid plot 4 1 :sticky "n")
      (grid quit 4 2 :sticky "n"))))
		    


(defun hello-1 ()
  (with-ltk ()
    (let ((button (make-instance 'button :text "A Button")))
      ;; Place BUTTON at x=50, y=40
      (place button 50 40 :width 100 :height 50) )))
