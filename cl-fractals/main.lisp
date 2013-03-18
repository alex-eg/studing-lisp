;;; Define package
(defpackage :cl-fractals
  (:use :ltk :cl))

(in-package :cl-fractals)

(defvar *axiom-widgets* '())

(defun add-axiom (listbox list text)
    (listbox-append listbox text)
    (setf list (cons text list)))


(defun create-window ()
  (with-ltk ()
    (let* ((c (make-instance 'canvas :borderwidth 2 :relief :sunken))
	   (th (make-instance 'entry :text "theorem"))
	   (f (make-instance 'frame :relief :sunken
			     :borderwidth 2))
	   (lb (make-instance 'listbox :master f))
	   (scrll (make-instance 'scrollbar :orientation :vertical :master f))
	   (add (make-instance 'button :text "Add"))
	   (del (make-instance 'button :text "Delete"))
	   (dpth (make-instance 'entry :text "Depth"))
	   (plot (make-instance 'button :text "Plot"))
	   (quit (make-instance 'button :text "Quit")))
      ;; add scrollbar to listbox
      (configure scrll "command" (concatenate 'string (widget-path lb) " yview"))
      (configure lb "yscrollcommand" (concatenate 'string (widget-path scrll) " set"))
      ;; canvas and theorem entry
      (grid c 0 0 :rowspan 5)
      (grid th 0 1 :columnspan 2 :padx 4 :sticky "we")
      ;; frame with axioms list and a scrollbar
      (grid f 1 1 :columnspan 2)
      (pack scrll :side :right :fill :y)
      (pack lb :side :left)
      ;; remaining widgets
      (grid add 2 1 :sticky "we")
      (grid del 2 2 :sticky "we")
      (grid dpth 3 1 :columnspan 2 :padx 4 :sticky "we")
      (grid plot 4 1)
      (grid quit 4 2))))
