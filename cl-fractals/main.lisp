;;; Define package
(defpackage :cl-fractals
  (:use :ltk :cl))

(in-package :cl-fractals)

(defvar *axioms* '())

(defun add-axiom (listbox list text)
    (listbox-append listbox text)
    (setf list (cons text list)))


(defun create-window ()
  (with-ltk ()
    (let* ((c (make-instance 'canvas :borderwidth 2 :relief :groove))
	   (th (make-instance 'entry :text "theorem"))
	   (f (make-instance 'frame :relief :groove :borderwidth 2))
	   (lb (make-instance 'listbox :master f))
	   (scrll (make-instance 'scrollbar :orientation :vertical :master f))
	   (axi (make-instance 'entry :text "axiom"))
	   (add (make-instance 'button :text "Add"
			       :command (lambda ()
					  (let ((txt (text axi)))
					    (add-axiom lb *axioms* txt)))))
	   (del (make-instance 'button :text "Delete"
			       :command (lambda ()
					  (let ((sel (car (listbox-get-selection lb))))
					    (if sel 
						(progn 
						  (listbox-delete lb sel)
						  (listbox-select lb (- sel 1))))))))
	   (dpth (make-instance 'entry :text "Depth"))
	   (plot (make-instance 'button :text "Plot"))
	   (quit (make-instance 'button :text "Quit")))
      ;; add scrollbar to listbox
      (configure scrll "command" (concatenate 'string (widget-path lb) " yview"))
      (configure lb "yscrollcommand" (concatenate 'string (widget-path scrll) " set"))
      ;; canvas and theorem entry
      (grid c 0 0 :rowspan 6 :sticky "ns")
      (grid th 0 1 :columnspan 2 :padx 4 :sticky "we" :pady 2)
      ;; frame with axioms list and a scrollbar
      (grid f 1 1 :columnspan 2)
      (pack scrll :side :right :fill :y)
      (pack lb :side :left)
      ;; remaining widgets
      (grid axi 2 1 :sticky "we" :columnspan 2 :padx 4 :pady 2)
      (grid add 3 1 :sticky "we")
      (grid del 3 2 :sticky "we")
      (grid dpth 4 1 :columnspan 2 :padx 4 :sticky "we" :pady 2)
      (grid plot 5 1 :sticky "we")
      (grid quit 5 2 :sticky "we"))))