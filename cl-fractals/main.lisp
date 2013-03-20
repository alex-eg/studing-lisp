;;; Define package
(defpackage :cl-fractals
  (:use :ltk :cl))

(in-package :cl-fractals)

;;; Virtual machine
(defvar *rules* '())
(setf *rules* nil)

(defvar *axiom* "")
(setf *axiom* "")

(defvar *depth* 0)
(setf *depth* 0)

(defvar *angle* 360/6)
(setf *angle* 360/6)

(defun parse-rule (text)
  (labels ((accum-key (rlist accum)
	   (if (and (equal (car rlist) #\-)
		    (equal (cadr rlist) #\>))
	       (cons accum (accum-value (cddr rlist) nil))
	       (accum-key (cdr rlist)
			  (append accum (list (car rlist))))))
	   
	   (accum-value (rlist accum)
	     (if rlist (accum-value (cdr rlist)
				    (append accum (list (car rlist))))
		 accum)))
    (accum-key (coerce text 'list) nil)))

(defun add-rule (text)
  (setf *rules* (cons (parse-rule text) *rules*)))

(defun del-rule (number)
  (setf *rules* (remove (nth number *rules*) *rules* :test #'equal)))

(defun set-axiom (text)
  (setf *axiom* text))

(defun set-depth (num)
  (setf *depth* num))

(defun plot ()
  (format t "rules: ~{~a~^, ~}~%theorem: ~a~%depth: ~a" *rules* *axiom* *depth*))

;;; View 
(defun create-window ()
  (with-ltk ()
    (let* ((c (make-instance 'canvas :borderwidth 2 :relief :groove))
	   (axi (make-instance 'entry :text "FXF--FF--FF"))
	   (f (make-instance 'frame :relief :groove :borderwidth 2))
	   (lb (make-instance 'listbox :master f))
	   (scrll (make-instance 'scrollbar :orientation :vertical :master f))
	   (rul (make-instance 'entry :text "->"))
	   (add (make-instance 'button :text "Add"
			       :command (lambda ()
					  (let ((txt (text rul)))
					    (listbox-append lb txt)
					    (add-rule txt)))))
	   (del (make-instance 'button :text "Delete"
			       :command (lambda ()
					  (let ((sel (car (listbox-get-selection lb))))
					    (if sel 
						(progn
						  (del-rule sel)
						  (listbox-delete lb sel)
						  (cond ((> 0 (- sel 1)) (listbox-select lb sel))
							(t (listbox-select lb (- sel 1))))))))))
	   (dpth (make-instance 'entry :text "Depth"))
	   (plot (make-instance 'button :text "Plot" 
				:command (lambda ()
					   (set-axiom (text axi))
					   (set-depth (read-from-string (text dpth)))
					   (plot))))
	   (quit (make-instance 'button :text "Quit" 
				:command (lambda ()
					   (setf *exit-mainloop* t)))))
      ;; add scrollbar to listbox
      (configure scrll "command" (concatenate 'string (widget-path lb) " yview"))
      (configure lb "yscrollcommand" (concatenate 'string (widget-path scrll) " set"))
      ;; canvas and theorem entry
      (grid c 0 0 :rowspan 6 :sticky "ns")
      (grid axi 0 1 :columnspan 2 :padx 4 :sticky "we" :pady 2)
      ;; frame with rules list and a scrollbar
      (grid f 1 1 :columnspan 2)
      (pack scrll :side :right :fill :y)
      (pack lb :side :left)
      ;; remaining widgets
      (grid rul 2 1 :sticky "we" :columnspan 2 :padx 4 :pady 2)
      (grid add 3 1 :sticky "we")
      (grid del 3 2 :sticky "we")
      (grid dpth 4 1 :columnspan 2 :padx 4 :sticky "we" :pady 2)
      (grid plot 5 1 :sticky "we")
      (grid quit 5 2 :sticky "we"))))
