(ql:quickload "ltk")
;;; Define package
(defpackage :cl-fractals
  (:use :ltk :cl))

(in-package :cl-fractals)

;;; Virtual machine
(defclass l-machine ()
  ;;; algorithm slots
  ((rules :accessor lm-rules
	  :initform nil
	  :initarg :rules)
   (axiom :accessor lm-axiom
	  :initform ""
	  :initarg :axiom)
   (angle-divisor :accessor lm-angle-divisor
	  :initform 4
	  :initarg :angle-divisor)
   (depth :accessor lm-depth
	  :initform 0
	  :initarg :depth)

   ;;; state slots
   (x :accessor lm-x
      :initform 0
      :initarg :x)
   (y :accessor lm-y
      :initform 0
      :initarg :y)
   (a :accessor lm-angle
      :initform 0
      :initarg :angle)
   (stack :accessor lm-stack
	  :initform nil
	  :initarg :stack)))

(defmethod lm-clear-state ((m l-machine))
  (setf (lm-x m) 0)
  (setf (lm-y m) 0)
  (setf (lm-stack m) nil)
  (setf (lm-angle m) 0))

(defmethod lm-add-rule ((m l-machine) (text string))
  (labels ((parse-rule (text)
	     (let ((ctext (coerce text 'list)))
	       (cons (car ctext) (accum-value (cdddr ctext) nil))))
	   (accum-value (rlist accum)
	     (if rlist 
		 (accum-value (cdr rlist)
			      (append accum (list (car rlist))))
		 accum)))
    (setf (lm-rules m) 
	  (cons (parse-rule text) (lm-rules m)))))

(defmethod lm-del-rule ((m l-machine) (n integer))
  (setf (lm-rules m)
	(remove (nth n (lm-rules m)) (lm-rules m))))

(defmethod lm-print-contents ((m l-machine))
  (format t "rules: ~{~a~^, ~}~%axiom: ~a~%depth: ~a~%angle: ~a~%"
	  (lm-rules m)
	  (lm-axiom m)
	  (lm-depth m)
	  (lm-angle m)))

;;; Functions
(defvar *rules*)
(setf *rules* nil)

(defvar *axiom*)
(setf *axiom* "")

(defvar *depth*)
(setf *depth* 0)

(defvar *angle*)
(setf *angle* 12)

(defun parse-rule (text)
  (labels ((accum-value (rlist accum)
	     (if rlist (accum-value (cdr rlist)
				    (append accum (list (car rlist))))
		 accum)))
    (let ((ctext (coerce text 'list)))
      (cons (car ctext) (accum-value (cdddr ctext) nil)))))

(defun add-rule (text)
  (setf *rules* (cons (parse-rule text) *rules*)))

(defun del-rule (number)
  (setf *rules* (remove (nth number *rules*) *rules* :test #'equal)))

(defun set-axiom (text)
  (setf *axiom* text))

(defun set-depth (num)
  (setf *depth* num))

(defun print-contents ()
  (format t "rules: ~{~a~^, ~}~%axiom: ~a~%depth: ~a~%" *rules* *axiom* *depth*))

(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(defvar *len*)
(setf *len* 4)

(defstruct state x y a)

(defun plot-fractal (canvas lm)
  (lm-clear-state lm)
  (let ((depth (lm-depth lm))
	(axiom (lm-axiom lm))
	(rules (lm-rules lm))
	(d-alpha (/ (* 2 pi) (lm-angle-divisor lm)))
;	(x (lm-x lm))
;	(y (lm-y lm))
	(x (/ *canvas-width* 2))
	(y (/ *canvas-height* 2))
	(a (lm-angle lm))
	(stack (lm-stack lm)))
    (defun rec-plot (cur-depth depth current-string c)
      (if current-string
	  ;; recursing down
	  (if (and (< cur-depth depth)
		   (assoc (car current-string) rules))
	      (progn 
		(rec-plot (+ 1 cur-depth)
			  depth
			  (cdr (assoc (car current-string) rules))
			  c)
		(rec-plot cur-depth depth (cdr current-string) c))
	      (progn 
		(cond ((equal (car current-string) #\F)
		       (create-line c (list x 
					    y
					    (+ x (* *len* (cos a)))
					    (+ y (* *len* (sin a)))))
		       (setf x (+ x (* *len* (cos a))))
		       (setf y (+ y (* *len* (sin a))))
		       )
		      
		      ((equal (car current-string) #\-) 
		       (setf a (- a d-alpha)))
		      
		      ((equal (car current-string) #\+)
		       (setf a (+ a d-alpha)))
		      
		      ((equal (car current-string) #\[)
		       (setf stack
			     (cons (make-state :x x :y y :a a)
				   stack)))
		      
		      ((equal (car current-string) #\])
		       (setf a (state-a (car stack)))
		       (setf x (state-x (car stack)))
		       (setf y (state-y (car stack)))
		       (setf stack (cdr stack))))

		(rec-plot cur-depth depth (cdr current-string) c)))))
    
    (lm-print-contents lm)
    (rec-plot 0 depth (coerce axiom 'list) canvas)))

;;; View 

(defvar *lm*)
(setf *lm* (make-instance 'l-machine))

(defun create-window ()
  (with-ltk ()
    (let* ((c (make-instance 'canvas			
			     :width *canvas-width*
     			     :height *canvas-height*))
	   (axi (make-instance 'entry :text "FXF--FF--FF"))
	   (f (make-instance 'frame :relief :groove :borderwidth 2))
	   (lb (make-instance 'listbox :master f))
	   (scrll (make-instance 'scrollbar :orientation :vertical :master f))
	   (rul (make-instance 'entry :text "->"))
	   (add (make-instance 'button :text "Add"
			       :command (lambda ()
					  (let ((txt (text rul)))
					    (listbox-append lb txt)
					    (lm-add-rule *lm* txt)))))
	   (del (make-instance 'button :text "Delete"
			       :command (lambda ()
					  (let ((sel (car (listbox-get-selection lb))))
					    (if sel 
						(progn
						  (lm-del-rule *lm* sel)
						  (listbox-delete lb sel)
						  (cond ((> 0 (- sel 1)) (listbox-select lb sel))
							(t (listbox-select lb (- sel 1))))))))))
	   (dpth_l (make-instance 'label :text "Depth:"))
	   (dpth (make-instance 'entry :text "4"))
	   (angl_l (make-instance 'label :text "Angle:"))
	   (angl (make-instance 'entry :text "4"))
	   (plot (make-instance 'button :text "Plot" 
				:command (lambda ()
					   (setf (lm-axiom *lm*) (text axi))
					   (setf (lm-depth *lm*) (read-from-string (text dpth)))
					   (setf (lm-angle-divisor *lm*) (read-from-string (text angl)))
					   (plot-fractal c *lm*))))
	   (quit (make-instance 'button :text "Quit" 
				:command (lambda ()
					   (setf *exit-mainloop* t)))))
      ;; add scrollbar to listbox
      (configure scrll "command" (concatenate 'string (widget-path lb) " yview"))
      (configure lb "yscrollcommand" (concatenate 'string (widget-path scrll) " set"))
      ;; canvas and theorem entry
      (grid c 0 0 :rowspan 7 :sticky "ns" :padx 4 :pady 4)
      (grid axi 0 1 :columnspan 2 :padx 4 :sticky "we" :pady 2)
      ;; frame with rules list and a scrollbar
      (grid f 1 1 :columnspan 2)
      (pack scrll :side :right :fill :y)
      (pack lb :side :left)
      ;; remaining widgets
      (grid rul 2 1 :sticky "we" :columnspan 2 :padx 4 :pady 2)
      (grid add 3 1 :sticky "we")
      (grid del 3 2 :sticky "we")
      (grid dpth_l 4 1)
      (grid dpth 4 2 :padx 4 :sticky "we" :pady 2)
      (grid angl_l 5 1)
      (grid angl 5 2 :padx 4 :sticky "we" :pady 2)
      (grid plot 6 1 :sticky "we")
      (grid quit 6 2 :sticky "we")
      ;; initialize canvas
      (configure c :background "white")
      (create-rectangle c 1 1 *canvas-width* *canvas-height*))))
