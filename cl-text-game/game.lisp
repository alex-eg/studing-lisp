;;; Small text game engine
;;; From The Land of Lisp!

(defparameter *nodes* '((first-floor (you are in a big room. old wizard snortling on the sofa.))
			(garden (you are in the garden. there is a well in front of you.))
			(attic (you are in the attic. there is a bed and a door to the right.))
			(balcony (you are at the balcony. you can see distant forest from here.))))

(defparameter *edges* '((first-floor (garden west door)
			 (attic upstairs ladder))
			(garden (first-floor east door))
			(attic (first-floor downstairs ladder)
			 (balcony north door))
			(balcony (attic south door))))

(defparameter *objects* '(vodka bucket frog chain binoculars))

(defparameter *object-locations* '((vodka first-floor)
				   (bucket first-floor)
				   (chain garden)
				   (frog garden)
				   (binoculars balcony)))

(defparameter *location* 'first-floor)

(defun object-at (loc objs obj-locs)
  (flet ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-visible-objects (loc objs obj-loc)
  (flet ((describe-obj (obj)
	   `(you see ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (object-at loc objs obj-loc)))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-visible-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next 
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way))))

(defun pickup (object)
  (cond ((member object
		 (object-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are carrying the ,object))
	(t '(you cannot get that))))

(defun inventory ()
  (cons 'items- (object-at 'body *objects* *object-locations*)))
