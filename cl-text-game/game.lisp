(defparameter *nodes* '((first-floor (you are in a big room. nobody else is present here.))
			(garden (you are in the garden. there is a well in front of you.))
			(attic (you are in the attic. there is a bed and a door to the right.))
			(balcony (you are at the balcony. you can see distant forest from here.))))

(defparameter *edges* '((first-floor (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living room downstairs ladder)
			 (balcony north door))
			(balcony (attic south door))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

