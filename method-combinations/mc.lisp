(defpackage :mc
  (:use :cl)
  (:export test
           test-test
           *test*))

(in-package :mc)

(defvar *test* nil)

(define-method-combination test ()
  ((primary () :required t)
   (test (:test)))
  (if *test*
      `(call-method ,(car test) ,(list (car primary)))
      `(call-method ,(car primary))))


(defclass ship ()
  ((health :initarg :health
           :accessor ship-health)
   (destroyed :initform nil
              :accessor ship-destroyed)))

(defclass asteroid ()
  ((mass :initarg :mass
         :reader asteroid-mass)))

(defgeneric collide (a b)
  (:method-combination test))


(defmethod collide ((s ship) (a asteroid))
  (let* ((h (ship-health s))
        (new-health (- h (asteroid-mass a))))
    (if (not (plusp new-health))
        (setf (ship-health s) 0
              (ship-destroyed s) t)
        (setf (ship-health s) new-health))))

(defmethod collide :test ((s1 ship) (a1 asteroid))
  (declare (ignore s1 a1))
  (let ((s (make-instance 'ship :health 100))
        (a (make-instance 'asteroid :mass 50)))
    (call-next-method s a)
    (assert (= 50 (ship-health s)))))

(defun test ()
  (let ((s (make-instance 'ship :health 20))
        (a (make-instance 'asteroid :mass 10)))
    (collide s a)
    (format t "Ship now has ~a health" (ship-health s))))

(defun test-test ()
  (let ((*test* t))
    (collide (make-instance 'ship) (make-instance 'asteroid))))
