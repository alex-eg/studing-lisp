(defpackage :setter-macros
  (:use :cl))

(in-package :setter-macros)

(defmacro make-setter (place function &rest subscripts)
  "Make setter by place and function, that must be known at runtime"
  (let ((accessor (list 'setf function))
        (new-value (gensym)))
    `(lambda (,new-value)
       (funcall (function ,accessor) ,new-value ,place ,@subscripts))))

(defmacro make-setf-setter (place)
  "Make setf setter maker by place. Actual setf functions may be
provided as arguments for setter-maker"
  (alexandria:with-gensyms (accessor
                            new-value subscripts
                            setter d v n g)
    `(lambda (,accessor &rest ,subscripts)
       (multiple-value-bind (,d ,v ,n ,setter ,g)
           (get-setf-expansion (append (list ,accessor)
                                       (list ,place)
                                       ,subscripts))
         (declare (ignorable ,d ,v ,n ,g))
         (lambda (,new-value)
           (apply (eval (second ,setter))
                  (append (list ,new-value ,place) ,subscripts)))))))

(defclass whatever ()
  ((a-slot :accessor a-slot :initform nil)
   (b-slot :accessor b-slot :initform nil)))

(defun test ()
  (let* ((w (make-instance 'whatever))
         (setter-maker (make-setf-setter w))
         (a-setter (funcall setter-maker 'a-slot))
         (b-setter (funcall setter-maker 'b-slot)))
    (format t "~A ~A~%" (a-slot w) (b-slot w))
    (funcall a-setter 34)
    (funcall b-setter 56)
    (format t "~A ~A~%" (a-slot w) (b-slot w)))

  (let* ((vec (vector 1 2 3 4))
         (v-setter-maker (make-setf-setter vec))
         (set-v-1 (funcall v-setter-maker 'aref 1))
         (set-v-3 (funcall v-setter-maker 'aref 3)))
    (funcall set-v-1 12)
    (funcall set-v-3 34)
    (format t "~A~%" vec)))
