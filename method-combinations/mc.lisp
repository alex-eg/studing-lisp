(defpackage :mc
  (:use :cl))

(in-package :mc)

(define-method-combination test
    ((primary () :required t)
     (test )))
