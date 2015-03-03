(eval-when (:compile-toplevel)
  (progn
    (asdf:load-system :cffi)
    (defpackage #:cffi-curl-test
      (:use #:cl
            #:cffi)
      (:export #:test-curl))))

(in-package #:cffi-curl-test)

(define-foreign-library libcurl
  (:unix "libcurl.so"))

(use-foreign-library libcurl)

(defctype curl-code :int)
(defctype easy-handle :pointer)
(defctype size :unsigned-int)

(defparameter *easy-handle* nil)
(defvar *easy-handle-cstrings* (make-hash-table))
(defvar *easy-handle-errorbuffers* (make-hash-table))
(defparameter *curl-error-size* 257)

(defcfun "curl_global_init" curl-code
  (flags :long))

(defcfun "curl_easy_init" :pointer)

(defcfun "curl_easy_cleanup" :void
  (easy-handle :pointer))

(defcfun "curl_easy_perform" curl-code
  (handle easy-handle))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-curl-handle-cstring (handle cstring)
    (car (push cstring (gethash handle *easy-handle-cstrings*))))

  (defun curry-curl-option-setter (fun-name opt-kw)
    (setf (symbol-function fun-name)
          (let ((c-fun (symbol-function fun-name)))
            (lambda (easy-handle new-value)
              (funcall c-fun easy-handle opt-kw
                       (if (stringp new-value)
                           (add-curl-handle-cstring
                            easy-handle
                            (foreign-string-alloc new-value))
                           new-value)))))))

(defmacro define-curl-option-setter (name option-type
                                     option-value foreign-type)
  `(progn
     (defcfun ("curl_easy_setopt" ,name) curl-code
       (easy-handle easy-handle)
       (option ,option-type)
       (new-value ,foreign-type))
     (curry-curl-option-setter ',name ',option-value)))


(defmacro define-curl-options (type-name type-offsets &rest enum-args)
  (flet ((enumerated-value (type offset)
           (+ (getf type-offsets type) offset))
         (map-enum-args (procedure)
           (mapcar (lambda (arg) (apply procedure arg)) enum-args))
         (make-setter-name (option-name)
           (intern (concatenate
                    'string "SET-" (symbol-name type-name)
                    "-" (symbol-name option-name)))))
    `(progn
       (defcenum ,type-name
         ,@(map-enum-args
            (lambda (name type number)
              (list name (enumerated-value type number)))))
       ,@(map-enum-args
          (lambda (name type number)
            (declare (ignore number))
            `(define-curl-option-setter ,(make-setter-name name)
                 ,type-name ,name ,(ecase type
                                          (long :long)
                                          (objectpoint :pointer)
                                          (functionpoint :pointer)
                                          (off-t :long)))))
       ',type-name)))

(defun free-easy-handle (handle)
  (curl-easy-cleanup handle)
  (foreign-free (gethash handle *easy-handle-errorbuffers*))
  (mapc #'foreign-string-free
        (gethash handle *easy-handle-cstrings*))
  (remhash handle *easy-handle-cstrings*)
  (remhash handle *easy-handle-errorbuffers*)
  (setf handle nil))

(defun get-easy-handle-error (handle)
  (foreign-string-to-lisp
   (gethash handle *easy-handle-errorbuffers*)))

(eval-when (:compile-toplevel)
  (define-curl-options curl-option
      (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
    (:noprogress long 43)
    (:nosignal long 99)
    (:errorbuffer objectpoint 10)
    (:url objectpoint 2)
    (:writefunction functionpoint 11)))

(defun make-easy-handle ()
  (let ((easy-handle (curl-easy-init)))
    (setf (gethash easy-handle *easy-handle-cstrings*) '())
    (setf (gethash easy-handle *easy-handle-errorbuffers*)
          (foreign-alloc :char
                         :count *curl-error-size*
                         :initial-element 0))
    (set-curl-option-errorbuffer easy-handle
                                 (gethash
                                  easy-handle
                                  *easy-handle-errorbuffers*))
    easy-handle))

(defun test-curl (url)
  (setf *easy-handle* (make-easy-handle))
  (set-curl-option-nosignal *easy-handle* 1)
  (set-curl-option-url *easy-handle* url)
  (set-curl-option-writefunction *easy-handle*
                                 (callback easy-write))
  (with-output-to-string (contents)
    (let ((*easy-write-procedure*
           (lambda (string)
             (write-string string contents))))
      (declare (special *easy-write-procedure*))
      (foreign-string-to-lisp (car (gethash *easy-handle*
                                            *easy-handle-cstrings*)))
      (curl-easy-perform *easy-handle*)
      (format t "Error: ~s~%" (get-easy-handle-error *easy-handle*))
      (free-easy-handle *easy-handle*)
      contents)))
