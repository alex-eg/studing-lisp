(defpackage #:cffi-curl-test
  (:use #:cl
        #:cffi
        #:asdf))

(in-package #:cffi-curl-test)

(asdf:load-system :cffi)

(use-package :cffi)

(define-foreign-library libcurl
  (:unix "libcurl.so"))

(use-foreign-library libcurl)

;; CURLcode curl_global_init(long flags);
(defctype curl-code :int)
(defcfun "curl_global_init" curl-code
  (flags :long))

;; CURL *curl_easy_init( );
(defcfun "curl_easy_init" :pointer)

;; void curl_easy_cleanup(CURL *handle);
(defcfun "curl_easy_cleanup" :void
  (easy-handle :pointer))

(defparameter *easy-handle* (curl-easy-init))

;; CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...);
;;
;; #define CURLOPTTYPE_LONG          0
;; #define CURLOPTTYPE_OBJECTPOINT   10000
;; #define CURLOPTTYPE_FUNCTIONPOINT 20000
;; #define CURLOPTTYPE_OFF_T         30000
;;
;; #define LONG          CURLOPTTYPE_LONG
;; #define OBJECTPOINT   CURLOPTTYPE_OBJECTPOINT
;; #define FUNCTIONPOINT CURLOPTTYPE_FUNCTIONPOINT
;; #define OFF_T         CURLOPTTYPE_OFF_T
;; #define CINIT(name,type,number) CURLOPT_/**/name = type + number
;; e.g.
;; /* "user:password" to use with proxy. */
;; CINIT(PROXYUSERPWD, OBJECTPOINT, 6),
;; /* Range to get, specified as an ASCII string. */
;; CINIT(RANGE, OBJECTPOINT, 7),

(defctype easy-handle :pointer)

(defmacro curl-easy-setopt (easy-handle enumerated-name
                            value-type new-value)
  `(foreign-funcall "curl_easy_setopt"
                    easy-handle ,easy-handle
                    curl-option ,enumerated-name
                    ,value-type ,new-value
                    curl-code))

(defun curry-curl-option-setter (fun-name opt-kw)
  (setf (symbol-function fun-name)
        (let ((c-fun (symbol-function fun-name)))
          (lambda (easy-handle new-value)
            (funcall c-fun easy-handle opt-kw new-value)))))

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

(define-curl-options curl-option
    (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
  (:noprogress long 43)
  (:nosignal long 99)
  (:errorbuffer objectpoint 10)
  (:url objectpoint 2))

(defvar *easy-handle-cstrings* (make-hash-table))

(defun make-easy-handle ()
  (let ((easy-handle (curl-easy-init)))
    (setf (gethash easy-handle *easy-handle-cstrings*) '())
    easy-handle))

(defun free-easy-handle (handle)
  (curl-easy-cleanup handle)
  (mapc #'foreign-string-free))

