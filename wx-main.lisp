(cl:when (not (cl:find-package "WX"))
  (cl:make-package "WX")
  (cl:use-package 'cl 'wx))

(cl:in-package :wx)

(import 'cxx::cxx-ptr)

(progn
 (defclass frame nil
           ((cxx::cxx-class-ptr :accessor cxx-ptr :initarg :cxx-ptr :initform
             (cxx::required "Use Class constructor function.")))
           (:documentation "Cxx class stored in lisp"))
 (defparameter *destruct-ptr-frame* nil)
 (defun destruct-ptr-frame (cxx::class-ptr)
   "delete class pointer"
   (if (not (cffi-sys:null-pointer-p cxx::class-ptr))
       (cffi:foreign-funcall-pointer *destruct-ptr-frame* nil :pointer
                                     cxx::class-ptr :void)))
 nil) 
(defconstant +default-frame-style+ 541072960) 
(defconstant +iconize+ 16384) 
(defconstant +caption+ 536870912) 
(defconstant +minimize+ 16384) 
(defconstant +minimize-box+ 1024) 
(defconstant +maximize+ 8192) 
(defconstant +maximize-box+ 512) 
(defconstant +close-box+ 4096) 
(defconstant +stay-on-top+ 32768) 
(defconstant +system-menu+ 2048) 
(defconstant +resize-border+ 64) 
(defconstant +frame-tool-window+ 4) 
(defconstant +frame-no-taskbar+ 2) 
(defconstant +frame-float-on-parent+ 8) 
(defconstant +frame-shaped+ 16) 
(progn
 (defparameter *%wx-evt-handler-connect-func-ptr* nil)
 (defun %wx-evt-handler-connect (v0 v1 v2 v3 v4)
   (cffi:foreign-funcall-pointer *%wx-evt-handler-connect-func-ptr* nil
                                 :pointer v0 :int32 v1 :int32 v2 :int32 v3
                                 :pointer v4 :int32))) 
(progn
 (defparameter *%wx-evt-handler-bind-func-ptr* nil)
 (defun %wx-evt-handler-bind (v0 v1 v2 v3 v4)
   (cffi:foreign-funcall-pointer *%wx-evt-handler-bind-func-ptr* nil :pointer
                                 v0 :int32 v1 :int32 v2 :int32 v3 :pointer v4
                                 :void))) 
(progn
 (defparameter *%wx-evt-handler-get-closure-func-ptr* nil)
 (defun %wx-evt-handler-get-closure (v0 v1 v2)
   (cffi:foreign-funcall-pointer *%wx-evt-handler-get-closure-func-ptr* nil
                                 :pointer v0 :int32 v1 :int32 v2 :pointer))) 
(progn
 (defparameter *%wx-closure-create-func-ptr* nil)
 (defun %wx-closure-create (v0 v1)
   (cffi:foreign-funcall-pointer *%wx-closure-create-func-ptr* nil :pointer v0
                                 :uint32 v1 :pointer))) 
(progn
 (defparameter *%wx-evt-handler-get-client-closure-func-ptr* nil)
 (defun %wx-evt-handler-get-client-closure (v0)
   (cffi:foreign-funcall-pointer *%wx-evt-handler-get-client-closure-func-ptr*
                                 nil :pointer v0 :pointer))) 
(progn
 (defparameter *%wx-object-set-client-closure-func-ptr* nil)
 (defun %wx-object-set-client-closure (v0 v1)
   (cffi:foreign-funcall-pointer *%wx-object-set-client-closure-func-ptr* nil
                                 :pointer v0 :pointer v1 :void))) 
(progn
 (defparameter *app-get-idle-interval-func-ptr* nil)
 (defun app-get-idle-interval ()
   (cffi:foreign-funcall-pointer *app-get-idle-interval-func-ptr* nil :int32))) 
(progn
 (defparameter *app-set-idle-interval-func-ptr* nil)
 (defun app-set-idle-interval (v0)
   (cffi:foreign-funcall-pointer *app-set-idle-interval-func-ptr* nil :int32 v0
                                 :void))) 
(progn
 (defparameter *%app-initialize-c-func-ptr* nil)
 (defun %app-initialize-c (v0 v1 v2)
   (cffi:foreign-funcall-pointer *%app-initialize-c-func-ptr* nil :pointer v0
                                 :int32 v1 :pointer v2 :void))) 
(progn
 (defparameter *%app-initialize-func-ptr* nil)
 (defun %app-initialize (v0 v1 v2 v3)
   (cffi:foreign-funcall-pointer *%app-initialize-func-ptr* nil :pointer v0
                                 :pointer v1 :int32 v2 :pointer v3 :void))) 
(progn
 (defparameter *set-title-func-ptr* nil)
 (defmethod set-title ((cxx::obj frame) v0)
   (cffi:foreign-funcall-pointer *set-title-func-ptr* nil :pointer
                                 (cxx-ptr cxx::obj) :string v0 :void))) 
(progn
 (defparameter *get-title-func-ptr* nil)
 (defmethod get-title ((cxx::obj frame))
   (let* ((cxx::val
           (cffi:foreign-funcall-pointer *get-title-func-ptr* nil :pointer
                                         (cxx-ptr cxx::obj) :string+ptr))
          (cxx::str (car cxx::val))
          (cxx::ptr (cadr cxx::val)))
     (trivial-garbage:finalize cxx::str
                               (lambda () (cxx:destruct-string cxx::ptr)))
     cxx::str))) 
(progn
 (defparameter *show-func-ptr* nil)
 (defmethod show ((cxx::obj frame) v0)
   (cffi:foreign-funcall-pointer *show-func-ptr* nil :pointer
                                 (cxx-ptr cxx::obj) :bool v0 :bool))) 
(progn
 (defparameter *create-frame8-func-ptr* nil)
 (defun create-frame8
        (v0 v1 v2 v3 v4 v5 v6 v7 &optional (class nil) &rest rest)
   (let* ((cxx::ptr
           (cffi:foreign-funcall-pointer *create-frame8-func-ptr* nil :pointer
                                         v0 :int32 v1 :string v2 :int32 v3
                                         :int32 v4 :int32 v5 :int32 v6 :int32
                                         v7 :pointer))
          (cxx::initargs (append '(:cxx-ptr) (list cxx::ptr)))
          (cxx::ename-class
           (if class
               class
               'frame)))
     (when rest (setf cxx::initargs (append rest cxx::initargs)))
     (let ((cxx::obj
            (handler-case
             (apply #'make-instance cxx::ename-class (values cxx::initargs))
             (error (cxx::err) (destruct-ptr-frame cxx::ptr)
                    (error cxx::err)))))
       (trivial-garbage:finalize cxx::obj
                                 (lambda () (destruct-ptr-frame cxx::ptr)))
       cxx::obj)))) 
(progn
 (defparameter *create-frame0-func-ptr* nil)
 (defun create-frame0 (&optional (class nil) &rest rest)
   (let* ((cxx::ptr
           (cffi:foreign-funcall-pointer *create-frame0-func-ptr* nil
                                         :pointer))
          (cxx::initargs (append '(:cxx-ptr) (list cxx::ptr)))
          (cxx::ename-class
           (if class
               class
               'frame)))
     (when rest (setf cxx::initargs (append rest cxx::initargs)))
     (let ((cxx::obj
            (handler-case
             (apply #'make-instance cxx::ename-class (values cxx::initargs))
             (error (cxx::err) (destruct-ptr-frame cxx::ptr)
                    (error cxx::err)))))
       (trivial-garbage:finalize cxx::obj
                                 (lambda () (destruct-ptr-frame cxx::ptr)))
       cxx::obj)))) 

(export '(cxx-ptr frame +default-frame-style+ +iconize+ +caption+ +minimize+
                  +minimize-box+ +maximize+ +maximize-box+ +close-box+
                  +stay-on-top+ +system-menu+ +resize-border+
                  +frame-tool-window+ +frame-no-taskbar+
                  +frame-float-on-parent+ +frame-shaped+ app-get-idle-interval
                  app-set-idle-interval set-title get-title show create-frame8
                  create-frame0))