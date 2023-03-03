(in-package :wx)

(defvar *app-start-func* nil)
(defvar *events-table* (make-hash-table))

(defvar *events-counter* 0)

(cffi:defcallback initialize-wxcl :void ((evt :pointer) (wxcl-event-id :unsigned-int))
  (declare (ignore wxcl-event-id))
  (when *app-start-func*
        (funcall *app-start-func* evt)))  

(defun start-app (init-func)
  (unwind-protect
       (progn 
         (setf *app-start-func* init-func
               *events-counter* 0)
		 (clrhash *events-table*)	   
         (%app-initialize-c (%wx-closure-create (cffi:callback initialize-wxcl) *events-counter*)
                             0 (cffi:null-pointer)))))  

(defun init-func (evt)
  (declare (ignore evt))
  (let ((fr (wx:create-frame8 (cffi:null-pointer) -1 "hello world" 100 100 500 500 wx:+default-frame-style+)))
    ;(print (wx:get-title fr))
    (wx:show fr t)))

(export 'start-app)

