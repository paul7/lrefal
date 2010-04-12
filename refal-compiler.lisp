;;;; Refal to Lisp compiler
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.compiler
  (:nicknames :rcomp :rtrans)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal
	:net.paul7.refal.parser
	:net.paul7.refal.matcher)
  (:export))

(in-package :net.paul7.refal.compiler)

;;; test functions

(defclass refal-module ()
  ((function-dict
    :initform (make-hash-table :test #'equalp)
    :accessor function-dict)
   (module-name
    :initform 'main
    :initarg :module-name
    :accessor module-name)))

(defun reset-module (module)
  (with-accessors ((dict function-dict)) module
    (setf dict (make-hash-table :test #'equalp))))

(defun compile-function (module fname statements)
  (with-accessors ((dict function-dict) 
		   (name module-name)) module
    (if (gethash fname dict)
	(error (format nil "duplicate function ~a in module ~a" fname module))
    (setf (gethash fname dict) (compile-multiple statements)))))

(defun refal-call (module fname data)
  (with-accessors ((dict function-dict) 
		   (name module-name)) module
    (let ((func (gethash fname dict)))
      (if func
	  (funcall func data)
	  (error (format nil "no function ~a in module ~a" fname module))))))

;; compile simple statement
(defun compile-multiple (statements)
  (let ((compiled-statements 
	 (mapcar #'(lambda (statement)
		     (apply #'compile-statement statement))
		 statements)))
    (labels ((compiled (code data)
	       (if code
		   (or (funcall (first code) data)
		       (compiled (rest code) data)))))
      #'(lambda (data)
	  (compiled compiled-statements data)))))
	       
(defun compile-statement (left right)
  (multiple-value-bind (pattern dict) (string->pattern left)
    (let ((construct (string->pattern right dict)))
      (flet ((unbind-all ()
	       (loop for var being each hash-value in dict do
		    (unbind-var var))))
	#'(lambda (data)
	    (unbind-all)
	    (if (match-pattern pattern data)
		(interpolate construct)))))))

(defun ref-test (left right data)
  (funcall (compile-statement left right) (string->scope data)))

(defclass refal-funcall ()
  ((module
    :initarg :module
    :initform (error "No module specified")
    :accessor module)
   (function-name
    :initarg :function-name
    :initform (error "No function name specified")
    :accessor function-name)
   (function-argument
    :initarg :function-argument
    :initform (string->pattern "")
    :accessor function-argument)))

(defmethod interpolate ((call refal-funcall))
  (with-accessors ((module module)
		   (name function-name)
		   (argument function-argument)) call
  (let ((actual-arg (interpolate argument)))
    (data (refal-call module name actual-arg)))))
