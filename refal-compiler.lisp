;;;; Refal to Lisp compiler
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.compiler
  (:nicknames :rcomp :rtrans)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal
	:net.paul7.refal.parser2
	:net.paul7.refal.matcher)
  (:export))

(in-package :net.paul7.refal.compiler)

;;; test functions

(defun compile-program (module source)
  (let ((functions (string->program source)))
    (mapc #'(lambda (fun)
	      (compile-function module fun))
	  functions)))

(defun compile-function (module function-info)
  (let ((fname (getf function-info :fname))
	(statements (getf function-info :statements)))
    (setf (refal-entry module fname) (compile-multiple statements))))

(defun refal-call (module fname &optional (data (string->scope "")))
  (funcall (refal-entry module fname) data))

;; compile simple statement
(defun compile-multiple (statements)
  (let ((compiled-statements 
	 (mapcar #'(lambda (statement)
		     (funcall #'compile-statement statement))
		 statements)))
    (labels ((compiled (code data)
	       (if code
		   (or (funcall (first code) data)
		       (compiled (rest code) data))
		   (error "no match"))))
      #'(lambda (data)
	  (compiled compiled-statements data)))))
	       
(defun compile-statement (statement)
  (let ((pattern (getf statement :left))
	(construct (getf statement :right))
	(dict (getf statement :dict)))
    (flet ((do-vars (fn)
	     (loop for var being each hash-value in dict do
		  (funcall fn var))))
      #'(lambda (data)
	  (do-vars #'push-scope)
	  (unwind-protect (if (match-pattern pattern data)
			      (interpolate construct))
	    (do-vars #'pop-scope))))))

(defmethod interpolate ((call refal-funcall))
  (with-accessors ((module module)
		   (name function-name)
		   (argument function-argument)) call
  (let ((actual-arg (interpolate argument)))
    (data (refal-call module name actual-arg)))))
