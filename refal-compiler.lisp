;;;; Refal to Lisp compiler
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.compiler
  (:nicknames :rcomp :rtrans)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal
	:net.paul7.refal.parser
	:net.paul7.refal.matcher)
  (:export compile-program
	   refal-call))

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
	       
(defun compile-clauses (clauses)
  (if (null clauses)
      (constantly t)
      (let ((first (first clauses))
	    (rest (rest clauses)))
	(let ((where (getf first :where))
	      (pattern (getf first :matches)))
	  #'(lambda ()
	      (match-pattern pattern (interpolate where) 
			     (compile-clauses rest)))))))

(defun compile-statement (statement)
  (let ((pattern (getf statement :left))
	(clauses (getf statement :clauses))
	(right (getf statement :right))
	(dict (getf statement :dict)))
    (flet ((do-vars (fn)
	     (loop for var being each hash-value in dict do
		  (funcall fn var)))
	   (compose-right (right)
	     (let ((replace (getf right :replace))
		   (when (getf right :when))
		   (matches (getf right :matches)))
	       (cond (replace 
		      #'(lambda ()
			  (interpolate replace)))
		     ((and when matches)
		      (let ((anon-func (compile-multiple matches)))
			#'(lambda ()
			    (funcall anon-func (interpolate when)))))
		     (t (error "malformed ir"))))))
      (let ((compose (compose-right right)))
	#'(lambda (data)
	    (do-vars #'push-scope)
	    (unwind-protect (if (match-pattern pattern data
					       (compile-clauses clauses))
				(funcall compose))
	      (do-vars #'pop-scope)))))))

(defmethod interpolate ((call refal-funcall))
  (with-accessors ((module module)
		   (name function-name)
		   (argument function-argument)) call
  (let ((actual-arg (interpolate argument)))
    (data (refal-call module name actual-arg)))))
