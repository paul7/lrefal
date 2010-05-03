;;;; Refal builtin functions & runtime
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.runtime
  (:nicknames :rrt)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal
	:net.paul7.refal.parser)
  (:export))

(in-package :net.paul7.refal.runtime)

;; define & register builtin function
(defmacro defbuiltin (name (scope)
		      &body body)
  `(setf (refal-entry *global* ,name)
	 #'(lambda (,scope)
	     ,@body)))
	    
(defmacro defapply (name function)
  (with-gensyms (scope)
    `(defbuiltin ,name (,scope)
       (data->scope (mklist (apply ,function (data ,scope)))))))

(defmacro defuncall (name function)
  (with-gensyms (scope) 
    `(defbuiltin ,name (,scope)
       (data->scope (mklist (funcall ,function (data ,scope)))))))

(reset-module *global*)
 
(defuncall "ident" #'identity)

(defapply "+" #'+)

(defapply "-" #'-)

(defapply "*" #'*)

(defapply "=" #'=)

(defuncall "Prout" #'prout)

(defuncall "Print" #'print-return)

(defbuiltin "Card" (scope) 
  (declare (ignore scope))
  (string->scope (read-line)))

(defgeneric prout (object))

(defmethod prout ((basic-object t))
  (format t "~a" basic-object))

(defmethod prout ((list cons))
  (dolist (each list)
    (prout each)
    (format t " ")))

(defmethod prout ((scope refal-scope))
  (format t "(")
  (prout (data scope))
  (format t ")"))

(defun print-return (object)
  (prout object)
  object)
