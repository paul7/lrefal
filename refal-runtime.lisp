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
       (data->scope (mklist (apply ,function (active ,scope)))))))

(defmacro defapply-arithmetic (name function)
  `(defapply ,name (compose #'normalize-integer #'mklist ,function)))

(defmacro defuncall (name function)
  (with-gensyms (scope) 
    `(defbuiltin ,name (,scope)
       (data->scope (mklist (funcall ,function (active ,scope)))))))

(reset-module *global*)
 
(defuncall "ident" #'identity)

(defapply-arithmetic "+" #'+)

(defapply-arithmetic "-" #'-)

(defapply-arithmetic "*" #'*)

(defapply-arithmetic "=" #'=)

(defuncall "Prout" #'prout)

(defuncall "Print" #'print-return)

(defuncall "Compare" #'compare)

(defbuiltin "Card" (scope) 
  (declare (ignore scope))
  (string->scope (read-line)))

(defbuiltin "Nl" (scope)
  (declare (ignore scope))
  (format t "~%")
  (refal-nil))

(defgeneric prout (object))

(defmethod prout ((basic-object t))
  (format t "~a " basic-object))

(defmethod prout ((char character))
  (format t "~a" char))

(defmethod prout ((list cons))
  (dolist (each list)
    (prout each)))

(defun print-return (object)
  (prout object)
  object)

(defun compare (list)
  (let ((a (first list))
	(b (second list)))
    (cond ((< a b)
	   #\- )
	  ((> a b)
	   #\+)
	  (t
	   #\=))))
	  
