;;;; Refal builtin functions & runtime
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.runtime
  (:nicknames :rrt)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal)
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
       (funcall ,function ,scope))))

(reset-module *global*)
 
(defuncall "ident" #'identity)

(defapply "+" #'+)

(defapply "-" #'-)

(defapply "*" #'*)

(defapply "=" #'=)

(defuncall "Prout" #'print)
