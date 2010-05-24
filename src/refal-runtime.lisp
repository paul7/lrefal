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
       (data->scope (mkvector (apply ,function 
				     (tolist (active ,scope))))))))

(defun tolist (sequence)
  (convert-sequence sequence 'list))

(defmacro defapply-arithmetic (name function)
  `(defapply ,name (compose #'normalize-integer 
			    #'mkvector
			    ,function)))

(defmacro defuncall (name function)
  (with-gensyms (scope) 
    `(defbuiltin ,name (,scope)
       (data->scope (mkvector (funcall ,function 
				       (tolist (active ,scope))))))))

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

(defun compare (data)
  (let ((a (elt data 0))
	(b (elt data 1)))
    (cond ((< a b)
	   #\- )
	  ((> a b)
	   #\+)
	  (t
	   #\=))))
	  
