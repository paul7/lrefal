;;;; Refal internal data representation
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.internal
  (:nicknames :ir)
  (:use :common-lisp
	:net.paul7.utility)
  (:export shift-scope
	   active-scope
	   scopep
	   empty
	   bound
	   value
	   data
	   make-uniform-type
	   make-var
	   refal-var
	   refal-t-var
	   refal-s-var
	   refal-e-var
	   refal-scope
	   refal-pattern
	   refal-funcall
	   function-name
	   function-argument
	   module
	   bind-var
	   unbind-var
	   push-scope
	   pop-scope
	   var-type
	   refal-module
	   reset-module
	   function-dict
	   module-name
	   refal-function
	   *main*
	   *global*
	   s
	   t
	   e))

(in-package :net.paul7.refal.internal)

;;; Refal variable hierarchy

(defclass refal-var ()
  ((name
    :initform 0
    :initarg :name
    :accessor name)
   (value
    :initform nil
    :initarg :value
    :accessor value)
   (bound 
    :initform nil
    :initarg bound
    :accessor bound)
   (value-stack
    :initform nil
    :accessor value-stack)
   (bound-stack
    :initform nil
    :accessor bound-stack)))

;; e.X
;; can be bound to anything
(defclass refal-e-var (refal-var) 
  ())

;; t.X
;; can be bound to atom or to parenthesized subexpression
(defclass refal-t-var (refal-var)
  ())

;; s.X
;; can be bound to atom only
(defclass refal-s-var (refal-t-var)
  ())

(defmethod initialize-instance :after ((var refal-var) &key)
  (with-accessors ((value value) 
		   (bound bound)) var
    (setf bound value)))

(defun push-scope (var)
  (with-accessors ((value value)
		   (bound bound)
		   (value-stack value-stack)
		   (bound-stack bound-stack)) var
    (push value value-stack)
    (push bound bound-stack)
    (unbind-var var)
    var))

(defun pop-scope (var)
  (with-accessors ((value value)
		   (bound bound)
		   (value-stack value-stack)
		   (bound-stack bound-stack)) var
    (setf value (pop value-stack))
    (setf bound (pop bound-stack))
    var))

;; return symbol corresponding to type
;; can be used to construct fresh variable of the same type
(defgeneric var-type (var))

(defmethod var-type ((var refal-e-var))
  'e)

(defmethod var-type ((var refal-t-var))
  't)

(defmethod var-type ((var refal-s-var))
  's)

;; make unbound Refal variable of given type and name
(defun make-uniform-type (type)
  (case type
    (#\s 's)
    (#\S 's)
    (s 's)
    (#\t 't)
    (#\T 't)
    (t 't)
    (#\e 'e)
    (#\E 'e)
    (e 'e)
    (otherwise  (error "Bad type"))))
  
(defun make-var (type name)
  (make-instance (case (make-uniform-type type)
		   (s 'refal-s-var)
		   (t 'refal-t-var)
		   (e 'refal-e-var))
		 :name name))    

;;; utilities for variables
(defmethod print-object ((var refal-var) stream)
  (print-unreadable-object (var stream)
    (if (bound var)
	(format stream "~a.~a => ~a" 
		(var-type var)
		(name var)
		(value var))
	(format stream "~a.~a"
		(var-type var)
		(name var)))))

(defun unbind-var (var)
  (setf (bound var) nil)
  (setf (value var) nil))

(defun bind-var (var value)
  (setf (bound var) t)
  (setf (value var) value))

;;; this class represents Refal data
;;; encapsulates list of atoms constituting scope
;;; and boundaries of scope yet unmatched
(defclass refal-scope ()
  ((start
    :accessor start
    :initarg :start
    :initform 0)
   (end
    :accessor end
    :initarg :end)
   (data
    :accessor data
    :initarg :data
    :initform nil)))

(defclass refal-pattern (refal-scope) 
  ())

(defmethod initialize-instance :after ((scope refal-scope) &key)
  (with-accessors ((end end) 
		   (data data)) scope
    (setf end (length data))))

(defgeneric scopep (obj))

(defmethod scopep ((obj t))
  nil)

(defmethod scopep ((obj refal-scope))
  t)

;; return list representing unmatched part of the scope
(defun active-scope (scope)
  (with-accessors ((start start)
		   (end end)
		   (data data)) scope
    (subseq data start end)))

;; promote scope boundaries after successful matching
(defun shift-scope (scope margin)
  (make-instance 'refal-scope 
		 :data (data scope)
		 :start (+ (start scope) margin)
		 :end (end scope)))

(defmethod print-object ((scope refal-scope) stream)
  (print-unreadable-object (scope stream)
    (format stream "~{~a ~}" (data scope))))

(defun empty (scope)
  (zerop (length (active-scope scope))))

(defclass refal-module ()
  ((function-dict
    :initform (make-hash-table :test #'equalp)
    :accessor function-dict)
   (module-name
    :initform 'main
    :initarg :module-name
    :accessor module-name)))

(defparameter *main* (make-instance 'refal-module))
(defparameter *global* (make-instance 'refal-module 
				      :module-name "$$global"))

(defun reset-module (module)
  (with-accessors ((dict function-dict)) module
    (setf dict (make-hash-table :test #'equalp))))

(defmethod print-object ((module refal-module) stream)
  (print-unreadable-object (module stream :type t :identity t)
    (format stream "~{~a ~}" 
	    (loop 
	       for name 
	       being each hash-key in (function-dict module)
	       collect name))))

(defclass refal-funcall ()
  ((module
    :initarg :module
    :initform *main*
    :accessor module)
   (function-name
    :initarg :function-name
    :initform (error "No function name specified")
    :accessor function-name)
   (function-argument
    :initarg :function-argument
    :initform (make-instance 'refal-pattern 
			     :data nil)
    :accessor function-argument)))

(defun refal-function (module fname)
  (with-accessors ((dict function-dict) 
		   (name module-name)) module
    (let ((func (gethash fname dict)))
      (or func
	  (if (equalp name "$$global")
	      nil
	      (let ((func (refal-function *global* fname)))
		(or func
		    (error (format nil "no function ~a in module ~a" 
				   fname module)))))))))

(defmethod (setf refal-function) (code module fname)
  (with-accessors ((dict function-dict) 
		   (name module-name)) module
    (if (gethash fname dict)
	(warn (format nil "duplicate function ~a in module ~a" 
		      fname module)))
    (setf (gethash fname dict) code)))
