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
(defgeneric active-scope (scope))

(defmethod active-scope ((scope refal-scope))
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

(defmethod empty ((scope refal-scope))
  (zerop (length (active-scope scope))))

(defclass refal-funcall ()
  ((module
    :initarg :module
    :initform rtrans::*main*
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
