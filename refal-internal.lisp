;;;; Refal internal data representation
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.internal
  (:nicknames :ir)
  (:use :common-lisp
	:net.paul7.utility)
  (:export scope-size
	   subscope
	   active
	   scope=
	   refal-scope
	   refal-scope-data
	   scope-first
	   scopep
	   empty
	   copy-scope-data
	   bound
	   value
	   make-uniform-type
	   make-var
	   refal-var
	   refal-t-var
	   refal-s-var
	   refal-e-var
	   refal-funcall
	   function-name
	   function-argument
	   module
	   bind-var
	   unbind-var
	   push-scope
	   pop-scope
	   data->scope 
	   data->pattern
	   refal-nil
	   var-type
	   refal-module
	   reset-module
	   function-dict
	   module-name
	   refal-entry
	   interpolate
	   normalize-integer
	   lispify-integer
	   *main*
	   *global*))

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
  #\e)

(defmethod var-type ((var refal-t-var))
  #\t)

(defmethod var-type ((var refal-s-var))
  #\s)

;; make unbound Refal variable of given type and name
(defun make-uniform-type (type)
  (case type
    (#\s #\s)
    (#\S #\s)
    (#\t #\t)
    (#\T #\t)
    (#\e #\e)
    (#\E #\e)
    (otherwise  (error "Bad type"))))
  
(defun make-var (type name)
  (make-instance (case (make-uniform-type type)
		   (#\s 'refal-s-var)
		   (#\t 'refal-t-var)
		   (#\e 'refal-e-var))
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

(defstruct refal-scope
  data start end)

(defmacro make-scope (data size)
  `(make-refal-scope :data ,data :start 0 :end ,size))

(defmacro active (scope)
  `(refal-scope-data ,scope))

(defmacro scope-size (scope)
  `(- (refal-scope-end ,scope) (refal-scope-start ,scope)))

(defmacro scope-first (scope)
  `(elt (refal-scope-data ,scope) (refal-scope-start ,scope)))

(defun scopep (obj)
  (typecase obj
    (refal-scope t)
    (t nil)))

(defun subscope (scope &key (shift 0) length reuse)
  (let ((data (refal-scope-data scope))
	(start (refal-scope-start scope))
	(end (refal-scope-end scope)))
    (let* ((new-start (min (+ start shift) end))
	   (new-end (if length
			(min (+ new-start length) end)
			end)))
      (if reuse
	  (progn 
	    (setf (refal-scope-start scope) new-start)
	    (setf (refal-scope-end scope) new-end)
	    scope)
	  (make-refal-scope :data data :start new-start :end new-end)))))

(defun copy-scope-data (scope)
  (subseq (refal-scope-data scope) 
	  (refal-scope-start scope) 
	  (refal-scope-end scope)))

(defun scope= (scope1 scope2)
  (and (= (scope-size scope1) (scope-size scope2))
       (do ((i (refal-scope-start scope1) (1+ i))
	    (j (refal-scope-start scope2) (1+ j))
	    (data1 (refal-scope-data scope1))
	    (data2 (refal-scope-data scope2))
	    (stop (refal-scope-end scope1)))
	   ((= i stop) t)
	 (if (not (equal (elt data1 i)
			 (elt data2 j)))
	     (return nil)))))

(defun empty (scope)
  (zerop (scope-size scope)))

(defun data->scope (data)
  (let ((vec (convert-sequence data 'vector)))
    (make-scope vec (length vec))))

(defun data->pattern (data)
  (data->scope data))

(defun refal-nil ()
  (data->scope nil))

(defclass refal-module ()
  ((function-dict
    :initform (make-hash-table :test #'equalp)
    :accessor function-dict)
   (module-name
    :initform 'main
    :initarg :module-name
    :accessor module-name)))

(defvar *main* (make-instance 'refal-module))
(defvar *global* (make-instance 'refal-module 
				      :module-name "$$global"))

(defun reset-module (module)
  (with-accessors ((dict function-dict)) module
    (setf dict (make-hash-table :test #'equalp))))

(defun refal-entry (module fname)
  (with-accessors ((dict function-dict) 
		   (name module-name)) module
    (let ((func (gethash fname dict)))
      (or func
	  (if (equalp name "$$global")
	      nil
	      (let ((func (refal-entry *global* fname)))
		(or func
		    (error (format nil "no function ~a in module ~a" 
				   fname module)))))))))

(defmethod (setf refal-entry) (code module fname)
  (with-accessors ((dict function-dict) 
		   (name module-name)) module
    (if (gethash fname dict)
	(warn (format nil "duplicate function ~a in module ~a" 
		      fname module)))
    (setf (gethash fname dict) code)))

(defmethod print-object ((module refal-module) stream)
  (print-unreadable-object (module stream :identity t)
    (format stream "~a ~a" 
	    (module-name module)
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
    :initform (data->pattern nil)
    :accessor function-argument)))

(defgeneric interpolate (object))

(defmethod interpolate ((var refal-var))
  (if (bound var)
      (copy-scope-data (value var))
      (error (format nil "~a is unbound" var))))
  
(defmethod interpolate ((pattern refal-scope))
  (data->scope (apply #'concatenate 'vector 
		      (map 'list (compose #'mkvector #'interpolate)
			   (refal-scope-data pattern)))))

(defun normalize-integer (data)
  (let ((int (elt data 0)))
    (if (< int 0)
	(vector #\- (abs int))
	(vector int))))

(defun lispify-integer (data)
  (if (eql #\- (elt data 0))
      (- (elt data 1))
      (elt data 0)))
