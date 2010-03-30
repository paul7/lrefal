;;;; Refal expression matcher engine
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.matcher
  (:nicknames :rmatch)
  (:use :common-lisp 
	:net.paul7.refal.parser
	:net.paul7.utility))

(in-package :net.paul7.refal.matcher)

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
    :accessor bound)))

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
(defun make-var (type name)
  (make-instance (case type
		   (s 'refal-s-var)
		   (t 'refal-t-var)
		   (e 'refal-e-var)
		   (otherwise  (error "Bad type")))
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

;; check if value is appropriate to be bound to var, i.e.
;; 1) check data type 
;; 2) make sure there's no conflict with already bound value
(defgeneric appropriate (var value)
  (:method-combination and))

(defmethod appropriate and ((var refal-var) value)
  (with-accessors ((oldvalue value)) var
    (or (not (bound var))
	(equal value oldvalue))))

(defmethod appropriate and ((var refal-t-var) value)
  (single value))

(defmethod appropriate and ((var refal-s-var) value)
  (and (consp value)
       (not (consp (first value)))
       (not (scopep (first value)))))

(defmethod appropriate and ((var-list list) value)
  (scopep value))

;; chomp size elements of the scope
;; try to bind var, if appropriate
(defun match-size (var scope size)
  (let ((elements (active-scope scope)))
    (if (<= size (length elements))
	(let ((matching (subseq elements 0 size)))
	  (when (appropriate var matching)
	    (bind-var var matching)
	    size)))))

;; bind variable in the data scope given
;; continue with the rest on success
(defgeneric match-var (first rest scope 
			     &optional next-op))

(defmethod match-var ((first refal-t-var) rest scope
		      &optional (next-op (constantly t)))
  (if (match-size first scope 1)
      (match-pattern rest (shift-scope scope 1) next-op)))

(defmethod match-var ((first refal-e-var) rest scope
		       &optional (next-op (constantly t)))
  (let ((bound (bound first)))
    (if bound
	(let ((size (length (value first))))
	  (if (match-size first scope size)
	      (match-pattern rest 
			     (shift-scope scope size)
			     next-op)))
	(do ((size 0 (1+ size)))
	    ((not (match-size first scope size))
	     (unbind-var first))
	  (if (match-pattern rest 
			     (shift-scope scope size)
			     next-op)
	      (return t)
	      (unbind-var first))))))

(defmethod match-var ((first list) rest scope
		       &optional (next-op (constantly t)))
  (let ((subexpr (first (active-scope scope))))
    (if (appropriate first subexpr)
	(flet ((chain-call ()
		 (match-pattern rest (shift-scope scope 1) 
				next-op)))
	  (match-pattern first subexpr #'chain-call)))))

;; try and bind vars to match the scope given
(defun match-pattern (pattern scope
		       &optional (next-op (constantly t)))
  (let ((first (first pattern))
	(rest (rest pattern))
	(active (active-scope scope)))
    (cond 
      ((and (not pattern) (not active))
       (funcall next-op))
      (pattern
       (match-var first rest scope next-op))
      (t nil))))

;;; pattern fiddling
(defun make-pattern (specs 
		     &optional (dict (make-hash-table)))
  (labels ((add-var (type name)
	     (let ((old-var (gethash name dict)))
	       (cond
		 ((not old-var) 
		  (setf (gethash name dict) (make-var type name)))
		 ((eq (var-type old-var) type) old-var)
		 (t (error "type mismatch")))))
	   (add-var-from-spec (spec)
	     (if (atom (first spec))
		 (add-var (first spec) (second spec))
		 (make-pattern spec dict))))
    (values (mapcar #'add-var-from-spec specs)
	    dict)))

;;; testing
(defun ref-test (pattern-spec string)
  (multiple-value-bind (pattern dict) 
      (make-pattern pattern-spec)
    (let ((scope (make-scope string)))
      (when (match-pattern pattern scope)
	(loop for var being each hash-value in dict do
	     (format t "~a~%" var))
	t))))
