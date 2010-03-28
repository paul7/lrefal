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
  (error "not implemented yet"))

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

;; bind variable in the data scope given
(defgeneric match-var (var scope &key rematch))

(defun unbind-var (var)
  (setf (bound var) nil)
  (setf (value var) nil))

(defun bind-var (var value)
  (setf (bound var) t)
  (setf (value var) value))

(defun match-size (var scope size)
  (let ((elements (active-scope scope)))
    (if (<= size (length elements))
	(let ((matching (subseq elements 0 size)))
	  (when (appropriate var matching)
	    (bind-var var matching)
	    size)))))

(defmethod match-var ((var refal-t-var) scope &key rematch)
  (if rematch
      nil
      (match-size var scope 1)))

(defmethod match-var ((var refal-e-var) scope &key rematch)
  (let ((size
	 (cond 
	   (rematch (1+ (length (value var))))
	   ((bound var) (length (value var)))
	   (t 0))))
    (if rematch
	(unbind-var var))
    (match-size var scope size)))

;; try and bind vars to match the scope given
;; ATTENTION: needs refactoring
(defun match-pattern (pattern scope 
		      &optional (next-op (constantly t)))
  (let ((first (first pattern))
	(rest (rest pattern))
	(active (active-scope scope)))
    (cond
      ((and (not pattern) (not active)) 
       (funcall next-op))
      (pattern
       (if (consp first)
	   (let ((subexpr (first active)))
	     (flet ((chain-call ()
		      (let ((next-scope 
			     (shift-scope scope 1)))
			(match-pattern rest next-scope next-op))))
	       (if (scopep subexpr)
		   (match-pattern first subexpr #'chain-call))))
	   (do ((bound (bound first))
		(match-var-result
		 (match-var first scope)
		 (match-var first scope :rematch t)))
	       ((not match-var-result)
		(if (not bound)
		    (unbind-var first)))
	     (let ((next-scope 
		    (shift-scope scope match-var-result)))
	       (if (match-pattern rest next-scope next-op)
		   (return t)
		   (if bound 
		       (return nil)))))))
      (t nil))))

(defun match-subexpr (pattern scope)
  (match-pattern pattern scope))

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
      (when (or (match-pattern pattern scope) nil)
	(loop for var being each hash-value in dict do
	     (format t "~a~%" var))
	t))))
