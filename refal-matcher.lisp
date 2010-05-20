;;;; Refal expression matcher engine
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.matcher
  (:nicknames :rmatch)
  (:use :common-lisp 
	:net.paul7.refal.internal
	:net.paul7.refal.parser
	:net.paul7.utility)
  (:export match-pattern))

(in-package :net.paul7.refal.matcher)

;; check if value is appropriate to be bound to var, i.e.
;; 1) check data type 
;; 2) make sure there's no conflict with already bound value
(defgeneric appropriate (var value)
  (:method-combination and))

(defmethod appropriate and ((var refal-var) value)
  (with-accessors ((oldvalue value)) var
    (or (not (bound var))
	(scope= value oldvalue))))

(defmethod appropriate and ((var refal-t-var) value)
  (= (scope-size value) 1))

(defmethod appropriate and ((var refal-s-var) value)
  (let ((first (scope-first value)))
    (not (scopep first))))

(defmethod appropriate and ((var-list refal-scope) value)
  (scopep value))

;; chomp size elements of the scope
;; try to bind var, if appropriate
(defun match-size (var scope size)
  (let ((scope-size (scope-size scope)))
    (if (<= size scope-size)
	(let ((matching (subscope scope :length size)))
	  (when (appropriate var matching)
	    (bind-var var matching)
	    size)))))

;; bind variable in the data scope given
;; continue with the rest on success
(defgeneric match-var (first rest scope &optional next-op))

(defmethod match-var ((first refal-t-var) rest scope 
		      &optional (next-op (constantly t)))
  (let ((bound (bound first)))
    (if (match-size first scope 1)
	(or (match-pattern rest (subscope scope :shift 1) next-op)
	    (if (not bound)
		(unbind-var first))))))

(defmethod match-var ((first refal-e-var) rest scope
		      &optional (next-op (constantly t)))
  (if (bound first)
      (let ((size (scope-size (value first))))
	(if (match-size first scope size)
	    (match-pattern rest (subscope scope :shift size) next-op)))
      (do ((size 0 (1+ size)))
	  ((not (match-size first scope size))
	   (unbind-var first))
	(if (match-pattern rest (subscope scope :shift size) next-op)
	    (return t)
	    (unbind-var first)))))

(defmethod match-var ((first refal-scope) rest scope
		       &optional (next-op (constantly t)))
  (if (not (empty scope))
      (let ((subexpr (scope-first scope)))
	(if (appropriate first subexpr)
	    (flet ((chain-call ()
		     (match-pattern rest 
				    (subscope scope :shift 1 :reuse t) 
				    next-op)))
	      (match-pattern first subexpr #'chain-call))))))

;; try and bind vars to match the scope given
;; on success, continue to next-op
;; retry, if it fails
(defun match-pattern (pattern scope &optional (next-op (constantly t)))
  (cond 
    ((and (empty pattern) (empty scope))
     (funcall next-op))
    ((not (empty pattern))
     (let ((first (scope-first pattern))
	   (rest (subscope pattern :shift 1)))
       (match-var first rest scope next-op)))
      (t nil)))
