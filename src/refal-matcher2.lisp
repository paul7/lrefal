;;;; faster Refal expression matcher engine
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.matcher2
  (:nicknames :rmatch2)
  (:use :common-lisp 
	:net.paul7.refal.internal
	:net.paul7.refal.parser
	:net.paul7.utility
	:net.paul7.continuations)
  (:export match-pattern))

(in-package :net.paul7.refal.matcher2)

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

(defgeneric match-var (first rest scope &key next-op from-end))

(defmethod match-var ((first refal-t-var) rest scope 
		      &key (next-op (constantly t)) from-end)
  (let ((bound (bound first)))
    (if (match-size first scope 1)
	(or (match-pattern rest (subscope scope :shift 1) 
			   :next-op next-op)
	    (unless bound
	      (unbind-var first))))))

(defmethod match-var ((first refal-e-var) rest scope
		      &key (next-op (constantly t)) from-end)
  (if (bound first)
      (let ((size (scope-size (value first))))
	(if (match-size first scope size)
	    (match-pattern rest (subscope scope :shift size) 
			   :next-op next-op)))
      (do ((size 0 (1+ size)))
	  ((not (match-size first scope size))
	   (unbind-var first))
	(if (match-pattern rest (subscope scope :shift size) 
			   :next-op next-op)
	    (return t)
	    (unbind-var first)))))

(defmethod match-var ((first refal-scope) rest scope
		       &key (next-op (constantly t)) from-end)
  (unless (empty scope)
    (let ((subexpr (scope-first scope)))
      (if (appropriate first subexpr)
	  (flet ((chain-call ()
		   (match-pattern rest 
				  (subscope scope :shift 1) 
				  :next-op next-op)))
	    (match-pattern first subexpr :next-op #'chain-call))))))

(defun match-e-vars (pattern scope 
		     &key (next-op (constantly t)))
  (match-pattern-slow pattern scope :next-op next-op))

(defun match-pattern-slow (pattern scope &key (next-op (constantly t)))
  (cond 
    ((and (empty pattern) (empty scope))
     (funcall next-op))
    ((not (empty pattern))
     (let ((first (scope-first pattern))
	   (rest (subscope pattern :shift 1)))
       (match-var first rest scope :next-op next-op)))
      (t nil)))

;; try and bind vars to match the scope given
;; on success, continue to next-op
;; retry, if it fails
(defun match-pattern (pattern scope 
		      &key (next-op (constantly t)))
  (match-e-vars pattern scope :next-op next-op))
