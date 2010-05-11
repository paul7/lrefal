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
(defgeneric appropriate (var value &optional size)
  (:method-combination and))

(defmethod appropriate and ((var refal-var) value &optional (size 1))
  (with-accessors ((oldvalue value)) var
    (or (not (bound var))
	(list-head= value oldvalue size))))

(defmethod appropriate and ((var refal-t-var) value &optional (size 1))
  (= size 1))

(defmethod appropriate and ((var refal-s-var) value &optional (size 1))
  (declare (ignore size))
  (and (consp value)
       (not (consp (first value)))
       (not (scopep (first value)))))

(defmethod appropriate and ((var-list refal-pattern) value &optional (size 1))
  (declare (ignore size))
  (scopep value))

;; chomp size elements of the scope
;; try to bind var, if appropriate
(defun match-size (var scope size)
  (let ((elements (active-scope scope)))
    (if (<= size (length elements))
	(when (appropriate var elements size)
	  (let ((matching (subseq elements 0 size)))
	    (bind-var var matching)
	    size)))))

;; bind variable in the data scope given
;; continue with the rest on success
(defgeneric match-var (first rest scope &optional next-op))

(defmethod match-var ((first refal-t-var) rest scope 
		      &optional (next-op (constantly t)))
  (let ((bound (bound first)))
    (if (match-size first scope 1)
	(or (match-pattern rest (shift-scope scope 1) next-op)
	    (if (not bound)
		(unbind-var first))))))

(defmethod match-var ((first refal-e-var) rest scope
		      &optional (next-op (constantly t)))
  (if (bound first)
      (let ((size (length (value first))))
	(if (match-size first scope size)
	    (match-pattern rest (shift-scope scope size) next-op)))
      (do ((size 0 (1+ size)))
	  ((not (match-size first scope size))
	   (unbind-var first))
	(if (match-pattern rest (shift-scope scope size) next-op)
	    (return t)
	    (unbind-var first)))))

(defmethod match-var ((first refal-pattern) rest scope
		       &optional (next-op (constantly t)))
  (let ((subexpr (first (active-scope scope))))
    (if (appropriate first subexpr)
	(flet ((chain-call ()
		 (match-pattern rest (shift-scope scope 1) next-op)))
	  (match-pattern first subexpr #'chain-call)))))

;; try and bind vars to match the scope given
;; on success, continue to next-op
;; retry, if it fails
(defun match-pattern (pattern scope &optional (next-op (constantly t)))
  (let ((first (first (active-scope pattern)))
	(rest (shift-scope pattern 1)))
    (cond 
      ((and (empty pattern) (empty scope))
       (funcall next-op))
      ((not (empty pattern))
       (match-var first rest scope next-op))
      (t nil))))

;;; pattern fiddling
(defun make-pattern (specs &optional (dict (make-hash-table)))
  (labels ((add-var (type name)
	     (let ((old-var (gethash name dict)))
	       (cond
		 ((not old-var) 
		  (setf (gethash name dict) (make-var type name)))
		 ((eq (var-type old-var) type) 
		  old-var)
		 (t (error "type mismatch")))))
	   (add-var-from-spec (spec)
	     (if (atom (first spec))
		 (add-var (make-uniform-type(first spec)) (second spec))
		 (make-pattern spec dict))))
    (values (data->pattern (mapcar #'add-var-from-spec specs))
	    dict)))

;;; testing
(defun ref-test (pattern-spec string)
  (multiple-value-bind (pattern dict) (string->pattern pattern-spec)
    (let ((scope (string->scope string)))
      (when (match-pattern pattern scope)
	(loop for var being each hash-value in dict do
	     (format t "~a~%" var))
	t))))
