;;;; continuations support utilities
;;;; (c) Paul Graham, 1993
;;;; (c) paul7, 2010

(defpackage :net.paul7.continuations
  (:nicknames :cont)
  (:use :common-lisp)
  (:export =/cc
	   =lambda
	   =defun
	   =bind
	   =values
	   =funcall
	   =apply))

(in-package :net.paul7.continuations)

(defmacro =/cc (&body body)
  `(let ((*cont* #'identity))
     ,@body))

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
	 `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

