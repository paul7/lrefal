;;;; Refal builtin functions & runtime
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.runtime
  (:nicknames :rrt)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal)
  (:export))

(in-package :net.paul7.refal.parser)

(defun register-builtin (name))

;; define & register builtin function
(defmacro defbuiltin (name (scope)
		      &body body)
  (register-builtin name)
  `(defun ,name (,scope)
     ,@body))
