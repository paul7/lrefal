;;;; Refal to Lisp compiler
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.compiler
  (:nicknames :rcomp :rtrans)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal
	:net.paul7.refal.parser
	:net.paul7.refal.matcher)
  (:export))

(in-package :net.paul7.refal.compiler)

;;; test functions

;; compile simple statement

(defun compile-statement (left right)
  (multiple-value-bind (pattern dict)
      (string->pattern left)
    (let ((construct (string->pattern right dict)))
      #'(lambda (data)
	  (if (match-pattern pattern data)
	      (interpolate construct))))))

(defun ref-test (left right data)
  (funcall
   (compile-statement left right)
   (string->scope data)))
