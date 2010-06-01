;;;; faster Refal expression matcher engine
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.matcher2
  (:nicknames :rmatch2)
  (:use :common-lisp 
	:net.paul7.refal.internal
	:net.paul7.refal.parser
	:net.paul7.utility))

(in-package :net.paul7.refal.matcher2)

