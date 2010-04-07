;;;; simple useful utilities
;;;; (c) paul7, 2010
;;;; (c) Paul Graham, 1993

(defpackage :net.paul7.utility
  (:nicknames :util)
  (:use :common-lisp)
  (:export with-gensyms
	   single
	   convert-sequence
	   post-incf
	   sequence-reader
	   test))

(in-package :net.paul7.utility)

;;; simple utilities	

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; check if list consists of sole element
(defun single (list)
  (and (consp list)
       (not (cdr list))))

(defmacro convert-sequence (sequence class)
  `(map ,class #'identity ,sequence))

(defmacro post-incf (place)
  (with-gensyms (old)
    `(let ((,old ,place))
       (incf ,place)
       ,old)))

(defun sequence-reader (sequence)
  (let ((position 0)
	(length (length sequence)))
    #'(lambda ()
	(if (< position length)
	    (elt sequence (post-incf position))))))

(defmacro test (form)
  (with-gensyms (result)
    `(let ((,result ,form))
       (format t "TESTING ~a => ~a~%" ',form ,result)
       ,result)))
