;;;; simple useful utilities
;;;; (c) paul7, 2010
;;;; (c) Paul Graham, 1993

(defpackage :net.paul7.utility
  (:nicknames :util)
  (:use :common-lisp)
  (:export with-gensyms
	   single
	   mklist
	   compose
	   convert-sequence
	   post-incf
	   sequence-reader
	   test
	   join-plists))

(in-package :net.paul7.utility)

;;; simple utilities	

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; check if list consists of sole element
(defun single (list)
  (and (consp list)
       (not (cdr list))))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

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

(defun join-plists (first second)
  (do ((result (copy-list first))
       (rest second (cddr rest)))
      ((not rest)
       result)
    (let ((key (car rest))
	  (value (cadr rest)))
      (setf (getf result key) value))))
