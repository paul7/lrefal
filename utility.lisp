;;;; simple useful utilities
;;;; (c) paul7, 2010
;;;; (c) Paul Graham, 1993

(defpackage :net.paul7.utility
  (:nicknames :util)
  (:use :common-lisp)
  (:export with-gensyms
	   single
	   pairp
	   mklist
	   compose
	   convert-sequence
	   post-incf
	   orf
	   sequence-reader
	   test
	   join-plists
	   list-head= ))

(in-package :net.paul7.utility)

;;; simple utilities	

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; check if list consists of sole element
(defun single (list)
  (and (consp list)
       (not (cdr list))))

(defun pairp (obj)
  (and (consp obj)
       (atom (cdr obj))))

(defun mklist (obj)
  (if (and (listp obj) (not (pairp obj)))
      obj 
      (list obj)))

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

(defmacro post-incf (place &optional (delta 1) &environment env)
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car new) (+ ,delta ,getter)))
       (prog1 ,getter
	 ,setter))))

(defmacro orf (place &rest alternatives &environment env)
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car new) (or ,getter ,@alternatives)))
       (prog1 ,getter
	 ,setter))))

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

(defun list-head= (a b length)
  (if (zerop length)
      t
      (and (equal (car a) (car b))
	   (list-head= (cdr a) (cdr b) (1- length)))))
