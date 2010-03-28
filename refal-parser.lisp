;;;; Refal parser and internal data representation
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.parser
  (:nicknames :rparse)
  (:use :common-lisp 
	:net.paul7.utility)
  (:export make-scope 
	   shift-scope
	   active-scope
	   scopep))

(in-package :net.paul7.refal.parser)

;;; Refal parser

(defclass refal-source ()
  ((data
    :accessor data
    :initarg :data
    :initform nil)
   (src-pos
    :accessor src-pos
    :initform 0)
   (saved-pos
    :accessor saved-pos
    :initform nil)
   (size 
    :accessor size
    :initform 0)))

(defmethod initialize-instance :after 
    ((src refal-source) &key)
  (with-accessors ((data data) (size size)) src
    (setf size (length data))))

(defun make-source (string)
  (make-instance 'refal-source 
		 :data (convert-sequence string 'list)))

(defgeneric read-source (src))

(defmethod read-source ((src refal-source))
  (with-accessors ((data data) 
		   (src-pos src-pos) 
		   (size size)) src
    (if (< src-pos size)
	(elt data (post-incf src-pos)))))

(defgeneric save-pos (src))

(defmethod save-pos ((src refal-source))
  (with-accessors ((saved-pos saved-pos)
		   (src-pos src-pos)) src
    (push src-pos saved-pos)))

(defgeneric load-pos (src))

(defmethod load-pos ((src refal-source))
  (with-accessors ((saved-pos saved-pos)) src
    (if saved-pos
	(pop saved-pos)
	(error "stack underflow"))))

(defgeneric try-token (src))

(defmethod try-token ((src refal-source))
  (save-pos src))

(defgeneric accept-token (src))

(defmethod accept-token ((src refal-source))
    (load-pos src))

(defgeneric reject-token (src))

(defmethod reject-token ((src refal-source))
  (with-accessors ((src-pos src-pos)) src
    (setf src-pos (load-pos src))))

(defmacro deftoken (name (src &rest args) &body body)
  (with-gensyms (result)
    `(defun ,name (,src ,@args)
       (try-token ,src)
       (let ((,result (progn
			,@body)))
	 (if ,result
	     (accept-token ,src)
	     (reject-token ,src))
	 ,result))))

(deftoken refal-char (src)
  (read-source src))

(deftoken refal-open-parenthesis (src level)
  (if (char= (read-source src) #\( )
      (refal-expr src (1+ level))))

(deftoken refal-close-parenthesis (src)
  (char= (read-source src) #\) ))

(deftoken refal-end-of-stream (src)
  (not (read-source src)))

(deftoken refal-bad (src)
  (not (characterp (read-source src))))

(deftoken refal-expr (src level)
  (let ((result nil))
    (loop
       (if (refal-end-of-stream src)
	   (if (zerop level)
	       (return (data->scope (nreverse result)))
	       (error "expected )")))
       (if (refal-bad src)
	   (error "bad source"))
       (if (refal-close-parenthesis src)
	   (if (not (zerop level))
	       (return (data->scope (nreverse result)))
	       (error "unexpected )")))
       (push (or
	      (refal-open-parenthesis src level)
	      (refal-char src))
	     result))))

;;; this class represents Refal data
;;; encapsulates list of atoms constituting scope
;;; and boundaries of scope yet unmatched
(defclass refal-scope ()
  ((start
    :accessor start
    :initarg :start
    :initform 0)
   (end
    :accessor end
    :initarg :end)
   (data
    :accessor data
    :initarg :data
    :initform nil)))

(defmethod initialize-instance :after 
    ((scope refal-scope) &key)
  (with-accessors ((end end) (data data)) scope
    (setf end (length data))))

(defgeneric scopep (obj))

(defmethod scopep ((obj t))
  nil)

(defmethod scopep ((obj refal-scope))
  t)

;; return list representing unmatched part of the scope
(defgeneric active-scope (scope))

(defmethod active-scope ((scope refal-scope))
  (with-accessors ((start start)
		   (end end)
		   (data data)) scope
    (subseq data start end)))

;; make refal-scope compatible atom list of the string
(defun parse-string (string)
  (let ((src (make-source string)))
    (refal-expr src 0)))

;; make scope corresponding to the string
(defun data->scope (data)
  (make-instance 'refal-scope :data data))

(defun make-scope (string)
  (parse-string string))

;; promote scope boundaries after successful matching
(defun shift-scope (scope margin)
  (make-instance 'refal-scope 
		 :data (data scope)
		 :start (+ (start scope) margin)
		 :end (end scope)))

(defmethod print-object ((scope refal-scope) stream)
  (print-unreadable-object (scope stream)
    (format stream "~a" (data scope))))
