;;;; Refal parser reworked
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.parser2
  (:nicknames :rparse2)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal)
  (:export))

(in-package :net.paul7.refal.parser2)

;;; Parser engine

(defclass token-source ()
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

(defmethod initialize-instance :after ((src token-source) &key)
  (with-accessors ((data data) 
		   (size size)) src
    (setf size (length data))))

(defun make-source (string)
  (make-instance 'token-source 
		 :data (convert-sequence string 'list)))

(defun read-source (src)
  (with-accessors ((data data) 
		   (src-pos src-pos) 
		   (size size)) src
    (if (< src-pos size)
	(elt data (post-incf src-pos)))))

(defun save-pos (src)
  (with-accessors ((saved-pos saved-pos)
		   (src-pos src-pos)) src
    (push src-pos saved-pos)))

(defun load-pos (src)
  (with-accessors ((saved-pos saved-pos)) src
    (if saved-pos
	(pop saved-pos)
	(error "stack underflow"))))

(defun try-token (src)
  (save-pos src))

(defun accept-token (src)
    (load-pos src))

(defun reject-token (src)
  (with-accessors ((src-pos src-pos)) src
    (setf src-pos (load-pos src))))

;; body should return list formed as follows:
;; (:token (token*) :option1 value1 ...)
(defmacro deftoken (name (src &rest args) 
			    &body body)
  (with-gensyms (result)
    `(defun ,name (,src ,@args)
       (try-token ,src)
       (let ((,result (progn
			,@body)))
	 (if ,result
	     (accept-token ,src)
	     (reject-token ,src))
	 ,result))))

;; single token
;; body returns just token itself
(defmacro deftoken-basic (name (src &rest args)
		    &body body)
  (with-gensyms (result)
    `(deftoken ,name (,src ,@args)
       (let ((,result (progn
			,@body)))
	 (if ,result
	     (list :token ,result))))))

(defmacro deftoken-collect (name (src &rest args)
			    &body body)
  (with-gensyms (result each)
    `(deftoken ,name (,src ,@args)
       (do ((,each (progn 
		     ,@body)
		   (progn
		     ,@body))
	    (,result nil))
	   ((or (not ,each)
		(getf ,each :end))
	    (list :token (nreverse ,result)))
	 (push (unwrap ,each) ,result)))))
     
(defmacro defmodifier (name (token &rest args) plist)
  `(defun ,name (,token ,@args)
     (if ,token
	 (join-plists ,token ',plist))))

(defmodifier token-end (token)
  (:end t))

(defmodifier token-splice (token)
  (:splice t))

(defun unwrap (token)
  (getf token :token))

(defun not-empty (token)
  (if (unwrap token)
      token))

;;; Refal syntax

(deftoken-basic refal-char (src)
  (read-source src))

(deftoken-basic exactly (src char)
  (let ((next (refal-char src)))
    (if (and next (char-equal char (unwrap next)))
	char)))

(deftoken one-of (src chars)
  (if chars
      (or (exactly src (first chars))
	  (one-of src (rest chars)))))

(deftoken refal-space (src)
  (one-of src '(#\Space #\Tab #\Newline)))

(deftoken refal-delimiter (src)
  (or (refal-space src) 
      (one-of src '(#\) #\( #\< #\> #\{ #\} ))))

(deftoken refal-word-char (src)
  (if (not (or (refal-delimiter src)
	       (refal-separator src)
	       (refal-statement-terminator src)))
      (refal-char src)))

(deftoken refal-open-parenthesis (src)
  (exactly src #\( ))

(deftoken refal-close-parenthesis (src)
  (exactly src #\) ))

(deftoken refal-open-funcall (src)
  (exactly src #\< ))

(deftoken refal-close-funcall (src)
  (exactly src #\> ))

(deftoken refal-open-block (src)
  (exactly src #\{ ))

(deftoken refal-close-block (src)
  (exactly src #\} ))

(deftoken refal-separator (src)
  (exactly src #\= ))

(deftoken refal-statement-terminator (src)
  (exactly src #\; ))

(deftoken refal-end-of-stream (src)
  (not (read-source src)))

(deftoken refal-bad (src)
  (not (characterp (read-source src))))

(deftoken-collect refal-word (src) 
  (refal-word-char src))

(deftoken-collect refal-spaces (src)
  (refal-space src))

(defmacro refal-skip-spaces (src)
  `(not-empty (refal-spaces ,src)))

(deftoken-basic refal-digit (src)
  (digit-char-p (unwrap (refal-char src))))

(deftoken-collect refal-digits (src)
  (refal-digit src))

(defun digits->integer (digits &optional (accum 0))
  (if digits
      (digits->integer (cdr digits) (+ (* 10 accum) (car digits)))
      accum))

(deftoken refal-integer (src)
  (let ((digits (refal-digits src)))
    (if digits
	(digits->integer (unwrap digits)))))

(deftoken refal-empty (src)
  (refal-skip-spaces src)
  (refal-end-of-stream src))
