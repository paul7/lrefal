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

(defmethod initialize-instance :after ((src refal-source) &key)
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

(defmacro deftoken (name (src &rest args)
		    &body body)
  (with-gensyms (result options success)
    `(defun ,name (,src ,@args)
       (try-token ,src)
       (multiple-value-bind (,result ,options) (progn ,@body)
	 (let ((,success (or (getf ,options :success) ,result)))
	   (if ,success
	       (accept-token ,src)
	       (reject-token ,src))
	   (values ,result ,options))))))

(defmacro deftoken-collect (name (src &rest args)
			    &body body)
  (with-gensyms (result token options splice success ignore end each)
    `(deftoken ,name (,src ,@args)
       (do ((,result nil))
	   ()
	 (multiple-value-bind (,token ,options) (progn ,@body)
	   (let ((,splice (getf ,options :splice))
		 (,success (or (getf ,options :success) ,token))
		 (,ignore (getf ,options :ignore))
		 (,end (getf ,options :end)))
	     (if ,end
		 (return (values (nreverse ,result) '(:success t))))
	     (if ,success
		 (if (not ,ignore)
		     (if ,splice
			 (dolist (,each ,token)
			   (push ,each ,result))
			 (push ,token ,result)))
		 (return nil))))))))

;;; Refal syntax

(deftoken refal-char (src)
  (read-source src))

(deftoken exactly (src char)
  (let ((next (refal-char src)))
    (if (and next (char-equal char next))
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

