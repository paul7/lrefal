;;;; Refal parser and internal data representation
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.parser
  (:nicknames :rparse)
  (:use :common-lisp 
	:net.paul7.utility
	:net.paul7.refal.internal)
  (:export string->scope 
	   string->pattern
	   data->pattern))

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
  (or (refal-space src) (one-of src '(#\) #\( ))))

(deftoken refal-word-char (src)
  (if (not (refal-delimiter src))
      (refal-char src)))

(deftoken refal-open-parenthesis (src level inside-token)
  (if (exactly src #\( )
      (funcall inside-token src (1+ level))))

(deftoken refal-close-parenthesis (src)
  (exactly src #\) ))

(deftoken refal-end-of-stream (src)
  (not (read-source src)))

(deftoken refal-bad (src)
  (not (characterp (read-source src))))

(defmacro deftoken-collect (name (src &rest args)
				&body cond)
  (with-gensyms (result each)
    `(deftoken ,name (,src ,@args)
       (let ((,result nil)
	     (,each nil))
	 (do ()
	     ((or 
	       (refal-end-of-stream ,src)
	       (not (setf ,each (progn ,@cond))))
	      (nreverse ,result))
	   (push ,each ,result))))))

(deftoken-collect refal-word (src) 
  (refal-word-char src))

(deftoken-collect refal-skip-spaces (src)
  (refal-space src))

(deftoken refal-check-end (src level)
  (cond
    ((refal-end-of-stream src)
     (if (zerop level)
	 t
	 (error "expected )")))
    ((refal-bad src)
     (error "bad source"))
    ((refal-close-parenthesis src)
     (if (not (zerop level))
	 t
	 (error "unexpected )")))
    (t nil)))

(defmacro deftoken-sequence (name (src level &rest args) 
			     &body body)
  (with-gensyms (result)
    `(deftoken ,name (,src ,level ,@args)
       (let ((,result nil))
	 (do ()
	     ((progn
		(refal-skip-spaces src)
		(refal-check-end ,src ,level))
	      (data->pattern (nreverse ,result)))
	   (push (progn 
		   ,@body) ,result))))))

(deftoken-sequence refal-expr (src level)
  (or
   (refal-open-parenthesis src level #'refal-expr)
   (refal-char src)))

(deftoken refal-literal (src)
  (let ((word (refal-word src)))
    (if word
	(make-instance 'refal-e-var :value word))))

(deftoken refal-id (src)
  (let ((id (refal-word src)))
    (if id
	(convert-sequence id 'string))))

(deftoken refal-var (src dict)
  (let ((type (one-of src '(#\e #\t #\s))))
    (if (and type (exactly src #\.))
	(let ((id (refal-id src)))
	  (let ((old-var (gethash id dict)))
	    (cond
	      ((not old-var) 
	       (setf (gethash id dict)
		     (make-var type id)))
	      ((eq (var-type old-var) 
		   (make-uniform-type type)) old-var)
	      (t (error "type mismatch"))))))))

(deftoken-sequence refal-pattern 
    (src level &optional 
	 (dict 
	  (make-hash-table :test #'equalp)))
  (let ((inner-pattern #'(lambda (src level)
			   (refal-pattern src level dict))))
    (or
     (refal-open-parenthesis src level inner-pattern)
     (refal-var src dict)
     (refal-literal src))))

;; make refal-scope compatible atom list of the string
(defun string->scope (string)
  (let ((src (make-source string)))
    (refal-expr src 0)))

;; make scope corresponding to the string
(defun data->scope (data)
  (make-instance 'refal-scope :data data))

(defun string->pattern (string)
  (let ((src (make-source string))
	(dict (make-hash-table :test #'equalp)))
    (values 
     (refal-pattern src 0 dict)
     dict)))

(defun data->pattern (data)
  (make-instance 'refal-pattern :data data))

(defgeneric interpolate (object))

(defmethod interpolate ((var refal-var))
  (if (bound var)
      (value var)
      (error (format nil "~a is unbound" var))))
  
(defmethod interpolate ((pattern refal-pattern))
  (data->scope (mapcan #'interpolate (data pattern))))
