;;;; Refal parser and internal data representation
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.parser
  (:nicknames :rparse)
  (:use :common-lisp 
	:net.paul7.utility
	:net.paul7.refal.internal)
  (:export string->scope 
	   string->pattern
	   string->statement
	   data->pattern
	   interpolate))

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

(defmethod initialize-instance :after ((src refal-source) &key)
  (with-accessors ((data data) 
		   (size size)) src
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
  (or (refal-space src) 
      (one-of src '(#\) #\( #\< #\> ))))

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
  (exactly src #\> ))

(deftoken refal-close-funcall (src)
  (exactly src #\> ))

(deftoken refal-separator (src)
  (exactly src #\= ))

(deftoken refal-statement-terminator (src)
  (exactly src #\; ))

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
	     ((or (refal-end-of-stream ,src)
		  (not (setf ,each (progn ,@cond))))
	      (nreverse ,result))
	   (push ,each ,result))))))

(deftoken-collect refal-word (src) 
  (refal-word-char src))

(deftoken-collect refal-skip-spaces (src)
  (refal-space src))

(deftoken refal-empty (src)
  (refal-skip-spaces src)
  (refal-end-of-stream src))

(deftoken refal-inner (src)
  (not (or (refal-close-parenthesis src)
	   (refal-close-funcall src)
	   (refal-separator src)
	   (refal-statement-terminator src))))

(deftoken refal-check-end (src)
  (cond 
    ((refal-end-of-stream src)
     t)
    ((refal-bad src)
     (error "bad source"))
    ((refal-inner src)
     nil)
    (t t)))

(defmacro deftoken-sequence (name (src &rest args) 
			     &body body)
  (with-gensyms (result)
    `(deftoken ,name (,src ,@args)
       (let ((,result nil))
	 (do ()
	     ((progn
		(refal-skip-spaces src)
		(refal-check-end ,src))
	      (data->pattern (nreverse ,result)))
	   (push (progn 
		   ,@body) ,result))))))

(defmacro defblock (name 
		    (src &rest args) 
		    (open 
		     body 
		     close) 
		    &optional (bad `(error "expected closing token")))
  (with-gensyms (subexpr)
    `(deftoken ,name (,src ,@args)
       (and (,open ,src)
	    (let ((,subexpr (,body ,src ,@args)))
	      (if (and ,subexpr 
		       (,close ,src))
		  ,subexpr
		  ,bad))))))

(defblock refal-subexpr 
    (src) 
  (refal-open-parenthesis 
   refal-expr
   refal-close-parenthesis)
  (error "expected )"))
  
(deftoken-sequence refal-expr (src)
  (or (refal-subexpr src)
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

#+tomorrow(deftoken refal-fun-and-args (src)
  (let ((id (refal-id src)))
    (if id
	(let (())))))

#+tomorrow(defblock refal-funcall 
    (src
     (refal-open-funcall
      refal-fun-and-args
      refal-close-funcall)
     (error "expected >")))
  
(defblock refal-subpattern 
    (src dict)
  (refal-open-parenthesis 
   refal-pattern
   refal-close-parenthesis)
  (error "expected )"))

(deftoken-sequence refal-pattern 
    (src &optional (dict (make-hash-table :test #'equalp)))
  (or (refal-subpattern src dict)
      (refal-var src dict)
      (refal-literal src)))

(deftoken refal-statement 
    (src &optional (dict (make-hash-table :test #'equalp)))
  (let ((left-pattern (refal-pattern src dict)))
    (if (refal-separator src)
	(let ((right-pattern (refal-pattern src dict)))
	  (if (refal-statement-terminator src)
	      (list left-pattern right-pattern dict))))))

;; make refal-scope compatible atom list of the string
(defun string->scope (string)
  (let* ((src (make-source string))
	 (expr (refal-expr src)))
    (if (refal-empty src)
	expr
	(error (format nil "unexpected ~a" (refal-char src))))))

;; make scope corresponding to the string
(defun data->scope (data)
  (make-instance 'refal-scope :data data))

(defun string->pattern (string 
			&optional (dict (make-hash-table :test #'equalp)))
  (let ((src (make-source string)))
    (values 
     (refal-pattern src dict)
     dict)))

(defun string->statement (string)
  (let ((src (make-source string)))
    (refal-statement src)))

(defun data->pattern (data)
  (make-instance 'refal-pattern :data data))

(defgeneric interpolate (object))

(defmethod interpolate ((var refal-var))
  (if (bound var)
      (value var)
      (error (format nil "~a is unbound" var))))
  
(defmethod interpolate ((pattern refal-pattern))
  (data->scope (mapcan (compose #'copy-list #'mklist #'interpolate)
		       (data pattern))))
