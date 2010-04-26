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

(deftoken end-of-stream (src)
  (if (not (read-source src))
      (list :token t)))

(defmacro deftoken-collect (name (src &rest args)
			    construct
			    &body body)
  (with-gensyms (result each)
    `(deftoken ,name (,src ,@args)
       (do ((,each (if (not (end-of-stream ,src))
			    (progn 
			      ,@body))
		   (if (not (end-of-stream ,src))
			    (progn 
			      ,@body)))
	    (,result nil))
	   ((or (not ,each)
		(getf ,each :end))
	    (list :token (funcall ,construct (nreverse ,result))))
	 (if (not (getf ,each :ignore))
	     (push (unwrap ,each) ,result))))))

(defmacro deftoken-list (name (src &rest args)
			 &body body)
  `(deftoken-collect ,name (,src ,@args)
       #'identity
     ,@body))

(defmacro defblock (name 
		    (src &rest args) 
		    (open-form 
		     body-form 
		     close-form) 
		    &optional (bad-form `(error "expected closing token")))
  (with-gensyms (subexpr)
    `(deftoken ,name (,src ,@args)
       (and ,open-form
	    (let ((,subexpr ,body-form))
	      (if ,subexpr 
		  (if ,close-form
		      ,subexpr
		      ,bad-form)))))))

(defmacro lookup (src &body body)
  `(unwind-protect (progn
		     (try-token ,src)
		     ,@body)
     (reject-token ,src)))
     
(defmacro defmodifier (name (token &rest args) plist)
  `(defun ,name (,token ,@args)
     (if ,token
	 (join-plists ,token ',plist))))

(defmodifier token-end (token)
  (:end t))

(defmodifier token-splice (token)
  (:splice t))

(defmodifier token-ignore (token)
  (:ignore t))

(defun unwrap (token)
  (getf token :token))

(defun not-empty (token)
  (if (unwrap token)
      token))

(defmacro stop (src &body body)
  `(lookup ,src (token-end ,@body)))

(defmacro invalid (token-form 
		   &optional (error-form `(error "invalid source")))
  `(if ,token-form
       ,error-form))

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

(deftoken refal-bad (src)
  (not (characterp (read-source src))))

(deftoken-list refal-word (src) 
  (refal-word-char src))

(deftoken-list refal-spaces (src)
  (refal-space src))

(defmacro refal-skip-spaces (src)
  `(token-ignore (not-empty (refal-spaces ,src))))

(deftoken-basic refal-digit (src)
  (digit-char-p (unwrap (refal-char src))))

(deftoken-list refal-digits (src)
  (refal-digit src))

(defun digits->integer (digits &optional (accum 0))
  (if digits
      (digits->integer (cdr digits) (+ (* 10 accum) (car digits)))
      accum))

(deftoken-basic refal-integer (src)
  (let ((digits (unwrap (refal-digits src))))
    (if digits
	(digits->integer digits))))

(deftoken refal-empty (src)
  (refal-skip-spaces src)
  (refal-end-of-stream src))

(defblock refal-subexpr 
    (src) 
  ((refal-open-parenthesis src)
   (refal-expr src)
   (refal-close-parenthesis src))
  (error "expected )"))
  
(deftoken-collect refal-expr (src)
    #'data->scope
  (or (stop src
	(or (refal-close-parenthesis src)
	    (refal-close-funcall src)
	    (refal-close-block src)
	    (refal-separator src)
	    (refal-statement-terminator src)))
      (refal-skip-spaces src)
      (refal-subexpr src)
      (refal-integer src)
      (refal-char src)))

(deftoken-basic refal-literal (src)
  (let ((word (or (refal-integer src) 
		  (refal-word src))))
    (if word
	(make-instance 'refal-e-var :value (mklist (unwrap word))))))

(deftoken-basic refal-id (src)
  (let ((id (refal-word src)))
    (if id
	(convert-sequence (unwrap id) 'string))))

(deftoken-basic refal-var (src dict)
  (let ((type (one-of src '(#\e #\t #\s))))
    (if (and type (exactly src #\.))
	(let ((id (refal-id src)))
	  (if id
	      (let* ((id (unwrap id))
		     (type (unwrap type))
		     (old-var (gethash id dict)))
		(cond
		  ((not old-var) 
		   (setf (gethash id dict)
			 (make-var type id)))
		  ((eq (var-type old-var) 
		       (make-uniform-type type)) old-var)
		  (t (error "type mismatch")))))))))

(deftoken refal-fun-and-args (src dict)
  (let ((id (refal-id src)))
    (if id
	(let ((arg (refal-pattern src dict)))
	  (if arg
	      (make-instance 'refal-funcall 
			     :function-name (unwrap id)
			     :function-argument (unwrap arg)))))))

(defblock refal-funcall 
    (src dict)
  ((refal-open-funcall src)
   (refal-fun-and-args src dict)
   (refal-close-funcall src))
  (error "expected >"))

(defblock refal-subpattern (src dict)
  ((refal-open-parenthesis src)
   (refal-pattern src dict)
   (refal-close-parenthesis src) )
  (error "expected )"))

(deftoken-collect refal-pattern (src dict)
    #'data->pattern
  (or (stop src
	(or (refal-close-parenthesis src)
	    (refal-close-funcall src)
	    (refal-close-block src)
	    (refal-separator src)
	    (refal-statement-terminator src)))
      (refal-skip-spaces src)
      (refal-subpattern src dict)
      (refal-funcall src dict)
      (refal-integer src)
      (refal-literal src)))

#+never(deftoken-sequence refal-pattern 
    (src &optional (dict (make-hash-table :test #'equalp))) 
    #'data->pattern #'refal-expression-char
  (or (refal-subpattern src dict)
      (refal-funcall src dict)
      (refal-var src dict)
      (refal-literal src)))
