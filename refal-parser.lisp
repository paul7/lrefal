;;;; Refal parser
;;;; (c) paul7, 2010

(defpackage :net.paul7.refal.parser
  (:nicknames :rparse)
  (:use :common-lisp
	:net.paul7.utility
	:net.paul7.refal.internal)
  (:export string->program
	   string->scope))

(in-package :net.paul7.refal.parser)

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

(defmacro with-token ((var-name token-form
				&key else)
		      &body body)
  (with-gensyms (var)
    (if var-name
	`(let ((,var ,token-form))
	   (if ,var
	       (let ((,var-name (unwrap ,var)))
		 ,@body)
	       ,else))
	`(let ((,var ,token-form))
	   (if ,var
	       ,@body
	       ,else)))))
  
(defmacro with-tokens* (token-list 
			&body body)
  (if (null token-list)
      `(progn 
	 ,@body)
      `(with-token ,(first token-list)
	 (with-tokens* ,(rest token-list)
	   ,@body))))

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
  (with-gensyms (result each subtoken)
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
	     (if (getf ,each :splice)
		 (dolist (,subtoken (unwrap ,each))
		   (push ,subtoken ,result))
		 (push (unwrap ,each) ,result)))))))

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
       (with-tokens* ((nil ,open-form)
		      (,subexpr ,body-form)
		      (nil ,close-form 
		       :else ,bad-form))
	 (list :token ,subexpr)))))

(defmacro lookup (src &body body)
  `(unwind-protect (progn
		     (try-token ,src)
		     ,@body)
     (reject-token ,src)))
     
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
  (with-token (next (refal-char src))
    (if (char-equal char next)
	char)))

(deftoken one-of (src chars)
  (if chars
      (or (exactly src (first chars))
	  (one-of src (rest chars)))))

(deftoken refal-space (src)
  (one-of src '(#\Space #\Tab #\Newline)))

(deftoken refal-delimiter (src)
  (or (refal-space src) 
      (one-of src '(#\) #\( #\< #\> #\{ #\} #\' #\" ))))

(deftoken refal-word-char (src)
  (if (not (or (refal-delimiter src)
	       (refal-separator src)
	       (refal-statement-terminator src)
	       (refal-where-separator src)
	       (refal-clause-separator src)))
      (refal-char src)))

(deftoken refal-quoted-char (src)
  (if (not (exactly src #\' ))
      (refal-char src)))

(deftoken refal-double-quoted-char (src)
  (if (not (exactly src #\" ))
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

(deftoken refal-where-separator (src)
  (one-of src '(#\, #\& )))

(deftoken refal-clause-separator (src)
  (exactly src #\: ))

(deftoken refal-bad (src)
  (not (characterp (read-source src))))

(deftoken-list refal-word-chars (src) 
  (refal-word-char src))

(defmacro refal-word (src)
  `(not-empty (refal-word-chars ,src)))

(deftoken-list refal-quoted-chars (src)
  (refal-quoted-char src))

(deftoken-list refal-double-quoted-chars (src)
  (refal-double-quoted-char src))

(deftoken-list refal-spaces (src)
  (refal-space src))

(defmacro refal-skip-spaces (src)
  `(token-ignore (not-empty (refal-spaces ,src))))

(deftoken-basic refal-digit (src)
  (with-token (next (refal-char src))
    (digit-char-p next)))

(deftoken-list refal-digits (src)
  (refal-digit src))

(defun digits->integer (digits &optional (accum 0))
  (if digits
      (digits->integer (cdr digits) (+ (* 10 accum) (car digits)))
      accum))

(deftoken-basic refal-integer (src)
  (with-token (digits (not-empty (refal-digits src)))
    (digits->integer digits)))

(deftoken refal-empty (src)
  (refal-skip-spaces src)
  (end-of-stream src))

(deftoken refal-quote (src)
  (exactly src #\' ))

(defblock refal-string-chars
    (src)
  ((refal-quote src)
   (refal-quoted-chars src)
   (refal-quote src)))

(defmacro refal-string (src)
  `(token-splice (refal-string-chars ,src)))

(deftoken-basic refal-id (src)
  (with-token (id (refal-word src))
    (convert-sequence id 'string)))

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
      (refal-string src)
      (refal-subexpr src)
      (refal-integer src)
      (refal-id src)))

(deftoken-basic refal-literal (src)
  (with-token (word (or (refal-integer src)
			(refal-string src)
			(refal-id src)))
    (make-instance 'refal-e-var :value (mklist word))))

(deftoken-basic refal-var (src dict)
  (with-tokens* ((type (one-of src '(#\e #\t #\s)))
		 (nil (exactly src #\.))
		 (id (refal-id src)))
    (let ((old-var (gethash id dict)))
      (cond
	((not old-var) 
	 (setf (gethash id dict)
	       (make-var type id)))
	((eq (var-type old-var) 
	     (make-uniform-type type)) old-var)
	(t (error "type mismatch"))))))

(deftoken-basic refal-fun-and-args (src dict)
  (with-tokens* ((id (refal-id src))
		 (arg (refal-pattern src dict)))
    (make-instance 'refal-funcall 
		   :function-name id
		   :function-argument  arg)))

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
	    (refal-statement-terminator src)
	    (refal-where-separator src)))
      (refal-subpattern src dict)
      (refal-skip-spaces src) 
      (refal-funcall src dict)
      (refal-var src dict)
      (refal-literal src)))

(deftoken-basic refal-where (src dict)
  (refal-skip-spaces src)
  (with-tokens* ((nil (refal-where-separator src))
		 (where-expr (refal-pattern src dict))
		 (nil (refal-clause-separator src))
		 (clause (refal-pattern src dict)))
    (if (lookup src
	  (refal-open-block src))
	nil ; hackery to prevent confusion with refal-case
	(list :where where-expr
	      :matches clause))))

(deftoken-list refal-clauses (src dict)
  (refal-where src dict))

(deftoken-basic refal-case (src dict)
  (refal-skip-spaces src)
  (with-tokens* ((nil (refal-where-separator src))
		 (when-expr (refal-pattern src dict))
		 (nil (refal-clause-separator src))
		 (clause (refal-block src dict)))
    (list :when when-expr
	  :matches clause)))

(deftoken-basic refal-match (src dict)
  (with-tokens* ((nil (refal-separator src))
		 (pattern (refal-pattern src dict)))
    (list :replace pattern)))

(deftoken-basic refal-right (src dict)
  (with-tokens* ((right (or (refal-match src dict)
			    (refal-case src dict)))
		 (nil (refal-statement-terminator src)))
    right))

(deftoken-basic refal-statement (src &optional dict) 
  (orf dict (make-hash-table :test #'equalp))
  (with-tokens* ((left-pattern (refal-pattern src dict))
		 (clauses (refal-clauses src dict)) 
		 (right (refal-right src dict)))
    (list :left left-pattern
	  :clauses clauses
	  :right right
	  :dict dict)))

(deftoken refal-function-header (src)
  (refal-id src))

(deftoken-list refal-funbody (src &optional dict) 
  (or (stop src
	(or (refal-close-parenthesis src)
	    (refal-close-funcall src)
	    (refal-close-block src)
	    (refal-statement-terminator src)))
      (refal-statement src dict)))

(defblock refal-block (src &optional dict)
  ((progn
     (refal-skip-spaces src)
     (refal-open-block src))
   (refal-funbody src dict)
   (progn 
     (refal-skip-spaces src)
     (refal-close-block src)))
  (error "expected }"))

(deftoken-basic refal-function (src)
  (refal-skip-spaces src)
  (with-tokens* ((fname (refal-function-header src))
		 (fbody (refal-block src)
			:else (error (format nil 
					     "syntax error in function ~a" 
					     fname))))
    (list :fname fname
	  :statements fbody)))

(deftoken-list refal-program (src)
  (or (refal-skip-spaces src)
      (refal-function src)))

;; make refal-scope compatible atom list of the string
(defun string->scope (string)
  (let ((src (make-source string)))
    (with-tokens* ((expr (refal-expr src))
		   (nil (refal-empty src)
			:else (error (format nil "unexpected ~a" 
					     (unwrap (refal-char src))))))
      expr)))

;; parse program
(defun string->program (string)
  (let ((src (make-source string)))
    (with-tokens* ((program (refal-program src))
		   (nil (refal-empty src) 
			:else (error (format nil "garbage at the end of input"))))
      program)))
