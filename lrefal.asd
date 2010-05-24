;;; -*- mode: lisp

(defsystem :lrefal
  :components 
  ((:file "utility")
   (:file "refal-internal" 
	  :depends-on ("utility"))
   (:file "refal-parser" 
	  :depends-on ("refal-internal"))
   (:file "refal-matcher" 
	  :depends-on ("refal-internal"))
   (:file "refal-compiler" 
	  :depends-on ("refal-parser"
		       "refal-matcher"))
   (:file "refal-runtime" 
	  :depends-on ("refal-compiler")))
  :depends-on ())
