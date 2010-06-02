;;; -*- mode: lisp

(defsystem :lrefal
  :components 
  ((:module "aux"
	    :components
	    ((:file "utility")
	     (:file "cont")))
   (:module "src"
	    :components
	    ((:file "refal-internal" )
	     (:file "refal-parser" 
		    :depends-on ("refal-internal"))
	     (:file "refal-matcher" 
		    :depends-on ("refal-internal"))
	     (:file "refal-matcher2"
		    :depends-on ("refal-internal"))
	     (:file "refal-compiler" 
		    :depends-on ("refal-parser"
				 "refal-matcher"))
	     (:file "refal-runtime" 
		    :depends-on ("refal-compiler")))
	    :depends-on ("aux")))
  :depends-on ())
