(defpackage #:cl-liblbfgs-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:cl-liblbfgs-system)  
 
(defsystem :cl-liblbfgs
  :description "cl-liblbfgs: bindings for liblbfgs."
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :depends-on (cffi)
  :serial t
  :components ((:file "package")  
	       (:file "lbfgs")
	       (:file "liblbfgs")))
