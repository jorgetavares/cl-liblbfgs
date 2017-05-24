;;;;
;;;; cl-libLBFGS
;;;;
;;;; These are a set of CFFI bindings to libLBFGS, a C port of the implementation 
;;;; of Limited-memory Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) method written
;;;; by Jorge Nocedal in FORTRAN and rewritten in C by Naoaki Okazaki.
;;;;
;;;; The C library libLBFGS must be installed and is available at:
;;;; http://www.chokkan.org/software/liblbfgs/
;;;; https://github.com/chokkan/liblbfgs
;;;;

(in-package :cl-liblbfgs)

;;;
;;; load foreign library
;;;

(cffi:define-foreign-library liblbfgs
  (:darwin "liblbfgs.dylib")
  (:unix (:or "libcurl.so.3" "libcurl.so"))
  (t (:default "liblbfgs")))
   
(cffi:use-foreign-library liblbfgs)


;;;
;;; define evaluate and progress functions
;;;

#|
(defmacro defevaluate (name &body body)
  ())


(defmacro defprogress (name &body body)
  `(cffi:defcallback ,name :int ((instance :pointer) (x :pointer) (g :pointer) 
				 (fx :double) (xnorm :double) (gnorm :double)
				 (step :double) (n :int) (k :int) (ls :int))
     (declare (ignore instance g xnorm gnorm step n ls))
     (progn
       (format t "Iteration ~d:~%" k)
       (format t "fx=~s x0=~a x1=~s~%" fx (cffi:mem-ref x :double 0) (cffi:mem-ref x :double 1))
       0)))
|#

;;;
;;; example
;;;

(cffi:defcallback evaluate :double  ((instance :pointer) (x :pointer) 
				     (g :pointer) (n :int) (step :double))
  (declare (ignore instance step))
  (let ((fx 0.0d0))
    (loop for i below n by 2
       do (let ((t1 (- 1.0d0 (cffi:mem-aref x :double i)))
		(t2 (* 10.0d0 (- (cffi:mem-aref x :double (1+ i)) 
				 (* (cffi:mem-aref x :double i)
				    (cffi:mem-aref x :double i))))))
	    (setf (cffi:mem-aref g :double (1+ i)) (* 20.0d0 t2)) 
	    (setf (cffi:mem-aref g :double i) 
		  (* -2.0d0 (+ (* (cffi:mem-aref x :double i) 
				  (cffi:mem-aref g :double (1+ i))) 
			       t1)))
	    (setf fx (+ fx (+ (* t1 t1)
			      (* t2 t2))))))
    fx))

(cffi:defcallback progress :int ((instance :pointer) (x :pointer) 
				 (g :pointer) (fx :double) 
				 (xnorm :double) (gnorm :double) (step :double) 
				 (n :int) (k :int) (ls :int))
  (declare (ignore instance g xnorm gnorm step n ls))
  (progn
    (format t "Iteration ~d:~%" k)
    (format t "fx=~s x0=~a x1=~s~%" fx (cffi:mem-ref x :double 0) (cffi:mem-ref x :double 1))
    0))


(defun sample ()
  (let* ((n 100)
	 (fx (cffi:foreign-alloc :double))
	 (x  (cffi:foreign-alloc :double :count n))
	 (return-code nil))
    ;; initialize variables
    (loop for i below n by 2
       do (setf (cffi:mem-aref x :double i     ) -1.2d0
		(cffi:mem-aref x :double (1+ i))  1.0d0))
    ;(loop for i below n
    ;   do (format t "~a~%" (cffi:mem-aref x :double i)))
    ;; init parameters and run lbfgs
    (cffi:with-foreign-object (params 'lbfgs_parameter_t)
      (lbfgs_parameter_init params)
      (cffi:with-foreign-slots ((m epsilon past delta max_iterations linesearch 
				   max_linesearch min_step max_step ftol wolfe gtol xtol) 
				params lbfgs_parameter_t)
	(format t "~a~%"
		(list m epsilon past delta max_iterations linesearch 
		      max_linesearch min_step max_step ftol wolfe gtol xtol)))
      (setf return-code 
	    (lbfgs n x fx 
		   (cffi:callback evaluate) (cffi:callback progress) 
		   (cffi:null-pointer) params)))
    ;; optimization results
    (format t "ret=~a fx=~a x0=~a x1=~a~%" 
	    return-code 
	    (cffi:mem-ref fx :double) 
	    (cffi:mem-aref x :double 0) (cffi:mem-aref x :double 1))))

