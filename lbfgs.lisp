(in-package :cl-liblbfgs)

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cl:defconstant LBFGS_FLOAT 64)

(cl:defconstant LBFGS_IEEE_FLOAT 1)

(defanonenum 
	(LBFGS_SUCCESS 0)
	(LBFGS_CONVERGENCE 0)
	LBFGS_STOP
	LBFGS_ALREADY_MINIMIZED
	(LBFGSERR_UNKNOWNERROR -1024)
	LBFGSERR_LOGICERROR
	LBFGSERR_OUTOFMEMORY
	LBFGSERR_CANCELED
	LBFGSERR_INVALID_N
	LBFGSERR_INVALID_N_SSE
	LBFGSERR_INVALID_X_SSE
	LBFGSERR_INVALID_EPSILON
	LBFGSERR_INVALID_TESTPERIOD
	LBFGSERR_INVALID_DELTA
	LBFGSERR_INVALID_LINESEARCH
	LBFGSERR_INVALID_MINSTEP
	LBFGSERR_INVALID_MAXSTEP
	LBFGSERR_INVALID_FTOL
	LBFGSERR_INVALID_WOLFE
	LBFGSERR_INVALID_GTOL
	LBFGSERR_INVALID_XTOL
	LBFGSERR_INVALID_MAXLINESEARCH
	LBFGSERR_INVALID_ORTHANTWISE
	LBFGSERR_INVALID_ORTHANTWISE_START
	LBFGSERR_INVALID_ORTHANTWISE_END
	LBFGSERR_OUTOFINTERVAL
	LBFGSERR_INCORRECT_TMINMAX
	LBFGSERR_ROUNDING_ERROR
	LBFGSERR_MINIMUMSTEP
	LBFGSERR_MAXIMUMSTEP
	LBFGSERR_MAXIMUMLINESEARCH
	LBFGSERR_MAXIMUMITERATION
	LBFGSERR_WIDTHTOOSMALL
	LBFGSERR_INVALIDPARAMETERS
	LBFGSERR_INCREASEGRADIENT)

(defanonenum 
	(LBFGS_LINESEARCH_DEFAULT 0)
	(LBFGS_LINESEARCH_MORETHUENTE 0)
	(LBFGS_LINESEARCH_BACKTRACKING_ARMIJO 1)
	(LBFGS_LINESEARCH_BACKTRACKING 2)
	(LBFGS_LINESEARCH_BACKTRACKING_WOLFE 2)
	(LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE 3))

(cffi:defcstruct lbfgs_parameter_t
	(m :int)
	(epsilon :double)
	(past :int)
	(delta :double)
	(max_iterations :int)
	(linesearch :int)
	(max_linesearch :int)
	(min_step :double)
	(max_step :double)
	(ftol :double)
	(wolfe :double)
	(gtol :double)
	(xtol :double)
	(orthantwise_c :double)
	(orthantwise_start :int)
	(orthantwise_end :int))

(cffi:defcfun ("lbfgs" lbfgs) :int
  (n :int)
  (x :pointer)
  (ptr_fx :pointer)
  (proc_evaluate :pointer)
  (proc_progress :pointer)
  (instance :pointer)
  (param :pointer))

(cffi:defcfun ("lbfgs_parameter_init" lbfgs_parameter_init) :void
  (param :pointer))

(cffi:defcfun ("lbfgs_malloc" lbfgs_malloc) :pointer
  (n :int))

(cffi:defcfun ("lbfgs_free" lbfgs_free) :void
  (x :pointer))


