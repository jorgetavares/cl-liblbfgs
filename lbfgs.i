%module lbfgs
 %{
 /* Includes the header in the wrapper code */
 #include "lbfgs.h"
 %}
 
 /* Parse the header file to generate wrappers */
 %include "lbfgs.h"
