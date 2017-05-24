# cl-liblbfgs
These are a set of CFFI bindings to libLBFGS, a C port of the implementation 
of Limited-memory Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) method written
by Jorge Nocedal in FORTRAN and rewritten in C by Naoaki Okazaki.

The C library libLBFGS must be installed and is available at:
* (http://www.chokkan.org/software/liblbfgs/)
* (https://github.com/chokkan/liblbfgs)

Warning: This is a very basic binding, not used extensively. Could be used 
as a starting point for a more robust lib or as an example to similar libs.
