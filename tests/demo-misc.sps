;;; demo-misc.sps --
;;
;;Demo miscellaneous stuff.
;;

#!vicare
(import (vicare)
  (ikarus system $flonums)
  (ikarus system $numerics))

(define LEAST-FX	-536870912)

(define x 1)
(define y 1/2+20i)

(define y.rep (real-part y))
(define y.imp (imag-part y))

(define denom (+ (sqr y.rep) (sqr y.imp)))
(define num.rep ($mul-fixnum-number x y.rep))
(define num.imp (let ((x.neg ($neg-fixnum x)))
		  (if (fixnum? x.neg)
		      ($mul-fixnum-number x.neg y.imp)
		    ($mul-bignum-number x.neg y.imp))))
(define rep ($div-number-number num.rep denom))
(define imp ($div-number-number num.imp denom))

(set-port-buffer-mode! (current-output-port)
		       (buffer-mode none))

(pretty-print (list y.rep (sqr y.rep)))
(pretty-print (list y.imp (sqr y.imp)))
(pretty-print (list 'denom denom))
(pretty-print (list 'num.rep num.rep))
(pretty-print (list 'num.imp num.imp))
(pretty-print (list rep imp))


;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
