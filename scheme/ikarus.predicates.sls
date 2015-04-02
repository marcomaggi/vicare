;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggo <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus predicates)
  (export
    fixnum?		flonum?		bignum?
    ratnum?		compnum?	cflonum?
    number?		complex?	real?
    rational?		integer?	exact?
    inexact?		exact-integer?
    finite?		infinite?	nan?
    real-valued?	rational-valued? integer-valued?
    eof-object?		bwp-object?	void-object?
    boolean?		char?		vector?
    bytevector?		string?		procedure?
    null?		pair?		symbol?
    eq?			eqv?
    immediate?		code?
    transcoder?		weak-pair?
    not			bwp-object)
  (import (except (vicare)
		  fixnum?		flonum?			bignum?
		  ratnum?		compnum?		cflonum?
		  number?		complex?		real?
		  rational?		integer?		exact?
		  inexact?		exact-integer?
		  finite?		infinite?		nan?
		  real-valued?		rational-valued?	integer-valued?
		  eof-object?		bwp-object?		void-object?
		  boolean?		char?			vector?
		  bytevector?		string?			procedure?
		  null?			pair?			symbol?
		  eq?			eqv?
		  immediate?		code?
		  transcoder?		weak-pair?
		  not			bwp-object)
    (vicare system $fx)
    (vicare system $flonums)
    (vicare system $compnums)
    (vicare system $pairs)
    (vicare system $chars)
    (vicare system $strings)
    (vicare system $vectors)
    (only (vicare system $pointers)
	  $pointer=)
    (vicare system $foreign)
    ;;These are the ones implemented as primitive operations.
    (rename (only (vicare)
		  fixnum? flonum? bignum? ratnum? compnum? cflonum?
                  eof-object? bwp-object? #;void-object?
		  immediate? boolean? char? vector? string?
                  bytevector? procedure? null? pair? symbol? code? eq?
                  transcoder?)
            (bignum?		sys:bignum?)
            (boolean?		sys:boolean?)
            (bwp-object?	sys:bwp-object?)
            (bytevector?	sys:bytevector?)
            (cflonum?		sys:cflonum?)
            (char?		sys:char?)
            (code?		sys:code?)
            (compnum?		sys:compnum?)
            (eof-object?	sys:eof-object?)
            (eq?		sys:eq?)
            (fixnum?		sys:fixnum?)
            (flonum?		sys:flonum?)
            (immediate?		sys:immediate?)
            (null?		sys:null?)
            (pair?		sys:pair?)
            (procedure?		sys:procedure?)
            (ratnum?		sys:ratnum?)
            (string?		sys:string?)
            (symbol?		sys:symbol?)
            (transcoder?	sys:transcoder?)
            (vector?		sys:vector?)
	    ;;FIXME  To be  uncommented at  the next  boot image  rotation, when  the
	    ;;primitive operation is implemented.  (Marco Maggi; Mon Mar 30, 2015)
	    #;(void-object?	sys:void-object?)
	    ))


;;;; object type predicates

(define (fixnum?  x) (sys:fixnum?  x))
(define (bignum?  x) (sys:bignum?  x))
(define (ratnum?  x) (sys:ratnum?  x))
(define (flonum?  x) (sys:flonum?  x))
(define (compnum? x) (sys:compnum? x))
(define (cflonum? x) (sys:cflonum? x))

(define (number? x)
  (or (sys:fixnum? x)
      (sys:bignum? x)
      (sys:flonum? x)
      (sys:ratnum? x)
      (sys:compnum? x)
      (sys:cflonum? x)))

(define (complex? x)
  (or (sys:fixnum? x)
      (sys:bignum? x)
      (sys:flonum? x)
      (sys:ratnum? x)
      (sys:compnum? x)
      (sys:cflonum? x)))

(define (real? x)
  (or (sys:fixnum? x)
      (sys:bignum? x)
      (sys:flonum? x)
      (sys:ratnum? x)))

(define (real-valued? x)
  (cond ((sys:fixnum? x)	#t)
	((sys:bignum? x)	#t)
	((sys:ratnum? x)	#t)
	((sys:flonum? x)	#t)
	((sys:compnum? x)	(zero? ($compnum-imag x)))
	((sys:cflonum? x)	($flzero? ($cflonum-imag x)))
	(else			#f)))

(define (rational? x)
  (cond ((sys:fixnum? x) #t)
	((sys:bignum? x) #t)
	((sys:ratnum? x) #t)
	((sys:flonum? x) ($flonum-rational? x))
	(else #f)))

(define (rational-valued? x)
  (cond ((rational? x) #t)
	((cflonum? x)
	 (and ($fl= ($cflonum-imag x) 0.0)
	      ($flonum-rational? ($cflonum-real x))))
	((compnum? x)
	 ;;The real  part must be  rational valued.  The  imaginary part
	 ;;must be exact or inexact zero.
	 (and (let ((imp ($compnum-imag x)))
		(or (and (fixnum?  imp)
			 ($fxzero? imp))
		    (and (flonum?  imp)
			 ($fl= 0.0 imp))))
	      (let ((rep ($compnum-real x)))
		(or (fixnum?   rep)
		    (bignum?   rep)
		    (rational? rep)
		    ($flonum-rational? rep)))))
	(else #f)))

;;; --------------------------------------------------------------------

(define (exact-integer? x)
  (cond ((sys:fixnum? x) #t)
	((sys:bignum? x) #t)
	(else #f)))

(define (integer? x)
  (cond ((sys:fixnum? x) #t)
	((sys:bignum? x) #t)
	((sys:ratnum? x) #f)
	((sys:flonum? x) ($flonum-integer? x))
	(else #f)))

(define (integer-valued? x)
  (cond ((integer? x) #t)
	((cflonum? x)
	 (and ($fl= ($cflonum-imag x) 0.0)
	      ($flonum-integer? ($cflonum-real x))))
	((compnum? x)
	 ;;The real part must be an integer.  The imaginary part must be
	 ;;exact or inexact zero.
	 (and (let ((imp ($compnum-imag x)))
		(cond ((and (fixnum?  imp)
			    ($fxzero? imp)))
		      ((and (flonum?  imp)
			    ($fl= 0.0 imp)))
		      (else #f)))
	      (let ((rep ($compnum-real x)))
		(or (fixnum? rep)
		    (bignum? rep)
		    ($flonum-integer? rep)))))
	(else #f)))


(define (exact? x)
  (cond ((sys:fixnum?  x) #t)
	((sys:bignum?  x) #t)
	((sys:ratnum?  x) #t)
	((sys:flonum?  x) #f)
	((sys:compnum? x)
	 (and (exact? ($compnum-real x))
	      (exact? ($compnum-imag x))))
	((sys:cflonum? x) #f)
	(else
	 (assertion-violation 'exact? "expected number as argument" x))))

(define (inexact? x)
  (cond ((sys:flonum?  x) #t)
	((sys:fixnum?  x) #f)
	((sys:bignum?  x) #f)
	((sys:ratnum?  x) #f)
	((sys:compnum? x)
	 (or (inexact? ($compnum-real x))
	     (inexact? ($compnum-imag x))))
	((sys:cflonum? x) #t)
	(else
	 (assertion-violation 'inexact? "expected number as argument" x))))

(define (finite? x)
  (cond ((sys:flonum?  x) (flfinite? x))
	((sys:fixnum?  x) #t)
	((sys:bignum?  x) #t)
	((sys:ratnum?  x) #t)
	((sys:compnum? x)
	 (or (finite? ($compnum-real x))
	     (finite? ($compnum-imag x))))
	((sys:cflonum? x)
	 (and (flfinite? ($cflonum-real x))
	      (flfinite? ($cflonum-imag x))))
	(else
	 (assertion-violation 'finite? "expected number as argument" x))))

(define (infinite? x)
  (cond ((sys:flonum?  x) (flinfinite? x))
	((sys:fixnum?  x) #f)
	((sys:bignum?  x) #f)
	((sys:ratnum?  x) #f)
	((sys:compnum? x)
	 (or (infinite? ($compnum-real x))
	     (infinite? ($compnum-imag x))))
	((sys:cflonum? x)
	 (or (flinfinite? ($cflonum-real x))
	     (flinfinite? ($cflonum-imag x))))
	(else
	 (assertion-violation 'infinite? "expected number as argument" x))))

(define (nan? x)
  (cond ((sys:flonum?  x) (flnan? x))
	((sys:fixnum?  x) #f)
	((sys:bignum?  x) #f)
	((sys:ratnum?  x) #f)
	((sys:compnum? x)
	 (or (nan? ($compnum-real x))
	     (nan? ($compnum-imag x))))
	((sys:cflonum? x)
	 (or (flnan? ($cflonum-real x))
	     (flnan? ($cflonum-imag x))))
	(else
	 (assertion-violation 'nan? "expected number as argument" x))))


(define (boolean?    x) (sys:boolean?    x))
(define (bwp-object? x) (sys:bwp-object? x))
;;FIXME This implementation must be removed at the next boot image rotation, when the
;;primitive operation is implemented.  (Marco Maggi; Mon Mar 30, 2015)
(define (void-object? x) (eq? x (void)))
;;FIXME This implementation  must be used at  the next boot image  rotation, when the
;;primitive operation is implemented.  (Marco Maggi; Mon Mar 30, 2015)
;;
;;(define (void-object? x) (sys:void-object? x))
(define (bytevector? x) (sys:bytevector? x))
(define (char?       x) (sys:char?       x))
(define (code?       x) (sys:code?       x))
(define (eof-object? x) (sys:eof-object? x))
(define (immediate?  x) (sys:immediate?  x))
(define (null?       x) (sys:null?       x))
(define (pair?       x) (sys:pair?       x))
(define (procedure?  x) (sys:procedure?  x))
(define (string?     x) (sys:string?     x))
(define (symbol?     x) (sys:symbol?     x))
(define (transcoder? x) (sys:transcoder? x))
(define (vector?     x) (sys:vector?     x))

(define (weak-pair? x)
  (and (pair? x)
       (foreign-call "ikrt_is_weak_pair" x)))

(define (not x)
  (if x #f #t))

(define (bwp-object)
  (foreign-call "ikrt_bwp_object"))


(define (eq? x y)
  (sys:eq? x y))

(define (eqv? x y)
  (import (vicare))
  (cond ((eq? x y)
	 #t)

	((flonum? x)
	 (and (flonum? y)
	      (cond (($fl< x y)		#f)
		    (($fl> x y)		#f)
		    ;;To distinguish between +0.0 and -0.0?
		    (($fl= x 0.0)	($fl= ($fl/ 1.0 x) ($fl/ 1.0 y)))
		    (($flnan? x)	($flnan? y))
		    (($flnan? y)	#f)
		    (else #t))))

	((bignum? x)
	 (and (bignum? y) (= x y)))

	((ratnum? x)
	 (and (ratnum? y) (= x y)))

	((compnum? x)
	 (and (compnum? y)
	      (and (or (= ($compnum-real x) ($compnum-real y))
		       (and (flonum? ($compnum-real x))
			    (flonum? ($compnum-real y))
			    ($flnan? ($compnum-real x))
			    ($flnan? ($compnum-real y))))
		   (or (= ($compnum-imag x) ($compnum-imag y))
		       (and (flonum? ($compnum-imag x))
			    (flonum? ($compnum-imag y))
			    ($flnan? ($compnum-imag x))
			    ($flnan? ($compnum-imag y)))))))

	((cflonum? x)
	 (and (cflonum? y)
	      (and (or ($fl= ($cflonum-real x) ($cflonum-real y))
		       (and ($flnan? ($cflonum-real x))
			    ($flnan? ($cflonum-real y))))
		   (or ($fl= ($cflonum-imag x) ($cflonum-imag y))
		       (and ($flnan? ($cflonum-imag x))
			    ($flnan? ($cflonum-imag y)))))))

	((pointer? x)
	 (and (pointer? y) ($pointer= x y)))

	((keyword? x)
	 (and (keyword? y) (keyword=? x y)))

	(else #f)))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.predicates")))

#| end of library |# )

;;; end of file
