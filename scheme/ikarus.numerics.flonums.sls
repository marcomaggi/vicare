;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (ikarus flonums)
  (export
    inexact->exact
    exact		$flonum->exact
    fixnum->flonum

    flzero?		$flzero?
    flpositive?		$flpositive?
    flnegative?		$flnegative?
    fleven?		$fleven?
    flodd?		$flodd?
			$flonum-integer?
			$flonum-rational?

    flround		$flround
    flfloor		$flfloor
    flceiling		$flceiling
    fltruncate		$fltruncate

    flnumerator		$flnumerator
    fldenominator	$fldenominator

    flabs		$flabs
    flsin		$flsin
    flcos		$flcos
    fltan		$fltan
    flasin		$flasin
    flacos		$flacos
    flatan		$flatan		$flatan2
    flexp		$flexp
    fllog		$fllog		$fllog2
    flexpm1		$flexpm1
    fllog1p		$fllog1p
    flexpt		$flexpt
    flsqrt		$flsqrt

    flinteger?		flnan?
    flfinite?		flinfinite?

    fl=?
    fl<?		fl>?
    fl<=?		fl>=?
    fl+			fl-
    fl*			fl/

    flmax		$flmax
    flmin		$flmin

    flonum-parts	flonum-bytes
    bytevector->flonum	flonum->bytevector)
  (import (except (ikarus)
		  inexact->exact	exact		fixnum->flonum
		  flzero?		flpositive?	flnegative?
		  fleven?		flodd?		flround
		  flfloor		flceiling	fltruncate
		  flnumerator		fldenominator	flabs
		  flsin			flcos		fltan
		  flasin		flacos		flatan
		  flexp			fllog		flexpm1
		  fllog1p		flexpt		flsqrt
		  flinteger?		flnan?		flfinite?
		  flinfinite?		fl=?		fl<?
		  fl>?			fl<=?		fl>=?
		  fl+			fl-		fl*
		  fl/			flmax		flmin
		  flonum-parts		flonum-bytes
		  bytevector->flonum	flonum->bytevector)
    (ikarus system $pairs)
    (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $ratnums)
    (ikarus system $bytevectors)
    (except (ikarus system $flonums)
	    $flonum->exact
	    $flzero?
	    $flpositive?
	    $flnegative?
	    $fleven?
	    $flodd?
	    $flonum-integer?
	    $flonum-rational?
	    $flround
	    $flfloor
	    $flceiling
	    $fltruncate
	    $flnumerator
	    $fldenominator
	    $flabs
	    $flsin
	    $flcos
	    $fltan
	    $flasin
	    $flacos
	    $flatan		$flatan2
	    $flexp
	    $fllog		$fllog2
	    $flexpm1
	    $fllog1p
	    $flexpt
	    $flsqrt
	    $flmax
	    $flmin)
    (vicare arguments validation)
    (only (vicare syntactic-extensions)
	  cond-numeric-operand))


;;;; helpers

(define-syntax define-fl-operation/one
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who x)
       (define who (quote ?safe-who))
       (with-arguments-validation (who)
	   ((flonum	x))
	 (?unsafe-who x))))))

(define-syntax define-fl-operation/one/forcall
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who ?foreign-who)
     (begin
       (define-fl-operation/one ?safe-who ?unsafe-who)
       (define (?unsafe-who x)
	 (foreign-call ?foreign-who x))))))

(define-syntax define-fl-operation/two
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who x y)
       (define who (quote ?safe-who))
       (with-arguments-validation (who)
	   ((flonum	x)
	    (flonum	y))
	 (?unsafe-who x y))))))


;;;; flonums parts

(define (flonum-bytes x)
  (define who 'flonum-bytes)
  (with-arguments-validation (who)
      ((flonum	x))
    (values ($flonum-u8-ref x 0)
	    ($flonum-u8-ref x 1)
	    ($flonum-u8-ref x 2)
	    ($flonum-u8-ref x 3)
	    ($flonum-u8-ref x 4)
	    ($flonum-u8-ref x 5)
	    ($flonum-u8-ref x 6)
	    ($flonum-u8-ref x 7))))

(define (flonum-parts x)
  (define who 'flonum-parts)
  (with-arguments-validation (who)
      ((flonum	x))
    (let-values (((b0 b1 b2 b3 b4 b5 b6 b7) (flonum-bytes x)))
      (values ($fxzero? ($fxlogand b0 128))
	      (+ ($fxsll ($fxlogand b0 127) 4)
		 ($fxsra b1 4))
	      (+ (+ b7 ($fxsll b6 8) ($fxsll b5 16))
		 (* (+ b4
		       ($fxsll b3 8)
		       ($fxsll b2 16)
		       ($fxsll ($fxlogand b1 #b1111) 24))
		    (expt 2 24)))))))


;;;; rounding

(define-fl-operation/one flround $flround)

(module ($flround)

  (define ($flround x)
    (let* ((sbe ($flonum-sbe x))	;this is a fixnum
	   (be  ($fxlogand sbe #x7FF)))	;this is a fixnum
      (if ($fx>= be 1075)
	  ;;Nans, infinities, magnitude large enough to be an integer.
	  x
	;;This really needs to get optimized.
	(let-values (((positive? be mantissa) (flonum-parts x)))
	  (if ($fxzero? be)
	      ;;denormalized
	      (if positive? +0.0 -0.0)
	    ;;normalized flonum
	    (let ((r (inexact (%ratnum-round (+ mantissa (expt 2 52)) ($fx- 1075 be)))))
	      (if positive?
		  r
		($fl* r -1.0))))))))

  (define (%ratnum-round n nbe)
    (let* ((d  (sll 1 nbe))
	   (q  (sra n nbe))
	   (r  (bitwise-and n (sub1 d)))
	   (r2 (+ r r)))
      (cond ((< r2 d)	q)
	    ((> r2 d)	(+ q 1))
	    (else
	     (if (even? q)
		 q
	       (+ q 1))))))

  #| end of module: $flround |# )

;;; --------------------------------------------------------------------

(define-fl-operation/one flceiling $flceiling)

(define ($flceiling x)
  ;;FIXME Optimize for integer flonums case.  (Abdulaziz Ghuloum)
  (let ((e ($flonum->exact x)))
    (if (ratnum? e)
	(exact->inexact (ceiling e))
      x)))

;;; --------------------------------------------------------------------

(module (flfloor $flfloor)

  (define-fl-operation/one flfloor $flfloor)

  (define ($flfloor x)
    ;;FIXME Optimize for integer flonums case.  (Abdulaziz Ghuloum)
    (let ((e ($flonum->exact x)))
      (if (ratnum? e)
	  (exact->inexact (%ratnum-floor e))
	x)))

  (define (%ratnum-floor x)
    (let* ((n ($ratnum-n x))
	   (d ($ratnum-d x))
	   (q (quotient n d)))
      (if (>= n 0)
	  q
	(- q 1))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (fltruncate $fltruncate)

  (define-fl-operation/one fltruncate $fltruncate)

  (define ($fltruncate x)
    ;;FIXME It should preserve the sign of -0.0.  (Abdulaziz Ghuloum)
    (let ((v ($flonum->exact x)))
      (if (ratnum? v)
	  (exact->inexact ($ratnum-truncate v))
	x)))

  (define ($ratnum-truncate x)
    (let ((n ($ratnum-n x))
	  (d ($ratnum-d x)))
      (quotient n d)))

  #| end of module |# )


(module ($flonum->exact)

  (define ($flonum->exact x)
    (let* ((sbe ($flonum-sbe x))
	   (be ($fxlogand sbe #x7FF)))
      (cond (($fx= be 2047) ;nans/infs
	     #f)
	    (($fx>= be 1075) ;magnitude large enough to be an integer
	     (bitwise-arithmetic-shift-left ($flonum-signed-mantissa x)
					    ($fx- be 1075)))
	    (else
	     ;;This really needs to get optimized.
	     (let-values (((pos? be m) (flonum-parts x)))
	       (cond ((= be 0) ;denormalized
		      (if (= m 0)
			  0
			(* (if pos? 1 -1)
			   (/ m (expt 2 1074)))))
		     (else ;normalized flonum
		      (/ (+ m (expt 2 52))
			 (bitwise-arithmetic-shift-left (if pos? 1 -1)
							(- 1075 be))))))))))

  (define ($flonum-signed-mantissa x)
    (let ((b0 ($flonum-u8-ref x 0)))
      (let ((m0 ($fx+ ($flonum-u8-ref x 7)
		      ($fx+ ($fxsll ($flonum-u8-ref x 6) 8)
			    ($fxsll ($flonum-u8-ref x 5) 16))))
	    (m1 ($fx+ ($flonum-u8-ref x 4)
		      ($fx+ ($fxsll ($flonum-u8-ref x 3) 8)
			    ($fxsll ($flonum-u8-ref x 2) 16))))
	    (m2 (let ((b1 ($flonum-u8-ref x 1)))
		  (if (and ($fx= ($fxlogand b0 #x7F) 0)
			   ($fx= ($fxsra b1 4) 0))
		      ($fxlogand b1 #xF)
		    ($fxlogor ($fxlogand b1 #xF) #x10)))))
	(if ($fx= 0 ($fxlogand #x80 b0))
	    (+ (bitwise-arithmetic-shift-left ($fxlogor m1 ($fxsll m2 24)) 24) m0)
	  (+ (bitwise-arithmetic-shift-left
	      ($fx- 0 ($fxlogor m1 ($fxsll m2 24))) 24)
	     ($fx- 0 m0))))))


  #| end of module: $flonum->exact |# )


;;;; numerator and denominator

(define-fl-operation/one flnumerator   $flnumerator)
(define-fl-operation/one fldenominator $fldenominator)

(define ($flnumerator x)
  (cond (($flonum-integer? x)
	 x)
	(($flonum-rational? x)
	 (exact->inexact (numerator ($flonum->exact x))))
	(else x)))

(define ($fldenominator x)
  (cond (($flonum-integer? x)
	 1.0)
	(($flonum-rational? x)
	 (exact->inexact (denominator ($flonum->exact x))))
	((flnan? x)
	 x)
	(else 1.0)))


;;;; even and odd

(define-fl-operation/one fleven? $fleven?)
(define-fl-operation/one flodd?  $flodd?)

(define ($fleven? x)
  ;;FIXME Optimize.  (Abdulaziz Ghuloum)
  (define who '$fleven?)
  (let ((v ($flonum->exact x)))
    (cond ((fixnum? v)
	   ($fx= ($fxlogand v 1) 0))
	  ((bignum? v)
	   (foreign-call "ikrt_even_bn" v))
	  (else
	   (assertion-violation who "not an integer flonum" x)))))

(define ($flodd? x)
  ;;FIXME Optimize.  (Abdulaziz Ghuloum)
  (define who '$flodd?)
  (let ((v ($flonum->exact x)))
    (cond ((fixnum? v)
	   ($fx= ($fxlogand v 1) 1))
	  ((bignum? v)
	   (not (foreign-call "ikrt_even_bn" v)))
	  (else
	   (assertion-violation who "not an integer flonum" x)))))


;;;; predicates

(define (flinteger? x)
  (define who 'flinteger?)
  (with-arguments-validation (who)
      ((flonum	x))
    ($flonum-integer? x)))

(define (flfinite? x)
  (define who 'flfinite?)
  (with-arguments-validation (who)
      ((flonum	x))
    (let ((be (fxlogand ($flonum-sbe x) ($fxsub1 ($fxsll 1 11)))))
      (not ($fx= be 2047)))))

(module (flinfinite? flnan?)

  (define (flinfinite? x)
    (define who 'flinfinite?)
    (with-arguments-validation (who)
	((flonum	x))
      (let ((be (fxlogand ($flonum-sbe x) ($fxsub1 ($fxsll 1 11)))))
	(and ($fx= be 2047) ;nans and infs
	     ($zero-m? x)))))

  (define (flnan? x)
    (define who 'flnan?)
    (with-arguments-validation (who)
	((flonum	x))
      (let ((be (fxlogand ($flonum-sbe x) ($fxsub1 ($fxsll 1 11)))))
	(and ($fx= be 2047) ;;; nans and infs
	     (not ($zero-m? x))))))

  (define ($zero-m? f)
    (and ($fxzero? ($flonum-u8-ref f 7))
	 ($fxzero? ($flonum-u8-ref f 6))
	 ($fxzero? ($flonum-u8-ref f 5))
	 ($fxzero? ($flonum-u8-ref f 4))
	 ($fxzero? ($flonum-u8-ref f 3))
	 ($fxzero? ($flonum-u8-ref f 2))
	 ($fxzero? ($fxlogand ($flonum-u8-ref f 1) #b1111))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define-fl-operation/one flzero? $flzero?)

(define ($flzero? x)
  (let ((be ($fxlogand ($flonum-sbe x) ($fxsub1 ($fxsll 1 11)))))
    (and ($fx= be 0) ;;; denormalized double, only +/-0.0 is integer
	 (and ($fx= ($flonum-u8-ref x 7) 0)
	      ($fx= ($flonum-u8-ref x 6) 0)
	      ($fx= ($flonum-u8-ref x 5) 0)
	      ($fx= ($flonum-u8-ref x 4) 0)
	      ($fx= ($flonum-u8-ref x 3) 0)
	      ($fx= ($flonum-u8-ref x 2) 0)
	      ($fx= ($flonum-u8-ref x 1) 0)))))

(define-fl-operation/one flpositive? $flpositive?)
(define-fl-operation/one flnegative? $flnegative?)

(define ($flpositive? x)
  ($fl> x +0.0))

(define ($flnegative? x)
  ($fl< x -0.0)
  ;;Below  is an  old implementation  from Ikarus.   It does  not behave
  ;;correctly when X = -0.0, which  should return #f.  (Marco Maggi; Sat
  ;;Nov 17, 2012)
  ;;
  ;; (let ((b0 ($flonum-u8-ref x 0)))
  ;;   ($fx> b0 127))
  )

;;; --------------------------------------------------------------------

(define ($flonum-rational? x)
  (let ((be ($fxlogand ($flonum-sbe x)
		       ($fxsub1 ($fxsll 1 11)))))
    ($fx< be 2047)))

(define ($flonum-integer? x)
  (let ((be ($fxlogand ($flonum-sbe x)
		       ($fxsub1 ($fxsll 1 11)))))
    (cond (($fx= be 2047) ;nans and infs
	   #f)
	  (($fx>= be 1075) ;magnitue large enough
	   #t)
	  (($fx= be 0) ;denormalized double, only +/-0.0 is integer
	   (and ($fx= ($flonum-u8-ref x 7) 0)
		($fx= ($flonum-u8-ref x 6) 0)
		($fx= ($flonum-u8-ref x 5) 0)
		($fx= ($flonum-u8-ref x 4) 0)
		($fx= ($flonum-u8-ref x 3) 0)
		($fx= ($flonum-u8-ref x 2) 0)
		($fx= ($flonum-u8-ref x 1) 0)))
	  (($fx< be ($fx+ 1075 -52)) ;too small to be an integer
	   #f)
	  (else
	   ($fl= x (foreign-call "ikrt_fl_round" x ($make-flonum)))))))


;;;; exactness

(define (fixnum->flonum x)
  (define who 'fixnum->flonum)
  (with-arguments-validation (who)
      ((fixnum	x))
    ($fixnum->flonum x)))

(module (inexact->exact exact)

  (define (inexact->exact x)
    ($exact x 'inexact->exact))

  (define (exact x)
    ($exact x 'exact))

  (define ($exact x who)
    (import (ikarus system $compnums))
    (cond ((flonum? x)
	   (or ($flonum->exact x)
	       (%error-no-real-value who x)))
	  ((cflonum? x)
	   (make-rectangular (or ($flonum->exact ($cflonum-real x))
				 (%error-no-real-value who x))
			     (or ($flonum->exact ($cflonum-imag x))
				 (%error-no-real-value who x))))
	  ((or (fixnum?  x)
	       (ratnum?  x)
	       (bignum?  x)
	       (compnum? x))
	   x)
	  (else
	   (assertion-violation who "expected number as argument" x))))

  (define (%error-no-real-value who x)
    (assertion-violation who "number has no real value" x))

  #| end of module |# )


;;;; min max

(module (flmax $flmax)

  (define who 'flmax)

  (define ($flmax x y)
    (if ($fl< x y)
	y
      x))

  (define flmax
    (case-lambda
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($flmax x y)))
     ((x y z . rest)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y)
	   (flonum	z))
	(let loop ((a  ($flmax x y))
		   (b  z)
		   (ls rest))
	  (with-arguments-validation (who)
	      ((flonum	z))
	    (if (null? ls)
		($flmax a b)
	      (loop ($flmax a b) ($car ls) ($cdr ls)))))))
     ((x)
      (with-arguments-validation (who)
	  ((flonum	x))
	x))))

  #| end of module |# )

(module (flmin $flmin)

  (define ($flmin x y)
    (if ($fl< x y) x y))

  (define who 'flmin)

  (define flmin
    (case-lambda
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($flmin x y)))
     ((x y z . rest)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	(let loop ((a  ($flmin x y))
		   (b  z)
		   (ls rest))
	  (with-arguments-validation (who)
	      ((flonum	z))
	    (if (null? ls)
		($flmin a b)
	      (loop ($flmin a b) ($car ls) ($cdr ls)))))))
     ((x)
      (with-arguments-validation (who)
	  ((flonum	x))
	x))))

  #| end of module |# )


;;;; comparison

(let-syntax ((define-flcmp
	       (syntax-rules ()
		 ((_ fl<? $fl<)
		  (module (fl<?)
		    (define who (quote fl<?))

		    (define fl<?
		      (case-lambda
		       ((x y)
			(with-arguments-validation (who)
			    ((flonum	x)
			     (flonum	y))
			  ($fl< x y)))

		       ((x y z)
			(with-arguments-validation (who)
			    ((flonum	x)
			     (flonum	y)
			     (flonum	z))
			  (and ($fl< x y)
			       ($fl< y z))))

		       ((x)
			(or (flonum? x)
			    (assertion-violation who "expected flonum as argument" x)))

		       ((x y . rest)
			(with-arguments-validation (who)
			    ((flonum	x)
			     (flonum	y))
			  (if ($fl< x y)
			      (let loop ((x  y)
					 (y  (car rest))
					 (ls (cdr rest)))
				(with-arguments-validation (who)
				    ((flonum	y))
				  (if (null? ls)
				      ($fl< x y)
				    (if ($fl< x y)
					(loop y ($car ls) ($cdr ls))
				      (%validate-rest ($car ls) ($cdr ls))))))
			    (%validate-rest (car rest) (cdr rest)))))
		       ))

		    (define (%validate-rest a ls)
		      (with-arguments-validation (who)
			  ((flonum	a))
			(if (null? ls)
			    #f
			  (%validate-rest ($car ls) ($cdr ls)))))

		    #| end of module |# )
		  ))))
  (define-flcmp fl=?	$fl=)
  (define-flcmp fl<?	$fl<)
  (define-flcmp fl<=?	$fl<=)
  (define-flcmp fl>?	$fl>)
  (define-flcmp fl>=?	$fl>=))


;;;; arithmetic

(module (fl+)

  (define who 'fl+)

  (define fl+
    (case-lambda
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($fl+ x y)))
     ((x y z)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y)
	   (flonum	z))
	($fl+ ($fl+ x y) z)))
     ((x y z q . rest)
      (with-arguments-validation (who)
	  ((fixnum	x)
	   (fixnum	y)
	   (fixnum	z)
	   (fixnum	q))
	(let loop ((ac   ($fl+ ($fl+ ($fl+ x y) z) q))
		   (rest rest))
	  (if (null? rest)
	      ac
	    (let ((x ($car rest)))
	      (with-arguments-validation (who)
		  ((fixnum	x))
		(loop ($fl+ ac x) ($cdr rest))))))))
     ((x)
      (with-arguments-validation (who)
	  ((flonum	x))
	x))
     (()
      ;;We return always the same flonum object: is this bad?
      0.0)))

  #| end of module |# )

(module (fl-)

  (define who 'fl-)

  (define fl-
    (case-lambda
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($fl- x y)))
     ((x y z)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y)
	   (flonum	z))
	($fl- ($fl- x y) z)))
     ((x y z q . rest)
      (with-arguments-validation (who)
	  ((fixnum	x)
	   (fixnum	y)
	   (fixnum	z)
	   (fixnum	q))
	(let loop ((ac   ($fl- ($fl- ($fl- x y) z) q))
		   (rest rest))
	  (if (null? rest)
	      ac
	    (let ((x ($car rest)))
	      (with-arguments-validation (who)
		  ((fixnum	x))
		(loop ($fl- ac x) ($cdr rest))))))))
     ((x)
      (with-arguments-validation (who)
	  ((fixnum	x))
	($fl* -1.0 x)))))

  #| end of module |# )

(module (fl*)
  (define who 'fl*)

  (define fl*
    (case-lambda
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($fl* x y)))
     ((x y z)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y)
	   (flonum	z))
	($fl* ($fl* x y) z)))
     ((x y z q . rest)
      (with-arguments-validation (who)
	  ((fixnum	x)
	   (fixnum	y)
	   (fixnum	z)
	   (fixnum	q))
	(let loop ((ac   ($fl* ($fl* ($fl* x y) z) q))
		   (rest rest))
	  (if (null? rest)
	      ac
	    (let ((x ($car rest)))
	      (with-arguments-validation (who)
		  ((fixnum	x))
		(loop ($fl* ac x) ($cdr rest))))))))
     ((x)
      (with-arguments-validation (who)
	  ((fixnum	x))
	x))
     (()
      ;;We return always the same flonum object: is this bad?
      1.0)))

  #| end of module |# )

(module (fl/)
  (define who 'fl/)

  (define fl/
    (case-lambda
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($fl/ x y)))
     ((x y z)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y)
	   (flonum	z))
	($fl/ ($fl/ x y) z)))
     ((x y z q . rest)
      (with-arguments-validation (who)
	  ((fixnum	x)
	   (fixnum	y)
	   (fixnum	z)
	   (fixnum	q))
	(let loop ((ac   ($fl/ ($fl/ ($fl/ x y) z) q))
		   (rest rest))
	  (if (null? rest)
	      ac
	    (let ((x ($car rest)))
	      (with-arguments-validation (who)
		  ((fixnum	x))
		(loop ($fl/ ac x) ($cdr rest))))))))
     ((x)
      (with-arguments-validation (who)
	  ((fixnum	x))
	($fl/ 1.0 x)))))

  #| end of module |# )


;;;; functions

(define-fl-operation/one flabs $flabs)
(define ($flabs x)
  (if ($fx> ($flonum-u8-ref x 0) 127)
      ($fl* x -1.0)
    x))

(define-fl-operation/one/forcall flsin $flsin "ikrt_fl_sin")
(define-fl-operation/one/forcall flcos $flcos "ikrt_fl_cos")
(define-fl-operation/one/forcall fltan $fltan "ikrt_fl_tan")

(define-fl-operation/one/forcall flasin $flasin "ikrt_fl_asin")
(define-fl-operation/one/forcall flacos $flacos "ikrt_fl_acos")

(module (flatan $flatan $flatan2)

  (define who 'flatan)

  (define flatan
    (case-lambda
     ((x)
      (with-arguments-validation (who)
	  ((flonum	x))
	($flatan x)))
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($flatan2 x y)))))

  (define ($flatan x)
    (foreign-call "ikrt_fl_atan" x))

  (define ($flatan2 x y)
    (foreign-call "ikrt_atan2" x y))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define-fl-operation/one flexp $flexp)

(define ($flexp x)
  (foreign-call "ikrt_fl_exp" x ($make-flonum)))

(define-fl-operation/one flexpm1 $flexpm1)

(define ($flexpm1 x)
  (foreign-call "ikrt_fl_expm1" x ($make-flonum)))

(define-fl-operation/one/forcall fllog1p $fllog1p "ikrt_fl_log1p")

(module (fllog $fllog $fllog2)

  (define who 'fllog)

  (define fllog
    (case-lambda
     ((x)
      (with-arguments-validation (who)
	  ((flonum	x))
	($fllog x)))
     ((x y)
      (with-arguments-validation (who)
	  ((flonum	x)
	   (flonum	y))
	($fllog2 x y)))))

  (define ($fllog x)
    (foreign-call "ikrt_fl_log" x))

  (define ($fllog2 x y)
    ($fl/ (foreign-call "ikrt_fl_log" x)
	  (foreign-call "ikrt_fl_log" y)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define-fl-operation/two flexpt $flexpt)

(define ($flexpt x y)
  (let ((y^ ($flonum->exact y)))
    ;;FIXME Performance bottleneck?  (Abdulaziz Ghuloum)
    (cond ((fixnum? y^)
	   (inexact (expt x y^)))
	  ((bignum? y^)
	   (inexact (expt x y^)))
	  (else
	   (foreign-call "ikrt_flfl_expt" x y ($make-flonum))))))

(define-fl-operation/one/forcall flsqrt $flsqrt "ikrt_fl_sqrt")


;;;; debugging functions

(define (flonum->bytevector flo)
  (define who 'flonum->bytevector)
  (with-arguments-validation (who)
      ((flonum	flo))
    (foreign-call "ikrt_debug_flonum_to_bytevector" flo)))

(define (bytevector->flonum bv)
  (define who 'bytevector->flonum)
  (with-arguments-validation (who)
      ((bytevector	bv))
    (foreign-call "ikrt_debug_flonum_from_bytevector" bv)))


;;;; done

)

(library (ikarus system flonums)
  (export $fixnum->flonum)
  (import (ikarus))
  (define $fixnum->flonum fixnum->flonum))

;;; end of file
;; Local Variables:
;; eval: (put 'cond-numeric-operand 'scheme-indent-function 1)
;; End:
