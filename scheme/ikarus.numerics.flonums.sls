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


(library (ikarus numerics flonums)
  (export
    list-of-flonums?

    inexact->exact	exact		$flonum->exact
    fixnum->flonum	$fixnum->flonum

    flzero?		$flzero?
    flzero?/positive	$flzero?/positive
    flzero?/negative	$flzero?/negative
    flpositive?		$flpositive?
    flnegative?		$flnegative?
    flnonnegative?	$flnonnegative?
    flnonpositive?	$flnonpositive?
    fleven?		$fleven?
    flodd?		$flodd?
			$flonum-integer?
			$flonum-rational?

    zero-flonum?
    positive-zero-flonum?	negative-zero-flonum?
    positive-flonum?		negative-flonum?
    non-negative-flonum?	non-positive-flonum?

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
    flsinh		$flsinh
    flcosh		$flcosh
    fltanh		$fltanh
    flasinh		$flasinh
    flacosh		$flacosh
    flatanh		$flatanh
    flexp		$flexp
    fllog		$fllog		$fllog2
    flexpm1		$flexpm1
    fllog1p		$fllog1p
    flexpt		$flexpt
    flsquare		$flsquare
    flcube		$flcube
    flsqrt		$flsqrt
    flcbrt		$flcbrt
    flhypot		$flhypot

    flinteger?
    flnan?		$flnan?
    flfinite?		$flfinite?
    flinfinite?		$flinfinite?

    fl=?		fl!=?		$fl=		$fl!=
    fl<?		fl>?		$fl<		$fl<=
    fl<=?		fl>=?		$fl>		$fl>=
    fl+			fl-
    fl*			fl/

    flmax		$flmax
    flmin		$flmin

    flonum-parts	flonum-bytes
    bytevector->flonum	flonum->bytevector)
  (import (except (vicare)
		  list-of-flonums?
		  inexact->exact	exact		fixnum->flonum
		  flzero?		flpositive?	flnegative?
		  flzero?/positive	flzero?/negative
		  flnonnegative?	flnonpositive?
		  zero-flonum?
		  positive-zero-flonum?	negative-zero-flonum?
		  positive-flonum?	negative-flonum?
		  non-negative-flonum?	non-positive-flonum?
		  fleven?		flodd?		flround
		  flfloor		flceiling	fltruncate
		  flnumerator		fldenominator	flabs
		  flsin			flcos		fltan
		  flasin		flacos		flatan
		  flsinh		flcosh		fltanh
		  flasinh		flacosh		flatanh
		  flexp			fllog		flexpm1
		  fllog1p		flexpt		flsqrt
		  flsquare		flcube		flhypot
		  flcbrt
		  flinteger?		flnan?		flfinite?
		  flinfinite?
		  fl=?			fl!=?
		  fl<?
		  fl>?			fl<=?		fl>=?
		  fl+			fl-		fl*
		  fl/			flmax		flmin
		  flonum-parts		flonum-bytes
		  bytevector->flonum	flonum->bytevector)
    (vicare system $pairs)
    (vicare system $fx)
    (vicare system $bignums)
    (vicare system $ratnums)
    (vicare system $bytevectors)
    (except (vicare system $flonums)
	    $fixnum->flonum
	    $flonum->exact
	    $flzero?
	    $flzero?/positive
	    $flzero?/negative
	    $flpositive?
	    $flnegative?
	    $flnonnegative?
	    $flnonpositive?
	    $fleven?
	    $flodd?
	    $flfinite?
	    $flinfinite?
	    $flnan?
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
	    $flsinh
	    $flcosh
	    $fltanh
	    $flasinh
	    $flacosh
	    $flatanh
	    $flexp
	    $fllog		$fllog2
	    $flexpm1
	    $fllog1p
	    $flexpt
	    $flsqrt
	    $flcbrt
	    $flsquare
	    $flcube
	    $flhypot
	    $flmax
	    $flmin
	    $fl=		$fl!=
	    $fl<		$fl<=
	    $fl>		$fl>=)
    (prefix (vicare system $flonums) sys::)
    (only (vicare language-extensions syntaxes)
	  cond-numeric-operand
	  cond-exact-integer-operand
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-unsafe-equality/sorting-predicate
	  define-inequality-predicate
	  define-unsafe-inequality-predicate))


;;;; helpers

(define-syntax define-fl-operation/one
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define* (?safe-who {x flonum?})
       (?unsafe-who x)))))

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
     (define* (?safe-who {x flonum?} {y flonum?})
       (?unsafe-who x y)))))

(define-syntax define-fl-operation/two/forcall
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who ?foreign-who)
     (begin
       (define-fl-operation/two ?safe-who ?unsafe-who)
       (define (?unsafe-who x y)
	 (foreign-call ?foreign-who x y))))))


;;;; predicates

(define-list-of-type-predicate list-of-flonums? flonum?)


;;;; flonums parts

(define* (flonum-bytes {x flonum?})
  ($flonum-bytes x))

(define ($flonum-bytes x)
  (values ($flonum-u8-ref x 0)
	  ($flonum-u8-ref x 1)
	  ($flonum-u8-ref x 2)
	  ($flonum-u8-ref x 3)
	  ($flonum-u8-ref x 4)
	  ($flonum-u8-ref x 5)
	  ($flonum-u8-ref x 6)
	  ($flonum-u8-ref x 7)))

(define* (flonum-parts {x flonum?})
  (receive (b0 b1 b2 b3 b4 b5 b6 b7)
      ($flonum-bytes x)
    (values ($fxzero? ($fxlogand b0 128))
	    (+ ($fxsll ($fxlogand b0 127) 4)
	       ($fxsra b1 4))
	    (+ (+ b7 ($fxsll b6 8) ($fxsll b5 16))
	       (* (+ b4
		     ($fxsll b3 8)
		     ($fxsll b2 16)
		     ($fxsll ($fxlogand b1 #b1111) 24))
		  (expt 2 24))))))


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
	(receive (positive? be mantissa)
	    (flonum-parts x)
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

(define* ($fleven? x)
  (let ((v ($flonum->exact x)))
    (cond-exact-integer-operand v
      ((fixnum?)	($fxeven? v))
      ((bignum?)	($bignum-even? v))
      (else
       (assertion-violation __who__ "not an integer flonum" x)))))

(define* ($flodd? x)
  (let ((v ($flonum->exact x)))
    (cond-exact-integer-operand v
      ((fixnum?)	($fxodd? v))
      ((bignum?)	($bignum-odd? v))
      (else
       (assertion-violation __who__ "not an integer flonum" x)))))


;;;; predicates

(define-fl-operation/one flinteger?	$flonum-integer?)
(define-fl-operation/one flfinite?	$flfinite?)
(define-fl-operation/one flinfinite?	$flinfinite?)
(define-fl-operation/one flnan?		$flnan?)

(define ($flfinite? x)
  (let ((be (fxlogand ($flonum-sbe x) ($fxsub1 ($fxsll 1 11)))))
    (not ($fx= be 2047))))

(module ($flinfinite? $flnan?)

  (define ($flinfinite? x)
    (let ((be (fxlogand ($flonum-sbe x) ($fxsub1 ($fxsll 1 11)))))
      (and ($fx= be 2047) ;nans and infs
	   ($zero-m? x))))

  (define ($flnan? x)
    (let ((be (fxlogand ($flonum-sbe x) ($fxsub1 ($fxsll 1 11)))))
      (and ($fx= be 2047) ;;; nans and infs
	   (not ($zero-m? x)))))

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
(define-fl-operation/one flzero?/positive $flzero?/positive)
(define-fl-operation/one flzero?/negative $flzero?/negative)

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

(define ($flzero?/positive x)
  (and ($flzero? x)
       ($fxzero? ($fxlogand ($flonum-u8-ref x 0) 128))))

(define ($flzero?/negative x)
  (and ($flzero? x)
       (not ($fxzero? ($fxlogand ($flonum-u8-ref x 0) 128)))))

;;; --------------------------------------------------------------------

(define-fl-operation/one flpositive?		$flpositive?)
(define-fl-operation/one flnegative?		$flnegative?)
(define-fl-operation/one flnonpositive?		$flnonpositive?)
(define-fl-operation/one flnonnegative?		$flnonnegative?)

(define ($flpositive? x)
  (sys::$fl> x +0.0))

(define ($flnegative? x)
  (sys::$fl< x -0.0))
;;NOTE Below is an old implementation from Ikarus.  It does not behave correctly when
;;X = -0.0, which should return #f.  (Marco Maggi; Sat Nov 17, 2012)
;;
;;   (define ($flnegative? x)
;;     (let ((b0 ($flonum-u8-ref x 0)))
;;       ($fx> b0 127))
;;

(define ($flnonpositive? x)
  (or ($flzero?/negative x)
      ($flnegative? x)))

(define ($flnonnegative? x)
  (or ($flzero?/positive x)
      ($flpositive? x)))

;;; --------------------------------------------------------------------

(define (zero-flonum? obj)
  (and (flonum?  obj)
       ($flzero? obj)))

(define (positive-zero-flonum? obj)
  (and (flonum?           obj)
       ($flzero?/positive obj)))

(define (negative-zero-flonum? obj)
  (and (flonum?           obj)
       ($flzero?/negative obj)))

(define (positive-flonum? obj)
  (and (flonum?      obj)
       ($flpositive? obj)))

(define (negative-flonum? obj)
  (and (flonum?      obj)
       ($flnegative? obj)))

(define (non-negative-flonum? obj)
  (and (flonum?         obj)
       ($flnonnegative? obj)))

(define (non-positive-flonum? obj)
  (and (flonum?         obj)
       ($flnonpositive? obj)))

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
	   (sys::$fl= x (foreign-call "ikrt_fl_round" x ($make-flonum)))))))


;;;; exactness

(define* (fixnum->flonum {x fixnum?})
  ($fixnum->flonum x))

(define ($fixnum->flonum fx)
  (sys::$fixnum->flonum fx))

(module (inexact->exact
	 exact
	 $flexact
	 $cflexact)

  (define* (inexact->exact x)
    ($exact x __who__))

  (define* (exact x)
    ($exact x __who__))

  (define ($exact x who)
    (cond-numeric-operand x
      ((flonum?)	($flexact x))
      ((cflonum?)	($cflexact x))
      ((fixnum?)	x)
      ((bignum?)	x)
      ((ratnum?)	x)
      ((compnum?)	x)
      (else
       (procedure-argument-violation who "expected number as argument" x))))

  (define* ($flexact x)
    (or ($flonum->exact x)
	(%error-no-real-value __who__ x)))

  (define* ($cflexact x)
    (import (vicare system $compnums))
    (make-rectangular (or ($flonum->exact ($cflonum-real x)) (%error-no-real-value __who__ x))
		      (or ($flonum->exact ($cflonum-imag x)) (%error-no-real-value __who__ x))))

  (define (%error-no-real-value who x)
    (procedure-argument-violation who "number has no real value" x))

  #| end of module |# )

(module ($flonum->exact)

  (define ($flonum->exact x)
    (let* ((sbe ($flonum-sbe x))
	   (be  ($fxlogand sbe #x7FF)))
      (cond (($fx= be 2047) ;nans/infs
	     #f)
	    (($fx>= be 1075) ;magnitude large enough to be an integer
	     (bitwise-arithmetic-shift-left ($flonum-signed-mantissa x)
					    ($fx- be 1075)))
	    (else
	     ;;This really needs to get optimized.
	     (receive (pos? be m)
		 (flonum-parts x)
	       (cond (($fx= be 0) ;denormalized
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
	    (+ (bitwise-arithmetic-shift-left ($fxlogor m1 ($fxsll m2 24)) 24)
	       m0)
	  (+ (bitwise-arithmetic-shift-left ($fx- 0 ($fxlogor m1 ($fxsll m2 24))) 24)
	     ($fx- 0 m0))))))

  #| end of module: $flonum->exact |# )


;;;; min max

(define-min/max-comparison flmax $flmax flonum?)
(define-min/max-comparison flmin $flmin flonum?)

(case-define $flmax
  ((x) x)
  ((x y)
   (cond (($flnan? x)		+nan.0)
	 (($flnan? y)		+nan.0)
	 ((sys::$fl< x y)	y)
	 (else			x)))
  ((x y . z*)
   (apply $flmax ($flmax x y) z*)))

(case-define $flmin
  ((x) x)
  ((x y)
   (cond (($flnan? x)		+nan.0)
	 (($flnan? y)		+nan.0)
	 ((sys::$fl< x y)	x)
	 (else			y)))
  ((x y . z*)
   (apply $flmin ($flmin x y) z*)))


;;;; comparison

(define-equality/sorting-predicate fl=?		sys::$fl=	flonum?)
(define-equality/sorting-predicate fl<?		sys::$fl<	flonum?)
(define-equality/sorting-predicate fl<=?	sys::$fl<=	flonum?)
(define-equality/sorting-predicate fl>?		sys::$fl>	flonum?)
(define-equality/sorting-predicate fl>=?	sys::$fl>=	flonum?)
(define-inequality-predicate       fl!=?	sys::$fl!=	flonum?)

(define-unsafe-equality/sorting-predicate $fl=		sys::$fl=)
(define-unsafe-equality/sorting-predicate $fl<		sys::$fl<)
(define-unsafe-equality/sorting-predicate $fl>		sys::$fl>)
(define-unsafe-equality/sorting-predicate $fl<=		sys::$fl<=)
(define-unsafe-equality/sorting-predicate $fl>=		sys::$fl>=)
(define-unsafe-inequality-predicate       $fl!=		sys::$fl!=)


;;;; arithmetic

(let-syntax
    ((define-arithmetic-operation
       (syntax-rules ()
	 ((_ ?who ?unary-unsafe-who ?binary-unsafe-who ?type-pred)
	  (case-define* ?who
	    (({x ?type-pred} {y ?type-pred})
	     (?binary-unsafe-who x y))

	    (({x ?type-pred} {y ?type-pred} {z ?type-pred})
	     (?binary-unsafe-who (?binary-unsafe-who x y) z))

	    (({x ?type-pred} {y ?type-pred} {z ?type-pred} {w ?type-pred} . {rest flonum?})
	     (let loop ((ac   (?binary-unsafe-who (?binary-unsafe-who (?binary-unsafe-who x y) z) w))
			(rest rest))
	       (if (pair? rest)
		   (loop (?binary-unsafe-who ac (car rest))
			 (cdr rest))
		 ac)))

	    (({x ?type-pred})
	     (?unary-unsafe-who x))

	    (()
	     ;;We always return the same flonum object: is this bad?
	     0.0)))
	 )))
  (define-arithmetic-operation fl+ $unary-fl+ $fl+ flonum?)
  (define-arithmetic-operation fl- $unary-fl- $fl- flonum?)
  (define-arithmetic-operation fl* $unary-fl* $fl* flonum?)
  (define-arithmetic-operation fl/ $unary-fl/ $fl/ flonum?)
  #| end of LET-SYNTAX |# )

(define-syntax-rule ($unary-fl+ ?fl)
  ?fl)

(define-syntax-rule ($unary-fl- ?fl)
  ($fl- ?fl))

(define-syntax-rule ($unary-fl* ?fl)
  ?fl)

(define-syntax-rule ($unary-fl/ ?fl)
  ($fl/ 1.0 ?fl))


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

(case-define* flatan
  (({x flonum?})
   ($flatan x))
  (({x flonum?} {y flonum?})
   ($flatan2 x y)))

(case-define $flatan
  ((x)
   (foreign-call "ikrt_fl_atan" x))
  ((x y)
   (foreign-call "ikrt_atan2" x y)))

(define ($flatan2 x y)
  (foreign-call "ikrt_atan2" x y))

;;; --------------------------------------------------------------------

(define-fl-operation/one/forcall flsinh $flsinh "ikrt_fl_sinh")
(define-fl-operation/one/forcall flcosh $flcosh "ikrt_fl_cosh")
(define-fl-operation/one/forcall fltanh $fltanh "ikrt_fl_tanh")

(define-fl-operation/one/forcall flasinh $flasinh "ikrt_fl_asinh")
(define-fl-operation/one/forcall flacosh $flacosh "ikrt_fl_acosh")
(define-fl-operation/one/forcall flatanh $flatanh "ikrt_fl_atanh")

;;; --------------------------------------------------------------------

(define-fl-operation/one flexp $flexp)

(define ($flexp x)
  (foreign-call "ikrt_fl_exp" x ($make-flonum)))

(define-fl-operation/one flexpm1 $flexpm1)

(define ($flexpm1 x)
  (foreign-call "ikrt_fl_expm1" x ($make-flonum)))

(define-fl-operation/one/forcall fllog1p $fllog1p "ikrt_fl_log1p")

(case-define* fllog
  (({x flonum?})
   ($fllog x))
  (({x flonum?} {y flonum?})
   ($fllog2 x y)))

(case-define $fllog
  ((x)
   (foreign-call "ikrt_fl_log" x))
  ((x y)
   ($fl/ (foreign-call "ikrt_fl_log" x)
	 (foreign-call "ikrt_fl_log" y))))

(define ($fllog2 x y)
  ($fl/ (foreign-call "ikrt_fl_log" x)
	(foreign-call "ikrt_fl_log" y)))

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
(define-fl-operation/one/forcall flcbrt $flcbrt "ikrt_fl_cbrt")
(define-fl-operation/two/forcall flhypot $flhypot "ikrt_fl_hypot")

(define-fl-operation/one flsquare	$flsquare)
(define-fl-operation/one flcube		$flcube)

(define ($flsquare x)
  ($fl* x x))

(define ($flcube x)
  ($fl* ($flsquare x) x))


;;;; debugging functions

(define* (flonum->bytevector {flo flonum?})
  (foreign-call "ikrt_debug_flonum_to_bytevector" flo))

(define* (bytevector->flonum {bv bytevector?})
  (foreign-call "ikrt_debug_flonum_from_bytevector" bv))


;;;; done

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.numerics.flonums"))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'cond-numeric-operand 'scheme-indent-function 1)
;; End:
