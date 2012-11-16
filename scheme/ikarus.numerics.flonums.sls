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
    $flround
    $flonum-integer?	$flonum-rational?
    $flonum->exact
    $flzero?		$flnegative?

    inexact->exact	exact
    flabs		flpositive?
    fixnum->flonum
    flsin		flasin
    flcos		flacos
    fltan		flatan
    fleven?		flodd?
    flfloor		flceiling
    flnumerator		fldenominator
    flexp		fllog
    flexpm1		fllog1p
    flexpt
    flinteger?		flnan?
    flfinite?		flinfinite?
    flonum-parts	flonum-bytes
    flround

    bytevector->flonum	flonum->bytevector)
  (import (except (ikarus)
		  inexact->exact	exact
		  flabs			flpositive?
		  fixnum->flonum
		  flsin			flasin
		  flcos			flacos
		  fltan			flatan
		  fleven?		flodd?
		  flfloor		flceiling
		  flnumerator		fldenominator
		  flexp			fllog
		  flexpm1		fllog1p
		  flexpt
		  flonum-parts		flonum-bytes
		  flinteger?		flnan?
		  flfinite?		flinfinite?
		  flround
		  bytevector->flonum	flonum->bytevector)
    (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $bytevectors)
    (only (ikarus system $flonums)
	  $fl>=
	  $flonum-sbe)
    (except (ikarus system $flonums)
	    $flonum-rational?
            $flonum-integer?
	    $flround)
    (vicare arguments validation)
    (only (vicare syntactic-extensions)
	  cond-numeric-operand))


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


(define ($zero-m? f)
  (and ($fxzero? ($flonum-u8-ref f 7))
       ($fxzero? ($flonum-u8-ref f 6))
       ($fxzero? ($flonum-u8-ref f 5))
       ($fxzero? ($flonum-u8-ref f 4))
       ($fxzero? ($flonum-u8-ref f 3))
       ($fxzero? ($flonum-u8-ref f 2))
       ($fxzero? ($fxlogand ($flonum-u8-ref f 1) #b1111))))

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


;;;; rounding

(define (flround x)
  (define who 'flround)
  (with-arguments-validation (who)
      ((flonum	x))
    ($flround x)))

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


(define (flnumerator x)
  (unless (flonum? x)
    (die 'flnumerator "not a flonum" x))
  (cond
   (($flonum-integer? x) x)
   (($flonum-rational? x)
    (exact->inexact (numerator ($flonum->exact x))))
   (else x)))

(define (fldenominator x)
  (unless (flonum? x)
    (die 'fldenominator "not a flonum" x))
  (cond
   (($flonum-integer? x) 1.0)
   (($flonum-rational? x)
    (exact->inexact (denominator ($flonum->exact x))))
   ((flnan? x) x)
   (else 1.0)))

(define (fleven? x)
    ;;; FIXME: optimize
  (unless (flonum? x)
    (die 'fleven? "not a flonum" x))
  (let ((v ($flonum->exact x)))
    (cond
     ((fixnum? v) ($fx= ($fxlogand v 1) 0))
     ((bignum? v)
      (foreign-call "ikrt_even_bn" v))
     (else (die 'fleven? "not an integer flonum" x)))))

(define (flodd? x)
  (unless (flonum? x)
    (die 'flodd? "not a flonum" x))
    ;;; FIXME: optimize
  (let ((v ($flonum->exact x)))
    (cond
     ((fixnum? v) ($fx= ($fxlogand v 1) 1))
     ((bignum? v)
      (not (foreign-call "ikrt_even_bn" v)))
     (else (die 'flodd? "not an integer flonum" x)))))

(define (flinteger? x)
  (if (flonum? x)
      ($flonum-integer? x)
    (die 'flinteger? "not a flonum" x)))

(define (flinfinite? x)
  (if (flonum? x)
      (let ((be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))))
	(and (fx= be 2047)  ;;; nans and infs
	     ($zero-m? x)))
    (die 'flinfinite? "not a flonum" x)))

(define (flnan? x)
  (if (flonum? x)
      (let ((be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))))
	(and (fx= be 2047)  ;;; nans and infs
	     (not ($zero-m? x))))
    (die 'flnan? "not a flonum" x)))

(define (flfinite? x)
  (if (flonum? x)
      (let ((be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))))
	(not (fx= be 2047)))
    (die 'flfinite? "not a flonum" x)))

(define ($flzero? x)
  (let ((be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))))
    (and
     (fx= be 0) ;;; denormalized double, only +/-0.0 is integer
     (and (fx= ($flonum-u8-ref x 7) 0)
	  (fx= ($flonum-u8-ref x 6) 0)
	  (fx= ($flonum-u8-ref x 5) 0)
	  (fx= ($flonum-u8-ref x 4) 0)
	  (fx= ($flonum-u8-ref x 3) 0)
	  (fx= ($flonum-u8-ref x 2) 0)
	  (fx= ($flonum-u8-ref x 1) 0)))))

(define ($flnegative? x)
  (let ((b0 ($flonum-u8-ref x 0)))
    (fx> b0 127)))

(define ($exact x who)
  (import (ikarus system $compnums))
  (cond
   ((flonum? x)
    (or ($flonum->exact x)
	(die who "number has no real value" x)))
   ((cflonum? x)
    (make-rectangular
     (or ($flonum->exact ($cflonum-real x))
	 (die who "number has no real value" x))
     (or ($flonum->exact ($cflonum-imag x))
	 (die who "number has no real value" x))))
   ((or (fixnum? x) (ratnum? x) (bignum? x) (compnum? x)) x)
   (else (die who "not a number" x))))

(define (inexact->exact x) ($exact x 'inexact->exact))
(define (exact x) ($exact x 'exact))


(define (flpositive? x)
  (if (flonum? x)
      ($fl> x 0.0)
    (die 'flpositive? "not a flonum" x)))

(define (flabs x)
  (if (flonum? x)
      (if ($fx> ($flonum-u8-ref x 0) 127)
	  ($fl* x -1.0)
	x)
    (die 'flabs "not a flonum" x)))

(define (fixnum->flonum x)
  (if (fixnum? x)
      ($fixnum->flonum x)
    (die 'fixnum->flonum "not a fixnum")))

(define (flsin x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_sin" x)
    (die 'flsin "not a flonum" x)))

(define (flcos x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_cos" x)
    (die 'flcos "not a flonum" x)))

(define (fltan x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_tan" x)
    (die 'fltan "not a flonum" x)))

(define (flasin x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_asin" x)
    (die 'flasin "not a flonum" x)))

(define (flacos x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_acos" x)
    (die 'flacos "not a flonum" x)))

(define flatan
  (case-lambda
   ((x)
    (if (flonum? x)
	(foreign-call "ikrt_fl_atan" x)
      (die 'flatan "not a flonum" x)))
   ((x y)
    (if (flonum? x)
	(if (flonum? y)
	    (foreign-call "ikrt_atan2" x y)
	  (die 'flatan "not a flonum" y))
      (die 'flatan "not a flonum" x)))))

(define (flfloor x)
  (define (ratnum-floor x)
    (let ((n (numerator x)) (d (denominator x)))
      (let ((q (quotient n d)))
	(if (>= n 0) q (- q 1)))))
  (cond
   ((flonum? x)
       ;;; optimize for integer flonums case
    (let ((e ($flonum->exact x)))
      (cond
       ((ratnum? e)
	(exact->inexact (ratnum-floor e)))
       (else x))))
   (else (die 'flfloor "not a flonum" x))))

(define (flceiling x)
  (cond
   ((flonum? x)
       ;;; optimize for integer flonums case
    (let ((e ($flonum->exact x)))
      (cond
       ((ratnum? e)
	(exact->inexact (ceiling e)))
       (else x))))
   (else (die 'flceiling "not a flonum" x))))

(define (flexp x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_exp" x ($make-flonum))
    (die 'flexp "not a flonum" x)))

(define (flexpm1 x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_expm1" x ($make-flonum))
    (die 'flexpm1 "not a flonum" x)))

(define fllog
  (case-lambda
   ((x)
    (if (flonum? x)
	(foreign-call "ikrt_fl_log" x)
      (die 'fllog "not a flonum" x)))
   ((x y)
    (if (flonum? x)
	(if (flonum? y)
	    (fl/ (foreign-call "ikrt_fl_log" x)
		 (foreign-call "ikrt_fl_log" y))
	  (die 'fllog "not a flonum" y))
      (die 'fllog "not a flonum" x)))))

(define (fllog1p x)
  (if (flonum? x)
      (foreign-call "ikrt_fl_log1p" x)
    (die 'fllog1p "not a flonum" x)))

(define (flexpt x y)
  (if (flonum? x)
      (if (flonum? y)
	  (let ((y^ ($flonum->exact y)))
              ;;; FIXME: performance bottleneck?
	    (cond
	     ((fixnum? y^) (inexact (expt x y^)))
	     ((bignum? y^) (inexact (expt x y^)))
	     (else
	      (foreign-call "ikrt_flfl_expt" x y ($make-flonum)))))
	(die 'flexpt "not a flonum" y))
    (die 'fllog "not a flonum" x)))


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
