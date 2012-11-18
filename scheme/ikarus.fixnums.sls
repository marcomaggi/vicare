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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus fixnums)
  (export
    fxzero?
    fxpositive?		fxnegative?
    fxeven?		fxodd?

    fxadd1		fxsub1
    fx+			fx-
    fx*
    fx+/carry		fx*/carry
    fx-/carry
    fxquotient		fxremainder
    fxmodulo		fxsign

    fxlogor		fxlogand
    fxlogxor		fxlognot
    fxior		fxand
    fxxor		fxnot
    fxif
    fxsll		fxsra
    fxarithmetic-shift-left
    fxarithmetic-shift-right
    fxarithmetic-shift

    fx=			fx=?
    fx<			fx<?
    fx<=		fx<=?
    fx>			fx>?
    fx>=		fx>=?
    fxmin		fxmax

    fixnum->string

;;; --------------------------------------------------------------------

    $fxpositive?	$fxnegative?
    $fxeven?		$fxodd?
    $fxmodulo		$fxremainder
    $fxsign

;;; --------------------------------------------------------------------

    error@fx+		error@fx*
    error@fx-		error@fxadd1
    error@fxsub1
    error@fxarithmetic-shift-left
    error@fxarithmetic-shift-right)
  (import (except (ikarus)
		  fxzero?
		  fxpositive?		fxnegative?
		  fxeven?		fxodd?

		  fxquotient		fxremainder
		  fxmodulo		fxsign

		  fxadd1		fxsub1
		  fx+			fx-
		  fx*
		  fx+/carry		fx*/carry
		  fx-/carry

		  fxlogor		fxlogand
		  fxlogxor		fxlognot
		  fxsll			fxsra

		  fx=			fx=?
		  fx<			fx<?
		  fx<=			fx<=?
		  fx>			fx>?
		  fx>=			fx>=?
		  fxmin			fxmax

		  fxior			fxand
		  fxxor			fxnot
		  fxif

		  fxarithmetic-shift-left
		  fxarithmetic-shift-right
		  fxarithmetic-shift

		  fixnum->string)
    (prefix (only (ikarus)
		  fx+ fx* fx-)
	    sys:)
    (except (ikarus system $fx)
	    $fxpositive?	$fxnegative?
	    $fxeven?		$fxodd?
	    $fxmodulo		$fxremainder
	    $fxsign)
    (ikarus system $chars)
    (ikarus system $pairs)
    (ikarus system $strings)
    (vicare arguments validation))


;;;; helpers

(define (%overflow-violation who . args)
  (raise
   (condition (make-implementation-restriction-violation)
	      (make-who-condition who)
	      (make-message-condition "overflow")
	      (make-irritants-condition args))))

(define-syntax define-fx-operation/one
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who x)
       (define who (quote ?safe-who))
       (with-arguments-validation (who)
	   ((fixnum	x))
	 (?unsafe-who x))))))

(define-syntax define-fx-operation/two
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who x y)
       (define who (quote ?safe-who))
       (with-arguments-validation (who)
	   ((fixnum	x)
	    (fixnum	y))
	 (?unsafe-who x y))))))

(define-syntax define-fx-operation/shift
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who x y)
       (define who (quote ?safe-who))
       (with-arguments-validation (who)
	   ((fixnum		x)
	    (fixnum-shift	y))
	 (?unsafe-who x y))))))

(define-argument-validation (fixnum-shift who obj)
  (and (fixnum? obj)
       ($fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as shfit argument" obj))

(define-syntax define-fx-operation/three
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who x y z)
       (define who (quote ?safe-who))
       (with-arguments-validation (who)
	   ((fixnum	x)
	    (fixnum	y)
	    (fixnum	z))
	 (?unsafe-who x y))))))

(define-syntax define-fx-operation/div
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who x y)
       (define who (quote ?safe-who))
       (with-arguments-validation (who)
	   ((fixnum		x)
	    (fixnum		y)
	    (non-zero-fixnum	y))
	 (?unsafe-who x y))))))


;;;; predicates

(define (fxzero? x)
  (cond ((eq? x 0)	#t)
	((fixnum? x)	#f)
	(else
	 (assertion-violation 'fxzero? "expected fixnum as argument" x))))

(define ($fxpositive? N)	($fx> N 0))
(define ($fxnegative? N)	($fx< N 0))
(define ($fxeven?     N)	($fxzero? ($fxlogand N 1)))
(define ($fxodd?      N)	(not ($fxzero? ($fxlogand N 1))))

(define-fx-operation/one fxpositive?	$fxpositive?)
(define-fx-operation/one fxnegative?	$fxnegative?)
(define-fx-operation/one fxeven?	$fxeven?)
(define-fx-operation/one fxodd?		$fxodd?)


;;;; bitwise logic operations

(define (fxlognot x)
  (define who 'fxlognot)
  (with-arguments-validation (who)
      ((fixnum	x))
    ($fxlognot x)))

(define fxnot fxlognot)

(let-syntax
    ((define-fxbitop
       (syntax-rules ()
	 ((_ ?who1 ?who2 ?unsafe-op ?identity)
	  (module (?who1 ?who2)
	    (define who (quote ?who1))

	    (define ?who1
	      (case-lambda
	       ((x y)
		(with-arguments-validation (who)
		    ((fixnum	x)
		     (fixnum	y))
		  (?unsafe-op x y)))
	       ((x y . ls)
		(with-arguments-validation (who)
		    ((fixnum	x)
		     (fixnum	y))
		  (let loop ((a  (?unsafe-op x y))
			     (ls ls))
		    (if (pair? ls)
			(let ((b ($car ls)))
			  (with-arguments-validation (who)
			      ((fixnum	b))
			    (loop (?unsafe-op a b) ($cdr ls))))
		      a))))
	       ((x)
		(with-arguments-validation (who)
		    ((fixnum	x))
		  x))
	       (()
		?identity)))

	    (define ?who2 ?who1)

	    #| end of module |# )
	  ))))

  (define-fxbitop fxlogor	fxior		$fxlogor	 0)
  (define-fxbitop fxlogand	fxand		$fxlogand	-1)
  (define-fxbitop fxlogxor	fxxor		$fxlogxor	 0))


;;;; bitwise operations

(define-fx-operation/shift fxsra $fxsra)
(define-fx-operation/shift fxsll $fxsll)
(define-fx-operation/three fxif  $fxif)

(define ($fxif x y z)
  ($fxlogor ($fxlogand x             y)
	    ($fxlogand ($fxlognot x) z)))

;;; --------------------------------------------------------------------

(define (fxarithmetic-shift-right x y)
  (import (ikarus))
  (fxarithmetic-shift-right x y))

(define (fxarithmetic-shift-left x y)
  (import (ikarus))
  (fxarithmetic-shift-left x y))

(module (fxarithmetic-shift)

  (define (fxarithmetic-shift x y)
    (import (ikarus))
    (define who 'fxarithmetic-shift)
    (with-arguments-validation (who)
	((fixnum	x)
	 (fixnum	y))
      (if ($fx>= y 0)
	  (with-arguments-validation (who)
	      ((positive-fixnum-shift-width	y))
	    (let ((r ($fxsll x y)))
	      (if ($fx= x ($fxsra r y))
		  r
		(%overflow-violation who x y))))
	(with-arguments-validation (who)
	    ((negative-fixnum-shift-width	y))
	  ($fxsra x ($fx- 0 y))))))

  (define-argument-validation (positive-fixnum-shift-width who obj)
    ($fx< obj (fixnum-width))
    (assertion-violation who
      "expected positive fixnum less than fixnum width as shift argument"
      obj))

  (define-argument-validation (negative-fixnum-shift-width who obj)
    ($fx> obj (- (fixnum-width)))
    (assertion-violation who
      "expected negative fixnum less than fixnum width as shift argument"
      obj))

  #| end of module: fxarithmetic-shift |# )

;;; --------------------------------------------------------------------

(define (error@fxarithmetic-shift who x y)
  (unless (fixnum? x)
    (assertion-violation who "not a fixnum" x))
  (unless (fixnum? y)
    (assertion-violation who "not a fixnum" y))
  (unless ($fx>= y 0)
    (assertion-violation who "negative shift not allowed" y))
  (unless ($fx< y (fixnum-width))
    (assertion-violation who "shift is not less than fixnum-width" y))
  (%overflow-violation who x y))

(define (error@fxarithmetic-shift-left x y)
  (error@fxarithmetic-shift 'arithmetic-shift-left x y))

(define (error@fxarithmetic-shift-right x y)
  (error@fxarithmetic-shift 'arithmetic-shift-right x y))


;;;; arithmetic operations

(define (fx+ x y)
  (sys:fx+ x y))

(define (fx* x y)
  (sys:fx* x y))

(define fx-
  (case-lambda
   ((x y) (sys:fx- x y))
   ((x)   (sys:fx- x))))

(define (fxadd1 n)
  (import (ikarus))
  (fxadd1 n))

(define (fxsub1 n)
  (import (ikarus))
  (fxsub1 n))

;;; --------------------------------------------------------------------

(module (error@fx+
	 error@fx-
	 error@fx*
	 error@fxadd1
	 error@fxsub1)

  (define (make-fx-error who)
    (case-lambda
     ((x y)
      (with-arguments-validation (who)
	  ((fixnum	x)
	   (fixnum	y))
	(%overflow-violation who x y)))
     ((x)
      (with-arguments-validation (who)
	  ((fixnum	x))
	(%overflow-violation who x)))))

  (define error@fx+    (make-fx-error 'fx+))
  (define error@fx-    (make-fx-error 'fx-))
  (define error@fx*    (make-fx-error 'fx*))
  (define error@fxadd1 (make-fx-error 'fxadd1))
  (define error@fxsub1 (make-fx-error 'fxsub1))

  #| end of module |# )


;;;; comparison predicates

(define (%fxcmp-validate-rest who ls)
  (if (pair? ls)
      (with-arguments-validation (who)
	  ((fixnum	($car ls)))
	(%fxcmp-validate-rest who ($cdr ls)))
    #f))

(let-syntax
    ((define-fxcmp
       (syntax-rules ()
	 ((_ ?who1 ?who2 $op)
	  (module (?who1 ?who2)
	    (define who (quote ?who1))

	    (define ?who1
	      (case-lambda
	       ((x y)
		(with-arguments-validation (who)
		    ((fixnum	x)
		     (fixnum	y))
		  ($op x y)))

	       ((x y . ls)
		(with-arguments-validation (who)
		    ((fixnum	x)
		     (fixnum	y))
		  (if ($op x y)
		      (let loop ((x  y)
				 (ls ls))
			(if (pair? ls)
			    (let ((y  ($car ls))
				  (ls ($cdr ls)))
			      (with-arguments-validation (who)
				  ((fixnum	y))
				(if ($op x y)
				    (loop y ls)
				  (%fxcmp-validate-rest 'who ls))))
			  #t))
		    (%fxcmp-validate-rest 'who ls))))

	       ((x)
		(with-arguments-validation (who)
		    ((fixnum	x))
		  #t))))

	    (define ?who2 ?who1)

	    #| end of module |# )
	  ))))

  (define-fxcmp fx=?	fx=	$fx=)
  (define-fxcmp fx<?	fx<	$fx<)
  (define-fxcmp fx<=?	fx<=	$fx<=)
  (define-fxcmp fx>?	fx>	$fx>)
  (define-fxcmp fx>=?	fx>=	$fx>=))


;;;; comparison functions

(define fxmin
  (case-lambda
   ((x y)
    (if (fixnum? x)
	(if (fixnum? y)
	    (if ($fx< x y) x y)
	  (assertion-violation 'fxmin "not a fixnum" y))
      (assertion-violation 'fxmin "not a fixnum" x)))
   ((x y z . ls)
    (fxmin (fxmin x y)
	   (if (fixnum? z)
	       (let f ((z z) (ls ls))
		 (if (null? ls)
		     z
                   (let ((a ($car ls)))
                     (if (fixnum? a)
                         (if ($fx< a z)
                             (f a ($cdr ls))
			   (f z ($cdr ls)))
		       (assertion-violation 'fxmin "not a fixnum" a)))))
             (assertion-violation 'fxmin "not a fixnum" z))))
   ((x) (if (fixnum? x) x (assertion-violation 'fxmin "not a fixnum" x)))))

(define fxmax
  (case-lambda
   ((x y)
    (if (fixnum? x)
	(if (fixnum? y)
	    (if ($fx> x y) x y)
	  (assertion-violation 'fxmax "not a fixnum" y))
      (assertion-violation 'fxmax "not a fixnum" x)))
   ((x y z . ls)
    (fxmax (fxmax x y)
	   (if (fixnum? z)
	       (let f ((z z) (ls ls))
		 (if (null? ls)
		     z
                   (let ((a ($car ls)))
                     (if (fixnum? a)
                         (if ($fx> a z)
                             (f a ($cdr ls))
			   (f z ($cdr ls)))
		       (assertion-violation 'fxmax "not a fixnum" a)))))
             (assertion-violation 'fxmax "not a fixnum" z))))
   ((x) (if (fixnum? x) x (assertion-violation 'fxmax "not a fixnum" x)))))


(define (fxquotient x y)
  (define who 'fxquotient)
  (with-arguments-validation (who)
      ((fixnum		x)
       (fixnum		y)
       (non-zero-fixnum	y))
    (if (eq? y -1)
	(if (eq? x (least-fixnum))
	    (%overflow-violation who x y)
	  ($fx- 0 x))
      ($fxquotient x y))))

(define-fx-operation/div fxremainder $fxremainder)
(define-fx-operation/div fxmodulo $fxmodulo)
(define-fx-operation/one fxsign $fxsign)

(define ($fxremainder x y)
  (let ((q ($fxquotient x y)))
    ($fx- x ($fx* q y))))

(define ($fxmodulo n1 n2)
  (* (fxsign n2)
     (mod (* (fxsign n2) n1)
	  (abs n2))))

(define ($fxsign n)
  (cond (($fxnegative? n)	-1)
	(($fxpositive? n)	+1)
	(else			0)))


(define-syntax define-fx
  (syntax-rules ()
    ((_ (name arg* ...) body)
     (define (name arg* ...)
       (unless (fixnum? arg*) (assertion-violation 'name "not a fixnum" arg*)) ...
       body))))

(define-fx (fx*/carry fx1 fx2 fx3)
  (let ((s0 ($fx+ ($fx* fx1 fx2) fx3)))
    (values
     s0
     (sra (+ (* fx1 fx2) (- fx3 s0)) (fixnum-width)))))

(define-fx (fx+/carry fx1 fx2 fx3)
  (let ((s0 ($fx+ ($fx+ fx1 fx2) fx3)))
    (values
     s0
     (sra (+ (+ fx1 fx2) (- fx3 s0)) (fixnum-width)))))

(define-fx (fx-/carry fx1 fx2 fx3)
  (let ((s0 ($fx- ($fx- fx1 fx2) fx3)))
    (values
     s0
     (sra (- (- fx1 fx2) (+ s0 fx3)) (fixnum-width)))))

(module (fixnum->string)
  (define mapping-string "0123456789ABCDEF")
  (define f
    (lambda (n i j radix)
      (cond
       (($fxzero? n)
	(values (make-string i) j))
       (else
	(let* ((q ($fxquotient n radix))
	       (c ($string-ref mapping-string
			       ($fx- n ($fx* q radix)))))
	  (call-with-values
	      (lambda () (f q ($fxadd1 i) j radix))
	    (lambda (str j)
	      (string-set! str j c)
	      (values str ($fxadd1 j)))))))))
  (define $fixnum->string
    (lambda (x radix)
      (cond
       (($fxzero? x) (string #\0))
       (($fx> x 0)
	(call-with-values
	    (lambda () (f x 0 0 radix))
	  (lambda (str j) str)))
       (($fx= x (least-fixnum))
	(string-append
	 ($fixnum->string ($fxquotient x radix) radix)
	 ($fixnum->string ($fx- radix ($fxmodulo x radix)) radix)))
       (else
	(call-with-values
	    (lambda () (f ($fx- 0 x) 1 1 radix))
	  (lambda (str j)
	    ($string-set! str 0 #\-)
	    str))))))
  (define fixnum->string
    (case-lambda
     ((x)
      (unless (fixnum? x) (assertion-violation 'fixnum->string "not a fixnum" x))
      ($fixnum->string x 10))
     ((x r)
      (unless (fixnum? x) (assertion-violation 'fixnum->string "not a fixnum" x))
      (case r
	((2)  ($fixnum->string x 2))
	((8)  ($fixnum->string x 8))
	((10) ($fixnum->string x 10))
	((16) ($fixnum->string x 16))
	(else (assertion-violation 'fixnum->string "invalid radix" r)))))))


)

(library (ikarus fixnums div-and-mod)
  (export fxdiv fxmod fxdiv-and-mod fxdiv0 fxmod0 fxdiv0-and-mod0)
  (import
      (ikarus system $fx)
    (except (ikarus) fxdiv fxmod fxdiv-and-mod fxdiv0 fxmod0 fxdiv0-and-mod0))

  (define ($fxdiv-and-mod n m)
    (let ((d0 ($fxquotient n m)))
      (let ((m0 ($fx- n ($fx* d0 m))))
        (if ($fx>= m0 0)
            (values d0 m0)
	  (if ($fx>= m 0)
	      (values ($fx- d0 1) ($fx+ m0 m))
	    (values ($fx+ d0 1) ($fx- m0 m)))))))

  (define ($fxdiv n m)
    (let ((d0 ($fxquotient n m)))
      (if ($fx>= n ($fx* d0 m))
          d0
	(if ($fx>= m 0)
	    ($fx- d0 1)
	  ($fx+ d0 1)))))

  (define ($fxmod n m)
    (let ((d0 ($fxquotient n m)))
      (let ((m0 ($fx- n ($fx* d0 m))))
        (if ($fx>= m0 0)
            m0
	  (if ($fx>= m 0)
	      ($fx+ m0 m)
	    ($fx- m0 m))))))

  (define-syntax define-div-proc
    (syntax-rules ()
      ((_ who $unsafe-op overflow-check?)
       (define (who x y)
         (if (fixnum? x)
             (if (fixnum? y)
                 (if ($fx> y 0)
                     ($unsafe-op x y)
		   (if ($fx= y 0)
		       (assertion-violation 'who "division by 0" x y)
		     (if (and overflow-check? ($fx= y -1))
			 (if ($fx= x (least-fixnum))
;;;Flatt's  test suite for  R6RS expects  an &implementation-restriction
;;;condition  here, in truth  for no  reason I  can figure  out.  (Marco
;;;Maggi; Oct 14, 2011)
			     (raise
			      (condition
			       (make-implementation-restriction-violation)
			       (make-who-condition 'who)
			       (make-message-condition "result not representable as fixnum")
			       (make-irritants-condition (list x y))))
			   #;(assertion-violation 'who "result not representable as fixnum" x y)
			   ($unsafe-op x y))
		       ($unsafe-op x y))))
	       (assertion-violation 'who "not a fixnum" y))
	   (assertion-violation 'who "not a fixnum" x))))))

  (define-div-proc fxdiv $fxdiv #t)
  (define-div-proc fxmod $fxmod #f)
  (define-div-proc fxdiv-and-mod $fxdiv-and-mod #t)


  (define ($fxdiv0-and-mod0 n m)
    (let ((d0 (quotient n m)))
      (let ((m0 (- n (* d0 m))))
        (if (>= m 0)
            (if (< (* m0 2) m)
                (if (<= (* m0 -2) m)
                    (values d0 m0)
		  (values (- d0 1) (+ m0 m)))
	      (values (+ d0 1) (- m0 m)))
	  (if (> (* m0 -2) m)
	      (if (>= (* m0 2) m)
		  (values d0 m0)
		(values (+ d0 1) (- m0 m)))
	    (values (- d0 1) (+ m0 m)))))))

  (define ($fxdiv0 n m)
    (let ((d0 (quotient n m)))
      (let ((m0 (- n (* d0 m))))
        (if (>= m 0)
            (if (< (* m0 2) m)
                (if (<= (* m0 -2) m)
                    d0
		  (- d0 1))
	      (+ d0 1))
	  (if (> (* m0 -2) m)
	      (if (>= (* m0 2) m)
		  d0
		(+ d0 1))
	    (- d0 1))))))

  (define ($fxmod0 n m)
    (let ((d0 (quotient n m)))
      (let ((m0 (- n (* d0 m))))
        (if (>= m 0)
            (if (< (* m0 2) m)
                (if (<= (* m0 -2) m)
                    m0
		  (+ m0 m))
	      (- m0 m))
	  (if (> (* m0 -2) m)
	      (if (>= (* m0 2) m)
		  m0
		(- m0 m))
	    (+ m0 m))))))

  (define (fxdiv0-and-mod0 x y)
    (if (fixnum? x)
        (if (fixnum? y)
            (if ($fx= y 0)
                (assertion-violation 'fxdiv0-and-mod0 "division by 0")
	      (begin
		(when (and ($fx= y -1) ($fx= x (least-fixnum)))
		  (raise (condition
			  (make-implementation-restriction-violation)
			  (make-who-condition 'fxdiv0-and-mod0)
			  (make-message-condition "result not representable as fixnum")
			  (make-irritants-condition (list x y)))))
		(let-values (((d m) ($fxdiv0-and-mod0 x y)))
		  (if (and (fixnum? d) (fixnum? m))
		      (values d m)
		    (assertion-violation 'fxdiv0-and-mod0
			 "results not representable as fixnums"
			 x y)))))
	  (assertion-violation 'fxdiv0-and-mod0 "not a fixnum" y))
      (assertion-violation 'fxdiv0-and-mod0 "not a fixnum" x)))

  (define (fxdiv0 x y)
    (if (fixnum? x)
        (if (fixnum? y)
            (if ($fx= y 0)
                (assertion-violation 'fxdiv0 "division by 0")
	      (begin
		(when (and ($fx= y -1) ($fx= x (least-fixnum)))
		  (raise (condition
			  (make-implementation-restriction-violation)
			  (make-who-condition 'fxdiv0)
			  (make-message-condition "result not representable as fixnum")
			  (make-irritants-condition (list x y)))))
		(let ((d ($fxdiv0 x y)))
		  (if (fixnum? d)
		      d
		    (assertion-violation 'fxdiv0 "result not representable as fixnum" x y)))))
	  (assertion-violation 'fxdiv0 "not a fixnum" y))
      (assertion-violation 'fxdiv0 "not a fixnum" x)))

  (define (fxmod0 x y)
    (if (fixnum? x)
	(if (fixnum? y)
	    (if ($fx= y 0)
		(assertion-violation 'fxmod0 "division by 0")
	      (let ((d ($fxmod0 x y)))
		(if (fixnum? d)
		    d
		  (assertion-violation 'fxmod0
		       "result not representable as fixnum"
		       x y))))
	  (assertion-violation 'fxmod0 "not a fixnum" y))
      (assertion-violation 'fxmod0 "not a fixnum" x)))


;;;; done

)

(library (ikarus fixnums unsafe)
  (export
    $fxzero?
    #;$fxpositive?	#;$fxnegative?
    $fxadd1		$fxsub1
    $fx+		$fx*
    $fx-
    $fx=
    $fx<		$fx<=
    $fx>		$fx>=
    $fxsll		$fxsra
    $fxlogor		$fxlogand
    $fxlognot)
  (import (ikarus))
  (define $fxzero? fxzero?)
  #;(define $fxpositive? fxpositive?)
  #;(define $fxnegative? fxnegative?)
  (define $fxadd1 fxadd1)
  (define $fxsub1 fxsub1)
  (define $fx+ fx+)
  (define $fx* fx*)
  (define $fx- fx-)
  (define $fx= fx=)
  (define $fx< fx<)
  (define $fx<= fx<=)
  (define $fx> fx>)
  (define $fx>= fx>=)
  (define $fxsll fxsll)
  (define $fxsra fxsra)
  (define $fxlogor fxlogor)
  (define $fxlogand fxlogand)
  (define $fxlognot fxlognot))

;;; end of file
