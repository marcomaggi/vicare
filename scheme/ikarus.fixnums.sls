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
    fxmodulo

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
    $fxmodulo

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
		  fxmodulo

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
	    $fxmodulo)
    (ikarus system $chars)
    (ikarus system $pairs)
    (ikarus system $strings)
    (vicare arguments validation))


;;;; helpers

(define (die/overflow who . args)
  (raise
   (condition (make-implementation-restriction-violation)
	      (make-who-condition who)
	      (make-message-condition "overflow")
	      (make-irritants-condition args))))


;;;; predicates

(define (fxzero? x)
  (cond ((eq? x 0)	#t)
	((fixnum? x)	#f)
	(else
	 (assertion-violation 'fxzero? "expected fixnum as argument" x))))


;;;; bitwise logic operations

(define (fxlognot x)
  (define who 'fxlognot)
  (with-arguments-validation (who)
      ((fixnum	x))
    ($fxlognot x)))

(define (fxnot x)
  (define who 'fxnot)
  (with-arguments-validation (who)
      ((fixnum	x))
    ($fxlognot x)))


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
	(die/overflow who x y)))
     ((x)
      (with-arguments-validation (who)
	  ((fixnum	x))
	(die/overflow who x)))))

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

(define-syntax define-fxcmp
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
     )))

(define-fxcmp fx=?	fx=	$fx=)
(define-fxcmp fx<?	fx<	$fx<)
(define-fxcmp fx<=?	fx<=	$fx<=)
(define-fxcmp fx>?	fx>	$fx>)
(define-fxcmp fx>=?	fx>=	$fx>=)


(define fxquotient
  (lambda (x y)
    (unless (fixnum? x)
      (assertion-violation 'fxquotient "not a fixnum" x))
    (unless (fixnum? y)
      (assertion-violation 'fxquotient "not a fixnum" y))
    (when ($fxzero? y)
      (assertion-violation 'fxquotient "zero dividend" y))
    (if (eq? y -1)
	(if (eq? x (least-fixnum))
	    (die/overflow 'fxquotient x y)
	  ($fx- 0 x))
      ($fxquotient x y))))

(define fxremainder
  (lambda (x y)
    (unless (fixnum? x)
      (assertion-violation 'fxremainder "not a fixnum" x))
    (unless (fixnum? y)
      (assertion-violation 'fxremainder "not a fixnum" y))
    (when ($fxzero? y)
      (assertion-violation 'fxremainder "zero dividend" y))
    (let ((q ($fxquotient x y)))
      ($fx- x ($fx* q y)))))

(define ($fxmodulo n1 n2)
  (* (fxsign n2) (mod (* (fxsign n2) n1) (abs n2))))

(define fxmodulo
  (lambda (x y)
    (unless (fixnum? x)
      (assertion-violation 'fxmodulo "not a fixnum" x))
    (unless (fixnum? y)
      (assertion-violation 'fxmodulo "not a fixnum" y))
    (when ($fxzero? y)
      (assertion-violation 'fxmodulo "zero dividend" y))
    ($fxmodulo x y)))

(define (fxsign n)
  (cond
   ((fxnegative? n) -1)
   ((fxpositive? n) 1)
   (else 0)))

(define-syntax fxbitop
  (syntax-rules ()
    ((_ who $op identity)
     (case-lambda
      ((x y)
       (if (fixnum? x)
	   (if (fixnum? y)
	       ($op x y)
	     (assertion-violation 'who "not a fixnum" y))
	 (assertion-violation 'who "not a fixnum" x)))
      ((x y . ls)
       (if (fixnum? x)
	   (if (fixnum? y)
	       (let f ((a ($op x y)) (ls ls))
		 (cond
		  ((pair? ls)
		   (let ((b ($car ls)))
		     (if (fixnum? b)
			 (f ($op a b) ($cdr ls))
		       (assertion-violation 'who "not a fixnum" b))))
		  (else a)))
	     (assertion-violation 'who "not a fixnum" y))
	 (assertion-violation 'who "not a fixnum" x)))
      ((x) (if (fixnum? x) x (assertion-violation 'who "not a fixnum" x)))
      (()   identity)))))

(define fxlogor (fxbitop fxlogor $fxlogor 0))
(define fxlogand (fxbitop fxlogand $fxlogand -1))
(define fxlogxor (fxbitop fxlogxor $fxlogxor 0))
(define fxior (fxbitop fxior $fxlogor 0))
(define fxand (fxbitop fxand $fxlogand -1))
(define fxxor (fxbitop fxxor $fxlogxor 0))

(define (fxif x y z)
  (if (fixnum? x)
      (if (fixnum? y)
	  (if (fixnum? z)
	      ($fxlogor
	       ($fxlogand x y)
	       ($fxlogand ($fxlognot x) z))
	    (assertion-violation 'fxif "not a fixnum" z))
	(assertion-violation 'fxif "not a fixnum" y))
    (assertion-violation 'fxif "not a fixnum" x)))

(define fxsra
  (lambda (x y)
    (unless (fixnum? x)
      (assertion-violation 'fxsra "not a fixnum" x))
    (unless (fixnum? y)
      (assertion-violation 'fxsra "not a fixnum" y))
    (unless ($fx>= y 0)
      (assertion-violation 'fxsra "negative shift not allowed" y))
    ($fxsra x y)))


(define fxarithmetic-shift-right
  (lambda (x y)
    (import (ikarus))
    (fxarithmetic-shift-right x y)))

(define fxsll
  (lambda (x y)
    (unless (fixnum? x)
      (assertion-violation 'fxsll "not a fixnum" x))
    (unless (fixnum? y)
      (assertion-violation 'fxsll "not a fixnum" y))
    (unless ($fx>= y 0)
      (assertion-violation 'fxsll "negative shift not allowed" y))
    ($fxsll x y)))


(define (error@fxarithmetic-shift who x y)
  (unless (fixnum? x)
    (assertion-violation who "not a fixnum" x))
  (unless (fixnum? y)
    (assertion-violation who "not a fixnum" y))
  (unless ($fx>= y 0)
    (assertion-violation who "negative shift not allowed" y))
  (unless ($fx< y (fixnum-width))
    (assertion-violation who "shift is not less than fixnum-width" y))
  (die/overflow who x y))

(define (error@fxarithmetic-shift-left x y)
  (error@fxarithmetic-shift 'arithmetic-shift-left x y))

(define (error@fxarithmetic-shift-right x y)
  (error@fxarithmetic-shift 'arithmetic-shift-right x y))

(define fxarithmetic-shift-left
  (lambda (x y)
    (import (ikarus))
    (fxarithmetic-shift-left x y)))

(define fxarithmetic-shift
  (lambda (x y)
    (import (ikarus))
    (define (err str x) (assertion-violation 'fxarithmetic-shift str x))
    (unless (fixnum? x) (err "not a fixnum" x))
    (unless (fixnum? y) (err "not a fixnum" y))
    (if ($fx>= y 0)
	(if ($fx< y (fixnum-width))
	    (let ((r ($fxsll x y)))
	      (if ($fx= x ($fxsra r y))
		  r
		(die/overflow 'fxarithmetic-shift x y)))
	  (err "invalid shift amount" y))
      (if ($fx> y (- (fixnum-width)))
	  ($fxsra x ($fx- 0 y))
	(err "invalid shift amount" y)))))

(define (fxpositive? x)
  (if (fixnum? x)
      ($fx> x 0)
    (assertion-violation 'fxpositive? "not a fixnum" x)))

(define (fxnegative? x)
  (if (fixnum? x)
      ($fx< x 0)
    (assertion-violation 'fxnegative? "not a fixnum" x)))

(define (fxeven? x)
  (if (fixnum? x)
      ($fxzero? ($fxlogand x 1))
    (assertion-violation 'fxeven? "not a fixnum" x)))

(define (fxodd? x)
  (if (fixnum? x)
      (not ($fxzero? ($fxlogand x 1)))
    (assertion-violation 'fxodd? "not a fixnum" x)))

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
    $fxpositive?	$fxnegative?
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
  (define $fxpositive? fxpositive?)
  (define $fxnegative? fxnegative?)
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
