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


#!vicare
(library (ikarus fixnums)
  (export
    list-of-fixnums?

    fxzero?
    fxpositive?			fxnegative?
    fxnonnegative?		fxnonpositive?
    fxeven?			fxodd?

    zero-fixnum?		non-zero-fixnum?
    positive-fixnum?		negative-fixnum?
    non-negative-fixnum?	non-positive-fixnum?

    byte-fixnum?		zero-byte-fixnum?
    positive-byte-fixnum?	negative-byte-fixnum?
    octet-fixnum?		zero-octet-fixnum?
    positive-octet-fixnum?

    fxadd1			fxsub1
    fx+				fx-
    fx*
    fx+/carry			fx*/carry
    fx-/carry
    fxdiv			fxmod
    fxdiv0			fxmod0
    fxdiv-and-mod		fxdiv0-and-mod0
    fxquotient			fxremainder
    fxmodulo			fxsign
    fxabs

    fxlogor			fxlogand
    fxlogxor			fxlognot
    fxior			fxand
    fxxor			fxnot
    fxif
    fxsll			fxsra
    fxarithmetic-shift-left	fxarithmetic-shift-right
    fxarithmetic-shift

    fx=				fx=?
    fx!=			fx!=?
    fx<				fx<?
    fx<=			fx<=?
    fx>				fx>?
    fx>=			fx>=?
    fxmin			fxmax

    fixnum->char		char->fixnum
    fixnum->string
    fixnum-in-character-range?

;;; --------------------------------------------------------------------

    $fx!=

    $fxpositive?		$fxnegative?
    $fxnonpositive?		$fxnonnegative?
    $fxeven?			$fxodd?
    $fxmodulo			$fxremainder
    $fxsign
    $fxmin			$fxmax

    $fxdiv			$fxdiv0
    $fxmod			$fxmod0
    $fxdiv-and-mod		$fxdiv0-and-mod0
    $fxabs

    $fixnum->string

;;; --------------------------------------------------------------------

    error@fx+			error@fx*
    error@fx-			error@fxadd1
    error@fxsub1
    error@fxarithmetic-shift-left
    error@fxarithmetic-shift-right)
  (import (except (vicare)
		  list-of-fixnums?

		  fxzero?
		  fxpositive?			fxnegative?
		  fxnonnegative?		fxnonpositive?
		  fxeven?			fxodd?

		  zero-fixnum?			non-zero-fixnum?
		  positive-fixnum?		negative-fixnum?
		  non-negative-fixnum?		non-positive-fixnum?

		  byte-fixnum?			zero-byte-fixnum?
		  positive-byte-fixnum?		negative-byte-fixnum?
		  octet-fixnum?			zero-octet-fixnum?
		  positive-octet-fixnum?

		  fxquotient			fxremainder
		  fxmodulo			fxsign

		  fxadd1			fxsub1
		  fx+				fx-
		  fx*
		  fx+/carry			fx*/carry
		  fx-/carry
		  fxdiv				fxmod
		  fxdiv0			fxmod0
		  fxdiv-and-mod			fxdiv0-and-mod0
		  fxabs

		  fxlogor			fxlogand
		  fxlogxor			fxlognot
		  fxsll				fxsra

		  fx=				fx=?
		  fx!=				fx!=?
		  fx<				fx<?
		  fx<=				fx<=?
		  fx>				fx>?
		  fx>=				fx>=?
		  fxmin				fxmax

		  fxior				fxand
		  fxxor				fxnot
		  fxif

		  fxarithmetic-shift-left	fxarithmetic-shift-right
		  fxarithmetic-shift

		  fixnum->char			char->fixnum
		  fixnum->string
		  fixnum-in-character-range?)
    (prefix (only (vicare)
		  fx+ fx* fx-)
	    sys:)
    (except (vicare system $fx)
	    $fx!=
	    $fxpositive?	$fxnegative?
	    $fxnonpositive?	$fxnonnegative?
	    $fxeven?		$fxodd?
	    $fxmodulo		$fxremainder
	    $fxsign
	    $fxmin		$fxmax
	    $fxdiv		$fxdiv0
	    $fxmod		$fxmod0
	    $fxdiv-and-mod	$fxdiv0-and-mod0
	    $fxabs
	    $fixnum->string)
    (prefix (only (vicare system $fx)
		  $fx!=)
	    sys::)
    (vicare system $chars)
    (vicare system $pairs)
    (vicare system $strings)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-inequality-predicate))


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
     (define* (?safe-who {x fixnum?})
       (?unsafe-who x)))))

(define-syntax define-fx-operation/two
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define* (?safe-who {x fixnum?} {y fixnum?})
       (?unsafe-who x y)))))

(define-syntax define-fx-operation/shift
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define* (?safe-who {x fixnum?} {y fixnum?})
       (?unsafe-who x y)))))

(define-syntax define-fx-operation/three
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define* (?safe-who {x fixnum?} {y fixnum?} {z fixnum?})
       (?unsafe-who x y z)))))

(define-syntax define-fx-operation/div
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define* (?safe-who {x fixnum?} {y fixnum?})
       (?unsafe-who x y)))))


;;;; predicates

(define-list-of-type-predicate list-of-fixnums? fixnum?)

;;; --------------------------------------------------------------------

(define (byte-fixnum? obj)
  (and (fixnum? obj)
       ($fx>= obj -128)
       ($fx<= obj +127)))

(define (zero-byte-fixnum? obj)
  (eq? obj 0))

(define (positive-byte-fixnum? obj)
  (and (fixnum? obj)
       ($fxpositive? obj)
       ($fx<= obj +127)))

(define (negative-byte-fixnum? obj)
  (and (fixnum? obj)
       ($fx>= obj -128)
       ($fxnegative? obj)))

;;; --------------------------------------------------------------------

(define (octet-fixnum? obj)
  (and (fixnum? obj)
       ($fxnonnegative? obj)
       ($fx<= obj 255)))

(define (zero-octet-fixnum? obj)
  (eq? obj 0))

(define (positive-octet-fixnum? obj)
  (and (fixnum? obj)
       ($fxpositive? obj)
       ($fx<= obj +255)))

;;; --------------------------------------------------------------------

(define* (fxzero? {x fixnum?})
  (eq? x 0))

(define ($fxpositive? N)	($fx> N 0))
(define ($fxnegative? N)	($fx< N 0))
(define ($fxeven?     N)	($fxzero? ($fxlogand N 1)))
(define ($fxodd?      N)	(not ($fxzero? ($fxlogand N 1))))

(define ($fxnonpositive? x)
  (or ($fxzero? x)
      ($fxnegative? x)))

(define ($fxnonnegative? x)
  (or ($fxzero? x)
      ($fxpositive? x)))

(define-fx-operation/one fxpositive?	$fxpositive?)
(define-fx-operation/one fxnegative?	$fxnegative?)
(define-fx-operation/one fxnonpositive?	$fxnonpositive?)
(define-fx-operation/one fxnonnegative?	$fxnonnegative?)
(define-fx-operation/one fxeven?	$fxeven?)
(define-fx-operation/one fxodd?		$fxodd?)

;;; --------------------------------------------------------------------

(define (zero-fixnum? obj)
  (eq? 0 obj))

(define (non-zero-fixnum? fx)
  (if (eq? 0 fx)
      #f
    (fixnum? fx)))

(define (positive-fixnum? obj)
  (and (fixnum?      obj)
       ($fxpositive? obj)))

(define (negative-fixnum? obj)
  (and (fixnum?      obj)
       ($fxnegative? obj)))

(define (non-negative-fixnum? obj)
  (and (fixnum?         obj)
       ($fxnonnegative? obj)))

(define (non-positive-fixnum? obj)
  (and (fixnum?         obj)
       ($fxnonpositive? obj)))


;;;; bitwise logic operations

(define* (fxlognot {x fixnum?})
  ($fxlognot x))

(define fxnot fxlognot)

(let-syntax
    ((define-fxbitop (syntax-rules ()
		       ((_ ?who1 ?who2 ?unsafe-op ?identity)
			(begin
			  (case-define* ?who1
			    (({x fixnum?} {y fixnum?})
			     (?unsafe-op x y))

			    (({x fixnum?} {y fixnum?} {z fixnum?} . {rest fixnum?})
			     (let loop ((accum  (?unsafe-op (?unsafe-op x y) z))
					(rest   rest))
			       (if (pair? rest)
				   (loop (?unsafe-op accum ($car rest))
					 ($cdr rest))
				 accum)))

			    (({x fixnum?})
			     x)

			    (()
			     ?identity))

			  (define ?who2 ?who1)))
		       )))
  (define-fxbitop fxlogor	fxior		$fxlogor	 0)
  (define-fxbitop fxlogand	fxand		$fxlogand	-1)
  (define-fxbitop fxlogxor	fxxor		$fxlogxor	 0)
  #| end of LET-SYNTAX |# )


;;;; bitwise operations

(define-fx-operation/shift fxsra $fxsra)
(define-fx-operation/shift fxsll $fxsll)
(define-fx-operation/three fxif  $fxif)

(define ($fxif x y z)
  ($fxlogor ($fxlogand x             y)
	    ($fxlogand ($fxlognot x) z)))

;;; --------------------------------------------------------------------

(define (fxarithmetic-shift-right x y)
  ;;This is also a primitive operation.
  (import (vicare))
  (fxarithmetic-shift-right x y))

(define (fxarithmetic-shift-left x y)
  ;;This is also a primitive operation.
  (import (vicare))
  (fxarithmetic-shift-left x y))

(define* (fxarithmetic-shift {x fixnum?} {y fixnum?})
  (cond (($fxzero? y)
	 x)
	(($positive-index-of-bit-in-fixnum-representation? y)
	 (let ((r ($fxsll x y)))
	   (if ($fx= x ($fxsra r y))
	       r
	     (%overflow-violation __who__ x y))))
	(($negative-index-of-bit-in-fixnum-representation? y)
	 ($fxsra x ($fx- 0 y)))
	(else
	 (procedure-argument-violation __who__
	   "expected positive or negative fixnum representing index of bit in fixnum representation as shift argument"
	   y))))

(define ($positive-index-of-bit-in-fixnum-representation? Y)
  (and ($fxpositive? Y)
       ($fx< Y (fixnum-width))))

(define ($negative-index-of-bit-in-fixnum-representation? Y)
  (and ($fxnegative? Y)
       ($fx> Y (- (fixnum-width)))))

;;; --------------------------------------------------------------------

(module (error@fxarithmetic-shift-left
	 error@fxarithmetic-shift-right)

  (define (error@fxarithmetic-shift-left x y)
    ;;Error function  called by the primitive  operation FXARITHMETIC-SHIFT-LEFT when
    ;;it cannot compute the result.
    ;;
    (error@fxarithmetic-shift 'arithmetic-shift-left x y))

  (define (error@fxarithmetic-shift-right x y)
    ;;Error function called by  the primitive operation FXARITHMETIC-SHIFT-RIGHT when
    ;;it cannot compute the result.
    ;;
    (error@fxarithmetic-shift 'arithmetic-shift-right x y))

  (define (error@fxarithmetic-shift who x y)
    (unless (fixnum? x)
      (procedure-argument-violation who "not a fixnum" x))
    (unless (fixnum? y)
      (procedure-argument-violation who "not a fixnum" y))
    (unless ($fx>= y 0)
      (procedure-argument-violation who "negative shift not allowed" y))
    (unless ($fx< y (fixnum-width))
      (procedure-argument-violation who "shift is not less than fixnum-width" y))
    (%overflow-violation who x y))

  #| end of module |# )


;;;; arithmetic operations

(define (fx+ x y)
  ;;This is also a primitive operation.
  (sys:fx+ x y))

(define (fx* x y)
  ;;This is also a primitive operation.
  (sys:fx* x y))

(case-define fx-
  ;;This is also a primitive operation.
  ((x y) (sys:fx- x y))
  ((x)   (sys:fx- x)))

(define* (fxadd1 {n fixnum?})
  ;;This is also a primitive operation.
  ($fxadd1 n))

(define* (fxsub1 {n fixnum?})
  ;;This is also a primitive operation.
  ($fxsub1 n))

;;; --------------------------------------------------------------------

;;Some core primitives are implemented both as:
;;
;;* Proper  procedures.  There exists  a loc gensym  whose "value" slot  references a
;;  closure object,  which in  turn references  a code  object implementing  the core
;;  primitive as machine code.
;;
;;* Primitive operations.  There exist functions that the compiler calls to integrate
;;  assembly instructions implementing the core primitive.
;;
;;When the core primitive is used as argument as in:
;;
;;   (map fx+ a* b*)
;;
;;the closure object implementation is used; when the core primitive is used as first
;;subform of an application form as in:
;;
;;   (fx+ 1 2)
;;
;;the primitive operation is used.
;;
;;Let's  consider FX+.   When  the  code object  implementation  detects overflow  or
;;underflow: it raises  an exception.  When the primitive  operation detects overflow
;;or  underflow  what should  it  do?   The  answer  is: every  integrated  primitive
;;operation  assembly  code  will jump  to  the  same  routine  which will  raise  an
;;exception.
;;
;;Such exception-raising  routines are the  ones below; they are  called ERROR@?PRIM,
;;where ?PRIM  is the  name of the  core primitive.  Notice  that, upon  entering the
;;routine, we  do not know  which error triggered the  call, so we  check everything:
;;first we  validate the operands as  fixnums, if they are  it means the error  is an
;;overflow.
;;
(let-syntax
    ((define-fx-error (syntax-rules ()
			((_ ?error-who ?who)
			 (case-define* ?error-who
			   (({x fixnum?} {y fixnum?})
			    (%overflow-violation (quote ?who) x y))
			   (({x fixnum?})
			    (%overflow-violation (quote ?who) x))))
			)))
  (define-fx-error error@fx+    fx+)
  (define-fx-error error@fx-    fx-)
  (define-fx-error error@fx*    fx*)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((define-fx-error (syntax-rules ()
			((_ ?error-who ?who)
			 (define* (?error-who {x fixnum?})
			   (%overflow-violation (quote ?who) x)))
			)))
  (define-fx-error error@fxadd1 fxadd1)
  (define-fx-error error@fxsub1 fxsub1)
  #| end of LET-SYNTAX |# )


;;;; comparison predicates

(define-equality/sorting-predicate fx=?		$fx=	fixnum?)
(define-equality/sorting-predicate fx<?		$fx<	fixnum?)
(define-equality/sorting-predicate fx<=?	$fx<=	fixnum?)
(define-equality/sorting-predicate fx>?		$fx>	fixnum?)
(define-equality/sorting-predicate fx>=?	$fx>=	fixnum?)
(define-inequality-predicate       fx!=?	$fx!=	fixnum?)

(define ($fx!= fx1 fx2)
  (sys::$fx!= fx1 fx2))

(define fx=	fx=?)
(define fx<	fx<?)
(define fx<=	fx<=?)
(define fx>	fx>?)
(define fx>=	fx>=?)
(define fx!=	fx!=?)


;;;; min max

(define-min/max-comparison fxmax $fxmax fixnum?)
(define-min/max-comparison fxmin $fxmin fixnum?)

;;FIXME This should be a proper primitive operation.  (Marco Maggi; Fri Mar 27, 2015)
;;
(define ($fxmin fx1 fx2)
  (if ($fx< fx1 fx2) fx1 fx2))

;;FIXME This should be a proper primitive operation.  (Marco Maggi; Fri Mar 27, 2015)
;;
(define ($fxmax fx1 fx2)
  (if ($fx< fx1 fx2) fx2 fx1))


(define* (fxquotient {x fixnum?} {y non-zero-fixnum?})
  (if (eq? y -1)
      ;;Remember that  we cannot simpy  use $fx- because  if X is  (least-fixnum) the
      ;;result will overflow.
      (if (eq? x (least-fixnum))
	  (%overflow-violation __who__ x y)
	($fx- x))
    ($fxquotient x y)))

(define-fx-operation/one fxabs		$fxabs)
(define-fx-operation/div fxremainder	$fxremainder)
(define-fx-operation/div fxmodulo	$fxmodulo)
(define-fx-operation/one fxsign		$fxsign)

(define* ($fxabs x)
  (if ($fxnegative? x)
      ;;Remember that  we cannot simpy  use $fx- because  if X is  (least-fixnum) the
      ;;result will overflow.
      (if ($fx= x (least-fixnum))
	  (%overflow-violation __who__ x)
	($fx- x))
    x))

(define ($fxsign n)
  (cond (($fxnegative? n)	-1)
	(($fxpositive? n)	+1)
	(else			0)))

(define ($fxremainder x y)
  ;;We have to assume that the result may not be a fixnum!!!
  (let ((q (fxquotient x y)))
    (- x (* q y))))

(define ($fxmodulo n1 n2)
  (* ($fxsign n2)
     ;;We have  to assume that  the result of the  product may not  be a
     ;;fixnum!!!  If N1 is (least-fixnum) and  the sign is -1 the result
     ;;of the product is a bignum.
     (mod (* ($fxsign n2) n1)
	  (abs n2))))


;;;; arithmetics with carry

(let-syntax
    ((define-fx (syntax-rules ()
		  ((_ (?who ?arg ...) ?body)
		   (define* (?who {?arg fixnum?} ...)
		     ?body))
		  )))

  (define-fx (fx*/carry fx1 fx2 fx3)
    (let ((s0 ($fx+ ($fx* fx1 fx2) fx3)))
      (values s0
	      (sra (+ (* fx1 fx2) (- fx3 s0)) (fixnum-width)))))

  (define-fx (fx+/carry fx1 fx2 fx3)
    (let ((s0 ($fx+ ($fx+ fx1 fx2) fx3)))
      (values s0
	      (sra (+ (+ fx1 fx2) (- fx3 s0)) (fixnum-width)))))

  (define-fx (fx-/carry fx1 fx2 fx3)
    (let ((s0 ($fx- ($fx- fx1 fx2) fx3)))
      (values s0
	      (sra (- (- fx1 fx2) (+ s0 fx3)) (fixnum-width)))))

  #| end of LET-SYNTAX |# )


;;;; conversion

(define (fixnum-in-character-range? obj)
  ;;Defined by Vicare.  Return #t  if OBJ is a fixnum and its value  is in one of the
  ;;ranges acceptable by Unicode code points; otherwise return #f.
  ;;
  (and (fixnum? obj)
       (or (and ($fx>= obj 0)
		($fx<  obj #xD800))
	   (and ($fx>  obj #xDFFF)
		($fx<= obj #x10FFFF)))))

(define* (fixnum->char {fx fixnum-in-character-range?})
  ($fixnum->char fx))

(define* (char->fixnum {ch char?})
  ($char->fixnum ch))

(module (fixnum->string $fixnum->string)

  (case-define* fixnum->string
    (({x fixnum?})
     ($fixnum->string x 10))

    (({x fixnum?} r)
     (case r
       ((2)  ($fixnum->string x 2))
       ((8)  ($fixnum->string x 8))
       ((10) ($fixnum->string x 10))
       ((16) ($fixnum->string x 16))
       (else
	(procedure-argument-violation __who__
	  "invalid radix, expected 2, 8, 10 or 16"
	  r)))))

  (define ($fixnum->string x radix)
    (cond (($fxzero? x)
	   (string #\0))
	  (($fx> x 0)
	   (call-with-values
	       (lambda () (f x 0 0 radix))
	     (lambda (str j) str)))
	  (($fx= x (least-fixnum))
	   (string-append ($fixnum->string ($fxquotient x radix)            radix)
			  ($fixnum->string ($fx- radix ($fxmodulo x radix)) radix)))
	  (else
	   (let-values (((str j) (f ($fx- 0 x) 1 1 radix)))
	     ($string-set! str 0 #\-)
	     str))))

  (define (f n i j radix)
    (define-constant MAPPING-STRING "0123456789ABCDEF")
    (if ($fxzero? n)
	(values (make-string i) j)
      (let* ((q ($fxquotient n radix))
	     (c ($string-ref MAPPING-STRING ($fx- n ($fx* q radix)))))
	(let-values (((str j) (f q ($fxadd1 i) j radix)))
	  (string-set! str j c)
	  (values str ($fxadd1 j))))))

  #| end of module: fixnum->string |# )


(module (fxdiv			fxdiv0
	 fxmod			fxmod0
	 fxdiv-and-mod		fxdiv0-and-mod0
	 $fxdiv			$fxdiv0
	 $fxmod			$fxmod0
	 $fxdiv-and-mod		$fxdiv0-and-mod0)

  (let-syntax
      ((define-div-proc
	 (syntax-rules ()
	   ((_ ?who $unsafe-op overflow-check?)
	    (define* (?who {x fixnum?} {y non-zero-fixnum?})
	      (if ($fx> y 0)
		  ($unsafe-op x y)
		(if (and overflow-check? ($fx= y -1))
		    (if ($fx= x (least-fixnum))
			(%error-result-not-fixnum __who__ x y)
		      ($unsafe-op x y))
		  ($unsafe-op x y)))))
	   )))
    (define-div-proc fxdiv $fxdiv #t)
    (define-div-proc fxmod $fxmod #f)
    (define-div-proc fxdiv-and-mod $fxdiv-and-mod #t)
    #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

  (define* (fxdiv0-and-mod0 {x fixnum?} {y non-zero-fixnum?})
    (%check-div-result-not-fixnum __who__ x y)
    (receive-and-return (d m)
	($fxdiv0-and-mod0 x y)
      (unless (fixnum? d)
	(%error-result-not-fixnum __who__ d))
      (unless (fixnum? m)
	(%error-result-not-fixnum __who__ m))))

  (define* (fxdiv0 {x fixnum?} {y non-zero-fixnum?})
    (%check-div-result-not-fixnum __who__ x y)
    (receive-and-return (d)
	($fxdiv0 x y)
      (unless (fixnum? d)
	(%error-result-not-fixnum __who__ d))))

  (define* (fxmod0 {x fixnum?} {y non-zero-fixnum?})
    (receive-and-return (d)
	($fxmod0 x y)
      (unless (fixnum? d)
	(%error-result-not-fixnum __who__ d))))

;;; --------------------------------------------------------------------

  (define ($fxdiv-and-mod n m)
    ;;Be careful to use FXQUOTIENT to avoid overflows!!!
    (let* ((d0 (fxquotient n m))
	   (m0 ($fx- n ($fx* d0 m))))
      (cond (($fx>= m0 0)
	     (values d0 m0))
	    (($fx>= m 0)
	     (values ($fx- d0 1) ($fx+ m0 m)))
	    (else
	     (values ($fx+ d0 1) ($fx- m0 m))))))

  (define ($fxdiv n m)
    ;;Be careful to use FXQUOTIENT to avoid overflows!!!
    (let ((d0 (fxquotient n m)))
      (cond (($fx>= n ($fx* d0 m))
	     d0)
	    (($fx>= m 0)
	     ($fx- d0 1))
	    (else
	     ($fx+ d0 1)))))

  (define ($fxmod n m)
    ;;Be careful to use FXQUOTIENT to avoid overflows!!!
    (let* ((d0 (fxquotient n m))
	   (m0 ($fx- n ($fx* d0 m))))
      (cond (($fx>= m0 0)
	     m0)
	    (($fx>= m 0)
	     ($fx+ m0 m))
	    (else
	     ($fx- m0 m)))))

;;; --------------------------------------------------------------------

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

;;; --------------------------------------------------------------------

  (define (%check-div-result-not-fixnum who x y)
    (when (and ($fx= y -1)
	       ($fx= x (least-fixnum)))
      (%error-result-not-fixnum who x y)))

  (define (%error-result-not-fixnum who . irritants)
    (raise (condition
	    (make-implementation-restriction-violation)
	    (make-who-condition who)
	    (make-message-condition "result not representable as fixnum")
	    (make-irritants-condition irritants))))

  #| end of module |# )


;;;; done

#| end of library |# )

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
  (import (vicare))
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
