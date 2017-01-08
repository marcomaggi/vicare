;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Modified by 2010-2017 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
  (options typed-language)
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

    $fxremainder		$fxmodulo
    $fxquotient
    $fxabs			$fxsign

    $fxmax			$fxmin

    $fxeven?			$fxodd?

    $fxdiv			$fxdiv0
    $fxmod			$fxmod0
    $fxdiv-and-mod		$fxdiv0-and-mod0

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

		  fx=				fx=?
		  fx!=				fx!=?
		  fx<				fx<?
		  fx<=				fx<=?
		  fx>				fx>?
		  fx>=				fx>=?
		  fxmin				fxmax

		  fxior				fxand
		  fxxor				fxnot
		  fxsll				fxsra
		  fxif

		  fxarithmetic-shift-left	fxarithmetic-shift-right
		  fxarithmetic-shift

		  fixnum->char			char->fixnum
		  fixnum->string
		  fixnum-in-character-range?)
    (prefix (only (vicare)
		  fx+ fx* fx- fxadd1 fxsub1
		  fxarithmetic-shift-right
		  fxarithmetic-shift-left)
	    sys::)
    (prefix (vicare system $fx)
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
     (define (?safe-who {x <fixnum>})
       (?unsafe-who x)))))

(define-syntax define-unsafe-operation/one
  (syntax-rules ()
    ((_ ?unsafe-who ?system-unsafe-who)
     (define/typed (?unsafe-who {N <fixnum>})
       (?system-unsafe-who N)))
    ))

;;; --------------------------------------------------------------------

(define-syntax define-fx-predicate/one
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define ({?safe-who <boolean>} {x <fixnum>})
       (?unsafe-who x)))))

(define-syntax define-fx-operation/two
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who {x <fixnum>} {y <fixnum>})
       (?unsafe-who x y)))))

(define-syntax define-unsafe-operation/two
  (syntax-rules ()
    ((_ ?unsafe-who ?system-unsafe-who)
     (define/typed (?unsafe-who {N <fixnum>} {M <fixnum>})
       (?system-unsafe-who N M)))
    ))

;;; --------------------------------------------------------------------

(define-syntax define-fx-operation/shift
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who {x <fixnum>} {y <fixnum>})
       (?unsafe-who x y)))))

(define-syntax define-fx-operation/three
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who {x <fixnum>} {y <fixnum>} {z <fixnum>})
       (?unsafe-who x y z)))))

(define-syntax define-fx-operation/div
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define (?safe-who {x <fixnum>} {y <fixnum>})
       (?unsafe-who x y)))))


;;;; predicates

(define-list-of-type-predicate list-of-fixnums? fixnum?)

;;; --------------------------------------------------------------------

(define (byte-fixnum? obj)
  (and (fixnum? obj)
       (sys::$fx>= obj -128)
       (sys::$fx<= obj +127)))

(define (zero-byte-fixnum? obj)
  (eq? obj 0))

(define (positive-byte-fixnum? obj)
  (and (fixnum? obj)
       #;($fxpositive? obj)
       (sys::$fx>  obj 0)
       (sys::$fx<= obj +127)))

(define (negative-byte-fixnum? obj)
  (and (fixnum? obj)
       (sys::$fx>= obj -128)
       (sys::$fx<  obj 0)
       #;($fxnegative? obj)))

;;; --------------------------------------------------------------------

(define (octet-fixnum? obj)
  (and (fixnum? obj)
       #;($fxnonnegative? obj)
       (sys::$fx>= obj 0)
       (sys::$fx<= obj 255)))

(define (zero-octet-fixnum? obj)
  (eq? obj 0))

(define (positive-octet-fixnum? obj)
  (and (fixnum? obj)
       #;($fxpositive? obj)
       (sys::$fx>  obj 0)
       (sys::$fx<= obj +255)))

;;; --------------------------------------------------------------------

(define-fx-operation/one fxzero?	sys::$fxzero?)
(define-fx-operation/one fxpositive?	sys::$fxpositive?)
(define-fx-operation/one fxnegative?	sys::$fxnegative?)
(define-fx-operation/one fxnonpositive?	sys::$fxnonpositive?)
(define-fx-operation/one fxnonnegative?	sys::$fxnonnegative?)

;;; --------------------------------------------------------------------

(define-fx-operation/one fxeven?	$fxeven?)
(define-fx-operation/one fxodd?		$fxodd?)

(define/typed ($fxeven? {N <fixnum>})
  ;;FIXME  To be  converted to  primitive  operation use  after the  next boot  image
  ;;rotation.  (Marco Maggi; Sun Dec 11, 2016)
  (sys::$fxzero? (sys::$fxlogand N 1))
  #;(sys::$fxeven? N))

(define/typed ($fxodd? {N <fixnum>})
  ;;FIXME  To be  converted to  primitive  operation use  after the  next boot  image
  ;;rotation.  (Marco Maggi; Sun Dec 11, 2016)
  (not ($fxeven? N))
  #;(sys::$fxodd? N))

;;; --------------------------------------------------------------------

(define (zero-fixnum? obj)
  (eq? 0 obj))

(define (non-zero-fixnum? fx)
  (if (eq? 0 fx)
      #f
    (fixnum? fx)))

(define (positive-fixnum? obj)
  (and (fixnum?      obj)
       (sys::$fxpositive? obj)))

(define (negative-fixnum? obj)
  (and (fixnum?      obj)
       (sys::$fxnegative? obj)))

(define (non-negative-fixnum? obj)
  (and (fixnum?         obj)
       (sys::$fxnonnegative? obj)))

(define (non-positive-fixnum? obj)
  (and (fixnum?         obj)
       (sys::$fxnonpositive? obj)))


;;;; bitwise logic operations

(let-syntax
    ((define-fxbitop (syntax-rules ()
		       ((_ ?who ?unsafe-op ?identity)
			(case-define ?who
			  (({x <fixnum>} {y <fixnum>})
			   (?unsafe-op x y))

			  (({x <fixnum>} {y <fixnum>} {z <fixnum>} . {rest (list-of <fixnum>)})
			   (unsafe-cast-signature (<fixnum>)
			     (let loop ((accum  (?unsafe-op (?unsafe-op x y) z))
					(rest   rest))
			       (if (pair? rest)
				   (loop (?unsafe-op accum ($car rest))
					 ($cdr rest))
				 accum))))

			  (({x <fixnum>})
			   x)

			  (()
			   ?identity)))
		       )))
  (define-fxbitop fxior		sys::$fxlogor	 0)
  (define-fxbitop fxand		sys::$fxlogand	-1)
  (define-fxbitop fxxor		sys::$fxlogxor	 0)
  #| end of LET-SYNTAX |# )

(define-fx-operation/one fxnot		sys::$fxlognot)


;;;; bitwise operations

(define-fx-operation/shift fxsra	sys::$fxsra)
(define-fx-operation/shift fxsll	sys::$fxsll)
(define-fx-operation/three fxif		$fxif)

(define/typed ({$fxif <fixnum>} {x <fixnum>} {y <fixnum>} {z <fixnum>})
  (sys::$fxlogor (sys::$fxlogand x                  y)
		 (sys::$fxlogand (sys::$fxlognot x) z)))

;;; --------------------------------------------------------------------

(define (fxarithmetic-shift-right x y)
  ;;This is also a primitive operation.
  (sys::fxarithmetic-shift-right x y))

(define (fxarithmetic-shift-left x y)
  ;;This is also a primitive operation.
  (sys::fxarithmetic-shift-left x y))

(define (fxarithmetic-shift {x <fixnum>} {y <fixnum>})
  (cond ((sys::$fxzero? y)
	 x)
	(($positive-index-of-bit-in-fixnum-representation? y)
	 (let ((r (sys::$fxsll x y)))
	   (if (sys::$fx= x (sys::$fxsra r y))
	       r
	     (%overflow-violation __who__ x y))))
	(($negative-index-of-bit-in-fixnum-representation? y)
	 (sys::$fxsra x (sys::fx- 0 y)))
	(else
	 (procedure-argument-violation __who__
	   "expected positive or negative fixnum representing index of bit in fixnum representation as shift argument"
	   y))))

(define/typed ($positive-index-of-bit-in-fixnum-representation? {Y <fixnum>})
  (and (sys::$fxpositive? Y)
       (sys::$fx< Y (fixnum-width))))

(define/typed ($negative-index-of-bit-in-fixnum-representation? {Y <fixnum>})
  (and (sys::$fxnegative? Y)
       (sys::$fx> Y (- (fixnum-width)))))

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
    (unless (sys::$fxnonnegative? y)
      (procedure-argument-violation who "negative shift not allowed" y))
    (unless (sys::$fx< y (fixnum-width))
      (procedure-argument-violation who "shift amount is not less than fixnum-width" y))
    (%overflow-violation who x y))

  #| end of module |# )


;;;; arithmetic operations

(define (fx+ x y)
  ;;This is also a primitive operation.
  (sys::fx+ x y))

(define (fx* x y)
  ;;This is also a primitive operation.
  (sys::fx* x y))

(case-define fx-
  ;;This is also a primitive operation.
  ((x y) (sys::fx- x y))
  ((x)   (sys::fx- x)))

;;This is also a primitive operation.
(define-fx-operation/one fxadd1		sys::fxadd1)

;;This is also a primitive operation.
(define-fx-operation/one fxsub1		sys::fxsub1)

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
			 (case-define ?error-who
			   (({x <fixnum>} {y <fixnum>})
			    (%overflow-violation (quote ?who) x y))
			   (({x <fixnum>})
			    (%overflow-violation (quote ?who) x))))
			)))
  (define-fx-error error@fx+    fx+)
  (define-fx-error error@fx-    fx-)
  (define-fx-error error@fx*    fx*)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((define-fx-error (syntax-rules ()
			((_ ?error-who ?who)
			 (define (?error-who {x <fixnum>})
			   (%overflow-violation (quote ?who) x)))
			)))
  (define-fx-error error@fxadd1 fxadd1)
  (define-fx-error error@fxsub1 fxsub1)
  #| end of LET-SYNTAX |# )


;;;; comparison predicates

(define-equality/sorting-predicate fx=?		sys::$fx=	fixnum?)
(define-equality/sorting-predicate fx<?		sys::$fx<	fixnum?)
(define-equality/sorting-predicate fx<=?	sys::$fx<=	fixnum?)
(define-equality/sorting-predicate fx>?		sys::$fx>	fixnum?)
(define-equality/sorting-predicate fx>=?	sys::$fx>=	fixnum?)
(define-inequality-predicate       fx!=?	sys::$fx!=	fixnum?)

(define fx=	fx=?)
(define fx<	fx<?)
(define fx<=	fx<=?)
(define fx>	fx>?)
(define fx>=	fx>=?)
(define fx!=	fx!=?)


;;;; min max

(define-min/max-comparison fxmax $fxmax fixnum?)
(define-min/max-comparison fxmin $fxmin fixnum?)

(define/typed ($fxmin {fx1 <fixnum>} {fx2 <fixnum>})
  ;;FIXME  To be  converted to  primitive  operation use  after the  next boot  image
  ;;rotation.  (Marco Maggi; Sun Dec 11, 2016)
  (if (sys::$fx< fx1 fx2) fx1 fx2)
  #;(sys::$fxmax fx1 fx2))

(define/typed ($fxmax {fx1 <fixnum>} {fx2 <fixnum>})
  ;;FIXME  To be  converted to  primitive  operation use  after the  next boot  image
  ;;rotation.  (Marco Maggi; Sun Dec 11, 2016)
  (if (sys::$fx< fx1 fx2) fx2 fx1)
  #;(sys::$fxmax fx1 fx2))


(define ({fxquotient <fixnum>} {x <fixnum>} {y <non-zero-fixnum>})
  (if (eq? y -1)
      ;;Remember that  we cannot simpy  use $fx- because  if X is  (least-fixnum) the
      ;;result will overflow.
      (if (eq? x (least-fixnum))
	  (%overflow-violation __who__ x y)
	(sys::$fx- x))
    (sys::$fxquotient x y)))

(define-fx-operation/one fxabs		$fxabs)
(define-fx-operation/one fxsign		$fxsign)
(define-fx-operation/div fxremainder	$fxremainder)
(define-fx-operation/div fxmodulo	$fxmodulo)

(define/typed ($fxabs {x <fixnum>})
  (if (sys::$fxnegative? x)
      ;;Remember that  we cannot simply use  $fx- because if X  is (least-fixnum) the
      ;;result would overflow.
      (if (sys::$fx= x (least-fixnum))
	  (%overflow-violation __who__ x)
	(sys::fx- x))
    x))

(define/typed ($fxsign {n <fixnum>})
  ;;FIXME  To be  converted to  primitive  operation use  after the  next boot  image
  ;;rotation.  (Marco Maggi; Sun Dec 11, 2016)
  (cond ((sys::$fxnegative? n)	-1)
	((sys::$fxpositive? n)	+1)
	(else			0))
  #;(sys::$fxsign fx1 fx2))

(define-unsafe-operation/two $fxquotient	sys::$fxquotient)

(define/typed ($fxremainder {x <fixnum>} {y <fixnum>})
  ;;We have to assume that the result may not be a fixnum!!!
  (let ((q (sys::$fxquotient x y)))
    (- x (* q y))))

(define/typed ($fxmodulo {n1 <fixnum>} {n2 <fixnum>})
  (* ($fxsign n2)
     ;;We have to assume that the result of the product may not be a fixnum!!!  If N1
     ;;is (least-fixnum) and the sign is -1 the result of the product is a bignum.
     (mod (* ($fxsign n2) n1)
	  (abs n2))))


;;;; arithmetics with carry

(let-syntax
    ((define-fx (syntax-rules ()
		  ((_ (?who ?arg ...) ?body)
		   (define (?who {?arg <fixnum>} ...)
		     ?body))
		  )))

  ;;The operation we want to perform is:
  ;;
  ;; (define-fx (fx*/carry fx1 fx2 fx3)
  ;;   (let* ((s (+ (* fx1 fx2) fx3))
  ;;          (s0 (mod0 s (expt 2 (fixnum-width))))
  ;;          (s1 (div0 s (expt 2 (fixnum-width)))))
  ;;     (values s0 s1)))
  ;;
  ;;but a bit more efficiently.
  ;;
  ;;Notice  that  the unsafe  fixnum  operations  may overflow/underflow  the  fixnum
  ;;representation, and we accept it.
  ;;
  (define-fx (fx*/carry fx1 fx2 fx3)
    (let ((s0 (sys::$fx+ (sys::$fx* fx1 fx2) fx3)))
      (values s0
	      (sra (+ (* fx1 fx2) (- fx3 s0)) (fixnum-width)))))

  ;;The operation we want to perform is:
  ;;
  ;; (define-fx (fx+/carry fx1 fx2 fx3)
  ;;   (let* ((s (+ (+ fx1 fx2) fx3))
  ;;          (s0 (mod0 s (expt 2 (fixnum-width))))
  ;;          (s1 (div0 s (expt 2 (fixnum-width)))))
  ;;     (values s0 s1)))
  ;;
  ;;but a bit more efficiently.
  ;;
  ;;Notice  that  the unsafe  fixnum  operations  may overflow/underflow  the  fixnum
  ;;representation, and we accept it.
  ;;
  (define-fx (fx+/carry fx1 fx2 fx3)
    (let ((s0 (sys::$fx+ (sys::$fx+ fx1 fx2) fx3)))
      (values s0
	      (sra (+ (+ fx1 fx2) (- fx3 s0)) (fixnum-width)))))

  ;;The operation we want to perform is:
  ;;
  ;; (define-fx (fx-/carry fx1 fx2 fx3)
  ;;   (let* ((s (- (- fx1 fx2) fx3))
  ;;          (s0 (mod0 s (expt 2 (fixnum-width))))
  ;;          (s1 (div0 s (expt 2 (fixnum-width)))))
  ;;     (values s0 s1)))
  ;;
  ;;but a bit more efficiently.
  ;;
  ;;Notice  that  the unsafe  fixnum  operations  may overflow/underflow  the  fixnum
  ;;representation, and we accept it.
  ;;
  (define-fx (fx-/carry fx1 fx2 fx3)
    (let ((s0 (sys::$fx- (sys::$fx- fx1 fx2) fx3)))
      (values s0
	      (sra (- (- fx1 fx2) (+ s0 fx3)) (fixnum-width)))))

  #| end of LET-SYNTAX |# )


;;;; conversion

(define (fixnum-in-character-range? obj)
  ;;Defined by Vicare.  Return #t  if OBJ is a fixnum and its value  is in one of the
  ;;ranges acceptable by Unicode code points; otherwise return #f.
  ;;
  (and (fixnum? obj)
       (or (and (sys::$fx>= obj 0)
		(sys::$fx<  obj #xD800))
	   (and (sys::$fx>  obj #xDFFF)
		(sys::$fx<= obj #x10FFFF)))))

(define (fixnum->char {fx <fixnum>})
  (if (fixnum-in-character-range? fx)
      ($fixnum->char fx)
    (procedure-signature-argument-violation __who__
      "expected fixnum in character range"
      1 '(fixnum-in-character-range? fx) fx)))

(define (char->fixnum {ch <char>})
  ($char->fixnum ch))

(module (fixnum->string $fixnum->string)

  (case-define fixnum->string
    (({x <fixnum>})
     ($fixnum->string x 10))

    (({x <fixnum>} r)
     (case r
       ((2)  ($fixnum->string x 2))
       ((8)  ($fixnum->string x 8))
       ((10) ($fixnum->string x 10))
       ((16) ($fixnum->string x 16))
       (else
	(procedure-argument-violation __who__
	  "invalid radix, expected 2, 8, 10 or 16"
	  r)))))

  (define/typed ($fixnum->string {x <fixnum>} {radix <fixnum>})
    (cond ((sys::$fxzero? x)
	   (string #\0))
	  ((sys::$fx> x 0)
	   (call-with-values
	       (lambda () (f x 0 0 radix))
	     (lambda (str j) str)))
	  ((sys::$fx= x (least-fixnum))
	   (string-append ($fixnum->string (sys::$fxquotient x radix)            radix)
			  ($fixnum->string (sys::fx- radix ($fxmodulo x radix)) radix)))
	  (else
	   (receive (str j)
	       (f (sys::fx- 0 x) 1 1 radix)
	     ($string-set! str 0 #\-)
	     str))))

  (define/typed (f {n <fixnum>} {i <fixnum>} {j <fixnum>} {radix <fixnum>})
    (define-constant MAPPING-STRING "0123456789ABCDEF")
    (if (sys::$fxzero? n)
	(values (make-string i) j)
      (let* ((q (sys::$fxquotient n radix))
	     (c ($string-ref MAPPING-STRING (sys::fx- n (sys::fx* q radix)))))
	(receive (str j)
	    (f q (sys::fxadd1 i) j radix)
	  ($string-set! str j c)
	  (values str (sys::fxadd1 j))))))

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
	   ((_ ?who ?unsafe-op overflow-check?)
	    (define (?who {x <fixnum>} {y <non-zero-fixnum>})
	      (if (sys::$fx> y 0)
		  (?unsafe-op x y)
		(if (and overflow-check? (sys::$fx= y -1))
		    (if (sys::$fx= x (least-fixnum))
			(%error-result-not-fixnum __who__ x y)
		      (?unsafe-op x y))
		  (?unsafe-op x y)))))
	   )))
    (define-div-proc fxdiv		$fxdiv		#t)
    (define-div-proc fxmod		$fxmod		#f)
    (define-div-proc fxdiv-and-mod	$fxdiv-and-mod	#t)
    #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

  (define (fxdiv0-and-mod0 {x <fixnum>} {y <non-zero-fixnum>})
    (%check-div-result-not-fixnum __who__ x y)
    (receive-and-return (d m)
	($fxdiv0-and-mod0 x y)
      (unless (fixnum? d)
	(%error-result-not-fixnum __who__ d))
      (unless (fixnum? m)
	(%error-result-not-fixnum __who__ m))))

  (define (fxdiv0 {x <fixnum>} {y <non-zero-fixnum>})
    (%check-div-result-not-fixnum __who__ x y)
    (receive-and-return (d)
	($fxdiv0 x y)
      (unless (fixnum? d)
	(%error-result-not-fixnum __who__ d))))

  (define (fxmod0 {x <fixnum>} {y <non-zero-fixnum>})
    (receive-and-return (d)
	($fxmod0 x y)
      (unless (fixnum? d)
	(%error-result-not-fixnum __who__ d))))

;;; --------------------------------------------------------------------

  (define/typed ($fxdiv-and-mod {n <fixnum>} {m <fixnum>})
    ;;Be careful to use FXQUOTIENT, not the unsafe variant, to avoid overflows!!!
    (let* ((d0 (fxquotient n m))
	   (m0 (sys::fx- n (sys::fx* d0 m))))
      (cond ((sys::$fx>= m0 0)
	     (values d0 m0))
	    ((sys::$fx>= m 0)
	     (values (sys::fx- d0 1) (sys::fx+ m0 m)))
	    (else
	     (values (sys::fx+ d0 1) (sys::fx- m0 m))))))

  (define/typed ($fxdiv {n <fixnum>} {m <fixnum>})
    ;;Be careful to use FXQUOTIENT, not the unsafe variant, to avoid overflows!!!
    (let ((d0 (fxquotient n m)))
      (cond ((sys::$fx>= n (sys::fx* d0 m))
	     d0)
	    ((sys::$fx>= m 0)
	     (sys::fx- d0 1))
	    (else
	     (sys::fx+ d0 1)))))

  (define/typed ($fxmod {n <fixnum>} {m <fixnum>})
    ;;Be careful to use FXQUOTIENT, not the unsafe variant, to avoid overflows!!!
    (let* ((d0 (fxquotient n m))
	   (m0 (sys::fx- n (sys::fx* d0 m))))
      (cond ((sys::$fx>= m0 0)
	     m0)
	    ((sys::$fx>= m 0)
	     (sys::fx+ m0 m))
	    (else
	     (sys::fx- m0 m)))))

;;; --------------------------------------------------------------------

  (define/typed ($fxdiv0-and-mod0 {n <fixnum>} {m <fixnum>})
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

  (define/typed ($fxdiv0 {n <fixnum>} {m <fixnum>})
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

  (define/typed ($fxmod0 {n <fixnum>} {m <fixnum>})
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
    (when (and (sys::$fx= y -1)
	       (sys::$fx= x (least-fixnum)))
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


(library (vicare system fixnums)
  ;;NOTE Putting the  unsafe functions definitions here avoids confusion  in the main
  ;;code between  using the functions  and using  the primitive operations,  which in
  ;;some case really makes the difference  to obtain a correct computation.  So let's
  ;;keep this library!  (Marco Maggi; Sat Jan 7, 2017)
  (options typed-language)
  (export
    $fx+		$fx-
    $fx*
    $fxadd1		$fxsub1
    ;;
    $fx=		$fx!=
    $fx<		$fx>
    $fx<=		$fx>=
    ;;
    $fxior		$fxxor
    $fxand		$fxnot
    $fxsll		$fxsra
    ;;
    $fxzero?
    $fxpositive?	$fxnegative?
    $fxnonpositive?	$fxnonnegative?
    #| end of export |# )
  (import (vicare)
    (prefix (vicare system $fx) sys::))

  (define-syntax define-unsafe-operation/one
    (syntax-rules ()
      ((_ ?unsafe-who ?system-unsafe-who)
       (define/typed (?unsafe-who {N <fixnum>})
	 (?system-unsafe-who N)))
      ))

  (define-syntax define-unsafe-operation/two
    (syntax-rules ()
      ((_ ?unsafe-who ?system-unsafe-who)
       (define/typed (?unsafe-who {N <fixnum>} {M <fixnum>})
	 (?system-unsafe-who N M)))
      ))

;;; --------------------------------------------------------------------

  (define-unsafe-operation/two $fx+		fx+)
  (define-unsafe-operation/two $fx*		fx*)
  (define-unsafe-operation/one $fxadd1		fxadd1)
  (define-unsafe-operation/one $fxsub1		fxsub1)

  (case-define/typed $fx-
    (({N <fixnum>})
     (fx- N))
    (({N <fixnum>} {M <fixnum>})
     (fx- N M)))

  (define-unsafe-operation/two $fx=		fx=?)
  (define-unsafe-operation/two $fx!=		fx!=?)
  (define-unsafe-operation/two $fx<		fx<?)
  (define-unsafe-operation/two $fx<=		fx<=?)
  (define-unsafe-operation/two $fx>		fx>?)
  (define-unsafe-operation/two $fx>=		fx>=?)

;;; --------------------------------------------------------------------

  (define/typed ($fxzero? {N <fixnum>})
    (eq? N 0)
  #;(sys::$fxzero? N))

  (define/typed ($fxpositive? {N <fixnum>})
    (sys::$fx> N 0)
  #;(sys::$fxpositive? N))

  (define/typed ($fxnegative? {N <fixnum>})
    (sys::$fx< N 0)
  #;(sys::$fxnegative? N))

  (define/typed ($fxnonpositive? {N <fixnum>})
    (sys::$fx<= N 0)
  #;(sys::$fxnonpositive? N))

  (define/typed ($fxnonnegative? {N <fixnum>})
    (sys::$fx>= N 0)
  #;(sys::$fxnonnegative? N))

;;; --------------------------------------------------------------------

  (define-unsafe-operation/two $fxior		fxior)
  (define-unsafe-operation/two $fxxor		fxxor)
  (define-unsafe-operation/two $fxand		fxand)
  (define-unsafe-operation/one $fxnot		fxnot)

;;; --------------------------------------------------------------------

  (define-unsafe-operation/two $fxsll		fxsll)
  (define-unsafe-operation/two $fxsra		fxsra)

  #| end of library |# )

;;; end of file
