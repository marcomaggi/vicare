;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: expt with fixnum exponent
;;;Date: Sun Dec  2, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (numerics helpers)
  (ikarus system $ratnums)
  (ikarus system $compnums)
  (ikarus system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: expt with fixnum exponent\n")


(parametrise ((check-test-name	'zero-fixnum-exponent))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check ($expt-number-zero-fixnum ?op) => ?expected)
	 (check ($expt-number-fixnum	?op 0) => ?expected)
	 (check (expt			?op 0) => ?expected)))
      ((_ ?op ?expected ?equal)
       (begin
	 (check ($expt-number-zero-fixnum ?op) (=> ?equal) ?expected)
	 (check ($expt-number-fixnum	?op 0) (=> ?equal) ?expected)
	 (check (expt			?op 0) (=> ?equal) ?expected)))
      ))

;;; --------------------------------------------------------------------

  ;; fixnums
  (test 0	1)
  (test +1	+1)
  (test -1	+1)
  (test +21	+1)
  (test -21	+1)

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op ?expected)
		 (check ($expt-fixnum-fixnum	?op 0) => ?expected))
		((_ ?op ?expected ?equal)
		 (check ($expt-fixnum-fixnum	?op 0) (=> ?equal) ?expected)))))
    (test2 +1	+1)
    (test2 -1	+1)
    (test2 +21	+1)
    (test2 -21	+1))

  ;; bignums
  (test GREATEST-FX-32-bit  +1)
  (test LEAST-FX-32-bit  +1)
  (test BN1  +1)
  (test BN2  +1)
  (test BN3  +1)
  (test BN4  +1)

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op ?expected)
		 (check ($expt-number-fixnum	?op 0) => ?expected))
		((_ ?op ?expected ?equal)
		 (check ($expt-number-fixnum	?op 0) (=> ?equal) ?expected)))))
    (test2 SMALLEST-POSITIVE-BIGNUM  +1)
    (test2 SMALLEST-NEGATIVE-BIGNUM  +1)
    (test2 BN1  +1)
    (test2 BN2  +1)
    (test2 BN3  +1)
    (test2 BN4  +1))

  ;; ratnum
  (test 123/456					+1)
  (test (/ SMALLEST-POSITIVE-BIGNUM 456)	+1)
  (test (/ 456 SMALLEST-POSITIVE-BIGNUM)	+1)

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op ?expected)
		 (check ($expt-ratnum-fixnum	?op 0) => ?expected))
		((_ ?op ?expected ?equal)
		 (check ($expt-ratnum-fixnum	?op 0) (=> ?equal) ?expected)))))
    (test2 123/456				+1)
    (test2 (/ SMALLEST-POSITIVE-BIGNUM 456)	+1)
    (test2 (/ 456 SMALLEST-POSITIVE-BIGNUM)	+1))

  ;; flonums
  (test 123.456		 +1.0)
  (test +nan.0		 +nan.0)
  (test +inf.0		 +1.0)
  (test -inf.0		 +1.0)

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op ?expected)
		 (check ($expt-flonum-fixnum	?op 0) => ?expected))
		((_ ?op ?expected ?equal)
		 (check ($expt-flonum-fixnum	?op 0) (=> ?equal) ?expected)))))
    (test2 123.456	+1.0)
    (test2 +nan.0	+nan.0)
    (test2 +inf.0	+1.0)
    (test2 -inf.0	+1.0))

  ;; compnums
  (test 123+456i	 +1)
  (test 12.3+456i	 +1.0+0.0i)
  (test 123+45.6i	 +1.0+0.0i)
  (test 1/23+4/56i	 +1)

  (test (make-rectangular 123 BN1)	+1)
  (test (make-rectangular BN1 456)	+1)
  (test (make-rectangular 1.23 BN1)	+1.0+0.0i)
  (test (make-rectangular BN1 4.56)	+1.0+0.0i)
  (test (make-rectangular 1/23 BN1)	+1)
  (test (make-rectangular BN1 4/56)	+1)

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op ?expected)
		 (check ($expt-compnum-fixnum	?op 0) => ?expected))
		((_ ?op ?expected ?equal)
		 (check ($expt-compnum-fixnum	?op 0) (=> ?equal) ?expected)))))
    (test2 123+456i	 +1)
    (test2 12.3+456i	 +1.0+0.0i)
    (test2 123+45.6i	 +1.0+0.0i)
    (test2 1/23+4/56i	 +1)

    (test2 (make-rectangular 123 BN1)	+1)
    (test2 (make-rectangular BN1 456)	+1)
    (test2 (make-rectangular 1.23 BN1)	+1.0+0.0i)
    (test2 (make-rectangular BN1 4.56)	+1.0+0.0i)
    (test2 (make-rectangular 1/23 BN1)	+1)
    (test2 (make-rectangular BN1 4/56)	+1))

  ;; cflonums
  (test 12.3+45.6i	+1.0+0.0i		compnum=?)
  (test 12.3+0.0i	+1.0+0.0i		compnum=?)
  (test  0.0+45.6i	+1.0+0.0i		compnum=?)
  (test +inf.0+45.6i	+1.0+0.0i		compnum=?)
  (test 12.3+inf.0i	+1.0+0.0i		compnum=?)
  (test +inf.0+inf.0i	+1.0+0.0i		compnum=?)
  (test +nan.0+45.6i	+nan.0+nan.0i		compnum=?)
  (test 12.3+nan.0i	+nan.0+nan.0i		compnum=?)
  (test +nan.0+nan.0i	+nan.0+nan.0i		compnum=?)

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op ?expected)
		 (check ($expt-cflonum-fixnum	?op 0) => ?expected))
		((_ ?op ?expected ?equal)
		 (check ($expt-cflonum-fixnum	?op 0) (=> ?equal) ?expected)))))
    (test2 12.3+45.6i		+1.0+0.0i		compnum=?)
    (test2 12.3+0.0i		+1.0+0.0i		compnum=?)
    (test2  0.0+45.6i		+1.0+0.0i		compnum=?)
    (test2 +inf.0+45.6i		+1.0+0.0i		compnum=?)
    (test2 12.3+inf.0i		+1.0+0.0i		compnum=?)
    (test2 +inf.0+inf.0i	+1.0+0.0i		compnum=?)
    (test2 +nan.0+45.6i		+nan.0+nan.0i		compnum=?)
    (test2 12.3+nan.0i		+nan.0+nan.0i		compnum=?)
    (test2 +nan.0+nan.0i	+nan.0+nan.0i		compnum=?))

  #t)


(parametrise ((check-test-name	'positive-fixnum-exponent))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op1 ?op2 ?expected)
       (begin
	 (check ($expt-number-positive-fixnum ?op1 ?op2) => ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) => ?expected)
	 (check (expt			?op1 ?op2) => ?expected)))
      ((_ ?op1 ?op2 ?expected ?equal)
       (begin
	 (check ($expt-number-positive-fixnum ?op1 ?op2) (=> ?equal) ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) (=> ?equal) ?expected)
	 (check (expt			?op1 ?op2) (=> ?equal) ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; fixnums

  (test		0	+1	0)
  (test		+1	+1	1)
  (test		-1	+1	-1)

  ;; even exponent
  (test		0	+10	0)
  (test		+1	+10	1)
  (test		-1	+10	1)
  (test		+12	+2	+144)
  (test		-12	+2	+144)

  ;; odd exponent
  (test		0	+11	0)
  (test		+1	+11	+1)
  (test		-1	+11	-1)
  (test		+12	+3	+1728)
  (test		-12	+3	-1728)

  ;; even exponent, result bignum
  (test		+123	+10	792594609605189126649)
  (test		-123	+10	792594609605189126649)

  ;; even exponent, result bignum
  (test		+123	+11	+97489136981438262577827)
  (test		-123	+11	-97489136981438262577827)

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-fixnum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-fixnum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))

    (test2	0	+1	0)
    (test2	+1	+1	1)
    (test2	-1	+1	-1)

    ;; even exponent
    (test2	0	+10	0)
    (test2	+1	+10	1)
    (test2	-1	+10	1)
    (test2	+12	+2	+144)
    (test2	-12	+2	+144)

    ;; odd exponent
    (test2	0	+11	0)
    (test2	+1	+11	+1)
    (test2	-1	+11	-1)
    (test2	+12	+3	+1728)
    (test2	-12	+3	-1728)

    ;; even exponent, result bignum
    (test2	+123	+10	792594609605189126649)
    (test2	-123	+10	792594609605189126649)

    ;; even exponent, result bignum
    (test2	+123	+11	+97489136981438262577827)
    (test2	-123	+11	-97489136981438262577827))

;;; --------------------------------------------------------------------
;;; bignums

  (test	SMALLEST-POSITIVE-BIGNUM	+1	SMALLEST-POSITIVE-BIGNUM)
  (test	SMALLEST-NEGATIVE-BIGNUM	+1	SMALLEST-NEGATIVE-BIGNUM)
  (test BN1				+1	BN1)
  (test BN2				+1	BN2)
  (test BN3				+1	BN3)
  (test BN4				+1	BN4)

  ;; even exponent
  (test	SMALLEST-POSITIVE-BIGNUM	+2	(square SMALLEST-POSITIVE-BIGNUM))
  (test	SMALLEST-NEGATIVE-BIGNUM	+2	(square SMALLEST-NEGATIVE-BIGNUM))
  (test BN1				+2	(square BN1))
  (test BN2				+2	(square BN2))
  (test BN3				+2	(square BN3))
  (test BN4				+2	(square BN4))

  ;; odd exponent
  (test	SMALLEST-POSITIVE-BIGNUM	+3	(cube SMALLEST-POSITIVE-BIGNUM))
  (test	SMALLEST-NEGATIVE-BIGNUM	+3	(cube SMALLEST-NEGATIVE-BIGNUM))
  (test BN1				+3	(cube BN1))
  (test BN2				+3	(cube BN2))
  (test BN3				+3	(cube BN3))
  (test BN4				+3	(cube BN4))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-number-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-number-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))
    (test2 SMALLEST-POSITIVE-BIGNUM	+1	SMALLEST-POSITIVE-BIGNUM)
    (test2 SMALLEST-NEGATIVE-BIGNUM	+1	SMALLEST-NEGATIVE-BIGNUM)
    (test2 BN1				+1	BN1)
    (test2 BN2				+1	BN2)
    (test2 BN3				+1	BN3)
    (test2 BN4				+1	BN4)

    ;; even exponent
    (test2 SMALLEST-POSITIVE-BIGNUM	+2	(square SMALLEST-POSITIVE-BIGNUM))
    (test2 SMALLEST-NEGATIVE-BIGNUM	+2	(square SMALLEST-NEGATIVE-BIGNUM))
    (test2 BN1				+2	(square BN1))
    (test2 BN2				+2	(square BN2))
    (test2 BN3				+2	(square BN3))
    (test2 BN4				+2	(square BN4))

    ;; odd exponent
    (test2 SMALLEST-POSITIVE-BIGNUM	+3	(cube SMALLEST-POSITIVE-BIGNUM))
    (test2 SMALLEST-NEGATIVE-BIGNUM	+3	(cube SMALLEST-NEGATIVE-BIGNUM))
    (test2 BN1				+3	(cube BN1))
    (test2 BN2				+3	(cube BN2))
    (test2 BN3				+3	(cube BN3))
    (test2 BN4				+3	(cube BN4)))

;;; --------------------------------------------------------------------
;;; ratnums

  (test		+2/3	+1	+2/3)
  (test		-2/3	+1	-2/3)

  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	+1	(/ 7 SMALLEST-POSITIVE-BIGNUM))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	+1	(/ 7 SMALLEST-NEGATIVE-BIGNUM))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	+1	(/ SMALLEST-POSITIVE-BIGNUM 7))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	+1	(/ SMALLEST-NEGATIVE-BIGNUM 7))

  ;; even exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	+2	(square (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	+2	(square (/ 7 SMALLEST-NEGATIVE-BIGNUM)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	+2	(square (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	+2	(square (/ SMALLEST-NEGATIVE-BIGNUM 7)))

  ;; odd exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	+3	(cube (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	+3	(cube (/ 7 SMALLEST-NEGATIVE-BIGNUM)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	+3	(cube (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	+3	(cube (/ SMALLEST-NEGATIVE-BIGNUM 7)))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-ratnum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-ratnum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))
    (test2 +2/3	+1	+2/3)
    (test2 -2/3	+1	-2/3)

    (test2 (/ 7 SMALLEST-POSITIVE-BIGNUM)	+1	(/ 7 SMALLEST-POSITIVE-BIGNUM))
    (test2 (/ 7 SMALLEST-NEGATIVE-BIGNUM)	+1	(/ 7 SMALLEST-NEGATIVE-BIGNUM))
    (test2 (/ SMALLEST-POSITIVE-BIGNUM 7)	+1	(/ SMALLEST-POSITIVE-BIGNUM 7))
    (test2 (/ SMALLEST-NEGATIVE-BIGNUM 7)	+1	(/ SMALLEST-NEGATIVE-BIGNUM 7))

    ;; even exponent
    (test2 (/ 7 SMALLEST-POSITIVE-BIGNUM)	+2	(square (/ 7 SMALLEST-POSITIVE-BIGNUM)))
    (test2 (/ 7 SMALLEST-NEGATIVE-BIGNUM)	+2	(square (/ 7 SMALLEST-NEGATIVE-BIGNUM)))
    (test2 (/ SMALLEST-POSITIVE-BIGNUM 7)	+2	(square (/ SMALLEST-POSITIVE-BIGNUM 7)))
    (test2 (/ SMALLEST-NEGATIVE-BIGNUM 7)	+2	(square (/ SMALLEST-NEGATIVE-BIGNUM 7)))

    ;; odd exponent
    (test2 (/ 7 SMALLEST-POSITIVE-BIGNUM)	+3	(cube (/ 7 SMALLEST-POSITIVE-BIGNUM)))
    (test2 (/ 7 SMALLEST-NEGATIVE-BIGNUM)	+3	(cube (/ 7 SMALLEST-NEGATIVE-BIGNUM)))
    (test2 (/ SMALLEST-POSITIVE-BIGNUM 7)	+3	(cube (/ SMALLEST-POSITIVE-BIGNUM 7)))
    (test2 (/ SMALLEST-NEGATIVE-BIGNUM 7)	+3	(cube (/ SMALLEST-NEGATIVE-BIGNUM 7))))

;;; --------------------------------------------------------------------
;;; flonums

  (test		+0.0		+1	+0.0)
  (test		-0.0		+1	-0.0)
  (test		+1.0		+1	+1.0)
  (test		-1.0		+1	-1.0)
  (test		+nan.0		+1	+nan.0)
  (test		+inf.0		+1	+inf.0)
  (test		-inf.0		+1	-inf.0)
  (test		+123.456	+1	+123.456)
  (test		-123.456	+1	-123.456)

  ;; even exponent
  (test		+0.0		+2	+0.0)
  (test		-0.0		+2	+0.0)
  (test		+1.0		+2	+1.0)
  (test		-1.0		+2	+1.0)
  (test		+nan.0		+2	+nan.0)
  (test		+inf.0		+2	+inf.0)
  (test		-inf.0		+2	+inf.0)
  (test		+123.456	+2	(square +123.456))
  (test		-123.456	+2	(square -123.456))

  ;; odd exponent
  (test		+0.0		+3	+0.0)
  (test		-0.0		+3	-0.0)
  (test		+1.0		+3	+1.0)
  (test		-1.0		+3	-1.0)
  (test		+nan.0		+3	+nan.0)
  (test		+inf.0		+3	+inf.0)
  (test		-inf.0		+3	-inf.0)
  (test		+123.456	+3	(cube +123.456))
  (test		-123.456	+3	(cube -123.456))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-flonum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-flonum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))
    (test2	+0.0		+1	+0.0)
    (test2	-0.0		+1	-0.0)
    (test2	+1.0		+1	+1.0)
    (test2	-1.0		+1	-1.0)
    (test2	+nan.0		+1	+nan.0)
    (test2	+inf.0		+1	+inf.0)
    (test2	-inf.0		+1	-inf.0)
    (test2	+123.456	+1	+123.456)
    (test2	-123.456	+1	-123.456)

    ;; even exponent
    (test2	+0.0		+2	+0.0)
    (test2	-0.0		+2	+0.0)
    (test2	+1.0		+2	+1.0)
    (test2	-1.0		+2	+1.0)
    (test2	+nan.0		+2	+nan.0)
    (test2	+inf.0		+2	+inf.0)
    (test2	-inf.0		+2	+inf.0)
    (test2	+123.456	+2	(square +123.456))
    (test2	-123.456	+2	(square -123.456))

    ;; odd exponent
    (test2	+0.0		+3	+0.0)
    (test2	-0.0		+3	-0.0)
    (test2	+1.0		+3	+1.0)
    (test2	-1.0		+3	-1.0)
    (test2	+nan.0		+3	+nan.0)
    (test2	+inf.0		+3	+inf.0)
    (test2	-inf.0		+3	-inf.0)
    (test2	+123.456	+3	(cube +123.456))
    (test2	-123.456	+3	(cube -123.456)))

;;; --------------------------------------------------------------------
;;; compnums

  (test		123+456i	+1	123+456i)
  (test		12.3+456i	+1	12.3+456i)
  (test		123+45.6i	+1	123+45.6i)
  (test		1/23+4/56i	+1	1/23+4/56i)

  (test		+nan.0+456i	+1	+nan.0+456i	inexact=?)
  (test		123+nan.0i	+1	+123+nan.0i	inexact=?)

  (test		+inf.0+456i	+1	+inf.0+456i	inexact=?)
  (test		123+inf.0i	+1	+123+inf.0i	inexact=?)

  (test		-inf.0+456i	+1	-inf.0+456i)
  (test		123-inf.0i	+1	123-inf.0i)

  (test (C 123 BN1)	+1	(C 123 BN1))
  (test (C BN1 456)	+1	(C BN1 456))
  (test (C 1.23 BN1)	+1	(C 1.23 BN1))
  (test (C BN1 4.56)	+1	(C BN1 4.56))
  (test (C 1/23 BN1)	+1	(C 1/23 BN1))
  (test (C BN1 4/56)	+1	(C BN1 4/56))

  (test (C 123 BN2)	+1	(C 123 BN2))
  (test (C BN2 456)	+1	(C BN2 456))
  (test (C 1.23 BN2)	+1	(C 1.23 BN2))
  (test (C BN2 4.56)	+1	(C BN2 4.56))
  (test (C 1/23 BN2)	+1	(C 1/23 BN2))
  (test (C BN2 4/56)	+1	(C BN2 4/56))

  (test (C 123 BN3)	+1	(C 123 BN3))
  (test (C BN3 456)	+1	(C BN3 456))
  (test (C 1.23 BN3)	+1	(C 1.23 BN3))
  (test (C BN3 4.56)	+1	(C BN3 4.56))
  (test (C 1/23 BN3)	+1	(C 1/23 BN3))
  (test (C BN3 4/56)	+1	(C BN3 4/56))

  (test (C 123 BN4)	+1	(C 123 BN4))
  (test (C BN4 456)	+1	(C BN4 456))
  (test (C 1.23 BN4)	+1	(C 1.23 BN4))
  (test (C BN4 4.56)	+1	(C BN4 4.56))
  (test (C 1/23 BN4)	+1	(C 1/23 BN4))
  (test (C BN4 4/56)	+1	(C BN4 4/56))

  ;; even exponent
  (test		123+456i	+2	(square 123+456i))
  (test		12.3+456i	+2	(square 12.3+456.0i))
  (test		123+45.6i	+2	(square 123.0+45.6i))
  (test		1/23+4/56i	+2	(square 1/23+4/56i))

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	+2	+nan.0+nan.0i	inexact=?)
  (test		123+nan.0i	+2	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	+2	+inf.0+nan.0i	inexact=?)
  (test		123+inf.0i	+2	-inf.0+inf.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	+2	+inf.0-inf.0i	inexact=?)
  (test		123-inf.0i	+2	-inf.0-inf.0i	inexact=?)

  (test		(C 123 BN1)	+2	(square (C 123 BN1)))
  (test		(C BN1 456)	+2	(square (C BN1 456)))
  (test		(C 1.23 BN1)	+2	(square (C 1.23 BN1)))
  (test		(C BN1 4.56)	+2	(square (C BN1 4.56)))
  (test		(C 1/23 BN1)	+2	(square (C 1/23 BN1)))
  (test		(C BN1 4/56)	+2	(square (C BN1 4/56)))

  (test		(C 123 BN2)	+2	(square (C 123 BN2)))
  (test		(C BN2 456)	+2	(square (C BN2 456)))
  (test		(C 1.23 BN2)	+2	(square (C 1.23 BN2)))
  (test		(C BN2 4.56)	+2	(square (C BN2 4.56)))
  (test		(C 1/23 BN2)	+2	(square (C 1/23 BN2)))
  (test		(C BN2 4/56)	+2	(square (C BN2 4/56)))

  (test		(C 123 BN3)	+2	(square (C 123 BN3)))
  (test		(C BN3 456)	+2	(square (C BN3 456)))
  (test		(C 1.23 BN3)	+2	(square (C 1.23 BN3)))
  (test		(C BN3 4.56)	+2	(square (C BN3 4.56)))
  (test		(C 1/23 BN3)	+2	(square (C 1/23 BN3)))
  (test		(C BN3 4/56)	+2	(square (C BN3 4/56)))

  (test		(C 123 BN4)	+2	(square (C 123 BN4)))
  (test		(C BN4 456)	+2	(square (C BN4 456)))
  (test		(C 1.23 BN4)	+2	(square (C 1.23 BN4)))
  (test		(C BN4 4.56)	+2	(square (C BN4 4.56)))
  (test		(C 1/23 BN4)	+2	(square (C 1/23 BN4)))
  (test		(C BN4 4/56)	+2	(square (C BN4 4/56)))

  ;; even exponent
  (test		123+456i	+3	(cube 123+456i)		inexact=?)
  (test		12.3+456i	+3	(cube 12.3+456.0i)	inexact=?)
  (test		123+45.6i	+3	(cube 123.0+45.6i)	inexact=?)
  (test		1/23+4/56i	+3	(cube 1/23+4/56i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	+3	+nan.0+nan.0i	inexact=?)
  (test		123+nan.0i	+3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	+3	+inf.0+nan.0i	inexact=?)
  (test		123+inf.0i	+3	-inf.0-inf.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	+3	-inf.0+inf.0i	inexact=?)
  (test		123-inf.0i	+3	-inf.0+inf.0i	inexact=?)

  (test		(C 123 BN1)	+3	(cube (C 123 BN1)))
  (test		(C BN1 456)	+3	(cube (C BN1 456)))
  (test		(C 1.23 BN1)	+3	(cube (C 1.23 BN1))	inexact=?)
   (test		(C BN1 4.56)	+3	(cube (C BN1 4.56))	inexact=?)
  (test		(C 1/23 BN1)	+3	(cube (C 1/23 BN1)))
  (test		(C BN1 4/56)	+3	(cube (C BN1 4/56)))

  (test		(C 123 BN2)	+3	(cube (C 123 BN2)))
  (test		(C BN2 456)	+3	(cube (C BN2 456)))
  (test		(C 1.23 BN2)	+3	(cube (C 1.23 BN2))	inexact=?)
  (test		(C BN2 4.56)	+3	(cube (C BN2 4.56))	inexact=?)
  (test		(C 1/23 BN2)	+3	(cube (C 1/23 BN2)))
  (test		(C BN2 4/56)	+3	(cube (C BN2 4/56)))

  (test		(C 123 BN3)	+3	(cube (C 123 BN3)))
  (test		(C BN3 456)	+3	(cube (C BN3 456)))
  (test		(C 1.23 BN3)	+3	(cube (C 1.23 BN3))	inexact=?)
  (test		(C BN3 4.56)	+3	(cube (C BN3 4.56))	inexact=?)
  (test		(C 1/23 BN3)	+3	(cube (C 1/23 BN3)))
  (test		(C BN3 4/56)	+3	(cube (C BN3 4/56)))

  (test		(C 123 BN4)	+3	(cube (C 123 BN4)))
  (test		(C BN4 456)	+3	(cube (C BN4 456)))
  (test		(C 1.23 BN4)	+3	(cube (C 1.23 BN4))	inexact=?)
  (test		(C BN4 4.56)	+3	(cube (C BN4 4.56))	inexact=?)
  (test		(C 1/23 BN4)	+3	(cube (C 1/23 BN4)))
  (test		(C BN4 4/56)	+3	(cube (C BN4 4/56)))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-compnum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-compnum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))
    (test2	123+456i	+1	123+456i)
    (test2	12.3+456i	+1	12.3+456i)
    (test2	123+45.6i	+1	123+45.6i)
    (test2	1/23+4/56i	+1	1/23+4/56i)

    (test2	+nan.0+456i	+1	+nan.0+456i	inexact=?)
    (test2	123+nan.0i	+1	+123+nan.0i	inexact=?)

    (test2	+inf.0+456i	+1	+inf.0+456i	inexact=?)
    (test2	123+inf.0i	+1	+123+inf.0i	inexact=?)

    (test2	-inf.0+456i	+1	-inf.0+456i)
    (test2	123-inf.0i	+1	123-inf.0i)

    (test2	(C 123 BN1)	+1	(C 123 BN1))
    (test2	(C BN1 456)	+1	(C BN1 456))
    (test2	(C 1.23 BN1)	+1	(C 1.23 BN1))
    (test2	(C BN1 4.56)	+1	(C BN1 4.56))
    (test2	(C 1/23 BN1)	+1	(C 1/23 BN1))
    (test2	(C BN1 4/56)	+1	(C BN1 4/56))

    (test2 (C 123 BN2)	+1	(C 123 BN2))
    (test2 (C BN2 456)	+1	(C BN2 456))
    (test2 (C 1.23 BN2)	+1	(C 1.23 BN2))
    (test2 (C BN2 4.56)	+1	(C BN2 4.56))
    (test2 (C 1/23 BN2)	+1	(C 1/23 BN2))
    (test2 (C BN2 4/56)	+1	(C BN2 4/56))

    (test2 (C 123 BN3)	+1	(C 123 BN3))
    (test2 (C BN3 456)	+1	(C BN3 456))
    (test2 (C 1.23 BN3)	+1	(C 1.23 BN3))
    (test2 (C BN3 4.56)	+1	(C BN3 4.56))
    (test2 (C 1/23 BN3)	+1	(C 1/23 BN3))
    (test2 (C BN3 4/56)	+1	(C BN3 4/56))

    (test2 (C 123 BN4)	+1	(C 123 BN4))
    (test2 (C BN4 456)	+1	(C BN4 456))
    (test2 (C 1.23 BN4)	+1	(C 1.23 BN4))
    (test2 (C BN4 4.56)	+1	(C BN4 4.56))
    (test2 (C 1/23 BN4)	+1	(C 1/23 BN4))
    (test2 (C BN4 4/56)	+1	(C BN4 4/56))

    ;; even exponent
    (test2	123+456i	+2	(square 123+456i))
    (test2	12.3+456i	+2	(square 12.3+456.0i))
    (test2	123+45.6i	+2	(square 123.0+45.6i))
    (test2	1/23+4/56i	+2	(square 1/23+4/56i))

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+nan.0+456i	+2	+nan.0+nan.0i	inexact=?)
    (test2	123+nan.0i	+2	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+456i	+2	+inf.0+nan.0i	inexact=?)
    (test2	123+inf.0i	+2	-inf.0+inf.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	-inf.0+456i	+2	+inf.0-inf.0i	inexact=?)
    (test2	123-inf.0i	+2	-inf.0-inf.0i	inexact=?)

    (test2	(C 123 BN1)	+2	(square (C 123 BN1)))
    (test2	(C BN1 456)	+2	(square (C BN1 456)))
    (test2	(C 1.23 BN1)	+2	(square (C 1.23 BN1)))
    (test2	(C BN1 4.56)	+2	(square (C BN1 4.56)))
    (test2	(C 1/23 BN1)	+2	(square (C 1/23 BN1)))
    (test2	(C BN1 4/56)	+2	(square (C BN1 4/56)))

    (test2	(C 123 BN2)	+2	(square (C 123 BN2)))
    (test2	(C BN2 456)	+2	(square (C BN2 456)))
    (test2	(C 1.23 BN2)	+2	(square (C 1.23 BN2)))
    (test2	(C BN2 4.56)	+2	(square (C BN2 4.56)))
    (test2	(C 1/23 BN2)	+2	(square (C 1/23 BN2)))
    (test2	(C BN2 4/56)	+2	(square (C BN2 4/56)))

    (test2	(C 123 BN3)	+2	(square (C 123 BN3)))
    (test2	(C BN3 456)	+2	(square (C BN3 456)))
    (test2	(C 1.23 BN3)	+2	(square (C 1.23 BN3)))
    (test2	(C BN3 4.56)	+2	(square (C BN3 4.56)))
    (test2	(C 1/23 BN3)	+2	(square (C 1/23 BN3)))
    (test2	(C BN3 4/56)	+2	(square (C BN3 4/56)))

    (test2	(C 123 BN4)	+2	(square (C 123 BN4)))
    (test2	(C BN4 456)	+2	(square (C BN4 456)))
    (test2	(C 1.23 BN4)	+2	(square (C 1.23 BN4)))
    (test2	(C BN4 4.56)	+2	(square (C BN4 4.56)))
    (test2	(C 1/23 BN4)	+2	(square (C 1/23 BN4)))
    (test2	(C BN4 4/56)	+2	(square (C BN4 4/56)))

    ;; even exponent
    (test2	123+456i	+3	(cube 123+456i)		inexact=?)
    (test2	12.3+456i	+3	(cube 12.3+456.0i)	inexact=?)
    (test2	123+45.6i	+3	(cube 123.0+45.6i)	inexact=?)
    (test2	1/23+4/56i	+3	(cube 1/23+4/56i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+nan.0+456i	+3	+nan.0+nan.0i	inexact=?)
    (test2	123+nan.0i	+3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+456i	+3	+inf.0+nan.0i	inexact=?)
    (test2	123+inf.0i	+3	-inf.0-inf.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	-inf.0+456i	+3	-inf.0+inf.0i	inexact=?)
    (test2	123-inf.0i	+3	-inf.0+inf.0i	inexact=?)

    (test2	(C 123 BN1)	+3	(cube (C 123 BN1)))
    (test2	(C BN1 456)	+3	(cube (C BN1 456)))
    (test2	(C 1.23 BN1)	+3	(cube (C 1.23 BN1))	inexact=?)
    (test2	(C BN1 4.56)	+3	(cube (C BN1 4.56))	inexact=?)
    (test2	(C 1/23 BN1)	+3	(cube (C 1/23 BN1)))
    (test2	(C BN1 4/56)	+3	(cube (C BN1 4/56)))

    (test2	(C 123 BN2)	+3	(cube (C 123 BN2)))
    (test2	(C BN2 456)	+3	(cube (C BN2 456)))
    (test2	(C 1.23 BN2)	+3	(cube (C 1.23 BN2))	inexact=?)
    (test2	(C BN2 4.56)	+3	(cube (C BN2 4.56))	inexact=?)
    (test2	(C 1/23 BN2)	+3	(cube (C 1/23 BN2)))
    (test2	(C BN2 4/56)	+3	(cube (C BN2 4/56)))

    (test2	(C 123 BN3)	+3	(cube (C 123 BN3)))
    (test2	(C BN3 456)	+3	(cube (C BN3 456)))
    (test2	(C 1.23 BN3)	+3	(cube (C 1.23 BN3))	inexact=?)
    (test2	(C BN3 4.56)	+3	(cube (C BN3 4.56))	inexact=?)
    (test2	(C 1/23 BN3)	+3	(cube (C 1/23 BN3)))
    (test2	(C BN3 4/56)	+3	(cube (C BN3 4/56)))

    (test2	(C 123 BN4)	+3	(cube (C 123 BN4)))
    (test2	(C BN4 456)	+3	(cube (C BN4 456)))
    (test2	(C 1.23 BN4)	+3	(cube (C 1.23 BN4))	inexact=?)
    (test2	(C BN4 4.56)	+3	(cube (C BN4 4.56))	inexact=?)
    (test2	(C 1/23 BN4)	+3	(cube (C 1/23 BN4)))
    (test2	(C BN4 4/56)	+3	(cube (C BN4 4/56))))

;;; --------------------------------------------------------------------
;;; cflonums

  (test		12.3+45.6i	+1	12.3+45.6i	inexact=?)
  (test		12.3+0.0i	+1	12.3+0.0i	inexact=?)
  (test		0.0+45.6i	+1	0.0+45.6i	inexact=?)
  (test		+inf.0+45.6i	+1	+inf.0+45.6i	inexact=?)
  (test		12.3+inf.0i	+1	12.3+inf.0i	inexact=?)

  (test		+nan.0+45.6i	+1	+nan.0+45.6i	inexact=?)
  (test		12.3+nan.0i	+1	+12.3+nan.0i	inexact=?)
  (test		+nan.0+nan.0i	+1	+nan.0+nan.0i	inexact=?)

  (test		+inf.0+inf.0i	+1	+inf.0+inf.0i	inexact=?)
  (test		+inf.0-inf.0i	+1	+inf.0-inf.0i	inexact=?)
  (test		-inf.0+inf.0i	+1	-inf.0+inf.0i	inexact=?)
  (test		-inf.0-inf.0i	+1	-inf.0-inf.0i	inexact=?)

  ;; even exponent
  (test		12.3+45.6i	+2	(square 12.3+45.6i)	inexact=?)
  (test		12.3+0.0i	+2	(square 12.3+0.0i)	inexact=?)
  (test		0.0+45.6i	+2	(square 0.0+45.6i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+45.6i	+2	+inf.0+nan.0i		inexact=?)
  (test		12.3+inf.0i	+2	-inf.0+inf.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+45.6i	+2	+nan.0+nan.0i		inexact=?)
  (test		12.3+nan.0i	+2	+nan.0+nan.0i		inexact=?)
  (test		+nan.0+nan.0i	+2	+nan.0+nan.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+inf.0i	+2	+inf.0+inf.0i		inexact=?)
  (test		+inf.0-inf.0i	+2	+inf.0-inf.0i		inexact=?)
  (test		-inf.0+inf.0i	+2	-inf.0-inf.0i		inexact=?)
  (test		-inf.0-inf.0i	+2	-inf.0+inf.0i		inexact=?)

  ;; odd exponent
  (test		12.3+45.6i	+3	(cube 12.3+45.6i)	inexact=?)
  (test		12.3+0.0i	+3	(cube 12.3+0.0i)	inexact=?)
  (test		0.0+45.6i	+3	(cube 0.0+45.6i)	inexact=?)
  (test		+inf.0+45.6i	+3	+inf.0+nan.0i		inexact=?)
  (test		12.3+inf.0i	+3	-inf.0-inf.0i		inexact=?)

  (test		+nan.0+45.6i	+3	+nan.0+nan.0i		inexact=?)
  (test		12.3+nan.0i	+3	+nan.0+nan.0i		inexact=?)
  (test		+nan.0+nan.0i	+3	+nan.0+nan.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+inf.0i	+3	-inf.0+inf.0i	inexact=?)
  (test		+inf.0-inf.0i	+3	-inf.0-inf.0i	inexact=?)
  (test		-inf.0+inf.0i	+3	+inf.0+inf.0i	inexact=?)
  (test		-inf.0-inf.0i	+3	+inf.0-inf.0i	inexact=?)

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-cflonum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-cflonum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))
    (test2	12.3+45.6i	+1	12.3+45.6i	inexact=?)
    (test2	12.3+0.0i	+1	12.3+0.0i	inexact=?)
    (test2	0.0+45.6i	+1	0.0+45.6i	inexact=?)
    (test2	+inf.0+45.6i	+1	+inf.0+45.6i	inexact=?)
    (test2	12.3+inf.0i	+1	12.3+inf.0i	inexact=?)

    (test2	+nan.0+45.6i	+1	+nan.0+45.6i	inexact=?)
    (test2	12.3+nan.0i	+1	+12.3+nan.0i	inexact=?)
    (test2	+nan.0+nan.0i	+1	+nan.0+nan.0i	inexact=?)

    (test2	+inf.0+inf.0i	+1	+inf.0+inf.0i	inexact=?)
    (test2	+inf.0-inf.0i	+1	+inf.0-inf.0i	inexact=?)
    (test2	-inf.0+inf.0i	+1	-inf.0+inf.0i	inexact=?)
    (test2	-inf.0-inf.0i	+1	-inf.0-inf.0i	inexact=?)

    ;; even exponent
    (test2	12.3+45.6i	+2	(square 12.3+45.6i)	inexact=?)
    (test2	12.3+0.0i	+2	(square 12.3+0.0i)	inexact=?)
    (test2	0.0+45.6i	+2	(square 0.0+45.6i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+45.6i	+2	+inf.0+nan.0i		inexact=?)
    (test2	12.3+inf.0i	+2	-inf.0+inf.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+nan.0+45.6i	+2	+nan.0+nan.0i		inexact=?)
    (test2	12.3+nan.0i	+2	+nan.0+nan.0i		inexact=?)
    (test2	+nan.0+nan.0i	+2	+nan.0+nan.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+inf.0i	+2	+inf.0+inf.0i		inexact=?)
    (test2	+inf.0-inf.0i	+2	+inf.0-inf.0i		inexact=?)
    (test2	-inf.0+inf.0i	+2	-inf.0-inf.0i		inexact=?)
    (test2	-inf.0-inf.0i	+2	-inf.0+inf.0i		inexact=?)

    ;; odd exponent
    (test2	12.3+45.6i	+3	(cube 12.3+45.6i)	inexact=?)
    (test2	12.3+0.0i	+3	(cube 12.3+0.0i)	inexact=?)
    (test2	0.0+45.6i	+3	(cube 0.0+45.6i)	inexact=?)
    (test2	+inf.0+45.6i	+3	+inf.0+nan.0i		inexact=?)
    (test2	12.3+inf.0i	+3	-inf.0-inf.0i		inexact=?)

    (test2	+nan.0+45.6i	+3	+nan.0+nan.0i		inexact=?)
    (test2	12.3+nan.0i	+3	+nan.0+nan.0i		inexact=?)
    (test2	+nan.0+nan.0i	+3	+nan.0+nan.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+inf.0i	+3	-inf.0+inf.0i	inexact=?)
    (test2	+inf.0-inf.0i	+3	-inf.0-inf.0i	inexact=?)
    (test2	-inf.0+inf.0i	+3	+inf.0+inf.0i	inexact=?)
    (test2	-inf.0-inf.0i	+3	+inf.0-inf.0i	inexact=?))

  #t)


(parametrise ((check-test-name	'negative-fixnum-exponent))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op1 ?op2 ?expected)
       (begin
	 (check ($expt-number-negative-fixnum ?op1 ?op2) => ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) => ?expected)
	 (check (expt			?op1 ?op2) => ?expected)))
      ((_ ?op1 ?op2 ?expected ?equal)
       (begin
	 (check ($expt-number-negative-fixnum ?op1 ?op2) (=> ?equal) ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) (=> ?equal) ?expected)
	 (check (expt			?op1 ?op2) (=> ?equal) ?expected)))
      ))

  (define-syntax catch-division-by-zero
    (syntax-rules ()
      ((_ . ?body)
       (check
	   (guard (E ((assertion-violation? E)
		      (condition-message E))
		     (else E))
	     (begin . ?body))
	 => "division by zero"))))

  (define-syntax test-division-by-zero
    (syntax-rules ()
      ((_ ?exponent)
       (begin
	 (catch-division-by-zero (expt 0 ?exponent))
	 (catch-division-by-zero ($expt-number-fixnum 0 ?exponent))
	 (catch-division-by-zero ($expt-number-negative-fixnum 0 ?exponent))
	 (catch-division-by-zero ($expt-fixnum-fixnum 0 ?exponent))
	 (catch-division-by-zero ($expt-fixnum-negative-fixnum 0 ?exponent))))))

;;; --------------------------------------------------------------------
;;; fixnums

  (test-division-by-zero -1)
  (test		+1	-1	1)
  (test		-1	-1	-1)

  ;; even exponent
  (test-division-by-zero -10)
  (test		+1	-10	1)
  (test		-1	-10	1)
  (test		+12	-2	+1/144)
  (test		-12	-2	+1/144)

  ;; odd exponent
  (test-division-by-zero -11)
  (test		+1	-11	+1)
  (test		-1	-11	-1)
  (test		+12	-3	+1/1728)
  (test		-12	-3	-1/1728)

  ;; even exponent, result bignum
  (test		+123	-10	1/792594609605189126649)
  (test		-123	-10	1/792594609605189126649)

  ;; even exponent, result bignum
  (test		+123	-11	+1/97489136981438262577827)
  (test		-123	-11	-1/97489136981438262577827)

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-fixnum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-fixnum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))
    (test2	+1	-1	1)
    (test2	-1	-1	-1)

    ;; even exponent
    (test2	+1	-10	1)
    (test2	-1	-10	1)
    (test2	+12	-2	+1/144)
    (test2	-12	-2	+1/144)

    ;; odd exponent
    (test2	+1	-11	+1)
    (test2	-1	-11	-1)
    (test2	+12	-3	+1/1728)
    (test2	-12	-3	-1/1728)

    ;; even exponent, result bignum
    (test2	+123	-10	1/792594609605189126649)
    (test2	-123	-10	1/792594609605189126649)

    ;; even exponent, result bignum
    (test2	+123	-11	+1/97489136981438262577827)
    (test2	-123	-11	-1/97489136981438262577827))

;;; --------------------------------------------------------------------
;;; bignums

  (test	SMALLEST-POSITIVE-BIGNUM	-1	(/ SMALLEST-POSITIVE-BIGNUM))
  (test	SMALLEST-NEGATIVE-BIGNUM	-1	(/ SMALLEST-NEGATIVE-BIGNUM))
  (test BN1				-1	(/ BN1))
  (test BN2				-1	(/ BN2))
  (test BN3				-1	(/ BN3))
  (test BN4				-1	(/ BN4))

  ;; even exponent
  (test	SMALLEST-POSITIVE-BIGNUM	-2	(/ (square SMALLEST-POSITIVE-BIGNUM)))
  (test	SMALLEST-NEGATIVE-BIGNUM	-2	(/ (square SMALLEST-NEGATIVE-BIGNUM)))
  (test BN1				-2	(/ (square BN1)))
  (test BN2				-2	(/ (square BN2)))
  (test BN3				-2	(/ (square BN3)))
  (test BN4				-2	(/ (square BN4)))

  ;; odd exponent
  (test	SMALLEST-POSITIVE-BIGNUM	-3	(/ (cube SMALLEST-POSITIVE-BIGNUM)))
  (test	SMALLEST-NEGATIVE-BIGNUM	-3	(/ (cube SMALLEST-NEGATIVE-BIGNUM)))
  (test BN1				-3	(/ (cube BN1)))
  (test BN2				-3	(/ (cube BN2)))
  (test BN3				-3	(/ (cube BN3)))
  (test BN4				-3	(/ (cube BN4)))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-number-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-number-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))

    (test2	SMALLEST-POSITIVE-BIGNUM	-1	(/ SMALLEST-POSITIVE-BIGNUM))
    (test2	SMALLEST-NEGATIVE-BIGNUM	-1	(/ SMALLEST-NEGATIVE-BIGNUM))
    (test2	BN1				-1	(/ BN1))
    (test2	BN2				-1	(/ BN2))
    (test2	BN3				-1	(/ BN3))
    (test2	BN4				-1	(/ BN4))

    ;; even exponent
    (test2	SMALLEST-POSITIVE-BIGNUM	-2	(/ (square SMALLEST-POSITIVE-BIGNUM)))
    (test2	SMALLEST-NEGATIVE-BIGNUM	-2	(/ (square SMALLEST-NEGATIVE-BIGNUM)))
    (test2	BN1				-2	(/ (square BN1)))
    (test2	BN2				-2	(/ (square BN2)))
    (test2	BN3				-2	(/ (square BN3)))
    (test2	BN4				-2	(/ (square BN4)))

    ;; odd exponent
    (test2	SMALLEST-POSITIVE-BIGNUM	-3	(/ (cube SMALLEST-POSITIVE-BIGNUM)))
    (test2	SMALLEST-NEGATIVE-BIGNUM	-3	(/ (cube SMALLEST-NEGATIVE-BIGNUM)))
    (test2	BN1				-3	(/ (cube BN1)))
    (test2	BN2				-3	(/ (cube BN2)))
    (test2	BN3				-3	(/ (cube BN3)))
    (test2	BN4				-3	(/ (cube BN4))))

;;; --------------------------------------------------------------------
;;; ratnums

  (test		+2/3	-1	+3/2)
  (test		-2/3	-1	-3/2)

  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-1	(/ SMALLEST-POSITIVE-BIGNUM 7))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-1	(/ SMALLEST-NEGATIVE-BIGNUM 7))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	-1	(/ 7 SMALLEST-POSITIVE-BIGNUM))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-1	(/ 7 SMALLEST-NEGATIVE-BIGNUM))

  ;; even exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-2	(square (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-2	(square (/ SMALLEST-NEGATIVE-BIGNUM 7)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	-2	(square (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-2	(square (/ 7 SMALLEST-NEGATIVE-BIGNUM)))

  ;; odd exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-3	(cube (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-3	(cube (/ SMALLEST-NEGATIVE-BIGNUM 7)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	-3	(cube (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-3	(cube (/ 7 SMALLEST-NEGATIVE-BIGNUM)))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-ratnum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-ratnum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))

    (test2	+2/3	-1	+3/2)
    (test2	-2/3	-1	-3/2)

    (test2	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-1	(/ SMALLEST-POSITIVE-BIGNUM 7))
    (test2	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-1	(/ SMALLEST-NEGATIVE-BIGNUM 7))
    (test2	(/ SMALLEST-POSITIVE-BIGNUM 7)	-1	(/ 7 SMALLEST-POSITIVE-BIGNUM))
    (test2	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-1	(/ 7 SMALLEST-NEGATIVE-BIGNUM))

    ;; even exponent
    (test2	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-2	(square (/ SMALLEST-POSITIVE-BIGNUM 7)))
    (test2	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-2	(square (/ SMALLEST-NEGATIVE-BIGNUM 7)))
    (test2	(/ SMALLEST-POSITIVE-BIGNUM 7)	-2	(square (/ 7 SMALLEST-POSITIVE-BIGNUM)))
    (test2	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-2	(square (/ 7 SMALLEST-NEGATIVE-BIGNUM)))

    ;; odd exponent
    (test2	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-3	(cube (/ SMALLEST-POSITIVE-BIGNUM 7)))
    (test2	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-3	(cube (/ SMALLEST-NEGATIVE-BIGNUM 7)))
    (test2	(/ SMALLEST-POSITIVE-BIGNUM 7)	-3	(cube (/ 7 SMALLEST-POSITIVE-BIGNUM)))
    (test2	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-3	(cube (/ 7 SMALLEST-NEGATIVE-BIGNUM))))

;;; --------------------------------------------------------------------
;;; flonums

  (test		+0.0		-1	+inf.0)
  (test		-0.0		-1	-inf.0)
  (test		+1.0		-1	+1.0)
  (test		-1.0		-1	-1.0)
  (test		+nan.0		-1	+nan.0)
  (test		+inf.0		-1	+0.0)
  (test		-inf.0		-1	-0.0)
  (test		+123.456	-1	(/ +123.456))
  (test		-123.456	-1	(/ -123.456))

  ;; even exponent
  (test		+0.0		-2	+inf.0)
  (test		-0.0		-2	+inf.0)
  (test		+1.0		-2	+1.0)
  (test		-1.0		-2	+1.0)
  (test		+nan.0		-2	+nan.0)
  (test		+inf.0		-2	+0.0)
  (test		-inf.0		-2	+0.0)
  (test		+123.456	-2	(/ (square +123.456)))
  (test		-123.456	-2	(/ (square -123.456)))

  ;; odd exponent
  (test		+0.0		-3	+inf.0)
  (test		-0.0		-3	-inf.0)
  (test		+1.0		-3	+1.0)
  (test		-1.0		-3	-1.0)
  (test		+nan.0		-3	+nan.0)
  (test		+inf.0		-3	+0.0)
  (test		-inf.0		-3	-0.0)
  (test		+123.456	-3	(/ (cube +123.456)))
  (test		-123.456	-3	(/ (cube -123.456)))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-flonum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-flonum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))

    (test2		+0.0		-1	+inf.0)
    (test2		-0.0		-1	-inf.0)
    (test2		+1.0		-1	+1.0)
    (test2		-1.0		-1	-1.0)
    (test2		+nan.0		-1	+nan.0)
    (test2		+inf.0		-1	+0.0)
    (test2		-inf.0		-1	-0.0)
    (test2		+123.456	-1	(/ +123.456))
    (test2		-123.456	-1	(/ -123.456))

    ;; even exponent
    (test2		+0.0		-2	+inf.0)
    (test2		-0.0		-2	+inf.0)
    (test2		+1.0		-2	+1.0)
    (test2		-1.0		-2	+1.0)
    (test2		+nan.0		-2	+nan.0)
    (test2		+inf.0		-2	+0.0)
    (test2		-inf.0		-2	+0.0)
    (test2		+123.456	-2	(/ (square +123.456)))
    (test2		-123.456	-2	(/ (square -123.456)))

    ;; odd exponent
    (test2		+0.0		-3	+inf.0)
    (test2		-0.0		-3	-inf.0)
    (test2		+1.0		-3	+1.0)
    (test2		-1.0		-3	-1.0)
    (test2		+nan.0		-3	+nan.0)
    (test2		+inf.0		-3	+0.0)
    (test2		-inf.0		-3	-0.0)
    (test2		+123.456	-3	(/ (cube +123.456)))
    (test2		-123.456	-3	(/ (cube -123.456))))

;;; --------------------------------------------------------------------
;;; compnums

  (test		123+456i	-1	(/ 123+456i))
  (test		12.3+456i	-1	(/ 12.3+456i))
  (test		123+45.6i	-1	(/ 123+45.6i))
  (test		1/23+4/56i	-1	(/ 1/23+4/56i))

  (test		+nan.0+456i	-1	(/ +nan.0+456i)		inexact=?)
  (test		123+nan.0i	-1	(/ +123+nan.0i)		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	-1	+nan.0-0.0i		inexact=?)
  (test		123+inf.0i	-1	+0.0+nan.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	-1	+nan.0-0.0i		inexact=?)
  (test		123-inf.0i	-1	+0.0+nan.0i		inexact=?)

  (test (C 123 BN1)	-1	(/ (C 123 BN1)))
  (test (C BN1 456)	-1	(/ (C BN1 456)))
  (test (C 1.23 BN1)	-1	(/ (C 1.23 BN1)))
  (test (C BN1 4.56)	-1	(/ (C BN1 4.56)))
  (test (C 1/23 BN1)	-1	(/ (C 1/23 BN1)))
  (test (C BN1 4/56)	-1	(/ (C BN1 4/56)))

  (test (C 123 BN2)	-1	(/ (C 123 BN2)))
  (test (C BN2 456)	-1	(/ (C BN2 456)))
  (test (C 1.23 BN2)	-1	(/ (C 1.23 BN2)))
  (test (C BN2 4.56)	-1	(/ (C BN2 4.56)))
  (test (C 1/23 BN2)	-1	(/ (C 1/23 BN2)))
  (test (C BN2 4/56)	-1	(/ (C BN2 4/56)))

  (test (C 123 BN3)	-1	(/ (C 123 BN3)))
  (test (C BN3 456)	-1	(/ (C BN3 456)))
  (test (C 1.23 BN3)	-1	(/ (C 1.23 BN3)))
  (test (C BN3 4.56)	-1	(/ (C BN3 4.56)))
  (test (C 1/23 BN3)	-1	(/ (C 1/23 BN3)))
  (test (C BN3 4/56)	-1	(/ (C BN3 4/56)))

  (test (C 123 BN4)	-1	(/ (C 123 BN4)))
  (test (C BN4 456)	-1	(/ (C BN4 456)))
  (test (C 1.23 BN4)	-1	(/ (C 1.23 BN4)))
  (test (C BN4 4.56)	-1	(/ (C BN4 4.56)))
  (test (C 1/23 BN4)	-1	(/ (C 1/23 BN4)))
  (test (C BN4 4/56)	-1	(/ (C BN4 4/56)))

  ;; even exponent
  (test		123+456i	-2	(/ (square 123+456i)))
  (test		12.3+456i	-2	(/ (square 12.3+456.0i)))
  (test		123+45.6i	-2	(/ (square 123.0+45.6i)))
  (test		1/23+4/56i	-2	(/ (square 1/23+4/56i)))

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	-2	(/ +nan.0+nan.0i)	inexact=?)
  (test		123+nan.0i	-2	(/ +nan.0+nan.0i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	-2	(/ +inf.0+nan.0i)	inexact=?)
  (test		123+inf.0i	-2	(/ -inf.0+inf.0i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	-2	(/ +inf.0-inf.0i)	inexact=?)
  (test		123-inf.0i	-2	(/ -inf.0-inf.0i)	inexact=?)

  (test		(C 123 BN1)	-2	(/ (square (C 123 BN1))))
  (test		(C BN1 456)	-2	(/ (square (C BN1 456))))
  (test		(C 1.23 BN1)	-2	(/ (square (C 1.23 BN1))))
  (test		(C BN1 4.56)	-2	(/ (square (C BN1 4.56))))
  (test		(C 1/23 BN1)	-2	(/ (square (C 1/23 BN1))))
  (test		(C BN1 4/56)	-2	(/ (square (C BN1 4/56))))

  (test		(C 123 BN2)	-2	(/ (square (C 123 BN2))))
  (test		(C BN2 456)	-2	(/ (square (C BN2 456))))
  (test		(C 1.23 BN2)	-2	(/ (square (C 1.23 BN2))))
  (test		(C BN2 4.56)	-2	(/ (square (C BN2 4.56))))
  (test		(C 1/23 BN2)	-2	(/ (square (C 1/23 BN2))))
  (test		(C BN2 4/56)	-2	(/ (square (C BN2 4/56))))

  (test		(C 123 BN3)	-2	(/ (square (C 123 BN3))))
  (test		(C BN3 456)	-2	(/ (square (C BN3 456))))
  (test		(C 1.23 BN3)	-2	(/ (square (C 1.23 BN3))))
  (test		(C BN3 4.56)	-2	(/ (square (C BN3 4.56))))
  (test		(C 1/23 BN3)	-2	(/ (square (C 1/23 BN3))))
  (test		(C BN3 4/56)	-2	(/ (square (C BN3 4/56))))

  (test		(C 123 BN4)	-2	(/ (square (C 123 BN4))))
  (test		(C BN4 456)	-2	(/ (square (C BN4 456))))
  (test		(C 1.23 BN4)	-2	(/ (square (C 1.23 BN4))))
  (test		(C BN4 4.56)	-2	(/ (square (C BN4 4.56))))
  (test		(C 1/23 BN4)	-2	(/ (square (C 1/23 BN4))))
  (test		(C BN4 4/56)	-2	(/ (square (C BN4 4/56))))

  ;; even exponent
  (test		123+456i	-3	(/ (cube 123+456i)))
  (test		12.3+456i	-3	(/ (cube 12.3+456.0i))	inexact=?)
  (test		123+45.6i	-3	(/ (cube 123.0+45.6i))	inexact=?)
  (test		1/23+4/56i	-3	(/ (cube 1/23+4/56i)))

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	-3	+nan.0+nan.0i	inexact=?)
  (test		123+nan.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	-3	+nan.0+nan.0i	inexact=?)
  (test		123+inf.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	-3	+nan.0+nan.0i	inexact=?)
  (test		123-inf.0i	-3	-nan.0+nan.0i	inexact=?)

  (test		(C 123 BN1)	-3	(/ (cube (C 123 BN1))))
  (test		(C BN1 456)	-3	(/ (cube (C BN1 456))))
  (test		(C 1.23 BN1)	-3	(/ (cube (C 1.23 BN1)))	inexact=?)
  (test		(C BN1 4.56)	-3	(/ (cube (C BN1 4.56)))	inexact=?)
  (test		(C 1/23 BN1)	-3	(/ (cube (C 1/23 BN1))))
  (test		(C BN1 4/56)	-3	(/ (cube (C BN1 4/56))))

  (test		(C 123 BN2)	-3	(/ (cube (C 123 BN2))))
  (test		(C BN2 456)	-3	(/ (cube (C BN2 456))))
  (test		(C 1.23 BN2)	-3	(/ (cube (C 1.23 BN2)))	inexact=?)
  (test		(C BN2 4.56)	-3	(/ (cube (C BN2 4.56)))	inexact=?)
  (test		(C 1/23 BN2)	-3	(/ (cube (C 1/23 BN2))))
  (test		(C BN2 4/56)	-3	(/ (cube (C BN2 4/56))))

  (test		(C 123 BN3)	-3	(/ (cube (C 123 BN3))))
  (test		(C BN3 456)	-3	(/ (cube (C BN3 456))))
  (test		(C 1.23 BN3)	-3	(/ (cube (C 1.23 BN3)))	inexact=?)
  (test		(C BN3 4.56)	-3	(/ (cube (C BN3 4.56)))	inexact=?)
  (test		(C 1/23 BN3)	-3	(/ (cube (C 1/23 BN3))))
  (test		(C BN3 4/56)	-3	(/ (cube (C BN3 4/56))))

  (test		(C 123 BN4)	-3	(/ (cube (C 123 BN4))))
  (test		(C BN4 456)	-3	(/ (cube (C BN4 456))))
  (test		(C 1.23 BN4)	-3	(/ (cube (C 1.23 BN4)))	inexact=?)
  (test		(C BN4 4.56)	-3	(/ (cube (C BN4 4.56)))	inexact=?)
  (test		(C 1/23 BN4)	-3	(/ (cube (C 1/23 BN4))))
  (test		(C BN4 4/56)	-3	(/ (cube (C BN4 4/56))))

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-compnum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-compnum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))

    (test2	123+456i	-1	(/ 123+456i))
    (test2	12.3+456i	-1	(/ 12.3+456i))
    (test2	123+45.6i	-1	(/ 123+45.6i))
    (test2	1/23+4/56i	-1	(/ 1/23+4/56i))

    (test2	+nan.0+456i	-1	(/ +nan.0+456i)		inexact=?)
    (test2	123+nan.0i	-1	(/ +123+nan.0i)		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+456i	-1	+nan.0-0.0i		inexact=?)
    (test2	123+inf.0i	-1	+0.0+nan.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	-inf.0+456i	-1	+nan.0-0.0i		inexact=?)
    (test2	123-inf.0i	-1	+0.0+nan.0i		inexact=?)

    (test2 (C 123 BN1)	-1	(/ (C 123 BN1)))
    (test2 (C BN1 456)	-1	(/ (C BN1 456)))
    (test2 (C 1.23 BN1)	-1	(/ (C 1.23 BN1)))
    (test2 (C BN1 4.56)	-1	(/ (C BN1 4.56)))
    (test2 (C 1/23 BN1)	-1	(/ (C 1/23 BN1)))
    (test2 (C BN1 4/56)	-1	(/ (C BN1 4/56)))

    (test2 (C 123 BN2)	-1	(/ (C 123 BN2)))
    (test2 (C BN2 456)	-1	(/ (C BN2 456)))
    (test2 (C 1.23 BN2)	-1	(/ (C 1.23 BN2)))
    (test2 (C BN2 4.56)	-1	(/ (C BN2 4.56)))
    (test2 (C 1/23 BN2)	-1	(/ (C 1/23 BN2)))
    (test2 (C BN2 4/56)	-1	(/ (C BN2 4/56)))

    (test2 (C 123 BN3)	-1	(/ (C 123 BN3)))
    (test2 (C BN3 456)	-1	(/ (C BN3 456)))
    (test2 (C 1.23 BN3)	-1	(/ (C 1.23 BN3)))
    (test2 (C BN3 4.56)	-1	(/ (C BN3 4.56)))
    (test2 (C 1/23 BN3)	-1	(/ (C 1/23 BN3)))
    (test2 (C BN3 4/56)	-1	(/ (C BN3 4/56)))

    (test2 (C 123 BN4)	-1	(/ (C 123 BN4)))
    (test2 (C BN4 456)	-1	(/ (C BN4 456)))
    (test2 (C 1.23 BN4)	-1	(/ (C 1.23 BN4)))
    (test2 (C BN4 4.56)	-1	(/ (C BN4 4.56)))
    (test2 (C 1/23 BN4)	-1	(/ (C 1/23 BN4)))
    (test2 (C BN4 4/56)	-1	(/ (C BN4 4/56)))

    ;; even exponent
    (test2	123+456i	-2	(/ (square 123+456i)))
    (test2	12.3+456i	-2	(/ (square 12.3+456.0i)))
    (test2	123+45.6i	-2	(/ (square 123.0+45.6i)))
    (test2	1/23+4/56i	-2	(/ (square 1/23+4/56i)))

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+nan.0+456i	-2	(/ +nan.0+nan.0i)	inexact=?)
    (test2	123+nan.0i	-2	(/ +nan.0+nan.0i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+456i	-2	(/ +inf.0+nan.0i)	inexact=?)
    (test2	123+inf.0i	-2	(/ -inf.0+inf.0i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	-inf.0+456i	-2	(/ +inf.0-inf.0i)	inexact=?)
    (test2	123-inf.0i	-2	(/ -inf.0-inf.0i)	inexact=?)

    (test2	(C 123 BN1)	-2	(/ (square (C 123 BN1))))
    (test2	(C BN1 456)	-2	(/ (square (C BN1 456))))
    (test2	(C 1.23 BN1)	-2	(/ (square (C 1.23 BN1))))
    (test2	(C BN1 4.56)	-2	(/ (square (C BN1 4.56))))
    (test2	(C 1/23 BN1)	-2	(/ (square (C 1/23 BN1))))
    (test2	(C BN1 4/56)	-2	(/ (square (C BN1 4/56))))

    (test2	(C 123 BN2)	-2	(/ (square (C 123 BN2))))
    (test2	(C BN2 456)	-2	(/ (square (C BN2 456))))
    (test2	(C 1.23 BN2)	-2	(/ (square (C 1.23 BN2))))
    (test2	(C BN2 4.56)	-2	(/ (square (C BN2 4.56))))
    (test2	(C 1/23 BN2)	-2	(/ (square (C 1/23 BN2))))
    (test2	(C BN2 4/56)	-2	(/ (square (C BN2 4/56))))

    (test2	(C 123 BN3)	-2	(/ (square (C 123 BN3))))
    (test2	(C BN3 456)	-2	(/ (square (C BN3 456))))
    (test2	(C 1.23 BN3)	-2	(/ (square (C 1.23 BN3))))
    (test2	(C BN3 4.56)	-2	(/ (square (C BN3 4.56))))
    (test2	(C 1/23 BN3)	-2	(/ (square (C 1/23 BN3))))
    (test2	(C BN3 4/56)	-2	(/ (square (C BN3 4/56))))

    (test2	(C 123 BN4)	-2	(/ (square (C 123 BN4))))
    (test2	(C BN4 456)	-2	(/ (square (C BN4 456))))
    (test2	(C 1.23 BN4)	-2	(/ (square (C 1.23 BN4))))
    (test2	(C BN4 4.56)	-2	(/ (square (C BN4 4.56))))
    (test2	(C 1/23 BN4)	-2	(/ (square (C 1/23 BN4))))
    (test2	(C BN4 4/56)	-2	(/ (square (C BN4 4/56))))

    ;; even exponent
    (test2	123+456i	-3	(/ (cube 123+456i)))
    (test2	12.3+456i	-3	(/ (cube 12.3+456.0i))	inexact=?)
    (test2	123+45.6i	-3	(/ (cube 123.0+45.6i))	inexact=?)
    (test2	1/23+4/56i	-3	(/ (cube 1/23+4/56i)))

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+nan.0+456i	-3	+nan.0+nan.0i	inexact=?)
    (test2	123+nan.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	+inf.0+456i	-3	+nan.0+nan.0i	inexact=?)
    (test2	123+inf.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2	-inf.0+456i	-3	+nan.0+nan.0i	inexact=?)
    (test2	123-inf.0i	-3	-nan.0+nan.0i	inexact=?)

    (test2	(C 123 BN1)	-3	(/ (cube (C 123 BN1))))
    (test2	(C BN1 456)	-3	(/ (cube (C BN1 456))))
    (test2	(C 1.23 BN1)	-3	(/ (cube (C 1.23 BN1)))	inexact=?)
    (test2	(C BN1 4.56)	-3	(/ (cube (C BN1 4.56)))	inexact=?)
    (test2	(C 1/23 BN1)	-3	(/ (cube (C 1/23 BN1))))
    (test2	(C BN1 4/56)	-3	(/ (cube (C BN1 4/56))))

    (test2	(C 123 BN2)	-3	(/ (cube (C 123 BN2))))
    (test2	(C BN2 456)	-3	(/ (cube (C BN2 456))))
    (test2	(C 1.23 BN2)	-3	(/ (cube (C 1.23 BN2)))	inexact=?)
    (test2	(C BN2 4.56)	-3	(/ (cube (C BN2 4.56)))	inexact=?)
    (test2	(C 1/23 BN2)	-3	(/ (cube (C 1/23 BN2))))
    (test2	(C BN2 4/56)	-3	(/ (cube (C BN2 4/56))))

    (test2	(C 123 BN3)	-3	(/ (cube (C 123 BN3))))
    (test2	(C BN3 456)	-3	(/ (cube (C BN3 456))))
    (test2	(C 1.23 BN3)	-3	(/ (cube (C 1.23 BN3)))	inexact=?)
    (test2	(C BN3 4.56)	-3	(/ (cube (C BN3 4.56)))	inexact=?)
    (test2	(C 1/23 BN3)	-3	(/ (cube (C 1/23 BN3))))
    (test2	(C BN3 4/56)	-3	(/ (cube (C BN3 4/56))))

    (test2	(C 123 BN4)	-3	(/ (cube (C 123 BN4))))
    (test2	(C BN4 456)	-3	(/ (cube (C BN4 456))))
    (test2	(C 1.23 BN4)	-3	(/ (cube (C 1.23 BN4)))	inexact=?)
    (test2	(C BN4 4.56)	-3	(/ (cube (C BN4 4.56)))	inexact=?)
    (test2	(C 1/23 BN4)	-3	(/ (cube (C 1/23 BN4))))
    (test2	(C BN4 4/56)	-3	(/ (cube (C BN4 4/56)))))

;;; --------------------------------------------------------------------
;;; cflonums

  (test		12.3+45.6i	-1	(/ 12.3+45.6i)	inexact=?)
  (test		12.3+0.0i	-1	(/ 12.3+0.0i)	inexact=?)
  (test		0.0+45.6i	-1	(/ 0.0+45.6i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+45.6i	-1	+nan.0-0.0i	inexact=?)
  (test		12.3+inf.0i	-1	+0.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+45.6i	-1	+nan.0+nan.0i	inexact=?)
  (test		12.3+nan.0i	-1	+nan.0+nan.0i	inexact=?)
  (test		+nan.0+nan.0i	-1	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+inf.0i	-1	+nan.0+nan.0i	inexact=?)
  (test		+inf.0-inf.0i	-1	+nan.0-nan.0i	inexact=?)
  (test		-inf.0+inf.0i	-1	+nan.0+nan.0i	inexact=?)
  (test		-inf.0-inf.0i	-1	+nan.0+nan.0i	inexact=?)

  ;; even exponent
  (test		12.3+45.6i	-2	(/ (square 12.3+45.6i))	inexact=?)
  (test		12.3+0.0i	-2	(/ (square 12.3+0.0i))	inexact=?)
  (test		0.0+45.6i	-2	(/ (square 0.0+45.6i))	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+45.6i	-2	+nan.0+nan.0i	inexact=?)
  (test		12.3+inf.0i	-2	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+45.6i	-2	+nan.0+nan.0i	inexact=?)
  (test		12.3+nan.0i	-2	+nan.0+nan.0i	inexact=?)
  (test		+nan.0+nan.0i	-2	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+inf.0i	-2	+nan.0+nan.0i	inexact=?)
  (test		+inf.0-inf.0i	-2	+nan.0+nan.0i	inexact=?)
  (test		-inf.0+inf.0i	-2	+nan.0+nan.0i	inexact=?)
  (test		-inf.0-inf.0i	-2	+nan.0+nan.0i	inexact=?)

  ;; odd exponent
  (test		12.3+45.6i	-3	(/ (cube 12.3+45.6i))	inexact=?)
  (test		12.3+0.0i	-3	(/ (cube 12.3+0.0i))	inexact=?)
  (test		0.0+45.6i	-3	(/ (cube 0.0+45.6i))	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+45.6i	-3	+nan.0+nan.0i	inexact=?)
  (test		12.3+inf.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+45.6i	-3	+nan.0+nan.0i	inexact=?)
  (test		12.3+nan.0i	-3	+nan.0+nan.0i	inexact=?)
  (test		+nan.0+nan.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+inf.0i	-3	+nan.0+nan.0i	inexact=?)
  (test		+inf.0-inf.0i	-3	+nan.0+nan.0i	inexact=?)
  (test		-inf.0+inf.0i	-3	+nan.0+nan.0i	inexact=?)
  (test		-inf.0-inf.0i	-3	+nan.0+nan.0i	inexact=?)

;;; --------------------------------------------------------------------

  (let-syntax
      ((test2 (syntax-rules ()
		((_ ?op1 ?op2 ?expected)
		 (check ($expt-cflonum-fixnum	?op1 ?op2) => ?expected))
		((_ ?op1 ?op2 ?expected ?equal)
		 (check ($expt-cflonum-fixnum	?op1 ?op2) (=> ?equal) ?expected)))))

    (test2		12.3+45.6i	-1	(/ 12.3+45.6i)	inexact=?)
    (test2		12.3+0.0i	-1	(/ 12.3+0.0i)	inexact=?)
    (test2		0.0+45.6i	-1	(/ 0.0+45.6i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+inf.0+45.6i	-1	+nan.0-0.0i	inexact=?)
    (test2		12.3+inf.0i	-1	+0.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+nan.0+45.6i	-1	+nan.0+nan.0i	inexact=?)
    (test2		12.3+nan.0i	-1	+nan.0+nan.0i	inexact=?)
    (test2		+nan.0+nan.0i	-1	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+inf.0+inf.0i	-1	+nan.0+nan.0i	inexact=?)
    (test2		+inf.0-inf.0i	-1	+nan.0-nan.0i	inexact=?)
    (test2		-inf.0+inf.0i	-1	+nan.0+nan.0i	inexact=?)
    (test2		-inf.0-inf.0i	-1	+nan.0+nan.0i	inexact=?)

    ;; even exponent
    (test2		12.3+45.6i	-2	(/ (square 12.3+45.6i))	inexact=?)
    (test2		12.3+0.0i	-2	(/ (square 12.3+0.0i))	inexact=?)
    (test2		0.0+45.6i	-2	(/ (square 0.0+45.6i))	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+inf.0+45.6i	-2	+nan.0+nan.0i	inexact=?)
    (test2		12.3+inf.0i	-2	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+nan.0+45.6i	-2	+nan.0+nan.0i	inexact=?)
    (test2		12.3+nan.0i	-2	+nan.0+nan.0i	inexact=?)
    (test2		+nan.0+nan.0i	-2	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+inf.0+inf.0i	-2	+nan.0+nan.0i	inexact=?)
    (test2		+inf.0-inf.0i	-2	+nan.0+nan.0i	inexact=?)
    (test2		-inf.0+inf.0i	-2	+nan.0+nan.0i	inexact=?)
    (test2		-inf.0-inf.0i	-2	+nan.0+nan.0i	inexact=?)

    ;; odd exponent
    (test2		12.3+45.6i	-3	(/ (cube 12.3+45.6i))	inexact=?)
    (test2		12.3+0.0i	-3	(/ (cube 12.3+0.0i))	inexact=?)
    (test2		0.0+45.6i	-3	(/ (cube 0.0+45.6i))	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+inf.0+45.6i	-3	+nan.0+nan.0i	inexact=?)
    (test2		12.3+inf.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+nan.0+45.6i	-3	+nan.0+nan.0i	inexact=?)
    (test2		12.3+nan.0i	-3	+nan.0+nan.0i	inexact=?)
    (test2		+nan.0+nan.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
    (test2		+inf.0+inf.0i	-3	+nan.0+nan.0i	inexact=?)
    (test2		+inf.0-inf.0i	-3	+nan.0+nan.0i	inexact=?)
    (test2		-inf.0+inf.0i	-3	+nan.0+nan.0i	inexact=?)
    (test2		-inf.0-inf.0i	-3	+nan.0+nan.0i	inexact=?))

  #t)


;;;; done

(check-report)

;;; end of file
