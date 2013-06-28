;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: log
;;;Date: Sun Dec  9, 2012
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
  (ikarus system $numerics)
  (vicare checks)
  (only (vicare language-extensions syntaxes)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: log\n")


(parametrise ((check-test-name	'fixnums))

  (let-syntax ((test (make-inexact-test-1 log $log-fixnum)))
    (test +1			0)
    (test -1			0.0+3.141592653589793i)

    (test FX1 0)
    (test FX2 0.0+3.141592653589793i)
    (test FX3 20.10126823437577)
    (test FX4 20.101268236238415+3.141592653589793i)

    #f)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-inexact-test-1 log)))
    (test BN1 20.101268236238415)
    (test BN2 20.101268420640267)
    (test BN3 20.10126823810106+3.141592653589793i)
    (test BN4 20.101268422502912+3.141592653589793i)
    #f)

  (let-syntax ((test (make-inexact-test-1 log $log-bignum)))
    (test VBN1 41.58883083359672)
    (test VBN2 41.58883083359672)
    (test VBN3 41.58883083359672+3.141592653589793i)
    (test VBN4 41.58883083359672+3.141592653589793i)

    #f)

  #t)


(parametrise ((check-test-name	'ratnums))


(let-syntax ((test (make-inexact-test-1 log)))
    (test +1/2			-0.6931471805599453)
    (test -1/2			-0.6931471805599453+3.141592653589793i)

    (test (/ BN1 123)		15.289083882728642)
    (test (/ BN2 123)		15.289083899492448)
    (test (/ BN3 123)		15.289083884591287+3.141592653589793i)
    (test (/ BN4 123)		15.289083901355093+3.141592653589793i)

    (test (/ 123 BN1)		-15.289083882728642)
    (test (/ 123 BN2)		-15.289083899492448)
    (test (/ 123 BN3)		-15.289083884591287+3.141592653589793i)
    (test (/ 123 BN4)		-15.289083901355093+3.141592653589793i)
    #f)

  (let-syntax ((test (make-inexact-test-1 log $log-bignum)))
    (test (/ VBN1 123) 36.7766464782243)
    (test (/ VBN2 123) 36.7766464782243)
    (test (/ VBN3 123) 36.7766464782243+3.141592653589793i)
    (test (/ VBN4 123) 36.7766464782243+3.141592653589793i)
    (test (/ 123 VBN1) -36.7766464782243)
    (test (/ 123 VBN2) -36.7766464782243)
    (test (/ 123 VBN3) -36.7766464782243+3.141592653589793i)
    (test (/ 123 VBN4) -36.7766464782243+3.141592653589793i)
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0			-inf.0)
  (test -0.0			-inf.0+3.141592653589793i)
  (test +1.0			0.0)
  (test -1.0			0.0+3.141592653589793i)

  (test +inf.0			+inf.0)
  (test -inf.0			+inf.0+3.141592653589793i)
  (test +nan.0			+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		0.0+0.0i)
  (test -1+0.0i		0.0+3.141592653589793i)
  (test +1-0.0i		0.0-0.0i)
  (test -1-0.0i		0.0-3.141592653589793i)
  (test +0.0+1i		0.0+1.5707963267948966i)
  (test +0.0-1i		0.0-1.5707963267948966i)
  (test -0.0+1i		0.0+1.5707963267948966i)
  (test -0.0-1i		0.0-1.5707963267948966i)

  (test +1.0+1i		0.34657359027997264+0.7853981633974483i)
  (test +1-1.0i		0.34657359027997264-0.7853981633974483i)
  (test -1.0+1i		0.34657359027997264+2.356194490192345i)
  (test -1-1.0i		0.34657359027997264-2.356194490192345i)

  (test +0+inf.0i	+inf.0+1.5707963267948966i)
  (test +0-inf.0i	+inf.0-1.5707963267948966i)
  (test -0+inf.0i	+inf.0+1.5707963267948966i)
  (test -0-inf.0i	+inf.0-1.5707963267948966i)

  (test +1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test +1+3i		1.151292546497023+1.2490457723982544i)
  (test +1-3i		1.151292546497023-1.2490457723982544i)
  (test -1+3i		1.151292546497023+1.892546881191539i)
  (test -1-3i		1.151292546497023-1.892546881191539i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i		-inf.0+0.0i)
  (test -0.0+0.0i		-inf.0+3.141592653589793i)
  (test +0.0-0.0i		-inf.0-0.0i)
  (test -0.0-0.0i		-inf.0-3.141592653589793i)

  (test +1.0+0.0i		0.0+0.0i)
  (test -1.0+0.0i		0.0+3.141592653589793i)
  (test +1.0-0.0i		0.0-0.0i)
  (test -1.0-0.0i		0.0-3.141592653589793i)
  (test +0.0+1.0i		0.0+1.5707963267948966i)
  (test +0.0-1.0i		0.0-1.5707963267948966i)
  (test -0.0+1.0i		0.0+1.5707963267948966i)
  (test -0.0-1.0i		0.0-1.5707963267948966i)

  (test +1.0+1.0i		0.34657359027997264+0.7853981633974483i)
  (test +1.0-1.0i		0.34657359027997264-0.7853981633974483i)
  (test -1.0+1.0i		0.34657359027997264+2.356194490192345i)
  (test -1.0-1.0i		0.34657359027997264-2.356194490192345i)

  (test +inf.0+0.0i		+inf.0+0.0i)
  (test -inf.0+0.0i		+inf.0+3.141592653589793i)
  (test +inf.0-0.0i		+inf.0-0.0i)
  (test -inf.0-0.0i		+inf.0-3.141592653589793i)

  (test +0.0+inf.0i		+inf.0+1.5707963267948966i)
  (test +0.0-inf.0i		+inf.0-1.5707963267948966i)
  (test -0.0+inf.0i		+inf.0+1.5707963267948966i)
  (test -0.0-inf.0i		+inf.0-1.5707963267948966i)

  (test +inf.0+inf.0i		+inf.0+0.7853981633974483i)
  (test +inf.0-inf.0i		+inf.0-0.7853981633974483i)
  (test -inf.0+inf.0i		+inf.0+2.356194490192345i)
  (test -inf.0-inf.0i		+inf.0-2.356194490192345i)

  (test +1.0+nan.0i		+nan.0+nan.0i)
  (test +nan.0+1.0i		+nan.0+nan.0i)
  (test +nan.0+nan.0i		+nan.0+nan.0i)

  (test +1.2+3.4i		1.2824746787307684+1.2315037123408519i)
  (test +1.2-3.4i		1.2824746787307684-1.2315037123408519i)
  (test -1.2+3.4i		1.2824746787307684+1.9100889412489412i)
  (test -1.2-3.4i		1.2824746787307684-1.9100889412489412i)

  (test +1.0+3.0i		1.151292546497023+1.2490457723982544i)
  (test +1.0-3.0i		1.151292546497023-1.2490457723982544i)
  (test -1.0+3.0i		1.151292546497023+1.892546881191539i)
  (test -1.0-3.0i		1.151292546497023-1.892546881191539i)

  #t)


;;;; done

(check-report)

;;; end of file
