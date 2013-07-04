;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: exp
;;;Date: Mon Dec 10, 2012
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
  (only (vicare platform words)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: exp\n")


(parametrise ((check-test-name	'fixnums))

  (let-syntax ((test (make-inexact-test-1 exp $exp-fixnum)))
    (test 0			1)
    (test +1			2.718281828459045)
    (test -1			0.36787944117144233)
    (test +2			7.38905609893065)
    (test -2			0.1353352832366127)

    (test FX1 2.718281828459045)
    (test FX2 0.36787944117144233)
    (test FX3 +inf.0)
    (test FX4 0.0)
    #f)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-inexact-test-1 exp)))
    (test BN1			+inf.0)
    (test BN2			+inf.0)
    (test BN3			0.0)
    (test BN4			0.0)
    #f)

  (let-syntax ((test (make-inexact-test-1 exp $exp-bignum)))
    (test VBN1			+inf.0)
    (test VBN2			+inf.0)
    (test VBN3			0.0)
    (test VBN4			0.0)
    #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (let-syntax ((test (make-inexact-test-1 exp)))
    (test +1/2			1.6487212707001282)
    (test -1/2			0.6065306597126334)

    (test VRN01 1.0081632201581154)
    (test VRN02 0.9919028784279246)
    (test VRN03 0.9919028784279246)
    (test VRN04 0.0)

    (test (/ BN1 123)		+inf.0)
    (test (/ BN2 123)		+inf.0)
    (test (/ BN3 123)		+0.0)
    (test (/ BN4 123)		+0.0)

    (test (/ 123 BN1)		1.0000002291053791)
    (test (/ 123 BN2)		1.0000002291053753)
    (test (/ 123 BN3)		0.9999997708946737)
    (test (/ 123 BN4)		0.9999997708946776)
    #f)

  (let-syntax ((test (make-inexact-test-1 exp)))
    (test VRN01 1.0081632201581154)
    (test VRN02 0.9919028784279246)
    (test VRN03 0.9919028784279246)
    (test VRN04 0.0)
    (test (/ VBN1 123) +inf.0)
    (test (/ VBN2 123) +inf.0)
    (test (/ VBN3 123) 0.0)
    (test (/ VBN4 123) 0.0)
    (test (/ 123 VBN1) 1.0)
    (test (/ 123 VBN2) 1.0)
    (test (/ 123 VBN3) 0.9999999999999999)
    (test (/ 123 VBN4) 0.9999999999999999)
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0			1.0)
  (test -0.0			1.0)
  (test +1.0			2.718281828459045)
  (test -1.0			0.36787944117144233)
  (test +2.0			7.38905609893065)
  (test -2.0			0.1353352832366127)

  (test +inf.0			+inf.0)
  (test -inf.0			0.0)
  (test +nan.0			+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		2.718281828459045+0.0i)
  (test -1+0.0i		0.36787944117144233+0.0i)
  (test +1-0.0i		2.718281828459045+0.0i)
  (test -1-0.0i		0.36787944117144233+0.0i)
  (test +0.0+1i		0.5403023058681398+0.8414709848078965i)
  (test +0.0-1i		0.5403023058681398-0.8414709848078965i)
  (test -0.0+1i		0.5403023058681398+0.8414709848078965i)
  (test -0.0-1i		0.5403023058681398-0.8414709848078965i)

  (test +1.0+1.0i	1.4686939399158851+2.2873552871788423i)
  (test +1.0-1.0i	1.4686939399158851-2.2873552871788423i)
  (test -1.0+1.0i	0.19876611034641298+0.3095598756531122i)
  (test -1.0-1.0i	0.19876611034641298-0.3095598756531122i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i -2.6910786138197937+0.383603953541131i)
  (test 1-3i -2.6910786138197937-0.383603953541131i)
  (test -1+3i -0.36419788641329287+0.05191514970317339i)
  (test -1-3i -0.36419788641329287-0.05191514970317339i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i		1.0+0.0i)
  (test -0.0+0.0i		1.0+0.0i)
  (test +0.0-0.0i		1.0-0.0i)
  (test -0.0-0.0i		1.0-0.0i)

  (test +1.0+0.0i		2.718281828459045+0.0i)
  (test -1.0+0.0i		0.36787944117144233+0.0i)
  (test +1.0-0.0i		2.718281828459045-0.0i)
  (test -1.0-0.0i		0.36787944117144233-0.0i)

  (test +0.0+1.0i		0.5403023058681398+0.8414709848078965i)
  (test +0.0-1.0i		0.5403023058681398-0.8414709848078965i)
  (test -0.0+1.0i		0.5403023058681398+0.8414709848078965i)
  (test -0.0-1.0i		0.5403023058681398-0.8414709848078965i)

  (test +1.0+1.0i		1.4686939399158851+2.2873552871788423i)
  (test +1.0-1.0i		1.4686939399158851-2.2873552871788423i)
  (test -1.0+1.0i		0.19876611034641298+0.3095598756531122i)
  (test -1.0-1.0i		0.19876611034641298-0.3095598756531122i)

  (test +inf.0+0.0i		+inf.0+nan.0i)
  (test -inf.0+0.0i		0.0+0.0i)
  (test +inf.0-0.0i		+inf.0+nan.0i)
  (test -inf.0-0.0i		0.0-0.0i)

  (test +0.0+inf.0i		+nan.0+nan.0i)
  (test +0.0-inf.0i		+nan.0+nan.0i)
  (test -0.0+inf.0i		+nan.0+nan.0i)
  (test -0.0-inf.0i		+nan.0+nan.0i)

  (test +inf.0+inf.0i		+nan.0+nan.0i)
  (test +inf.0-inf.0i		+nan.0+nan.0i)
  (test -inf.0+inf.0i		+nan.0+nan.0i)
  (test -inf.0-inf.0i		+nan.0+nan.0i)

  (test 1.0+nan.0i		+nan.0+nan.0i)
  (test +nan.0+1.0i		+nan.0+nan.0i)
  (test +nan.0+nan.0i		+nan.0+nan.0i)

  (test +1.2+3.4i		-3.209883040054176-0.8484263372940289i)
  (test +1.2-3.4i		-3.209883040054176+0.8484263372940289i)
  (test -1.2+3.4i		-0.2911940196921122-0.07696750083614708i)
  (test -1.2-3.4i		-0.2911940196921122+0.07696750083614708i)

  (test +1.0+3.0i		-2.6910786138197937+0.383603953541131i)
  (test +1.0-3.0i		-2.6910786138197937-0.383603953541131i)
  (test -1.0+3.0i		-0.36419788641329287+0.05191514970317339i)
  (test -1.0-3.0i		-0.36419788641329287-0.05191514970317339i)

  #t)


;;;; done

(check-report)

;;; end of file
