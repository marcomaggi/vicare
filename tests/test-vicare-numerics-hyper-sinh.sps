;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: sinh
;;;Date: Fri Dec 14, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (libtest numerics-helpers)
  (vicare system $numerics)
  (vicare checks)
  (only (vicare platform words)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: hyperbolic sine\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sinh ?op)		(=> inexact=?) ?expected)
	 (check ($sinh-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	+1.1752011936438014)
  (test -1	-1.1752011936438014)
  (test +2	+3.6268604078470186)
  (test -2	-3.6268604078470186)

  (case-word-size
   ((32)
    (test (greatest-fixnum) +inf.0)
    (test (least-fixnum) -inf.0))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sinh ?op)		(=> inexact=?) ?expected)
	 (check ($sinh-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 +inf.0)
    (test BN2 +inf.0)
    (test BN3 -inf.0)
    (test BN4 -inf.0))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sinh ?op)		(=> inexact=?) ?expected)
	 (check ($sinh-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.5210953054937474)
  (test -1/2 -0.5210953054937474)

  (test RN01 0.008130170865095402)
  (test RN02 -0.008130170865095402)
  (test RN03 -0.008130170865095402)
  (test RN04 -inf.0)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 1.1752011907695898)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 1.175200906222709)
  (test RN12 -1.1752009090969195)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -1.175201187895378)
  (test RN16 1.1752011907695898)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -1.1752009033484987)
  (test RN20 1.1752009062227093)
  (test RN29 1.1752011965180131)
  (test RN30 1.1752014810649885)
  (test RN31 -1.175201199392225)
  (test RN32 -1.1752014839392004)
  (test RN34 -1.1752014781907758)
  (test RN35 1.1752011965180131)
  (test RN36 1.1752014810649878)

  (case-word-size
   ((32)
    (test (/ BN1 123) +inf.0)
    (test (/ BN2 123) +inf.0)
    (test (/ BN3 123) -inf.0)
    (test (/ BN4 123) -inf.0)
    (test (/ 123 BN1) 2.2910535292866775e-7)
    (test (/ 123 BN2) 2.2910534908799006e-7)
    (test (/ 123 BN3) -2.2910535250192578e-7)
    (test (/ 123 BN4) -2.291053486612481e-7))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sinh ?op)		(=> inexact=?) ?expected)
	 (check ($sinh-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	+0.0)
  (test -0.0	-0.0)

  (test +1.0	+1.1752011936438014)
  (test -1.0	-1.1752011936438014)

  (test +2.0	+3.6268604078470186)
  (test -2.0	-3.6268604078470186)

  (test +inf.0	+inf.0)
  (test -inf.0	-inf.0)

  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sinh ?op)		(=> inexact=?) ?expected)
	 (check ($sinh-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		1.1752011936438014+0.0i)
  (test -1+0.0i		-1.1752011936438014+0.0i)
  (test +1-0.0i		1.1752011936438014-0.0i)
  (test -1-0.0i		-1.1752011936438014-0.0i)

  (test +0.0+1i		0.0+0.8414709848078965i)
  (test +0.0-1i		0.0-0.8414709848078965i)
  (test -0.0+1i		-0.0+0.8414709848078965i)
  (test -0.0-1i		-0.0-0.8414709848078965i)

  (test +1+1.0i		0.6349639147847361+1.2984575814159773i)
  (test +1-1.0i		0.6349639147847361-1.2984575814159773i)
  (test -1.0+1i		-0.6349639147847361+1.2984575814159773i)
  (test -1.0-1i		-0.6349639147847361-1.2984575814159773i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i		-1.1634403637032504+0.21775955162215221i)
  (test 1-3i		-1.1634403637032504-0.21775955162215221i)

  (test -1+3i		1.1634403637032504+0.21775955162215221i)
  (test -1-3i		1.1634403637032504-0.21775955162215221i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sinh ?op)		(=> inexact=?) ?expected)
	 (check ($sinh-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	+0.0+0.0i)
  (test -0.0+0.0i	-0.0+0.0i)
  (test +0.0-0.0i	+0.0-0.0i)
  (test -0.0-0.0i	-0.0-0.0i)

  (test +1.0+0.0i	+1.1752011936438014+0.0i)
  (test -1.0+0.0i	-1.1752011936438014+0.0i)
  (test +1.0-0.0i	+1.1752011936438014-0.0i)
  (test -1.0-0.0i	-1.1752011936438014-0.0i)

  (test +0.0+1.0i	+0.0+0.8414709848078965i)
  (test +0.0-1.0i	+0.0-0.8414709848078965i)
  (test -0.0+1.0i	-0.0+0.8414709848078965i)
  (test -0.0-1.0i	-0.0-0.8414709848078965i)

  (test +1.0+1.0i	+0.6349639147847361+1.2984575814159773i)
  (test +1.0-1.0i	+0.6349639147847361-1.2984575814159773i)
  (test -1.0+1.0i	-0.6349639147847361+1.2984575814159773i)
  (test -1.0-1.0i	-0.6349639147847361-1.2984575814159773i)

  (test +inf.0+0.0i	+inf.0+0.0i)
  (test -inf.0+0.0i	-inf.0+0.0i)
  (test +inf.0-0.0i	+inf.0-0.0i)
  (test -inf.0-0.0i	-inf.0-0.0i)

  (test +0.0+inf.0i	+nan.0+nan.0i)
  (test +0.0-inf.0i	+nan.0+nan.0i)
  (test -0.0+inf.0i	+nan.0+nan.0i)
  (test -0.0-inf.0i	+nan.0+nan.0i)

  (test +inf.0+inf.0i	+nan.0+nan.0i)
  (test +inf.0-inf.0i	+nan.0+nan.0i)
  (test -inf.0+inf.0i	+nan.0+nan.0i)
  (test -inf.0-inf.0i	+nan.0+nan.0i)

  (test 1.0+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1.0i	+nan.0+nan.0i)
  (test +nan.0+nan.0i	+nan.0+nan.0i)

  (test +1.2+3.4i	-1.4593445101810318-0.46269691906508803i)
  (test +1.2-3.4i	-1.4593445101810318+0.46269691906508803i)
  (test -1.2+3.4i	+1.4593445101810318-0.46269691906508803i)
  (test -1.2-3.4i	+1.4593445101810318+0.46269691906508803i)

  (test +1.0+3.0i	-1.1634403637032504+0.21775955162215221i)
  (test +1.0-3.0i	-1.1634403637032504-0.21775955162215221i)
  (test -1.0+3.0i	+1.1634403637032504+0.21775955162215221i)
  (test -1.0-3.0i	+1.1634403637032504-0.21775955162215221i)

  #t)


;;;; done

(check-report)

;;; end of file
