;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: sign
;;;Date: Fri Nov 30, 2012
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
  (ikarus system $flonums)
  (ikarus system $ratnums)
  (ikarus system $compnums)
  (ikarus system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: sign\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test-1 sign $sign-fixnum))

  (test 0 0)
  (test 1 1)
  (test -1 -1)
  (test FX1 +1)
  (test FX2 -1)
  (test FX3 +1)
  (test FX4 -1)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test-1 sign #;$sign-bignum)))
    (test BN1 +1)
    (test BN2 +1)
    (test BN3 -1)
    (test BN4 -1)
    #f)

  (let-syntax ((test (make-test-1 sign $sign-bignum)))
    (test VBN1 +1)
    (test VBN2 +1)
    (test VBN3 -1)
    (test VBN4 -1)
    #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (make-test-1 sign $sign-ratnum))

  (test 1/2 +1)
  (test -1/2 -1)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-test-1 sign $sign-flonum))

  (test FL1 +1.0)
  (test FL2 -1.0)
  (test FL3 +1.0)
  (test FL4 -1.0)
  (test FL5 +1.0)
  (test FL6 -1.0)
  (test FL7 +nan.0)

  #t)


;;;; done

(check-report)

;;; end of file
