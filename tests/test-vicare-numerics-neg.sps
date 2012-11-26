;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: neg
;;;Date: Mon Nov 26, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rnrs)
  (ikasus system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare numerics functions: neg, unary minus\n")


(parametrise ((check-test-name	'neg))

  (define-syntax make-test
    (syntax-rules ()
      ((_ ?op ?unsafe-op)
       (syntax-rules ()
	 ((_ ?op ?expected-result)
	  (begin
	    (check
		(?op ?op)
	      => ?expected-result)
	    (check
		(?unsafe-op ?op)
	      => ?expected-result)
	    (check
		(?op ?op)
	      => (?unsafe-op ?op))
	    ))))))

  (define GREATEST-FX		+536870911)
  (define LEAST-FX		-536870912)

  (define BN1			+536870912) ;GREATEST-FX + 1
  (define BN2			+536871011) ;GREATEST-FX + 100
  (define BN3			-536870913) ;LEAST-FX - 1
  (define BN4			-536871012) ;LEAST-FX - 100

  (define NEG-BN1		-536870912)
  (define NEG-BN2		-536871011)
  (define NEG-BN3		+536870913)
  (define NEG-BN4		+536871012)

;;; --------------------------------------------------------------------

  (let-syntax ((test (make-test - $neg-fixnum)))

    (test 0 0)
    (test +1 +1)
    (test -1 -1)
    (test (greatest-fixnum)	(- (greatest-fixnum)))
    (test (least-fixnum)	(- (least-fixnum)))

    #f)

;;; --------------------------------------------------------------------

  (let-syntax ((test (make-test - $neg-bignum)))

    (test BN1 NEG-BN1)
    (test BN2 NEG-BN2)
    (test BN3 NEG-BN3)
    (test BN4 NEG-BN4)

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
