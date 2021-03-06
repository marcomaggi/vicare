;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: lcm
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
  (libtest numerics-helpers)
  (vicare system $ratnums)
  (vicare system $compnums)
  (vicare system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: lcm, least common multiple\n")



;; Only integer flonums for this operation.
(define IFL1		+0.0)
(define IFL2		-0.0)
(define IFL3		+1.0)
(define IFL4		-1.0)
(define IFL5		+2.0)
(define IFL6		-2.0)



(parametrise ((check-test-name	'fixnums))

  (check (lcm 0)		=> 0)
  (check (lcm +123)		=> +123)
  (check (lcm -123)		=> +123)

  (let-syntax ((test (make-test lcm $lcm-fixnum-fixnum)))
    (test 0 0 0)
    (test 1 0 0)
    (test -1 0 0)
    (test 0 1 0)
    (test 0 -1 0)
    (test FX1 FX1 1)
    (test FX2 FX1 1)
    (test FX3 FX1 536870911)
    (test FX4 FX1 536870912)
    (test FX1 FX2 1)
    (test FX2 FX2 1)
    (test FX3 FX2 536870911)
    (test FX4 FX2 536870912)
    (test FX1 FX3 536870911)
    (test FX2 FX3 536870911)
    (test FX3 FX3 536870911)
    (test FX4 FX3 288230375614840832)
    (test FX1 FX4 536870912)
    (test FX2 FX4 536870912)
    (test FX3 FX4 288230375614840832)
    (test FX4 FX4 536870912)
    #f)

  (let-syntax ((test (make-test lcm #;$lcm-fixnum-bignum)))
    (test 0 BN1 0)
    (test FX1 BN1 536870912)
    (test FX2 BN1 536870912)
    (test FX3 BN1 288230375614840832)
    (test FX4 BN1 536870912)
    (test FX1 BN2 536871011)
    (test FX2 BN2 536871011)
    (test FX3 BN2 288230428765061021)
    (test FX4 BN2 288230429301932032)
    (test FX1 BN3 536870913)
    (test FX2 BN3 536870913)
    (test FX3 BN3 288230376151711743)
    (test FX4 BN3 288230376688582656)
    (test FX1 BN4 536871012)
    (test FX2 BN4 536871012)
    (test FX3 BN4 288230429301931932)
    (test FX4 BN4 72057607459700736)
    #f)

  (let-syntax ((test (make-flonum-test lcm $lcm-fixnum-flonum)))
    (test 0 IFL1 0.0)
    (test FX1 IFL1 0.0)
    (test FX2 IFL1 0.0)
    (test FX3 IFL1 0.0)
    (test FX4 IFL1 0.0)
    (test FX1 IFL2 0.0)
    (test FX2 IFL2 0.0)
    (test FX3 IFL2 0.0)
    (test FX4 IFL2 0.0)
    (test FX1 IFL3 1.0)
    (test FX2 IFL3 1.0)
    (test FX3 IFL3 536870911.0)
    (test FX4 IFL3 536870912.0)
    (test FX1 IFL4 1.0)
    (test FX2 IFL4 1.0)
    (test FX3 IFL4 536870911.0)
    (test FX4 IFL4 536870912.0)
    (test FX1 IFL5 2.0)
    (test FX2 IFL5 2.0)
    (test FX3 IFL5 1073741822.0)
    (test FX4 IFL5 536870912.0)
    (test FX1 IFL6 2.0)
    (test FX2 IFL6 2.0)
    (test FX3 IFL6 1073741822.0)
    (test FX4 IFL6 536870912.0)
    #f)

  (let-syntax ((test (make-test lcm $lcm-fixnum-bignum)))
    (test 0 VBN1 0)
    (test FX1 VBN1 1152921504606846976)
    (test FX2 VBN1 1152921504606846976)
    (test FX3 VBN1 618970018489768632842715136)
    (test FX4 VBN1 1152921504606846976)
    (test FX1 VBN2 1152921504606847075)
    (test FX2 VBN2 1152921504606847075)
    (test FX3 VBN2 618970018489768685992935325)
    (test FX4 VBN2 618970019642690190599782400)
    (test FX1 VBN3 1152921504606846977)
    (test FX2 VBN3 1152921504606846977)
    (test FX3 VBN3 618970018489768633379586047)
    (test FX4 VBN3 618970019642690137986433024)
    (test FX1 VBN4 1152921504606847076)
    (test FX2 VBN4 1152921504606847076)
    (test FX3 VBN4 618970018489768686529806236)
    (test FX4 VBN4 154742504910672547784163328)
    #f)

  #t)


(parametrise ((check-test-name	'bignums))

  (check (lcm (+ 1 (greatest-fixnum)))		=> (+ 1 (greatest-fixnum)))
  (check (lcm (- (+ 1 (greatest-fixnum))))	=> (+ 1 (greatest-fixnum)))

  (let-syntax ((test (make-test lcm #;$lcm-bignum-fixnum)))
    (test BN1 0 0)
    (test BN1 FX1 536870912)
    (test BN2 FX1 536871011)
    (test BN3 FX1 536870913)
    (test BN4 FX1 536871012)
    (test BN1 FX2 536870912)
    (test BN2 FX2 536871011)
    (test BN3 FX2 536870913)
    (test BN4 FX2 536871012)
    (test BN1 FX3 288230375614840832)
    (test BN2 FX3 288230428765061021)
    (test BN3 FX3 288230376151711743)
    (test BN4 FX3 288230429301931932)
    (test BN1 FX4 536870912)
    (test BN2 FX4 288230429301932032)
    (test BN3 FX4 288230376688582656)
    (test BN4 FX4 72057607459700736)
    #f)

  (let-syntax ((test (make-test lcm #;$lcm-bignum-bignum)))
    (test BN1 BN1 536870912)
    (test BN2 BN1 288230429301932032)
    (test BN3 BN1 288230376688582656)
    (test BN4 BN1 72057607459700736)
    (test BN1 BN2 288230429301932032)
    (test BN2 BN2 536871011)
    (test BN3 BN2 288230429838803043)
    (test BN4 BN2 288230482989033132)
    (test BN1 BN3 288230376688582656)
    (test BN2 BN3 288230429838803043)
    (test BN3 BN3 536870913)
    (test BN4 BN3 96076810125224652)
    (test BN1 BN4 72057607459700736)
    (test BN2 BN4 288230482989033132)
    (test BN3 BN4 96076810125224652)
    (test BN4 BN4 536871012)
    #f)

  (let-syntax ((test (make-inexact-test lcm #;$lcm-bignum-flonum)))
    (test BN1 IFL1 0.0)
    (test BN2 IFL1 0.0)
    (test BN3 IFL1 0.0)
    (test BN4 IFL1 0.0)
    (test BN1 IFL2 0.0)
    (test BN2 IFL2 0.0)
    (test BN3 IFL2 0.0)
    (test BN4 IFL2 0.0)
    (test BN1 IFL3 536870912.0)
    (test BN2 IFL3 536871011.0)
    (test BN3 IFL3 536870913.0)
    (test BN4 IFL3 536871012.0)
    (test BN1 IFL4 536870912.0)
    (test BN2 IFL4 536871011.0)
    (test BN3 IFL4 536870913.0)
    (test BN4 IFL4 536871012.0)
    (test BN1 IFL5 536870912.0)
    (test BN2 IFL5 1073742022.0)
    (test BN3 IFL5 1073741826.0)
    (test BN4 IFL5 536871012.0)
    (test BN1 IFL6 536870912.0)
    (test BN2 IFL6 1073742022.0)
    (test BN3 IFL6 1073741826.0)
    (test BN4 IFL6 536871012.0)
    #f)

;;; --------------------------------------------------------------------

  (let-syntax ((test (make-test lcm $lcm-bignum-fixnum)))
    (test VBN1 0 0)
    (test VBN1 FX1 1152921504606846976)
    (test VBN2 FX1 1152921504606847075)
    (test VBN3 FX1 1152921504606846977)
    (test VBN4 FX1 1152921504606847076)
    (test VBN1 FX2 1152921504606846976)
    (test VBN2 FX2 1152921504606847075)
    (test VBN3 FX2 1152921504606846977)
    (test VBN4 FX2 1152921504606847076)
    (test VBN1 FX3 618970018489768632842715136)
    (test VBN2 FX3 618970018489768685992935325)
    (test VBN3 FX3 618970018489768633379586047)
    (test VBN4 FX3 618970018489768686529806236)
    (test VBN1 FX4 1152921504606846976)
    (test VBN2 FX4 618970019642690190599782400)
    (test VBN3 FX4 618970019642690137986433024)
    (test VBN4 FX4 154742504910672547784163328)
    #f)

  (let-syntax ((test (make-test lcm $lcm-bignum-bignum)))
    (test VBN1 VBN1 1152921504606846976)
    (test VBN2 VBN1 1329227995784915987043036016358195200)
    (test VBN3 VBN1 1329227995784915874056728564887191552)
    (test VBN4 VBN1 332306998946228997048989380241260544)
    (test VBN1 VBN2 1329227995784915987043036016358195200)
    (test VBN2 VBN2 1152921504606847075)
    (test VBN3 VBN2 1329227995784915988195957520965042275)
    (test VBN4 VBN2 1329227995784916102335186477042902700)
    (test VBN1 VBN3 1329227995784915874056728564887191552)
    (test VBN2 VBN3 1329227995784915988195957520965042275)
    (test VBN3 VBN3 1152921504606846977)
    (test VBN4 VBN3 1329227995784915989348879025571889252)
    (test VBN1 VBN4 332306998946228997048989380241260544)
    (test VBN2 VBN4 1329227995784916102335186477042902700)
    (test VBN3 VBN4 1329227995784915989348879025571889252)
    (test VBN4 VBN4 1152921504606847076)
    #f)

  (let-syntax ((test (make-inexact-test lcm $lcm-bignum-flonum)))
    (test VBN1 IFL1 0.0)
    (test VBN2 IFL1 0.0)
    (test VBN3 IFL1 0.0)
    (test VBN4 IFL1 0.0)
    (test VBN1 IFL2 0.0)
    (test VBN2 IFL2 0.0)
    (test VBN3 IFL2 0.0)
    (test VBN4 IFL2 0.0)
    (test VBN1 IFL3 1.152921504606847e18)
    (test VBN2 IFL3 1.152921504606847e18)
    (test VBN3 IFL3 1.152921504606847e18)
    (test VBN4 IFL3 1.152921504606847e18)
    (test VBN1 IFL4 1.152921504606847e18)
    (test VBN2 IFL4 1.152921504606847e18)
    (test VBN3 IFL4 1.152921504606847e18)
    (test VBN4 IFL4 1.152921504606847e18)
    (test VBN1 IFL5 1.152921504606847e18)
    (test VBN2 IFL5 2.305843009213694e18)
    (test VBN3 IFL5 2.305843009213694e18)
    (test VBN4 IFL5 1.152921504606847e18)
    (test VBN1 IFL6 1.152921504606847e18)
    (test VBN2 IFL6 2.305843009213694e18)
    (test VBN3 IFL6 2.305843009213694e18)
    (test VBN4 IFL6 1.152921504606847e18)
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (check (lcm +0.0)		=> +0.0)
  (check (lcm -0.0)		=> +0.0)
  (check (lcm +123.0)		=> +123.0)
  (check (lcm -123.0)		=> +123.0)

  (let-syntax ((test (make-inexact-test lcm $lcm-flonum-fixnum)))
    (test IFL1 0 0.0)
    (test IFL1 FX1 0.0)
    (test IFL2 FX1 0.0)
    (test IFL3 FX1 1.0)
    (test IFL4 FX1 1.0)
    (test IFL5 FX1 2.0)
    (test IFL6 FX1 2.0)
    (test IFL1 FX2 0.0)
    (test IFL2 FX2 0.0)
    (test IFL3 FX2 1.0)
    (test IFL4 FX2 1.0)
    (test IFL5 FX2 2.0)
    (test IFL6 FX2 2.0)
    (test IFL1 FX3 0.0)
    (test IFL2 FX3 0.0)
    (test IFL3 FX3 536870911.0)
    (test IFL4 FX3 536870911.0)
    (test IFL5 FX3 1073741822.0)
    (test IFL6 FX3 1073741822.0)
    (test IFL1 FX4 0.0)
    (test IFL2 FX4 0.0)
    (test IFL3 FX4 536870912.0)
    (test IFL4 FX4 536870912.0)
    (test IFL5 FX4 536870912.0)
    (test IFL6 FX4 536870912.0)
    #f)

  (let-syntax ((test (make-inexact-test lcm #;$lcm-flonum-bignum)))
    (test IFL1 BN1 0.0)
    (test IFL2 BN1 0.0)
    (test IFL3 BN1 536870912.0)
    (test IFL4 BN1 536870912.0)
    (test IFL5 BN1 536870912.0)
    (test IFL6 BN1 536870912.0)
    (test IFL1 BN2 0.0)
    (test IFL2 BN2 0.0)
    (test IFL3 BN2 536871011.0)
    (test IFL4 BN2 536871011.0)
    (test IFL5 BN2 1073742022.0)
    (test IFL6 BN2 1073742022.0)
    (test IFL1 BN3 0.0)
    (test IFL2 BN3 0.0)
    (test IFL3 BN3 536870913.0)
    (test IFL4 BN3 536870913.0)
    (test IFL5 BN3 1073741826.0)
    (test IFL6 BN3 1073741826.0)
    (test IFL1 BN4 0.0)
    (test IFL2 BN4 0.0)
    (test IFL3 BN4 536871012.0)
    (test IFL4 BN4 536871012.0)
    (test IFL5 BN4 536871012.0)
    (test IFL6 BN4 536871012.0)
    #f)

  (let-syntax ((test (make-flonum-test lcm $lcm-flonum-flonum)))
    (test 25.0 10.0 50.0)
    (test 10.0 25.0 50.0)
    (test IFL1 IFL1 0.0)
    (test IFL2 IFL1 0.0)
    (test IFL3 IFL1 0.0)
    (test IFL4 IFL1 0.0)
    (test IFL5 IFL1 0.0)
    (test IFL6 IFL1 0.0)
    (test IFL1 IFL2 0.0)
    (test IFL2 IFL2 0.0)
    (test IFL3 IFL2 0.0)
    (test IFL4 IFL2 0.0)
    (test IFL5 IFL2 0.0)
    (test IFL6 IFL2 0.0)
    (test IFL1 IFL3 0.0)
    (test IFL2 IFL3 0.0)
    (test IFL3 IFL3 1.0)
    (test IFL4 IFL3 1.0)
    (test IFL5 IFL3 2.0)
    (test IFL6 IFL3 2.0)
    (test IFL1 IFL4 0.0)
    (test IFL2 IFL4 0.0)
    (test IFL3 IFL4 1.0)
    (test IFL4 IFL4 1.0)
    (test IFL5 IFL4 2.0)
    (test IFL6 IFL4 2.0)
    (test IFL1 IFL5 0.0)
    (test IFL2 IFL5 0.0)
    (test IFL3 IFL5 2.0)
    (test IFL4 IFL5 2.0)
    (test IFL5 IFL5 2.0)
    (test IFL6 IFL5 2.0)
    (test IFL1 IFL6 0.0)
    (test IFL2 IFL6 0.0)
    (test IFL3 IFL6 2.0)
    (test IFL4 IFL6 2.0)
    (test IFL5 IFL6 2.0)
    (test IFL6 IFL6 2.0)
    #f)

  (let-syntax ((test (make-inexact-test lcm $lcm-flonum-bignum)))
    (test IFL1 VBN1 0.0)
    (test IFL2 VBN1 0.0)
    (test IFL3 VBN1 1.152921504606847e18)
    (test IFL4 VBN1 1.152921504606847e18)
    (test IFL5 VBN1 1.152921504606847e18)
    (test IFL6 VBN1 1.152921504606847e18)
    (test IFL1 VBN2 0.0)
    (test IFL2 VBN2 0.0)
    (test IFL3 VBN2 1.152921504606847e18)
    (test IFL4 VBN2 1.152921504606847e18)
    (test IFL5 VBN2 2.305843009213694e18)
    (test IFL6 VBN2 2.305843009213694e18)
    (test IFL1 VBN3 0.0)
    (test IFL2 VBN3 0.0)
    (test IFL3 VBN3 1.152921504606847e18)
    (test IFL4 VBN3 1.152921504606847e18)
    (test IFL5 VBN3 2.305843009213694e18)
    (test IFL6 VBN3 2.305843009213694e18)
    (test IFL1 VBN4 0.0)
    (test IFL2 VBN4 0.0)
    (test IFL3 VBN4 1.152921504606847e18)
    (test IFL4 VBN4 1.152921504606847e18)
    (test IFL5 VBN4 1.152921504606847e18)
    (test IFL6 VBN4 1.152921504606847e18)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
