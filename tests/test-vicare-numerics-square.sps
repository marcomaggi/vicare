;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: square
;;;Date: Thu Nov 29, 2012
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
(check-display "*** testing Vicare numerics functions: square\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test-1 square $square-fixnum))

  (test 0 0)
  (test 1 1)
  (test -1 1)
  (test FX1 1)
  (test FX2 1)
  (test FX3 288230375077969921)
  (test FX4 288230376151711744)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test-1 square #;$square-bignum)))
    (test BN1 288230376151711744)
    (test BN2 288230482452162121)
    (test BN3 288230377225453569)
    (test BN4 288230483525904144)
    #f)

  (let-syntax ((test (make-test-1 square $square-bignum)))
    (test VBN1 1329227995784915872903807060280344576)
    (test VBN2 1329227995784916101182264972436055625)
    (test VBN3 1329227995784915875209650069494038529)
    (test VBN4 1329227995784916103488107981649749776)
    #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (let-syntax ((test (make-test-1 square #;$square-ratnum)))
    (test 1/2 1/4)
    (test -1/2 1/4)
    (test RN01 1/15129)
    (test RN02 1/15129)
    (test RN03 1/15129)
    (test RN04 288230376151711744/15129)
    (test RN05 1/288230376151711744)
    (test RN06 1/288230376151711744)
    (test RN07 288230375077969921/288230376151711744)
    (test RN09 1/288230482452162121)
    (test RN10 1/288230482452162121)
    (test RN11 288230375077969921/288230482452162121)
    (test RN12 288230376151711744/288230482452162121)
    (test RN13 1/288230377225453569)
    (test RN14 1/288230377225453569)
    (test RN15 288230375077969921/288230377225453569)
    (test RN16 288230376151711744/288230377225453569)
    (test RN17 1/288230483525904144)
    (test RN18 1/288230483525904144)
    (test RN19 288230375077969921/288230483525904144)
    (test RN20 18014398509481984/18014405220369009)
    (test RN29 288230376151711744/288230375077969921)
    (test RN30 288230482452162121/288230375077969921)
    (test RN31 288230377225453569/288230375077969921)
    (test RN32 288230483525904144/288230375077969921)
    (test RN34 288230482452162121/288230376151711744)
    (test RN35 288230377225453569/288230376151711744)
    (test RN36 18014405220369009/18014398509481984)
    #f)

  (let-syntax ((test (make-test-1 square $square-ratnum)))
    (test VRN01 1/15129)
    (test VRN02 1/15129)
    (test VRN03 1/15129)
    (test VRN04 288230376151711744/15129)
    (test VRN05 1/288230376151711744)
    (test VRN06 1/288230376151711744)
    (test VRN07 288230375077969921/288230376151711744)
    (test VRN09 1/288230482452162121)
    (test VRN10 1/288230482452162121)
    (test VRN11 288230375077969921/288230482452162121)
    (test VRN12 288230376151711744/288230482452162121)
    (test VRN13 1/288230377225453569)
    (test VRN14 1/288230377225453569)
    (test VRN15 288230375077969921/288230377225453569)
    (test VRN16 288230376151711744/288230377225453569)
    (test VRN17 1/288230483525904144)
    (test VRN18 1/288230483525904144)
    (test VRN19 288230375077969921/288230483525904144)
    (test VRN20 18014398509481984/18014405220369009)
    (test VRN29 288230376151711744/288230375077969921)
    (test VRN30 288230482452162121/288230375077969921)
    (test VRN31 288230377225453569/288230375077969921)
    (test VRN32 288230483525904144/288230375077969921)
    (test VRN34 288230482452162121/288230376151711744)
    (test VRN35 288230377225453569/288230376151711744)
    (test VRN36 18014405220369009/18014398509481984)
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (let-syntax ((test (make-test-1 square $flsquare)))
    (test FL1 0.0)
    (test FL2 0.0)
    (test FL3 4.507129000000001)
    (test FL4 4.507129000000001)
    (test FL5 +inf.0)
    (test FL6 +inf.0)
    (test FL7 +nan.0)
    #f)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test-1 square $square-cflonum))

  (test CFL01 0.0+0.0i)
  (test CFL02 0.0-0.0i)
  (test CFL03 0.0-0.0i)
  (test CFL04 0.0+0.0i)
  (test CFL05 1.44+0.0i)
  (test CFL06 1.44-0.0i)
  (test CFL07 -1.44-0.0i)
  (test CFL08 -1.44+0.0i)
  (test CFL09 -inf.0+inf.0i)
  (test CFL10 -inf.0-inf.0i)
  (test CFL11 +inf.0-inf.0i)
  (test CFL12 +inf.0+inf.0i)
  (test CFL13 +nan.0+nan.0i)
  (test CFL14 +nan.0+nan.0i)
  (test CFL15 +nan.0+nan.0i)
  (test CFL16 +nan.0+nan.0i)

  #t)


(parametrise ((check-test-name	'compnums))

  (let-syntax ((test (make-test-1 square #;$square-compnum)))
    (test 10+20i -300+400i)
    (test 1.0+20.0i -399.0+40.0i)
    (test 10.0+2.0i 96.0+40.0i)
    (test 1/2+20i -1599/4+20i)
    (test 10+2/3i 896/9+40/3i)
    (test (C BN1 20) 288230376151711344+21474836480i)
    (test (C 10 BN1) -288230376151711644+10737418240i)
    #f)

  (let-syntax ((test (make-test-1 square $square-compnum)))
    (test 10+20i -300+400i)
    (test 1.0+20.0i -399.0+40.0i)
    (test 10.0+2.0i 96.0+40.0i)
    (test 1/2+20i -1599/4+20i)
    (test 10+2/3i 896/9+40/3i)
    (test (C VBN1 20) 1329227995784915872903807060280344176+46116860184273879040i)
    (test (C 10 VBN1) -1329227995784915872903807060280344476+23058430092136939520i)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
