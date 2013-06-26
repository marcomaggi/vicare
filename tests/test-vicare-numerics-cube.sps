;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: cube
;;;Date: Wed Dec  5, 2012
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
(check-display "*** testing Vicare numerics functions: cube\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test-1 cube $cube-fixnum))

  (test 0	0)
  (test +1	+1)
  (test -1	-1)
  (test +2	+8)
  (test -2	-8)
  (test +3	+27)
  (test -3	-27)
  (test FX1 (%cube FX1))
  (test FX2 (%cube FX2))
  (test FX3 (%cube FX3))
  (test FX4 (%cube FX4))

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test-1 cube #;$cube-bignum)))
    (test BN1 (%cube BN1))
    (test BN2 (%cube BN2))
    (test BN3 (%cube BN3))
    (test BN4 (%cube BN4))
    #f)

  (let-syntax ((test (make-test-1 cube $cube-bignum)))
    (test VBN1 (%cube VBN1))
    (test VBN2 (%cube VBN2))
    (test VBN3 (%cube VBN3))
    (test VBN4 (%cube VBN4))
    #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (let-syntax ((test (make-test-1 cube #;$cube-ratnum)))
    (test +1/2	(%cube +1/2))
    (test -1/2	(%cube -1/2))

    (test RN01	(%cube RN01))
    (test RN02	(%cube RN02))
    (test RN03	(%cube RN03))
    (test RN04	(%cube RN04))
    (test RN05	(%cube RN05))
    (test RN06	(%cube RN06))
    (test RN07	(%cube RN07))
    (test RN09	(%cube RN09))
    (test RN10	(%cube RN10))
    (test RN11	(%cube RN11))
    (test RN12	(%cube RN12))
    (test RN13	(%cube RN13))
    (test RN14	(%cube RN14))
    (test RN15	(%cube RN15))
    (test RN16	(%cube RN16))
    (test RN17	(%cube RN17))
    (test RN18	(%cube RN18))
    (test RN19	(%cube RN19))
    (test RN20	(%cube RN20))
    (test RN29	(%cube RN29))
    (test RN30	(%cube RN30))
    (test RN31	(%cube RN31))
    (test RN32	(%cube RN32))
    (test RN34	(%cube RN34))
    (test RN35	(%cube RN35))
    (test RN36	(%cube RN36))
    #f)

  (let-syntax ((test (make-test-1 cube $cube-ratnum)))
    (test VRN01	(%cube VRN01))
    (test VRN02	(%cube VRN02))
    (test VRN03	(%cube VRN03))
    (test VRN04	(%cube VRN04))
    (test VRN05	(%cube VRN05))
    (test VRN06	(%cube VRN06))
    (test VRN07	(%cube VRN07))
    (test VRN09	(%cube VRN09))
    (test VRN10	(%cube VRN10))
    (test VRN11	(%cube VRN11))
    (test VRN12	(%cube VRN12))
    (test VRN13	(%cube VRN13))
    (test VRN14	(%cube VRN14))
    (test VRN15	(%cube VRN15))
    (test VRN16	(%cube VRN16))
    (test VRN17	(%cube VRN17))
    (test VRN18	(%cube VRN18))
    (test VRN19	(%cube VRN19))
    (test VRN20	(%cube VRN20))
    (test VRN29	(%cube VRN29))
    (test VRN30	(%cube VRN30))
    (test VRN31	(%cube VRN31))
    (test VRN32	(%cube VRN32))
    (test VRN34	(%cube VRN34))
    (test VRN35	(%cube VRN35))
    (test VRN36	(%cube VRN36))
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-test-1 cube $flcube))

  (test FL1	(%cube FL1))
  (test FL2	(%cube FL2))
  (test FL3	(%cube FL3))
  (test FL4	(%cube FL4))
  (test FL5	(%cube FL5))
  (test FL6	(%cube FL6))
  (test FL7	(%cube FL7))

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test-1 cube $cube-cflonum))

  (test CFL01	(%cube CFL01))
  (test CFL02	-0.0+0.0i #;(%cube CFL02)) ;different!!!
  (test CFL03	(%cube CFL03))
  (test CFL04	-0.0-0.0i #;(%cube CFL04)) ;different!!!
  (test CFL05	(%cube CFL05))
  (test CFL06	(%cube CFL06))
  (test CFL07	(%cube CFL07))
  (test CFL08	(%cube CFL08))
  (test CFL09	(%cube CFL09))
  (test CFL10	(%cube CFL10))
  (test CFL11	(%cube CFL11))
  (test CFL12	(%cube CFL12))
  (test CFL13	(%cube CFL13))
  (test CFL14	(%cube CFL14))
  (test CFL15	(%cube CFL15))
  (test CFL16	(%cube CFL16))

  #t)


(parametrise ((check-test-name	'compnums))

  (let-syntax ((test (make-test-1 cube #;$cube-compnum)))
    (test 0+0.0i	(%cube 0+0.0i))
    (test 0-0.0i	(%cube 0-0.0i))

    (test 10+20i	(%cube 10+20i))
    (test 1.0+20.0i	(%cube 1.0+20.0i))
    (test 10.0+2.0i	(%cube 10.0+2.0i))
    (test 1/2+20i	(%cube 1/2+20i))
    (test 10+2/3i	(%cube 10+2/3i))
    (test (C BN1 20)	(%cube (C BN1 20)))
    (test (C 10 BN1)	(%cube (C 10 BN1)))
    (test (C BN1 2.0)	(%cube (C BN1 2.0)))
    (test (C 1.0 BN1)	(%cube (C 1.0 BN1)))
    #f)

  (let-syntax ((test (make-test-1 cube $cube-compnum)))
    (test 0+0.0i	(%cube 0+0.0i))
    (test 0-0.0i	(%cube 0-0.0i))

    (test 10+20i	(%cube 10+20i))
    (test 1.0+20.0i	(%cube 1.0+20.0i))
    (test 10.0+2.0i	(%cube 10.0+2.0i))
    (test 1/2+20i	(%cube 1/2+20i))
    (test 10+2/3i	(%cube 10+2/3i))
    (test (C VBN1 20)	(%cube (C VBN1 20)))
    (test (C 10 VBN1)	(%cube (C 10 VBN1)))
    (test (C VBN1 2.0)	(%cube (C VBN1 2.0)))
    (test (C 1.0 VBN1)	(%cube (C 1.0 VBN1)))
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
