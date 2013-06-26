;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: inv
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
(import (vicare)
  (numerics helpers)
  (ikarus system $ratnums)
  (ikarus system $compnums)
  (ikarus system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: inv, unary division\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test-1 / $inv-fixnum))

  #;(test 0	0)
  (test FX1	INV-FX1)
  (test FX2	INV-FX2)
  (test FX3	INV-FX3)
  (test FX4	INV-FX4)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test-1 / #;$inv-bignum)))
    (test BN1 INV-BN1)
    (test BN2 INV-BN2)
    (test BN3 INV-BN3)
    (test BN4 INV-BN4)
    #f)

  (let-syntax ((test (make-test-1 / $inv-bignum)))
    (test VBN1 INV-VBN1)
    (test VBN2 INV-VBN2)
    (test VBN3 INV-VBN3)
    (test VBN4 INV-VBN4)
    #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (let-syntax ((test (make-test-1 / #;$inv-ratnum)))
    (test RN01	INV-RN01)
    (test RN02	INV-RN02)
    (test RN03	INV-RN03)
    (test RN04	INV-RN04)
    (test RN05	INV-RN05)
    (test RN06	INV-RN06)
    (test RN07	INV-RN07)
    ;;(test RN08	INV-RN08) ;not a ratnum
    (test RN09	INV-RN09)

    (test RN10	INV-RN10)
    (test RN11	INV-RN11)
    (test RN12	INV-RN12)
    (test RN13	INV-RN13)
    (test RN14	INV-RN14)
    (test RN15	INV-RN15)
    (test RN16	INV-RN16)
    (test RN17	INV-RN17)
    (test RN18	INV-RN18)
    (test RN19	INV-RN19)

    (test RN20	INV-RN20)
    ;;not ratnums
    ;;
    ;; (test RN21	INV-RN21)
    ;; (test RN22	INV-RN22)
    ;; (test RN23	INV-RN23)
    ;; (test RN24	INV-RN24)
    ;; (test RN25	INV-RN25)
    ;; (test RN26	INV-RN26)
    ;; (test RN27	INV-RN27)
    ;; (test RN28	INV-RN28)
    (test RN29	INV-RN29)

    (test RN30	INV-RN30)
    (test RN31	INV-RN31)
    (test RN32	INV-RN32)
    ;;(test RN33	INV-RN33) ;not a ratnum
    (test RN34	INV-RN34)
    (test RN35	INV-RN35)
    (test RN36	INV-RN36)
    #f)

;;; --------------------------------------------------------------------

  (let-syntax ((test (make-test-1 / $inv-ratnum)))
    (test VRN01	INV-VRN01)
    (test VRN02	INV-VRN02)
    (test VRN03	INV-VRN03)
    (test VRN04	INV-VRN04)
    (test VRN05	INV-VRN05)
    (test VRN06	INV-VRN06)
    (test VRN07	INV-VRN07)
    ;;(test VRN08	INV-VRN08) ;not a ratnum
    (test VRN09	INV-VRN09)

    (test VRN10	INV-VRN10)
    (test VRN11	INV-VRN11)
    (test VRN12	INV-VRN12)
    (test VRN13	INV-VRN13)
    (test VRN14	INV-VRN14)
    (test VRN15	INV-VRN15)
    (test VRN16	INV-VRN16)
    (test VRN17	INV-VRN17)
    (test VRN18	INV-VRN18)
    (test VRN19	INV-VRN19)

    (test VRN20	INV-VRN20)
    ;;not ratnums
    ;;
    ;; (test VRN21	INV-VRN21)
    ;; (test VRN22	INV-VRN22)
    ;; (test VRN23	INV-VRN23)
    ;; (test VRN24	INV-VRN24)
    ;; (test VRN25	INV-VRN25)
    ;; (test VRN26	INV-VRN26)
    ;; (test VRN27	INV-VRN27)
    ;; (test VRN28	INV-VRN28)
    (test VRN29	INV-VRN29)

    (test VRN30	INV-VRN30)
    (test VRN31	INV-VRN31)
    (test VRN32	INV-VRN32)
    ;;(test VRN33	INV-VRN33) ;not a ratnum
    (test VRN34	INV-VRN34)
    (test VRN35	INV-VRN35)
    (test VRN36	INV-VRN36)
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-flonum-test-1 / $inv-flonum))

  (test FL1 INV-FL1)
  (test FL2 INV-FL2)
  (test FL3 INV-FL3)
  (test FL4 INV-FL4)
  (test FL5 INV-FL5)
  (test FL6 INV-FL6)
  (test FL7 INV-FL7)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test-1 / $inv-cflonum))

  (test CFL01 INV-CFL01)
  (test CFL02 INV-CFL02)
  (test CFL03 INV-CFL03)
  (test CFL04 INV-CFL04)
  (test CFL05 INV-CFL05)
  (test CFL06 INV-CFL06)
  (test CFL07 INV-CFL07)
  (test CFL08 INV-CFL08)
  (test CFL09 INV-CFL09)

  (test CFL11 INV-CFL11)
  (test CFL12 INV-CFL12)
  (test CFL13 INV-CFL13)
  (test CFL14 INV-CFL14)
  (test CFL15 INV-CFL15)
  (test CFL16 INV-CFL16)

  #t)


(parametrise ((check-test-name	'compnums))

  (let-syntax ((test (make-compnum-test-1 / $inv-compnum)))
    (test CN001 INV-CN001)
    (test CN002 INV-CN002)
    (test CN003 INV-CN003)
    (test CN004 INV-CN004)

    (test CN005 INV-CN005)
    (test CN006 INV-CN006)
    (test CN007 INV-CN007)
    (test CN008 INV-CN008)

    (test CN009 INV-CN009)
    (test CN010 INV-CN010)
    (test CN011 INV-CN011)
    (test CN012 INV-CN012)

    (test CN013 INV-CN013)
    (test CN014 INV-CN014)
    (test CN015 INV-CN015)
    (test CN016 INV-CN016)

;;; --------------------------------------------------------------------

    (test CN017 INV-CN017)
    (test CN018 INV-CN018)
    (test CN019 INV-CN019)
    (test CN020 INV-CN020)

    (test CN021 INV-CN021)
    (test CN022 INV-CN022)
    (test CN023 INV-CN023)
    (test CN024 INV-CN024)

    (test CN025 INV-CN025)
    (test CN026 INV-CN026)
    (test CN027 INV-CN027)
    (test CN028 INV-CN028)

    (test CN029 INV-CN029)
    (test CN030 INV-CN030)
    (test CN031 INV-CN031)
    (test CN032 INV-CN032)

;;; --------------------------------------------------------------------

    (test CN033 INV-CN033)
    (test CN034 INV-CN034)
    (test CN035 INV-CN035)
    (test CN036 INV-CN036)

    (test CN037 INV-CN037)
    (test CN038 INV-CN038)
    (test CN039 INV-CN039)
    (test CN040 INV-CN040)

    (test CN041 INV-CN041)
    (test CN042 INV-CN042)
    (test CN043 INV-CN043)
    (test CN044 INV-CN044)

    (test CN045 INV-CN045)
    (test CN046 INV-CN046)
    (test CN047 INV-CN047)
    (test CN048 INV-CN048)

;;; --------------------------------------------------------------------

    (test CN049 INV-CN049)
    (test CN050 INV-CN050)
    (test CN051 INV-CN051)
    (test CN052 INV-CN052)

    (test CN053 INV-CN053)
    (test CN054 INV-CN054)
    (test CN055 INV-CN055)
    (test CN056 INV-CN056)

    (test CN057 INV-CN057)
    (test CN058 INV-CN058)
    (test CN059 INV-CN059)
    (test CN060 INV-CN060)

    (test CN061 INV-CN061)
    (test CN062 INV-CN062)
    (test CN063 INV-CN063)
    (test CN064 INV-CN064)

;;; --------------------------------------------------------------------

    (test CN065 INV-CN065)
    (test CN066 INV-CN066)
    (test CN067 INV-CN067)
    (test CN068 INV-CN068)

    (test CN069 INV-CN069)
    (test CN070 INV-CN070)
    (test CN071 INV-CN071)
    (test CN072 INV-CN072)

    (test CN073 INV-CN073)
    (test CN074 INV-CN074)
    (test CN075 INV-CN075)
    (test CN076 INV-CN076)

    (test CN077 INV-CN077)
    (test CN078 INV-CN078)
    (test CN079 INV-CN079)
    (test CN080 INV-CN080)

;;; --------------------------------------------------------------------

    (test CN081 INV-CN081)
    (test CN082 INV-CN082)
    (test CN083 INV-CN083)
    (test CN084 INV-CN084)

    (test CN085 INV-CN085)
    (test CN086 INV-CN086)
    (test CN087 INV-CN087)
    (test CN088 INV-CN088)

    (test CN089 INV-CN089)
    (test CN090 INV-CN090)
    (test CN091 INV-CN091)
    (test CN092 INV-CN092)

    (test CN093 INV-CN093)
    (test CN094 INV-CN094)
    (test CN095 INV-CN095)
    (test CN096 INV-CN096)

;;; --------------------------------------------------------------------

    (test CN097 INV-CN097)
    (test CN098 INV-CN098)
    (test CN099 INV-CN099)
    (test CN100 INV-CN100)

    (test CN101 INV-CN101)
    (test CN102 INV-CN102)
    (test CN103 INV-CN103)
    (test CN104 INV-CN104)

    (test CN105 INV-CN105)
    (test CN106 INV-CN106)
    (test CN107 INV-CN107)
    (test CN108 INV-CN108)

    (test CN109 INV-CN109)
    (test CN110 INV-CN110)
    (test CN111 INV-CN111)
    (test CN112 INV-CN112)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
