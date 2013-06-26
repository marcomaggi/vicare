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
(check-display "*** testing Vicare numerics functions: neg, unary minus\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test-1 - $neg-fixnum))

  (test 0	0)
  (test FX1	NEG-FX1)
  (test FX2	NEG-FX2)
  (test FX3	NEG-FX3)
  (test FX4	NEG-FX4)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test-1 - #;$neg-bignum)))
    (test BN1 NEG-BN1)
    (test BN2 NEG-BN2)
    (test BN3 NEG-BN3)
    (test BN4 NEG-BN4)
    #f)

  (let-syntax ((test (make-test-1 - $neg-bignum)))
    (test VBN1 NEG-VBN1)
    (test VBN2 NEG-VBN2)
    (test VBN3 NEG-VBN3)
    (test VBN4 NEG-VBN4)
    #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (make-test-1 - $neg-ratnum))

  (test RN01	NEG-RN01)
  (test RN02	NEG-RN02)
  (test RN03	NEG-RN03)
  (test RN04	NEG-RN04)
  (test RN05	NEG-RN05)
  (test RN06	NEG-RN06)
  (test RN07	NEG-RN07)
  ;;(test RN08	NEG-RN08) ;not a ratnum
  (test RN09	NEG-RN09)

  (test RN10	NEG-RN10)
  (test RN11	NEG-RN11)
  (test RN12	NEG-RN12)
  (test RN13	NEG-RN13)
  (test RN14	NEG-RN14)
  (test RN15	NEG-RN15)
  (test RN16	NEG-RN16)
  (test RN17	NEG-RN17)
  (test RN18	NEG-RN18)
  (test RN19	NEG-RN19)

  (test RN20	NEG-RN20)
  ;;not ratnums
  ;;
  ;; (test RN21	NEG-RN21)
  ;; (test RN22	NEG-RN22)
  ;; (test RN23	NEG-RN23)
  ;; (test RN24	NEG-RN24)
  ;; (test RN25	NEG-RN25)
  ;; (test RN26	NEG-RN26)
  ;; (test RN27	NEG-RN27)
  ;; (test RN28	NEG-RN28)
  (test RN29	NEG-RN29)

  (test RN30	NEG-RN30)
  (test RN31	NEG-RN31)
  (test RN32	NEG-RN32)
  ;;(test RN33	NEG-RN33) ;not a ratnum
  (test RN34	NEG-RN34)
  (test RN35	NEG-RN35)
  (test RN36	NEG-RN36)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-flonum-test-1 - $neg-flonum))

  (test FL1 NEG-FL1)
  (test FL2 NEG-FL2)
  (test FL3 NEG-FL3)
  (test FL4 NEG-FL4)
  (test FL5 NEG-FL5)
  (test FL6 NEG-FL6)
  (test FL7 NEG-FL7)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test-1 - $neg-cflonum))

  (test CFL01 NEG-CFL01)
  (test CFL02 NEG-CFL02)
  (test CFL03 NEG-CFL03)
  (test CFL04 NEG-CFL04)
  (test CFL05 NEG-CFL05)
  (test CFL06 NEG-CFL06)
  (test CFL07 NEG-CFL07)
  (test CFL08 NEG-CFL08)
  (test CFL09 NEG-CFL09)

  (test CFL11 NEG-CFL11)
  (test CFL12 NEG-CFL12)
  (test CFL13 NEG-CFL13)
  (test CFL14 NEG-CFL14)
  (test CFL15 NEG-CFL15)
  (test CFL16 NEG-CFL16)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (make-compnum-test-1 - $neg-compnum))


  (test CN001 NEG-CN001)
  (test CN002 NEG-CN002)
  (test CN003 NEG-CN003)
  (test CN004 NEG-CN004)

  (test CN005 NEG-CN005)
  (test CN006 NEG-CN006)
  (test CN007 NEG-CN007)
  (test CN008 NEG-CN008)

  (test CN009 NEG-CN009)
  (test CN010 NEG-CN010)
  (test CN011 NEG-CN011)
  (test CN012 NEG-CN012)

  (test CN013 NEG-CN013)
  (test CN014 NEG-CN014)
  (test CN015 NEG-CN015)
  (test CN016 NEG-CN016)

;;; --------------------------------------------------------------------

  (test CN017 NEG-CN017)
  (test CN018 NEG-CN018)
  (test CN019 NEG-CN019)
  (test CN020 NEG-CN020)

  (test CN021 NEG-CN021)
  (test CN022 NEG-CN022)
  (test CN023 NEG-CN023)
  (test CN024 NEG-CN024)

  (test CN025 NEG-CN025)
  (test CN026 NEG-CN026)
  (test CN027 NEG-CN027)
  (test CN028 NEG-CN028)

  (test CN029 NEG-CN029)
  (test CN030 NEG-CN030)
  (test CN031 NEG-CN031)
  (test CN032 NEG-CN032)

;;; --------------------------------------------------------------------

  (test CN033 NEG-CN033)
  (test CN034 NEG-CN034)
  (test CN035 NEG-CN035)
  (test CN036 NEG-CN036)

  (test CN037 NEG-CN037)
  (test CN038 NEG-CN038)
  (test CN039 NEG-CN039)
  (test CN040 NEG-CN040)

  (test CN041 NEG-CN041)
  (test CN042 NEG-CN042)
  (test CN043 NEG-CN043)
  (test CN044 NEG-CN044)

  (test CN045 NEG-CN045)
  (test CN046 NEG-CN046)
  (test CN047 NEG-CN047)
  (test CN048 NEG-CN048)

;;; --------------------------------------------------------------------

  (test CN049 NEG-CN049)
  (test CN050 NEG-CN050)
  (test CN051 NEG-CN051)
  (test CN052 NEG-CN052)

  (test CN053 NEG-CN053)
  (test CN054 NEG-CN054)
  (test CN055 NEG-CN055)
  (test CN056 NEG-CN056)

  (test CN057 NEG-CN057)
  (test CN058 NEG-CN058)
  (test CN059 NEG-CN059)
  (test CN060 NEG-CN060)

  (test CN061 NEG-CN061)
  (test CN062 NEG-CN062)
  (test CN063 NEG-CN063)
  (test CN064 NEG-CN064)

;;; --------------------------------------------------------------------

  (test CN065 NEG-CN065)
  (test CN066 NEG-CN066)
  (test CN067 NEG-CN067)
  (test CN068 NEG-CN068)

  (test CN069 NEG-CN069)
  (test CN070 NEG-CN070)
  (test CN071 NEG-CN071)
  (test CN072 NEG-CN072)

  (test CN073 NEG-CN073)
  (test CN074 NEG-CN074)
  (test CN075 NEG-CN075)
  (test CN076 NEG-CN076)

  (test CN077 NEG-CN077)
  (test CN078 NEG-CN078)
  (test CN079 NEG-CN079)
  (test CN080 NEG-CN080)

;;; --------------------------------------------------------------------

  (test CN081 NEG-CN081)
  (test CN082 NEG-CN082)
  (test CN083 NEG-CN083)
  (test CN084 NEG-CN084)

  (test CN085 NEG-CN085)
  (test CN086 NEG-CN086)
  (test CN087 NEG-CN087)
  (test CN088 NEG-CN088)

  (test CN089 NEG-CN089)
  (test CN090 NEG-CN090)
  (test CN091 NEG-CN091)
  (test CN092 NEG-CN092)

  (test CN093 NEG-CN093)
  (test CN094 NEG-CN094)
  (test CN095 NEG-CN095)
  (test CN096 NEG-CN096)

;;; --------------------------------------------------------------------

  (test CN097 NEG-CN097)
  (test CN098 NEG-CN098)
  (test CN099 NEG-CN099)
  (test CN100 NEG-CN100)

  (test CN101 NEG-CN101)
  (test CN102 NEG-CN102)
  (test CN103 NEG-CN103)
  (test CN104 NEG-CN104)

  (test CN105 NEG-CN105)
  (test CN106 NEG-CN106)
  (test CN107 NEG-CN107)
  (test CN108 NEG-CN108)

  (test CN109 NEG-CN109)
  (test CN110 NEG-CN110)
  (test CN111 NEG-CN111)
  (test CN112 NEG-CN112)

  #t)


;;;; done

(check-report)

;;; end of file
