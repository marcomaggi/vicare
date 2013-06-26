;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: helpers for numerics functions tests
;;;Date: Wed Jun 26, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (numerics helpers)
  (export
    C R I
    make-test			make-test-1
    make-flonum-test		make-flonum-test-1
    make-cflonum-test		make-cflonum-test-1
    make-compnum-test		make-compnum-test-1
    make-inexact-test		make-inexact-test-1
    flonum=?
    cflonum=?
    compnum=?
    inexact=?
    flonum-quasi=?
    cflonum-quasi=?
    compnum-quasi=?
    %cube

;;; --------------------------------------------------------------------

    GREATEST-FX-32-bit
    LEAST-FX-32-bit
    GREATEST-FX-64-bit
    LEAST-FX-64-bit

;;; --------------------------------------------------------------------

    FX1
    FX2
    FX3
    FX4

;;; --------------------------------------------------------------------

    BN1
    BN2
    BN3
    BN4

;;; --------------------------------------------------------------------

    VBN1
    VBN2
    VBN3
    VBN4

;;; --------------------------------------------------------------------

    RN01
    RN02
    RN03
    RN04
    RN05
    RN06
    RN07

    RN09
    RN10
    RN11
    RN12

    RN13
    RN14
    RN15
    RN16

    RN17
    RN18
    RN19
    RN20

    RN29
    RN30
    RN31
    RN32

    RN34
    RN35
    RN36

    VRN01
    VRN02
    VRN03
    VRN04

    VRN05
    VRN06
    VRN07

    VRN09
    VRN10
    VRN11
    VRN12

    VRN13
    VRN14
    VRN15
    VRN16

    VRN17
    VRN18
    VRN19
    VRN20

    VRN29
    VRN30
    VRN31
    VRN32

    VRN34
    VRN35
    VRN36

;;; --------------------------------------------------------------------

    FL1
    FL2
    FL3
    FL4
    FL5
    FL6
    FL7

;;; --------------------------------------------------------------------

    CFL01
    CFL02
    CFL03
    CFL04

    CFL05
    CFL06
    CFL07
    CFL08

    CFL09
    CFL10
    CFL11
    CFL12

    CFL13
    CFL14
    CFL15
    CFL16

;;; --------------------------------------------------------------------

    NEG-GREATEST-FX-32-bit
    NEG-LEAST-FX-32-bit
    NEG-GREATEST-FX-64-bit
    NEG-LEAST-FX-64-bit

;;; --------------------------------------------------------------------

    NEG-FX1
    NEG-FX2
    NEG-FX3
    NEG-FX4

;;; --------------------------------------------------------------------

    NEG-BN1
    NEG-BN2
    NEG-BN3
    NEG-BN4

;;; --------------------------------------------------------------------

    NEG-VBN1
    NEG-VBN2
    NEG-VBN3
    NEG-VBN4

;;; --------------------------------------------------------------------

    NEG-RN01
    NEG-RN02
    NEG-RN03
    NEG-RN04
    NEG-RN05
    NEG-RN06
    NEG-RN07

    NEG-RN09
    NEG-RN10
    NEG-RN11
    NEG-RN12

    NEG-RN13
    NEG-RN14
    NEG-RN15
    NEG-RN16

    NEG-RN17
    NEG-RN18
    NEG-RN19
    NEG-RN20

    NEG-RN29
    NEG-RN30
    NEG-RN31
    NEG-RN32

    NEG-RN34
    NEG-RN35
    NEG-RN36

    NEG-VRN01
    NEG-VRN02
    NEG-VRN03
    NEG-VRN04

    NEG-VRN05
    NEG-VRN06
    NEG-VRN07

    NEG-VRN09
    NEG-VRN10
    NEG-VRN11
    NEG-VRN12

    NEG-VRN13
    NEG-VRN14
    NEG-VRN15
    NEG-VRN16

    NEG-VRN17
    NEG-VRN18
    NEG-VRN19
    NEG-VRN20

    NEG-VRN29
    NEG-VRN30
    NEG-VRN31
    NEG-VRN32

    NEG-VRN34
    NEG-VRN35
    NEG-VRN36

;;; --------------------------------------------------------------------

    NEG-FL1
    NEG-FL2
    NEG-FL3
    NEG-FL4
    NEG-FL5
    NEG-FL6
    NEG-FL7

;;; --------------------------------------------------------------------

    NEG-CFL01
    NEG-CFL02
    NEG-CFL03
    NEG-CFL04

    NEG-CFL05
    NEG-CFL06
    NEG-CFL07
    NEG-CFL08

    NEG-CFL09
    NEG-CFL10
    NEG-CFL11
    NEG-CFL12

    NEG-CFL13
    NEG-CFL14
    NEG-CFL15
    NEG-CFL16

;;;; compnum constants

    CN001
    CN002
    CN003
    CN004

    CN005
    CN006
    CN007
    CN008

    CN009
    CN010
    CN011
    CN012

    CN013
    CN014
    CN015
    CN016

;;; --------------------------------------------------------------------

    CN017
    CN018
    CN019
    CN020

    CN021
    CN022
    CN023
    CN024

    CN025
    CN026
    CN027
    CN028

    CN029
    CN030
    CN031
    CN032

;;; --------------------------------------------------------------------

    CN033
    CN034
    CN035
    CN036

    CN037
    CN038
    CN039
    CN040

    CN041
    CN042
    CN043
    CN044

    CN045
    CN046
    CN047
    CN048

;;; --------------------------------------------------------------------

    CN049
    CN050
    CN051
    CN052

    CN053
    CN054
    CN055
    CN056

    CN057
    CN058
    CN059
    CN060

    CN061
    CN062
    CN063
    CN064

;;; --------------------------------------------------------------------

    CN065
    CN066
    CN067
    CN068

    CN069
    CN070
    CN071
    CN072

    CN073
    CN074
    CN075
    CN076

    CN077
    CN078
    CN079
    CN080

;;; --------------------------------------------------------------------

    CN081
    CN082
    CN083
    CN084

    CN085
    CN086
    CN087
    CN088

    CN089
    CN090
    CN091
    CN092

    CN093
    CN094
    CN095
    CN096

;;; --------------------------------------------------------------------

    CN097
    CN098
    CN099
    CN100

    CN101
    CN102
    CN103
    CN104

    CN105
    CN106
    CN107
    CN108

    CN109
    CN110
    CN111
    CN112

;;; --------------------------------------------------------------------

    NEG-CN001
    NEG-CN002
    NEG-CN003
    NEG-CN004

    NEG-CN005
    NEG-CN006
    NEG-CN007
    NEG-CN008

    NEG-CN009
    NEG-CN010
    NEG-CN011
    NEG-CN012

    NEG-CN013
    NEG-CN014
    NEG-CN015
    NEG-CN016

;;; --------------------------------------------------------------------

    NEG-CN017
    NEG-CN018
    NEG-CN019
    NEG-CN020

    NEG-CN021
    NEG-CN022
    NEG-CN023
    NEG-CN024

    NEG-CN025
    NEG-CN026
    NEG-CN027
    NEG-CN028

    NEG-CN029
    NEG-CN030
    NEG-CN031
    NEG-CN032

;;; --------------------------------------------------------------------

    NEG-CN033
    NEG-CN034
    NEG-CN035
    NEG-CN036

    NEG-CN037
    NEG-CN038
    NEG-CN039
    NEG-CN040

    NEG-CN041
    NEG-CN042
    NEG-CN043
    NEG-CN044

    NEG-CN045
    NEG-CN046
    NEG-CN047
    NEG-CN048

;;; --------------------------------------------------------------------

    NEG-CN049
    NEG-CN050
    NEG-CN051
    NEG-CN052

    NEG-CN053
    NEG-CN054
    NEG-CN055
    NEG-CN056

    NEG-CN057
    NEG-CN058
    NEG-CN059
    NEG-CN060

    NEG-CN061
    NEG-CN062
    NEG-CN063
    NEG-CN064

;;; --------------------------------------------------------------------

    NEG-CN065
    NEG-CN066
    NEG-CN067
    NEG-CN068

    NEG-CN069
    NEG-CN070
    NEG-CN071
    NEG-CN072

    NEG-CN073
    NEG-CN074
    NEG-CN075
    NEG-CN076

    NEG-CN077
    NEG-CN078
    NEG-CN079
    NEG-CN080

;;; --------------------------------------------------------------------

    NEG-CN081
    NEG-CN082
    NEG-CN083
    NEG-CN084

    NEG-CN085
    NEG-CN086
    NEG-CN087
    NEG-CN088

    NEG-CN089
    NEG-CN090
    NEG-CN091
    NEG-CN092

    NEG-CN093
    NEG-CN094
    NEG-CN095
    NEG-CN096

;;; --------------------------------------------------------------------

    NEG-CN097
    NEG-CN098
    NEG-CN099
    NEG-CN100

    NEG-CN101
    NEG-CN102
    NEG-CN103
    NEG-CN104

    NEG-CN105
    NEG-CN106
    NEG-CN107
    NEG-CN108

    NEG-CN109
    NEG-CN110
    NEG-CN111
    NEG-CN112

;;; --------------------------------------------------------------------
;;; inverse constants

    INV-GREATEST-FX
    INV-LEAST-FX

;;; --------------------------------------------------------------------

    INV-FX1
    INV-FX2
    INV-FX3
    INV-FX4

;;; --------------------------------------------------------------------

    INV-BN1
    INV-BN2
    INV-BN3
    INV-BN4

    INV-VBN1
    INV-VBN2
    INV-VBN3
    INV-VBN4

;;; --------------------------------------------------------------------

    INV-RN01
    INV-RN02
    INV-RN03
    INV-RN04

    INV-RN05
    INV-RN06
    INV-RN07
    ;; INV-RN08

    INV-RN09
    INV-RN10
    INV-RN11
    INV-RN12

    INV-RN13
    INV-RN14
    INV-RN15
    INV-RN16

    INV-RN17
    INV-RN18
    INV-RN19
    INV-RN20

    ;; INV-RN21
    ;; INV-RN22
    ;; INV-RN23
    ;; INV-RN24

    ;; INV-RN25
    ;; INV-RN26
    ;; INV-RN27
    ;; INV-RN28

    INV-RN29
    INV-RN30
    INV-RN31
    INV-RN32

    ;; INV-RN33
    INV-RN34
    INV-RN35
    INV-RN36

    INV-VRN01
    INV-VRN02
    INV-VRN03
    INV-VRN04

    INV-VRN05
    INV-VRN06
    INV-VRN07

    INV-VRN09
    INV-VRN10
    INV-VRN11
    INV-VRN12

    INV-VRN13
    INV-VRN14
    INV-VRN15
    INV-VRN16

    INV-VRN17
    INV-VRN18
    INV-VRN19
    INV-VRN20

    INV-VRN29
    INV-VRN30
    INV-VRN31
    INV-VRN32

    INV-VRN34
    INV-VRN35
    INV-VRN36

;;; --------------------------------------------------------------------

    INV-FL1
    INV-FL2
    INV-FL3
    INV-FL4
    INV-FL5
    INV-FL6
    INV-FL7

;;; --------------------------------------------------------------------

    INV-CFL01
    INV-CFL02
    INV-CFL03
    INV-CFL04

    INV-CFL05
    INV-CFL06
    INV-CFL07
    INV-CFL08

    INV-CFL09
    INV-CFL10
    INV-CFL11
    INV-CFL12

    INV-CFL13
    INV-CFL14
    INV-CFL15
    INV-CFL16

;;; --------------------------------------------------------------------
;;; inverse compnum constants

    INV-CN001
    INV-CN002
    INV-CN003
    INV-CN004
    INV-CN005
    INV-CN006
    INV-CN007
    INV-CN008
    INV-CN009
    INV-CN010
    INV-CN011
    INV-CN012
    INV-CN013
    INV-CN014
    INV-CN015
    INV-CN016
    INV-CN017
    INV-CN018
    INV-CN019
    INV-CN020
    INV-CN021
    INV-CN022
    INV-CN023
    INV-CN024
    INV-CN025
    INV-CN026
    INV-CN027
    INV-CN028
    INV-CN029
    INV-CN030
    INV-CN031
    INV-CN032
    INV-CN033
    INV-CN034
    INV-CN035
    INV-CN036
    INV-CN037
    INV-CN038
    INV-CN039
    INV-CN040
    INV-CN041
    INV-CN042
    INV-CN043
    INV-CN044
    INV-CN045
    INV-CN046
    INV-CN047
    INV-CN048
    INV-CN049
    INV-CN050
    INV-CN051
    INV-CN052
    INV-CN053
    INV-CN054
    INV-CN055
    INV-CN056
    INV-CN057
    INV-CN058
    INV-CN059
    INV-CN060
    INV-CN061
    INV-CN062
    INV-CN063
    INV-CN064
    INV-CN065
    INV-CN066
    INV-CN067
    INV-CN068
    INV-CN069
    INV-CN070
    INV-CN071
    INV-CN072
    INV-CN073
    INV-CN074
    INV-CN075
    INV-CN076
    INV-CN077
    INV-CN078
    INV-CN079
    INV-CN080
    INV-CN081
    INV-CN082
    INV-CN083
    INV-CN084
    INV-CN085
    INV-CN086
    INV-CN087
    INV-CN088
    INV-CN089
    INV-CN090
    INV-CN091
    INV-CN092
    INV-CN093
    INV-CN094
    INV-CN095
    INV-CN096
    INV-CN097
    INV-CN098
    INV-CN099
    INV-CN100
    INV-CN101
    INV-CN102
    INV-CN103
    INV-CN104
    INV-CN105
    INV-CN106
    INV-CN107
    INV-CN108
    INV-CN109
    INV-CN110
    INV-CN111
    INV-CN112
    )
  (import (vicare)
    (vicare checks))


;;;; helpers

(define C make-rectangular)
(define R real-part)
(define I imag-part)

(define-syntax make-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	=> ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	=> ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	=> (?unsafe-fun ?op1 ?op2))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(check (?safe-fun   ?op1 ?op2)	=> ?expected-result))))
    ))

(define-syntax make-test-1
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	=> ?expected-result)
	  (check (?unsafe-fun ?op1)	=> ?expected-result)
	  (check (?safe-fun   ?op1)	=> (?unsafe-fun ?op1))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(check (?safe-fun   ?op1)	=> ?expected-result))))
    ))

;;; --------------------------------------------------------------------

(define-syntax make-flonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> flonum=?) (?unsafe-fun ?op1 ?op2))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  ))))
    ))

(define-syntax make-flonum-test-1
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1)	(=> flonum=?) ?expected-result)
	  (check (?safe-fun   ?op1)	(=> flonum=?) (?unsafe-fun ?op1))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> flonum=?) ?expected-result)
	  ))))
    ))

;;; --------------------------------------------------------------------

(define-syntax make-cflonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> cflonum=?) (?unsafe-fun ?op1 ?op2))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  ))))
    ))

(define-syntax make-cflonum-test-1
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> cflonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1)	(=> cflonum=?) ?expected-result)
	  (check (?safe-fun   ?op1)	(=> cflonum=?) (?unsafe-fun ?op1))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> cflonum=?) ?expected-result)
	  ))))
    ))

;;; --------------------------------------------------------------------

(define-syntax make-compnum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> compnum=?) (?unsafe-fun ?op1 ?op2))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  ))))
    ))

(define-syntax make-compnum-test-1
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> compnum=?) ?expected-result)
	  (check (?unsafe-fun ?op1)	(=> compnum=?) ?expected-result)
	  (check (?safe-fun   ?op1)	(=> compnum=?) (?unsafe-fun ?op1))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> compnum=?) ?expected-result)
	  ))))
    ))

;;; --------------------------------------------------------------------

(define-syntax make-inexact-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> inexact=?) (?unsafe-fun ?op1 ?op2))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  ))))
    ))

(define-syntax make-inexact-test-1
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> inexact=?) ?expected-result)
	  (check (?unsafe-fun ?op1)	(=> inexact=?) ?expected-result)
	  (check (?safe-fun   ?op1)	(=> inexact=?) (?unsafe-fun ?op1))
	  ))))
    ((_ ?safe-fun)
     (syntax-rules ()
       ((_ ?op1 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1)	(=> inexact=?) ?expected-result)
	  ))))
    ))

;;; --------------------------------------------------------------------

(define (flonum=? x y)
  (cond ((flnan? x)
	 (flnan? y))
	((flzero?/positive x)
	 (flzero?/positive y))
	((flzero?/negative x)
	 (flzero?/negative y))
	((fl=? x y))))

(define (cflonum=? x y)
  (and (flonum=? (real-part x) (real-part y))
       (flonum=? (imag-part x) (imag-part y))))

(define (compnum=? x y)
  (cond ((and (cflonum? x)
	      (cflonum? y))
	 (cflonum=? x y))
	((and (flonum? x)
	      (flonum? y))
	 (flonum=? x y))
	(else
	 (= x y))))

;;; --------------------------------------------------------------------

(define (inexact=? x y)
  (cond ((and (cflonum? x)
	      (cflonum? y))
	 (cflonum-quasi=? x y))
	((and (flonum? x)
	      (flonum? y))
	 (flonum-quasi=? x y))
	((and (compnum? x)
	      (compnum? y))
	 (compnum-quasi=? x y))
	(else
	 (= x y))))

(define (flonum-quasi=? x y)
  (cond ((flnan? x)
	 (flnan? y))
	((flzero?/positive x)
	 (flzero?/positive y))
	((flzero?/negative x)
	 (flzero?/negative y))
	(else
	 (fl<? (flabs (fl- x y)) 1e-8))))

(define (cflonum-quasi=? x y)
  (and (flonum-quasi=? (real-part x) (real-part y))
       (flonum-quasi=? (imag-part x) (imag-part y))))

(define (compnum-quasi=? x y)
  (and (inexact=? (real-part x) (real-part y))
       (inexact=? (imag-part x) (imag-part y))))

;;; --------------------------------------------------------------------

(define (%cube x)
  (* x x x))


;;;; constants

;;*NOTE* Let's keep  these constants as explicit  numbers, without using
;;GREATEST-FIXNUM and LEAST-FIXNUM; this way the code can be executed as
;;is  by other  Scheme  implementations, for  which GREATEST-FIXNUM  and
;;LEAST-FIXNUM may be different than those of Vicare.

(define GREATEST-FX-32-bit	+536870911)
(define LEAST-FX-32-bit		-536870912)
(define GREATEST-FX-64-bit	+1152921504606846975)
(define LEAST-FX-64-bit		-1152921504606846976)

;;; --------------------------------------------------------------------

(define FX1		+1)
(define FX2		-1)
(define FX3		GREATEST-FX-32-bit)
(define FX4		LEAST-FX-32-bit)

;;; --------------------------------------------------------------------

(define BN1		(+ GREATEST-FX-32-bit +1))
(define BN2		(+ GREATEST-FX-32-bit +100))
(define BN3		(+ LEAST-FX-32-bit -1))
(define BN4		(+ LEAST-FX-32-bit -100))

;;; --------------------------------------------------------------------

(define VBN1		(+ GREATEST-FX-64-bit +1))
(define VBN2		(+ GREATEST-FX-64-bit +100))
(define VBN3		(+ LEAST-FX-64-bit -1))
(define VBN4		(+ LEAST-FX-64-bit -100))

;;; --------------------------------------------------------------------

(define RN01		(/ FX1 123))
(define RN02		(/ FX2 123))
(define RN03		(/ FX2 123))
(define RN04		(/ FX4 123))

(define RN05		(/ FX1 BN1))
(define RN06		(/ FX2 BN1))
(define RN07		(/ FX3 BN1))
;;;(define RN08		(/ FX4 BN1)) ;= -1, not a ratnum

(define RN09		(/ FX1 BN2))
(define RN10		(/ FX2 BN2))
(define RN11		(/ FX3 BN2))
(define RN12		(/ FX4 BN2))

(define RN13		(/ FX1 BN3))
(define RN14		(/ FX2 BN3))
(define RN15		(/ FX3 BN3))
(define RN16		(/ FX4 BN3))

(define RN17		(/ FX1 BN4))
(define RN18		(/ FX2 BN4))
(define RN19		(/ FX3 BN4))
(define RN20		(/ FX4 BN4))

;;(define RN21		536870912		#;(/ BN1 FX1)) ;not a ratnum
;;(define RN22		536871011		#;(/ BN2 FX1)) ;not a ratnum
;;(define RN23		-536870913		#;(/ BN3 FX1)) ;not a ratnum
;;(define RN24		-536871012		#;(/ BN4 FX1)) ;not a ratnum

;;(define RN25		-536870912		#;(/ BN1 FX2)) ;not a ratnum
;;(define RN26		-536871011		#;(/ BN2 FX2)) ;not a ratnum
;;(define RN27		536870913		#;(/ BN3 FX2)) ;not a ratnum
;;(define RN28		536871012		#;(/ BN4 FX2)) ;not a ratnum

(define RN29		(/ BN1 FX3))
(define RN30		(/ BN2 FX3))
(define RN31		(/ BN3 FX3))
(define RN32		(/ BN4 FX3))

;;(define RN33		-1			#;(/ BN1 FX4)) ;not a ratnum
(define RN34		(/ BN2 FX4))
(define RN35		(/ BN3 FX4))
(define RN36		(/ BN4 FX4))

(define VRN01		(/ FX1 123))
(define VRN02		(/ FX2 123))
(define VRN03		(/ FX2 123))
(define VRN04		(/ FX4 123))

(define VRN05		(/ FX1 VBN1))
(define VRN06		(/ FX2 VBN1))
(define VRN07		(/ FX3 VBN1))
;;;(define VRN08		(/ FX4 VBN1)) ;= -1, not a ratnum

(define VRN09		(/ FX1 VBN2))
(define VRN10		(/ FX2 VBN2))
(define VRN11		(/ FX3 VBN2))
(define VRN12		(/ FX4 VBN2))

(define VRN13		(/ FX1 VBN3))
(define VRN14		(/ FX2 VBN3))
(define VRN15		(/ FX3 VBN3))
(define VRN16		(/ FX4 VBN3))

(define VRN17		(/ FX1 VBN4))
(define VRN18		(/ FX2 VBN4))
(define VRN19		(/ FX3 VBN4))
(define VRN20		(/ FX4 VBN4))

;;(define VRN21		536870912		#;(/ VBN1 FX1)) ;not a ratnum
;;(define VRN22		536871011		#;(/ VBN2 FX1)) ;not a ratnum
;;(define VRN23		-536870913		#;(/ VBN3 FX1)) ;not a ratnum
;;(define VRN24		-536871012		#;(/ VBN4 FX1)) ;not a ratnum

;;(define VRN25		-536870912		#;(/ VBN1 FX2)) ;not a ratnum
;;(define VRN26		-536871011		#;(/ VBN2 FX2)) ;not a ratnum
;;(define VRN27		536870913		#;(/ VBN3 FX2)) ;not a ratnum
;;(define VRN28		536871012		#;(/ VBN4 FX2)) ;not a ratnum

(define VRN29		(/ VBN1 FX3))
(define VRN30		(/ VBN2 FX3))
(define VRN31		(/ VBN3 FX3))
(define VRN32		(/ VBN4 FX3))

;;(define VRN33		-1			#;(/ VBN1 FX4)) ;not a ratnum
(define VRN34		(/ VBN2 FX4))
(define VRN35		(/ VBN3 FX4))
(define VRN36		(/ VBN4 FX4))

;;; --------------------------------------------------------------------

(define FL1		+0.0)
(define FL2		-0.0)
(define FL3		+2.123)
(define FL4		-2.123)
(define FL5		+inf.0)
(define FL6		-inf.0)
(define FL7		+nan.0)

;;; --------------------------------------------------------------------

(define CFL01		+0.0+0.0i)
(define CFL02		-0.0+0.0i)
(define CFL03		+0.0-0.0i)
(define CFL04		-0.0-0.0i)

(define CFL05		-1.2-0.0i)
(define CFL06		-1.2+0.0i)
(define CFL07		+0.0-1.2i)
(define CFL08		-0.0-1.2i)

(define CFL09		-1.2-inf.0i)
(define CFL10		-1.2+inf.0i)
(define CFL11		+inf.0-1.2i)
(define CFL12		-inf.0-1.2i)

(define CFL13		-1.2-nan.0i)
(define CFL14		-1.2+nan.0i)
(define CFL15		+nan.0-1.2i)
(define CFL16		-nan.0-1.2i)


;;;; compnum constants

(define CN001		(make-rectangular FX1 FX1))
(define CN002		(make-rectangular FX2 FX1))
(define CN003		(make-rectangular FX3 FX1))
(define CN004		(make-rectangular FX4 FX1))

(define CN005		(make-rectangular FX1 FX2))
(define CN006		(make-rectangular FX2 FX2))
(define CN007		(make-rectangular FX3 FX2))
(define CN008		(make-rectangular FX4 FX2))

(define CN009		(make-rectangular FX1 FX3))
(define CN010		(make-rectangular FX2 FX3))
(define CN011		(make-rectangular FX3 FX3))
(define CN012		(make-rectangular FX4 FX3))

(define CN013		(make-rectangular FX1 FX4))
(define CN014		(make-rectangular FX2 FX4))
(define CN015		(make-rectangular FX3 FX4))
(define CN016		(make-rectangular FX4 FX4))

;;; --------------------------------------------------------------------

(define CN017		(make-rectangular BN1 FX1))
(define CN018		(make-rectangular BN2 FX1))
(define CN019		(make-rectangular BN3 FX1))
(define CN020		(make-rectangular BN4 FX1))

(define CN021		(make-rectangular BN1 FX2))
(define CN022		(make-rectangular BN2 FX2))
(define CN023		(make-rectangular BN3 FX2))
(define CN024		(make-rectangular BN4 FX2))

(define CN025		(make-rectangular BN1 FX3))
(define CN026		(make-rectangular BN2 FX3))
(define CN027		(make-rectangular BN3 FX3))
(define CN028		(make-rectangular BN4 FX3))

(define CN029		(make-rectangular BN1 FX4))
(define CN030		(make-rectangular BN2 FX4))
(define CN031		(make-rectangular BN3 FX4))
(define CN032		(make-rectangular BN4 FX4))

;;; --------------------------------------------------------------------

(define CN033		(make-rectangular FX1 BN1))
(define CN034		(make-rectangular FX2 BN1))
(define CN035		(make-rectangular FX3 BN1))
(define CN036		(make-rectangular FX4 BN1))

(define CN037		(make-rectangular FX1 BN2))
(define CN038		(make-rectangular FX2 BN2))
(define CN039		(make-rectangular FX3 BN2))
(define CN040		(make-rectangular FX4 BN2))

(define CN041		(make-rectangular FX1 BN3))
(define CN042		(make-rectangular FX2 BN3))
(define CN043		(make-rectangular FX3 BN3))
(define CN044		(make-rectangular FX4 BN3))

(define CN045		(make-rectangular FX1 BN4))
(define CN046		(make-rectangular FX2 BN4))
(define CN047		(make-rectangular FX3 BN4))
(define CN048		(make-rectangular FX4 BN4))

;;; --------------------------------------------------------------------

(define CN049		(make-rectangular BN1 FL1))
(define CN050		(make-rectangular BN2 FL1))
(define CN051		(make-rectangular BN3 FL1))
(define CN052		(make-rectangular BN4 FL1))

(define CN053		(make-rectangular BN1 FL2))
(define CN054		(make-rectangular BN2 FL2))
(define CN055		(make-rectangular BN3 FL2))
(define CN056		(make-rectangular BN4 FL2))

(define CN057		(make-rectangular BN1 FL3))
(define CN058		(make-rectangular BN2 FL3))
(define CN059		(make-rectangular BN3 FL3))
(define CN060		(make-rectangular BN4 FL3))

(define CN061		(make-rectangular BN1 FL4))
(define CN062		(make-rectangular BN2 FL4))
(define CN063		(make-rectangular BN3 FL4))
(define CN064		(make-rectangular BN4 FL4))

;;; --------------------------------------------------------------------

(define CN065		(make-rectangular FL1 BN1))
(define CN066		(make-rectangular FL2 BN1))
(define CN067		(make-rectangular FL3 BN1))
(define CN068		(make-rectangular FL4 BN1))

(define CN069		(make-rectangular FL1 BN2))
(define CN070		(make-rectangular FL2 BN2))
(define CN071		(make-rectangular FL3 BN2))
(define CN072		(make-rectangular FL4 BN2))

(define CN073		(make-rectangular FL1 BN3))
(define CN074		(make-rectangular FL2 BN3))
(define CN075		(make-rectangular FL3 BN3))
(define CN076		(make-rectangular FL4 BN3))

(define CN077		(make-rectangular FL1 BN4))
(define CN078		(make-rectangular FL2 BN4))
(define CN079		(make-rectangular FL3 BN4))
(define CN080		(make-rectangular FL4 BN4))

;;; --------------------------------------------------------------------

(define CN081		(make-rectangular RN01 FL1))
(define CN082		(make-rectangular RN02 FL1))
(define CN083		(make-rectangular RN03 FL1))
(define CN084		(make-rectangular RN04 FL1))

(define CN085		(make-rectangular RN01 FL2))
(define CN086		(make-rectangular RN02 FL2))
(define CN087		(make-rectangular RN03 FL2))
(define CN088		(make-rectangular RN04 FL2))

(define CN089		(make-rectangular RN01 FL3))
(define CN090		(make-rectangular RN02 FL3))
(define CN091		(make-rectangular RN03 FL3))
(define CN092		(make-rectangular RN04 FL3))

(define CN093		(make-rectangular RN01 FL4))
(define CN094		(make-rectangular RN02 FL4))
(define CN095		(make-rectangular RN03 FL4))
(define CN096		(make-rectangular RN04 FL4))

;;; --------------------------------------------------------------------

(define CN097		(make-rectangular FL1 RN01))
(define CN098		(make-rectangular FL2 RN01))
(define CN099		(make-rectangular FL3 RN01))
(define CN100		(make-rectangular FL4 RN01))

(define CN101		(make-rectangular FL1 RN02))
(define CN102		(make-rectangular FL2 RN02))
(define CN103		(make-rectangular FL3 RN02))
(define CN104		(make-rectangular FL4 RN02))

(define CN105		(make-rectangular FL1 RN03))
(define CN106		(make-rectangular FL2 RN03))
(define CN107		(make-rectangular FL3 RN03))
(define CN108		(make-rectangular FL4 RN03))

(define CN109		(make-rectangular FL1 RN04))
(define CN110		(make-rectangular FL2 RN04))
(define CN111		(make-rectangular FL3 RN04))
(define CN112		(make-rectangular FL4 RN04))


;;;; negated constants

(define NEG-GREATEST-FX-32-bit	(- +536870911))
(define NEG-LEAST-FX-32-bit	(- -536870912))
(define NEG-GREATEST-FX-64-bit	(- +1152921504606846975))
(define NEG-LEAST-FX-64-bit	(- -1152921504606846976))

(define NEG-FX1		(- +1))
(define NEG-FX2		(- -1))
(define NEG-FX3		(- GREATEST-FX-32-bit))
(define NEG-FX4		(- LEAST-FX-32-bit))

;;; --------------------------------------------------------------------

(define NEG-BN1		(- (+ GREATEST-FX-32-bit +1)))
(define NEG-BN2		(- (+ GREATEST-FX-32-bit +100)))
(define NEG-BN3		(- (+ LEAST-FX-32-bit -1)))
(define NEG-BN4		(- (+ LEAST-FX-32-bit -100)))

;;; --------------------------------------------------------------------

(define NEG-VBN1		(- (+ GREATEST-FX-64-bit +1)))
(define NEG-VBN2		(- (+ GREATEST-FX-64-bit +100)))
(define NEG-VBN3		(- (+ LEAST-FX-64-bit -1)))
(define NEG-VBN4		(- (+ LEAST-FX-64-bit -100)))

;;; --------------------------------------------------------------------

(define NEG-RN01		(- (/ FX1 123)))
(define NEG-RN02		(- (/ FX2 123)))
(define NEG-RN03		(- (/ FX2 123)))
(define NEG-RN04		(- (/ FX4 123)))

(define NEG-RN05		(- (/ FX1 BN1)))
(define NEG-RN06		(- (/ FX2 BN1)))
(define NEG-RN07		(- (/ FX3 BN1)))
;;;(define NEG-RN08		(- (/ FX4 BN1))) ; not a ratnum

(define NEG-RN09		(- (/ FX1 BN2)))
(define NEG-RN10		(- (/ FX2 BN2)))
(define NEG-RN11		(- (/ FX3 BN2)))
(define NEG-RN12		(- (/ FX4 BN2)))

(define NEG-RN13		(- (/ FX1 BN3)))
(define NEG-RN14		(- (/ FX2 BN3)))
(define NEG-RN15		(- (/ FX3 BN3)))
(define NEG-RN16		(- (/ FX4 BN3)))

(define NEG-RN17		(- (/ FX1 BN4)))
(define NEG-RN18		(- (/ FX2 BN4)))
(define NEG-RN19		(- (/ FX3 BN4)))
(define NEG-RN20		(- (/ FX4 BN4)))

;;(define NEG-RN21		536870912		#;(/ BN1 FX1)) ;not a ratnum
;;(define NEG-RN22		536871011		#;(/ BN2 FX1)) ;not a ratnum
;;(define NEG-RN23		-536870913		#;(/ BN3 FX1)) ;not a ratnum
;;(define NEG-RN24		-536871012		#;(/ BN4 FX1)) ;not a ratnum

;;(define NEG-RN25		-536870912		#;(/ BN1 FX2)) ;not a ratnum
;;(define NEG-RN26		-536871011		#;(/ BN2 FX2)) ;not a ratnum
;;(define NEG-RN27		536870913		#;(/ BN3 FX2)) ;not a ratnum
;;(define NEG-RN28		536871012		#;(/ BN4 FX2)) ;not a ratnum

(define NEG-RN29		(- (/ BN1 FX3)))
(define NEG-RN30		(- (/ BN2 FX3)))
(define NEG-RN31		(- (/ BN3 FX3)))
(define NEG-RN32		(- (/ BN4 FX3)))

;;(define NEG-RN33		-1			#;(/ BN1 FX4)) ;not a ratnum
(define NEG-RN34		(- (/ BN2 FX4)))
(define NEG-RN35		(- (/ BN3 FX4)))
(define NEG-RN36		(- (/ BN4 FX4)))

(define NEG-VRN01		(- (/ FX1 123)))
(define NEG-VRN02		(- (/ FX2 123)))
(define NEG-VRN03		(- (/ FX2 123)))
(define NEG-VRN04		(- (/ FX4 123)))

(define NEG-VRN05		(- (/ FX1 VBN1)))
(define NEG-VRN06		(- (/ FX2 VBN1)))
(define NEG-VRN07		(- (/ FX3 VBN1)))
;;;(define NEG-VRN08		(- (/ FX4 VBN1))) ;= 1, not a ratnum

(define NEG-VRN09		(- (/ FX1 VBN2)))
(define NEG-VRN10		(- (/ FX2 VBN2)))
(define NEG-VRN11		(- (/ FX3 VBN2)))
(define NEG-VRN12		(- (/ FX4 VBN2)))

(define NEG-VRN13		(- (/ FX1 VBN3)))
(define NEG-VRN14		(- (/ FX2 VBN3)))
(define NEG-VRN15		(- (/ FX3 VBN3)))
(define NEG-VRN16		(- (/ FX4 VBN3)))

(define NEG-VRN17		(- (/ FX1 VBN4)))
(define NEG-VRN18		(- (/ FX2 VBN4)))
(define NEG-VRN19		(- (/ FX3 VBN4)))
(define NEG-VRN20		(- (/ FX4 VBN4)))

;;(define NEG-VRN21		536870912		#;(/ VBN1 FX1)) ;not a ratnum
;;(define NEG-VRN22		536871011		#;(/ VBN2 FX1)) ;not a ratnum
;;(define NEG-VRN23		-536870913		#;(/ VBN3 FX1)) ;not a ratnum
;;(define NEG-VRN24		-536871012		#;(/ VBN4 FX1)) ;not a ratnum

;;(define NEG-VRN25		-536870912		#;(/ VBN1 FX2)) ;not a ratnum
;;(define NEG-VRN26		-536871011		#;(/ VBN2 FX2)) ;not a ratnum
;;(define NEG-VRN27		536870913		#;(/ VBN3 FX2)) ;not a ratnum
;;(define NEG-VRN28		536871012		#;(/ VBN4 FX2)) ;not a ratnum

(define NEG-VRN29		(- (/ VBN1 FX3)))
(define NEG-VRN30		(- (/ VBN2 FX3)))
(define NEG-VRN31		(- (/ VBN3 FX3)))
(define NEG-VRN32		(- (/ VBN4 FX3)))

;;(define NEG-VRN33		-1			#;(/ VBN1 FX4)) ;not a ratnum
(define NEG-VRN34		(- (/ VBN2 FX4)))
(define NEG-VRN35		(- (/ VBN3 FX4)))
(define NEG-VRN36		(- (/ VBN4 FX4)))

;;; --------------------------------------------------------------------

(define NEG-FL1		(- +0.0))
(define NEG-FL2		(- -0.0))
(define NEG-FL3		(- +2.123))
(define NEG-FL4		(- -2.123))
(define NEG-FL5		(- +inf.0))
(define NEG-FL6		(- -inf.0))
(define NEG-FL7		(- +nan.0))

;;; --------------------------------------------------------------------

(define NEG-CFL01		(- +0.0+0.0i))
(define NEG-CFL02		(- -0.0+0.0i))
(define NEG-CFL03		(- +0.0-0.0i))
(define NEG-CFL04		(- -0.0-0.0i))

(define NEG-CFL05		(- -1.2-0.0i))
(define NEG-CFL06		(- -1.2+0.0i))
(define NEG-CFL07		(- +0.0-1.2i))
(define NEG-CFL08		(- -0.0-1.2i))

(define NEG-CFL09		(- -1.2-inf.0i))
(define NEG-CFL10		(- -1.2+inf.0i))
(define NEG-CFL11		(- +inf.0-1.2i))
(define NEG-CFL12		(- -inf.0-1.2i))

(define NEG-CFL13		(- -1.2-nan.0i))
(define NEG-CFL14		(- -1.2+nan.0i))
(define NEG-CFL15		(- +nan.0-1.2i))
(define NEG-CFL16		(- -nan.0-1.2i))


;;;; negated compnum constants

(define NEG-CN001		(make-rectangular NEG-FX1 NEG-FX1))
(define NEG-CN002		(make-rectangular NEG-FX2 NEG-FX1))
(define NEG-CN003		(make-rectangular NEG-FX3 NEG-FX1))
(define NEG-CN004		(make-rectangular NEG-FX4 NEG-FX1))

(define NEG-CN005		(make-rectangular NEG-FX1 NEG-FX2))
(define NEG-CN006		(make-rectangular NEG-FX2 NEG-FX2))
(define NEG-CN007		(make-rectangular NEG-FX3 NEG-FX2))
(define NEG-CN008		(make-rectangular NEG-FX4 NEG-FX2))

(define NEG-CN009		(make-rectangular NEG-FX1 NEG-FX3))
(define NEG-CN010		(make-rectangular NEG-FX2 NEG-FX3))
(define NEG-CN011		(make-rectangular NEG-FX3 NEG-FX3))
(define NEG-CN012		(make-rectangular NEG-FX4 NEG-FX3))

(define NEG-CN013		(make-rectangular NEG-FX1 NEG-FX4))
(define NEG-CN014		(make-rectangular NEG-FX2 NEG-FX4))
(define NEG-CN015		(make-rectangular NEG-FX3 NEG-FX4))
(define NEG-CN016		(make-rectangular NEG-FX4 NEG-FX4))

;;; --------------------------------------------------------------------

(define NEG-CN017		(make-rectangular NEG-BN1 NEG-FX1))
(define NEG-CN018		(make-rectangular NEG-BN2 NEG-FX1))
(define NEG-CN019		(make-rectangular NEG-BN3 NEG-FX1))
(define NEG-CN020		(make-rectangular NEG-BN4 NEG-FX1))

(define NEG-CN021		(make-rectangular NEG-BN1 NEG-FX2))
(define NEG-CN022		(make-rectangular NEG-BN2 NEG-FX2))
(define NEG-CN023		(make-rectangular NEG-BN3 NEG-FX2))
(define NEG-CN024		(make-rectangular NEG-BN4 NEG-FX2))

(define NEG-CN025		(make-rectangular NEG-BN1 NEG-FX3))
(define NEG-CN026		(make-rectangular NEG-BN2 NEG-FX3))
(define NEG-CN027		(make-rectangular NEG-BN3 NEG-FX3))
(define NEG-CN028		(make-rectangular NEG-BN4 NEG-FX3))

(define NEG-CN029		(make-rectangular NEG-BN1 NEG-FX4))
(define NEG-CN030		(make-rectangular NEG-BN2 NEG-FX4))
(define NEG-CN031		(make-rectangular NEG-BN3 NEG-FX4))
(define NEG-CN032		(make-rectangular NEG-BN4 NEG-FX4))

;;; --------------------------------------------------------------------

(define NEG-CN033		(make-rectangular NEG-FX1 NEG-BN1))
(define NEG-CN034		(make-rectangular NEG-FX2 NEG-BN1))
(define NEG-CN035		(make-rectangular NEG-FX3 NEG-BN1))
(define NEG-CN036		(make-rectangular NEG-FX4 NEG-BN1))

(define NEG-CN037		(make-rectangular NEG-FX1 NEG-BN2))
(define NEG-CN038		(make-rectangular NEG-FX2 NEG-BN2))
(define NEG-CN039		(make-rectangular NEG-FX3 NEG-BN2))
(define NEG-CN040		(make-rectangular NEG-FX4 NEG-BN2))

(define NEG-CN041		(make-rectangular NEG-FX1 NEG-BN3))
(define NEG-CN042		(make-rectangular NEG-FX2 NEG-BN3))
(define NEG-CN043		(make-rectangular NEG-FX3 NEG-BN3))
(define NEG-CN044		(make-rectangular NEG-FX4 NEG-BN3))

(define NEG-CN045		(make-rectangular NEG-FX1 NEG-BN4))
(define NEG-CN046		(make-rectangular NEG-FX2 NEG-BN4))
(define NEG-CN047		(make-rectangular NEG-FX3 NEG-BN4))
(define NEG-CN048		(make-rectangular NEG-FX4 NEG-BN4))

;;; --------------------------------------------------------------------

(define NEG-CN049		(make-rectangular NEG-BN1 NEG-FL1))
(define NEG-CN050		(make-rectangular NEG-BN2 NEG-FL1))
(define NEG-CN051		(make-rectangular NEG-BN3 NEG-FL1))
(define NEG-CN052		(make-rectangular NEG-BN4 NEG-FL1))

(define NEG-CN053		(make-rectangular NEG-BN1 NEG-FL2))
(define NEG-CN054		(make-rectangular NEG-BN2 NEG-FL2))
(define NEG-CN055		(make-rectangular NEG-BN3 NEG-FL2))
(define NEG-CN056		(make-rectangular NEG-BN4 NEG-FL2))

(define NEG-CN057		(make-rectangular NEG-BN1 NEG-FL3))
(define NEG-CN058		(make-rectangular NEG-BN2 NEG-FL3))
(define NEG-CN059		(make-rectangular NEG-BN3 NEG-FL3))
(define NEG-CN060		(make-rectangular NEG-BN4 NEG-FL3))

(define NEG-CN061		(make-rectangular NEG-BN1 NEG-FL4))
(define NEG-CN062		(make-rectangular NEG-BN2 NEG-FL4))
(define NEG-CN063		(make-rectangular NEG-BN3 NEG-FL4))
(define NEG-CN064		(make-rectangular NEG-BN4 NEG-FL4))

;;; --------------------------------------------------------------------

(define NEG-CN065		(make-rectangular NEG-FL1 NEG-BN1))
(define NEG-CN066		(make-rectangular NEG-FL2 NEG-BN1))
(define NEG-CN067		(make-rectangular NEG-FL3 NEG-BN1))
(define NEG-CN068		(make-rectangular NEG-FL4 NEG-BN1))

(define NEG-CN069		(make-rectangular NEG-FL1 NEG-BN2))
(define NEG-CN070		(make-rectangular NEG-FL2 NEG-BN2))
(define NEG-CN071		(make-rectangular NEG-FL3 NEG-BN2))
(define NEG-CN072		(make-rectangular NEG-FL4 NEG-BN2))

(define NEG-CN073		(make-rectangular NEG-FL1 NEG-BN3))
(define NEG-CN074		(make-rectangular NEG-FL2 NEG-BN3))
(define NEG-CN075		(make-rectangular NEG-FL3 NEG-BN3))
(define NEG-CN076		(make-rectangular NEG-FL4 NEG-BN3))

(define NEG-CN077		(make-rectangular NEG-FL1 NEG-BN4))
(define NEG-CN078		(make-rectangular NEG-FL2 NEG-BN4))
(define NEG-CN079		(make-rectangular NEG-FL3 NEG-BN4))
(define NEG-CN080		(make-rectangular NEG-FL4 NEG-BN4))

;;; --------------------------------------------------------------------

(define NEG-CN081		(make-rectangular NEG-RN01 NEG-FL1))
(define NEG-CN082		(make-rectangular NEG-RN02 NEG-FL1))
(define NEG-CN083		(make-rectangular NEG-RN03 NEG-FL1))
(define NEG-CN084		(make-rectangular NEG-RN04 NEG-FL1))

(define NEG-CN085		(make-rectangular NEG-RN01 NEG-FL2))
(define NEG-CN086		(make-rectangular NEG-RN02 NEG-FL2))
(define NEG-CN087		(make-rectangular NEG-RN03 NEG-FL2))
(define NEG-CN088		(make-rectangular NEG-RN04 NEG-FL2))

(define NEG-CN089		(make-rectangular NEG-RN01 NEG-FL3))
(define NEG-CN090		(make-rectangular NEG-RN02 NEG-FL3))
(define NEG-CN091		(make-rectangular NEG-RN03 NEG-FL3))
(define NEG-CN092		(make-rectangular NEG-RN04 NEG-FL3))

(define NEG-CN093		(make-rectangular NEG-RN01 NEG-FL4))
(define NEG-CN094		(make-rectangular NEG-RN02 NEG-FL4))
(define NEG-CN095		(make-rectangular NEG-RN03 NEG-FL4))
(define NEG-CN096		(make-rectangular NEG-RN04 NEG-FL4))

;;; --------------------------------------------------------------------

(define NEG-CN097		(make-rectangular NEG-FL1 NEG-RN01))
(define NEG-CN098		(make-rectangular NEG-FL2 NEG-RN01))
(define NEG-CN099		(make-rectangular NEG-FL3 NEG-RN01))
(define NEG-CN100		(make-rectangular NEG-FL4 NEG-RN01))

(define NEG-CN101		(make-rectangular NEG-FL1 NEG-RN02))
(define NEG-CN102		(make-rectangular NEG-FL2 NEG-RN02))
(define NEG-CN103		(make-rectangular NEG-FL3 NEG-RN02))
(define NEG-CN104		(make-rectangular NEG-FL4 NEG-RN02))

(define NEG-CN105		(make-rectangular NEG-FL1 NEG-RN03))
(define NEG-CN106		(make-rectangular NEG-FL2 NEG-RN03))
(define NEG-CN107		(make-rectangular NEG-FL3 NEG-RN03))
(define NEG-CN108		(make-rectangular NEG-FL4 NEG-RN03))

(define NEG-CN109		(make-rectangular NEG-FL1 NEG-RN04))
(define NEG-CN110		(make-rectangular NEG-FL2 NEG-RN04))
(define NEG-CN111		(make-rectangular NEG-FL3 NEG-RN04))
(define NEG-CN112		(make-rectangular NEG-FL4 NEG-RN04))


;;;; inverse constants

(define INV-GREATEST-FX		-536870911)
(define INV-LEAST-FX		+536870912)

;;; --------------------------------------------------------------------

(define INV-FX1		+1)
(define INV-FX2		-1)
(define INV-FX3		+1/536870911 #;(/ GREATEST-FX))
(define INV-FX4		-1/536870912 #;(/ INV-LEAST-FX))

;;; --------------------------------------------------------------------

(define INV-BN1		+1/536870912)
(define INV-BN2		+1/536871011)
(define INV-BN3		-1/536870913)
(define INV-BN4		-1/536871012)

(define INV-VBN1	(/ 1 (+ GREATEST-FX-64-bit +1)))
(define INV-VBN2	(/ 1 (+ GREATEST-FX-64-bit +100)))
(define INV-VBN3	(/ 1 (+ LEAST-FX-64-bit -1)))
(define INV-VBN4	(/ 1 (+ LEAST-FX-64-bit -100)))

;;; --------------------------------------------------------------------

(define INV-RN01	123			#;(/ INV-FX1 123))
(define INV-RN02	-123			#;(/ INV-FX2 123))
(define INV-RN03	-123			#;(/ INV-FX2 123))
(define INV-RN04	-123/536870912		#;(/ INV-FX4 123))

(define INV-RN05	536870912		#;(/ INV-FX1 BN1))
(define INV-RN06	-536870912		#;(/ INV-FX2 BN1))
(define INV-RN07	536870912/536870911	#;(/ INV-FX3 BN1))
;;(define INV-RN08	1			#;(/ INV-FX4 BN1)) ;not a ratnum

(define INV-RN09	536871011		#;(/ INV-FX1 BN2))
(define INV-RN10	-536871011		#;(/ INV-FX2 BN2))
(define INV-RN11	536871011/536870911	#;(/ INV-FX3 BN2))
(define INV-RN12	-536871011/536870912	#;(/ INV-FX4 BN2))

(define INV-RN13	-536870913		#;(/ INV-FX1 BN3))
(define INV-RN14	536870913		#;(/ INV-FX2 BN3))
(define INV-RN15	-536870913/536870911	#;(/ INV-FX3 BN3))
(define INV-RN16	536870913/536870912	#;(/ INV-FX4 BN3))

(define INV-RN17	-536871012		#;(/ INV-FX1 BN4))
(define INV-RN18	536871012		#;(/ INV-FX2 BN4))
(define INV-RN19	-536871012/536870911	#;(/ INV-FX3 BN4))
(define INV-RN20	134217753/134217728	#;(/ INV-FX4 BN4))

;;(define INV-RN21	-536870912		#;(/ INV-BN1 FX1)) ;not a ratnum
;;(define INV-RN22	-536871011		#;(/ INV-BN2 FX1)) ;not a ratnum
;;(define INV-RN23	536870913		#;(/ INV-BN3 FX1)) ;not a ratnum
;;(define INV-RN24	536871012		#;(/ INV-BN4 FX1)) ;not a ratnum

;;(define INV-RN25	536870912		#;(/ INV-BN1 FX2)) ;not a ratnum
;;(define INV-RN26	536871011		#;(/ INV-BN2 FX2)) ;not a ratnum
;;(define INV-RN27	-536870913		#;(/ INV-BN3 FX2)) ;not a ratnum
;;(define INV-RN28	-536871012		#;(/ INV-BN4 FX2)) ;not a ratnum

(define INV-RN29	536870911/536870912	#;(/ INV-BN1 FX3))
(define INV-RN30	536870911/536871011	#;(/ INV-BN2 FX3))
(define INV-RN31	-536870911/536870913	#;(/ INV-BN3 FX3))
(define INV-RN32	-536870911/536871012	#;(/ INV-BN4 FX3))

;;(define INV-RN33	-1			#;(/ INV-BN1 FX4)) ;not a ratnum
(define INV-RN34	-536870912/536871011	#;(/ INV-BN2 FX4))
(define INV-RN35	536870912/536870913	#;(/ INV-BN3 FX4))
(define INV-RN36	134217728/134217753	#;(/ INV-BN4 FX4))

;;; --------------------------------------------------------------------

(define INV-VRN01		(/ 123 FX1))
(define INV-VRN02		(/ 123 FX2))
(define INV-VRN03		(/ 123 FX2))
(define INV-VRN04		(/ 123 FX4))

(define INV-VRN05		(/ VBN1 FX1))
(define INV-VRN06		(/ VBN1 FX2))
(define INV-VRN07		(/ VBN1 FX3))

(define INV-VRN09		(/ VBN2 FX1))
(define INV-VRN10		(/ VBN2 FX2))
(define INV-VRN11		(/ VBN2 FX3))
(define INV-VRN12		(/ VBN2 FX4))

(define INV-VRN13		(/ VBN3 FX1))
(define INV-VRN14		(/ VBN3 FX2))
(define INV-VRN15		(/ VBN3 FX3))
(define INV-VRN16		(/ VBN3 FX4))

(define INV-VRN17		(/ VBN4 FX1))
(define INV-VRN18		(/ VBN4 FX2))
(define INV-VRN19		(/ VBN4 FX3))
(define INV-VRN20		(/ VBN4 FX4))

(define INV-VRN29		(/ FX3 VBN1))
(define INV-VRN30		(/ FX3 VBN2))
(define INV-VRN31		(/ FX3 VBN3))
(define INV-VRN32		(/ FX3 VBN4))

(define INV-VRN34		(/ FX4 VBN2))
(define INV-VRN35		(/ FX4 VBN3))
(define INV-VRN36		(/ FX4 VBN4))

;;; --------------------------------------------------------------------

(define INV-FL1		+inf.0)
(define INV-FL2		-inf.0)
(define INV-FL3		0.4710315591144606)
(define INV-FL4		-0.4710315591144606)
(define INV-FL5		+0.0)
(define INV-FL6		-0.0)
(define INV-FL7		+nan.0)

;;; --------------------------------------------------------------------

(define INV-CFL01	+nan.0+nan.0i)
(define INV-CFL02	+nan.0+nan.0i)
(define INV-CFL03	+nan.0+nan.0i)
(define INV-CFL04	+nan.0+nan.0i)

(define INV-CFL05	-0.8333333333333333+0.0i)
(define INV-CFL06	-0.8333333333333333-0.0i)
(define INV-CFL07	0.0+0.8333333333333333i)
(define INV-CFL08	-0.0+0.8333333333333333i)

(define INV-CFL09	-0.0+nan.0i)
(define INV-CFL10	-0.0+nan.0i)
(define INV-CFL11	+nan.0+0.0i)
(define INV-CFL12	+nan.0+0.0i)

(define INV-CFL13	+nan.0+nan.0i)
(define INV-CFL14	+nan.0+nan.0i)
(define INV-CFL15	+nan.0+nan.0i)
(define INV-CFL16	+nan.0+nan.0i)


;;;; inverse compnum constants

(define INV-CN001 1/2-1/2i)
(define INV-CN002 -1/2-1/2i)
(define INV-CN003 536870911/288230375077969922-1/288230375077969922i)
(define INV-CN004 -536870912/288230376151711745-1/288230376151711745i)
(define INV-CN005 1/2+1/2i)
(define INV-CN006 -1/2+1/2i)
(define INV-CN007 536870911/288230375077969922+1/288230375077969922i)
(define INV-CN008 -536870912/288230376151711745+1/288230376151711745i)
(define INV-CN009 1/288230375077969922-536870911/288230375077969922i)
(define INV-CN010 -1/288230375077969922-536870911/288230375077969922i)
(define INV-CN011 1/1073741822-1/1073741822i)
(define INV-CN012 -536870912/576460751229681665-536870911/576460751229681665i)
(define INV-CN013 1/288230376151711745+536870912/288230376151711745i)
(define INV-CN014 -1/288230376151711745+536870912/288230376151711745i)
(define INV-CN015 536870911/576460751229681665+536870912/576460751229681665i)
(define INV-CN016 -1/1073741824+1/1073741824i)
(define INV-CN017 536870912/288230376151711745-1/288230376151711745i)
(define INV-CN018 536871011/288230482452162122-1/288230482452162122i)
(define INV-CN019 -536870913/288230377225453570-1/288230377225453570i)
(define INV-CN020 -536871012/288230483525904145-1/288230483525904145i)
(define INV-CN021 536870912/288230376151711745+1/288230376151711745i)
(define INV-CN022 536871011/288230482452162122+1/288230482452162122i)
(define INV-CN023 -536870913/288230377225453570+1/288230377225453570i)
(define INV-CN024 -536871012/288230483525904145+1/288230483525904145i)
(define INV-CN025 536870912/576460751229681665-536870911/576460751229681665i)
(define INV-CN026 536871011/576460857530132042-536870911/576460857530132042i)
(define INV-CN027 -536870913/576460752303423490-536870911/576460752303423490i)
(define INV-CN028 -536871012/576460858603874065-536870911/576460858603874065i)
(define INV-CN029 1/1073741824+1/1073741824i)
(define INV-CN030 536871011/576460858603873865+536870912/576460858603873865i)
(define INV-CN031 -536870913/576460753377165313+536870912/576460753377165313i)
(define INV-CN032 -134217753/144115214919403972+33554432/36028803729850993i)
(define INV-CN033 1/288230376151711745-536870912/288230376151711745i)
(define INV-CN034 -1/288230376151711745-536870912/288230376151711745i)
(define INV-CN035 536870911/576460751229681665-536870912/576460751229681665i)
(define INV-CN036 -1/1073741824-1/1073741824i)
(define INV-CN037 1/288230482452162122-536871011/288230482452162122i)
(define INV-CN038 -1/288230482452162122-536871011/288230482452162122i)
(define INV-CN039 536870911/576460857530132042-536871011/576460857530132042i)
(define INV-CN040 -536870912/576460858603873865-536871011/576460858603873865i)
(define INV-CN041 1/288230377225453570+536870913/288230377225453570i)
(define INV-CN042 -1/288230377225453570+536870913/288230377225453570i)
(define INV-CN043 536870911/576460752303423490+536870913/576460752303423490i)
(define INV-CN044 -536870912/576460753377165313+536870913/576460753377165313i)
(define INV-CN045 1/288230483525904145+536871012/288230483525904145i)
(define INV-CN046 -1/288230483525904145+536871012/288230483525904145i)
(define INV-CN047 536870911/576460858603874065+536871012/576460858603874065i)
(define INV-CN048 -33554432/36028803729850993+134217753/144115214919403972i)
(define INV-CN049 1.862645149230957e-9-0.0i)
(define INV-CN050 1.862644805755772e-9-0.0i)
(define INV-CN051 -1.86264514576151e-9-0.0i)
(define INV-CN052 -1.8626448022863266e-9-0.0i)
(define INV-CN053 1.862645149230957e-9+0.0i)
(define INV-CN054 1.862644805755772e-9+0.0i)
(define INV-CN055 -1.86264514576151e-9+0.0i)
(define INV-CN056 -1.8626448022863266e-9+0.0i)
(define INV-CN057 1.862645149230957e-9-7.365635878997524e-18i)
(define INV-CN058 1.862644805755772e-9-7.365633162524219e-18i)
(define INV-CN059 -1.86264514576151e-9-7.365635851558391e-18i)
(define INV-CN060 -1.8626448022863266e-9-7.365633135085102e-18i)
(define INV-CN061 1.862645149230957e-9+7.365635878997524e-18i)
(define INV-CN062 1.862644805755772e-9+7.365633162524219e-18i)
(define INV-CN063 -1.86264514576151e-9+7.365635851558391e-18i)
(define INV-CN064 -1.8626448022863266e-9+7.365633135085102e-18i)
(define INV-CN065 0.0-1.862645149230957e-9i)
(define INV-CN066 -0.0-1.862645149230957e-9i)
(define INV-CN067 7.365635878997524e-18-1.862645149230957e-9i)
(define INV-CN068 -7.365635878997524e-18-1.862645149230957e-9i)
(define INV-CN069 0.0-1.862644805755772e-9i)
(define INV-CN070 -0.0-1.862644805755772e-9i)
(define INV-CN071 7.365633162524219e-18-1.862644805755772e-9i)
(define INV-CN072 -7.365633162524219e-18-1.862644805755772e-9i)
(define INV-CN073 0.0+1.86264514576151e-9i)
(define INV-CN074 -0.0+1.86264514576151e-9i)
(define INV-CN075 7.365635851558391e-18+1.86264514576151e-9i)
(define INV-CN076 -7.365635851558391e-18+1.86264514576151e-9i)
(define INV-CN077 0.0+1.8626448022863266e-9i)
(define INV-CN078 -0.0+1.8626448022863266e-9i)
(define INV-CN079 7.365633135085102e-18+1.8626448022863266e-9i)
(define INV-CN080 -7.365633135085102e-18+1.8626448022863266e-9i)
(define INV-CN081 123.0-0.0i)
(define INV-CN082 -123.0-0.0i)
(define INV-CN083 -123.0-0.0i)
(define INV-CN084 -2.2910535335540771e-7-0.0i)
(define INV-CN085 123.0+0.0i)
(define INV-CN086 -123.0+0.0i)
(define INV-CN087 -123.0+0.0i)
(define INV-CN088 -2.2910535335540771e-7+0.0i)
(define INV-CN089 0.0018038006173773664-0.47102465141513433i)
(define INV-CN090 -0.0018038006173773664-0.47102465141513433i)
(define INV-CN091 -0.0018038006173773664-0.47102465141513433i)
(define INV-CN092 -2.291053533553535e-7-1.1143470521332715e-13i)
(define INV-CN093 0.0018038006173773664+0.47102465141513433i)
(define INV-CN094 -0.0018038006173773664+0.47102465141513433i)
(define INV-CN095 -0.0018038006173773664+0.47102465141513433i)
(define INV-CN096 -2.291053533553535e-7+1.1143470521332715e-13i)
(define INV-CN097 0.0-123.0i)
(define INV-CN098 -0.0-123.0i)
(define INV-CN099 0.47102465141513433-0.0018038006173773664i)
(define INV-CN100 -0.47102465141513433-0.0018038006173773664i)
(define INV-CN101 0.0+123.0i)
(define INV-CN102 -0.0+123.0i)
(define INV-CN103 0.47102465141513433+0.0018038006173773664i)
(define INV-CN104 -0.47102465141513433+0.0018038006173773664i)
(define INV-CN105 0.0+123.0i)
(define INV-CN106 -0.0+123.0i)
(define INV-CN107 0.47102465141513433+0.0018038006173773664i)
(define INV-CN108 -0.47102465141513433+0.0018038006173773664i)
(define INV-CN109 0.0+2.2910535335540771e-7i)
(define INV-CN110 -0.0+2.2910535335540771e-7i)
(define INV-CN111 1.1143470521332715e-13+2.291053533553535e-7i)
(define INV-CN112 -1.1143470521332715e-13+2.291053533553535e-7i)


;;;; done

)

;;; end of file
