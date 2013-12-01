;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the destructuring match library
;;;Date: Sat Apr 20, 2013
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


#!vicare
(import (nausicaa)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa: destructuring match syntax\n")


(parametrise ((check-test-name	'wildcard))

  (check
      (match #t
        (_	#\1)
	(else	#f))
    => #\1)

  (check
      (match '(1 2 3)
        (_	#\1)
	(else	#f))
    => #\1)

  (check
      (match "ciao"
        (_	#\1)
	(else	#f))
    => #\1)

;;; --------------------------------------------------------------------

  (check
      (match 1
        (2	#\A)
        (_	#\B)
	(else	#f))
    => #\B)

;;; --------------------------------------------------------------------

  (check
      (match '(#t)
        ((_)	#\1)
	(else	#f))
    => #\1)

  (check
      (match #t
        ((_)	#\1)
	(else	#f))
    => #f)

  (check
      (match '(1 2 3)
        ((_ _ _)	#\1)
	(else		#f))
    => #\1)

  (check
      (match '(1 2 3)
        ((_ 2 _)	#\1)
	(else		#f))
    => #\1)

  (check
      (match '(1 99 3)
        ((_ 2 _)	#\1)
	(else		#f))
    => #f)

  (check
      (match '((((1))))
        (((((_))))	#\1)
	(else		#f))
    => #\1)

  #t)


(parametrise ((check-test-name	'booleans))

  (check
      (match #t
        (#t	#\1)
        (#f	#\2)
	(else	#f))
    => #\1)

  (check
      (match #f
        (#t	#\1)
        (#f	#\2)
	(else	#f))
    => #\2)

  (check
      (match 1
        (#t	#\1)
        (#f	#\2)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'chars))

  (check
      (match #\A
        (#\A	#\1)
        (#\B	#\2)
	(else	#f))
    => #\1)

  (check
      (match #\B
        (#\A	#\1)
        (#\B	#\2)
	(else	#f))
    => #\2)

  (check
      (match 1
        (#\A	#\1)
        (#\B	#\2)
	(else	#f))
    => #f)

  #\A)


(parametrise ((check-test-name	'fixnums))

  (check
      (match 123
        (123	#\1)
        (456	#\2)
        (789	#\3)
	(else	#f))
    => #\1)

  (check
      (match 456
        (123	#\1)
        (456	#\2)
        (789	#\3)
	(else	#f))
    => #\2)

  (check
      (match 789
        (123	#\1)
        (456	#\2)
        (789	#\3)
	(else	#f))
    => #\3)

  (check
      (match 0
        (123	#\1)
        (456	#\2)
        (789	#\3)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'bignums))

  (check
      (match #e123e10
        (#e123e10	#\1)
        (#e456e10	#\2)
        (#e789e10	#\3)
	(else	#f))
    => #\1)

  (check
      (match #e456e10
        (#e123e10	#\1)
        (#e456e10	#\2)
        (#e789e10	#\3)
	(else	#f))
    => #\2)

  (check
      (match #e789e10
        (#e123e10	#\1)
        (#e456e10	#\2)
        (#e789e10	#\3)
	(else	#f))
    => #\3)

  (check
      (match #e1000e10
        (#e123e10	#\1)
        (#e456e10	#\2)
        (#e789e10	#\3)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (check
      (match 1.23
        (1.23	#\1)
        (4.56	#\2)
        (7.89	#\3)
	(else	#f))
    => #\1)

  (check
      (match 4.56
        (1.23	#\1)
        (4.56	#\2)
        (7.89	#\3)
	(else	#f))
    => #\2)

  (check
      (match 7.89
        (1.23	#\1)
        (4.56	#\2)
        (7.89	#\3)
	(else	#f))
    => #\3)

  (check
      (match 0.0
        (1.23	#\1)
        (4.56	#\2)
        (7.89	#\3)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (check
      (match 1/23
        (1/23	#\1)
        (4/56	#\2)
        (7/89	#\3)
	(else	#f))
    => #\1)

  (check
      (match 4/56
        (1/23	#\1)
        (4/56	#\2)
        (7/89	#\3)
	(else	#f))
    => #\2)

  (check
      (match 7/89
        (1/23	#\1)
        (4/56	#\2)
        (7/89	#\3)
	(else	#f))
    => #\3)

  (check
      (match 8/9
        (1/23	#\1)
        (4/56	#\2)
        (7/89	#\3)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'cflonums))

  (check
      (match 1.23+1.24i
        (1.23+1.24i	#\1)
        (4.56+4.57i	#\2)
        (7.89+7.88i	#\3)
	(else	#f))
    => #\1)

  (check
      (match 4.56+4.57i
        (1.23+1.24i	#\1)
        (4.56+4.57i	#\2)
        (7.89+7.88i	#\3)
	(else	#f))
    => #\2)

  (check
      (match 7.89+7.88i
        (1.23+1.24i	#\1)
        (4.56+4.57i	#\2)
        (7.89+7.88i	#\3)
	(else	#f))
    => #\3)

  (check
      (match 0.0+0.0i
        (1.23+1.24i	#\1)
        (4.56+4.57i	#\2)
        (7.89+7.88i	#\3)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'compnums))

  (check
      (match 123+124i
        (123+124i	#\1)
        (456+457i	#\2)
        (789+788i	#\3)
	(else	#f))
    => #\1)

  (check
      (match 456+457i
        (123+124i	#\1)
        (456+457i	#\2)
        (789+788i	#\3)
	(else	#f))
    => #\2)

  (check
      (match 789+788i
        (123+124i	#\1)
        (456+457i	#\2)
        (789+788i	#\3)
	(else	#f))
    => #\3)

  (check
      (match 1+2i
        (123+124i	#\1)
        (456+457i	#\2)
        (789+788i	#\3)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'numbers))

  (check
      (match +nan.0
        (+nan.0	#\1)
        (+inf.0	#\2)
        (-inf.0	#\3)
	(else	#f))
    => #\1)

  (check
      (match +inf.0
        (+nan.0	#\1)
        (+inf.0	#\2)
        (-inf.0	#\3)
	(else	#f))
    => #\2)

  (check
      (match -inf.0
        (+nan.0	#\1)
        (+inf.0	#\2)
        (-inf.0	#\3)
	(else	#f))
    => #\3)

  (check
      (match 0
        (+nan.0	#\1)
        (+inf.0	#\2)
        (-inf.0	#\3)
	(else	#f))
    => #f)

  #t)


(parametrise ((check-test-name	'quoted-symbols))

  (check
      (match 'ciao
        ('ciao		#\1)
        ('hello		#\2)
        ('salut		#\2)
	(else		#f))
    => #\1)

  (check
      (match 'hello
        ('ciao		#\1)
        ('hello		#\2)
        ('salut		#\3)
	(else		#f))
    => #\2)

  (check
      (match 'salut
        ('ciao		#\1)
        ('hello		#\2)
        ('salut		#\3)
	(else		#f))
    => #\3)

  (check
      (match 'hey
        ('ciao		#\1)
        ('hello		#\2)
        ('salut		#\3)
	(else		#f))
    => #f)

  #t)


(parametrise ((check-test-name	'strings))

  (check
      (match "ciao"
        ("ciao"		#\1)
        ("hello"	#\2)
        (""		#\3)
	(else		#f))
    => #\1)

  (check
      (match "hello"
        ("ciao"		#\1)
        ("hello"	#\2)
        (""		#\3)
	(else		#f))
    => #\2)

  (check
      (match ""
        ("ciao"		#\1)
        ("hello"	#\2)
        (""		#\3)
	(else		#f))
    => #\3)

  (check
      (match "salut"
        ("ciao"		#\1)
        ("hello"	#\2)
        (""		#\3)
	(else		#f))
    => #f)

  #t)


(parametrise ((check-test-name	'bytevectors))

  (check
      (match '#ve(ascii "ciao")
        (#ve(ascii "ciao")	#\1)
        (#ve(ascii "hello")	#\2)
        (#ve(ascii "")		#\3)
	(else			#f))
    => #\1)

  (check
      (match '#ve(ascii "hello")
        (#ve(ascii "ciao")	#\1)
        (#ve(ascii "hello")	#\2)
        (#ve(ascii "")		#\3)
	(else			#f))
    => #\2)

  (check
      (match '#ve(ascii "")
        (#ve(ascii "ciao")	#\1)
        (#ve(ascii "hello")	#\2)
        (#ve(ascii "")		#\3)
	(else			#f))
    => #\3)

  (check
      (match "salut"
        (#ve(ascii "ciao")	#\1)
        (#ve(ascii "hello")	#\2)
        (#ve(ascii "")		#\3)
	(else			#f))
    => #f)

  #t)


(parametrise ((check-test-name	'pairs))

  (check
      (match '()
        ((1)	#\1)
        (()	#\0)
        ((4)	#\2)
        ((7)	#\3)
  	(else	#f))
    => #\0)

  (check
      (match '()
        ((1)	#\1)
        ((4)	#\2)
        ((7)	#\3)
  	(else	#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '(1)
        ((1)	#\1)
        ((4)	#\2)
	(()	#\0)
        ((7)	#\3)
  	(else	#f))
    => #\1)

  (check
      (match '(4)
        ((1)	#\1)
	(()	#\0)
        ((4)	#\2)
        ((7)	#\3)
  	(else	#f))
    => #\2)

  (check
      (match '(7)
        ((1)	#\1)
        ((4)	#\2)
        ((7)	#\3)
  	(else	#f))
    => #\3)

  (check
      (match '(0)
        ((1)	#\1)
        ((4)	#\2)
	(()	#\0)
        ((7)	#\3)
  	(else	#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '(1 2 3)
        ((1 2 3)	#\1)
        ((4 5 6)	#\2)
        ((7 8 9)	#\3)
  	(else		#f))
    => #\1)

  (check
      (match '(1 2)
        ((1 2 3)	#\1)
        ((4 5 6)	#\2)
        ((7 8 9)	#\3)
  	(else		#f))
    => #f)

  (check
      (match '(1)
        ((1 2 3)	#\1)
        ((4 5 6)	#\2)
        ((7 8 9)	#\3)
  	(else		#f))
    => #f)

  (check
      (match 1
        ((1 2 3)	#\1)
        ((4 5 6)	#\2)
        ((7 8 9)	#\3)
  	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '(4 5 6)
        ((1 2 3)	#\1)
        ((4 5 6)	#\2)
        ((7 8 9)	#\3)
	(else		#f))
    => #\2)

  (check
      (match '(7 8 9)
        ((1 2 3)	#\1)
        ((4 5 6)	#\2)
        ((7 8 9)	#\3)
	(else		#f))
    => #\3)

  #t)


(parametrise ((check-test-name	'vectors))

  (check
      (match '#()
        (#(1)	#\1)
        (#()	#\0)
        (#(4)	#\2)
        (#(7)	#\3)
  	(else	#f))
    => #\0)

  (check
      (match '#()
        (#(1)	#\1)
        (#(4)	#\2)
        (#(7)	#\3)
  	(else	#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '#(1)
        (#(1)	#\1)
        (#(4)	#\2)
	(#()	#\0)
        (#(7)	#\3)
  	(else	#f))
    => #\1)

  (check
      (match '#(4)
        (#(1)	#\1)
	(#()	#\0)
        (#(4)	#\2)
        (#(7)	#\3)
  	(else	#f))
    => #\2)

  (check
      (match '#(7)
        (#(1)	#\1)
        (#(4)	#\2)
        (#(7)	#\3)
  	(else	#f))
    => #\3)

  (check
      (match '#(0)
        (#(1)	#\1)
        (#(4)	#\2)
	(#()	#\0)
        (#(7)	#\3)
  	(else	#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '#(1 2 3)
        (#(1 2 3)	#\1)
        (#(4 5 6)	#\2)
        (#(7 8 9)	#\3)
  	(else		#f))
    => #\1)

  (check
      (match '#(1 2)
        (#(1 2 3)	#\1)
        (#(4 5 6)	#\2)
        (#(7 8 9)	#\3)
  	(else		#f))
    => #f)

  (check
      (match '#(1)
        (#(1 2 3)	#\1)
        (#(4 5 6)	#\2)
        (#(7 8 9)	#\3)
  	(else		#f))
    => #f)

  (check
      (match 1
        (#(1 2 3)	#\1)
        (#(4 5 6)	#\2)
        (#(7 8 9)	#\3)
  	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '#(4 5 6)
        (#(1 2 3)	#\1)
        (#(4 5 6)	#\2)
        (#(7 8 9)	#\3)
	(else		#f))
    => #\2)

  (check
      (match '#(7 8 9)
        (#(1 2 3)	#\1)
        (#(4 5 6)	#\2)
        (#(7 8 9)	#\3)
	(else		#f))
    => #\3)

  #t)


(parametrise ((check-test-name	'variable-binding))

  (check
      (match 1
        ((let X)	X)
  	(else		#f))
    => 1)

  (check
      (match 1
        ((let X)	#\A)
  	(else		#f))
    => #\A)

  (check
      (match '(1)
        ((let X)	X)
  	(else		#f))
    => '(1))

  (check
      (match '(1)
        (((let X))	X)
  	(else		#f))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (match '(1 2 3)
        (((let X) (let Y) (let Z))
	 (vector X Y Z))
  	(else
	 #f))
    => '#(1 2 3))

  (check
      (match '(1 2)
        (((let X) (let Y) (let Z))
	 (vector X Y Z))
  	(else
	 #f))
    => #f)

  (check
      (match '(1)
        (((let X) (let Y) (let Z))
	 (vector X Y Z))
  	(else
	 #f))
    => #f)

  (check
      (match 123
	((and (let X) (eval (positive? X)))
	 X)
	(else #f))
    => 123)

;;; --------------------------------------------------------------------
;;; nested bindings

  (check
      (match '(1 2 3)
	(((let X) (let X) (let X))
	 X)
	(else
	 #f))
    => 3)

;;; --------------------------------------------------------------------
;;; ellipsis in list

  (check
      (match '(1 2 3 4 5)
	(((let X) (let Y) (let Z ...))
	 (vector X Y Z))
	(else #f))
    => '#(1 2 (3 4 5)))

  (check
      (match '(1 2)
	(((let X) (let Y) (let Z ...))
	 (vector X Y Z))
	(else #f))
    => '#(1 2 ()))

  (check	;improper list
      (match '(1 2 . 3)
	(((let X) (let Y) (let Z ...))
	 (vector X Y Z))
	(else #f))
    => #f)

  (check	;improper list
      (match '(1 2 3 4 . 5)
	(((let X) (let Y) (let Z ...))
	 (vector X Y Z))
	(else #f))
    => #f)

;;; --------------------------------------------------------------------
;;; ellipsis in vector

  (check
      (match '#(1 2 3 4 5)
	(#((let X) (let Y) (let Z ...))
	 (vector X Y Z))
	(else #f))
    => '#(1 2 (3 4 5)))

  (check
      (match '#(1 2)
	(#((let X) (let Y) (let Z ...))
	 (vector X Y Z))
	(else #f))
    => '#(1 2 ()))

;;; --------------------------------------------------------------------
;;; errors

  (check	;LET alone
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 (let		#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => 'let)

  (check	;empty LET
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((let)		#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(let))

  (check	;LET with multiple subpatterns
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((let id 2)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(let id 2))

  (check	;LET with ellipsis not in list
      (guard (E ((syntax-violation? E)
		 #;(check-pretty-print (condition-message E))
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 (check-pretty-print E)
		 #f))
	(eval '(match '(1 2 3)
		 ((let id ...)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(let id ...))

  #t)


(parametrise ((check-test-name	'variable-reference))

  (check
      (let ((X 1))
	(match 1
	  (X		X)
	  (else		#f)))
    => 1)

  (check
      (let ((X 1))
	(match 1
	  (X		#\A)
	  (else		#f)))
    => #\A)

;;; --------------------------------------------------------------------

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match 1
	  (X		#\A)
	  (Y		#\B)
	  (Z		#\C)
	  (else		#f)))
    => #\A)

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match 2
	  (X		#\A)
	  (Y		#\B)
	  (Z		#\C)
	  (else		#f)))
    => #\B)

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match 3
	  (X		#\A)
	  (Y		#\B)
	  (Z		#\C)
	  (else		#f)))
    => #\C)

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match 0
	  (X		#\A)
	  (Y		#\B)
	  (Z		#\C)
	  (else		#f)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match '(1 2)
	  ((X Y)	#\A)
	  ((Y Z)	#\B)
	  ((Z X)	#\C)
	  (else		#f)))
    => #\A)

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match '(2 3)
	  ((X Y)	#\A)
	  ((Y Z)	#\B)
	  ((Z X)	#\C)
	  (else		#f)))
    => #\B)

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match '(3 1)
	  ((X Y)	#\A)
	  ((Y Z)	#\B)
	  ((Z X)	#\C)
	  (else		#f)))
    => #\C)

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match '(1 9)
	  ((X Y)	#\A)
	  ((Y Z)	#\B)
	  ((Z X)	#\C)
	  (else		#f)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((X 1) (Y 2) (Z 3))
	(match '(1 2)
	  ('(X Y)	#\A)
	  ((Y Z)	#\B)
	  ((Z X)	#\C)
	  (else		#f)))
    => #f)

;;; --------------------------------------------------------------------
;;; binding and referencing

  (check
      (match '(1 1)
	(((let X) X)	X)
	(else		#f))
    => 1)

  (check
      (match '(1 2)
	(((let X) X)	#t)
	(else		#f))
    => #f)

  (check
      (match '(1 2 1 2)
	(((let X) (let Y) X Y)
	 (list X Y))
	(else
	 #f))
    => '(1 2))

  #t)


(parametrise ((check-test-name	'quoted-data))

  (check
      (match '(1)
	('(1)		#\A)
	('(2)		#\B)
	('(3)		#\C)
	(else		#f))
    => #\A)

  (check
      (match '(2)
	('(1)		#\A)
	('(2)		#\B)
	('(3)		#\C)
	(else		#f))
    => #\B)

  (check
      (match '(3)
	('(1)		#\A)
	('(2)		#\B)
	('(3)		#\C)
	(else		#f))
    => #\C)

  (check
      (match '(0)
	('(1)		#\A)
	('(2)		#\B)
	('(3)		#\C)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '(1 2 3)
	('(1 2 3)	#\A)
	('(2 3 4)	#\B)
	('(3 4 5)	#\C)
	(else		#f))
    => #\A)

  (check
      (match '(2 3 4)
	('(1 2 3)	#\A)
	('(2 3 4)	#\B)
	('(3 4 5)	#\C)
	(else		#f))
    => #\B)

  (check
      (match '(3 4 5)
	('(1 2 3)	#\A)
	('(2 3 4)	#\B)
	('(3 4 5)	#\C)
	(else		#f))
    => #\C)

  (check
      (match '(0)
	('(1 2 3)	#\A)
	('(2 3 4)	#\B)
	('(3 4 5)	#\C)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match '(1 (2) 3)
	('(1 (2) 3)	#\A)
	('(2 (3 4))	#\B)
	('((3 4) 5)	#\C)
	(else		#f))
    => #\A)

  (check
      (match '(2 (3 4))
	('(1 (2) 3)	#\A)
	('(2 (3 4))	#\B)
	('((3 4) 5)	#\C)
	(else		#f))
    => #\B)

  (check
      (match '((3 4) 5)
	('(1 (2) 3)	#\A)
	('(2 (3 4))	#\B)
	('((3 4) 5)	#\C)
	(else		#f))
    => #\C)

  (check
      (match '(0)
	('(1 (2) 3)	#\A)
	('(2 (3 4))	#\B)
	('((3 4) 5)	#\C)
	(else		#f))
    => #f)

  #t)


(parametrise ((check-test-name	'quasiquoted-data))

  (check
      (match '(1)
	(`(1)		#\A)
	(`(2)		#\B)
	(`(3)		#\C)
	(else		#f))
    => #\A)

  (check
      (match '(2)
	(`(1)		#\A)
	(`(2)		#\B)
	(`(3)		#\C)
	(else		#f))
    => #\B)

  (check
      (match '(3)
	(`(1)		#\A)
	(`(2)		#\B)
	(`(3)		#\C)
	(else		#f))
    => #\C)

  (check
      (match '(0)
	(`(1)		#\A)
	(`(2)		#\B)
	(`(3)		#\C)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((X 2))
	(match '(1 2)
	  (`(1 ,X)	#\A)
	  (`(2 ,X)	#\B)
	  (`(3 ,X)	#\C)
	  (else		#f)))
    => #\A)

  (check
      (let ((X 2))
	(match '(2 2)
	  (`(1 ,X)	#\A)
	  (`(2 ,X)	#\B)
	  (`(3 ,X)	#\C)
	  (else		#f)))
    => #\B)

  (check
      (let ((X 2))
	(match '(3 2)
	  (`(1 ,X)	#\A)
	  (`(2 ,X)	#\B)
	  (`(3 ,X)	#\C)
	  (else		#f)))
    => #\C)

  (check
      (let ((X 2))
	(match '(0 2)
	  (`(1 ,X)	#\A)
	  (`(2 ,X)	#\B)
	  (`(3 ,X)	#\C)
	  (else		#f)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((X 9))
	(match '(2 (9))
	  (`(1 ,X)		#\A)
	  (`(2 ,(list X))	#\B)
	  (`(3 ,X)		#\C)
	  (else			#f)))
    => #\B)

  #t)


(parametrise ((check-test-name	'apply))

  (check
      (match +1
	((apply positive?)	#\A)
	((apply negative?)	#\B)
	(else #f))
    => #\A)

  (check
      (match -1
	((apply positive?)	#\A)
	((apply negative?)	#\B)
	(else #f))
    => #\B)

  (check
      (match 0
	((apply positive?)	#\A)
	((apply negative?)	#\B)
	(else #f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match 0
	((apply (lambda (x)
		  (or (positive? x)
		      (zero? x))))
	 #\A)
	((apply negative?)
	 #\B)
	(else #f))
    => #\A)

  (check
      (match 0
	((apply negative?)
	 #\B)
	((apply (lambda (x)
		  (or (positive? x)
		      (zero? x))))
	 #\A)
	(else #f))
    => #\A)

;;; --------------------------------------------------------------------
;;; errors

  (check	;empty APPLY
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((apply)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(apply))

  #t)


(parametrise ((check-test-name	'eval))

  (check
      (match 1
	((eval #t)
	 #t)
	(else #f))
    => #t)

  (check
      (match 1
	((eval #f)	#\A)
	(else		#f))
    => #f)

  (check
      (match 123
	((and (let X) (eval X))
	 X)
	(else #f))
    => 123)

  (check
      (match #f
	((and (let X) (eval X))
	 #t)
	(else #f))
    => #f)

  (check
      (match '(1 2)
	(((let X) (eval X))
	 X)
	(else #f))
    => 1)

  (check
      (match '(1 2 3)
	(((let X) (eval X) (let Y))
	 (vector X Y))
	(else #f))
    => '#(1 3))

;;; --------------------------------------------------------------------
;;; errors

  (check	;empty EVAL
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((eval)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(eval))

  (check	;EVAL with multiple subpatterns
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((eval 1 2)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(eval 1 2))

  #t)


(parametrise ((check-test-name	'and))

  (check	;empty AND succeeds
      (match 1
	((and)	#t)
	(else	#f))
    => #t)

  (check
      (match 1
	((and 1 (let X))	(+ 10 X))
	(else			#f))
    => 11)

  (check
      (match 1
	((and (let X) X)	(+ 10 X))
	(else			#f))
    => 11)

  (check
      (match 1
	((and 1 (let X))	(+ 10 X))
	(else			#f))
    => 11)

  (check
      (match 0
	((and 1 (let X))	(+ 10 X))
	(else			#f))
    => #f)

  (check
      (match 0
	((and (let X) 1)	(+ 10 X))
	(else			#f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match 123
	((and (let X) (let Y))	(vector X Y))
	(else			#f))
    => '#(123 123))

  (check
      (match 123
	((and (let X) (let Y) (let Z))
	 (vector X Y Z))
	(else #f))
    => '#(123 123 123))

;;; --------------------------------------------------------------------

  (check
      (match '(1 2 3)
	((and ((let X) (let Y) (let Z))
	      (1 2 3))
	 (vector X Y Z))
	(else #f))
    => '#(1 2 3))

  (check
      (match '(1 2 3)
	((and ((apply fixnum?) (apply fixnum?) (apply fixnum?))
	      ((let X) (let Y) (let Z)))
	 (vector X Y Z))
	(else #f))
    => '#(1 2 3))

  #t)


(parametrise ((check-test-name	'or))

  (check	;empty OR fails
      (match 1
	((or)	#t)
	(else	#f))
    => #f)

  (check
      (match 1
	((or 1)	#t)
	(else	#f))
    => #t)

  (check
      (match 2
	((or 1)	#t)
	(else	#f))
    => #f)

  (check
      (match 1
	((or (apply positive?)
	     (apply zero?))
	 #t)
	(else	#f))
    => #t)

  (check
      (match 0
	((or (apply positive?)
	     (apply zero?))
	 #t)
	(else	#f))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (match '(1 2 3)
	((or (1 2 5)
	     (1 2 6)
	     (1 2 3))
	 #t)
	(else #f))
    => #t)

  (check
      (match '(1 2 3)
	((or (1 2 5)
	     (1 2 6))
	 #\A)
	((or (1 2 3))
	 #\B)
	(else #f))
    => #\B)

  (check
      (match '(1 2 0)
	((or (1 2 5)
	     (1 2 6))
	 #\A)
	((or (1 2 3))
	 #\B)
	(else #f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (match 1
	((or (and (apply fixnum?) (let X))
	     (and (apply bignum?) (let X)))
	 (+ 10 X))
	(else	#f))
    => 11)

  (check
      (match (+ 10 (greatest-fixnum))
	((or (and (apply fixnum?) (let X))
	     (and (apply bignum?) (let X)))
	 (- X))
	(else	#f))
    => (- (+ 10 (greatest-fixnum))))

;;; --------------------------------------------------------------------

  (check	;check  that  OR  patterns defining  different  bindings
		;raise an error
      (guard (E ((syntax-violation? E)
		 #;(check-pretty-print E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((or (let X)
		      (let Y))
		  #\A)
		 (else	#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '((let X) (let Y)))

  #t)


(parametrise ((check-test-name	'not))

  (check
      (match 1
	((not 1)	#t)
	(else		#f))
    => #f)

  (check
      (match 2
	((not 1)	#t)
	(else		#f))
    => #t)

;;; --------------------------------------------------------------------
;;; errors

  (check	;empty NOT
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((not)		#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(not))

  (check	;multiple values
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((not 1 2)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(not 1 2))

  #t)


(parametrise ((check-test-name	'ellipsis))

;;; list patterns

  (check
      (match '(1)
	((1 ...)	#t)
	(else		#f))
    => '(#t))

  (check
      (match '(1 1)
	((1 ...)	#t)
	(else		#f))
    => '(#t #t))

  (check
      (match '(1 1 1)
	((1 ...)	#t)
	(else		#f))
    => '(#t #t #t))

  (check	;no match
      (match '(1 2)
	((0 1 ...)	#t)
	(else		#f))
    => #f)

  (check	;no match
      (match '(0 2)
	((0 1 ...)	#t)
	(else		#f))
    => #f)

  (check	;no match
      (match '(0)
	((1 ...)	#t)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------
;;; vector patterns

  (check
      (match '#(1)
	(#(1 ...)	#t)
	(else		#f))
    => '(#t))

  (check
      (match '#(1 1)
	(#(1 ...)	#t)
	(else		#f))
    => '(#t #t))

  (check
      (match '#(1 1 1)
	(#(1 ...)	#t)
	(else		#f))
    => '(#t #t #t))

  (check	;no match
      (match '#(1 1 2)
	(#(1 ...)	#t)
	(else		#f))
    => #f)

;;; --------------------------------------------------------------------
;;; variable bindings

  (check
      (match '(1 2 3)
	(((let X) ...)	(+ 10 X))
	(else		#f))
    => '(11 12 13))

  (check
      (match '(1 2 3)
	(((let X) (let Y) ...)
	 (vector X Y))
	(else
	 #f))
    => '(#(1 2) #(1 3)))

  (check
      (match '((1 2 3) (4 5 6) (7 8 9))
	((((let X) (let Y) (let Z))
	  ...)
	 (list X Y Z))
	(else #f))
    => '((1 2 3) (4 5 6) (7 8 9)))

  (check
      (match '((1 2 3)
	       (4 5 6)
	       (7 8 9))
	( (((let X) (let Y) ...) ...)
	 (vector X Y))
	(else #f))
    => '((#(1 2) #(1 3))
	 (#(4 5) #(4 6))
	 (#(7 8) #(7 9))))

;;; --------------------------------------------------------------------
;;; not patterns

  (check
      (match '(1 2 3)
	(((not 0) ...)	#t)
	(else		#f))
    => '(#t #t #t))

  (check	;no match
      (match '(1 2 3)
	(((not 3) ...)	#t)
	(else		#f))
    => #f)

  (check
      (match '(1 2 3)
	(((and (not 0) (let X)) ...)
	 (+ 10 X))
	(else #f))
    => '(11 12 13))

;;; --------------------------------------------------------------------
;;; errors

  (check	;ellipsis alone
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 (...		#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '...)

  (check	;ellipsis in non-tail position
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((... 1)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(... 1))

  (check	;ellipsis in non-tail position
      (guard (E ((syntax-violation? E)
		 (syntax->datum (syntax-violation-subform E)))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 ((2 ... 1)	#\A)
		 (else		#\B))
	      (environment '(vicare language-extensions simple-match))))
    => '(... 1))

  #t)


(parametrise ((check-test-name	'escape))

  (check
      (match '(1 2 3)
	((1 2 3)
	 (=> escape)
	 #t)
	(else #f))
    => #t)

  (check
      (match '(1 2 0)
	((1 2 3)
	 (=> escape)
	 #t)
	(else #f))
    => #f)

  (check
      (match '(1 2 3)
	((1 2 3)
	 (=> escape)
	 (escape))
	(else #f))
    => #f)

  (check
      (match '(1 2 3)
	((1 2 3)
	 (=> escape)
	 (escape))
	((1 2 3)
	 #t)
	(else #f))
    => #t)

  (check	;no match
      (guard (E ((error? E)
		 (condition-who E))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match '(1 2 3)
		 ((1 2 3)
		  (=> escape)
		  (escape)))
	      (environment '(vicare language-extensions simple-match))))
    => 'match)

  #t)


(parametrise ((check-test-name	'syntax))

  (check
      (match '(1 2 3)
	((syntax (1 2 3))
	 #t)
	(else #f))
    => #t)

  (check	;match an identifier
      (let ((ciao #f))
	(match #'ciao
	  ((syntax ciao (ciao))
	   #t)
	  (else #f)))
    => #t)

  (check	;match pattern variables
      (let ((ciao #f))
	(match '(1 2 3)
	  ((syntax (a b c))
	   (syntax->datum #'b))
	  (else #f)))
    => 2)

  #t)


(parametrise ((check-test-name	'misc))

  (check	;no match
      (guard (E ((error? E)
		 (condition-who E))
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 (2	#\A)
		 (3	#\B))
	      (environment '(vicare language-extensions simple-match))))
    => 'match)

;;; --------------------------------------------------------------------

  (check	;check that  EXPR is not  erroneously bound in  the ELSE
		;clause
      (guard (E ((undefined-violation? E)
		 #;(check-pretty-print E)
		 #t)
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 (1	#\A)
		 (else	expr))
	      (environment '(vicare language-extensions simple-match))))
    => #t)

  (check	;check that EXPR is not erroneously bound in a clause
      (guard (E ((undefined-violation? E)
		 #t)
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 (1	expr)
		 (else	#\B))
	      (environment '(vicare language-extensions simple-match))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
