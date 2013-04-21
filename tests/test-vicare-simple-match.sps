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
(import (vicare)
  (vicare language-extensions simple-match)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: destructuring match syntax\n")


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
      (match '(1)
        ((1)	#\1)
        ((4)	#\2)
        ((7)	#\3)
  	(else	#f))
    => #\1)

  (check
      (match '(4)
        ((1)	#\1)
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


(parametrise ((check-test-name	'misc))

  (check	;check that  EXPR is not  erroneously bound in  the ELSE
		;clause
      (guard (E ((undefined-violation? E)
		 #t)
		(else
		 #;(check-pretty-print E)
		 #f))
	(eval '(match 1
		 (1	#f)
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
		 (else	#f))
	      (environment '(vicare language-extensions simple-match))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
