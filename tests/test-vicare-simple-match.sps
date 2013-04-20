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


#!r6rs
(import (vicare)
  (vicare language-extensions simple-match)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: destructuring match syntax\n")


(parametrise ((check-test-name	'fixnums-and-pairs))

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
