;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare flonum-parser)
;;;Date: Wed Feb 15, 2012
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare flonum-parser)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing library (vicare flonum-parser)\n")


(parametrise ((check-test-name	'base))

  (check
      (parse-flonum 1.23456789
		    (lambda args
		      (cons 'real args))
		    (lambda args
		      (cons 'unreal args)))
    => '(real #t
	      (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\0 #\0 #\0 #\0
	       #\0 #\0 #\1)
	      1))

  (check
      (parse-flonum 12.34e5
		    (lambda args
		      (cons 'real args))
		    (lambda args
		      (cons 'unreal args)))
    => '(real #t (#\1 #\2 #\3 #\4) 7))

  (check
      (parse-flonum -12.34e5
		    (lambda args
		      (cons 'real args))
		    (lambda args
		      (cons 'unreal args)))
    => '(real #f (#\1 #\2 #\3 #\4) 7))

  (check
      (parse-flonum +inf.0
		    (lambda args
		      (cons 'real args))
		    (lambda args
		      (cons 'unreal args)))
    => '(unreal "+inf.0"))

  (check
      (parse-flonum +nan.0
		    (lambda args
		      (cons 'real args))
		    (lambda args
		      (cons 'unreal args)))
    => '(unreal "+nan.0"))

  #t)


;;;; done

(check-report)

;;; end of file
