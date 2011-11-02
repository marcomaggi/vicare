;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 20, shared structures displayed peculiarly
;;;Date: Mon Jun 13, 2011
;;;
;;;Abstract
;;;
;;;	Reported by Göran Weinholt.
;;;
;;;		> (let ((l '(b c)))
;;;		    (display (list l (cdr l)))
;;;		    (newline))
;;;		((b . (c)) (c))
;;;		>
;;;
;;;	As you  can see,  the printer decided  to use the  dot notation,
;;;	even  though it  wasn't necessary  in this  case.  This  can get
;;;	pretty confusing in larger examples.
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (ikarus)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 20, shared structures displayed peculiarly\n")


(check
    (call-with-string-output-port
	(lambda (p)
	  (display '(1 . 2) p)))
  => "(1 . 2)")

(check
    (call-with-string-output-port
	(lambda (p)
	  (display '(1 2) p)))
  => "(1 2)")

(check
    (call-with-string-output-port
	(lambda (p)
	  (display '(1 2 . 3) p)))
  => "(1 2 . 3)")

(check
    (call-with-string-output-port
	(lambda (p)
	  (display '(1 2 3) p)))
  => "(1 2 3)")

(parameterize ((print-graph #f))
  (check
      (call-with-string-output-port
	  (lambda (p)
	    (let ((l '(b c)))
	      (display (list l (cdr l)) p))))
    => "((b (c)) (c))")

  (check
      (call-with-string-output-port
	  (lambda (p)
	    (let ((l '(b c)))
	      (write (list l (cdr l)) p))))
    => "((b (c)) (c))"))

(parameterize ((print-graph #t))
  (check
      (call-with-string-output-port
	  (lambda (p)
	    (let ((l '(b c)))
	      (display (list l (cdr l)) p))))
    => "((b . #0=(c)) #0#)")

  (check
      (call-with-string-output-port
	  (lambda (p)
	    (let ((l '(b c)))
	      (write (list l (cdr l)) p))))
    => "((b . #0=(c)) #0#)"))


;;;; done

(check-report)

;;; end of file
