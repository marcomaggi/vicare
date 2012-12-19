;;;
;;;Part of: Vicare Scheme
;;;Contents: test for SRFI 26
;;;Date: Wed Dec 19, 2012
;;;
;;;Abstract
;;;
;;;	This file checks a few  assertions about the implementation.  If
;;;	you run it and no error message is issued, the implementation is
;;;	correct on the cases that have been tested.
;;;
;;;Copyright (c) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Sebastian.Egner@philips.com, 3-Jun-2002.
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
  (srfi :26)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 26, cut\n")


(parametrise ((check-test-name	'cut))

  (check ((cut list))				=> '())
  (check ((cut list <...>))			=> '())
  (check ((cut list 1))				=> '(1))
  (check ((cut list <>) 1)			=> '(1))
  (check ((cut list <...>) 1)			=> '(1))
  (check ((cut list 1 2))			=> '(1 2))
  (check ((cut list 1 <>) 2)			=> '(1 2))
  (check ((cut list 1 <...>) 2)			=> '(1 2))
  (check ((cut list 1 <...>) 2 3 4)		=> '(1 2 3 4))
  (check ((cut list 1 <> 3 <>) 2 4)		=> '(1 2 3 4))
  (check ((cut list 1 <> 3 <...>) 2 4 5 6)	=> '(1 2 3 4 5 6))

  (check
      (let* ((x 'wrong)
	     (y (cut list x)))
	(set! x 'ok)
	(y))
    => '(ok))

  (check
      (let ((a 0))
	(map (cut + (begin
		      (set! a (+ a 1))
		      a)
		  <>)
	  '(1 2))
	a)
    => 2)

  #t)


(parametrise ((check-test-name	'cute))

  (check ((cute list))				=> '())
  (check ((cute list <...>))			=> '())
  (check ((cute list 1))			=> '(1))
  (check ((cute list <>) 1)			=> '(1))
  (check ((cute list <...>) 1)			=> '(1))
  (check ((cute list 1 2))			=> '(1 2))
  (check ((cute list 1 <>) 2)			=> '(1 2))
  (check ((cute list 1 <...>) 2)		=> '(1 2))
  (check ((cute list 1 <...>) 2 3 4)		=> '(1 2 3 4))
  (check ((cute list 1 <> 3 <>) 2 4)		=> '(1 2 3 4))
  (check ((cute list 1 <> 3 <...>) 2 4 5 6)	=> '(1 2 3 4 5 6))

  (check
      (let* ((x 'ok)
	     (y (cute list x)))
	(set! x 'wrong)
	(y))
    => '(ok))

  (check
      (let ((a 0))
	(map (cute + (begin
		       (set! a (+ a 1))
		       a)
		   <>)
	  '(1 2))
	a)
    => 1)

  #t)


;;;; done

(check-report)

;;; end of file
