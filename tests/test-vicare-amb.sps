;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the AMB operator
;;;Date: Thu Apr 18, 2013
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
  (vicare language amb)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: amb operator\n")


(parametrise ((check-test-name	'core))

  (check
      (amb (amb) 1)
    => 1)

  (check
      (amb 1 (amb))
    => 1)

  (check
      (with-result
       (let ((N (amb 1 2 3 4)))
	 (add-result N)
	 (unless (< 2 N)
	   (amb))
	 (unless (even? N)
	   (amb))
	 N))
    => '(4 (1 2 3 4)))

  (check
      (with-result
       (let ((N (amb 1 2 3 4)))
	 (add-result N)
	 (amb-assert (< 2 N))
	 (amb-assert (even? N))
	 N))
    => '(4 (1 2 3 4)))

  #t)


(parametrise ((check-test-name	'failures))

  (check
      (guard (E ((error? E)
		 #t)
		(else #f))
	(raise (make-amb-exhaustion)))
    => #t)

  (check
      (guard (E ((amb-exhaustion? E)
		 #t)
		(else #f))
	(raise (make-amb-exhaustion)))
    => #t)

  #;(check
      (with-result
       (guard (E ((amb-exhaustion? E)
		  (check-pretty-print E)
		  #t)
		 (else
		  (check-pretty-print E)
		  #f))
	 (let ((N (amb 1 3 5 7)))
	   (check-pretty-print N)
	   (add-result N)
	   (amb-assert (even? N))
	   N)))
    => '(#t (1 3 5 7)))

    #t)


(parametrise ((check-test-name	'random))

  (check
      (amb-random (amb-random) 1)
    => 1)

  (check
      (amb-random 1 (amb-random))
    => 1)

  (check
      (let ((N (amb-random 1 2 3 4)))
	(unless (< 2 N)
	  (amb))
	(unless (even? N)
	  (amb))
	N)
    => 4)

  (check
      (let ((N (amb-random 1 2 3 4)))
	(amb-assert (< 2 N))
	(amb-assert (even? N))
	N)
    => 4)

  (check
      (parametrise ((amb-random-fixnum-maker random))
	(let ((N (amb-random 1 2 3 4)))
	  (amb-assert (< 2 N))
	  (amb-assert (even? N))
	  N))
    => 4)

  #t)


;;;; done

(check-report)

;;; end of file
