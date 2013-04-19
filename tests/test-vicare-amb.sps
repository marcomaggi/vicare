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
  (vicare language-extensions amb)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: amb operator\n")


(parametrise ((check-test-name	'core))

  (check
      (with-ambiguous-choices
       (amb (amb) 1))
    => 1)

  (check
      (with-ambiguous-choices
       (amb 1 (amb)))
    => 1)

  (check	;successfully find value
      (with-result
       (with-ambiguous-choices
	(let ((N (amb 1 2 3 4)))
	  (add-result N)
	  (unless (< 2 N)
	    (amb))
	  (unless (even? N)
	    (amb))
	  N)))
    => '(4 (1 2 3 4)))

  (check	;successfully find value, test AMB-ASSERT
      (with-result
       (with-ambiguous-choices
	(let ((N (amb 1 2 3 4)))
	  (add-result N)
	  (amb-assert (< 2 N))
	  (amb-assert (even? N))
	  N)))
    => '(4 (1 2 3 4)))

  (check	;successfully find sequence
      (with-result
       (with-ambiguous-choices
	(guard (E ((amb-exhaustion? E)
		   #t)
		  (else
		   #f))
	  (let ((N (amb 1 2 3 4 5 6 7 8 9 10)))
	    (amb-assert (odd? N))
	    (add-result N)
	    (amb)))))
    => '(#t (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check	;seed, square, cube
      (with-ambiguous-choices
       (let* ((A (amb 1 2 3))
	      (B (amb 5 9 11)))
	 (amb-assert (= (square A) B))
	 (let ((C (amb 21 24 27 33)))
	   (amb-assert (= (cube A) C))
	   (list A B C))))
    => '(3 9 27))

  (check 	;seed, square, cube
      (let ()
	(define (print . args)
	  (apply fprintf (current-error-port) args))
	(with-ambiguous-choices
	 (let ((A (amb 1 3)))
	   (print "A=~a\n" A)
	   (let ((B (amb 5 9 11)))
	     (print "\tB=~a\n" B)
	     (amb-assert (= (square A) B))
	     (let ((C (amb 13 27 31)))
	       (print "\t\tC=~a\n" C)
	       (amb-assert (= (cube A) C))
	       (list A B C))))))
    => '(3 9 27))

  #t)


(parametrise ((check-test-name	'random))

  (check
      (with-ambiguous-choices
       (amb-random (amb-random) 1))
    => 1)

  (check
      (with-ambiguous-choices
       (amb-random 1 (amb-random)))
    => 1)

  (check
      (with-ambiguous-choices
       (let ((N (amb-random 1 2 3 4)))
	 (unless (< 2 N)
	   (amb))
	 (unless (even? N)
	   (amb))
	 N))
    => 4)

  (check
      (with-ambiguous-choices
       (let ((N (amb-random 1 2 3 4)))
	 (amb-assert (< 2 N))
	 (amb-assert (even? N))
	 N))
    => 4)

  (check
      (with-ambiguous-choices
       (parametrise ((amb-random-fixnum-maker random))
	 (let ((N (amb-random 1 2 3 4)))
	   (amb-assert (< 2 N))
	   (amb-assert (even? N))
	   N)))
    => 4)

  #t)


(parametrise ((check-test-name	'failures))

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(eval '(amb (amb) 1)
	      (environment '(vicare language-extensions amb))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((error? E)
		 #t)
		(else #f))
	(raise (make-amb-exhaustion)))
    => #f)

  (check
      (guard (E ((amb-exhaustion? E)
		 #t)
		(else #f))
	(raise (make-amb-exhaustion)))
    => #t)

;;; --------------------------------------------------------------------

  (check	;AMB failure
      (with-result
       (with-ambiguous-choices
	(guard (E ((amb-exhaustion? E)
		   #t)
		  (else
		   #f))
	  (let ((N (amb 1 3 5 7)))
	    (add-result N)
	    (amb-assert (even? N))
	    N))))
    => '(#t (1 3 5 7)))

  (check	;AMB-RANDOM failure
      (with-ambiguous-choices
       (guard (E ((amb-exhaustion? E)
		  #t)
		 (else
		  #f))
	 (let ((N (amb-random 1 3 5 7)))
	   (add-result N)
	   (amb-assert (even? N))
	   N)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
