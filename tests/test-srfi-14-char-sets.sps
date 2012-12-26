;;;
;;;Part of: Vicare Scheme
;;;Contents: test for general SRFI 14
;;;Date: Mon Dec 24, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (srfi :14 char-sets)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 14, char-sets\n")


(parametrise ((check-test-name 'general-procedures))

  (check
      (char-set? (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set? "abc")
    => #f)

;;; --------------------------------------------------------------------
;;; equality

  (check
      (char-set=)
    => #t)

  (check
      (char-set= (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set= (char-set #\a #\b #\c)
		 (char-set #\a #\b #\c))
    => #t)

  (check
      (let ((cs (char-set #\a #\b #\c)))
	(char-set= cs cs))
    => #t)

  (check	;3 equal
      (let ((cs1 (char-set #\a #\b #\c))
	    (cs2 (char-set #\a #\b #\c))
	    (cs3 (char-set #\a #\b #\c)))
	(char-set= cs1 cs2 cs3))
    => #t)

  (check	;3 different
      (let ((cs1 (char-set #\A #\b #\c))
	    (cs2 (char-set #\a #\b #\c))
	    (cs3 (char-set #\a #\b #\c)))
	(char-set= cs1 cs2 cs3))
    => #f)

  (check	;3 different
      (let ((cs1 (char-set #\a #\b #\c))
	    (cs2 (char-set #\a #\b #\c))
	    (cs3 (char-set #\a #\B #\c)))
	(char-set= cs1 cs2 cs3))
    => #f)

;;; --------------------------------------------------------------------
;;; subset

  (check
      (char-set<=)
    => #t)

  (check
      (char-set<= (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set<= (char-set #\a #\b #\c)
		  (char-set #\a #\b #\c))
    => #t)

  (check
      (let ((cs (char-set #\a #\b #\c)))
	(char-set<= cs cs))
    => #t)

  (check	;3 equal
      (let ((cs1 (char-set #\a #\b #\c))
	    (cs2 (char-set #\a #\b #\c))
	    (cs3 (char-set #\a #\b #\c)))
	(char-set<= cs1 cs2 cs3))
    => #t)

  (check	;3 different
      (let ((cs1 (char-set #\A #\b #\c))
	    (cs2 (char-set #\a #\b #\c))
	    (cs3 (char-set #\a #\b #\c)))
	(char-set<= cs1 cs2 cs3))
    => #f)

  (check	;3 different
      (let ((cs1 (char-set #\a #\b #\c))
	    (cs2 (char-set #\a #\b #\c))
	    (cs3 (char-set #\a #\B #\c)))
	(char-set<= cs1 cs2 cs3))
    => #f)

  (check	;first subset of second
      (let ((cs1 (char-set #\a #\b))
	    (cs2 (char-set #\a #\b #\c)))
	(char-set<= cs1 cs2))
    => #t)

  (check	;second subset of first
      (let ((cs1 (char-set #\a #\b #\c))
	    (cs2 (char-set #\a #\b)))
	(char-set<= cs1 cs2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (integer? (char-set-hash (char-set)))
    => #t)

  (check
      (integer? (char-set-hash (char-set #\a)))
    => #t)

  (check
      (integer? (char-set-hash (char-set #\a #\b #\c)))
    => #t)

  (check
      (let ((H (char-set-hash (char-set #\a #\b #\c) 10)))
	(and (integer? H)
	     (< H 10)))
    => #t)

  (check
      (= (char-set-hash (char-set #\a #\b #\c))
	 (char-set-hash (char-set #\a #\b #\c)))
    => #t)

  (check
      (= (char-set-hash (char-set #\a #\b #\c))
	 (char-set-hash (char-set #\A #\B #\C)))
    => #f)

  #t)


(parametrise ((check-test-name 'iterating))

  (define (do-iter cs)
    (let ((ell '()))
      (do ((iter (char-set-cursor cs) (char-set-cursor-next cs iter)))
	  ((end-of-char-set? iter)
	   ell)
	(set! ell (cons (char-set-ref cs iter) ell)))))

  (check
      (do-iter (char-set))
    => '())

  (check
      (do-iter (char-set #\a))
    => '(#\a))

  (check
      (do-iter (char-set #\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (do-iter (char-set #\a #\b #\c #\e #\f #\g #\i #\j #\k #\l #\m))
    => '(#\a #\b #\c #\e #\f #\g #\i #\j #\k #\l #\m))

;;; --------------------------------------------------------------------
;;; folding

  (check	;list of set members
      (char-set-fold cons '() (char-set #\a #\b #\c))
    => '(#\a #\b #\c))

  (check	;set size
      (char-set-fold (lambda (c i)
		       (+ i 1))
		     0
		     (char-set #\a #\b #\c))
    => 3)

;;; --------------------------------------------------------------------
;;; unfolding

  (check
      (let ((p (open-string-input-port "abc")))
	(char-set-unfold eof-object? values
			 (lambda (x) (read-char p))
			 (read-char p)))
    (=> char-set=)
    (char-set #\a #\b #\c))

  #t)


;;;; done

(check-report)

;;; end of file
