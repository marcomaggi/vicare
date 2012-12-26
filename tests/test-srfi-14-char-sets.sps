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

;;; --------------------------------------------------------------------
;;; for-each

  (check
      (with-result
       (char-set-for-each add-result (char-set #\a #\b #\c)))
    => (list (void) '(#\c #\b #\a)))

;;; --------------------------------------------------------------------
;;; map

  (check
      (char-set-map char-upcase (char-set #\a #\b #\c))
    (=> char-set=) (char-set #\A #\B #\C))

  #t)


(parametrise ((check-test-name 'creating))

  (check
      (char-set-copy (char-set))
    (=> char-set=)
    (char-set))

  (check
      (char-set-copy (char-set #\a))
    (=> char-set=)
    (char-set #\a))

  (check
      (char-set-copy (char-set #\a #\b #\c))
    (=> char-set=)
    (char-set #\a #\b #\c))

;;; --------------------------------------------------------------------

  (check
      (list->char-set '())
    (=> char-set=)
    (char-set))

  (check
      (list->char-set '(#\a))
    (=> char-set=)
    (char-set #\a))

  (check
      (list->char-set '(#\a #\b #\c))
    (=> char-set=)
    (char-set #\a #\b #\c))

  (check
      (list->char-set '() (char-set #\c #\d))
    (=> char-set=)
    (char-set #\c #\d))

  (check
      (list->char-set '(#\a) (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\c #\d))

  (check
      (list->char-set '(#\a #\b #\c) (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\b #\c #\d))

;;; --------------------------------------------------------------------

  (check
      (list->char-set! '() (char-set #\c #\d))
    (=> char-set=)
    (char-set #\c #\d))

  (check
      (list->char-set! '(#\a) (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\c #\d))

  (check
      (list->char-set! '(#\a #\b #\c) (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\b #\c #\d))

;;; --------------------------------------------------------------------

  (check
      (string->char-set "")
    (=> char-set=)
    (char-set))

  (check
      (string->char-set "a")
    (=> char-set=)
    (char-set #\a))

  (check
      (string->char-set "abc")
    (=> char-set=)
    (char-set #\a #\b #\c))

  (check
      (string->char-set "" (char-set #\c #\d))
    (=> char-set=)
    (char-set #\c #\d))

  (check
      (string->char-set "a" (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\c #\d))

  (check
      (string->char-set "abc" (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\b #\c #\d))

;;; --------------------------------------------------------------------

  (check
      (string->char-set! "" (char-set #\c #\d))
    (=> char-set=)
    (char-set #\c #\d))

  (check
      (string->char-set! "a" (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\c #\d))

  (check
      (string->char-set! "abc" (char-set #\c #\d))
    (=> char-set=)
    (char-set #\a #\b #\c #\d))

;;; --------------------------------------------------------------------

  (check
      (char-set-filter (lambda (ch)
			 #f)
		       (char-set #\a #\b #\c))
    (=> char-set=)
    (char-set))

  (check
      (char-set-filter (lambda (ch)
			 (char<=? #\b ch #\c))
		       (char-set #\a #\b #\c))
    (=> char-set=)
    (char-set #\b #\c))

  (check
      (char-set-filter (lambda (ch)
			 #f)
		       (char-set #\a #\b #\c)
		       (char-set #\d #\e))
    (=> char-set=)
    (char-set #\d #\e))

  (check
      (char-set-filter (lambda (ch)
			 (char<=? #\b ch #\c))
		       (char-set #\a #\b #\c)
		       (char-set #\d #\e))
    (=> char-set=)
    (char-set #\b #\c #\d #\e))

;;; --------------------------------------------------------------------

  (check
      (ucs-range->char-set (char->integer #\a)
			   (char->integer #\d))
    (=> char-set=)
    (char-set #\a #\b #\c))

  (check
      (ucs-range->char-set (char->integer #\a)
			   (char->integer #\d)
			   #f
			   (char-set #\d #\e))
    (=> char-set=)
    (char-set #\a #\b #\c #\d #\e))

;;; --------------------------------------------------------------------

  (check
      (->char-set "")
    (=> char-set=) (char-set))

  (check
      (->char-set "abc")
    (=> char-set=) (char-set #\a #\b #\c))

  (check
      (->char-set #\a)
    (=> char-set=) (char-set #\a))

  (check
      (->char-set (char-set))
    (=> char-set=) (char-set))

  (check
      (->char-set (char-set #\a #\b #\c))
    (=> char-set=) (char-set #\a #\b #\c))

  #t)


(parametrise ((check-test-name 'query))

  (check
      (char-set-size (char-set))
    => 0)

  (check
      (char-set-size (char-set #\a))
    => 1)

  (check
      (char-set-size (char-set #\a #\b #\c))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (char-set-count (lambda (ch)
			#t)
		      (char-set))
    => 0)

  (check
      (char-set-count (lambda (ch)
			(char>? ch #\A))
		      (char-set #\a))
    => 1)

  (check
      (char-set-count (lambda (ch)
			(char>? ch #\A))
		      (char-set #\a #\b #\c))
    => 3)

  (check
      (char-set-count (lambda (ch)
			(char<? ch #\A))
		      (char-set #\a))
    => 0)

  (check
      (char-set-count (lambda (ch)
			(char<? ch #\b))
		      (char-set #\a #\b #\c))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (char-set->list (char-set))
    => '())

  (check
      (char-set->list (char-set #\a))
    => '(#\a))

  (check
      (char-set->list (char-set #\a #\b #\c))
    => '(#\a #\b #\c))

;;; --------------------------------------------------------------------

  (check
      (char-set->string (char-set))
    => "")

  (check
      (char-set->string (char-set #\a))
    => "a")

  (check
      (char-set->string (char-set #\a #\b #\c))
    => "cba")

;;; --------------------------------------------------------------------

  (check
      (char-set-contains? (char-set) #\a)
    => #f)

  (check
      (char-set-contains? (char-set #\a) #\a)
    => #t)

  (check
      (char-set-contains? (char-set #\a) #\A)
    => #f)

  (check
      (char-set-contains? (char-set #\a #\b #\c) #\b)
    => #t)

  (check
      (char-set-contains? (char-set #\a #\b #\c) #\B)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char-set-every (lambda (ch) #f) (char-set))
    => #t)

  (check
      (char-set-every (lambda (ch)
			(char<? ch #\b))
		      (char-set #\a))
    => #t)

  (check
      (char-set-every (lambda (ch)
			(char<? ch #\z))
		      (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set-every (lambda (ch)
			(char>? ch #\z))
		      (char-set #\a #\b #\c))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char-set-any (lambda (ch) #f) (char-set))
    => #f)

  (check
      (char-set-any (lambda (ch)
			(char<? ch #\b))
		      (char-set #\a))
    => #t)

  (check
      (char-set-any (lambda (ch)
			(char<? ch #\z))
		      (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set-any (lambda (ch)
			(char>? ch #\z))
		      (char-set #\a #\b #\c))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
