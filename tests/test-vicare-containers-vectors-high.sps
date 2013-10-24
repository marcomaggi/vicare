;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the vectors library
;;;Date: Tue Jun 16, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (except (vicare)
		vector-append
		vector-copy
		vector-copy!)
  (vicare containers vectors)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers: vectors library\n")


;;;; helpers

(define (num-cmp a b)
  (cond ((> a b) +1)
	((< a b) -1)
	(else     0)))

(define (xcons d a)
  (cons a d))



(parameterise ((check-test-name 'views))

  (check
      (subvector* '#(0 1 2 3))
    => '#(0 1 2 3))

;;; --------------------------------------------------------------------

  (check
      (subvector* (view '#(0 1 2 3)))
    => '#(0 1 2 3))

  (check
      (subvector* (view '#(0 1 2 3) (start 2)))
    => '#(2 3))

  (check
      (subvector* (view '#(0 1 2 3) (start 0) (past 4)))
    => '#(0 1 2 3))

  (check
      (subvector* (view '#(0 1 2 3) (start 0) (past 0)))
    => '#())

  (check
      (subvector* (view '#(0 1 2 3) (start 1) (past 1)))
    => '#())

  (check
      (subvector* (view '#(0 1 2 3) (start 0) (past 1)))
    => '#(0))

  (check
      (subvector* (view '#(0 1 2 3) (past 2)))
    => '#(0 1))

;;; --------------------------------------------------------------------

  (check
      (subvector* (view '#(1 2 3 4) (start -2)))
    => '#(3 4))

  (check
      (subvector* (view '#(1 2 3 4) (past -1)))
    => '#(1 2 3))

  (check
      (subvector* (view '#(1 2 3 4) (past -2)))
    => '#(1 2))

  (check
      (subvector* (view '#(1 2 3 4) (start -3) (past -2)))
    => '#(2))

  #f)


(parameterise ((check-test-name 'constructor))

  (check
      (vector-append '#(0 1 2 3))
    => '#(0 1 2 3))

  (check
      (vector-append '#(0 1 2 3)
		     '#(4 5 6 7 8))
    => '#(0 1 2 3 4 5 6 7 8))

  (check
      (vector-append '#())
    => '#())

  (check
      (vector-append '#() '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => '#(#\A #\B #\C #\D))

  (check
      (vector-tabulate integer->char 0)
    => '#())

  )


(parameterise ((check-test-name 'concatenate))

  (check
      (vector-concatenate '(#(#\c #\i #\a #\o) #(#\space)
			    #(#\h #\e #\l #\l #\o) #(#\space)
			    #(#\s #\a #\l #\u #\t)))
    => '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t))

  (check
      (vector-concatenate '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-concatenate-reverse '(#(#\c #\i #\a #\o) #(#\space)
				    #(#\h #\e #\l #\l #\o) #(#\space)
				    #(#\s #\a #\l #\u #\t))
				  '#(#\space #\h #\o #\l #\a) 3)
    => '#(#\s #\a #\l #\u #\t #\space #\h #\e #\l #\l #\o #\space #\c #\i #\a #\o #\space #\h #\o))

  (check
      (vector-concatenate-reverse '(#(#\c #\i #\a #\o) #(#\space)
				    #(#\h #\e #\l #\l #\o) #(#\space)
				    #(#\s #\a #\l #\u #\t))
				  '#(#\space #\h #\o #\l #\a))
    => '#(#\s #\a #\l #\u #\t #\space #\h #\e #\l #\l #\o #\space
	  #\c #\i #\a #\o #\space #\h #\o #\l #\a))

  (check
      (vector-concatenate-reverse '(#(#\c #\i #\a #\o) #(#\space)
				    #(#\h #\e #\l #\l #\o) #(#\space)
				    #(#\s #\a #\l #\u #\t)))
    => '#(#\s #\a #\l #\u #\t #\space #\h #\e #\l #\l #\o #\space #\c #\i #\a #\o))

  (check
      (vector-concatenate-reverse '())
    => '#())

  )


(parameterise ((check-test-name 'predicates))

  (check
      (vector-null? '#(0 1 2 3))
    => #f)

  (check
      (vector-null? '#())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((vec '#(#\a #\b #\c)))
	(subvector-every char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\a #\b #\c #\2)))
	(subvector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(#\a #\b #\2 #\d)))
	(subvector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#()))
	(subvector-every char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(1 2 3 4)))
	(subvector-every (lambda (x) x) vec))
    => 4)

;;; --------------------------------------------------------------------

  (check
      (let* ((vec '#(#\a #\b #\c)))
	(vector-every char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\a #\b #\c #\2)))
	(vector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(#\a #\b #\2 #\d)))
	(vector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#()))
	(vector-every char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(1 2 3 4)))
	(vector-every (lambda (x) x) vec))
    => 4)

;;; --------------------------------------------------------------------

  (check
      (let* ((vec '#(#\1 #\2 #\a #\4)))
	(subvector-any char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\1 #\2 #\3 #\a)))
	(subvector-any char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\1 #\2 #\3 #\4)))
	(subvector-any char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#()))
	(subvector-any char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(1 2 3 4)))
	(subvector-any (lambda (x) x) vec))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let* ((vec '#(#\1 #\2 #\a #\4)))
	(vector-any char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\1 #\2 #\3 #\a)))
	(vector-any char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\1 #\2 #\3 #\4)))
	(vector-any char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#()))
	(vector-any char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(1 2 3 4)))
	(vector-any (lambda (x) x) vec))
    => 1)

  )


(parameterise ((check-test-name 'comparison))

  (check-for-true
   (let* ((vec '#(#\a #\b #\c #\d)))
     (vector= char=? vec vec)))

  (check-for-true
   (vector= char=?
	    (view '#(#\1 #\2 #\a #\b #\c #\d) (start 2))
	    '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector= char=? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector= char=? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-false
   (vector= char=? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector= char=? '#(#\a #\b #\c #\d) '#(#\a #\2 #\c #\d)))

;;; --------------------------------------------------------------------

  (check-for-false
   (vector<> char=? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector<> char=? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector<> char=? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-true
   (vector<> char=? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector<> char=? '#(#\a #\b #\c #\d) '#(#\a #\2 #\c #\d)))

;;; --------------------------------------------------------------------

  (check-for-false
   (vector< char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector< char=? char<? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector< char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-true
   (vector< char=? char<? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector< char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\2 #\c #\d)))

;;; --------------------------------------------------------------------

  (check-for-true
   (vector<= char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector<= char=? char<? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector<= char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-true
   (vector<= char=? char<? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector<= char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\2 #\c #\d)))

;;; --------------------------------------------------------------------

  (check-for-false
   (vector> char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector> char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-false
   (vector> char=? char<? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector> char=? char<? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector> char=? char<? '#(#\a #\2 #\c #\d) '#(#\a #\b #\c #\d)))

;;; --------------------------------------------------------------------

  (check-for-true
   (vector>= char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector>= char=? char<? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-false
   (vector>= char=? char<? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector>= char=? char<? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector>= char=? char<? '#(#\a #\2 #\c #\d) '#(#\a #\b #\c #\d)))

  )


(parameterise ((check-test-name 'mapping))

  (check
      (vector-map/with-index (lambda (i c) (list i (char-upcase c)))
			     '#(#\a #\b #\c #\d))
    => '#((0 #\A)
	  (1 #\B)
	  (2 #\C)
	  (3 #\D)))

  (check
      (vector-map/with-index list
			     '#(#\a #\b #\c #\d)
			     '#(#\0 #\1 #\2 #\3))
    => '#((0 #\a #\0)
	  (1 #\b #\1)
	  (2 #\c #\2)
	  (3 #\d #\3)))

  (check
      (vector-map/with-index list '#())
    => '#())

  (check
      (vector-map/with-index list '#() '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map! char-upcase vec)
	vec)
    => '#(#\A #\B #\C #\D))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map! list vec '#(#\0 #\1 #\2 #\3))
	vec)
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (let ((vec '#()))
	(vector-map! char-upcase vec)
	vec)
    => '#())

  (check
      (let ((vec '#()))
	(vector-map! char-upcase vec '#())
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map!/with-index list vec)
	vec)
    => '#((0 #\a)
	  (1 #\b)
	  (2 #\c)
	  (3 #\d)))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map!/with-index list vec '#(#\0 #\1 #\2 #\3))
	vec)
    => '#((0 #\a #\0)
	  (1 #\b #\1)
	  (2 #\c #\2)
	  (3 #\d #\3)))

  (check
      (let ((vec (vector-copy '#())))
	(vector-map!/with-index list vec)
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-map* char-upcase '#(#\a #\b #\c #\d))
    => '#(#\A #\B #\C #\D))

  (check
      (vector-map* list
		   '#(#\a #\b #\c #\d)
		   '#(#\0 #\1 #\2 #\3 #\4))
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (vector-map* char-upcase '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-map*/with-index (lambda (i c) (list i (char-upcase c)))
			      '#(#\a #\b #\c #\d))
    => '#((0 #\A)
	  (1 #\B)
	  (2 #\C)
	  (3 #\D)))

  (check
      (vector-map*/with-index list
			      '#(#\a #\b #\c #\d)
			      '#(#\0 #\1 #\2 #\3 #\4))
    => '#((0 #\a #\0)
	  (1 #\b #\1)
	  (2 #\c #\2)
	  (3 #\d #\3)))

  (check
      (vector-map*/with-index list '#())
    => '#())

  (check
      (vector-map*/with-index list '#() '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map*! char-upcase vec)
	vec)
    => '#(#\A #\B #\C #\D))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map*! list vec '#(#\0 #\1 #\2 #\3 #\4))
	vec)
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (let ((vec (vector-copy '#())))
	(vector-map*! list vec)
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map*!/with-index (lambda (i item) (list i item))
				 vec)
	vec)
    => '#((0 #\a)
	  (1 #\b)
	  (2 #\c)
	  (3 #\d)))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map*!/with-index (lambda (i item-a item-b) (list i item-a item-b))
				 vec
				 '#(#\0 #\1 #\2 #\3 #\4))
	vec)
    => '#((0 #\a #\0)
	  (1 #\b #\1)
	  (2 #\c #\2)
	  (3 #\d #\3)))

  (check
      (let ((vec (vector-copy '#())))
	(vector-map*!/with-index (lambda (i item) (list i item))
				 vec)
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (vector-for-each* add-result '#(#\a #\b #\c #\d))))
    => '(#\a #\b #\c #\d))

  (check
      (cadr (with-result
	     (vector-for-each* (lambda (item-a item-b) (add-result (list item-a item-b)))
			       '#(#\a #\b #\c #\d)
			       '#(#\0 #\1 #\2 #\3))))
    => '((#\a #\0)
	 (#\b #\1)
	 (#\c #\2)
	 (#\d #\3)))

  (check
      (cadr (with-result (vector-for-each* add-result '#())))
    => '())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (vector-for-each*/with-index (lambda (i item) (add-result (list i item)))
					  '#(#\a #\b #\c #\d))))
    => '((0 #\a)
	 (1 #\b)
	 (2 #\c)
	 (3 #\d)))

  (check
      (cadr (with-result
	     (vector-for-each*/with-index
	      (lambda (i item-a item-b) (add-result (list i item-a item-b)))
	      '#(#\a #\b #\c #\d)
	      '#(#\0 #\1 #\2 #\3))))
    => '((0 #\a #\0)
	 (1 #\b #\1)
	 (2 #\c #\2)
	 (3 #\d #\3)))

  (check
      (cadr (with-result
	     (vector-for-each*/with-index (lambda (i item) (add-result (list i item)))
					  '#())))
    => '())

;;; --------------------------------------------------------------------

  (check
      (subvector-map char-upcase '#(#\a #\b #\c #\d))
    => '#(#\A #\B #\C #\D))


  (check
      (subvector-map char-upcase (view '#(#\a #\b #\c #\d) (start 1) (past 3)))
    => '#(#\B #\C))

  (check
      (subvector-map char-upcase '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (subvector-map/with-index (lambda (i c) (list i c)) '#(#\a #\b #\c #\d))
    => '#((0 #\a)
	  (1 #\b)
	  (2 #\c)
	  (3 #\d)))

  (check
      (subvector-map/with-index (lambda (i c) (list i c))
				(view '#(#\a #\b #\c #\d) (start 1) (past 3)))
    => '#((1 #\b)
	  (2 #\c)))

  (check
      (subvector-map/with-index (lambda (i c) (list i c)) '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(subvector-map! char-upcase vec)
	vec)
    => '#(#\A #\B #\C #\D))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(subvector-map! char-upcase (view vec (start 1) (past 3)))
	vec)
    => '#(#\a #\B #\C #\d))

  (check
      (let ((vec '#()))
	(subvector-map! char-upcase vec)
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(subvector-map!/with-index (lambda (i c) (list i c)) vec)
	vec)
    => '#((0 #\a)
	  (1 #\b)
	  (2 #\c)
	  (3 #\d)))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(subvector-map!/with-index (lambda (i c) (list i c))
				   (view vec (start 1) (past 3)))
	vec)
    => '#(#\a
	  (1 #\b)
	  (2 #\c)
	  #\d))

  (check
      (let ((vec '#()))
	(subvector-map!/with-index (lambda (i c) (list i c)) vec)
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (subvector-for-each add-result
				 '#(#\a #\b #\c #\d))))
    => '(#\a #\b #\c #\d))

  (check
      (cadr (with-result
	     (subvector-for-each add-result
				 (view '#(#\a #\b #\c #\d) (start 1) (past 3)))))
    => '(#\b #\c))

  (check
      (cadr (with-result
	     (subvector-for-each add-result '#())))
    => '())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (subvector-for-each/with-index (lambda (i c) (add-result (list i c)))
					    '#(#\a #\b #\c #\d))))
    => '((0 #\a)
	 (1 #\b)
	 (2 #\c)
	 (3 #\d)))

  (check
      (cadr (with-result
	     (subvector-for-each/with-index (lambda (i c) (add-result (list i c)))
					    (view '#(#\a #\b #\c #\d) (start 1) (past 3)))))
    => '((1 #\b) (2 #\c)))

  (check
      (cadr (with-result
	     (subvector-for-each/with-index (lambda (i c) (add-result (list i c))) '#())))
    => '())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (subvector-for-each-index add-result
				       '#(#\a #\b #\c #\d))))
    => '(0 1 2 3))

  (check
      (cadr (with-result
	     (subvector-for-each-index add-result
				       (view '#(#\a #\b #\c #\d) (start 1) (past 3)))))
    => '(1 2))

  (check
      (cadr (with-result
	     (subvector-for-each-index add-result '#())))
    => '())

  #f)


(parameterise ((check-test-name 'mapping-syntax))

  (check
      (vector-map/stx char-upcase
		      '#(#\a #\b #\c #\d))
    => '#(#\A #\B #\C #\D))


  (check
      (vector-map/stx char-upcase
		      '#(#\a #\b #\c #\d))
    => '#(#\A #\B #\C #\D))

  (check
      (vector-map/stx list
		      '#(#\a #\b #\c #\d)
		      '#(#\0 #\1 #\2 #\3))
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (vector-map/stx char-upcase '#())
    => '#())

  (check
      (vector-map/stx char-upcase '#() '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-map*/stx char-upcase '#(#\a #\b #\c #\d))
    => '#(#\A #\B #\C #\D))


  (check
      (vector-map*/stx char-upcase '#(#\a #\b #\c #\d))
    => '#(#\A #\B #\C #\D))

  (check
      (vector-map*/stx list
		       '#(#\a #\b #\c #\d #\e)
		       '#(#\0 #\1 #\2 #\3))
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (vector-map*/stx char-upcase '#())
    => '#())

  (check
      (vector-map*/stx char-upcase '#() '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map!/stx char-upcase vec)
	vec)
    => '#(#\A #\B #\C #\D))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map!/stx list vec '#(#\0 #\1 #\2 #\3))
	vec)
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (let ((vec (vector-copy '#())))
	(vector-map!/stx list vec)
	vec)
    => '#())

  (check
      (let ((vec (vector-copy '#())))
	(vector-map!/stx list vec '#())
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map*!/stx char-upcase vec)
	vec)
    => '#(#\A #\B #\C #\D))

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map*!/stx list vec '#(#\0 #\1 #\2 #\3 #\4 #\5))
	vec)
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (let ((vec (vector-copy '#())))
	(vector-map*!/stx list vec)
	vec)
    => '#())

  (check
      (let ((vec (vector-copy '#())))
	(vector-map*!/stx list vec '#())
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (vector-for-each/stx add-result '#(#\a #\b #\c #\d))
	     #t)) ;this is required because for-each returns nothing
    => '(#\a #\b #\c #\d))

  (check
      (cadr (with-result
	     (vector-for-each/stx (lambda (item-a item-b)
				    (add-result (list item-a item-b)))
				  '#(#\a #\b #\c #\d)
				  '#(#\0 #\1 #\2 #\3))
	     #t)) ;this is required because for-each returns nothing
    => '((#\a #\0)
	 (#\b #\1)
	 (#\c #\2)
	 (#\d #\3)))

  (check
      (cadr (with-result
	     (vector-for-each* add-result '#())
	     #t)) ;this is required because for-each returns nothing
    => '())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (vector-for-each*/stx add-result '#(#\a #\b #\c #\d))
	     #t)) ;this is required because for-each returns nothing
    => '(#\a #\b #\c #\d))

  (check
      (cadr (with-result
	     (vector-for-each*/stx (lambda (item-a item-b) (add-result (list item-a item-b)))
				   '#(#\a #\b #\c #\d #\e #\f)
				   '#(#\0 #\1 #\2 #\3))
	     #t)) ;this is required because for-each returns nothing
    => '((#\a #\0)
	 (#\b #\1)
	 (#\c #\2)
	 (#\d #\3)))

  (check
      (cadr (with-result
	     (vector-for-each*/stx add-result '#())
	     #t))  ;this is required because for-each returns nothing
    => '())

  )


(parameterise ((check-test-name 'fold-left))

  (check
      (subvector-fold-left xcons '() '#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (subvector-fold-left xcons '() (view '#(#\a #\b #\c #\d) (start 1) (past 3)))
    => '(#\c #\b))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left (lambda (nil x) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold-left (lambda (nil x y) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (vector-fold-left (lambda (nil x) (cons x nil))
	'()
	'#())
    => '())

  (check
      (vector-fold-left (lambda (count c)
			  (if (char-upper-case? c)
			      (+ count 1)
			    count))
	0
	'#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left* (lambda (nil x) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold-left* (lambda (nil x y) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D #\E))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (vector-fold-left* (lambda (nil x) (cons x nil))
	'()
	'#())
    => '())

  (check
      (vector-fold-left* (lambda (count c)
			   (if (char-upper-case? c)
			       (+ count 1)
			     count))
	0
	'#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left/with-index (lambda (i nil x) (cons x nil))
				   '()
				   '#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold-left/with-index (lambda (i nil x y) (cons (cons x y) nil))
				   '()
				   '#(#\a #\b #\c #\d)
				   '#(#\A #\B #\C #\D))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (vector-fold-left/with-index (lambda (i nil x) (cons x nil))
				   '()
				   '#())
    => '())

  (check
      (vector-fold-left/with-index (lambda (i count c)
				     (if (char-upper-case? c)
					 (+ count 1)
				       count))
				   0
				   '#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left*/with-index (lambda (i nil x) (cons x nil))
				    '()
				    '#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold-left*/with-index (lambda (i nil x y) (cons (cons x y) nil))
				    '()
				    '#(#\a #\b #\c #\d)
				    '#(#\A #\B #\C #\D #\E))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (vector-fold-left*/with-index (lambda (i nil x) (cons x nil))
				    '()
				    '#())
    => '())

  (check
      (vector-fold-left*/with-index (lambda (i count c)
				      (if (char-upper-case? c)
					  (+ count 1)
					count))
				    0
				    '#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

  )


(parameterise ((check-test-name 'fold-right))

  (check
      (subvector-fold-right cons '() '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (subvector-fold-right cons '() (view '#(#\a #\b #\c #\d)
					   (start 1)
					   (past 3)))
    => '(#\b #\c))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right (lambda (x nil) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right (lambda (x y nil) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (vector-fold-right (lambda (x nil) (cons x nil))
	'()
	'#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right/with-index (lambda (i x nil) (cons x nil))
				    '()
				    '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right/with-index (lambda (i x y nil) (cons (cons x y) nil))
				    '()
				    '#(#\a #\b #\c #\d)
				    '#(#\A #\B #\C #\D))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (vector-fold-right/with-index (lambda (i x nil) (cons x nil))
				    '()
				    '#())
    => '())

;;; --------------------------------------------------------------------


  (check
      (vector-fold-right* (lambda (x nil) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right* (lambda (x y nil) (cons (cons x y) nil))

	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D #\E))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (vector-fold-right* (lambda (x nil) (cons x nil))
	'()
	'#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right*/with-index (lambda (i x nil) (cons x nil))
				     '()
				     '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right*/with-index (lambda (i x y nil) (cons (cons x y) nil))
				     '()
				     '#(#\a #\b #\c #\d)
				     '#(#\A #\B #\C #\D #\E))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (vector-fold-right*/with-index (lambda (i x nil) (cons x nil))
				     '()
				     '#())
    => '())

  )


(parameterise ((check-test-name 'fold-left-syntax))

  (check
      (vector-fold-left/stx (lambda (nil x) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold-left/stx (lambda (nil x y) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (vector-fold-left/stx (lambda (nil x) (cons x nil))
	'()
	'#())
    => '())

  (check
      (vector-fold-left/stx (lambda (count c)
			      (if (char-upper-case? c)
				  (+ count 1)
				count))
	0
	'#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (vector-fold-left*/stx (lambda (nil x) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold-left*/stx (lambda (nil x y) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D #\E))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (vector-fold-left*/stx (lambda (nil x) (cons x nil))
	'()
	'#())
    => '())

  (check
      (vector-fold-left*/stx (lambda (count c)
			       (if (char-upper-case? c)
				   (+ count 1)
				 count))
	0
	'#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

  )


(parameterise ((check-test-name 'fold-right-syntax))

  (check
      (vector-fold-right/stx (lambda (x nil) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right/stx (lambda (x y nil) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (vector-fold-right/stx (lambda (x nil) (cons x nil))
	'()
	'#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right*/stx (lambda (x nil) (cons x nil))
			      '()
			      '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right*/stx (lambda (x y nil) (cons (cons x y) nil))

			      '()
			      '#(#\a #\b #\c #\d)
			      '#(#\A #\B #\C #\D #\E))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (vector-fold-right*/stx (lambda (x nil) (cons x nil))
			      '()
			      '#())
    => '())

  )


(parameterise ((check-test-name 'unfold))

  (check
      (vector-unfold null? car cdr '(#\a #\b #\c #\d))
    => '#(#\a #\b #\c #\d))

  (check
      (vector-unfold null? car cdr '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-unfold-right null? car cdr '(#\a #\b #\c #\d))
    => '#(#\d #\c #\b #\a))

  (check
      (vector-unfold-right null? car cdr '())
    => '#())

  )


(parameterise ((check-test-name 'selecting))

  (check
      (vector-copy '#(0 1 2 3 4 5 6 7 8 9))
    => '#(0 1 2 3 4 5 6 7 8 9))

  (check
      (vector-copy (view '#(0 1 2 3 4 5 6 7 8 9)
			 (start 4)
			 (past 8)))
    => '#(4 5 6 7))

  (check
      (vector-copy (view '#(0 1 2 3 4 5 6 7 8 9)
			 (start 4)
			 (past 4)))
    => '#())

  (check
      (vector-copy '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-reverse-copy '#(0 1 2 3 4 5 6 7 8 9))
    => '#(9 8 7 6 5 4 3 2 1 0))

  (check
      (vector-reverse-copy (view '#(0 1 2 3 4 5 6 7 8 9)
				 (start 4)
				 (past 8)))
    => '#(7 6 5 4))

  (check
      (vector-reverse-copy (view '#(0 1 2 3 4 5 6 7 8 9)
				 (start 4)
				 (past 4)))
    => '#())

  (check
      (vector-reverse-copy '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-take '#(#\a #\b #\c #\d) 2)
    => '#(#\a #\b))

  (check
      (vector-take '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-take '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-take-right '#(#\a #\b #\c #\d) 2)
    => '#(#\c #\d))

  (check
      (vector-take-right '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-take-right '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-drop '#(#\a #\b #\c #\d) 2)
    => '#(#\c #\d))

  (check
      (vector-drop '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-drop '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-drop-right '#(#\a #\b #\c #\d) 2)
    => '#(#\a #\b))

  (check
      (vector-drop-right '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-drop-right '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-trim '#(#\A #\A #\A #\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim '#(#\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-trim-right '#(#\b #\c #\d #\A #\A #\A) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-right '#(#\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-right '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-trim-both '#(#\A #\A #\A #\b #\c #\d #\A #\A #\A) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-both '#(#\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-both '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-pad '#(#\a #\b #\c) 3 #\0)
    => '#(#\a #\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 5 #\0)
    => '#(#\0 #\0 #\a #\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 5)
    => '#(#f #f #\a #\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 2 #\0)
    => '#(#\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 0 #\0)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-pad-right '#(#\a #\b #\c) 3 #\0)
    => '#(#\a #\b #\c))

  (check
      (vector-pad-right '#(#\a #\b #\c) 5 #\0)
    => '#(#\a #\b #\c #\0 #\0))

  (check
      (vector-pad-right '#(#\a #\b #\c) 2 #\0)
    => '#(#\a #\b))

  (check
      (vector-pad-right '#(#\a #\b #\c) 0 #\0)
    => '#())

  )


(parameterise ((check-test-name 'prefix))

  (check
      (vector-prefix-length '#(#\a #\b #\c #\d #\e #\f #\g)
			    '#(#\a #\b #\c #\d #\1 #\2 #\3)
			    char=?)
    => 4)

  (check
      (vector-prefix-length '#(#\a #\B #\c #\d #\e #\f #\g)
			    '#(#\a #\b #\c #\d #\1 #\2 #\3)
			    char=?)
    => 1)

  (check
      (vector-prefix-length '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-prefix-length '#(#\a) '#(#\a) char=?)
    => 1)

  (check
      (vector-prefix-length '#(1) '#(2) =)
    => 0)

  (check
      (vector-prefix-length '#() '#(#\a #\b #\c #\d #\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-prefix-length '#(#\a #\b #\c #\d #\e #\f #\g) '#() char=?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix-length '#(#\e #\f #\g #\a #\b #\c #\d)
			    '#(#\1 #\2 #\3 #\a #\b #\c #\d)
			    char=?)
    => 4)

  (check
      (vector-suffix-length '#(#\e #\f #\g #\a #\b #\c #\d)
			    '#(#\1 #\2 #\3 #\a #\b #\C #\d)
			    char=?)
    => 1)

  (check
      (vector-suffix-length '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-suffix-length '#(1) '#(1) =)
    => 1)

  (check
      (vector-suffix-length '#(1) '#(2) =)
    => 0)

  (check
      (vector-suffix-length '#() '#(#\a #\b #\c #\d #\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-suffix-length '#(#\a #\b #\c #\d #\e #\f #\g) '#() char=?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-prefix? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d #\1 #\2 #\3) char=?)
    => #t)

  (check
      (vector-prefix? '#(#\a #\b #\c #\d) '#(#\a #\B #\c #\d #\1 #\2 #\3) char=?)
    => #f)

  (check
      (vector-prefix? '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => #f)

  (check
      (vector-prefix? '#() '#(#\1 #\2 #\3) char=?)
    => #t)

  (check
      (vector-prefix? '#(#\e #\f #\g) '#() char=?)
    => #f)

  (check
      (vector-prefix? '#() '#() char=?)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix? '#(#\a #\b #\c #\d) '#(#\1 #\2 #\3 #\a #\b #\c #\d) char=?)
    => #t)

  (check
      (vector-suffix? '#(#\a #\b #\c #\d) '#(#\1 #\2 #\3 #\a #\B #\c #\d) char=?)
    => #f)

  (check
      (vector-suffix? '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => #f)

  (check
      (vector-suffix? '#() '#(#\1 #\2 #\3) char=?)
    => #t)

  (check
      (vector-suffix? '#(#\e #\f #\g) '#() char=?)
    => #f)

  (check
      (vector-suffix? '#() '#() char=?)
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (vector-index '#(#\a #\B #\c #\d) char-upper-case?)
    => 1)

  (check
      (vector-index (view '#(#\a #\B #\c #\d)
			  (start 1)) char-upper-case?)
    => 1)

  (check
      (vector-index '#(#\a #\b #\c #\d) char-upper-case?)
    => #f)

  (check
      (vector-index '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-index-right '#(#\a #\B #\c #\d) char-upper-case?)
    => 1)

  (check
      (vector-index-right (view '#(#\a #\B #\c #\d)
				(start 1))
			  char-upper-case?)
    => 1)

  (check
      (vector-index-right '#(#\a #\b #\c #\d) char-upper-case?)
    => #f)

  (check
      (vector-index-right '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip '#(#\B #\a #\c #\d) char-upper-case?)
    => 1)

  (check
      (vector-skip (view '#(#\B #\a #\c #\d)
			 (start 1)) char-upper-case?)
    => 1)

  (check
      (vector-skip '#(#\A #\B #\C #\D) char-upper-case?)
    => #f)

  (check
      (vector-skip '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip-right '#(#\a #\c #\d #\B) char-upper-case?)
    => 2)

  (check
      (vector-skip-right (view '#(#\a #\c #\d #\B)
			       (start 1))
			 char-upper-case?)
    => 2)

  (check
      (vector-skip-right '#(#\A #\B #\C #\D) char-upper-case?)
    => #f)

  (check
      (vector-skip-right '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-count '#(#\a #\B #\c #\A #\d) char-upper-case?)
    => 2)

  (check
      (vector-count (view '#(#\a #\B #\c #\d)
			  (start 1)) char-upper-case?)
    => 1)

  (check
      (vector-count '#(#\a #\b #\c #\d) char-upper-case?)
    => 0)

  (check
      (vector-count '#() char-upper-case?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-contains '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t)
		       '#(#\h #\e #\l #\l #\o)
		       char=?)
    => 5)

  (check
      (vector-contains '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t)
		       '#(#\h #\o #\l #\a)
		       char=?)
    => #f)

  (check
      (vector-contains '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t)
		       '#()
		       char=?)
    => 0)

  (check
      (vector-contains '#() '#(#\h #\e #\l #\l #\o) char=?)
    => #f)

;;; --------------------------------------------------------------------

  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 0 num-cmp) => 0)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 1 num-cmp) => 1)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 2 num-cmp) => 2)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 3 num-cmp) => 3)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 4 num-cmp) => 4)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 5 num-cmp) => 5)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 6 num-cmp) => 6)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 7 num-cmp) => 7)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 8 num-cmp) => 8)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9) 9 num-cmp) => 9)

  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  0 num-cmp) => 0)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  1 num-cmp) => 1)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  2 num-cmp) => 2)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  3 num-cmp) => 3)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  4 num-cmp) => 4)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  5 num-cmp) => 5)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  6 num-cmp) => 6)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  7 num-cmp) => 7)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  8 num-cmp) => 8)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10)  9 num-cmp) => 9)
  (check (vector-binary-search '#(0 1 2 3 4 5 6 7 8 9 10) 10 num-cmp) => 10)

  )


(parameterise ((check-test-name 'filtering))

  (check
      (vector-delete '#(#\a #\B #\c #\B #\d) char-upper-case?)
    => '#(#\a #\c #\d))

  (check
      (vector-delete '#(#\a #\b #\c #\b #\d) char-upper-case?)
    => '#(#\a #\b #\c #\b #\d))

  (check
      (vector-delete '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-filter '#(#\a #\B #\c #\B #\d) char-upper-case?)
    => '#(#\B #\B))

  (check
      (vector-filter '#(#\a #\b #\c #\b #\d) char-upper-case?)
    => '#())

  (check
      (vector-filter '#() char-upper-case?)
    => '#())

  )


(parameterise ((check-test-name 'lists))

  (check
      (vector->list* '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector->list* (view '#(#\a #\b #\c #\d)
			   (start 1)
			   (past 3)))
    => '(#\b #\c))

  (check
      (vector->list* '#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-list->vector '(#\a #\b #\c #\d))
    => '#(#\d #\c #\b #\a))

  (check
      (reverse-list->vector '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (reverse-vector->list '#(#\d #\c #\b #\a))
    => '(#\a #\b #\c #\d))

  (check
      (reverse-vector->list '#(#\a))
    => '(#\a))

  (check
      (reverse-vector->list '#())
    => '())

  )


(parameterise ((check-test-name 'xsubvector))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 0 5)
    => '#(#\c #\i #\a #\o #\space))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 0 9)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) -5 5)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o #\space))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 2 4)
    => '#(#\a #\o))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) -3 7)
    => '#(#\a #\o #\space #\c #\i #\a #\o #\space #\c #\i))

  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 1 7)
    => '#(#\b #\c #\d #\e #\f #\a))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 2 8)
    => '#(#\c #\d #\e #\f #\a #\b))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 3 9)
    => '#(#\d #\e #\f #\a #\b #\c))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 4 10)
    => '#(#\e #\f #\a #\b #\c #\d))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 5 11)
    => '#(#\f #\a #\b #\c #\d #\e))

  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -1 5)
    => '#(#\f #\a #\b #\c #\d #\e))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -2 4)
    => '#(#\e #\f #\a #\b #\c #\d ))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -3 3)
    => '#(#\d #\e #\f #\a #\b #\c))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -4 2)
    => '#(#\c #\d #\e #\f #\a #\b))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -5 1)
    => '#(#\b #\c #\d #\e #\f #\a))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 3 3)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(xsubvector '#() 0 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) 0 5)
	result)
    => '#(#\c #\i #\a #\o #\space))

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) 0 9)
	result)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o))

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) -5 5)
	result)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o #\space))

  (check
      (let ((result (vector-copy '#(#\0 #\1))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) 2 4)
	result)
    => '#(#\a #\o))

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) -3 7)
	result)
    => '#(#\a #\o #\space #\c #\i #\a #\o #\space #\c #\i))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	  (vector-xcopy! '#() '#() 0 5))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-fill*! vec #\b)
	vec)
    => '#(#\b #\b #\b #\b))

  (check
      (let* ((vec (vector-copy '#(#\a #\c #\c #\d))))
	(vector-fill*! (view vec (start 1) (past 3)) #\b)
	vec)
    => '#(#\a #\b #\b #\d))

  (check
      (let* ((vec (vector-copy '#())))
	(vector-fill*! (view vec (start 0) (past 0)) #\b)
	vec)
    => '#())

  )


(parameterise ((check-test-name 'reverse))

  (check
      (vector-reverse '#(#\a #\b #\c #\d))
    => '#(#\d #\c #\b #\a))

  (check
      (vector-reverse '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-reverse! vec)
	vec)
    => '#(#\d #\c #\b #\a))

  (check
      (let* ((vec (vector-copy '#())))
	(vector-reverse! vec)
	vec)
    => '#())

  )


(parameterise ((check-test-name 'replace))

  (check
      (vector-replace '#(#\a #\b #\c #\d) '#(#\1 #\2 #\3 #\4))
    => '#(#\1 #\2 #\3 #\4))

  (check
      (vector-replace (view '#(#\a #\b #\c #\d) (start 2) (past 2)) '#(#\1 #\2 #\3 #\4))
    => '#(#\a #\b #\1 #\2 #\3 #\4 #\c #\d))

  (check
      (vector-replace (view '#(#\a #\b #\c #\d) (start 2) (past 2)) '#())
    => '#(#\a #\b #\c #\d))

  (check
      (vector-replace (view '#(#\a #\b #\c #\d) (start 1) (past 3)) '#(#\1 #\2 #\3 #\4))
    => '#(#\a #\1 #\2 #\3 #\4 #\d))

  (check
      (vector-replace (view '#(#\a #\b #\c #\d) (start 0) (past 3)) '#(#\1 #\2 #\3 #\4))
    => '#(#\1 #\2 #\3 #\4 #\d))

  (check
      (vector-replace (view '#(#\a #\b #\c #\d) (start 1) (past 4)) '#(#\1 #\2 #\3 #\4))
    => '#(#\a #\1 #\2 #\3 #\4))

  )


(parameterise ((check-test-name 'mutating))

  (check
      (let* ((vec (vector-copy '#(#\1 #\2))))
	;; not enough room in destination vector
	;;(vector-copy! (vec 3) (view '#(#\a #\b #\c #\d) (past 2)))
	(guard (exc ((assertion-violation? exc) #t))
	  (vector-copy! (view vec (start 3))
			(view '#(#\a #\b #\c #\d) (past 2)))))
    => #t)

  (check
      ;; whole vector copy
      (let* ((vec (vector-copy '#(#\1 #\2 #\3))))
	(vector-copy! vec '#(#\a #\b #\c))
	vec)
    => '#(#\a #\b #\c))

  (check
      ;; zero-elements vector copy
      (let* ((vec (vector-copy '#(#\1 #\2 #\3))))
	(vector-copy! vec (view '#(#\a #\b #\c) (start 2) (past 2)))
	vec)
    => '#(#\1 #\2 #\3))

  (check
      ;; one-element vector copy
      (let* ((vec (vector-copy '#(#\1 #\2 #\3))))
	(vector-copy! vec (view '#(#\a #\b #\c) (start 1) (past 2)))
	vec)
    => '#(#\b #\2 #\3))

  (check
      ;; two-elements vector copy
      (let* ((vec (vector-copy '#(#\1 #\2))))
	(vector-copy! vec (view '#(#\a #\b #\c #\d) (past 2)))
	vec)
    => '#(#\a #\b))

  (check
      (let ((vec '#()))
	(vector-copy! vec (view '#(#\a #\b #\c #\d) (start 0) (past 0)))
	vec)
    => '#())

  (check
      ;; over the same vector, full
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-copy! vec vec)
	vec)
    => '#(0 1 2 3 4 5 6 7 8 9))

  (check
      ;; over the same vector, in place
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-copy! (view vec (start 5))
		      (view vec (start 5)))
	vec)
    => '#(0 1 2 3 4 5 6 7 8 9))

  (check
      ;; over the same vector, backwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-copy! (view vec (start 2))
		      (view vec (start 4) (past 8)))
	vec)
    => '#(0 1 4 5 6 7 6 7 8 9))

  (check
      ;; over the same vector, backwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-copy! (view vec (start 0))
		      (view vec (start 4) (past 8)))
	vec)
    => '#(4 5 6 7 4 5 6 7 8 9))

  (check
      ;; over the same vector, forwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-copy! (view vec (start 4))
		      (view vec (start 2) (past 6)))
	vec)
    => '#(0 1 2 3 2 3 4 5 8 9))

  (check
      ;; over the same vector, forwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-copy! (view vec (start 6))
		      (view vec (start 2) (past 6)))
	vec)
    => '#(0 1 2 3 4 5 2 3 4 5))

;;; --------------------------------------------------------------------

  (check
      (let* ((vec (vector-copy '#(#\1 #\2))))
	;; not enough room in destination vector
	;;(vector-reverse-copy! (vec 3) (view '#(#\a #\b #\c #\d) (past 2)))
	(guard (exc ((assertion-violation? exc) #t))
	  (vector-reverse-copy! (view vec (start 3))
				(view '#(#\a #\b #\c #\d) (past 2)))))
    => #t)

  (check
      ;; whole vector copy
      (let* ((vec (vector-copy '#(#\1 #\2 #\3))))
	(vector-reverse-copy! vec '#(#\a #\b #\c))
	vec)
    => '#(#\c #\b #\a))

  (check
      ;; zero-elements vector copy
      (let* ((vec (vector-copy '#(#\1 #\2 #\3))))
	(vector-reverse-copy! vec (view '#(#\a #\b #\c) (start 2) (past 2)))
	vec)
    => '#(#\1 #\2 #\3))

  (check
      ;; one-element vector copy
      (let* ((vec (vector-copy '#(#\1 #\2 #\3))))
	(vector-reverse-copy! vec (view '#(#\a #\b #\c) (start 1) (past 2)))
	vec)
    => '#(#\b #\2 #\3))

  (check
      ;; two-elements vector copy
      (let* ((vec (vector-copy '#(#\1 #\2))))
	(vector-reverse-copy! vec (view '#(#\a #\b #\c #\d) (past 2)))
	vec)
    => '#(#\b #\a))

  (check
      (let ((vec '#()))
	(vector-reverse-copy! vec (view '#(#\a #\b #\c #\d) (start 0) (past 0)))
	vec)
    => '#())

  (check
      ;; over the same vector, full
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-reverse-copy! vec vec)
	vec)
    => '#(9 8 7 6 5 4 3 2 1 0))

  (check
      ;; over the same vector
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-reverse-copy! (view vec (start 5))
			      (view vec (start 5)))
	vec)
    => '#(0 1 2 3 4 9 8 7 6 5))

  (check
      ;; over the same vector, backwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-reverse-copy! (view vec (start 2))
			      (view vec (start 4) (past 8)))
	vec)
    => '#(0 1 7 6 5 4 6 7 8 9))

  (check
      ;; over the same vector, backwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-reverse-copy! (view vec (start 0))
			      (view vec (start 4) (past 8)))
	vec)
    => '#(7 6 5 4 4 5 6 7 8 9))

  (check
      ;; over the same vector, forwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-reverse-copy! (view vec (start 4))
			      (view vec (start 2) (past 6)))
	vec)
    => '#(0 1 2 3 5 4 3 2 8 9))

  (check
      ;; over the same vector, forwards
      (let* ((vec (vector-copy '#(0 1 2 3 4 5 6 7 8 9))))
	(vector-reverse-copy! (view vec (start 6))
			      (view vec (start 2) (past 6)))
	vec)
    => '#(0 1 2 3 4 5 5 4 3 2))

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(0 1 2 3 4 5))))
	(vector-swap! vec 2 4)
	vec)
    => '#(0 1 4 3 2 5))

  (check
      (let ((vec (vector-copy '#(0 1 2 3 4 5))))
	(vector-swap! vec 2 2)
	vec)
    => '#(0 1 2 3 4 5))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-swap! '#() 0 1))
    => #t)

  )


;;;; done

(check-report)

;;; end of file
