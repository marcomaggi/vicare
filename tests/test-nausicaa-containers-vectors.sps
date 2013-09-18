;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for label <xvector>
;;;Date: Wed Sep 18, 2013
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
(import (nausicaa)
  (nausicaa containers vectors)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: label <xvector>\n")


;;;; helpers

(define (num-cmp a b)
  (cond ((> a b) +1)
	((< a b) -1)
	(else     0)))

(define (xcons d a)
  (cons a d))


(parametrise ((check-test-name	'constructors))

  (check
      (let (((o <xvector>) '#(#\c #\i #\a #\o)))
	(o concatenate '(#(#\space)
			 #(#\h #\e #\l #\l #\o) #(#\space)
			 #(#\s #\a #\l #\u #\t))))
    => '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t))

  (check
      (let (((o <xvector>) '#(#\c #\i #\a #\o)))
	(o concatenate-reverse '(#(#\space)
				 #(#\h #\e #\l #\l #\o) #(#\space)
				 #(#\s #\a #\l #\u #\t))
	   '#(#\space #\h #\o #\l #\a)
	   3))
    => '#(#\s #\a #\l #\u #\t #\space #\h #\e #\l #\l #\o #\space #\c #\i #\a #\o #\space #\h #\o))

  (check
      (let (((o <xvector>) '#(0 1 2 3)))
	(o append '#(4 5 6 7 8)))
    => '#(0 1 2 3 4 5 6 7 8))

  #t)


(parameterise ((check-test-name 'predicates))

  (check
      (let (((o <xvector>) '#(0 1 2 3)))
	(o null?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <xvector>) '#(#\a #\b #\c)))
	(o every char-alphabetic?))
    => #t)

  (check
      (let (((o <xvector>) '#(1 2 3)))
	(o every = '#(1 2 3)))
    => #t)

  (check
      (let (((o <xvector>) '#(1 2 3)))
	(o every = '#(1 2 3) '#(1 2 3)))
    => #t)

  (check
      (let (((o <xvector>) '#(1 2 3)))
	(o every = '#(1 2 3) '#(9 9 9)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <xvector>) '#(#\1 #\2 #\a #\4)))
	(o any char-alphabetic?))
    => #t)

  #t)


(parameterise ((check-test-name 'comparison))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c)))
	(o = char=? '#(#\a #\b #\c)))
    => #t)

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o <> char=?       '#(#\a #\b #\c #\d)))
    => #f)

  (check-for-true
   (let (((o <xvector>) '#(#\a #\b #\c)))
     (o < char=? char<? '#(#\a #\b #\c #\d))))

  (check-for-true
   (let (((o <xvector>) '#(#\a #\b #\c #\d)))
     (o <= char=? char<? '#(#\a #\b #\c #\d))))

  (check-for-true
   (let (((o <xvector>) '#(#\a #\b #\c #\d)))
     (o > char=? char<? '#(#\a #\b #\c))))

  (check-for-true
   (let (((o <xvector>) '#(#\a #\b #\c #\d)))
     (o >= char=? char<? '#(#\a #\b #\c #\d))))

  #t)


(parameterise ((check-test-name 'mapping))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o map/with-index (lambda (i c) (list i (char-upcase c)))))
    => '#((0 #\A)
	  (1 #\B)
	  (2 #\C)
	  (3 #\D)))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o map! char-upcase)
	o)
    => '#(#\A #\B #\C #\D))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o map!/with-index list)
	o)
    => '#((0 #\a)
	  (1 #\b)
	  (2 #\c)
	  (3 #\d)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o map* char-upcase))
    => '#(#\A #\B #\C #\D))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o map*/with-index (lambda (i c) (list i (char-upcase c)))))
    => '#((0 #\A)
	  (1 #\B)
	  (2 #\C)
	  (3 #\D)))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o map*! char-upcase)
	o)
    => '#(#\A #\B #\C #\D))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o map*!/with-index (lambda (i item-a item-b) (list i item-a item-b))
	   '#(#\0 #\1 #\2 #\3 #\4))
	o)
    => '#((0 #\a #\0)
	  (1 #\b #\1)
	  (2 #\c #\2)
	  (3 #\d #\3)))

  (check
      (cadr (with-result
	     (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	       (o for-each* (lambda (item-a item-b) (add-result (list item-a item-b)))
		  '#(#\0 #\1 #\2 #\3)))))
    => '((#\a #\0)
	 (#\b #\1)
	 (#\c #\2)
	 (#\d #\3)))

  (check
      (cadr (with-result
	     (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	       (o for-each*/with-index
		  (lambda (i item-a item-b) (add-result (list i item-a item-b)))
		  '#(#\0 #\1 #\2 #\3)))))
    => '((0 #\a #\0)
	 (1 #\b #\1)
	 (2 #\c #\2)
	 (3 #\d #\3)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o subvector-map char-upcase))
    => '#(#\A #\B #\C #\D))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o subvector-map/with-index (lambda (i c) (list i c))))
    => '#((0 #\a)
	  (1 #\b)
	  (2 #\c)
	  (3 #\d)))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o subvector-map! char-upcase)
	o)
    => '#(#\A #\B #\C #\D))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o subvector-map!/with-index (lambda (i c) (list i c)))
	o)
    => '#((0 #\a)
	  (1 #\b)
	  (2 #\c)
	  (3 #\d)))

  (check
      (cadr (with-result
	     (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	       (o subvector-for-each add-result))))
    => '(#\a #\b #\c #\d))

  (check
      (cadr (with-result
	     (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	       (o subvector-for-each/with-index (lambda (i c) (add-result (list i c)))))))
    => '((0 #\a)
	 (1 #\b)
	 (2 #\c)
	 (3 #\d)))

  (check
      (cadr (with-result
	     (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	       (o subvector-for-each-index add-result))))
    => '(0 1 2 3))

  #t)


(parameterise ((check-test-name 'mapping-syntax))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o map/stx list '#(#\0 #\1 #\2 #\3)))
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d #\e)))
	(o map*/stx list '#(#\0 #\1 #\2 #\3)))
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o map!/stx list '#(#\0 #\1 #\2 #\3))
	o)
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o map*!/stx list '#(#\0 #\1 #\2 #\3 #\4 #\5))
	o)
    => '#((#\a #\0)
	  (#\b #\1)
	  (#\c #\2)
	  (#\d #\3)))

  (check
      (cadr (with-result
	     (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	       (o for-each/stx (lambda (item-a item-b)
				 (add-result (list item-a item-b)))
		  '#(#\0 #\1 #\2 #\3)))
	     #t)) ;this is required because for-each returns nothing
    => '((#\a #\0)
	 (#\b #\1)
	 (#\c #\2)
	 (#\d #\3)))

  (check
      (cadr (with-result
	     (let (((o <xvector>) '#(#\a #\b #\c #\d #\e #\f)))
	       (o for-each*/stx (lambda (item-a item-b) (add-result (list item-a item-b)))
		  '#(#\0 #\1 #\2 #\3)))
	     #t)) ;this is required because for-each returns nothing
    => '((#\a #\0)
	 (#\b #\1)
	 (#\c #\2)
	 (#\d #\3)))

  #t)


(parameterise ((check-test-name 'fold-left))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o subvector-fold-left xcons '()))
    => '(#\d #\c #\b #\a))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-left (lambda (nil x y) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D)))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-left* (lambda (nil x y) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D #\E)))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-left/with-index (lambda (i nil x y) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D)))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-left*/with-index (lambda (i nil x y) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D #\E)))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))


  #t)


(parameterise ((check-test-name 'fold-right))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o subvector-fold-right cons '()))
    => '(#\a #\b #\c #\d))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-right (lambda (x y nil) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D)))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-right/with-index (lambda (i x y nil) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D)))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-right* (lambda (x y nil) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D #\E)))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-right*/with-index (lambda (i x y nil) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D #\E)))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  #t)


(parameterise ((check-test-name 'fold-left-syntax))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-left/stx (lambda (nil x y) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D)))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-left*/stx (lambda (nil x y) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D #\E)))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-right/stx (lambda (x y nil) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D)))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o fold-right*/stx (lambda (x y nil) (cons (cons x y) nil))
	   '()
	   '#(#\A #\B #\C #\D #\E)))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  #t)


(parameterise ((check-test-name 'selecting))

  (check
      (let (((o <xvector>) '#(0 1 2 3 4 5 6 7 8 9)))
	(o copy))
    => '#(0 1 2 3 4 5 6 7 8 9))

  (check
      (let (((o <xvector>) '#(0 1 2 3 4 5 6 7 8 9)))
	(o reverse-copy))
    => '#(9 8 7 6 5 4 3 2 1 0))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o take 2))
    => '#(#\a #\b))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o take-right  2))
    => '#(#\c #\d))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o drop  2))
    => '#(#\c #\d))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o drop-right  2))
    => '#(#\a #\b))

  (check
      (let (((o <xvector>) '#(#\A #\A #\A #\b #\c #\d)))
	(o trim  char-upper-case?))
    => '#(#\b #\c #\d))

  (check
      (let (((o <xvector>) '#(#\b #\c #\d #\A #\A #\A)))
	(o trim-right  char-upper-case?))
    => '#(#\b #\c #\d))

  (check
      (let (((o <xvector>) '#(#\A #\A #\A #\b #\c #\d #\A #\A #\A)))
	(o trim-both  char-upper-case?))
    => '#(#\b #\c #\d))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c)))
	(o pad  5 #\0))
    => '#(#\0 #\0 #\a #\b #\c))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c)))
	(o pad-right  5 #\0))
    => '#(#\a #\b #\c #\0 #\0))

  #t)


(parameterise ((check-test-name 'prefix))

  (check
      (let (((o <xvector>) '#(#\a #\B #\c #\d #\e #\f #\g)))
	(o prefix-length '#(#\a #\b #\c #\d #\1 #\2 #\3)
	   char=?))
    => 1)

  (check
      (let (((o <xvector>) '#(#\e #\f #\g #\a #\b #\c #\d)))
	(o suffix-length '#(#\1 #\2 #\3 #\a #\b #\c #\d)
	   char=?))
    => 4)

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o prefix?  '#(#\a #\b #\c #\d #\1 #\2 #\3) char=?))
    => #t)

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o suffix?  '#(#\1 #\2 #\3 #\a #\b #\c #\d) char=?))
    => #t)

  #t)


(parameterise ((check-test-name 'searching))

  (check
      (let (((o <xvector>) '#(#\a #\B #\c #\d)))
	(o index  char-upper-case?))
    => 1)

  (check
      (let (((o <xvector>) '#(#\a #\B #\c #\d)))
	(o index-right  char-upper-case?))
    => 1)

  (check
      (let (((o <xvector>) '#(#\B #\a #\c #\d)))
	(o skip char-upper-case?))
    => 1)

  (check
      (let (((o <xvector>) '#(#\a #\c #\d #\B)))
	(o skip-right  char-upper-case?))
    => 2)

  (check
      (let (((o <xvector>) '#(#\a #\B #\c #\A #\d)))
	(o count  char-upper-case?))
    => 2)

  (check
      (let (((o <xvector>) '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t)))
	(o contains '#(#\h #\e #\l #\l #\o)
	   char=?))
    => 5)

  (check
      (let (((o <xvector>) '#(0 1 2 3 4 5 6 7 8 9)))
	(o binary-search  0 num-cmp))
    => 0)

  #t)


(parameterise ((check-test-name 'filtering))

  (check
      (let (((o <xvector>) '#(#\a #\B #\c #\B #\d)))
	(o delete  char-upper-case?))
    => '#(#\a #\c #\d))

  (check
      (let (((o <xvector>) '#(#\a #\B #\c #\B #\d)))
	(o filter  char-upper-case?))
    => '#(#\B #\B))


  #t)


(parameterise ((check-test-name 'lists))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o list*))
    => '(#\a #\b #\c #\d))

  (check
      (let (((o <xvector>) '#(#\d #\c #\b #\a)))
	(o reverse-vector->list))
    => '(#\a #\b #\c #\d))

  #t)


(parameterise ((check-test-name 'xsubvector))

  (check
      (let (((o <xvector>) '#(#\c #\i #\a #\o #\space)))
	(o xsubvector  0 5))
    => '#(#\c #\i #\a #\o #\space))

  (check
      (let (((o <xvector>) (vector-copy '#(#\0 #\1 #\2 #\3 #\4))))
	(o xcopy! '#(#\c #\i #\a #\o #\space) 0 5)
	o)
    => '#(#\c #\i #\a #\o #\space))

  #t)


(parameterise ((check-test-name 'filling))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o fill*! #\b)
	o)
    => '#(#\b #\b #\b #\b))

  #t)


(parameterise ((check-test-name 'reverse))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o reverse))
    => '#(#\d #\c #\b #\a))

  (check
      (let (((o <xvector>) (vector-copy '#(#\a #\b #\c #\d))))
	(o reverse!)
	o)
    => '#(#\d #\c #\b #\a))

  #t)


(parameterise ((check-test-name 'replace))

  (check
      (let (((o <xvector>) '#(#\a #\b #\c #\d)))
	(o replace '#(#\1 #\2 #\3 #\4)))
    => '#(#\1 #\2 #\3 #\4))

  (check	; whole vector copy
      (let (((o <xvector>) (vector-copy '#(#\1 #\2 #\3))))
	(o copy! '#(#\a #\b #\c))
	o)
    => '#(#\a #\b #\c))

  (check	; whole vector copy
      (let (((o <xvector>) (vector-copy '#(#\1 #\2 #\3))))
	(o reverse-copy! '#(#\a #\b #\c))
	o)
    => '#(#\c #\b #\a))

  (check
      (let (((o <xvector>) (vector-copy '#(0 1 2 3 4 5))))
	(o swap! 2 4)
	o)
    => '#(0 1 4 3 2 5))

  #t)


;;;; done

(check-report)

;;; end of file
