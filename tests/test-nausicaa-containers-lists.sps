;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the label <xlist>
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
  (prefix (vicare containers lists) lists.)
  (nausicaa containers lists)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: label <xlist>\n")


(parametrise ((check-test-name	'constructors))

  (define ell '(1 2 3 4 5))

  (check
      (let (((o <xlist>) ell))
	(o list-copy))
    => ell)

  (check
      (let (((o <xlist>) ell))
	(o tree-copy))
    => ell)

  #t)


(parametrise ((check-test-name	'circular))

  (define ell '(1 2 3 4 5))

  (check
      (let (((o <xlist>) ell))
	(o list-copy))
    => ell)

  (check
      (let (((o <xlist>) ell))
	(o tree-copy))
    => ell)

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o circular-list!)
	(lists.circular-list? o))
    => #t)

  (check
      (let (((o <xlist>) (lists.list->circular-list! (lists.list-copy ell))))
	(o break-circular-list!)
	o)
    => ell)

  (check
      (let* ((ell1		(lists.list->circular-list! (lists.list-copy ell)))
	     ((o <xlist>)	ell1))
	(o circular-list-copy)
	(lists.circular-list= = o ell1))
    => #t)

  (check
      (let (((o <xlist>) (lists.list->circular-list! (lists.list-copy ell))))
	(o circular-list-length))
    => 5)

  #t)


(parametrise ((check-test-name	'predicates))

  (define ell '(1 2 3 4 5))

  (check
      (let (((o <xlist>) (lists.list->circular-list! (lists.list-copy ell))))
	(o circular-list?))
    => #t)

  (check
      (let (((o <xlist>) (lists.list->circular-list! (lists.list-copy ell))))
	(o circular-list?/or-null))
    => #t)

  (check
      (let (((o <xlist>) '(1 2 3 4 . 5)))
	(o dotted-list?))
    => #t)

  (check
      (let (((o <xlist>) '(1 2 3 4 . 5)))
	(o dotted-list?/or-null))
    => #t)

  #t)


(parametrise ((check-test-name	'selectors))

  (define ell '(0 1 2 3 4 5 6 7 8 9))

  (check
      (let (((o <xlist>) ell))
	(call-with-values
	    (lambda ()
	      (o car+cdr))
	  list))
    => '(0 (1 2 3 4 5 6 7 8 9)))

  (check (let (((o <xlist>) ell)) (o first))	=> 0)
  (check (let (((o <xlist>) ell)) (o second))	=> 1)
  (check (let (((o <xlist>) ell)) (o third))	=> 2)
  (check (let (((o <xlist>) ell)) (o fourth))	=> 3)
  (check (let (((o <xlist>) ell)) (o fifth))	=> 4)
  (check (let (((o <xlist>) ell)) (o sixth))	=> 5)
  (check (let (((o <xlist>) ell)) (o seventh))	=> 6)
  (check (let (((o <xlist>) ell)) (o eighth))	=> 7)
  (check (let (((o <xlist>) ell)) (o ninth))	=> 8)
  (check (let (((o <xlist>) ell)) (o tenth))	=> 9)

  (check
      (let (((o <xlist>) ell))
	(o take-left 5))
    => '(0 1 2 3 4))

  (check
      (let (((o <xlist>) ell))
	(o take-right 5))
    => '(5 6 7 8 9))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o take-left! 5)
	o)
    => '(0 1 2 3 4))

  (check
      (let (((o <xlist>) ell))
	(o drop-left 5))
    => '(5 6 7 8 9))

  (check
      (let (((o <xlist>) ell))
	(o drop-right 5))
    => '(0 1 2 3 4))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o drop-right! 5)
	o)
    => '(0 1 2 3 4))

  (check
      (let (((o <xlist>) ell))
	(call-with-values
	    (lambda ()
	      (o split-at 5))
	  list))
    => '((0 1 2 3 4)
	 (5 6 7 8 9)))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(call-with-values
	    (lambda ()
	      (o split-at! 5))
	  list))
    => '((0 1 2 3 4)
	 (5 6 7 8 9)))

  (check
      (let (((o <xlist>) ell))
	(o last))
    => 9)

  (check
      (let (((o <xlist>) ell))
	(o last-pair))
    => '(9))

  #t)


(parametrise ((check-test-name	'miscellaneous))

  (define ell '(0 1 2 3 4 5 6 7 8 9))

  (check
      (let (((o <xlist>) ell))
	(o length+))
    => 10)

  (check
      (let (((o <xlist>) ell))
	(o length+))
    => 10)

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o reverse!)
	o)
    => (reverse ell))

  (check
      (let (((o <xlist>) ell))
;	(o append-reverse '(a b c))
	(lists.append-reverse o '(a b c))
	)
    => '(9 8 7 6 5 4 3 2 1 0 a b c))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o append-reverse! '(a b c))
	o)
    => '(9 8 7 6 5 4 3 2 1 0 a b c))

  (check
      (let (((o <xlist>) '(one two three)))
	(o zip '(1 2 3) '(odd even odd)))
    => '((one 1 odd) (two 2 even) (three 3 odd)))

  (check
      (let (((o <xlist>) '(one two three)))
	(o zip* '(1 2 3) '(odd even odd)))
    => '((one 1 odd) (two 2 even) (three 3 odd)))

  (check
      (let (((o <xlist>) '((1 one)
			   (2 two)
			   (3 three))))
	(o unzip1))
    => '(1 2 3))

  (check
      (let (((o <xlist>) '((1 one)
			   (2 two)
			   (3 three))))
	(call-with-values
	    (lambda ()
	      (o unzip2))
	  list))
    => '((1 2 3)
	 (one two three)))

  (check
      (let (((o <xlist>) '((1 10 100)
			   (2 20 200)
			   (3 30 300))))
	(call-with-values
	    (lambda ()
	      (o unzip3))
	  list))
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)))

  (check
      (let (((o <xlist>) '((1 10 100 1000)
			   (2 20 200 2000)
			   (3 30 300 3000))))
	(call-with-values
	    (lambda ()
	      (o unzip4))
	  list))
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)
	 (1000 2000 3000)))

  (check
      (let (((o <xlist>) '((1 10 100 1000 10000)
			   (2 20 200 2000 20000)
			   (3 30 300 3000 30000))))
	(call-with-values
	    (lambda ()
	      (o unzip5))
	  list))
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)
	 (1000 2000 3000)
	 (10000 20000 30000)))

  (check
      (let (((o <xlist>) ell))
	(o count even?))
    => 5)

  #t)


(parametrise ((check-test-name	'fold))

  (define ell '(0 1 2 3 4 5 6 7 8 9))

  (check
      (let (((o <xlist>) ell))
	(o and-fold-left* 0 +))
    => 45)

  (check
      (let (((o <xlist>) ell))
	(o and-fold-right* 0 +))
    => 45)

  (check
      (let (((o <xlist>) ell))
	(o fold-left/pred -1 <))
    => 9)

  (check
      (let (((o <xlist>) ell))
	(o pair-fold 0 (lambda (val knil)
			 (+ knil (car val)))))
    => 45)

  (check
      (let (((o <xlist>) ell))
	(o pair-fold* 0 (lambda (val knil)
			  (+ knil (car val)))))
    => 45)

  (check
      (let (((o <xlist>) ell))
	(o reduce 0 max))
    => 9)

  (check
      (let (((o <xlist>) ell))
	(o reduce* 0 max))
    => 9)

  #t)


(parametrise ((check-test-name	'map))

  (define ell '(0 1 2 3 4 5 6 7 8 9))

  (check
      (let (((o <xlist>) ell))
	(o map-in-order* -))
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o map! -)
	o)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (let (((o <xlist>) '(1 2 3))
	    (r		 '()))
	(o pair-for-each (lambda (x)
			   (set! r (cons x r))))
	r)
    => '((3)
	 (2 3)
	 (1 2 3)))

  (check
      (let (((o <xlist>) '(a 1 b 3 c 7)))
	(o filter-map (lambda (x)
			(and (number? x)
			     (* x x)))))
    => '(1 9 49))

  #t)


(parametrise ((check-test-name	'filter))

  (define ell '(0 1 2 3 4 5 6 7 8 9))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o filter! even?)
	o)
    => '(0 2 4 6 8))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o remove* even?))
    => '(1 3 5 7 9))

  (check
      (let (((o <xlist>) (lists.list-copy ell)))
	(o remove*! even?)
	o)
    => '(1 3 5 7 9))

  #t)


(parametrise ((check-test-name	'searching))

  (define ell '(0 1 2 3 4 5 6 7 8 9))

  (check
      (let (((o <xlist>) '(1 2 3)))
	(o find-tail even?))
    => '(2 3))

  (check
      (let (((o <xlist>) '(2 4 6 1 3)))
	(o take-while even?))
    => '(2 4 6))

  (check
      (let (((o <xlist>) '(2 4 6 1 3)))
	(o take-while! even?)
	o)
    => '(2 4 6))

  (check
      (let (((o <xlist>) '(2 4 6 1 3)))
	(o drop-while even?))
    => '(1 3))

  (check
      (let (((o <xlist>) '(2 4 6 1 3)))
	(call-with-values
	    (lambda () (o span even?))
	  list))
    => '((2 4 6) (1 3)))

  (check
      (let (((o <xlist>) '(1 3 2 4 6)))
	(call-with-values
	    (lambda () (o break even?))
	  list))
    => '((1 3) (2 4 6)))

  (check
      (let (((o <xlist>) '(1 3 5 7 2)))
	(and (o any even?)
	     #t))
    => #t)

  (check
      (let (((o <xlist>) '(4 8 10 12)))
	(and (o every even?)
	     #t))
    => #t)

  (check
      (let (((o <xlist>) '(1 2 3 5)))
	(o list-index even?))
    => 1)

  (check
      (let (((o <xlist>) '(b (a) c)))
	(o member* '(a)))
    => '((a) c))

  (check
      (let (((o <xlist>) '(1 2 3 5)))
	(o position 2))
    => 1)

  #t)


(parametrise ((check-test-name	'deleting))

  (check
      (let (((o <xlist>) '(1 2 8 3 4 5 8 6 7 8)))
	(o delete 8))
    => '(1 2 3 4 5 6 7))

  (check
      (let (((o <xlist>) '(1 2 8 3 4 5 8 6 7 8)))
	(o delete! 8)
	o)
    => '(1 2 3 4 5 6 7))

  (check
      (let (((o <xlist>) '(1 2 3 2 4 5 4 6 1 7)))
	(o delete-duplicates))
    => '(1 2 3 4 5 6 7))

  (check
      (let (((o <xlist>) '(1 2 3 2 4 5 4 6 1 7)))
	(o delete-duplicates!)
	o)
    => '(1 2 3 4 5 6 7))

  #t)


(parametrise ((check-test-name	'sorting))

  (check
      (let (((o <xlist>)  '(0 1 2 3 4 6 7)))
	(o sorted-list-insert 5 >))
    => '(0 1 2 3 4 5 6 7))

  (check
      (let (((o <xlist>) (lists.list-copy '(0 1 2 3 4 6 7))))
	(o sorted-list-insert! 5 >)
	o)
    => '(0 1 2 3 4 5 6 7))

  (check
      (let (((o <xlist>) '(0 1 2 3 4 6 7)))
	(o sorted-list-insert/uniq 5 < >))
    => '(0 1 2 3 4 5 6 7))

  (check
      (let (((o <xlist>) '(0 1 2 3 4 6 7)))
	(o sorted-list-insert/uniq! 5 < >)
	o)
    => '(0 1 2 3 4 5 6 7))

  #t)


;;;; done

(check-report)

;;; end of file
