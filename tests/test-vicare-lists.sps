;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for list6
;;;Date: Tue Mar  1, 2011
;;;
;;;Abstract
;;;
;;;	This test file was originally in Nausicaa.
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (for (except (vicare)
		     break

		     ;; from (rnrs base (6))
		     pair?		cons
		     car		cdr
		     null?
		     caar		cdar
		     cadr		cddr
		     caaar		cdaar
		     caadr		cdadr
		     cadar		cddar
		     caddr		cdddr
		     caaaar		cdaaar
		     caaadr		cdaadr
		     caadar		cdadar
		     caaddr		cdaddr
		     cadaar		cddaar
		     cadadr		cddadr
		     caddar		cdddar
		     cadddr		cddddr
		     list?
		     list		length
		     append		reverse
		     list-tail		list-ref
		     map		for-each

		     ;; from (rnrs lists (6))
		     assoc		assp		assq
		     assv		cons*		exists
		     filter		find		fold-left
		     fold-right		for-all		member
		     memp		memq		memv
		     partition		remove		remp
		     remq		remv)
	     expand run)
  (lists)
  (prefix (vicare) six.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare lists functions\n")


;;;; helpers

(define numbers '(0 1 2 3 4 5 6 7 8 9))


;;;; miscellaneous tests on circular lists

(let ()

  (define end  (six.cons  1 '()))
  (define tail (six.cons* 3 2 end))
  (define ell  (six.cons* 5 4 tail))
  (six.set-cdr! end tail)

  (check (six.car ell)			=> 5)
  (check (six.cadr ell)			=> 4)
  (check (six.caddr ell)		=> 3)
  (check (six.cadddr ell)		=> 2)
  (check (six.cadddr (six.cdr ell))	=> 1)
  (check (six.cdddr (six.cddr ell))	=> tail)
  (check (six.cadddr (six.cddr ell))	=> 3)
  (check (six.cadddr (six.cdddr ell))	=> 2)

  ;;When  this test  is  run  with the  source  optimiser inserted:  the
  ;;mutation of  the pair must be  recognised; the CAR and  the CDR must
  ;;precomputed after the mutation.
  ;;
  (check
      (let ((b (six.cons 1 2)))
	(six.set-car! b 10)
	(six.set-cdr! b 20)
	(vector (six.car b) (six.cdr b)))
    => '#(10 20))

  ;;The following  test will fail  because we are mutating  a hard-coded
  ;;pair: the result is undefined.
  ;;
  ;; (check
  ;;     (let ((b '(1 . 2)))
  ;; 	(six.set-car! b 10)
  ;; 	(six.set-cdr! b 20)
  ;; 	(vector (six.car b) (six.cdr b)))
  ;;   => '#(10 20))

  #f)


(let ()		;constructors

  (check
      (six.cons* 1 2 3 4 '(5 6 7 8))
    => '(1 2 3 4 5 6 7 8))

;;; --------------------------------------------------------------------

  (check
      (xcons 1 2)
    => '(2 . 1))

;;; --------------------------------------------------------------------

  (check
      (six.make-list 4 'c)
    => '(c c c c))

  (check
      (six.make-list 0)
    => '())

  (check
      (six.make-list 0 #f)
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-tabulate 4 (lambda (i)
			 (six.cons i 'a)))
    => '((0 . a)
	 (1 . a)
	 (2 . a)
	 (3 . a)))

  (check
      (list-tabulate 1 (lambda (i)
			 (six.cons i 'a)))
    => '((0 . a)))

  (check
      (list-tabulate 0 (lambda (i)
			 (six.cons i 'a)))
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-tabulate/reverse 4 (lambda (i)
				 (six.cons i 'a)))
    => '((0 . a)
	 (1 . a)
	 (2 . a)
	 (3 . a)))

  (check
      (list-tabulate/reverse 1 (lambda (i)
				 (six.cons i 'a)))
    => '((0 . a)))

  (check
      (list-tabulate/reverse 0 (lambda (i)
				 (six.cons i 'a)))
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-copy '())
    => '())

  (check
      (list-copy '(1))
    => '(1))

  (check
      (list-copy numbers)
    => numbers)

  (check
      (list-copy '(1 . 2))
    => '(1 . 2))

  (check
      (list-copy '(1 2 3 . 4))
    => '(1 2 3 . 4))

;;; --------------------------------------------------------------------

  (check
      (tree-copy '())
    => '())

  (check
      (tree-copy '(1))
    => '(1))

  (check
      (tree-copy '(1 . 2))
    => '(1 . 2))

  (let ((ell '(1 2 3 4)))
    (check
	(tree-copy ell)
      => ell))

  (let ((ell '(1 2 3 4 . 5)))
    (check
	(tree-copy ell)
      => ell))

  (let ((ell '(1 (2 (3 4)
		    5
		    6)
		 7 8
		 (9 10))))
    (check
	(tree-copy ell)
      => ell))

  (let ((ell '(1 (2 (3 . 4)
		    5
		    6)
		 7 8
		 (9 . 10))))
    (check
	(tree-copy ell)
      => ell))

;;; --------------------------------------------------------------------

  (check
      (iota 5 10)
    => '(10 11 12 13 14))

  (check
      (iota 5)
    => '(0 1 2 3 4))

  (check
      (iota 5 10 5)
    => '(10 15 20 25 30))

  (check
      (guard (exc (else #t))
	(iota -5 10 5))
    => #t)

  #f)


(let ()		;kind-predicates

  (check
      (six.list? '())
    => #t)

  (check
      (six.list? '(1 2 3))
    => #t)

  (check
      (six.list? '(1 2 3 . 4))
    => #f)

  (check
      (six.list? '(1 . 2))
    => #f)

  (check
      (six.list? (circular-list 1 2))
    => #f)

  (check
      (six.list? (circular-list 1 2 3 4 5 6))
    => #f)

  (check
      (six.list? (six.list 1 2 3 4 (circular-list 5 6 7 8)))
    => #t)

  (check
      (six.list? 123)
    => #f)

  (check
      (six.list? #\a)
    => #f)

  (check
      (six.list? 'alpha)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (circular-list? '())
    => #f)

  (check
      (circular-list? '(1 2 3))
    => #f)

  (check
      (circular-list? '(1 . 2))
    => #f)

  (check
      (circular-list? '(1 2 3 . 4))
    => #f)

  (check
      (circular-list? (circular-list 1 2))
    => #t)

  (check
      (circular-list? (circular-list 1 2 3 4 5 6))
    => #t)

  (check
      (circular-list? (six.list 1 2 3 4 (circular-list 5 6 7 8)))
    => #f)

  (check
      (circular-list? (six.cons 1 (six.cons 2 (six.cons 3 (six.cons 4 (circular-list 5 6 7 8))))))
    => #t)

  (check
      (circular-list? 123)
    => #f)

  (check
      (circular-list? #\a)
    => #f)

  (check
      (circular-list? 'alpha)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (dotted-list? '())
    => #f)

  (check
      (dotted-list? '(1 2 3))
    => #f)

  (check
      (dotted-list? '(1 . 2))
    => #t)

  (check
      (dotted-list? '(1 2 3 . 4))
    => #t)

  (check
      (dotted-list? (circular-list 1 2))
    => #f)

  (check
      (dotted-list? (circular-list 1 2 3 4 5 6))
    => #f)

  (check
      (dotted-list? (six.list 1 2 3 4 (circular-list 5 6 7 8)))
    => #f)

  (check
      (dotted-list? (six.cons 1 (six.cons 2 (six.cons 3 (six.cons 4 (circular-list 5 6 7 8))))))
    => #f)

  (check
      (dotted-list? 123)
    => #f)

  (check
      (dotted-list? #\a)
    => #f)

  (check
      (dotted-list? 'alpha)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (six.null? '(1 2))
    => #f)

  (check
      (six.null? '(1 . 2))
    => #f)

  (check
      (six.null? '(1))
    => #f)

  (check
      (six.null? '())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (six.pair? '(1 2))
    => #t)

  (check
      (six.pair? '(1 . 2))
    => #t)

  (check
      (six.pair? '(1))
    => #t)

  (check
      (six.pair? '())
    => #f)

  (check
      (six.pair? 1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (not-pair? '(1 2))
    => #f)

  (check
      (not-pair? '(1 . 2))
    => #f)

  (check
      (not-pair? '(1))
    => #f)

  (check
      (not-pair? '())
    => #t)

  (check
      (not-pair? 1)
    => #t)

  #f)


(let ()		;null-predicates

  (check (and-null?)			=> #t)
  (check (and-null? '())		=> #t)
  (check (and-null? '() '())		=> #t)
  (check (and-null? '() '() '())	=> #t)

  (check (and-null? '(1))		=> #f)
  (check (and-null? '(1) '(1) '(1))	=> #f)
  (check (and-null? '()  '()  '(1))	=> #f)

  (check (or-null?)			=> #f)
  (check (or-null? '())			=> #t)
  (check (or-null? '() '())		=> #t)
  (check (or-null? '() '() '())		=> #t)

  (check (or-null? '(1))		=> #f)
  (check (or-null? '(1) '(1) '(1))	=> #f)
  (check (or-null? '()  '()  '(1))	=> #t)

  (let-syntax ((check-values	(syntax-rules ()
				  ((_ ?expr ?expected)
				   (check (receive (a o) ?expr (six.list a o)) => ?expected)))))
    (check-values (and/or-null?)		 '(#t #f))
    (check-values (and/or-null? '())		 '(#t #t))
    (check-values (and/or-null? '() '())	 '(#t #t))
    (check-values (and/or-null? '() '() '())	 '(#t #t))

    (check-values (and/or-null? '(1))		 '(#f #f))
    (check-values (and/or-null? '(1) '(1) '(1))	 '(#f #f))
    (check-values (and/or-null? '()  '()  '(1))	 '(#f #t)))

  #f)


(let ()		;comparison

  (check
      (list=? =)
    => #t)

  (check
      (list=? = numbers)
    => #t)

  (check
      (list=? = numbers numbers)
    => #t)

  (check
      (list=? = numbers numbers numbers)
    => #t)

  (check
      (list=? = numbers numbers numbers numbers)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (list=? =
	      '(1 2 3 4)
	      '(1 9 3 4))
    => #f)

  (check
      (list=? =
	      '(1 2 3)
	      '(9 2 3))
    => #f)

  (check
      (list=? =
	      '(1 2 3)
	      '(9 2 3)
	      '(9 2 3))
    => #f)
  (check
      (list=? =
	      '(9 2 3)
	      '(1 2 3)
	      '(9 2 3))
    => #f)

  (check
      (list=? =
	      '(9 2 3)
	      '(9 2 3)
	      '(1 2 3))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (list=? = '(1))
    => #t)

  (check
      (list=? = '(1) '(1))
    => #t)

  (check
      (list=? = '(1) '(1) '(1))
    => #t)

  (check
      (list=? = '(1) '(1) '(1) '(1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (list=? = '(1) '(1 2))
    => #f)

  (check
      (list=? = '(1 2) '(1))
    => #f)

  (check
      (list=? = '(1 2) '(1) '(1))
    => #f)

  (check
      (list=? = '(1) '(1 2) '(1))
    => #f)

  (check
      (list=? = '(1) '(1) '(1 2))
    => #f)

  (check
      (list=? = numbers '(0 1 2 3 4))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (list=? = '())
    => #t)

  (check
      (list=? = '() '())
    => #t)

  (check
      (list=? = '() '() '())
    => #t)

  (check
      (list=? = '() '() '() '())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (list=? = '() numbers)
    => #f)

  (check
      (list=? = numbers '())
    => #f)

  (check
      (list=? = numbers '() '())
    => #f)

  (check
      (list=? = '() numbers '())
    => #f)

  (check
      (list=? = '() '() numbers)
    => #f)

  #f)


(let ()		;selectors

  (check (first numbers)	=> 0)
  (check (second numbers)	=> 1)
  (check (third numbers)  => 2)
  (check (fourth numbers)	=> 3)
  (check (fifth numbers)  => 4)
  (check (sixth numbers)	=> 5)
  (check (seventh numbers) => 6)
  (check (eighth numbers)	=> 7)
  (check (ninth numbers)	=> 8)
  (check (tenth numbers)	=> 9)

;;; --------------------------------------------------------------------

  (check
      (six.list-ref numbers 0)
    => 0)

  (check
      (six.list-ref numbers 3)
    => 3)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (car+cdr numbers))
	six.list)
    => (six.list (six.car numbers) (six.cdr numbers)))

;;; --------------------------------------------------------------------

  (check
      (take-left '() 0)
    => '())

  (check
      (take-left numbers 0)
    => '())

  (check
      (take-left numbers 5)
    => '(0 1 2 3 4))

  (check
      (take-left numbers 10)
    => numbers)

  (check
      (six.append (take-left numbers 3)
		  (drop-left numbers 3))
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (drop-right numbers 5)
    => '(0 1 2 3 4))

  (check
      (drop-right numbers 0)
    => numbers)

  (check
      (drop-right '() 0)
    => '())

  (check
      (drop-right numbers 10)
    => '())

  (check
      (six.append (drop-right numbers 3)
		  (take-right numbers 3))
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (take-right numbers 5)
    => '(5 6 7 8 9))

  (check
      (take-right numbers 0)
    => '())

  (check
      (take-right '() 0)
    => '())

  (check
      (take-right numbers 10)
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (drop-right numbers 5)
    => '(0 1 2 3 4))

  (check
      (drop-right numbers 0)
    => numbers)

  (check
      (drop-right '() 0)
    => '())

  (check
      (drop-right numbers 10)
    => '())

;;; --------------------------------------------------------------------

  (check
      (take-left! (six.list 1 3 5) 2)
    => '(1 3))

  (check
      (guard (exc (else #t))
	(take-left! '() 2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (drop-right! (six.list 1 3 5) 1)
    => '(1 3))

  (check
      (guard (exc (else #t))
	(drop-right! '() 1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (split-at numbers 5))
	six.list)
    => '((0 1 2 3 4)
	 (5 6 7 8 9)))

;;; --------------------------------------------------------------------

  (check
      (last numbers)
    => 9)

  (check
      (last '(9))
    => 9)

;;; This raises an error.
  ;;
  ;; (check
  ;;     (last '())
  ;;   => '())

;;; --------------------------------------------------------------------

  (check
      (six.last-pair numbers)
    => '(9))

  (check
      (six.last-pair '(9))
    => '(9))

;;; The empty list is not a pair, so the following raises an error.
  ;;
  ;; (check
  ;;     (last-pair '())
  ;;   => '())

  #f)


(let ()		;miscellaneous

  (check
      (six.length '(1 2 3 4 5 6))
    => 6)

  (check
      (six.length '(1))
    => 1)

  (check
      (six.length '())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (length+ '())
    => 0)

  (check
      (length+ '(1))
    => 1)

  (check
      (length+ '(1 2 3 4 5 6))
    => 6)

  (check
      (length+ (circular-list 1 2 3 4 5 6))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (six.append '(x) '(y))
    => '(x y))

  (check
      (six.append '(a) '(b c d))
    => '(a b c d))

  (check
      (six.append '(a (b)) '((c)))
    => '(a (b) (c)))

  (check
      (six.append '(a b) '(c . d))
    => '(a b c . d))

  (check
      (six.append '() 'a)
    => 'a)

  (check
      (six.append '(a) '())
    => '(a))

  (check
      (six.append '(x y))
    => '(x y))

  (check
      (six.append)
    => '())

;;; --------------------------------------------------------------------

  (check
      (append!)
    => '())

  (check
      (append! '())
    => '())

  (check
      (append! '() '())
    => '())

  (check
      (append! '() '() '())
    => '())

  (check
      (append! (six.list 'y))
    => '(y))

  (check
      (append! (six.list 'x) (six.list 'y))
    => '(x y))

  (check
      (append! (six.list 'x) (six.list 'y) (six.list 'z))
    => '(x y z))

  (check
      (append! (six.list 'a) '(b c d))
    => '(a b c d))

  (check
      (append! (six.list 'a '(b)) '((c)))
    => '(a (b) (c)))

  (check
      (append! (six.list 'a 'b) '(c . d))
    => '(a b c . d))

  (check
      (append! '() 'a)
    => 'a)

  (check
      (append! (six.list 'a) '())
    => '(a))

  (check
      (append! (six.list 'x 'y))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (concatenate '())
    => '())

  (check
      (concatenate '(()))
    => '())

  (check
      (concatenate '(() ()))
    => '())

  (check
      (concatenate '(() () ()))
    => '())

  (check
      (concatenate '((x)))
    => '(x))

  (check
      (concatenate '((x) (y)))
    => '(x y))

  (check
      (concatenate '((x) (y) (z)))
    => '(x y z))

  (check
      (concatenate '((a)
		     (b c d)))
    => '(a b c d))

  (check
      (concatenate '((a b)
		     (c d)))
    => '(a b c d))

  (check
      (concatenate '((a b)
		     (c d)
		     (e f)))
    => '(a b c d e f))

  (check
      (concatenate '((a b c d e f g)
		     (h i)
		     (l m n o)))
    => '(a b c d e f g h i l m n o))

  (check
      (concatenate '((a (b)) ((c))))
    => '(a (b) (c)))

  (check
      (concatenate '((a b) (c . d)))
    => '(a b c . d))

  (check
      (concatenate '(() (a)))
    => '(a))

  (check
      (concatenate '((x y)))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (concatenate! '())
    => '())

  (check
      (concatenate! '(()))
    => '())

  (check
      (concatenate! '(() ()))
    => '())

  (check
      (concatenate! '(() () ()))
    => '())

  (check
      (concatenate! '((x)))
    => '(x))

  (check
      (concatenate! (tree-copy '((x) (y))))
    => '(x y))

  (check
      (concatenate! (tree-copy '((x) (y) (z))))
    => '(x y z))

  (check
      (concatenate! (tree-copy '((a)
				 (b c d))))
    => '(a b c d))

  (check
      (concatenate! (tree-copy '((a b)
				 (c d))))
    => '(a b c d))

  (check
      (concatenate! (tree-copy '((a b)
				 (c d)
				 (e f))))
    => '(a b c d e f))

  (check
      (concatenate! (tree-copy '((a b c d e f g)
				 (h i)
				 (l m n o))))
    => '(a b c d e f g h i l m n o))

  (check
      (concatenate! (tree-copy '((a (b)) ((c)))))
    => '(a (b) (c)))

  (check
      (concatenate! (tree-copy '((a b) (c . d))))
    => '(a b c . d))

  (check
      (concatenate! (tree-copy '(() (a))))
    => '(a))

  (check
      (concatenate! (tree-copy '((x y))))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (six.reverse '())
    => '())

  (check
      (six.reverse '(1))
    => '(1))

  (check
      (six.reverse '(1 2 3))
    => '(3 2 1))

  (check
      (reverse! '())
    => '())

  (check
      (reverse! (tree-copy '(1)))
    => '(1))

  (check
      (reverse! (tree-copy '(1 2 3)))
    => '(3 2 1))

;;; --------------------------------------------------------------------

  (check
      (append-reverse '() '())
    => '())

  (check
      (append-reverse '(x) '(y))
    => '(x y))

  (check
      (append-reverse '(1 2 3) '(4 5 6))
    => '(3 2 1 4 5 6))

  (check
      (append-reverse '(a) '(b c d))
    => '(a b c d))

  (check
      (append-reverse '(a (b)) '((c)))
    => '((b) a (c)))

  (check
      (append-reverse '(a) '())
    => '(a))

;;; --------------------------------------------------------------------

  (check
      (append-reverse! '() '())
    => '())

  (check
      (append-reverse! (tree-copy '(x)) (tree-copy '(y)))
    => '(x y))

  (check
      (append-reverse! (tree-copy '(1 2 3)) (tree-copy '(4 5 6)))
    => '(3 2 1 4 5 6))

  (check
      (append-reverse! (tree-copy '(a)) (tree-copy '(b c d)))
    => '(a b c d))

  (check
      (append-reverse! (tree-copy '(a (b))) (tree-copy '((c))))
    => '((b) a (c)))

  (check
      (append-reverse! (tree-copy '(a)) '())
    => '(a))

;;; --------------------------------------------------------------------

  (check
      (zip '(one two three)
	   '(1 2 3)
	   '(odd even odd))
    => '((one 1 odd) (two 2 even) (three 3 odd)))

  (check	;unequal length
      ;;This should fail in Larceny, too!
      (guard (E (else #t))
	(zip '(one two three)
	     '(1 2 3)
	     '(odd even odd even odd even odd even)))
    => #t)

  (check
      (zip '(1 2 3))
    => '((1) (2) (3)))

  (check	;circular list is rejected as list of unequal length
      (guard (E (else #t))
	(zip '(3 1 4 1)
	     (circular-list #f #t)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (zip* '(one two three)
	    '(1 2 3)
	    '(odd even odd even odd even odd even))
    => '((one 1 odd) (two 2 even) (three 3 odd)))

  (check
      (zip* '(1 2 3))
    => '((1) (2) (3)))

  (check
      (zip* '(3 1 4 1)
	    (circular-list #f #t))
    => '((3 #f)
	 (1 #t)
	 (4 #f)
	 (1 #t)))

;;; --------------------------------------------------------------------

  (check
      (unzip1 '((1)))
    => '(1))

  (check
      (unzip1 '((1)
		(2)))
    => '(1 2))

  (check
      (unzip1 '((1)
		(2)
		(3)))
    => '(1 2 3))

  (check
      (unzip1 '((1 one)
		(2 two)
		(3 three)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (unzip2 '((1 one))))
	six.list)
    => '((1)
	 (one)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip2 '((1 one)
		      (2 two))))
	six.list)
    => '((1 2)
	 (one two)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip2 '((1 one)
		      (2 two)
		      (3 three))))
	six.list)
    => '((1 2 3)
	 (one two three)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (unzip3 '((1 10 100)
		      (2 20 200)
		      (3 30 300))))
	six.list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip4 '((1 10 100 1000)
		      (2 20 200 2000)
		      (3 30 300 3000))))
	six.list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)
	 (1000 2000 3000)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip5 '((1 10 100 1000 10000)
		      (2 20 200 2000 20000)
		      (3 30 300 3000 30000))))
	six.list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)
	 (1000 2000 3000)
	 (10000 20000 30000)))


;;; --------------------------------------------------------------------

  (check
      (count even? numbers)
    => 5)

  (check
      (count even? '(1))
    => 0)

  (check
      (count even? '(2))
    => 1)

  (check
      (count even? '())
    => 0)

  #f)


(let ()		;left-folding

  (check
      (fold + 0 numbers)
    => 45)

  (check
      (fold six.cons '() numbers)
    => '(9 8 7 6 5 4 3 2 1 0))

  (check
      (fold six.cons '(4 5 6) '(3 2 1))
    => '(1 2 3 4 5 6))

  (check
      (fold six.cons '(4 5 6) '())
    => '(4 5 6))

  (check
      (fold six.cons '(4 5 6) '(3))
    => '(3 4 5 6))

  (check
      (fold (lambda (x count)
	      (if (symbol? x)
		  (+ count 1)
		count))
	    0
	    '(a 1 b 2 c 3))
    => 3)

  (check
      (fold (lambda (s len)
	      (max len (string-length s)))
	    0
	    '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold six.cons* '()
	    '(a b c)
	    '(1 2 3 4 5))
    => '(c 3 b 2 a 1))

  (check
      (fold six.cons* '()
	    '(a)
	    '(1))
    => '(a 1))

  (check
      (fold (lambda (a b c knil)
	      (six.cons (six.list a b c)
			knil))
	    '()
	    '(1 2 3)
	    '(10 20 30)
	    '(100 200 300))
    => '((3 30 300)
	 (2 20 200)
	 (1 10 100)))

  (check
      (fold (lambda (a b c knil)
	      (six.cons (six.list a b c)
			knil))
	    '()
	    '(1 2 3)
	    '(10 20)
	    '(100 200 300 400))
    => '((2 20 200)
	 (1 10 100)))

;;; --------------------------------------------------------------------

  (check
      (fold-left* + 0 numbers)
    => 45)

  (check
      (fold-left* xcons '() numbers)
    => '(9 8 7 6 5 4 3 2 1 0))

  (check
      (fold-left* xcons '(4 5 6) '(3 2 1))
    => '(1 2 3 4 5 6))

  (check
      (fold-left* xcons '(4 5 6) '())
    => '(4 5 6))

  (check
      (fold-left* xcons '(4 5 6) '(3))
    => '(3 4 5 6))

  (check
      (fold-left* (lambda (count x)
  		    (if (symbol? x)
  			(+ count 1)
  		      count))
  		  0
  		  '(a 1 b 2 c 3))
    => 3)

  (check
      (fold-left* (lambda (len s)
  		    (max len (string-length s)))
  		  0
  		  '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold-left* (lambda (knil a b c)
		    (six.cons (six.list a b c)
			      knil))
		  '()
		  '(1 2 3)
		  '(10 20)
		  '(100 200 300 400))
    => '((2 20 200)
	 (1 10 100)))

;;; --------------------------------------------------------------------

  (check
      (fold-left/pred < 0 '(1 2 3 4 5 6))
    => 6)

  (check
      (fold-left/pred < 0 '(1 2 3 -4 5 6))
    => #f)

  #f)


(let ()		;right-folding

  (check
      (fold* six.cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (fold* six.cons '() numbers)
    => numbers)

  (check
      (fold* + 0 numbers)
    => 45)

  (check
      (fold* six.cons '(4 5 6) '(1 2 3))
    => '(1 2 3 4 5 6))

  (check
      (fold* (lambda (x count)
	       (if (symbol? x)
		   (+ count 1)
		 count))
	     0
	     '(a 1 b 2 c 3))
    => 3)

  (check
      (fold* (lambda (s len)
	       (max len (string-length s)))
	     0
	     '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold* (lambda (x l)
	       (if (even? x)
		   (six.cons x l)
		 l))
	     '()
	     '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (fold* six.cons* '()
	     '(a b c)
	     '(1 2 3 4 5))
    => '(a 1 b 2 c 3))

  (check
      (fold* six.cons* '()
	     '(a)
	     '(1))
    => '(a 1))

  (check
      (fold* (lambda (a b c knil)
	       (six.cons (six.list a b c)
			 knil))
	     '()
	     '(1 2 3)
	     '(10 20 30)
	     '(100 200 300))
    => '((1 10 100)
	 (2 20 200)
	 (3 30 300)))

  (check
      (fold* (lambda (a b c knil)
	       (six.cons (six.list a b c)
			 knil))
	     '()
	     '(1 2 3)
	     '(10 20)
	     '(100 200 300 400))
    => '((1 10 100)
	 (2 20 200)))

;;; --------------------------------------------------------------------

  (check
      (fold-right* six.cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (fold-right* six.cons '(1 2 3) '())
    => '(1 2 3))

  (check
      (fold-right* six.cons '(1 2 3) '(9))
    => '(9 1 2 3))

  (check
      (fold-right* six.cons '() numbers)
    => numbers)

  (check
      (fold-right* + 0 numbers)
    => 45)

  (check
      (fold-right* six.cons '(4 5 6) '(1 2 3))
    => '(1 2 3 4 5 6))

  (check
      (fold-right* (lambda (x count)
		     (if (symbol? x)
			 (+ count 1)
		       count))
		   0
		   '(a 1 b 2 c 3))
    => 3)

  (check
      (fold-right* (lambda (s len)
		     (max len (string-length s)))
		   0
		   '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold-right* (lambda (x l)
		     (if (even? x)
			 (six.cons x l)
		       l))
		   '()
		   '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (fold-right* six.cons* '()
		   '(a b c)
		   '(1 2 3))
    => '(a 1 b 2 c 3))

  (check
      (fold-right* six.cons* '()
		   '(a)
		   '(1))
    => '(a 1))

  (check
      (fold-right* (lambda (a b c knil)
		     (six.cons (six.list a b c)
			       knil))
		   '()
		   '(1 2 3)
		   '(10 20 30)
		   '(100 200 300))
    => '((1 10 100)
	 (2 20 200)
	 (3 30 300)))

  (check
      (fold-right* (lambda (a b c knil)
		     (six.cons (six.list a b c)
			       knil))
		   '()
		   '(1 2 3)
		   '(10 20)
		   '(100 200 300 400))
    => '((1 10 100)
	 (2 20 200)))

  #f)


(let ()		;pair-folding

  (check
      (pair-fold (lambda (elm knil)
		   (six.cons (six.car elm) knil))
		 '(999)
		 '(1 2 3))
    => '(3 2 1 999))

  (check
      (pair-fold (lambda (pair tail)
		   (six.set-cdr! pair tail)
		   pair)
		 '()
		 (list-copy numbers))
    => (six.reverse numbers))

;;; --------------------------------------------------------------------

  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '(1 2 3)
		 '(10 20 30)
		 '(100 200 300))
    => '((3 30 300)
	 (2 20 200)
	 (1 10 100)
	 999))

  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '(1)
		 '(10)
		 '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '(1)
		 '(10 20 30)
		 '(100 200 300))
    => '((1 10 100)
	 999))

  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '(1 2 3)
		 '(10)
		 '(100 200 300))
    => '((1 10 100)
	 999))
  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '(1 2 3)
		 '(10 20 30)
		 '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '()
		 '(10 20 30)
		 '(100 200 300))
    => '(999))

  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '(1 2 3)
		 '()
		 '(100 200 300))
    => '(999))

  (check
      (pair-fold (lambda (a b c knil)
		   (six.cons (six.list (six.car a)
				       (six.car b)
				       (six.car c))
			     knil))
		 '(999)
		 '(1 2 3)
		 '(10 20 30)
		 '())
    => '(999))

;;; --------------------------------------------------------------------

  (check
      (pair-fold* (lambda (elm knil)
		    (six.cons (six.car elm) knil))
		  '(999)
		  '(1 2 3))
    => '(1 2 3 999))

  (check
      (pair-fold* (lambda (pair tail)
		    (six.set-cdr! pair tail)
		    pair)
		  '()
		  (list-copy numbers))
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '(1 2 3)
		  '(10 20 30)
		  '(100 200 300))
    => '((1 10 100)
	 (2 20 200)
	 (3 30 300)
	 999))

  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '(1)
		  '(10)
		  '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '(1)
		  '(10 20 30)
		  '(100 200 300))
    => '((1 10 100)
	 999))

  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '(1 2 3)
		  '(10)
		  '(100 200 300))
    => '((1 10 100)
	 999))
  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '(1 2 3)
		  '(10 20 30)
		  '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '()
		  '(10 20 30)
		  '(100 200 300))
    => '(999))

  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '(1 2 3)
		  '()
		  '(100 200 300))
    => '(999))

  (check
      (pair-fold* (lambda (a b c knil)
		    (six.cons (six.list (six.car a)
					(six.car b)
					(six.car c))
			      knil))
		  '(999)
		  '(1 2 3)
		  '(10 20 30)
		  '())
    => '(999))

  #f)


(let ()		;reducing

  (check
      (reduce + 0 numbers)
    => 45)

  (check
      (reduce + 0 '())
    => 0)

  (check
      (reduce max 0 '(1 2 3 4 5))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (reduce* + 0 numbers)
    => 45)

  (check
      (reduce* + 0 '())
    => 0)

  (check
      (reduce* max 0 '(1 2 3 4 5))
    => 5)

  (check
      (reduce* six.append
	       '()
	       '((1 2 3)
		 (4 5)
		 (6 7 8 9)
		 (0)))
    => '(1 2 3 4 5 6 7 8 9 0))

  #f)


(let ()		;unfolding

  (check
      (unfold (lambda (x) (< 5 x))
	      (lambda (x) (* x x))
	      (lambda (x) (+ x 1))
	      1)
    => '(1 4 9 16 25))

  (check
      (unfold (lambda (x) (< 5 x))
	      (lambda (x) (* x x))
	      (lambda (x) (+ x 1))
	      1
	      (lambda (x) (- x)))
    => '(1 4 9 16 25 . -6))

  (check
      (unfold (lambda (x) #t)
	      (lambda (x) (* x x))
	      (lambda (x) (+ x 1))
	      1
	      (lambda (x) (- x)))
    => -1)

  (check
      (unfold six.null? six.car six.cdr numbers)
    => numbers)

  (check
      (unfold not-pair? six.car six.cdr '(1 2 3 4 . 5) values)
    => '(1 2 3 4 . 5))

  (check
      (unfold six.null? six.car six.cdr '(1 2 3) (lambda (x) '(4 5 6)))
    => '(1 2 3 4 5 6))

;;; --------------------------------------------------------------------

  (check
      (unfold-right zero?
		    (lambda (x) (* x x))
		    (lambda (x) (- x 1))
		    5)
    => '(1 4 9 16 25))

  (check
      (unfold-right six.null? six.car six.cdr '(1 2 3 4 5))
    => '(5 4 3 2 1))

  (check
      (unfold-right six.null? six.car six.cdr '(3 2 1) '(4 5 6))
    => '(1 2 3 4 5 6))

  #f)


(let ()		;mapping

  (check
      (six.map - '())
    => '())

  (check
      (six.map - '() '())
    => '())

  (check
      (six.map - '() '() '())
    => '())

  (check
      (six.map - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (six.map +
	       '(1 2 3)
	       '(10 20 30))
    => '(11 22 33))

  (check
      (six.map +
	       '(1 2 3)
	       '(10 20 30)
	       '(100 200 300))
    => '(111 222 333))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(six.for-each
	 (lambda (e)
	   (set! r (+ e r)))
	 '())
	r)
    => 0)

  (check
      (let ((r 0))
	(six.for-each
	 (lambda (e1 e2)
	   (set! r (+ e1 e2 r)))
	 '() '())
	r)
    => 0)

  (check
      (let ((r 0))
	(six.for-each
	 (lambda (e1 e2 e3)
	   (set! r (+ e1 e2 e3 r)))
	 '() '() '())
	r)
    => 0)

  (check
      (let ((r '(0 0)))
	(six.for-each
	 (lambda (e1 e2)
	   (set! r (six.list (+ e1 (six.car r))
			     (+ e2 (six.cadr r)))))
	 '(1 10 100)
	 '(2 20 200))
	r)
    => '(111 222))


  (check
      (let ((r '(0 0 0)))
	(six.for-each
	 (lambda (e1 e2 e3)
	   (set! r (six.list (+ e1 (six.car r))
			     (+ e2 (six.cadr r))
			     (+ e3 (six.caddr r)))))
	 '(1 10 100)
	 '(2 20 200)
	 '(3 30 300))
	r)
    => '(111 222 333))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(six.for-each-in-order
	 (lambda (e)
	   (set! r (+ e r)))
	 '())
	r)
    => 0)

  (check
      (let ((r 0))
	(six.for-each-in-order
	 (lambda (e1 e2)
	   (set! r (+ e1 e2 r)))
	 '() '())
	r)
    => 0)

  (check
      (let ((r 0))
	(six.for-each-in-order
	 (lambda (e1 e2 e3)
	   (set! r (+ e1 e2 e3 r)))
	 '() '() '())
	r)
    => 0)

  (check
      (let ((r '(0 0)))
	(six.for-each-in-order
	 (lambda (e1 e2)
	   (set! r (six.list (+ e1 (six.car r))
			     (+ e2 (six.cadr r)))))
	 '(1 10 100)
	 '(2 20 200))
	r)
    => '(111 222))

  (check
      (let ((r '(0 0 0)))
	(six.for-each-in-order
	 (lambda (e1 e2 e3)
	   (set! r (six.list (+ e1 (six.car r))
			     (+ e2 (six.cadr r))
			     (+ e3 (six.caddr r)))))
	 '(1 10 100)
	 '(2 20 200)
	 '(3 30 300))
	r)
    => '(111 222 333))

  (check
      (with-result
       (six.for-each-in-order add-result '(1 2 3 4)))
    => `(4 (1 2 3 4)))

;;; --------------------------------------------------------------------

  (check
      (map* - '())
    => '())

  (check
      (map* - '() '())
    => '())

  (check
      (map* - '() '() '())
    => '())

  (check
      (map* - '() '() '() '())
    => '())

  (check
      (map* - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map* + '(1 2 3))
    => '(1 2 3))

  (check
      (map* +
	    '(1 2 3)
	    '(10 20 30))
    => '(11 22 33))

  (check
      (map* +
	    '(1 2 3)
	    '(10 20 30)
	    '(100 200 300))
    => '(111 222 333))

  (check
      (map* +
	    '(1 2 3)
	    '(10 20)
	    '(100 200 300))
    => '(111 222))

  (check
      (map* +
	    '(1 2)
	    '(10 20 30)
	    '(100 200 300))
    => '(111 222))

  (check
      (map* +
	    '(1 2 3)
	    '(10 20 30)
	    '(100 200))
    => '(111 222))

  (check
      (map* +
	    '()
	    '(10 20 30)
	    '(100 200 300))
    => '())

  (check
      (map* +
	    '(1 2 3)
	    '()
	    '(100 200 300))
    => '())

  (check
      (map* +
	    '(1 2 3)
	    '(10 20 30)
	    '())
    => '())

  (check
      (map* +
	    '(3 1 4 1)
	    (circular-list 1 0))
    => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(for-each*
	 (lambda (e)
	   (set! r (+ e r)))
	 '())
	r)
    => 0)

  (check
      (let ((r 0))
	(for-each*
	 (lambda (e1 e2)
	   (set! r (+ e1 e2 r)))
	 '() '())
	r)
    => 0)

  (check
      (let ((r 0))
	(for-each*
	 (lambda (e1 e2 e3)
	   (set! r (+ e1 e2 e3 r)))
	 '() '() '())
	r)
    => 0)

  (check
      (let ((r '(0 0)))
	(for-each*
	 (lambda (e1 e2)
	   (set! r (six.list (+ e1 (six.car r))
			     (+ e2 (six.cadr r)))))
	 '(1 10 100)
	 '(2 20 200))
	r)
    => '(111 222))


  (check
      (let ((r '(0 0 0)))
	(for-each*
	 (lambda (e1 e2 e3)
	   (set! r (six.list (+ e1 (six.car r))
			     (+ e2 (six.cadr r))
			     (+ e3 (six.caddr r)))))
	 '(1 10 100)
	 '(2 20 200)
	 '(3 30 300))
	r)
    => '(111 222 333))

  (check
      (let ((r '(0 0 0)))
	(for-each*
	 (lambda (e1 e2 e3)
	   (set! r (six.list (+ e1 (six.car r))
			     (+ e2 (six.cadr r))
			     (+ e3 (six.caddr r)))))
	 '(1 10 100)
	 '(2 20 200)
	 (circular-list 3 30 300))
	r)
    => '(111 222 333))

;;; --------------------------------------------------------------------

  (let ()
    (define (f x)
      (six.list x (- x)))

    (check
	(append-map f '())
      => '())

    (check
	(append-map six.list '() '())
      => '())

    (check
	(append-map six.list '() '() '())
      => '())

    (check
	(append-map f '(1))
      => '(1 -1))

    (check
	(append-map six.list '(1) '(2))
      => '(1 2))

    (check
	(append-map six.list '(1) '(2) '(3))
      => '(1 2 3))

    (check
	(append-map f '(1 3 8))
      => '(1 -1 3 -3 8 -8))

    (check
	(append-map six.list
		    '(1 2 3)
		    '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map six.list
		    '(1 2 3)
		    '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map six.list
		    '(1 2 3)
		    '(10 20 30)
		    '(100 200 300))
      => '(1 10 100 2 20 200 3 30 300))

    (check
	(append-map six.list
		    '(1 2)
		    '(10 20 30)
		    '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map six.list
		    '(1 2 3)
		    '(10 20)
		    '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map six.list
		    '(1 2 3)
		    '(10 20 30)
		    '(100 200))
      => '(1 10 100 2 20 200))

;;; --------------------------------------------------------------------

    (check
	(append-map! f '())
      => '())

    (check
	(append-map! six.list '() '())
      => '())

    (check
	(append-map! six.list '() '() '())
      => '())

    (check
	(append-map! f '(1))
      => '(1 -1))

    (check
	(append-map! six.list '(1) '(2))
      => '(1 2))

    (check
	(append-map! six.list '(1) '(2) '(3))
      => '(1 2 3))

    (check
	(append-map! f '(1 3 8))
      => '(1 -1 3 -3 8 -8))

    (check
	(append-map! six.list
		     '(1 2 3)
		     '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map! six.list
		     '(1 2 3)
		     '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map! six.list
		     '(1 2 3)
		     '(10 20 30)
		     '(100 200 300))
      => '(1 10 100 2 20 200 3 30 300))

    (check
	(append-map! six.list
		     '(1 2)
		     '(10 20 30)
		     '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map! six.list
		     '(1 2 3)
		     '(10 20)
		     '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map! six.list
		     '(1 2 3)
		     '(10 20 30)
		     '(100 200))
      => '(1 10 100 2 20 200))

    #f)

;;; --------------------------------------------------------------------

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '(1 2 3))
	r)
    => '((3)
	 (2 3)
	 (1 2 3)))

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y)
	   (set! r (six.cons (six.list x y)
			     r)))
	 '()
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '()
	 '()
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '(1))
	r)
    => '((1)))

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '(1 2))
	r)
    => '((2)
	 (1 2)))

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y)
	   (set! r (six.cons (six.list x y)
			     r)))
	 '(1 2 3)
	 '(10 20 30))
	r)
    => '(((3) (30))
	 ((2 3) (20 30))
	 ((1 2 3) (10 20 30))))

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1 2 3)
	 '(10 20 30)
	 '(100 200 300))
	r)
    => '(((3) (30) (300))
	 ((2 3) (20 30) (200 300))
	 ((1 2 3) (10 20 30) (100 200 300))))

  (check	;lists of different length
      (guard (E (else #t))
	(let ((r '()))
	  (pair-for-each
	   (lambda (x y z)
	     (set! r (six.cons (six.list x y z)
			       r)))
	   '(1 2)
	   '(10 20 30)
	   '(100 200 300))
	  r))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(let ((r '()))
	  (pair-for-each
	   (lambda (x y z)
	     (set! r (six.cons (six.list x y z)
			       r)))
	   '(1 2 3)
	   '(10 20)
	   '(100 200 300))
	  r))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(let ((r '()))
	  (pair-for-each
	   (lambda (x y z)
	     (set! r (six.cons (six.list x y z)
			       r)))
	   '(1 2 3)
	   '(10 20 30)
	   '(100 200))
	  r))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(let ((r '()))
	  (pair-for-each
	   (lambda (x y z)
	     (set! r (six.cons (six.list x y z)
			       r)))
	   '()
	   '(10 20 30)
	   '(100 200 300))
	  r))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(let ((r '()))
	  (pair-for-each
	   (lambda (x y z)
	     (set! r (six.cons (six.list x y z)
			       r)))
	   '(1 2 3)
	   '()
	   '(100 200 300))
	  r))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(let ((r '()))
	  (pair-for-each
	   (lambda (x y z)
	     (set! r (six.cons (six.list x y z)
			       r)))
	   '(1 2 3)
	   '(10 20 30)
	   '())
	  r))
    => #t)

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1)
	 '(10)
	 '(100))
	r)
    => '(((1) (10) (100))))

;;; --------------------------------------------------------------------

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '(1 2 3))
	r)
    => '((3)
	 (2 3)
	 (1 2 3)))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '()
	 '()
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y)
	   (set! r (six.cons (six.list x y)
			     r)))
	 '()
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '(1))
	r)
    => '((1)))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x)
	   (set! r (six.cons x r)))
	 '(1 2))
	r)
    => '((2)
	 (1 2)))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y)
	   (set! r (six.cons (six.list x y)
			     r)))
	 '(1 2 3)
	 '(10 20 30))
	r)
    => '(((3) (30))
	 ((2 3) (20 30))
	 ((1 2 3) (10 20 30))))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1 2 3)
	 '(10 20 30)
	 '(100 200 300))
	r)
    => '(((3) (30) (300))
	 ((2 3) (20 30) (200 300))
	 ((1 2 3) (10 20 30) (100 200 300))))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1 2)
	 '(10 20 30)
	 '(100 200 300))
	r)
    => '(((2) (20 30) (200 300))
	 ((1 2) (10 20 30) (100 200 300))))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1 2 3)
	 '(10 20)
	 '(100 200 300))
	r)
    => '(((2 3) (20) (200 300))
	 ((1 2 3) (10 20) (100 200 300))))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1 2 3)
	 '(10 20 30)
	 '(100 200))
	r)
    => '(((2 3) (20 30) (200))
	 ((1 2 3) (10 20 30) (100 200))))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '()
	 '(10 20 30)
	 '(100 200 300))
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1 2 3)
	 '()
	 '(100 200 300))
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1 2 3)
	 '(10 20 30)
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (six.cons (six.list x y z)
			     r)))
	 '(1)
	 '(10)
	 '(100))
	r)
    => '(((1) (10) (100))))

;;; --------------------------------------------------------------------

  (check
      (map! - '())
    => '())

  (check
      (map! - (list-copy numbers))
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map! + (tree-copy '(1 2 3)))
    => '(1 2 3))

  (check
      (map! - '() '())
    => '())

  (check
      (map! - '() '() '())
    => '())

  (check
      (map! - '() '() '() '())
    => '())

  (check
      (map! +
	    (tree-copy '(1 2 3))
	    (tree-copy '(10 20 30)))
    => '(11 22 33))

  (check
      (map! +
	    (tree-copy '(1 2 3))
	    (tree-copy '(10 20 30))
	    (tree-copy '(100 200 300)))
    => '(111 222 333))

;;; Only the first list argument can be shorter!!!
  (check
      (map! +
	    (tree-copy '(1 2))
	    (tree-copy '(10 20 30))
	    (tree-copy '(100 200 300)))
    => '(111 222))

  (check
      (map! +
	    '()
	    '(10 20 30)
	    '(100 200 300))
    => '())

  (check
      (map! +
	    (tree-copy '(3 1 4 1))
	    (circular-list 1 0))
    => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (map*! - '())
    => '())

  (check
      (map*! - (list-copy numbers))
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map*! + (tree-copy '(1 2 3)))
    => '(1 2 3))

  (check
      (map*! - '() '())
    => '())

  (check
      (map*! - '() '() '())
    => '())

  (check
      (map*! - '() '() '() '())
    => '())

  (check
      (map*! +
	     (tree-copy '(1 2 3))
	     (tree-copy '(10 20 30)))
    => '(11 22 33))

  (check
      (map*! +
	     (tree-copy '(1 2 3))
	     (tree-copy '(10 20 30))
	     (tree-copy '(100 200 300)))
    => '(111 222 333))

;;; Only the first list argument can be shorter!!!
  (check
      (map*! +
	     (tree-copy '(1 2))
	     (tree-copy '(10 20 30))
	     (tree-copy '(100 200 300)))
    => '(111 222))

  (check
      (map*! +
	     '()
	     (tree-copy '(10 20 30))
	     (tree-copy '(100 200 300)))
    => '())

  (check
      (map*! +
	     (tree-copy '(3 1 4 1))
	     (circular-list 1 0))
    => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (filter-map
       (lambda (x)
	 (and (number? x)
	      (* x x)))
       '(a 1 b 3 c 7))
    => '(1 9 49))

  (check
      (filter-map - '())
    => '())

  (check
      (filter-map - '() '())
    => '())

  (check
      (filter-map - '() '() '())
    => '())

  (check
      (filter-map - '() '() '() '())
    => '())

  (check
      (filter-map - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (filter-map + '(1 2 3))
    => '(1 2 3))

  (check
      (filter-map +
		  '(1 2 3)
		  '(10 20 30))
    => '(11 22 33))

  (check
      (filter-map +
		  '(1 2 3)
		  '(10 20 30)
		  '(100 200 300))
    => '(111 222 333))

  (check	;lists of different length
      (guard (E (else #t))
	(filter-map +
		    '(1 2 3)
		    '(10 20)
		    '(100 200 300)))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(filter-map +
		    '(1 2)
		    '(10 20 30)
		    '(100 200 300)))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(filter-map +
		    '(1 2 3)
		    '(10 20 30)
		    '(100 200)))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(filter-map +
		    '()
		    '(10 20 30)
		    '(100 200 300)))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(filter-map +
		    '(1 2 3)
		    '()
		    '(100 200 300)))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(filter-map +
		    '(1 2 3)
		    '(10 20 30)
		    '()))
    => #t)

  (check	;lists of different length
      (guard (E (else #t))
	(filter-map +
		    '(3 1 4 1)
		    (circular-list 1 0)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (filter-map*
       (lambda (x)
	 (and (number? x)
	      (* x x)))
       '(a 1 b 3 c 7))
    => '(1 9 49))

  (check
      (filter-map* - '())
    => '())

  (check
      (filter-map* - '() '())
    => '())

  (check
      (filter-map* - '() '() '())
    => '())

  (check
      (filter-map* - '() '() '() '())
    => '())

  (check
      (filter-map* - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (filter-map* + '(1 2 3))
    => '(1 2 3))

  (check
      (filter-map* +
		   '(1 2 3)
		   '(10 20 30))
    => '(11 22 33))

  (check
      (filter-map* +
		   '(1 2 3)
		   '(10 20 30)
		   '(100 200 300))
    => '(111 222 333))

  (check
      (filter-map* +
		   '(1 2 3)
		   '(10 20)
		   '(100 200 300))
    => '(111 222))

  (check
      (filter-map* +
		   '(1 2)
		   '(10 20 30)
		   '(100 200 300))
    => '(111 222))

  (check
      (filter-map* +
		   '(1 2 3)
		   '(10 20 30)
		   '(100 200))
    => '(111 222))

  (check
      (filter-map* +
		   '()
		   '(10 20 30)
		   '(100 200 300))
    => '())

  (check
      (filter-map* +
		   '(1 2 3)
		   '()
		   '(100 200 300))
    => '())

  (check
      (filter-map* +
		   '(1 2 3)
		   '(10 20 30)
		   '())
    => '())

  (check
      (filter-map* +
		   '(3 1 4 1)
		   (circular-list 1 0))
    => '(4 1 5 1))

  #f)


(let ()		;filtering

  (check
      (six.filter even? '())
    => '())

  (check
      (six.filter even? '(1))
    => '())

  (check
      (six.filter even? '(2))
    => '(2))

  (check
      (six.filter even? numbers)
    => '(0 2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (filter! even? '())
    => '())

  (check
      (filter! even? '(1))
    => '())

  (check
      (filter! even? '(2))
    => '(2))

  (check
      (filter! even? (list-copy numbers))
    => '(0 2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (six.partition even? '()))
	six.list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (six.partition even? '(1)))
	six.list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (six.partition even? '(2)))
	six.list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (six.partition even? '(1 3)))
	six.list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (six.partition even? '(2 4)))
	six.list)
    => '((2 4) ()))

  (check
      (call-with-values
	  (lambda ()
	    (six.partition even? numbers))
	six.list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '()))
	six.list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(1)))
	six.list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(2)))
	six.list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(1 3)))
	six.list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(2 4)))
	six.list)
    => '((2 4) ()))


  (check
      (call-with-values
	  (lambda ()
	    (partition! even? (list-copy numbers)))
	six.list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (six.remove 8 numbers)
    => '(0 1 2 3 4 5 6 7 9))

  (check
      (six.remove 8 '(1 2 3))
    => '(1 2 3))

  (check
      (six.remove 8 '(1))
    => '(1))

  (check
      (six.remove 8 '())
    => '())

;;; --------------------------------------------------------------------

  (check
      (remove* even? '())
    => '())

  (check
      (remove* even? '(1))
    => '(1))

  (check
      (remove* even? '(2))
    => '())

  (check
      (remove* even? numbers)
    => '(1 3 5 7 9))

;;; --------------------------------------------------------------------

  (check
      (remove*! even? '())
    => '())

  (check
      (remove*! even? '(1))
    => '(1))

  (check
      (remove*! even? '(2))
    => '())

  (check
      (remove*! even? (list-copy numbers))
    => '(1 3 5 7 9))

  #f)


(let ()		;finding

  (check
      (six.find even? '())
    => #f)

  (check
      (six.find even? '(1))
    => #f)

  (check
      (six.find even? '(2))
    => 2)

  (check
      (six.find even? '(1 2 3))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (find-tail even? '())
    => #f)

  (check
      (find-tail even? '(1))
    => #f)

  (check
      (find-tail even? '(2))
    => '(2))

  (check
      (find-tail even? '(1 2 3))
    => '(2 3))

;;; --------------------------------------------------------------------

  (check
      (take-while even? '())
    => '())

  (check
      (take-while even? '(1))
    => '())

  (check
      (take-while even? '(2))
    => '(2))

  (check
      (take-while even? '(2 4 6 1 3))
    => '(2 4 6))

;;; --------------------------------------------------------------------

  (check
      (take-while! even? '())
    => '())

  (check
      (take-while! even? (tree-copy '(1)))
    => '())

  (check
      (take-while! even? (tree-copy '(2)))
    => '(2))

  (check
      (take-while! even? (tree-copy '(2 4 6 1 3)))
    => '(2 4 6))

;;; --------------------------------------------------------------------

  (check
      (drop-while even? '())
    => '())

  (check
      (drop-while even? '(1))
    => '(1))

  (check
      (drop-while even? '(2))
    => '())

  (check
      (drop-while even? '(2 4 6 1 3))
    => '(1 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (span even? '()))
	six.list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (span even? '(1)))
	six.list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (span even? '(2)))
	six.list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (span even? '(2 4 6 1 3)))
	six.list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (span! even? '()))
	six.list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (span! even? (tree-copy '(1))))
	six.list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (span! even? (tree-copy '(2))))
	six.list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (span! even? (tree-copy '(2 4 6 1 3))))
	six.list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (break even? '()))
	six.list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (break even? '(1)))
	six.list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (break even? '(2)))
	six.list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (break even? '(1 3 2 4 6)))
	six.list)
    => '((1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (break! even? '()))
	six.list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (break! even? (tree-copy '(1))))
	six.list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (break! even? (tree-copy '(2))))
	six.list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (break! even? (tree-copy '(1 3 2 4 6))))
	six.list)
    => '((1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (any even? '())
    => #f)

  (check
      (any even? '(1))
    => #f)

  (check
      (and (any even? '(2))
	   #t)
    => #t)

  (check
      (and (any even? '(1 2))
	   #t)
    => #t)

  (check
      (and (any even? '(1 3 5 7 2))
	   #t)
    => #t)

  (check
      (any (lambda args
	     (integer? (apply + args)))
	'() '())
    => #f)

  (check
      (any (lambda args
	     (integer? (apply + args)))
	'() '() '())
    => #f)

;;; The following are  false because when a list  is empty the predicate
;;; is not applied at all and the return value is false.
  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '() '() '())
	   #t)
    => #f)
  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '() '() '())
	   #t)
    => #f)
  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '() '() '())
	   #t)
    => #f)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1 2)
	     '(2 2.2)
	     '(1.1 3))
	   #t)
    => #f)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1 2)
	     '(2 2)
	     '(1.1 3))
	   #t)
    => #t)

  (check
      (guard (E (else #t))
	(any (lambda args
	       (integer? (apply + args)))
	  '(1 2)
	  '(2 2)
	  '(1.1 3)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (any* even? '())
    => #f)

  (check
      (any* even? '(1))
    => #f)

  (check
      (and (any* even? '(2))
	   #t)
    => #t)

  (check
      (and (any* even? '(1 2))
	   #t)
    => #t)

  (check
      (and (any* even? '(1 3 5 7 2))
	   #t)
    => #t)

  (check
      (any* (lambda args
	      (integer? (apply + args)))
	    '() '())
    => #f)

  (check
      (any* (lambda args
	      (integer? (apply + args)))
	    '() '() '())
    => #f)

;;; The following are  false because when a list  is empty the predicate
;;; is not applied at all and the return value is false.
  (check
      (and (any* (lambda args
		   (integer? (apply + args)))
		 '(1) '() '())
	   #t)
    => #f)
  (check
      (and (any* (lambda args
		   (integer? (apply + args)))
		 '() '(1) '())
	   #t)
    => #f)
  (check
      (and (any* (lambda args
		   (integer? (apply + args)))
		 '() '() '(1))
	   #t)
    => #f)

  (check
      (and (any* (lambda args
		   (integer? (apply + args)))
		 '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (any* (lambda args
		   (integer? (apply + args)))
		 '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (any* (lambda args
		   (integer? (apply + args)))
		 '(1 2)
		 '(2 2.2)
		 '(1.1 3))
	   #t)
    => #f)

  (check
      (and (any* (lambda args
		   (integer? (apply + args)))
		 '(1 2)
		 '(2 2)
		 '(1.1 3))
	   #t)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (every even? '())
    => #t)

  (check
      (every even? '(1))
    => #f)

  (check
      (and (every even? '(2))
	   #t)
    => #t)

  (check
      (and (every even? '(1 2))
	   #t)
    => #f)

  (check
      (and (every even? '(4 8 10 12))
	   #t)
    => #t)

  (check
      (every (lambda args
	       (integer? (apply + args)))
	'() '())
    => #t)

  (check
      (every (lambda args
	       (integer? (apply + args)))
	'() '() '())
    => #t)

;;; The following are true because when a list is empty the predicate is
;;; not applied at all and the return value is true.
  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '() '() '())
	   #t)
    => #t)
  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '() '() '())
	   #t)
    => #t)
  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '() '() '())
	   #t)
    => #t)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1 2)
	     '(2 2.2)
	     '(1 3))
	   #t)
    => #f)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1 2)
	     '(2 2)
	     '(1 3))
	   #t)
    => #t)

  (check
      (guard (E (else #t))
	(every (lambda args
		 (integer? (apply + args)))
	  '(1 2)
	  '(2 2 2)
	  '(1 3)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (every* even? '())
    => #t)

  (check
      (every* even? '(1))
    => #f)

  (check
      (and (every* even? '(2))
	   #t)
    => #t)

  (check
      (and (every* even? '(1 2))
	   #t)
    => #f)

  (check
      (and (every* even? '(4 8 10 12))
	   #t)
    => #t)

  (check
      (every* (lambda args
		(integer? (apply + args)))
	      '() '())
    => #t)

  (check
      (every* (lambda args
		(integer? (apply + args)))
	      '() '() '())
    => #t)

;;; The following are true because when a list is empty the predicate is
;;; not applied at all and the return value is true.
  (check
      (and (every* (lambda args
		     (integer? (apply + args)))
		   '(1) '() '())
	   #t)
    => #t)
  (check
      (and (every* (lambda args
		     (integer? (apply + args)))
		   '() '(1) '())
	   #t)
    => #t)
  (check
      (and (every* (lambda args
		     (integer? (apply + args)))
		   '() '() '(1))
	   #t)
    => #t)

  (check
      (and (every* (lambda args
		     (integer? (apply + args)))
		   '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (every* (lambda args
		     (integer? (apply + args)))
		   '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (every* (lambda args
		     (integer? (apply + args)))
		   '(1 2)
		   '(2 2.2)
		   '(1 3))
	   #t)
    => #f)

  (check
      (and (every* (lambda args
		     (integer? (apply + args)))
		   '(1 2)
		   '(2 2)
		   '(1 3))
	   #t)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (list-index even? '())
    => #f)

  (check
      (list-index even? '() '())
    => #f)

  (check
      (list-index even? '() '() '())
    => #f)

  (check
      (list-index even? '(1))
    => #f)

  (check
      (list-index even? '(1 3 5))
    => #f)

  (check
      (list-index even? '(2))
    => 0)

  (check
      (list-index even? '(1 2 3 5))
    => 1)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1 2 3))
    => 0)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1.1 2 3))
    => 1)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1 2 3)
	'(1 2 3))
    => 0)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1.1 2 3)
	'(1 2 3))
    => 1)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1 2 3)
	'(1.1 2.1 3))
    => 2)

  (check
      (guard (E (else #t))
	(list-index (lambda args #f)
	  '(1 2 3)
	  '(1 2 3 10)
	  '(1.1 2.1 3)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (list-index* even? '())
    => #f)

  (check
      (list-index* even? '() '())
    => #f)

  (check
      (list-index* even? '() '() '())
    => #f)

  (check
      (list-index* even? '(1))
    => #f)

  (check
      (list-index* even? '(1 3 5))
    => #f)

  (check
      (list-index* even? '(2))
    => 0)

  (check
      (list-index* even? '(1 2 3 5))
    => 1)

  (check
      (list-index* (lambda args
		     (integer? (apply + args)))
		   '(1 2 3)
		   '(1 2 3))
    => 0)

  (check
      (list-index* (lambda args
		     (integer? (apply + args)))
		   '(1 2 3)
		   '(1.1 2 3))
    => 1)

  (check
      (list-index* (lambda args
		     (integer? (apply + args)))
		   '(1 2 3)
		   '(1 2 3)
		   '(1 2 3))
    => 0)

  (check
      (list-index* (lambda args
		     (integer? (apply + args)))
		   '(1 2 3)
		   '(1.1 2 3)
		   '(1 2 3))
    => 1)

  (check
      (list-index* (lambda args
		     (integer? (apply + args)))
		   '(1 2 3)
		   '(1 2 3)
		   '(1.1 2.1 3))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (position 9 '())
    => #f)

  (check
      (position 9 '(1))
    => #f)

  (check
      (position 9 '(1 3 5))
    => #f)

  (check
      (position 2 '(2))
    => 0)

  (check
      (position 2 '(1 2 3 5))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (six.memq 'a '(a b c))
    => '(a b c))

  (check
      (six.memq 'b '(a b c))
    => '(b c))

  (check
      (six.memq 'a '(b c d))
    => #f)

  (check
      (six.memq (six.list 'a) '(b (a) c))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (six.member '(a)
		  '(b (a) c))
    => '((a) c))

  (check
      (member* '(a)
	       '(b (a) c))
    => '((a) c))

  (check
      (member* '(a)
	       '(b a c))
    => #f)

  (check
      (member* '(a)
	       '())
    => #f)

  (check
      (member* 10
	       '(1 2 3 11 4 5)
	       (lambda (a b)
		 (= (+ 1 a) b)))
    => '(11 4 5))

;;; --------------------------------------------------------------------

  (check
      (six.memv 101 '(100 101 102))
    => '(101 102))

  #f)


(let ()		;deletion

  (check
      (delete 8 '())
    => '())

  (check
      (delete 8 '(1))
    => '(1))

  (check
      (delete 8 '(8))
    => '())

  (check
      (delete 8 '(1 2 3))
    => '(1 2 3))

  (check
      (delete 8 '(1 2 8 3 4 5 8 6 7 8))
    => '(1 2 3 4 5 6 7))

  (check
      (delete 8 '() =)
    => '())

  (check
      (delete 8 '(1) =)
    => '(1))

  (check
      (delete 8 '(8) =)
    => '())

  (check
      (delete 8 '(1 2 3) =)
    => '(1 2 3))

  (check
      (delete 8 '(1 2 8 3 4 5 8 6 7 8) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete! 8 '())
    => '())

  (check
      (delete! 8 (tree-copy '(1)))
    => '(1))

  (check
      (delete! 8 (tree-copy '(8)))
    => '())

  (check
      (delete! 8 (tree-copy '(1 2 3)))
    => '(1 2 3))

  (check
      (delete! 8 (tree-copy '(1 2 8 3 4 5 8 6 7 8)))
    => '(1 2 3 4 5 6 7))


  (check
      (delete! 8 '() =)
    => '())

  (check
      (delete! 8 (tree-copy '(1)) =)
    => '(1))

  (check
      (delete! 8 (tree-copy '(8)) =)
    => '())

  (check
      (delete! 8 (tree-copy '(1 2 3)) =)
    => '(1 2 3))

  (check
      (delete! 8 (tree-copy '(1 2 8 3 4 5 8 6 7 8)) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates '())
    => '())

  (check
      (delete-duplicates '(1))
    => '(1))

  (check
      (delete-duplicates '(1 2))
    => '(1 2))

  (check
      (delete-duplicates '(1 1))
    => '(1))

  (check
      (delete-duplicates '(1 1 1))
    => '(1))

  (check
      (delete-duplicates '(1 2 3 2 4 5 4 6 1 7))
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates! '())
    => '())

  (check
      (delete-duplicates! (tree-copy '(1)))
    => '(1))

  (check
      (delete-duplicates! (tree-copy '(1 2)))
    => '(1 2))

  (check
      (delete-duplicates! (tree-copy '(1 1)))
    => '(1))

  (check
      (delete-duplicates! (tree-copy '(1 1 1)))
    => '(1))

  (check
      (delete-duplicates! (tree-copy '(1 2 3 2 4 5 4 6 1 7)))
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates '() =)
    => '())

  (check
      (delete-duplicates '(1) =)
    => '(1))

  (check
      (delete-duplicates '(1 2) =)
    => '(1 2))

  (check
      (delete-duplicates '(1 1) =)
    => '(1))

  (check
      (delete-duplicates '(1 1 1) =)
    => '(1))

  (check
      (delete-duplicates '(1 2 3 2 4 5 4 6 1 7) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates! '() =)
    => '())

  (check
      (delete-duplicates! (tree-copy '(1)) =)
    => '(1))

  (check
      (delete-duplicates! (tree-copy '(1 2)) =)
    => '(1 2))

  (check
      (delete-duplicates! (tree-copy '(1 1)) =)
    => '(1))

  (check
      (delete-duplicates! (tree-copy '(1 1 1)) =)
    => '(1))

  (check
      (delete-duplicates! (tree-copy '(1 2 3 2 4 5 4 6 1 7)) =)
    => '(1 2 3 4 5 6 7))

  #f)


(let ()		;sorted-lists

  (check
      (sorted-list-insert 5 '() >)
    => '(5))

  (check
      (sorted-list-insert 5 '(0 1 2 3 4 6 7) >)
    => '(0 1 2 3 4 5 6 7))

  (check
      (sorted-list-insert 0 '(1 2 3 4 5 6 7) >)
    => '(0 1 2 3 4 5 6 7))

  (check
      (sorted-list-insert 7 '(0 1 2 3 4 5 6) >)
    => '(0 1 2 3 4 5 6 7))

  (check
      (sorted-list-insert 5 '(0 1 2 3 4 5 6 7) >)
    => '(0 1 2 3 4 5 5 6 7))

  (check
      (sorted-list-insert 5 '(0 1 2 3 4 5 5 5 5 5 6 7) >)
    => '(0 1 2 3 4 5 5 5 5 5 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (sorted-list-insert/uniq 5 '() < >)
    => '(5))

  (check
      (sorted-list-insert/uniq 5 '(0 1 2 3 4 6 7) < >)
    => '(0 1 2 3 4 5 6 7))

  (check
      (sorted-list-insert/uniq 0 '(1 2 3 4 5 6 7) < >)
    => '(0 1 2 3 4 5 6 7))

  (check
      (sorted-list-insert/uniq 7 '(0 1 2 3 4 5 6) < >)
    => '(0 1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (union-of-sorted-lists '() '() < >)
    => '())

  (check
      (union-of-sorted-lists '(10) '() < >)
    => '(10))

  (check
      (union-of-sorted-lists '() '(10) < >)
    => '(10))

  (check
      (union-of-sorted-lists '(0 1 3 8 9 10)
			     '(1 2 4 5 11 23)
			     < >)
    => '(0 1 1 2 3 4 5 8 9 10 11 23))

  (check
      (union-of-sorted-lists '(0 1 3 8 9 10)
			     '(0 1 3 8 9 10)
			     < >)
    => '(0 0 1 1 3 3 8 8 9 9 10 10))

;;; --------------------------------------------------------------------

  (check
      (union-of-sorted-lists/uniq '() '() < >)
    => '())

  (check
      (union-of-sorted-lists/uniq '(10) '() < >)
    => '(10))

  (check
      (union-of-sorted-lists/uniq '() '(10) < >)
    => '(10))

  (check
      (union-of-sorted-lists/uniq '(0 1 3 8 9 10)
				  '(1 2 4 5 11 23)
				  < >)
    => '(0 1 2 3 4 5 8 9 10 11 23))

  (check
      (union-of-sorted-lists/uniq '(0 1 3 8 9 10)
				  '(0 1 3 8 9 10)
				  < >)
    => '(0 1 3 8 9 10))

  #f)


(let ()		;alists

  (check
      (six.assoc 'a
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(a . 1))

  (check
      (six.assoc 'b
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(b . 2))

  (check
      (six.assoc 'c
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (assoc* 'c
	      '())
    => #f)

  (check
      (assoc* 'd
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => #f)

  (check
      (assoc* 'a
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => '(a . 1))

  (check
      (assoc* 'b
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => '(b . 2))

  (check
      (assoc* 'c
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => '(c . 3))

  (check
      (assoc* 'a
	      '((a . 1)
		(b . 2)
		(c . 3))
	      eq?)
    => '(a . 1))

  (check
      (assoc* 'b
	      '((a . 1)
		(b . 2)
		(c . 3))
	      eq?)
    => '(b . 2))

  (check
      (assoc* 'c
	      '((a . 1)
		(b . 2)
		(c . 3))
	      eq?)
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (six.assq 'c
		'())
    => #f)

  (check
      (six.assq 'd
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => #f)

  (check
      (six.assq 'a
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => '(a . 1))

  (check
      (six.assq 'b
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => '(b . 2))

  (check
      (six.assq 'c
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (six.assv 'c
		'())
    => #f)

  (check
      (six.assv 'd
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => #f)

  (check
      (six.assv 'a
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => '(a . 1))

  (check
      (six.assv 'b
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => '(b . 2))

  (check
      (six.assv 'c
		'((a . 1)
		  (b . 2)
		  (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (alist-cons 'a 1
		  '((b . 2)
		    (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-cons 'a 1
		  '())
    => '((a . 1)))

  (check
      (alist-cons 'b 2
		  '((b . 2)
		    (c . 3)))
    => '((b . 2)
	 (b . 2)
	 (c . 3)))

;;; --------------------------------------------------------------------

  (check
      (alist-copy '((a . 1)
		    (b . 2)
		    (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-copy '((a . 1)))
    => '((a . 1)))

  (check
      (alist-copy '())
    => '())

;;; --------------------------------------------------------------------

  (check
      (alist-delete 'a
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((b . 2)
	 (c . 3)))

  (check
      (alist-delete 'b
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((a . 1)
	 (c . 3)))

  (check
      (alist-delete 'c
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((a . 1)
	 (b . 2)))

  (check
      (alist-delete 'd
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-delete 'a
		    '((a . 1)
		      (a . 2)
		      (c . 3)))
    => '((c . 3)))

  (check
      (alist-delete 'a
		    '())
    => '())

  (check
      (alist-delete 'a
		    '((a . 1)))
    => '())

  (check
      (alist-delete 'a
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((b . 2)
	 (c . 3)))

  (check
      (alist-delete 'b
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((a . 1)
	 (c . 3)))

  (check
      (alist-delete 'c
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((a . 1)
	 (b . 2)))

  (check
      (alist-delete 'd
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-delete 'a
		    '((a . 1)
		      (a . 2)
		      (c . 3))
		    eq?)
    => '((c . 3)))

  (check
      (alist-delete 'a
		    '()
		    eq?)
    => '())

  (check
      (alist-delete 'a
		    '((a . 1))
		    eq?)
    => '())

  #f)


(let ()		;circular-lists

  (check
      (let ((ell (circular-list 1 2)))
	(six.list (six.car ell)
		  (six.cadr ell)
		  (six.caddr ell)
		  (six.cadddr ell)))
    => '(1 2 1 2))

;;; --------------------------------------------------------------------

  (check
      (let ((ell (list->circular-list! (tree-copy '(1 2 3)))))
	(six.list (six.car ell)
		  (six.cadr ell)
		  (six.caddr ell)
		  (six.cadddr ell)
		  (six.cadddr (six.cdr ell))))
    => '(1 2 3 1 2))

;;; --------------------------------------------------------------------

  (check
      (circular-list->list! (circular-list 1 2 3 4 5 6 7))
    => '(1 2 3 4 5 6 7))

  (check
      (circular-list->list! (circular-list 1))
    => '(1))

;;; --------------------------------------------------------------------

  (check
      (circular-list->list! (circular-list-copy '()))
    => '())

  (check
      (circular-list->list! (circular-list-copy (circular-list 1 2 3 4 5)))
    => '(1 2 3 4 5))

;;; --------------------------------------------------------------------

  (check
      (circular-list-length '())
    => 0)

  (check
      (circular-list-length (circular-list 1))
    => 1)

  (check
      (circular-list-length (circular-list 1 2 3 4 5))
    => 5)

  (check
      (circular-list-length (list->circular-list! (list-copy '(1 2 3 4 5))))
    => 5)

;;; --------------------------------------------------------------------

  #|  The following  are commented  out because  = from  (rnrs)  must be
called with at least two arguments.

  (check
      (circular-list= =)
    => #t)

  (check
      (circular-list= = '())
    => #t)

  (check
      (circular-list= = '() '() '())
    => #t)

  (check
      (circular-list= = '() '(1) '())
    => #f)

  (check
      (circular-list= = '() (circular-list 1) '())
    => #f)

  (check
      (circular-list= = (circular-list 1))
    => #t)

  |#

  (check
      (circular-list= = (circular-list 1) (circular-list 1) (circular-list 1))
    => #t)

  (check
      (circular-list= = (circular-list 1) (circular-list 2) (circular-list 1))
    => #f)

  (check
      (circular-list= =
  		      (circular-list 1 2 3 4 5)
  		      (circular-list 1 2 3 4 5)
  		      (circular-list 1 2 3 4 5))
    => #t)

  (check
      (circular-list= =
  		      (circular-list 1 2 3 4 5)
  		      (circular-list 1 2 3 99 5)
  		      (circular-list 1 2 3 4 5))
    => #f)

  (check
      (circular-list= =
  		      (circular-list 1 2 3 4 5)
  		      (circular-list 1 2 3)
  		      (circular-list 1 2 3 4 5))
    => #f)

  #f)


(let ()		;sets

  (check
      (lset<=? =)
    => #t)

  (check
      (lset<=? = '())
    => #t)

  (check
      (lset<=? = '() '())
    => #t)

  (check
      (lset<=? = '() '() '())
    => #t)

  (check
      (lset<=? =
	       '(1)
	       '(1))
    => #t)

  (check
      (lset<=? =
	       '(1)
	       '(1)
	       '(1))
    => #t)

  (check
      (lset<=? =
	       '(1)
	       '(1 2)
	       '(1 2 3))
    => #t)

  (check
      (lset<=? =
	       '(1)
	       '(1 2)
	       '(1 2))
    => #t)

  (check
      (lset<=? =
	       '(1)
	       '(1 2)
	       '(1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (lset=? =)
    => #t)

  (check
      (lset=? = '())
    => #t)

  (check
      (lset=? = '() '())
    => #t)

  (check
      (lset=? = '() '() '())
    => #t)

  (check
      (lset=? =
	      '(1)
	      '(1))
    => #t)

  (check
      (lset=? =
	      '(1)
	      '(1)
	      '(1))
    => #t)

  (check
      (lset=? =
	      '(1)
	      '(1 2)
	      '(1 2 3))
    => #f)

  (check
      (lset=? =
	      '(1)
	      '(1 2)
	      '(1 2))
    => #f)

  (check
      (lset=? =
	      '(1)
	      '(1 2)
	      '(1))
    => #f)

  #f)


(parametrise ((check-test-name	'queues))

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs)))
	(empty?))
    => #t)

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs)))
	(enqueue! 1)
	(empty?))
    => #f)

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs)))
	(enqueue! 1)
	(let ((rv0 (empty?))
	      (rv1 (dequeue!))
	      (rv2 (empty?)))
	  (six.list rv0 rv1 rv2)))
    => '(#f 1 #t))

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs)))
	(enqueue! 1)
	(enqueue! 2)
	(enqueue! 3)
	(let ((rv0 (dequeue!))
	      (rv1 (dequeue!))
	      (rv2 (dequeue!))
	      (rv3 (empty?)))
	  (six.list rv0 rv1 rv2 rv3)))
    => '(1 2 3 #t))

;;; --------------------------------------------------------------------
;;; with init values

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs '())))
	(empty?))
    => #t)

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs '(1))))
	(empty?))
    => #f)

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs '(1 2 3))))
	(let ((rv0 (dequeue!))
	      (rv1 (dequeue!))
	      (rv2 (dequeue!))
	      (rv3 (empty?)))
	  (six.list rv0 rv1 rv2 rv3)))
    => '(1 2 3 #t))

  (check
      (let-values (((empty? enqueue! dequeue!)
		    (make-queue-procs '(1 2))))
	(enqueue! 3)
	(enqueue! 4)
	(let ((rv0 (dequeue!))
	      (rv1 (dequeue!))
	      (rv2 (dequeue!))
	      (rv3 (dequeue!))
	      (rv4 (empty?)))
	  (six.list rv0 rv1 rv2 rv3 rv4)))
    => '(1 2 3 4 #t))

  #t)


;;;; done

(check-report)

;;; end of file
