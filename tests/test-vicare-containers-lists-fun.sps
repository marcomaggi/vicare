;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare containers lists)
;;;Date: Mon Dec 29, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
		break)
  (vicare checks)
  (vicare containers lists))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers libraries: lists functions\n")


;;;; helpers

(define numbers '(0 1 2 3 4 5 6 7 8 9))


(parameterise ((check-test-name 'circular))

;;;Miscellaneous tests on circular lists.

  (let ()

    (define end  (cons  1 '()))
    (define tail (cons* 3 2 end))
    (define ell  (cons* 5 4 tail))
    (set-cdr! end tail)

    (check (car ell)		=> 5)
    (check (cadr ell)		=> 4)
    (check (caddr ell)		=> 3)
    (check (cadddr ell)		=> 2)
    (check (cadddr (cdr ell))	=> 1)
    (check (cdddr (cddr ell))	=> tail)
    (check (cadddr (cddr ell))	=> 3)
    (check (cadddr (cdddr ell))	=> 2)

    (check (circular-list? ell)	=> #t)

    #f)

  #t)


(parameterise ((check-test-name 'constructors))

  (check
      (cons* 1 2 3 4 '(5 6 7 8))
    => '(1 2 3 4 5 6 7 8))

;;; --------------------------------------------------------------------

  (check
      (xcons 1 2)
    => '(2 . 1))

;;; --------------------------------------------------------------------

  (check
      (make-list 4 'c)
    => '(c c c c))

  (check
      (make-list 0)
    => '())

  (check
      (make-list 0 #f)
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-tabulate 4 (lambda (i)
			 (cons i 'a)))
    => '((0 . a)
	 (1 . a)
	 (2 . a)
	 (3 . a)))

  (check
      (list-tabulate 1 (lambda (i)
			 (cons i 'a)))
    => '((0 . a)))

  (check
      (list-tabulate 0 (lambda (i)
			 (cons i 'a)))
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-tabulate/reverse 4 (lambda (i)
				 (cons i 'a)))
    => '((0 . a)
	 (1 . a)
	 (2 . a)
	 (3 . a)))

  (check
      (list-tabulate/reverse 1 (lambda (i)
				 (cons i 'a)))
    => '((0 . a)))

  (check
      (list-tabulate/reverse 0 (lambda (i)
				 (cons i 'a)))
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

  )


(parameterise ((check-test-name 'kind-predicates))

  (check
      (list? '())
    => #t)

  (check
      (list? '(1 2 3))
    => #t)

  (check
      (list? '(1 2 3 . 4))
    => #f)

  (check
      (list? '(1 . 2))
    => #f)

  (check
      (list? (circular-list 1 2))
    => #f)

  (check
      (list? (circular-list 1 2 3 4 5 6))
    => #f)

  (check
      (list? (list 1 2 3 4 (circular-list 5 6 7 8)))
    => #t)

  (check
      (list? 123)
    => #f)

  (check
      (list? #\a)
    => #f)

  (check
      (list? 'alpha)
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
      (circular-list? (list 1 2 3 4 (circular-list 5 6 7 8)))
    => #f)

  (check
      (circular-list? (cons 1 (cons 2 (cons 3 (cons 4 (circular-list 5 6 7 8))))))
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
      (dotted-list? (list 1 2 3 4 (circular-list 5 6 7 8)))
    => #f)

  (check
      (dotted-list? (cons 1 (cons 2 (cons 3 (cons 4 (circular-list 5 6 7 8))))))
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
      (null? '(1 2))
    => #f)

  (check
      (null? '(1 . 2))
    => #f)

  (check
      (null? '(1))
    => #f)

  (check
      (null? '())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (pair? '(1 2))
    => #t)

  (check
      (pair? '(1 . 2))
    => #t)

  (check
      (pair? '(1))
    => #t)

  (check
      (pair? '())
    => #f)

  (check
      (pair? 1)
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

  )


(parameterise ((check-test-name 'null-predicates))

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
				   (check (receive (a o) ?expr (list a o)) => ?expected)))))
    (check-values (and/or-null?)		 '(#t #f))
    (check-values (and/or-null? '())		 '(#t #t))
    (check-values (and/or-null? '() '())	 '(#t #t))
    (check-values (and/or-null? '() '() '())	 '(#t #t))

    (check-values (and/or-null? '(1))		 '(#f #f))
    (check-values (and/or-null? '(1) '(1) '(1))	 '(#f #f))
    (check-values (and/or-null? '()  '()  '(1))	 '(#f #t)))

  )


(parameterise ((check-test-name 'comparison))

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

  )


(parameterise ((check-test-name 'selectors))

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
      (list-ref numbers 0)
    => 0)

  (check
      (list-ref numbers 3)
    => 3)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (car+cdr numbers))
	list)
    => (list (car numbers) (cdr numbers)))

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
      (append (take-left numbers 3)
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
      (append (drop-right numbers 3)
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
      (take-left! '(1 3 5) 2)
    => '(1 3))

  (check
      (guard (exc (else #t))
	(take-left! '() 2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (drop-right! '(1 3 5) 1)
    => '(1 3))

  (check
      (guard (exc (else #t))
	(drop-right! '() 1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (split-at numbers 5))
	list)
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
      (last-pair numbers)
    => '(9))

  (check
      (last-pair '(9))
    => '(9))

;;; The empty list is not a pair, so the following raises an error.
  ;;
  ;; (check
  ;;     (last-pair '())
  ;;   => '())

  )


(parameterise ((check-test-name 'miscellaneous))

  (check
      (length '(1 2 3 4 5 6))
    => 6)

  (check
      (length '(1))
    => 1)

  (check
      (length '())
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
      (append '(x) '(y))
    => '(x y))

  (check
      (append '(a) '(b c d))
    => '(a b c d))

  (check
      (append '(a (b)) '((c)))
    => '(a (b) (c)))

  (check
      (append '(a b) '(c . d))
    => '(a b c . d))

  (check
      (append '() 'a)
    => 'a)

  (check
      (append '(a) '())
    => '(a))

  (check
      (append '(x y))
    => '(x y))

  (check
      (append)
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
      (append! '(y))
    => '(y))

  (check
      (append! '(x) '(y))
    => '(x y))

  (check
      (append! '(x) '(y) '(z))
    => '(x y z))

  (check
      (append! '(a) '(b c d))
    => '(a b c d))

  (check
      (append! '(a (b)) '((c)))
    => '(a (b) (c)))

  (check
      (append! '(a b) '(c . d))
    => '(a b c . d))

  (check
      (append! '() 'a)
    => 'a)

  (check
      (append! '(a) '())
    => '(a))

  (check
      (append! '(x y))
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
      (concatenate! '((x) (y)))
    => '(x y))

  (check
      (concatenate! '((x) (y) (z)))
    => '(x y z))

  (check
      (concatenate! '((a)
		      (b c d)))
    => '(a b c d))

  (check
      (concatenate! '((a b)
		      (c d)))
    => '(a b c d))

  (check
      (concatenate! '((a b)
		      (c d)
		      (e f)))
    => '(a b c d e f))

  (check
      (concatenate! '((a b c d e f g)
		      (h i)
		      (l m n o)))
    => '(a b c d e f g h i l m n o))

  (check
      (concatenate! '((a (b)) ((c))))
    => '(a (b) (c)))

  (check
      (concatenate! '((a b) (c . d)))
    => '(a b c . d))

  (check
      (concatenate! '(() (a)))
    => '(a))

  (check
      (concatenate! '((x y)))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (reverse '())
    => '())

  (check
      (reverse '(1))
    => '(1))

  (check
      (reverse '(1 2 3))
    => '(3 2 1))

  (check
      (reverse! '())
    => '())

  (check
      (reverse! '(1))
    => '(1))

  (check
      (reverse! '(1 2 3))
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
      (append-reverse! '(x) '(y))
    => '(x y))

  (check
      (append-reverse! '(1 2 3) '(4 5 6))
    => '(3 2 1 4 5 6))

  (check
      (append-reverse! '(a) '(b c d))
    => '(a b c d))

  (check
      (append-reverse! '(a (b)) '((c)))
    => '((b) a (c)))

  (check
      (append-reverse! '(a) '())
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
	list)
    => '((1)
	 (one)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip2 '((1 one)
		      (2 two))))
	list)
    => '((1 2)
	 (one two)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip2 '((1 one)
		      (2 two)
		      (3 three))))
	list)
    => '((1 2 3)
	 (one two three)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (unzip3 '((1 10 100)
		      (2 20 200)
		      (3 30 300))))
	list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip4 '((1 10 100 1000)
		      (2 20 200 2000)
		      (3 30 300 3000))))
	list)
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
	list)
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

  )


(parameterise ((check-test-name 'left-folding))

  (check
      (fold + 0 numbers)
    => 45)

  (check
      (fold cons '() numbers)
    => '(9 8 7 6 5 4 3 2 1 0))

  (check
      (fold cons '(4 5 6) '(3 2 1))
    => '(1 2 3 4 5 6))

  (check
      (fold cons '(4 5 6) '())
    => '(4 5 6))

  (check
      (fold cons '(4 5 6) '(3))
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
      (fold cons* '()
	    '(a b c)
	    '(1 2 3 4 5))
    => '(c 3 b 2 a 1))

  (check
      (fold cons* '()
	    '(a)
	    '(1))
    => '(a 1))

  (check
      (fold (lambda (a b c knil)
	      (cons (list a b c)
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
	      (cons (list a b c)
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
		    (cons (list a b c)
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

  )


(parameterise ((check-test-name 'right-folding))

  (check
      (fold* cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (fold* cons '() numbers)
    => numbers)

  (check
      (fold* + 0 numbers)
    => 45)

  (check
      (fold* cons '(4 5 6) '(1 2 3))
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
		    (cons x l)
		  l))
	      '()
	      '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (fold* cons* '()
	      '(a b c)
	      '(1 2 3 4 5))
    => '(a 1 b 2 c 3))

  (check
      (fold* cons* '()
	      '(a)
	      '(1))
    => '(a 1))

  (check
      (fold* (lambda (a b c knil)
	       (cons (list a b c)
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
	       (cons (list a b c)
		     knil))
	     '()
	     '(1 2 3)
	     '(10 20)
	     '(100 200 300 400))
    => '((1 10 100)
	 (2 20 200)))

;;; --------------------------------------------------------------------

  (check
      (fold-right* cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (fold-right* cons '(1 2 3) '())
    => '(1 2 3))

  (check
      (fold-right* cons '(1 2 3) '(9))
    => '(9 1 2 3))

  (check
      (fold-right* cons '() numbers)
    => numbers)

  (check
      (fold-right* + 0 numbers)
    => 45)

  (check
      (fold-right* cons '(4 5 6) '(1 2 3))
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
			 (cons x l)
		       l))
		   '()
		   '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (fold-right* cons* '()
		   '(a b c)
		   '(1 2 3))
    => '(a 1 b 2 c 3))

  (check
      (fold-right* cons* '()
		   '(a)
		   '(1))
    => '(a 1))

  (check
      (fold-right* (lambda (a b c knil)
		     (cons (list a b c)
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
		     (cons (list a b c)
			   knil))
		   '()
		   '(1 2 3)
		   '(10 20)
		   '(100 200 300 400))
    => '((1 10 100)
	 (2 20 200)))

  )


(parameterise ((check-test-name 'pair-folding))

  (check
      (pair-fold (lambda (elm knil)
		   (cons (car elm) knil))
		 '(999)
		 '(1 2 3))
    => '(3 2 1 999))

  (check
      (pair-fold (lambda (pair tail)
		   (set-cdr! pair tail)
		   pair)
		 '()
		 (list-copy numbers))
    => (reverse numbers))

;;; --------------------------------------------------------------------

  (check
      (pair-fold (lambda (a b c knil)
		   (cons (list (car a)
			       (car b)
			       (car c))
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
		   (cons (list (car a)
			       (car b)
			       (car c))
			 knil))
		 '(999)
		 '(1)
		 '(10)
		 '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold (lambda (a b c knil)
		   (cons (list (car a)
			       (car b)
			       (car c))
			 knil))
		 '(999)
		 '(1)
		 '(10 20 30)
		 '(100 200 300))
    => '((1 10 100)
	 999))

  (check
      (pair-fold (lambda (a b c knil)
		   (cons (list (car a)
			       (car b)
			       (car c))
			 knil))
		 '(999)
		 '(1 2 3)
		 '(10)
		 '(100 200 300))
    => '((1 10 100)
	 999))
  (check
      (pair-fold (lambda (a b c knil)
		   (cons (list (car a)
			       (car b)
			       (car c))
			 knil))
		 '(999)
		 '(1 2 3)
		 '(10 20 30)
		 '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold (lambda (a b c knil)
		   (cons (list (car a)
			       (car b)
			       (car c))
			 knil))
		 '(999)
		 '()
		 '(10 20 30)
		 '(100 200 300))
    => '(999))

  (check
      (pair-fold (lambda (a b c knil)
		   (cons (list (car a)
			       (car b)
			       (car c))
			 knil))
		 '(999)
		 '(1 2 3)
		 '()
		 '(100 200 300))
    => '(999))

  (check
      (pair-fold (lambda (a b c knil)
		   (cons (list (car a)
			       (car b)
			       (car c))
			 knil))
		 '(999)
		 '(1 2 3)
		 '(10 20 30)
		 '())
    => '(999))

;;; --------------------------------------------------------------------

  (check
      (pair-fold* (lambda (elm knil)
			 (cons (car elm) knil))
		       '(999)
		       '(1 2 3))
    => '(1 2 3 999))

  (check
      (pair-fold* (lambda (pair tail)
			 (set-cdr! pair tail)
			 pair)
		       '()
		       (list-copy numbers))
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (pair-fold* (lambda (a b c knil)
			 (cons (list (car a)
				     (car b)
				     (car c))
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
			 (cons (list (car a)
				     (car b)
				     (car c))
			       knil))
		       '(999)
		       '(1)
		       '(10)
		       '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold* (lambda (a b c knil)
			 (cons (list (car a)
				     (car b)
				     (car c))
			       knil))
		       '(999)
		       '(1)
		       '(10 20 30)
		       '(100 200 300))
    => '((1 10 100)
	 999))

  (check
      (pair-fold* (lambda (a b c knil)
			 (cons (list (car a)
				     (car b)
				     (car c))
			       knil))
		       '(999)
		       '(1 2 3)
		       '(10)
		       '(100 200 300))
    => '((1 10 100)
	 999))
  (check
      (pair-fold* (lambda (a b c knil)
			 (cons (list (car a)
				     (car b)
				     (car c))
			       knil))
		       '(999)
		       '(1 2 3)
		       '(10 20 30)
		       '(100))
    => '((1 10 100)
	 999))

  (check
      (pair-fold* (lambda (a b c knil)
			 (cons (list (car a)
				     (car b)
				     (car c))
			       knil))
		       '(999)
		       '()
		       '(10 20 30)
		       '(100 200 300))
    => '(999))

  (check
      (pair-fold* (lambda (a b c knil)
			 (cons (list (car a)
				     (car b)
				     (car c))
			       knil))
		       '(999)
		       '(1 2 3)
		       '()
		       '(100 200 300))
    => '(999))

  (check
      (pair-fold* (lambda (a b c knil)
			 (cons (list (car a)
				     (car b)
				     (car c))
			       knil))
		       '(999)
		       '(1 2 3)
		       '(10 20 30)
		       '())
    => '(999))

  )


(parameterise ((check-test-name 'reducing))

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
      (reduce* append
	       '()
	       '((1 2 3)
		 (4 5)
		 (6 7 8 9)
		 (0)))
    => '(1 2 3 4 5 6 7 8 9 0))

  )


(parameterise ((check-test-name 'unfolding))

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
      (unfold null? car cdr numbers)
    => numbers)

  (check
      (unfold not-pair? car cdr '(1 2 3 4 . 5) values)
    => '(1 2 3 4 . 5))

  (check
      (unfold null? car cdr '(1 2 3) (lambda (x) '(4 5 6)))
    => '(1 2 3 4 5 6))

;;; --------------------------------------------------------------------

  (check
      (unfold-right zero?
		    (lambda (x) (* x x))
		    (lambda (x) (- x 1))
		    5)
    => '(1 4 9 16 25))

  (check
      (unfold-right null? car cdr '(1 2 3 4 5))
    => '(5 4 3 2 1))

  (check
      (unfold-right null? car cdr '(3 2 1) '(4 5 6))
    => '(1 2 3 4 5 6))

  )


(parameterise ((check-test-name 'mapping))

  (check
      (map - '())
    => '())

  (check
      (map - '() '())
    => '())

  (check
      (map - '() '() '())
    => '())

  (check
      (map - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map +
	'(1 2 3)
	'(10 20 30))
    => '(11 22 33))

  (check
      (map +
	'(1 2 3)
	'(10 20 30)
	'(100 200 300))
    => '(111 222 333))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(for-each
	    (lambda (e)
	      (set! r (+ e r)))
	  '())
	r)
    => 0)

  (check
      (let ((r 0))
	(for-each
	    (lambda (e1 e2)
	      (set! r (+ e1 e2 r)))
	  '() '())
	r)
    => 0)

  (check
      (let ((r 0))
	(for-each
	    (lambda (e1 e2 e3)
	      (set! r (+ e1 e2 e3 r)))
	  '() '() '())
	r)
    => 0)

  (check
      (let ((r '(0 0)))
	(for-each
	    (lambda (e1 e2)
	      (set! r (list (+ e1 (car r))
			    (+ e2 (cadr r)))))
	  '(1 10 100)
	  '(2 20 200))
	r)
    => '(111 222))


  (check
      (let ((r '(0 0 0)))
	(for-each
	    (lambda (e1 e2 e3)
	      (set! r (list (+ e1 (car r))
			    (+ e2 (cadr r))
			    (+ e3 (caddr r)))))
	  '(1 10 100)
	  '(2 20 200)
	  '(3 30 300))
	r)
    => '(111 222 333))

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
	   (set! r (list (+ e1 (car r))
			 (+ e2 (cadr r)))))
	 '(1 10 100)
	 '(2 20 200))
	r)
    => '(111 222))


  (check
      (let ((r '(0 0 0)))
	(for-each*
	 (lambda (e1 e2 e3)
	   (set! r (list (+ e1 (car r))
			 (+ e2 (cadr r))
			 (+ e3 (caddr r)))))
	 '(1 10 100)
	 '(2 20 200)
	 '(3 30 300))
	r)
    => '(111 222 333))

  (check
      (let ((r '(0 0 0)))
	(for-each*
	 (lambda (e1 e2 e3)
	   (set! r (list (+ e1 (car r))
			 (+ e2 (cadr r))
			 (+ e3 (caddr r)))))
	 '(1 10 100)
	 '(2 20 200)
	 (circular-list 3 30 300))
	r)
    => '(111 222 333))

;;; --------------------------------------------------------------------

  (let ()
    (define (f x)
      (list x (- x)))

    (check
	(append-map f '())
      => '())

    (check
	(append-map list '() '())
      => '())

    (check
	(append-map list '() '() '())
      => '())

    (check
	(append-map f '(1))
      => '(1 -1))

    (check
	(append-map list '(1) '(2))
      => '(1 2))

    (check
	(append-map list '(1) '(2) '(3))
      => '(1 2 3))

    (check
	(append-map f '(1 3 8))
      => '(1 -1 3 -3 8 -8))

    (check
	(append-map list
		    '(1 2 3)
		    '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map list
		    '(1 2 3)
		    '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map list
		    '(1 2 3)
		    '(10 20 30)
		    '(100 200 300))
      => '(1 10 100 2 20 200 3 30 300))

    (check
	(append-map list
		    '(1 2)
		    '(10 20 30)
		    '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map list
		    '(1 2 3)
		    '(10 20)
		    '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map list
		    '(1 2 3)
		    '(10 20 30)
		    '(100 200))
      => '(1 10 100 2 20 200))

;;; --------------------------------------------------------------------

    (check
	(append-map! f '())
      => '())

    (check
	(append-map! list '() '())
      => '())

    (check
	(append-map! list '() '() '())
      => '())

    (check
	(append-map! f '(1))
      => '(1 -1))

    (check
	(append-map! list '(1) '(2))
      => '(1 2))

    (check
	(append-map! list '(1) '(2) '(3))
      => '(1 2 3))

    (check
	(append-map! f '(1 3 8))
      => '(1 -1 3 -3 8 -8))

    (check
	(append-map! list
		     '(1 2 3)
		     '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map! list
		     '(1 2 3)
		     '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(append-map! list
		     '(1 2 3)
		     '(10 20 30)
		     '(100 200 300))
      => '(1 10 100 2 20 200 3 30 300))

    (check
	(append-map! list
		     '(1 2)
		     '(10 20 30)
		     '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map! list
		     '(1 2 3)
		     '(10 20)
		     '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(append-map! list
		     '(1 2 3)
		     '(10 20 30)
		     '(100 200))
      => '(1 10 100 2 20 200))

    )

;;; --------------------------------------------------------------------

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x)
	   (set! r (cons x r)))
	 '(1 2 3))
	r)
    => '((3)
	 (2 3)
	 (1 2 3)))

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x)
	   (set! r (cons x r)))
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y)
	   (set! r (cons (list x y)
			 r)))
	 '()
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y z)
	   (set! r (cons (list x y z)
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
	   (set! r (cons x r)))
	 '(1))
	r)
    => '((1)))

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x)
	   (set! r (cons x r)))
	 '(1 2))
	r)
    => '((2)
	 (1 2)))

  (check
      (let ((r '()))
	(pair-for-each
	 (lambda (x y)
	   (set! r (cons (list x y)
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
	   (set! r (cons (list x y z)
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
	     (set! r (cons (list x y z)
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
	     (set! r (cons (list x y z)
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
	     (set! r (cons (list x y z)
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
	     (set! r (cons (list x y z)
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
	     (set! r (cons (list x y z)
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
	     (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons x r)))
	 '(1 2 3))
	r)
    => '((3)
	 (2 3)
	 (1 2 3)))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x)
	   (set! r (cons x r)))
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y z)
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y)
			 r)))
	 '()
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x)
	   (set! r (cons x r)))
	 '(1))
	r)
    => '((1)))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x)
	   (set! r (cons x r)))
	 '(1 2))
	r)
    => '((2)
	 (1 2)))

  (check
      (let ((r '()))
	(pair-for-each*
	 (lambda (x y)
	   (set! r (cons (list x y)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
	   (set! r (cons (list x y z)
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
      (map! + '(1 2 3))
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
	    '(1 2 3)
	    '(10 20 30))
    => '(11 22 33))

  (check
      (map! +
	    '(1 2 3)
	    '(10 20 30)
	    '(100 200 300))
    => '(111 222 333))

;;; Only the first list argument can be shorter!!!
  (check
      (map! +
	    '(1 2)
	    '(10 20 30)
	    '(100 200 300))
    => '(111 222))

  (check
      (map! +
	    '()
	    '(10 20 30)
	    '(100 200 300))
    => '())

  (check
      (map! +
	    '(3 1 4 1)
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
      (map*! + '(1 2 3))
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
	     '(1 2 3)
	     '(10 20 30))
    => '(11 22 33))

  (check
      (map*! +
	     '(1 2 3)
	     '(10 20 30)
	     '(100 200 300))
    => '(111 222 333))

;;; Only the first list argument can be shorter!!!
  (check
      (map*! +
	     '(1 2)
	     '(10 20 30)
	     '(100 200 300))
    => '(111 222))

  (check
      (map*! +
	     '()
	     '(10 20 30)
	     '(100 200 300))
    => '())

  (check
      (map*! +
	     '(3 1 4 1)
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

  )


(parameterise ((check-test-name 'filtering))

  (check
      (filter even? '())
    => '())

  (check
      (filter even? '(1))
    => '())

  (check
      (filter even? '(2))
    => '(2))

  (check
      (filter even? numbers)
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
	    (partition even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(1 3)))
	list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(2 4)))
	list)
    => '((2 4) ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? numbers))
	list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(1 3)))
	list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(2 4)))
	list)
    => '((2 4) ()))


  (check
      (call-with-values
	  (lambda ()
	    (partition! even? (list-copy numbers)))
	list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (remove 8 numbers)
    => '(0 1 2 3 4 5 6 7 9))

  (check
      (remove 8 '(1 2 3))
    => '(1 2 3))

  (check
      (remove 8 '(1))
    => '(1))

  (check
      (remove 8 '())
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

  )


(parameterise ((check-test-name 'finding))

  (check
      (find even? '())
    => #f)

  (check
      (find even? '(1))
    => #f)

  (check
      (find even? '(2))
    => 2)

  (check
      (find even? '(1 2 3))
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
      (take-while! even? '(1))
    => '())

  (check
      (take-while! even? '(2))
    => '(2))

  (check
      (take-while! even? '(2 4 6 1 3))
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
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (span even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (span even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (span even? '(2 4 6 1 3)))
	list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (span! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (span! even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (span! even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (span! even? '(2 4 6 1 3)))
	list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (break even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (break even? '(1)))
	list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (break even? '(2)))
	list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (break even? '(1 3 2 4 6)))
	list)
    => '((1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (break! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (break! even? '(1)))
	list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (break! even? '(2)))
	list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (break! even? '(1 3 2 4 6)))
	list)
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
      (memq 'a '(a b c))
    => '(a b c))

  (check
      (memq 'b '(a b c))
    => '(b c))

  (check
      (memq 'a '(b c d))
    => #f)

  (check
      (memq (list 'a) '(b (a) c))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (member '(a)
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
      (memv 101 '(100 101 102))
    => '(101 102))

  )


(parameterise ((check-test-name 'deletion))

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
      (delete! 8 '(1))
    => '(1))

  (check
      (delete! 8 '(8))
    => '())

  (check
      (delete! 8 '(1 2 3))
    => '(1 2 3))

  (check
      (delete! 8 '(1 2 8 3 4 5 8 6 7 8))
    => '(1 2 3 4 5 6 7))


  (check
      (delete! 8 '() =)
    => '())

  (check
      (delete! 8 '(1) =)
    => '(1))

  (check
      (delete! 8 '(8) =)
    => '())

  (check
      (delete! 8 '(1 2 3) =)
    => '(1 2 3))

  (check
      (delete! 8 '(1 2 8 3 4 5 8 6 7 8) =)
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
      (delete-duplicates! '(1))
    => '(1))

  (check
      (delete-duplicates! '(1 2))
    => '(1 2))

  (check
      (delete-duplicates! '(1 1))
    => '(1))

  (check
      (delete-duplicates! '(1 1 1))
    => '(1))

  (check
      (delete-duplicates! '(1 2 3 2 4 5 4 6 1 7))
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
      (delete-duplicates! '(1) =)
    => '(1))

  (check
      (delete-duplicates! '(1 2) =)
    => '(1 2))

  (check
      (delete-duplicates! '(1 1) =)
    => '(1))

  (check
      (delete-duplicates! '(1 1 1) =)
    => '(1))

  (check
      (delete-duplicates! '(1 2 3 2 4 5 4 6 1 7) =)
    => '(1 2 3 4 5 6 7))

  )


(parameterise ((check-test-name 'sorted-lists))

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

  )


(parameterise ((check-test-name 'alists))

  (check
      (assoc 'a
	     '((a . 1)
	       (b . 2)
	       (c . 3)))
    => '(a . 1))

  (check
      (assoc 'b
	     '((a . 1)
	       (b . 2)
	       (c . 3)))
    => '(b . 2))

  (check
      (assoc 'c
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
      (assq 'c
	    '())
    => #f)

  (check
      (assq 'd
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => #f)

  (check
      (assq 'a
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(a . 1))

  (check
      (assq 'b
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(b . 2))

  (check
      (assq 'c
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (assv 'c
	    '())
    => #f)

  (check
      (assv 'd
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => #f)

  (check
      (assv 'a
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(a . 1))

  (check
      (assv 'b
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(b . 2))

  (check
      (assv 'c
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

  )


(parameterise ((check-test-name 'circular-lists))

  (check
      (let ((ell (circular-list 1 2)))
	(list (car ell)
	      (cadr ell)
	      (caddr ell)
	      (cadddr ell)))
    => '(1 2 1 2))

;;; --------------------------------------------------------------------

  (check
      (let ((ell (list->circular-list! '(1 2 3))))
	(list (car ell)
	      (cadr ell)
	      (caddr ell)
	      (cadddr ell)
	      (cadddr (cdr ell))))
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

  )


(parameterise ((check-test-name 'sets))

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

  )


;;;; done

(check-report)

;;; end of file
