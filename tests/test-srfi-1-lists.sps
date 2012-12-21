;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 1
;;;Date: Mon Dec 29, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (srfi :1) srfi.)
  (rnrs mutable-pairs)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 1 lists\n")


;;;; helpers

(define numbers '(0 1 2 3 4 5 6 7 8 9))


(parametrise ((check-test-name 'circular))

;;;Miscellaneous tests on circular lists.

  (let ()

    (define end  (srfi.cons  1 '()))
    (define tail (srfi.cons* 3 2 end))
    (define ell  (srfi.cons* 5 4 tail))
    (srfi.set-cdr! end tail)

    (check (srfi.car ell)			=> 5)
    (check (srfi.cadr ell)			=> 4)
    (check (srfi.caddr ell)			=> 3)
    (check (srfi.cadddr ell)			=> 2)
    (check (srfi.cadddr (srfi.cdr ell))		=> 1)
    (check (srfi.cdddr (srfi.cddr ell))		=> tail)
    (check (srfi.cadddr (srfi.cddr ell))	=> 3)
    (check (srfi.cadddr (srfi.cdddr ell))	=> 2)

    (check (srfi.circular-list? ell)		=> #t)

    #f)

  #t)


(parametrise ((check-test-name 'constructors))

  (check
      (srfi.cons* 1 2 3 4 '(5 6 7 8))
    => '(1 2 3 4 5 6 7 8))

;;; --------------------------------------------------------------------

  (check
      (srfi.xcons 1 2)
    => '(2 . 1))

;;; --------------------------------------------------------------------

  (check
      (srfi.make-list 4 'c)
    => '(c c c c))

  (check
      (srfi.make-list 0)
    => '())

  (check
      (srfi.make-list 0 #f)
    => '())

;;; --------------------------------------------------------------------

  (check
      (srfi.list-tabulate 4 (lambda (i)
			      (srfi.cons i 'a)))
    => '((0 . a)
	 (1 . a)
	 (2 . a)
	 (3 . a)))

  (check
      (srfi.list-tabulate 1 (lambda (i)
			      (cons i 'a)))
    => '((0 . a)))

  (check
      (srfi.list-tabulate 0 (lambda (i)
			      (cons i 'a)))
    => '())

;;; --------------------------------------------------------------------

  (check
      (srfi.list-copy '())
    => '())

  (check
      (srfi.list-copy '(1))
    => '(1))

  (check
      (srfi.list-copy numbers)
    => numbers)

  (check
      (srfi.list-copy '(1 . 2))
    => '(1 . 2))

  (check
      (srfi.list-copy '(1 2 3 . 4))
    => '(1 2 3 . 4))

;;; --------------------------------------------------------------------

  (check
      (srfi.tree-copy '())
    => '())

  (check
      (srfi.tree-copy '(1))
    => '(1))

  (check
      (srfi.tree-copy '(1 . 2))
    => '(1 . 2))

  (let ((ell '(1 2 3 4)))
    (check
	(srfi.tree-copy ell)
      => ell))

  (let ((ell '(1 2 3 4 . 5)))
    (check
	(srfi.tree-copy ell)
      => ell))

  (let ((ell '(1 (2 (3 4)
		    5
		    6)
		 7 8
		 (9 10))))
    (check
	(srfi.tree-copy ell)
      => ell))

  (let ((ell '(1 (2 (3 . 4)
		    5
		    6)
		 7 8
		 (9 . 10))))
    (check
	(srfi.tree-copy ell)
      => ell))

;;; --------------------------------------------------------------------

  (check
      (srfi.iota 5 10)
    => '(10 11 12 13 14))

  (check
      (srfi.iota 5)
    => '(0 1 2 3 4))

  (check
      (srfi.iota 5 10 5)
    => '(10 15 20 25 30))

  (check
      (guard (exc (else #t))
	(srfi.iota -5 10 5))
    => #t)

  #f)


(parametrise ((check-test-name 'kind-predicates))

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
      (list? (srfi.circular-list 1 2))
    => #f)

  (check
      (list? (srfi.circular-list 1 2 3 4 5 6))
    => #f)

  (check
      (list? (srfi.list 1 2 3 4 (srfi.circular-list 5 6 7 8)))
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
      (srfi.circular-list? '())
    => #f)

  (check
      (srfi.circular-list? '(1 2 3))
    => #f)

  (check
      (srfi.circular-list? '(1 . 2))
    => #f)

  (check
      (srfi.circular-list? '(1 2 3 . 4))
    => #f)

  (check
      (srfi.circular-list? (srfi.circular-list 1 2))
    => #t)

  (check
      (srfi.circular-list? (srfi.circular-list 1 2 3 4 5 6))
    => #t)

  (check
      (srfi.circular-list? (srfi.list 1 2 3 4 (srfi.circular-list 5 6 7 8)))
    => #f)

  (check
      (srfi.circular-list? (cons 1 (cons 2 (cons 3 (cons 4 (srfi.circular-list 5 6 7 8))))))
    => #t)

  (check
      (srfi.circular-list? 123)
    => #f)

  (check
      (srfi.circular-list? #\a)
    => #f)

  (check
      (srfi.circular-list? 'alpha)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (srfi.dotted-list? '())
    => #f)

  (check
      (srfi.dotted-list? '(1 2 3))
    => #f)

  (check
      (srfi.dotted-list? '(1 . 2))
    => #t)

  (check
      (srfi.dotted-list? '(1 2 3 . 4))
    => #t)

  (check
      (srfi.dotted-list? (srfi.circular-list 1 2))
    => #f)

  (check
      (srfi.dotted-list? (srfi.circular-list 1 2 3 4 5 6))
    => #f)

  (check
      (srfi.dotted-list? (srfi.list 1 2 3 4 (srfi.circular-list 5 6 7 8)))
    => #f)

  (check
      (srfi.dotted-list? (cons 1 (cons 2 (cons 3 (cons 4 (srfi.circular-list 5 6 7 8))))))
    => #f)

  (check
      (srfi.dotted-list? 123)
    => #t)

  (check
      (srfi.dotted-list? #\a)
    => #t)

  (check
      (srfi.dotted-list? 'alpha)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.null? '(1 2))
    => #f)

  (check
      (srfi.null? '(1 . 2))
    => #f)

  (check
      (srfi.null? '(1))
    => #f)

  (check
      (srfi.null? '())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.pair? '(1 2))
    => #t)

  (check
      (srfi.pair? '(1 . 2))
    => #t)

  (check
      (srfi.pair? '(1))
    => #t)

  (check
      (srfi.pair? '())
    => #f)

  (check
      (srfi.pair? 1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (srfi.not-pair? '(1 2))
    => #f)

  (check
      (srfi.not-pair? '(1 . 2))
    => #f)

  (check
      (srfi.not-pair? '(1))
    => #f)

  (check
      (srfi.not-pair? '())
    => #t)

  (check
      (srfi.not-pair? 1)
    => #t)

  #t)


(parametrise ((check-test-name 'comparison))

  (check
      (srfi.list= =)
    => #t)

  (check
      (srfi.list= = numbers)
    => #t)

  (check
      (srfi.list= = numbers numbers)
    => #t)

  (check
      (srfi.list= = numbers numbers numbers)
    => #t)

  (check
      (srfi.list= = numbers numbers numbers numbers)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.list= =
		   '(1 2 3 4)
		   '(1 9 3 4))
    => #f)

  (check
      (srfi.list= =
		   '(1 2 3)
		   '(9 2 3))
    => #f)

  (check
      (srfi.list= =
		   '(1 2 3)
		   '(9 2 3)
		   '(9 2 3))
    => #f)
  (check
      (srfi.list= =
		   '(9 2 3)
		   '(1 2 3)
		   '(9 2 3))
    => #f)

  (check
      (srfi.list= =
		   '(9 2 3)
		   '(9 2 3)
		   '(1 2 3))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (srfi.list= = '(1))
    => #t)

  (check
      (srfi.list= = '(1) '(1))
    => #t)

  (check
      (srfi.list= = '(1) '(1) '(1))
    => #t)

  (check
      (srfi.list= = '(1) '(1) '(1) '(1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.list= = '(1) '(1 2))
    => #f)

  (check
      (srfi.list= = '(1 2) '(1))
    => #f)

  (check
      (srfi.list= = '(1 2) '(1) '(1))
    => #f)

  (check
      (srfi.list= = '(1) '(1 2) '(1))
    => #f)

  (check
      (srfi.list= = '(1) '(1) '(1 2))
    => #f)

  (check
      (srfi.list= = numbers '(0 1 2 3 4))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (srfi.list= = '())
    => #t)

  (check
      (srfi.list= = '() '())
    => #t)

  (check
      (srfi.list= = '() '() '())
    => #t)

  (check
      (srfi.list= = '() '() '() '())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.list= = '() numbers)
    => #f)

  (check
      (srfi.list= = numbers '())
    => #f)

  (check
      (srfi.list= = numbers '() '())
    => #f)

  (check
      (srfi.list= = '() numbers '())
    => #f)

  (check
      (srfi.list= = '() '() numbers)
    => #f)

  #f)


(parametrise ((check-test-name 'selectors))

  (check (srfi.first numbers)	=> 0)
  (check (srfi.second numbers)	=> 1)
  (check (srfi.third numbers)	=> 2)
  (check (srfi.fourth numbers)	=> 3)
  (check (srfi.fifth numbers)	=> 4)
  (check (srfi.sixth numbers)	=> 5)
  (check (srfi.seventh numbers) => 6)
  (check (srfi.eighth numbers)	=> 7)
  (check (srfi.ninth numbers)	=> 8)
  (check (srfi.tenth numbers)	=> 9)

;;; --------------------------------------------------------------------

  (check
      (srfi.list-ref numbers 0)
    => 0)

  (check
      (srfi.list-ref numbers 3)
    => 3)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (srfi.car+cdr numbers))
	list)
    => (list (srfi.car numbers) (srfi.cdr numbers)))

;;; --------------------------------------------------------------------

  (check
      (srfi.drop-right numbers 5)
    => '(0 1 2 3 4))

  (check
      (srfi.drop-right numbers 0)
    => numbers)

  (check
      (srfi.drop-right '() 0)
    => '())

  (check
      (srfi.drop-right numbers 10)
    => '())

  (check
      (append (srfi.drop-right numbers 3)
	      (srfi.take-right numbers 3))
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (srfi.take-right numbers 5)
    => '(5 6 7 8 9))

  (check
      (srfi.take-right numbers 0)
    => '())

  (check
      (srfi.take-right '() 0)
    => '())

  (check
      (srfi.take-right numbers 10)
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (srfi.drop-right numbers 5)
    => '(0 1 2 3 4))

  (check
      (srfi.drop-right numbers 0)
    => numbers)

  (check
      (srfi.drop-right '() 0)
    => '())

  (check
      (srfi.drop-right numbers 10)
    => '())

;;; --------------------------------------------------------------------

  (check
      (srfi.drop-right! '(1 3 5) 1)
    => '(1 3))

  (check
      (guard (exc (else #t))
	(srfi.drop-right! '() 1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (srfi.split-at numbers 5))
	list)
    => '((0 1 2 3 4)
	 (5 6 7 8 9)))

;;; --------------------------------------------------------------------

  (check
      (srfi.last numbers)
    => 9)

  (check
      (srfi.last '(9))
    => 9)

;;; This raises an error.
  ;;
  ;; (check
  ;;     (srfi.last '())
  ;;   => '())

;;; --------------------------------------------------------------------

  (check
      (srfi.last-pair numbers)
    => '(9))

  (check
      (srfi.last-pair '(9))
    => '(9))

;;; The empty list is not a pair, so the following raises an error.
  ;;
  ;; (check
  ;;     (srfi.last-pair '())
  ;;   => '())

  #f)


(parametrise ((check-test-name 'miscellaneous))

  (check
      (srfi.length '(1 2 3 4 5 6))
    => 6)

  (check
      (srfi.length '(1))
    => 1)

  (check
      (srfi.length '())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (srfi.length+ '())
    => 0)

  (check
      (srfi.length+ '(1))
    => 1)

  (check
      (srfi.length+ '(1 2 3 4 5 6))
    => 6)

  (check
      (srfi.length+ (srfi.circular-list 1 2 3 4 5 6))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (srfi.append '(x) '(y))
    => '(x y))

  (check
      (srfi.append '(a) '(b c d))
    => '(a b c d))

  (check
      (srfi.append '(a (b)) '((c)))
    => '(a (b) (c)))

  (check
      (srfi.append '(a b) '(c . d))
    => '(a b c . d))

  (check
      (srfi.append '() 'a)
    => 'a)

  (check
      (srfi.append '(a) '())
    => '(a))

  (check
      (srfi.append '(x y))
    => '(x y))

  (check
      (srfi.append)
    => '())

;;; --------------------------------------------------------------------

  (check
      (srfi.append!)
    => '())

  (check
      (srfi.append! '())
    => '())

  (check
      (srfi.append! '() '())
    => '())

  (check
      (srfi.append! '() '() '())
    => '())

  (check
      (srfi.append! '(y))
    => '(y))

  (check
      (srfi.append! '(x) '(y))
    => '(x y))

  (check
      (srfi.append! '(x) '(y) '(z))
    => '(x y z))

  (check
      (srfi.append! '(a) '(b c d))
    => '(a b c d))

  (check
      (srfi.append! '(a (b)) '((c)))
    => '(a (b) (c)))

  (check
      (srfi.append! '(a b) '(c . d))
    => '(a b c . d))

  (check
      (srfi.append! '() 'a)
    => 'a)

  (check
      (srfi.append! '(a) '())
    => '(a))

  (check
      (srfi.append! '(x y))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (srfi.concatenate '())
    => '())

  (check
      (srfi.concatenate '(()))
    => '())

  (check
      (srfi.concatenate '(() ()))
    => '())

  (check
      (srfi.concatenate '(() () ()))
    => '())

  (check
      (srfi.concatenate '((x)))
    => '(x))

  (check
      (srfi.concatenate '((x) (y)))
    => '(x y))

  (check
      (srfi.concatenate '((x) (y) (z)))
    => '(x y z))

  (check
      (srfi.concatenate '((a)
			  (b c d)))
    => '(a b c d))

  (check
      (srfi.concatenate '((a b)
			  (c d)))
    => '(a b c d))

  (check
      (srfi.concatenate '((a b)
			  (c d)
			  (e f)))
    => '(a b c d e f))

  (check
      (srfi.concatenate '((a b c d e f g)
			  (h i)
			  (l m n o)))
    => '(a b c d e f g h i l m n o))

  (check
      (srfi.concatenate '((a (b)) ((c))))
    => '(a (b) (c)))

  (check
      (srfi.concatenate '((a b) (c . d)))
    => '(a b c . d))

  (check
      (srfi.concatenate '(() (a)))
    => '(a))

  (check
      (srfi.concatenate '((x y)))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (srfi.concatenate! '())
    => '())

  (check
      (srfi.concatenate! '(()))
    => '())

  (check
      (srfi.concatenate! '(() ()))
    => '())

  (check
      (srfi.concatenate! '(() () ()))
    => '())

  (check
      (srfi.concatenate! '((x)))
    => '(x))

  (check
      (srfi.concatenate! '((x) (y)))
    => '(x y))

  (check
      (srfi.concatenate! '((x) (y) (z)))
    => '(x y z))

  (check
      (srfi.concatenate! '((a)
			   (b c d)))
    => '(a b c d))

  (check
      (srfi.concatenate! '((a b)
			   (c d)))
    => '(a b c d))

  (check
      (srfi.concatenate! '((a b)
			   (c d)
			   (e f)))
    => '(a b c d e f))

  (check
      (srfi.concatenate! '((a b c d e f g)
			   (h i)
			   (l m n o)))
    => '(a b c d e f g h i l m n o))

  (check
      (srfi.concatenate! '((a (b)) ((c))))
    => '(a (b) (c)))

  (check
      (srfi.concatenate! '((a b) (c . d)))
    => '(a b c . d))

  (check
      (srfi.concatenate! '(() (a)))
    => '(a))

  (check
      (srfi.concatenate! '((x y)))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (srfi.reverse '())
    => '())

  (check
      (srfi.reverse '(1))
    => '(1))

  (check
      (srfi.reverse '(1 2 3))
    => '(3 2 1))

  (check
      (srfi.reverse! '())
    => '())

  (check
      (srfi.reverse! '(1))
    => '(1))

  (check
      (srfi.reverse! '(1 2 3))
    => '(3 2 1))

;;; --------------------------------------------------------------------

  (check
      (srfi.append-reverse '() '())
    => '())

  (check
      (srfi.append-reverse '(x) '(y))
    => '(x y))

  (check
      (srfi.append-reverse '(1 2 3) '(4 5 6))
    => '(3 2 1 4 5 6))

  (check
      (srfi.append-reverse '(a) '(b c d))
    => '(a b c d))

  (check
      (srfi.append-reverse '(a (b)) '((c)))
    => '((b) a (c)))

  (check
      (srfi.append-reverse '(a) '())
    => '(a))

;;; --------------------------------------------------------------------

  (check
      (srfi.append-reverse! '() '())
    => '())

  (check
      (srfi.append-reverse! '(x) '(y))
    => '(x y))

  (check
      (srfi.append-reverse! '(1 2 3) '(4 5 6))
    => '(3 2 1 4 5 6))

  (check
      (srfi.append-reverse! '(a) '(b c d))
    => '(a b c d))

  (check
      (srfi.append-reverse! '(a (b)) '((c)))
    => '((b) a (c)))

  (check
      (srfi.append-reverse! '(a) '())
    => '(a))

;;; --------------------------------------------------------------------

  (check
      (srfi.zip '(one two three)
		'(1 2 3)
		'(odd even odd even odd even odd even))
    => '((one 1 odd) (two 2 even) (three 3 odd)))

  (check
      (srfi.zip '(1 2 3))
    => '((1) (2) (3)))

  (check
      (srfi.zip '(3 1 4 1)
		(srfi.circular-list #f #t))
    => '((3 #f)
	 (1 #t)
	 (4 #f)
	 (1 #t)))

;;; --------------------------------------------------------------------

  (check
      (srfi.unzip1 '((1)))
    => '(1))

  (check
      (srfi.unzip1 '((1)
		     (2)))
    => '(1 2))

  (check
      (srfi.unzip1 '((1)
		     (2)
		     (3)))
    => '(1 2 3))

  (check
      (srfi.unzip1 '((1 one)
		     (2 two)
		     (3 three)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (srfi.unzip2 '((1 one))))
	list)
    => '((1)
	 (one)))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.unzip2 '((1 one)
			   (2 two))))
	list)
    => '((1 2)
	 (one two)))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.unzip2 '((1 one)
			   (2 two)
			   (3 three))))
	list)
    => '((1 2 3)
	 (one two three)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (srfi.unzip3 '((1 10 100)
			   (2 20 200)
			   (3 30 300))))
	list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.unzip4 '((1 10 100 1000)
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
	    (srfi.unzip5 '((1 10 100 1000 10000)
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
      (srfi.count even? numbers)
    => 5)

  (check
      (srfi.count even? '(1))
    => 0)

  (check
      (srfi.count even? '(2))
    => 1)

  (check
      (srfi.count even? '())
    => 0)

  #f)


(parametrise ((check-test-name 'left-folding))

  (check
      (srfi.fold + 0 numbers)
    => 45)

  (check
      (srfi.fold cons '() numbers)
    => '(9 8 7 6 5 4 3 2 1 0))

  (check
      (srfi.fold cons '(4 5 6) '(3 2 1))
    => '(1 2 3 4 5 6))

  (check
      (srfi.fold cons '(4 5 6) '())
    => '(4 5 6))

  (check
      (srfi.fold cons '(4 5 6) '(3))
    => '(3 4 5 6))

  (check
      (srfi.fold (lambda (x count)
		   (if (symbol? x)
		       (+ count 1)
		     count))
		 0
		 '(a 1 b 2 c 3))
    => 3)

  (check
      (srfi.fold (lambda (s len)
		   (max len (string-length s)))
		 0
		 '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (srfi.fold cons* '()
		 '(a b c)
		 '(1 2 3 4 5))
    => '(c 3 b 2 a 1))

  (check
      (srfi.fold cons* '()
		 '(a)
		 '(1))
    => '(a 1))

  (check
      (srfi.fold (lambda (a b c knil)
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
      (srfi.fold (lambda (a b c knil)
		   (cons (list a b c)
			 knil))
		 '()
		 '(1 2 3)
		 '(10 20)
		 '(100 200 300 400))
    => '((2 20 200)
	 (1 10 100)))

  #f)


(parametrise ((check-test-name 'right-folding))

  (check
      (srfi.fold-right cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (srfi.fold-right cons '() numbers)
    => numbers)

  (check
      (srfi.fold-right + 0 numbers)
    => 45)

  (check
      (srfi.fold-right cons '(4 5 6) '(1 2 3))
    => '(1 2 3 4 5 6))

  (check
      (srfi.fold-right (lambda (x count)
			 (if (symbol? x)
			     (+ count 1)
			   count))
		       0
		       '(a 1 b 2 c 3))
    => 3)

  (check
      (srfi.fold-right (lambda (s len)
			 (max len (string-length s)))
		       0
		       '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (srfi.fold-right (lambda (x l)
			 (if (even? x)
			     (cons x l)
			   l))
		       '()
		       '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (srfi.fold-right cons* '()
		       '(a b c)
		       '(1 2 3 4 5))
    => '(a 1 b 2 c 3))

  (check
      (srfi.fold-right cons* '()
		       '(a)
		       '(1))
    => '(a 1))

  (check
      (srfi.fold-right (lambda (a b c knil)
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
      (srfi.fold-right (lambda (a b c knil)
			 (cons (list a b c)
			       knil))
		       '()
		       '(1 2 3)
		       '(10 20)
		       '(100 200 300 400))
    => '((1 10 100)
	 (2 20 200)))

  #f)


(parametrise ((check-test-name 'pair-folding))

  (check
      (srfi.pair-fold (lambda (elm knil)
			(cons (car elm) knil))
		      '(999)
		      '(1 2 3))
    => '(3 2 1 999))

  (check
      (srfi.pair-fold (lambda (pair tail)
			(set-cdr! pair tail)
			pair)
		      '()
		      (srfi.list-copy numbers))
    => (srfi.reverse numbers))

;;; --------------------------------------------------------------------

  (check
      (srfi.pair-fold (lambda (a b c knil)
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
      (srfi.pair-fold (lambda (a b c knil)
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
      (srfi.pair-fold (lambda (a b c knil)
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
      (srfi.pair-fold (lambda (a b c knil)
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
      (srfi.pair-fold (lambda (a b c knil)
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
      (srfi.pair-fold (lambda (a b c knil)
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
      (srfi.pair-fold (lambda (a b c knil)
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
      (srfi.pair-fold (lambda (a b c knil)
			(cons (list (car a)
				    (car b)
				    (car c))
			      knil))
		      '(999)
		      '(1 2 3)
		      '(10 20 30)
		      '())
    => '(999))

  #f)


(parametrise ((check-test-name 'reducing))

  (check
      (srfi.reduce + 0 numbers)
    => 45)

  (check
      (srfi.reduce + 0 '())
    => 0)

  (check
      (srfi.reduce max 0 '(1 2 3 4 5))
    => 5)

  #f)


(parametrise ((check-test-name 'unfolding))

  (check
      (srfi.unfold (lambda (x) (< 5 x))
		   (lambda (x) (* x x))
		   (lambda (x) (+ x 1))
		   1)
    => '(1 4 9 16 25))

  (check
      (srfi.unfold (lambda (x) (< 5 x))
		   (lambda (x) (* x x))
		   (lambda (x) (+ x 1))
		   1
		   (lambda (x) (- x)))
    => '(1 4 9 16 25 . -6))

  (check
      (srfi.unfold (lambda (x) #t)
		   (lambda (x) (* x x))
		   (lambda (x) (+ x 1))
		   1
		   (lambda (x) (- x)))
    => -1)

  (check
      (srfi.unfold null? car cdr numbers)
    => numbers)

  (check
      (srfi.unfold srfi.not-pair? car cdr '(1 2 3 4 . 5) values)
    => '(1 2 3 4 . 5))

  (check
      (srfi.unfold null? car cdr '(1 2 3) (lambda (x) '(4 5 6)))
    => '(1 2 3 4 5 6))

;;; --------------------------------------------------------------------

  (check
      (srfi.unfold-right zero?
			 (lambda (x) (* x x))
			 (lambda (x) (- x 1))
			 5)
    => '(1 4 9 16 25))

  (check
      (srfi.unfold-right null? car cdr '(1 2 3 4 5))
    => '(5 4 3 2 1))

  (check
      (srfi.unfold-right null? car cdr '(3 2 1) '(4 5 6))
    => '(1 2 3 4 5 6))

  #f)


(parametrise ((check-test-name 'mapping))

  (check
      (srfi.map - '())
    => '())

  (check
      (srfi.map - '() '())
    => '())

  (check
      (srfi.map - '() '() '())
    => '())

  (check
      (srfi.map - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (srfi.map +
		'(1 2 3)
		'(10 20 30))
    => '(11 22 33))

  (check
      (srfi.map +
		'(1 2 3)
		'(10 20 30)
		'(100 200 300))
    => '(111 222 333))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(srfi.for-each
	 (lambda (e)
	   (set! r (+ e r)))
	 '())
	r)
    => 0)

  (check
      (let ((r 0))
	(srfi.for-each
	 (lambda (e1 e2)
	   (set! r (+ e1 e2 r)))
	 '() '())
	r)
    => 0)

  (check
      (let ((r 0))
	(srfi.for-each
	 (lambda (e1 e2 e3)
	   (set! r (+ e1 e2 e3 r)))
	 '() '() '())
	r)
    => 0)

  (check
      (let ((r '(0 0)))
	(srfi.for-each
	 (lambda (e1 e2)
	   (set! r (list (+ e1 (car r))
			 (+ e2 (cadr r)))))
	 '(1 10 100)
	 '(2 20 200))
	r)
    => '(111 222))


  (check
      (let ((r '(0 0 0)))
	(srfi.for-each
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

  (let ()
    (define (f x)
      (list x (- x)))

    (check
	(srfi.append-map f '())
      => '())

    (check
	(srfi.append-map list '() '())
      => '())

    (check
	(srfi.append-map list '() '() '())
      => '())

    (check
	(srfi.append-map f '(1))
      => '(1 -1))

    (check
	(srfi.append-map list '(1) '(2))
      => '(1 2))

    (check
	(srfi.append-map list '(1) '(2) '(3))
      => '(1 2 3))

    (check
	(srfi.append-map f '(1 3 8))
      => '(1 -1 3 -3 8 -8))

    (check
	(srfi.append-map list
			 '(1 2 3)
			 '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(srfi.append-map list
			 '(1 2 3)
			 '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(srfi.append-map list
			 '(1 2 3)
			 '(10 20 30)
			 '(100 200 300))
      => '(1 10 100 2 20 200 3 30 300))

    (check
	(srfi.append-map list
			 '(1 2)
			 '(10 20 30)
			 '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(srfi.append-map list
			 '(1 2 3)
			 '(10 20)
			 '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(srfi.append-map list
			 '(1 2 3)
			 '(10 20 30)
			 '(100 200))
      => '(1 10 100 2 20 200))

;;; --------------------------------------------------------------------

    (check
	(srfi.append-map! f '())
      => '())

    (check
	(srfi.append-map! list '() '())
      => '())

    (check
	(srfi.append-map! list '() '() '())
      => '())

    (check
	(srfi.append-map! f '(1))
      => '(1 -1))

    (check
	(srfi.append-map! list '(1) '(2))
      => '(1 2))

    (check
	(srfi.append-map! list '(1) '(2) '(3))
      => '(1 2 3))

    (check
	(srfi.append-map! f '(1 3 8))
      => '(1 -1 3 -3 8 -8))

    (check
	(srfi.append-map! list
			  '(1 2 3)
			  '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(srfi.append-map! list
			  '(1 2 3)
			  '(10 20 30))
      => '(1 10 2 20 3 30))

    (check
	(srfi.append-map! list
			  '(1 2 3)
			  '(10 20 30)
			  '(100 200 300))
      => '(1 10 100 2 20 200 3 30 300))

    (check
	(srfi.append-map! list
			  '(1 2)
			  '(10 20 30)
			  '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(srfi.append-map! list
			  '(1 2 3)
			  '(10 20)
			  '(100 200 300))
      => '(1 10 100 2 20 200))

    (check
	(srfi.append-map! list
			  '(1 2 3)
			  '(10 20 30)
			  '(100 200))
      => '(1 10 100 2 20 200))

    #f)

;;; --------------------------------------------------------------------

  (check
      (let ((r '()))
	(srfi.pair-for-each
	 (lambda (x)
	   (set! r (cons x r)))
	 '(1 2 3))
	r)
    => '((3)
	 (2 3)
	 (1 2 3)))

  (check
      (let ((r '()))
	(srfi.pair-for-each
	 (lambda (x)
	   (set! r (cons x r)))
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(srfi.pair-for-each
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
	(srfi.pair-for-each
	 (lambda (x y)
	   (set! r (cons (list x y)
			 r)))
	 '()
	 '())
	r)
    => '())

  (check
      (let ((r '()))
	(srfi.pair-for-each
	 (lambda (x)
	   (set! r (cons x r)))
	 '(1))
	r)
    => '((1)))

  (check
      (let ((r '()))
	(srfi.pair-for-each
	 (lambda (x)
	   (set! r (cons x r)))
	 '(1 2))
	r)
    => '((2)
	 (1 2)))

  (check
      (let ((r '()))
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
	(srfi.pair-for-each
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
      (srfi.map! - '())
    => '())

  (check
      (srfi.map! - (srfi.list-copy numbers))
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (srfi.map! + '(1 2 3))
    => '(1 2 3))

  (check
      (srfi.map! - '() '())
    => '())

  (check
      (srfi.map! - '() '() '())
    => '())

  (check
      (srfi.map! - '() '() '() '())
    => '())

  (check
      (srfi.map! +
		 '(1 2 3)
		 '(10 20 30))
    => '(11 22 33))

  (check
      (srfi.map! +
		 '(1 2 3)
		 '(10 20 30)
		 '(100 200 300))
    => '(111 222 333))

;;; Only the first list argument can be shorter!!!
  (check
      (srfi.map! +
		 '(1 2)
		 '(10 20 30)
		 '(100 200 300))
    => '(111 222))

  (check
      (srfi.map! +
		 '()
		 '(10 20 30)
		 '(100 200 300))
    => '())

  (check
      (srfi.map! +
		 '(3 1 4 1)
		 (srfi.circular-list 1 0))
    => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (srfi.filter-map
       (lambda (x)
	 (and (number? x)
	      (* x x)))
       '(a 1 b 3 c 7))
    => '(1 9 49))

  (check
      (srfi.filter-map - '())
    => '())

  (check
      (srfi.filter-map - '() '())
    => '())

  (check
      (srfi.filter-map - '() '() '())
    => '())

  (check
      (srfi.filter-map - '() '() '() '())
    => '())

  (check
      (srfi.filter-map - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (srfi.filter-map + '(1 2 3))
    => '(1 2 3))

  (check
      (srfi.filter-map +
		       '(1 2 3)
		       '(10 20 30))
    => '(11 22 33))

  (check
      (srfi.filter-map +
		       '(1 2 3)
		       '(10 20 30)
		       '(100 200 300))
    => '(111 222 333))

  (check
      (srfi.filter-map +
		       '(1 2 3)
		       '(10 20)
		       '(100 200 300))
    => '(111 222))

  (check
      (srfi.filter-map +
		       '(1 2)
		       '(10 20 30)
		       '(100 200 300))
    => '(111 222))

  (check
      (srfi.filter-map +
		       '(1 2 3)
		       '(10 20 30)
		       '(100 200))
    => '(111 222))

  (check
      (srfi.filter-map +
		       '()
		       '(10 20 30)
		       '(100 200 300))
    => '())

  (check
      (srfi.filter-map +
		       '(1 2 3)
		       '()
		       '(100 200 300))
    => '())

  (check
      (srfi.filter-map +
		       '(1 2 3)
		       '(10 20 30)
		       '())
    => '())

  (check
      (srfi.filter-map +
		       '(3 1 4 1)
		       (srfi.circular-list 1 0))
    => '(4 1 5 1))

  #f)


(parametrise ((check-test-name 'filtering))

  (check
      (srfi.filter even? '())
    => '())

  (check
      (srfi.filter even? '(1))
    => '())

  (check
      (srfi.filter even? '(2))
    => '(2))

  (check
      (srfi.filter even? numbers)
    => '(0 2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (srfi.filter! even? '())
    => '())

  (check
      (srfi.filter! even? '(1))
    => '())

  (check
      (srfi.filter! even? '(2))
    => '(2))

  (check
      (srfi.filter! even? (srfi.list-copy numbers))
    => '(0 2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition even? '(1 3)))
	list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition even? '(2 4)))
	list)
    => '((2 4) ()))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition even? numbers))
	list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition! even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition! even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition! even? '(1 3)))
	list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition! even? '(2 4)))
	list)
    => '((2 4) ()))


  (check
      (call-with-values
	  (lambda ()
	    (srfi.partition! even? (srfi.list-copy numbers)))
	list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (srfi.remove even? '())
    => '())

  (check
      (srfi.remove even? '(1))
    => '(1))

  (check
      (srfi.remove even? '(2))
    => '())

  (check
      (srfi.remove even? numbers)
    => '(1 3 5 7 9))

;;; --------------------------------------------------------------------

  (check
      (srfi.remove! even? '())
    => '())

  (check
      (srfi.remove! even? '(1))
    => '(1))

  (check
      (srfi.remove! even? '(2))
    => '())

  (check
      (srfi.remove! even? (srfi.list-copy numbers))
    => '(1 3 5 7 9))

  #f)


(parametrise ((check-test-name 'finding))

  (check
      (srfi.find even? '())
    => #f)

  (check
      (srfi.find even? '(1))
    => #f)

  (check
      (srfi.find even? '(2))
    => 2)

  (check
      (srfi.find even? '(1 2 3))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (srfi.find-tail even? '())
    => #f)

  (check
      (srfi.find-tail even? '(1))
    => #f)

  (check
      (srfi.find-tail even? '(2))
    => '(2))

  (check
      (srfi.find-tail even? '(1 2 3))
    => '(2 3))

;;; --------------------------------------------------------------------

  (check
      (srfi.take-while even? '())
    => '())

  (check
      (srfi.take-while even? '(1))
    => '())

  (check
      (srfi.take-while even? '(2))
    => '(2))

  (check
      (srfi.take-while even? '(2 4 6 1 3))
    => '(2 4 6))

;;; --------------------------------------------------------------------

  (check
      (srfi.take-while! even? '())
    => '())

  (check
      (srfi.take-while! even? '(1))
    => '())

  (check
      (srfi.take-while! even? '(2))
    => '(2))

  (check
      (srfi.take-while! even? '(2 4 6 1 3))
    => '(2 4 6))

;;; --------------------------------------------------------------------

  (check
      (srfi.drop-while even? '())
    => '())

  (check
      (srfi.drop-while even? '(1))
    => '(1))

  (check
      (srfi.drop-while even? '(2))
    => '())

  (check
      (srfi.drop-while even? '(2 4 6 1 3))
    => '(1 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (srfi.span even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (srfi.span even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (srfi.span even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (srfi.span even? '(2 4 6 1 3)))
	list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (srfi.span! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (srfi.span! even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (srfi.span! even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (srfi.span! even? '(2 4 6 1 3)))
	list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (srfi.break even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (srfi.break even? '(1)))
	list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (srfi.break even? '(2)))
	list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (srfi.break even? '(1 3 2 4 6)))
	list)
    => '((1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (srfi.break! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (srfi.break! even? '(1)))
	list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (srfi.break! even? '(2)))
	list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (srfi.break! even? '(1 3 2 4 6)))
	list)
    => '((1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (srfi.any even? '())
    => #f)

  (check
      (srfi.any even? '(1))
    => #f)

  (check
      (and (srfi.any even? '(2))
	   #t)
    => #t)

  (check
      (and (srfi.any even? '(1 2))
	   #t)
    => #t)

  (check
      (and (srfi.any even? '(1 3 5 7 2))
	   #t)
    => #t)

  (check
      (srfi.any (lambda args
		  (integer? (apply + args)))
		'() '())
    => #f)

  (check
      (srfi.any (lambda args
		  (integer? (apply + args)))
		'() '() '())
    => #f)

;;; The following are  false because when a list  is empty the predicate
;;; is not applied at all and the return value is false.
  (check
      (and (srfi.any (lambda args
		       (integer? (apply + args)))
		     '() '() '())
	   #t)
    => #f)
  (check
      (and (srfi.any (lambda args
		       (integer? (apply + args)))
		     '() '() '())
	   #t)
    => #f)
  (check
      (and (srfi.any (lambda args
		       (integer? (apply + args)))
		     '() '() '())
	   #t)
    => #f)

  (check
      (and (srfi.any (lambda args
		       (integer? (apply + args)))
		     '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (srfi.any (lambda args
		       (integer? (apply + args)))
		     '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (srfi.any (lambda args
		       (integer? (apply + args)))
		     '(1 2)
		     '(2 2.2)
		     '(1.1 3))
	   #t)
    => #f)

  (check
      (and (srfi.any (lambda args
		       (integer? (apply + args)))
		     '(1 2)
		     '(2 2)
		     '(1.1 3))
	   #t)
    => #t)

  (check
      (guard (E (else #t))
	(srfi.any (lambda args
		    (integer? (apply + args)))
		  '(1 2)
		  '(2 2)
		  '(1.1 3)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.every even? '())
    => #t)

  (check
      (srfi.every even? '(1))
    => #f)

  (check
      (and (srfi.every even? '(2))
	   #t)
    => #t)

  (check
      (and (srfi.every even? '(1 2))
	   #t)
    => #f)

  (check
      (and (srfi.every even? '(4 8 10 12))
	   #t)
    => #t)

  (check
      (srfi.every (lambda args
		    (integer? (apply + args)))
		  '() '())
    => #t)

  (check
      (srfi.every (lambda args
		    (integer? (apply + args)))
		  '() '() '())
    => #t)

;;; The following are true because when a list is empty the predicate is
;;; not applied at all and the return value is true.
  (check
      (and (srfi.every (lambda args
			 (integer? (apply + args)))
		       '() '() '())
	   #t)
    => #t)
  (check
      (and (srfi.every (lambda args
			 (integer? (apply + args)))
		       '() '() '())
	   #t)
    => #t)
  (check
      (and (srfi.every (lambda args
			 (integer? (apply + args)))
		       '() '() '())
	   #t)
    => #t)

  (check
      (and (srfi.every (lambda args
			 (integer? (apply + args)))
		       '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (srfi.every (lambda args
			 (integer? (apply + args)))
		       '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (srfi.every (lambda args
			 (integer? (apply + args)))
		       '(1 2)
		       '(2 2.2)
		       '(1 3))
	   #t)
    => #f)

  (check
      (and (srfi.every (lambda args
			 (integer? (apply + args)))
		       '(1 2)
		       '(2 2)
		       '(1 3))
	   #t)
    => #t)

  (check
      (guard (E (else #t))
	(srfi.every (lambda args
		      (integer? (apply + args)))
		    '(1 2)
		    '(2 2 2)
		    '(1 3)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.list-index even? '())
    => #f)

  (check
      (srfi.list-index even? '() '())
    => #f)

  (check
      (srfi.list-index even? '() '() '())
    => #f)

  (check
      (srfi.list-index even? '(1))
    => #f)

  (check
      (srfi.list-index even? '(1 3 5))
    => #f)

  (check
      (srfi.list-index even? '(2))
    => 0)

  (check
      (srfi.list-index even? '(1 2 3 5))
    => 1)

  (check
      (srfi.list-index (lambda args
			 (integer? (apply + args)))
		       '(1 2 3)
		       '(1 2 3))
    => 0)

  (check
      (srfi.list-index (lambda args
			 (integer? (apply + args)))
		       '(1 2 3)
		       '(1.1 2 3))
    => 1)

  (check
      (srfi.list-index (lambda args
			 (integer? (apply + args)))
		       '(1 2 3)
		       '(1 2 3)
		       '(1 2 3))
    => 0)

  (check
      (srfi.list-index (lambda args
			 (integer? (apply + args)))
		       '(1 2 3)
		       '(1.1 2 3)
		       '(1 2 3))
    => 1)

  (check
      (srfi.list-index (lambda args
			 (integer? (apply + args)))
		       '(1 2 3)
		       '(1 2 3)
		       '(1.1 2.1 3))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (srfi.memq 'a '(a b c))
    => '(a b c))

  (check
      (srfi.memq 'b '(a b c))
    => '(b c))

  (check
      (srfi.memq 'a '(b c d))
    => #f)

  (check
      (srfi.memq (list 'a) '(b (a) c))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (srfi.member '(a)
		   '(b (a) c))
    => '((a) c))

;;; --------------------------------------------------------------------

  (check
      (srfi.memv 101 '(100 101 102))
    => '(101 102))

  #f)


(parametrise ((check-test-name 'deletion))

  (check
      (srfi.delete 8 '())
    => '())

  (check
      (srfi.delete 8 '(1))
    => '(1))

  (check
      (srfi.delete 8 '(8))
    => '())

  (check
      (srfi.delete 8 '(1 2 3))
    => '(1 2 3))

  (check
      (srfi.delete 8 '(1 2 8 3 4 5 8 6 7 8))
    => '(1 2 3 4 5 6 7))

  (check
      (srfi.delete 8 '() =)
    => '())

  (check
      (srfi.delete 8 '(1) =)
    => '(1))

  (check
      (srfi.delete 8 '(8) =)
    => '())

  (check
      (srfi.delete 8 '(1 2 3) =)
    => '(1 2 3))

  (check
      (srfi.delete 8 '(1 2 8 3 4 5 8 6 7 8) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (srfi.delete! 8 '())
    => '())

  (check
      (srfi.delete! 8 '(1))
    => '(1))

  (check
      (srfi.delete! 8 '(8))
    => '())

  (check
      (srfi.delete! 8 '(1 2 3))
    => '(1 2 3))

  (check
      (srfi.delete! 8 '(1 2 8 3 4 5 8 6 7 8))
    => '(1 2 3 4 5 6 7))


  (check
      (srfi.delete! 8 '() =)
    => '())

  (check
      (srfi.delete! 8 '(1) =)
    => '(1))

  (check
      (srfi.delete! 8 '(8) =)
    => '())

  (check
      (srfi.delete! 8 '(1 2 3) =)
    => '(1 2 3))

  (check
      (srfi.delete! 8 '(1 2 8 3 4 5 8 6 7 8) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (srfi.delete-duplicates '())
    => '())

  (check
      (srfi.delete-duplicates '(1))
    => '(1))

  (check
      (srfi.delete-duplicates '(1 2))
    => '(1 2))

  (check
      (srfi.delete-duplicates '(1 1))
    => '(1))

  (check
      (srfi.delete-duplicates '(1 1 1))
    => '(1))

  (check
      (srfi.delete-duplicates '(1 2 3 2 4 5 4 6 1 7))
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (srfi.delete-duplicates! '())
    => '())

  (check
      (srfi.delete-duplicates! '(1))
    => '(1))

  (check
      (srfi.delete-duplicates! '(1 2))
    => '(1 2))

  (check
      (srfi.delete-duplicates! '(1 1))
    => '(1))

  (check
      (srfi.delete-duplicates! '(1 1 1))
    => '(1))

  (check
      (srfi.delete-duplicates! '(1 2 3 2 4 5 4 6 1 7))
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (srfi.delete-duplicates '() =)
    => '())

  (check
      (srfi.delete-duplicates '(1) =)
    => '(1))

  (check
      (srfi.delete-duplicates '(1 2) =)
    => '(1 2))

  (check
      (srfi.delete-duplicates '(1 1) =)
    => '(1))

  (check
      (srfi.delete-duplicates '(1 1 1) =)
    => '(1))

  (check
      (srfi.delete-duplicates '(1 2 3 2 4 5 4 6 1 7) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (srfi.delete-duplicates! '() =)
    => '())

  (check
      (srfi.delete-duplicates! '(1) =)
    => '(1))

  (check
      (srfi.delete-duplicates! '(1 2) =)
    => '(1 2))

  (check
      (srfi.delete-duplicates! '(1 1) =)
    => '(1))

  (check
      (srfi.delete-duplicates! '(1 1 1) =)
    => '(1))

  (check
      (srfi.delete-duplicates! '(1 2 3 2 4 5 4 6 1 7) =)
    => '(1 2 3 4 5 6 7))

  #f)


(parametrise ((check-test-name 'alists))

  (check
      (srfi.assoc 'a
		  '((a . 1)
		    (b . 2)
		    (c . 3)))
    => '(a . 1))

  (check
      (srfi.assoc 'b
		  '((a . 1)
		    (b . 2)
		    (c . 3)))
    => '(b . 2))

  (check
      (srfi.assoc 'c
		  '((a . 1)
		    (b . 2)
		    (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (srfi.assq 'c
		 '())
    => #f)

  (check
      (srfi.assq 'd
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => #f)

  (check
      (srfi.assq 'a
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(a . 1))

  (check
      (srfi.assq 'b
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(b . 2))

  (check
      (srfi.assq 'c
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (srfi.assv 'c
		 '())
    => #f)

  (check
      (srfi.assv 'd
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => #f)

  (check
      (srfi.assv 'a
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(a . 1))

  (check
      (srfi.assv 'b
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(b . 2))

  (check
      (srfi.assv 'c
		 '((a . 1)
		   (b . 2)
		   (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (srfi.alist-cons 'a 1
		       '((b . 2)
			 (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (srfi.alist-cons 'a 1
		       '())
    => '((a . 1)))

  (check
      (srfi.alist-cons 'b 2
		       '((b . 2)
			 (c . 3)))
    => '((b . 2)
	 (b . 2)
	 (c . 3)))

;;; --------------------------------------------------------------------

  (check
      (srfi.alist-copy '((a . 1)
			 (b . 2)
			 (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (srfi.alist-copy '((a . 1)))
    => '((a . 1)))

  (check
      (srfi.alist-copy '())
    => '())

;;; --------------------------------------------------------------------

  (check
      (srfi.alist-delete 'a
			 '((a . 1)
			   (b . 2)
			   (c . 3)))
    => '((b . 2)
	 (c . 3)))

  (check
      (srfi.alist-delete 'b
			 '((a . 1)
			   (b . 2)
			   (c . 3)))
    => '((a . 1)
	 (c . 3)))

  (check
      (srfi.alist-delete 'c
			 '((a . 1)
			   (b . 2)
			   (c . 3)))
    => '((a . 1)
	 (b . 2)))

  (check
      (srfi.alist-delete 'd
			 '((a . 1)
			   (b . 2)
			   (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (srfi.alist-delete 'a
			 '((a . 1)
			   (a . 2)
			   (c . 3)))
    => '((c . 3)))

  (check
      (srfi.alist-delete 'a
			 '())
    => '())

  (check
      (srfi.alist-delete 'a
			 '((a . 1)))
    => '())

  (check
      (srfi.alist-delete 'a
			 '((a . 1)
			   (b . 2)
			   (c . 3))
			 eq?)
    => '((b . 2)
	 (c . 3)))

  (check
      (srfi.alist-delete 'b
			 '((a . 1)
			   (b . 2)
			   (c . 3))
			 eq?)
    => '((a . 1)
	 (c . 3)))

  (check
      (srfi.alist-delete 'c
			 '((a . 1)
			   (b . 2)
			   (c . 3))
			 eq?)
    => '((a . 1)
	 (b . 2)))

  (check
      (srfi.alist-delete 'd
			 '((a . 1)
			   (b . 2)
			   (c . 3))
			 eq?)
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (srfi.alist-delete 'a
			 '((a . 1)
			   (a . 2)
			   (c . 3))
			 eq?)
    => '((c . 3)))

  (check
      (srfi.alist-delete 'a
			 '()
			 eq?)
    => '())

  (check
      (srfi.alist-delete 'a
			 '((a . 1))
			 eq?)
    => '())

  #f)


(parametrise ((check-test-name 'sets))

  (check
      (srfi.lset<= =)
    => #t)

  (check
      (srfi.lset<= = '())
    => #t)

  (check
      (srfi.lset<= = '() '())
    => #t)

  (check
      (srfi.lset<= = '() '() '())
    => #t)

  (check
      (srfi.lset<= =
		   '(1)
		   '(1))
    => #t)

  (check
      (srfi.lset<= =
		   '(1)
		   '(1)
		   '(1))
    => #t)

  (check
      (srfi.lset<= =
		   '(1)
		   '(1 2)
		   '(1 2 3))
    => #t)

  (check
      (srfi.lset<= =
		   '(1)
		   '(1 2)
		   '(1 2))
    => #t)

  (check
      (srfi.lset<= =
		   '(1)
		   '(1 2)
		   '(1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (srfi.lset= =)
    => #t)

  (check
      (srfi.lset= = '())
    => #t)

  (check
      (srfi.lset= = '() '())
    => #t)

  (check
      (srfi.lset= = '() '() '())
    => #t)

  (check
      (srfi.lset= =
		  '(1)
		  '(1))
    => #t)

  (check
      (srfi.lset= =
		  '(1)
		  '(1)
		  '(1))
    => #t)

  (check
      (srfi.lset= =
		  '(1)
		  '(1 2)
		  '(1 2 3))
    => #f)

  (check
      (srfi.lset= =
		  '(1)
		  '(1 2)
		  '(1 2))
    => #f)

  (check
      (srfi.lset= =
		  '(1)
		  '(1 2)
		  '(1))
    => #f)

  #f)


;;;; done

(check-report)

;;; end of file
