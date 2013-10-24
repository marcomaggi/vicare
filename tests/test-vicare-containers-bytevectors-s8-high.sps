;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the bytevector s8 library
;;;Date: Tue Jul  5, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; setup

(import (except (vicare)
		subbytevector-u8
		subbytevector-s8
		s8-list->bytevector)
  (vicare language-extensions ascii-chars)
  (vicare containers bytevectors s8)
  (vicare containers char-sets)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: bytevectors s8, high level\n")


;;;; helpers

(define S string->utf8)
(define B utf8->string)

(define (number->bytevector-s8 num)
  (string->utf8 (number->string num)))


(parameterise ((check-test-name 'views))

  (check
      (subbytevector-s8* (S "ciao"))
    => (S "ciao"))

;;; --------------------------------------------------------------------

  (check
      (subbytevector-s8* (view (S "ciao")))
    => (S "ciao"))

  (check
      (subbytevector-s8* (view (S "ciao") (start 2)))
    => (S "ao"))

  (check
      (subbytevector-s8* (view (S "ciao") (start 0) (past 4)))
    => (S "ciao"))

  (check
      (subbytevector-s8* (view (S "ciao") (start 0) (past 0)))
    => '#vu8())

  (check
      (subbytevector-s8* (view (S "ciao") (start 1) (past 1)))
    => '#vu8())

  (check
      (subbytevector-s8* (view (S "ciao") (start 0) (past 1)))
    => (S "c"))

  (check
      (subbytevector-s8* (view (S "ciao") (past 2)))
    => (S "ci"))

  )


(parameterise ((check-test-name 'constructors))

  (check
      (bytevector-s8-append (S "0123"))
    => (S "0123"))

  (check
      (bytevector-s8-append (S "0123") (S "45678"))
    => (S "012345678"))

  (check
      (bytevector-s8-append '#vu8())
    => '#vu8())

  (check
      (bytevector-s8-append '#vu8() '#vu8())
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-tabulate (lambda (idx) (+ 65 idx)) 4)
    => (S "ABCD"))

  (check
      (bytevector-s8-tabulate (lambda (x) x) 0)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-concatenate `(,(S "ciao") ,(S " ") ,(S "hello") ,(S " ") ,(S "salut")))
    => (S "ciao hello salut"))

  (check
      (bytevector-s8-concatenate '())
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-concatenate-reverse `(,(S "ciao") ,(S " ") ,(S "hello") ,(S " ") ,(S "salut"))
					 (S " hola") 3)
    => (S "salut hello ciao ho"))

  (check
      (bytevector-s8-concatenate-reverse `(,(S "ciao") ,(S " ") ,(S "hello") ,(S " ") ,(S "salut"))
					 (S " hola"))
    => (S "salut hello ciao hola"))

  (check
      (bytevector-s8-concatenate-reverse `(,(S "ciao") ,(S " ") ,(S "hello") ,(S " ") ,(S "salut")))
    => (S "salut hello ciao"))

  (check
      (bytevector-s8-concatenate-reverse '())
    => '#vu8())

  #f)


(parameterise ((check-test-name 'predicates))

  (check
      (bytevector-s8-null? (S "ciao"))
    => #f)

  (check
      (bytevector-s8-null? '#vu8())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(bytevector-s8-every "not a criterion" (S "abc")))
    => '%bytevector-s8-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa")))
	(bytevector-s8-every #\a str))
    => #t)

  (check
      (let* ((str (S "aaaab")))
	(bytevector-s8-every #\a str))
    => #f)

  (check
      (let* ((str (S "aabaa")))
	(bytevector-s8-every #\a str))
    => #f)

  (check
      (let* ((str '#vu8()))
	(bytevector-s8-every #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa")))
	(bytevector-s8-every (char-set #\a) str))
    => #t)

  (check
      (let* ((str (S "aaaab")))
	(bytevector-s8-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str (S "aabaa")))
	(bytevector-s8-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str '#vu8()))
	(bytevector-s8-every (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa")))
	(bytevector-s8-every ascii-alphabetic? str))
    => #t)

  (check
      (let* ((str (S "aaaa2")))
	(bytevector-s8-every ascii-alphabetic? str))
    => #f)

  (check
      (let* ((str (S "aa2aa")))
	(bytevector-s8-every ascii-alphabetic? str))
    => #f)

  (check
      (let* ((str '#vu8()))
	(bytevector-s8-every ascii-alphabetic? str))
    => #f)

  (check
      (let* ((str (S "1234")))
	(bytevector-s8-every (lambda (x) x) str))
    => (char->integer #\4))

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(bytevector-s8-any "not a criterion" (S "abc")))
    => '%bytevector-s8-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "ddadd")))
	(bytevector-s8-any #\a str))
    => #t)

  (check
      (let* ((str (S "dddda")))
	(bytevector-s8-any #\a str))
    => #t)

  (check
      (let* ((str (S "ddd")))
	(bytevector-s8-any #\a str))
    => #f)

  (check
      (let* ((str '#vu8()))
	(bytevector-s8-any #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "dddaddd")))
	(bytevector-s8-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str (S "ddda")))
	(bytevector-s8-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str (S "dddd")))
	(bytevector-s8-any (char-set #\a) str))
    => #f)

  (check
      (let* ((str '#vu8()))
	(bytevector-s8-any (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "11a11")))
	(bytevector-s8-any ascii-alphabetic? str))
    => #t)

  (check
      (let* ((str (S "11111a")))
	(bytevector-s8-any ascii-alphabetic? str))
    => #t)

  (check
      (let* ((str (S "1111")))
	(bytevector-s8-any ascii-alphabetic? str))
    => #f)

  (check
      (let* ((str '#vu8()))
	(bytevector-s8-any ascii-alphabetic? str))
    => #f)

  (check
      (let* ((str (S "1234")))
	(bytevector-s8-any (lambda (x) x) str))
    => (char->integer #\1))

  #f)


(parameterise ((check-test-name 'comparison-case-sensitive))

  (check
      (bytevector-s8-compare (S "abcdefg") (S "abcd123") values values values)
    => 4)

  (check
      (bytevector-s8-compare (S "abcdef") (S "abcd123") values values values)
    => 4)

  (check
      (bytevector-s8-compare (S "efg") (S "123") values values values)
    => 0)

  (check
      (bytevector-s8-compare '#vu8() (S "abcd") values values values)
    => 0)

  (check
      (bytevector-s8-compare (S "abcd") '#vu8() values values values)
    => 0)

  (check
      (bytevector-s8-compare (S "abcdA") (S "abcdA")
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'equal)

  (check
      (bytevector-s8-compare (S "abcdA") (S "abcdB")
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'less)

  (check
      (bytevector-s8-compare (S "abcdB") (S "abcdA")
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str (S "abcd")))
     (bytevector-s8= str str)))

  (check-for-true
   (bytevector-s8= (view (S "12abcd") (start 2)) (S "abcd")))

  (check-for-false
   (bytevector-s8= (S "abc") (S "abcd")))

  (check-for-false
   (bytevector-s8= (S "abcd") (S "abc")))

  (check-for-false
   (bytevector-s8= (S "ABcd") (S "abcd")))

  (check-for-false
   (bytevector-s8= (S "abcd") (S "a2cd")))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-s8<> (S "abcd") (S "abcd")))

  (check-for-true
   (bytevector-s8<> (S "abc") (S "abcd")))

  (check-for-true
   (bytevector-s8<> (S "abcd") (S "abc")))

  (check-for-true
   (bytevector-s8<> (S "ABcd") (S "abcd")))

  (check-for-true
   (bytevector-s8<> (S "abcd") (S "a2cd")))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-s8< (S "abcd") (S "abcd")))

  (check-for-true
   (bytevector-s8< (S "abc") (S "abcd")))

  (check-for-false
   (bytevector-s8< (S "abcd") (S "abc")))

  (check-for-true
   (bytevector-s8< (S "ABcd") (S "abcd")))

  (check-for-false
   (bytevector-s8< (S "abcd") (S "a2cd")))

;;; --------------------------------------------------------------------

  (check-for-true
   (bytevector-s8<= (S "abcd") (S "abcd")))

  (check-for-true
   (bytevector-s8<= (S "abc") (S "abcd")))

  (check-for-false
   (bytevector-s8<= (S "abcd") (S "abc")))

  (check-for-true
   (bytevector-s8<= (S "ABcd") (S "abcd")))

  (check-for-false
   (bytevector-s8<= (S "abcd") (S "a2cd")))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-s8> (S "abcd") (S "abcd")))

  (check-for-true
   (bytevector-s8> (S "abcd") (S "abc")))

  (check-for-false
   (bytevector-s8> (S "abc") (S "abcd")))

  (check-for-true
   (bytevector-s8> (S "abcd") (S "ABcd")))

  (check-for-false
   (bytevector-s8> (S "a2cd") (S "abcd")))

;;; --------------------------------------------------------------------

  (check-for-true
   (bytevector-s8>= (S "abcd") (S "abcd")))

  (check-for-true
   (bytevector-s8>= (S "abcd") (S "abc")))

  (check-for-false
   (bytevector-s8>= (S "abc") (S "abcd")))

  (check-for-true
   (bytevector-s8>= (S "abcd") (S "ABcd")))

  (check-for-false
   (bytevector-s8>= (S "a2cd") (S "abcd")))

  #f)


(parameterise ((check-test-name 'mapping))

  (check
      (let ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-map! (lambda (i ch) (ascii-upcase ch))
			    str)
	str)
    => (S "ABCD"))

  (check
      (let ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-map! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
			    str (S "0123"))
	str)
    => (S "a1c3"))

  (check
      (let ((str (bytevector-copy '#vu8())))
	(bytevector-s8-map! (lambda (i ch) (ascii-upcase ch))
			    str)
	str)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-map*! (lambda (i ch) (ascii-upcase ch))
			     str)
	str)
    => (S "ABCD"))

  (check
      (let ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-map*! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
			     str (S "01234"))
	str)
    => (S "a1c3"))

  (check
      (let ((str (bytevector-copy '#vu8())))
	(bytevector-s8-map*! (lambda (i ch) (ascii-upcase ch))
			     str)
	str)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (bytevector-s8-for-each* (lambda (i ch) (add-result (list i ch)))
				      (S "abcd"))))
    => `((0 ,(char->integer #\a))
	 (1 ,(char->integer #\b))
	 (2 ,(char->integer #\c))
	 (3 ,(char->integer #\d))))

  (check
      (cadr (with-result
	     (bytevector-s8-for-each* (lambda (i ch-a ch-b) (add-result (list i ch-a ch-b)))
				      (S "abcd") (S "01234"))))
    => `((0 ,(char->integer #\a) ,(char->integer #\0))
	 (1 ,(char->integer #\b) ,(char->integer #\1))
	 (2 ,(char->integer #\c) ,(char->integer #\2))
	 (3 ,(char->integer #\d) ,(char->integer #\3))))

  (check
      (cadr (with-result
	     (bytevector-s8-for-each* (lambda (i ch) (add-result (list i ch)))
				      '#vu8())))
    => '())

;;; --------------------------------------------------------------------

  (check
      (subbytevector-s8-map (lambda (ch) (ascii-upcase ch))
			    (S "abcd"))
    => (S "ABCD"))

  (check
      (subbytevector-s8-map (lambda (ch) (ascii-upcase ch))
			    (view (S "abcd") (start 1) (past 3)))
    => (S "BC"))

  (check
      (subbytevector-s8-map (lambda (ch) (ascii-upcase ch))
			    '#vu8())
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let ((str (bytevector-copy (S "abcd"))))
	(subbytevector-s8-map! (lambda (ch) (ascii-upcase ch))
			       str)
	str)
    => (S "ABCD"))

  (check
      (let ((str (bytevector-copy (S "abcd"))))
	(subbytevector-s8-map! (lambda (ch) (ascii-upcase ch))
			       (view str (start 1) (past 3)))
	str)
    => (S "aBCd"))

  (check
      (let ((str '#vu8()))
	(subbytevector-s8-map! (lambda (ch) (ascii-upcase ch))
			       str)
	str)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (subbytevector-s8-for-each add-result
					(S "abcd"))))
    => (map char->integer '(#\a #\b #\c #\d)))

  (check
      (cadr (with-result
	     (subbytevector-s8-for-each add-result
					(view (S "abcd") (start 1) (past 3)))))
    => (map char->integer '(#\b #\c)))

  (check
      (cadr (with-result
	     (subbytevector-s8-for-each add-result '#vu8())))
    => '())

  #f)


(parameterise ((check-test-name 'case))

  (check
      (bytevector-s8-upcase* (S "abcd"))
    => (S "ABCD"))

  (check
      (bytevector-s8-upcase* (S "123abcd"))
    => (S "123ABCD"))

  (check
      (bytevector-s8-upcase* (S "---abcd"))
    => (S "---ABCD"))

  (check
      (bytevector-s8-upcase* (S "abcd efgh"))
    => (S "ABCD EFGH"))

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-upcase*! str)
	str)
    => (S "ABCD"))

  (check
      (let* ((str (bytevector-copy (S "123abcd"))))
	(bytevector-s8-upcase*! str)
	str)
    => (S "123ABCD"))

  (check
      (let* ((str (bytevector-copy (S "---abcd"))))
	(bytevector-s8-upcase*! str)
	str)
    => (S "---ABCD"))

  (check
      (let* ((str (bytevector-copy (S "abcd efgh"))))
	(bytevector-s8-upcase*! str)
	str)
    => (S "ABCD EFGH"))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-downcase* (S "ABCD"))
    => (S "abcd"))

  (check
      (bytevector-s8-downcase* (S "123AbcD"))
    => (S "123abcd"))

  (check
      (bytevector-s8-downcase* (S "---aBCd"))
    => (S "---abcd"))

  (check
      (bytevector-s8-downcase* (S "abcd EFGH"))
    => (S "abcd efgh"))

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-copy (S "aBCd"))))
	(bytevector-s8-downcase*! str)
	str)
    => (S "abcd"))

  (check
      (let* ((str (bytevector-copy (S "123ABcd"))))
	(bytevector-s8-downcase*! str)
	str)
    => (S "123abcd"))

  (check
      (let* ((str (bytevector-copy (S "---aBCD"))))
	(bytevector-s8-downcase*! str)
	str)
    => (S "---abcd"))

  (check
      (let* ((str (bytevector-copy (S "abCD Efgh"))))
	(bytevector-s8-downcase*! str)
	str)
    => (S "abcd efgh"))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-titlecase* (S "abcd"))
    => (S "Abcd"))

  (check
      (bytevector-s8-titlecase* (S "123abcd"))
    => (S "123Abcd"))

  (check
      (bytevector-s8-titlecase* (S "---abcd"))
    => (S "---Abcd"))

  (check
      (bytevector-s8-titlecase* (S "abcd efgh"))
    => (S "Abcd Efgh"))

  (check
      (bytevector-s8-titlecase* (view (S "greasy fried chicken") (start 2)))
    => (S "Easy Fried Chicken"))

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-titlecase*! str)
	str)
    => (S "Abcd"))

  (check
      (let* ((str (bytevector-copy (S "123abcd"))))
	(bytevector-s8-titlecase*! str)
	str)
    => (S "123Abcd"))

  (check
      (let* ((str (bytevector-copy (S "---abcd"))))
	(bytevector-s8-titlecase*! str)
	str)
    => (S "---Abcd"))

  (check
      (let* ((str (bytevector-copy (S "abcd efgh"))))
	(bytevector-s8-titlecase*! str)
	str)
    => (S "Abcd Efgh"))

  (check
      (let ((str (bytevector-copy (S "greasy fried chicken"))))
	(bytevector-s8-titlecase*! (view str (start 2)))
	str)
    => (S "grEasy Fried Chicken"))

  )


(parameterise ((check-test-name 'folding))

  (check
      (bytevector-s8-fold-left (lambda (i nil x) (cons x nil)) '() (S "abcd"))
    => (map char->integer '(#\d #\c #\b #\a)))

  (check
      (bytevector-s8-fold-left (lambda (i nil x y) (cons (cons x y) nil)) '()
			       (S "abcd")
			       (S "ABCD"))
    => `((,(char->integer #\d) . ,(char->integer #\D))
	 (,(char->integer #\c) . ,(char->integer #\C))
	 (,(char->integer #\b) . ,(char->integer #\B))
	 (,(char->integer #\a) . ,(char->integer #\A))))

  (check
      (bytevector-s8-fold-left (lambda (i nil x) (cons x nil)) '() '#vu8())
    => '())

  (check
      (bytevector-s8-fold-left (lambda (i count c)
			  (if (ascii-upper-case? c)
			      (+ count 1)
			    count))
			0
			(S "ABCdefGHi"))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-fold-right (lambda (i nil x) (cons x nil)) '() (S "abcd"))
    => (map char->integer '(#\a #\b #\c #\d)))

  (check
      (bytevector-s8-fold-right (lambda (i nil x y) (cons (cons x y) nil)) '()
				(S "abcd")
				(S "ABCD"))
    => `((,(char->integer #\a) . ,(char->integer #\A))
	 (,(char->integer #\b) . ,(char->integer #\B))
	 (,(char->integer #\c) . ,(char->integer #\C))
	 (,(char->integer #\d) . ,(char->integer #\D))))

  (check
      (bytevector-s8-fold-right (lambda (i nil x) (cons x nil)) '() '#vu8())
    => '())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-fold-left* (lambda (i nil x) (cons x nil)) '() (S "abcd"))
    => (map char->integer '(#\d #\c #\b #\a)))

  (check
      (bytevector-s8-fold-left* (lambda (i nil x y) (cons (cons x y) nil)) '()
				(S "abcd")
				(S "ABCDE"))
    => `((,(char->integer #\d) . ,(char->integer #\D))
	 (,(char->integer #\c) . ,(char->integer #\C))
	 (,(char->integer #\b) . ,(char->integer #\B))
	 (,(char->integer #\a) . ,(char->integer #\A))))

  (check
      (bytevector-s8-fold-left* (lambda (i nil x) (cons x nil)) '() '#vu8())
    => '())

  (check
      (bytevector-s8-fold-left* (lambda (i count c)
			   (if (ascii-upper-case? c)
			       (+ count 1)
			     count))
			 0
			 (S "ABCdefGHi"))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-fold-right* (lambda (i nil x) (cons x nil)) '() (S "abcd"))
    => (map char->integer '(#\a #\b #\c #\d)))

  (check
      (bytevector-s8-fold-right* (lambda (i nil x y) (cons (cons x y) nil)) '()
				 (S "abcd")
				 (S "ABCDE"))
    => `((,(char->integer #\a) . ,(char->integer #\A))
	 (,(char->integer #\b) . ,(char->integer #\B))
	 (,(char->integer #\c) . ,(char->integer #\C))
	 (,(char->integer #\d) . ,(char->integer #\D))))

  (check
      (bytevector-s8-fold-right* (lambda (i nil x) (cons x nil)) '() '#vu8())
    => '())

;;; --------------------------------------------------------------------

  (check
      (subbytevector-s8-fold-left cons '() (S "abcd"))
    => (map char->integer '(#\d #\c #\b #\a)))

  (check
      (subbytevector-s8-fold-left cons '() '#vu8())
    => '())

  (check
      (subbytevector-s8-fold-left (lambda (c count)
				    (if (ascii-upper-case? c)
					(+ count 1)
				      count))
				  0
				  (S "ABCdefGHi"))
    => 5)

  (check
      (let* ((str (S "abc\\de\\f\\ghi"))
	     (ans-len (subbytevector-s8-fold-left
		       (lambda (c sum)
			 (+ sum (if (char=? (integer->char c) #\\) 2 1)))
		       0 str))
	     (ans (make-bytevector ans-len)))
	(subbytevector-s8-fold-left
	 (lambda (c i)
	   (let ((i (if (char=? (integer->char c) #\\)
			(begin
			  (bytevector-s8-set! ans i (char->integer #\\))
			  (+ i 1))
		      i)))
	     (bytevector-s8-set! ans i c)
	     (+ i 1)))
	 0 str)
	ans)
    => (S "abc\\\\de\\\\f\\\\ghi"))

;;; --------------------------------------------------------------------

  (check
      (subbytevector-s8-fold-right cons '() (S "abcd"))
    => (map char->integer '(#\a #\b #\c #\d)))

  (check
      (subbytevector-s8-fold-right cons '() '#vu8())
    => '())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-unfold null? car cdr (map char->integer '(#\a #\b #\c #\d)))
    => (S "abcd"))

  (check
      (bytevector-s8-unfold null? car cdr '())
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-unfold-right null? car cdr (map char->integer '(#\a #\b #\c #\d)))
    => (S "dcba"))

  (check
      (bytevector-s8-unfold-right null? car cdr '())
    => '#vu8())

  #f)


(parameterise ((check-test-name 'selecting))

  (check
      (bytevector-s8-take (S "abcd") 2)
    => (S "ab"))

  (check
      (bytevector-s8-take '#vu8() 0)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-s8-take (S "abcd") 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-take-right (S "abcd") 2)
    => (S "cd"))

  (check
      (bytevector-s8-take-right '#vu8() 0)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-s8-take-right (S "abcd") 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-drop (S "abcd") 2)
    => (S "cd"))

  (check
      (bytevector-s8-drop '#vu8() 0)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-s8-drop (S "abcd") 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-drop-right (S "abcd") 2)
    => (S "ab"))

  (check
      (bytevector-s8-drop-right '#vu8() 0)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-s8-drop-right (S "abcd") 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-trim (S "aaabcd") #\a)
    => (S "bcd"))

  (check
      (bytevector-s8-trim (S "bcd") #\a)
    => (S "bcd"))

  (check
      (bytevector-s8-trim '#vu8() #\a)
    => '#vu8())

  (check
      (bytevector-s8-trim (S "aaabcd") (char-set #\a #\b))
    => (S "cd"))

  (check
      (bytevector-s8-trim (S "bcd") (char-set #\a #\b))
    => (S "cd"))

  (check
      (bytevector-s8-trim '#vu8() (char-set #\a #\b))
    => '#vu8())

  (check
      (bytevector-s8-trim (S "AAAbcd") ascii-upper-case?)
    => (S "bcd"))

  (check
      (bytevector-s8-trim (S "bcd") ascii-upper-case?)
    => (S "bcd"))

  (check
      (bytevector-s8-trim '#vu8() ascii-upper-case?)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-trim-right (S "bcdaaa") #\a)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-right (S "bcd") #\a)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-right '#vu8() #\a)
    => '#vu8())

  (check
      (bytevector-s8-trim-right (S "cdbaaa") (char-set #\a #\b))
    => (S "cd"))

  (check
      (bytevector-s8-trim-right (S "cdb") (char-set #\a #\b))
    => (S "cd"))

  (check
      (bytevector-s8-trim-right '#vu8() (char-set #\a #\b))
    => '#vu8())

  (check
      (bytevector-s8-trim-right (S "bcdAAA") ascii-upper-case?)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-right (S "bcd") ascii-upper-case?)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-right '#vu8() ascii-upper-case?)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-trim-both (S "aaabcdaaa") #\a)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-both (S "bcd") #\a)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-both '#vu8() #\a)
    => '#vu8())

  (check
      (bytevector-s8-trim-both (S "aaabcdaa") (char-set #\a #\b))
    => (S "cd"))

  (check
      (bytevector-s8-trim-both (S "bcdb") (char-set #\a #\b))
    => (S "cd"))

  (check
      (bytevector-s8-trim-both '#vu8() (char-set #\a #\b))
    => '#vu8())

  (check
      (bytevector-s8-trim-both (S "AAAbcdAAA") ascii-upper-case?)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-both (S "bcd") ascii-upper-case?)
    => (S "bcd"))

  (check
      (bytevector-s8-trim-both '#vu8() ascii-upper-case?)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-pad (S "abc") 3 #\0)
    => (S "abc"))

  (check
      (bytevector-s8-pad (S "abc") 5 #\0)
    => (S "00abc"))

  (check
      (bytevector-s8-pad (S "abc") 5)
    => (S "  abc"))

  (check
      (bytevector-s8-pad (S "abc") 2 #\0)
    => (S "bc"))

  (check
      (bytevector-s8-pad (S "abc") 0 #\0)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-pad-right (S "abc") 3 #\0)
    => (S "abc"))

  (check
      (bytevector-s8-pad-right (S "abc") 5 #\0)
    => (S "abc00"))

  (check
      (bytevector-s8-pad-right (S "abc") 2 #\0)
    => (S "ab"))

  (check
      (bytevector-s8-pad-right (S "abc") 0 #\0)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'prefix))

  (check
      (bytevector-s8-prefix-length (S "abcdefg") (S "abcd123"))
    => 4)

  (check
      (bytevector-s8-prefix-length (S "aBcdefg") (S "abcd123"))
    => 1)

  (check
      (bytevector-s8-prefix-length (S "efg") (S "123"))
    => 0)

  (check
      (bytevector-s8-prefix-length (S "a") (S "a"))
    => 1)

  (check
      (bytevector-s8-prefix-length (S "1") (S "2"))
    => 0)

  (check
      (bytevector-s8-prefix-length '#vu8() (S "abcd123"))
    => 0)

  (check
      (bytevector-s8-prefix-length (S "abcdefg") '#vu8())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-suffix-length (S "efgabcd") (S "123abcd"))
    => 4)

  (check
      (bytevector-s8-suffix-length (S "efgabcd") (S "123abCd"))
    => 1)

  (check
      (bytevector-s8-suffix-length (S "efg") (S "123"))
    => 0)

  (check
      (bytevector-s8-suffix-length (S "a") (S "a"))
    => 1)

  (check
      (bytevector-s8-suffix-length (S "1") (S "2"))
    => 0)

  (check
      (bytevector-s8-suffix-length '#vu8() (S "abcd123"))
    => 0)

  (check
      (bytevector-s8-suffix-length (S "abcdefg") '#vu8())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-prefix-length-ci (S "aBcdefg") (S "aBcd123"))
    => 4)

  (check
      (bytevector-s8-prefix-length-ci (S "aBcdefg") (S "abcd123"))
    => 4)

  (check
      (bytevector-s8-prefix-length-ci (S "efg") (S "123"))
    => 0)

  (check
      (bytevector-s8-prefix-length-ci (S "a") (S "a"))
    => 1)

  (check
      (bytevector-s8-prefix-length-ci (S "1") (S "2"))
    => 0)

  (check
      (bytevector-s8-prefix-length-ci '#vu8() (S "abcd123"))
    => 0)

  (check
      (bytevector-s8-prefix-length-ci (S "abcdefg") '#vu8())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-suffix-length-ci (S "efgabCd") (S "123abCd"))
    => 4)

  (check
      (bytevector-s8-suffix-length-ci (S "efgabCd") (S "123abcd"))
    => 4)

  (check
      (bytevector-s8-suffix-length-ci (S "efg") (S "123"))
    => 0)

  (check
      (bytevector-s8-suffix-length-ci (S "a") (S "a"))
    => 1)

  (check
      (bytevector-s8-suffix-length-ci (S "1") (S "2"))
    => 0)

  (check
      (bytevector-s8-suffix-length-ci '#vu8() (S "abcd123"))
    => 0)

  (check
      (bytevector-s8-suffix-length-ci (S "abcdefg") '#vu8())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-prefix? (S "abcd") (S "abcd123"))
    => #t)

  (check
      (bytevector-s8-prefix? (S "abcd") (S "aBcd123"))
    => #f)

  (check
      (bytevector-s8-prefix? (S "efg") (S "123"))
    => #f)

  (check
      (bytevector-s8-prefix? '#vu8() (S "123"))
    => #t)

  (check
      (bytevector-s8-prefix? (S "efg") '#vu8())
    => #f)

  (check
      (bytevector-s8-prefix? '#vu8() '#vu8())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-prefix-ci? (S "aBcd") (S "aBcd123"))
    => #t)

  (check
      (bytevector-s8-prefix-ci? (S "abcd") (S "aBcd123"))
    => #t)

  (check
      (bytevector-s8-prefix-ci? (S "efg") (S "123"))
    => #f)

  (check
      (bytevector-s8-prefix-ci? '#vu8() (S "123"))
    => #t)

  (check
      (bytevector-s8-prefix-ci? (S "efg") '#vu8())
    => #f)

  (check
      (bytevector-s8-prefix-ci? '#vu8() '#vu8())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-suffix? (S "abcd") (S "123abcd"))
    => #t)

  (check
      (bytevector-s8-suffix? (S "abcd") (S "123aBcd"))
    => #f)

  (check
      (bytevector-s8-suffix? (S "efg") (S "123"))
    => #f)

  (check
      (bytevector-s8-suffix? '#vu8() (S "123"))
    => #t)

  (check
      (bytevector-s8-suffix? (S "efg") '#vu8())
    => #f)

  (check
      (bytevector-s8-suffix? '#vu8() '#vu8())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-suffix-ci? (S "aBcd") (S "123aBcd"))
    => #t)

  (check
      (bytevector-s8-suffix-ci? (S "abcd") (S "123aBcd"))
    => #t)

  (check
      (bytevector-s8-suffix-ci? (S "efg") (S "123"))
    => #f)

  (check
      (bytevector-s8-suffix-ci? '#vu8() (S "123"))
    => #t)

  (check
      (bytevector-s8-suffix-ci? (S "efg") '#vu8())
    => #f)

  (check
      (bytevector-s8-suffix-ci? '#vu8() '#vu8())
    => #t)

  #f)


(parameterise ((check-test-name 'searching))

  (check
      (bytevector-s8-index (S "abcd") #\b)
    => 1)

  (check
      (bytevector-s8-index (view (S "abcd") (start 1)) #\b)
    => 1)

  (check
      (bytevector-s8-index (S "abcd") #\1)
    => #f)

  (check
      (bytevector-s8-index '#vu8() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-index (S "abcd") (char-set #\b #\B))
    => 1)

  (check
      (bytevector-s8-index (view (S "abcd") (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-s8-index (S "abcd") (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-s8-index '#vu8() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-index (S "aBcd") ascii-upper-case?)
    => 1)

  (check
      (bytevector-s8-index (view (S "aBcd") (start 1)) ascii-upper-case?)
    => 1)

  (check
      (bytevector-s8-index (S "abcd") ascii-upper-case?)
    => #f)

  (check
      (bytevector-s8-index '#vu8() ascii-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-index-right (S "abcd") #\b)
    => 1)

  (check
      (bytevector-s8-index-right (view (S "abcd") (start 1)) #\b)
    => 1)

  (check
      (bytevector-s8-index-right (S "abcd") #\1)
    => #f)

  (check
      (bytevector-s8-index-right '#vu8() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-index-right (S "abcd") (char-set #\b #\B))
    => 1)

  (check
      (bytevector-s8-index-right (view (S "abcd") (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-s8-index-right (S "abcd") (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-s8-index-right '#vu8() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-index-right (S "aBcd") ascii-upper-case?)
    => 1)

  (check
      (bytevector-s8-index-right (view (S "aBcd") (start 1)) ascii-upper-case?)
    => 1)

  (check
      (bytevector-s8-index-right (S "abcd") ascii-upper-case?)
    => #f)

  (check
      (bytevector-s8-index-right '#vu8() ascii-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-skip (S "bacd") #\b)
    => 1)

  (check
      (bytevector-s8-skip (view (S "bacd") (start 1)) #\b)
    => 1)

  (check
      (bytevector-s8-skip (S "1111") #\1)
    => #f)

  (check
      (bytevector-s8-skip '#vu8() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-skip (S "bacd") (char-set #\b #\B))
    => 1)

  (check
      (bytevector-s8-skip (view (S "bacd") (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-s8-skip (S "1010") (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-s8-skip '#vu8() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-skip (S "Bacd") ascii-upper-case?)
    => 1)

  (check
      (bytevector-s8-skip (view (S "Bacd") (start 1)) ascii-upper-case?)
    => 1)

  (check
      (bytevector-s8-skip (S "ABCD") ascii-upper-case?)
    => #f)

  (check
      (bytevector-s8-skip '#vu8() ascii-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-skip-right (S "acdb") #\b)
    => 2)

  (check
      (bytevector-s8-skip-right (view (S "acdb") (start 1)) #\b)
    => 2)

  (check
      (bytevector-s8-skip-right (S "1111") #\1)
    => #f)

  (check
      (bytevector-s8-skip-right '#vu8() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-skip-right (S "acdb") (char-set #\b #\B))
    => 2)

  (check
      (bytevector-s8-skip-right (view (S "acdb") (start 1)) (char-set #\b #\B))
    => 2)

  (check
      (bytevector-s8-skip-right (S "0101") (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-s8-skip-right '#vu8() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-skip-right (S "acdB") ascii-upper-case?)
    => 2)

  (check
      (bytevector-s8-skip-right (view (S "acdB") (start 1)) ascii-upper-case?)
    => 2)

  (check
      (bytevector-s8-skip-right (S "ABCD") ascii-upper-case?)
    => #f)

  (check
      (bytevector-s8-skip-right '#vu8() ascii-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-count (S "abcbd") #\b)
    => 2)

  (check
      (bytevector-s8-count (view (S "abcd") (start 1)) #\b)
    => 1)

  (check
      (bytevector-s8-count (S "abcd") #\1)
    => 0)

  (check
      (bytevector-s8-count '#vu8() #\1)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-count (S "abcBd") (char-set #\b #\B))
    => 2)

  (check
      (bytevector-s8-count (view (S "abcd") (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-s8-count (S "abcd") (char-set #\0 #\1))
    => 0)

  (check
      (bytevector-s8-count '#vu8() (char-set #\0 #\1))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-count (S "aBcAd") ascii-upper-case?)
    => 2)

  (check
      (bytevector-s8-count (view (S "aBcd") (start 1)) ascii-upper-case?)
    => 1)

  (check
      (bytevector-s8-count (S "abcd") ascii-upper-case?)
    => 0)

  (check
      (bytevector-s8-count '#vu8() ascii-upper-case?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-contains (S "ciao hello salut") (S "hello"))
    => 5)

  (check
      (bytevector-s8-contains (S "ciao hello salut") (S "hola"))
    => #f)

  (check
      (bytevector-s8-contains (S "ciao hello salut") '#vu8())
    => 0)

  (check
      (bytevector-s8-contains '#vu8() (S "hello"))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-contains-ci (S "ciAO HELLO saLUT") (S "hello"))
    => 5)

  (check
      (bytevector-s8-contains-ci (S "ciao hello salut") (S "HOLA"))
    => #f)

  (check
      (bytevector-s8-contains-ci (S "ciao hello salut") '#vu8())
    => 0)

  (check
      (bytevector-s8-contains-ci '#vu8() (S "hello"))
    => #f)

  #f)


(parameterise ((check-test-name 'filtering))

  (check
      (bytevector-s8-delete (S "abcbd") #\b)
    => (S "acd"))

  (check
      (bytevector-s8-delete (S "abcbd") #\0)
    => (S "abcbd"))

  (check
      (bytevector-s8-delete '#vu8() #\b)
    => '#vu8())

  (check
      (bytevector-s8-delete (S "bbb") #\b)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-delete (S "abcbd") (char-set #\b #\B))
    => (S "acd"))

  (check
      (bytevector-s8-delete (S "abcbd") (char-set #\0 #\1))
    => (S "abcbd"))

  (check
      (bytevector-s8-delete '#vu8() (char-set #\b #\B))
    => '#vu8())

  (check
      (bytevector-s8-delete (S "BbB") (char-set #\b #\B))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-delete (S "aBcBd") ascii-upper-case?)
    => (S "acd"))

  (check
      (bytevector-s8-delete (S "abcbd") ascii-upper-case?)
    => (S "abcbd"))

  (check
      (bytevector-s8-delete '#vu8() ascii-upper-case?)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-filter (S "abcbd") #\b)
    => (S "bb"))

  (check
      (bytevector-s8-filter (S "abcbd") #\0)
    => '#vu8())

  (check
      (bytevector-s8-filter '#vu8() #\b)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-filter (S "abcbd") (char-set #\b #\B))
    => (S "bb"))

  (check
      (bytevector-s8-filter (S "abcbd") (char-set #\0 #\1))
    => '#vu8())

  (check
      (bytevector-s8-filter '#vu8() (char-set #\b #\B))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-filter (S "aBcBd") ascii-upper-case?)
    => (S "BB"))

  (check
      (bytevector-s8-filter (S "abcbd") ascii-upper-case?)
    => '#vu8())

  (check
      (bytevector-s8-filter '#vu8() ascii-upper-case?)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'lists))

  (check
      (bytevector->s8-list* (S "abcd"))
    => (map char->integer '(#\a #\b #\c #\d)))

  (check
      (bytevector->s8-list* (view (S "abcd") (start 1) (past 3)))
    => (map char->integer '(#\b #\c)))

  (check
      (bytevector->s8-list* '#vu8())
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-s8-list->bytevector (map char->integer '(#\a #\b #\c #\d)))
    => (S "dcba"))

  (check
      (reverse-s8-list->bytevector '())
    => '#vu8())

  #f)

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'tokenize))

  (check
      (bytevector-s8-tokenize (S "ciao hello salut")
			      (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => `(,(S "ciao") ,(S "hello") ,(S "salut")))

  (check
      (bytevector-s8-tokenize '#vu8() (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '())

  (check
      (bytevector-s8-tokenize (S "ciao hello salut") (char-set))
    => '())

  (check
      (bytevector-s8-tokenize (S "Help make programs run, run, RUN!")
			      (char-set-complement (char-set #\space)
						   char-set:ascii))
    => `(,(S "Help") ,(S "make") ,(S "programs") ,(S "run,") ,(S "run,") ,(S "RUN!")))

  #f)

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'infix)
    => (S "c,i,a,o"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'strict-infix)
    => (S "c,i,a,o"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'suffix)
    => (S "c,i,a,o,"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'prefix)
    => (S ",c,i,a,o"))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-join '() (S ",") 'infix)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(bytevector-s8-join '() (S ",") 'strict-infix))
    => #t)

  (check
      (bytevector-s8-join '() (S ",") 'suffix)
    => '#vu8())

  (check
      (bytevector-s8-join '() (S ",") 'prefix)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-join `(,(S "c")) (S ",") 'infix)
    => (S "c"))

  (check
      (bytevector-s8-join `(,(S "c")) (S ",") 'strict-infix)
    => (S "c"))

  (check
      (bytevector-s8-join `(,(S "c")) (S ",") 'suffix)
    => (S "c,"))

  (check
      (bytevector-s8-join `(,(S "c")) (S ",") 'prefix)
    => (S ",c"))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-join '(#vu8()) (S ",") 'infix)
    => '#vu8())

  (check
      (bytevector-s8-join '(#vu8()) (S ",") 'strict-infix)
    => '#vu8())

  (check
      (bytevector-s8-join '(#vu8()) (S ",") 'suffix)
    => (S ","))

  (check
      (bytevector-s8-join '(#vu8()) (S ",") 'prefix)
    => (S ","))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'infix)
    => (S "ciao"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'strict-infix)
    => (S "ciao"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'suffix)
    => (S "ciao"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'prefix)
    => (S "ciao"))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'infix)
    => (S "c,;;i,;;a,;;o"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'strict-infix)
    => (S "c,;;i,;;a,;;o"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'suffix)
    => (S "c,;;i,;;a,;;o,;;"))

  (check
      (bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'prefix)
    => (S ",;;c,;;i,;;a,;;o"))

  #f)


(parameterise ((check-test-name 'xsubbytevector-s8))

  (check
      (xsubbytevector-s8 (S "ciao ") 0 5)
    => (S "ciao "))

  (check
      (xsubbytevector-s8 (S "ciao ") 0 9)
    => (S "ciao ciao"))

  (check
      (xsubbytevector-s8 (S "ciao ") -5 5)
    => (S "ciao ciao "))

  (check
      (xsubbytevector-s8 (S "ciao ") 2 4)
    => (S "ao"))

  (check
      (xsubbytevector-s8 (S "ciao ") -3 7)
    => (S "ao ciao ci"))

  (check (xsubbytevector-s8 (S "abcdef") 1 7) => (S "bcdefa"))
  (check (xsubbytevector-s8 (S "abcdef") 2 8) => (S "cdefab"))
  (check (xsubbytevector-s8 (S "abcdef") 3 9) => (S "defabc"))
  (check (xsubbytevector-s8 (S "abcdef") 4 10) => (S "efabcd"))
  (check (xsubbytevector-s8 (S "abcdef") 5 11) => (S "fabcde"))

  (check (xsubbytevector-s8 (S "abcdef") -1 5) => (S "fabcde"))
  (check (xsubbytevector-s8 (S "abcdef") -2 4) => (S "efabcd"))
  (check (xsubbytevector-s8 (S "abcdef") -3 3) => (S "defabc"))
  (check (xsubbytevector-s8 (S "abcdef") -4 2) => (S "cdefab"))
  (check (xsubbytevector-s8 (S "abcdef") -5 1) => (S "bcdefa"))

  (check
      (xsubbytevector-s8 (S "ciao ") 3 3)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(xsubbytevector-s8 '#vu8() 0 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((result (bytevector-copy (S "01234"))))
	(bytevector-s8-xcopy! result (S "ciao ") 0 5)
	result)
    => (S "ciao "))

  (check
      (let ((result (bytevector-copy (S "012345678"))))
	(bytevector-s8-xcopy! result (S "ciao ") 0 9)
	result)
    => (S "ciao ciao"))

  (check
      (let ((result (bytevector-copy (S "0123456789"))))
	(bytevector-s8-xcopy! result (S "ciao ") -5 5)
	result)
    => (S "ciao ciao "))

  (check
      (let ((result (bytevector-copy (S "01"))))
	(bytevector-s8-xcopy! result (S "ciao ") 2 4)
	result)
    => (S "ao"))

  (check
      (let ((result (bytevector-copy (S "0123456789"))))
	(bytevector-s8-xcopy! result (S "ciao ") -3 7)
	result)
    => (S "ao ciao ci"))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	  (bytevector-s8-xcopy! '#vu8() '#vu8() 0 5))
    => #t)

  #f)


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-fill*! str #\b)
	str)
    => (S "bbbb"))

  (check
      (let* ((str (bytevector-copy (S "accd"))))
	(bytevector-s8-fill*! (view str (start 1) (past 3)) #\b)
	str)
    => (S "abbd"))

  (check
      (let* ((str (bytevector-copy '#vu8())))
	(bytevector-s8-fill*! (view str (start 0) (past 0)) #\b)
	str)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'reverse))

  (check
      (bytevector-s8-reverse (S "abcd"))
    => (S "dcba"))

  (check
      (bytevector-s8-reverse '#vu8())
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-copy (S "abcd"))))
	(bytevector-s8-reverse! str)
	str)
    => (S "dcba"))

  (check
      (let* ((str (bytevector-copy '#vu8())))
	(bytevector-s8-reverse! str)
	str)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'replace))

  (check
      (bytevector-s8-replace (S "abcd") (S "1234"))
    => (S "1234"))

  (check
      (bytevector-s8-replace (view (S "abcd") (start 2) (past 2)) (S "1234"))
    => (S "ab1234cd"))

  (check
      (bytevector-s8-replace (view (S "abcd") (start 2) (past 2)) '#vu8())
    => (S "abcd"))

  (check
      (bytevector-s8-replace (view (S "abcd") (start 1) (past 3)) (S "1234"))
    => (S "a1234d"))

  (check
      (bytevector-s8-replace (view (S "abcd") (start 0) (past 3)) (S "1234"))
    => (S "1234d"))

  (check
      (bytevector-s8-replace (view (S "abcd") (start 1) (past 4)) (S "1234"))
    => (S "a1234"))

  #f)


(parameterise ((check-test-name 'mutating))

  (check
      (let* ((str (bytevector-copy (S "12"))))
	;; not enough room in destination bytevector-s8
	(guard (exc ((assertion-violation? exc) #t))
	  (bytevector-s8-copy*! (view str (start 3))
				(view (S "abcd") (past 2)))))
    => #t)

  (check
      ;; whole bytevector-s8 copy
      (let* ((str (bytevector-copy (S "123"))))
	(bytevector-s8-copy*! str (S "abc"))
	str)
    => (S "abc"))

  (check
      ;; zero-elements bytevector-s8 copy
      (let* ((str (bytevector-copy (S "123"))))
	(bytevector-s8-copy*! str (view (S "abc") (start 2) (past 2)))
	str)
    => (S "123"))

  (check
      ;; one-element bytevector-s8 copy
      (let* ((str (bytevector-copy (S "123"))))
	(bytevector-s8-copy*! str (view (S "abc") (start 1) (past 2)))
	str)
    => (S "b23"))

  (check
      ;; two-elements bytevector-s8 copy
      (let* ((str (bytevector-copy (S "12"))))
	(bytevector-s8-copy*! str (view (S "abcd") (past 2)))
	str)
    => (S "ab"))

  (check
      (let ((str '#vu8()))
	(bytevector-s8-copy*! str (view (S "abcd") (start 0) (past 0)))
	str)
    => '#vu8())

  (check
      ;; over the same bytevector-s8, full
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-copy*! str str)
	str)
    => (S "0123456789"))

  (check
      ;; over the same bytevector-s8, in place
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-copy*! (view str (start 5)) (view str (start 5)))
	str)
    => (S "0123456789"))

  (check
      ;; over the same bytevector-s8, backwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-copy*! (view str (start 2))
			      (view str (start 4) (past 8)))
	str)
    => (S "0145676789"))

  (check
      ;; over the same bytevector-s8, backwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-copy*! (view str (start 0))
			      (view str (start 4) (past 8)))
	str)
    => (S "4567456789"))

  (check
      ;; over the same bytevector-s8, forwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-copy*! (view str (start 4))
			      (view str (start 2) (past 6)))
	str)
    => (S "0123234589"))

  (check
      ;; over the same bytevector-s8, forwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-copy*! (view str (start 6))
			      (view str (start 2) (past 6)))
	str)
    => (S "0123452345"))

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-copy (S "12"))))
	;; not enough room in destination bytevector-s8
	;;(bytevector-s8-reverse-copy*! (str 3) (view '#(#\a #\b #\c #\d) (past 2)))
	(guard (exc ((assertion-violation? exc) #t))
	  (bytevector-s8-reverse-copy*! (view str (start 3))
					(view (S "abcd") (past 2)))))
    => #t)

  (check
      ;; whole bytevector-s8 copy
      (let* ((str (bytevector-copy (S "123"))))
	(bytevector-s8-reverse-copy*! str (S "abc"))
	str)
    => (S "cba"))

  (check
      ;; zero-elements bytevector-s8 copy
      (let* ((str (bytevector-copy (S "123"))))
	(bytevector-s8-reverse-copy*! str (view (S "abc") (start 2) (past 2)))
	str)
    => (S "123"))

  (check
      ;; one-element bytevector-s8 copy
      (let* ((str (bytevector-copy (S "123"))))
	(bytevector-s8-reverse-copy*! str (view (S "abc") (start 1) (past 2)))
	str)
    => (S "b23"))

  (check
      ;; two-elements bytevector-s8 copy
      (let* ((str (bytevector-copy (S "12"))))
	(bytevector-s8-reverse-copy*! str (view (S "abcd") (past 2)))
	str)
    => (S "ba"))

  (check
      (let ((str '#vu8()))
	(bytevector-s8-reverse-copy*! str (view (S "abcd") (start 0) (past 0)))
	str)
    => '#vu8())

  (check
      ;; over the same bytevector-s8, full
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-reverse-copy*! str str)
	str)
    => (S "9876543210"))

  (check
      ;; over the same bytevector-s8
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-reverse-copy*! (view str (start 5))
				      (view str (start 5)))
	str)
    => (S "0123498765"))

  (check
      ;; over the same bytevector-s8, backwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-reverse-copy*! (view str (start 2))
				      (view str (start 4) (past 8)))
	str)
    => (S "0176546789"))

  (check
      ;; over the same bytevector-s8, backwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-reverse-copy*! (view str (start 0))
				      (view str (start 4) (past 8)))
	str)
    => (S "7654456789"))

  (check
      ;; over the same bytevector-s8, forwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-reverse-copy*! (view str (start 4))
				      (view str (start 2) (past 6)))
	str)
    => (S "0123543289"))

  (check
      ;; over the same bytevector-s8, forwards
      (let* ((str (bytevector-copy (S "0123456789"))))
	(bytevector-s8-reverse-copy*! (view str (start 6))
				      (view str (start 2) (past 6)))
	str)
    => (S "0123455432"))

;;; --------------------------------------------------------------------

  (check
      (let ((str (bytevector-copy (S "012345"))))
	(bytevector-s8-swap! str 2 4)
	str)
    => (S "014325"))

  (check
      (let ((str (bytevector-copy (S "012345"))))
	(bytevector-s8-swap! str 2 2)
	str)
    => (S "012345"))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-s8-swap! '#vu8() 0 1))
    => #t)

  #f)


;;;; done

(check-report)

;;; end of fil
