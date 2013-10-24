;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for label <xstring>
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
  (nausicaa containers strings)
  (vicare containers char-sets)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: label <xstring>\n")


(parametrise ((check-test-name	'inheritance))

  (check
      (let (((S <xstring>) "ciao"))
        (S append " mamma"))
    => "ciao mamma")

  #t)


(parametrise ((check-test-name	'constructors))

  (check
      (let (((o <xstring>) "ciao"))
        (o concatenate '(" mamma")))
    => "ciao mamma")

  (check
      (let (((o <xstring>) "ciao"))
	(o concatenate-reverse '(" " "hello" " " "salut") " hola" 3))
    => "salut hello ciao ho")

  (check
      (let (((o <xstring>) "ciao"))
	(o concatenate-reverse '(" " "hello" " " "salut") " hola"))
    => "salut hello ciao hola")

  (check
      (let (((o <xstring>) "ciao"))
	(o concatenate-reverse '(" " "hello" " " "salut")))
    => "salut hello ciao")

  #t)


(parameterise ((check-test-name 'predicates))

  (check
      (let (((o <xstring>)  "ciao"))
	(o null?))
    => #f)

  (check
      (let (((o <xstring>) "aaaa"))
	(o every #\a))
    => #t)

  (check
      (let (((o <xstring>) "ddadd"))
	(o any #\a))
    => #t)

  #t)


(parameterise ((check-test-name 'comparison-case-sensitive))

  (check
      (let (((o <xstring>) "abcdefg"))
	(o compare "abcd123" values values values))
    => 4)

  (check-for-true
   (let (((o <xstring>) "abcd"))
     (o = o)))

  (check-for-false
   (let (((o <xstring>) "abcd"))
     (o <>  "abcd")))

  (check-for-false
   (let (((o <xstring>) "abcd"))
     (o <  "abcd")))

  (check-for-true
   (let (((o <xstring>) "abcd"))
     (o <=  "abcd")))

  (check-for-false
   (let (((o <xstring>) "abcd"))
     (o >  "abcd")))

  (check-for-true
   (let (((o <xstring>) "abcd"))
     (o >=  "abc")))

  #t)


(parameterise ((check-test-name 'comparison-case-insensitive))

  (check
      (let (((o <xstring>) "aBcdefg"))
	(o compare-ci  "abcd123" values values values))
    => 4)

  (check-for-true
   (let (((o <xstring>) "abcd"))
     (o ci=  "abcd")))

  (check-for-true
   (let (((o <xstring>) "abc"))
     (o ci<>  "abcd")))

  (check-for-true
   (let (((o <xstring>) "abc"))
     (o ci<  "abcd")))

  (check-for-false
   (let (((o <xstring>) "abcd"))
     (o ci<=  "abc")))

  (check-for-false
   (let (((o <xstring>) "abc"))
     (o ci>  "abcd")))

  (check-for-false
   (let (((o <xstring>) "abc"))
     (o ci>=  "abcd")))

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-sensitive))

  (check
      (let (((o <xstring>) "ci ao"))
	(o dictionary=?  "ciao"))
    => #t)

  (check
      (let (((o <xstring>) "ciao"))
	(o dictionary<?  "ciao1"))
    => #t)

  (check
      (let (((o <xstring>) "ci ao"))
	(o dictionary<=?  "ciao"))
    => #t)

  (check
      (let (((o <xstring>) "ciao"))
	(o dictionary>?  "ciao1"))
    => #f)

  (check
      (let (((o <xstring>) "ciao"))
	(o dictionary>=?  "ciao1"))
    => #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-insensitive))

  (check
      (let (((o <xstring>) "CIAO1"))
	(o dictionary-ci=?  "ciao"))
    => #f)

  (check
      (let (((o <xstring>) "CIAO1"))
	(o dictionary-ci<?  "ciao"))
    => #f)

  (check
      (let (((o <xstring>) "CIAO1"))
	(o dictionary-ci<=?  "ciao"))
    => #f)

  (check
      (let (((o <xstring>) "ciao"))
	(o dictionary-ci>?  "ciao"))
    => #f)

  (check
      (let (((o <xstring>) "CIAO"))
	(o dictionary-ci>=?  "ciao1"))
    => #f)

  #t)


(parameterise ((check-test-name 'comparison-string/number-case-sensitive))

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers=?  "foo4bar3zab2"))
    => #f)

  (check
      (let (((o <xstring>) "a"))
	(o string/numbers<>?  "ab"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers<?  "foo4bar3zab2"))
    => #f)

  (check
      (let (((o <xstring>) "foo4bar3zab2"))
	(o string/numbers<=?  "foo4bar3zab10"))
    => #t)

  (check
      (let (((o <xstring>) "foo12"))
	(o string/numbers>?  "12foo"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab"))
	(o string/numbers>=?  "foo4bar10"))
    => #f)

  #t)


(parameterise ((check-test-name 'comparison-string/number-case-insensitive))

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers-ci=?  "foo4bar3zab2"))
    => #f)

  (check
      (let (((o <xstring>) "ciao10"))
	(o string/numbers-ci<>?  "ciao3"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar10"))
	(o string/numbers-ci<?  "foo4bar3zab"))
    => #f)

  (check
      (let (((o <xstring>) "foo4bar3zab"))
	(o string/numbers-ci<=?  "foo4bar10"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers-ci>?  "foo4bar3zab2"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar10"))
	(o string/numbers-ci>=?  "foo4bar3zab"))
    => #t)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-string/number-case-sensitive))

  (check
      (let (((o <xstring>) "foo4bar10"))
	(o string/numbers-dictionary=?  "foo4bar3zab"))
    => #f)

  (check
      (let (((o <xstring>) "foo4bar3zab2"))
	(o string/numbers-dictionary<>?  "foo4bar3zab10"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers-dictionary<?  "foo4bar3zab2"))
    => #f)

  (check
      (let (((o <xstring>) "foo4bar3zab2"))
	(o string/numbers-dictionary<=?  "foo4bar3zab10"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers-dictionary>?  "foo4bar3zab2"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers-dictionary>=?  "foo4bar3zab2"))
    => #t)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-string/number-case-insensitive))

  (check
      (let (((o <xstring>) "foo4bar3zab10"))
	(o string/numbers-dictionary-ci=?  "foo4bar3zab2"))
    => #f)

  (check
      (let (((o <xstring>) "foo4bar3zab2"))
	(o string/numbers-dictionary-ci<>?  "foo4bar3zab10"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab2"))
	(o string/numbers-dictionary-ci<?  "foo4bar3zab10"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab2"))
	(o string/numbers-dictionary-ci<=?  "foo4bar3zab10"))
    => #t)

  (check
      (let (((o <xstring>) "foo4bar3zab2"))
	(o string/numbers-dictionary-ci>? "foo4bar3zab10"))
    => #f)

  (check
      (let (((o <xstring>) "foo4bar3zab"))
	(o string/numbers-dictionary-ci>=?  "foo4bar10"))
    => #f)

  #t)


(parameterise ((check-test-name 'mapping))

  (check
      (let (((o <xstring>) (string-copy "abcd")))
	(o map! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
		"0123")
	o)
    => "a1c3")

  (check
      (let (((o <xstring>) (string-copy "abcd")))
	(o map*! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
		 "01234")
	o)
    => "a1c3")

  (check
      (cadr (with-result
	     (let (((o <xstring>) "abcd"))
	       (o for-each* (lambda (i ch-a ch-b) (add-result (list i ch-a ch-b)))
			    "01234"))))
    => '((0 #\a #\0)
	 (1 #\b #\1)
	 (2 #\c #\2)
	 (3 #\d #\3)))

  (check
      (let (((o <xstring>) "abcd"))
	(o substring-map (lambda (ch) (char-upcase ch))))
    => "ABCD")

  (check
      (let (((o <xstring>) (string-copy "abcd")))
	(o substring-map! (lambda (ch) (char-upcase ch)))
	o)
    => "ABCD")

  (check
      (cadr (with-result
	     (let (((o <xstring>) "abcd"))
	       (o substring-for-each add-result))))
    => '(#\a #\b #\c #\d))

  #t)


(parameterise ((check-test-name 'case))

  (check
      (let (((o <xstring>) "123abcd"))
	(o upcase*))
    => "123ABCD")

  (check
      (let (((o <xstring>) (string-copy "123abcd")))
	(o upcase*!)
	o)
    => "123ABCD")

  (check
      (let (((o <xstring>) "123AbcD"))
	(o downcase*))
    => "123abcd")

  (check
      (let (((o <xstring>) (string-copy "123ABcd")))
	(o downcase*!)
	o)
    => "123abcd")

  (check
      (let (((o <xstring>) "123abcd"))
	(o titlecase*))
    => "123Abcd")

  (check
      (let (((o <xstring>) (string-copy "---abcd")))
	(o titlecase*!)
	o)
    => "---Abcd")

  #t)


(parameterise ((check-test-name 'folding))

  (check
      (let (((o <xstring>) "abcd"))
	(o fold-left (lambda (i nil x y) (cons (cons x y) nil)) '()
		     "ABCD"))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (let (((o <xstring>) "abcd"))
	(o fold-right (lambda (i nil x y) (cons (cons x y) nil)) '()
		      "ABCD"))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (let (((o <xstring>) "abcd"))
	(o fold-left* (lambda (i nil x y) (cons (cons x y) nil)) '()
		      "ABCDE"))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (let (((o <xstring>) "abcd"))
	(o fold-right* (lambda (i nil x y) (cons (cons x y) nil)) '()
		       "ABCDE"))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (let (((o <xstring>)  "abcd"))
	(o substring-fold-left cons '()))
    => '(#\d #\c #\b #\a))

  (check
      (let (((o <xstring>)  "abcd"))
	(o substring-fold-right cons '()))
    => '(#\a #\b #\c #\d))

  #t)


(parameterise ((check-test-name 'selecting))

  (check
      (let (((o <xstring>)  "abcd"))
	(o take 2))
    => "ab")

  (check
      (let (((o <xstring>)  "abcd"))
	(o take-right 2))
    => "cd")

  (check
      (let (((o <xstring>)  "abcd"))
	(o drop 2))
    => "cd")

  (check
      (let (((o <xstring>)  "abcd"))
	(o drop-right 2))
    => "ab")

  (check
      (let (((o <xstring>)  "aaabcd"))
	(o trim #\a))
    => "bcd")

  (check
      (let (((o <xstring>) "bcdaaa"))
	(o trim-right  #\a))
    => "bcd")

  (check
      (let (((o <xstring>) "aaabcdaaa"))
	(o trim-both  #\a))
    => "bcd")

  (check
      (let (((o <xstring>) "abc"))
	(o pad  5 #\0))
    => "00abc")

  (check
      (let (((o <xstring>) "abc"))
	(o pad-right  3 #\0))
    => "abc")

  #t)


(parameterise ((check-test-name 'prefix))

  (check
      (let (((o <xstring>) "abcdefg"))
	(o prefix-length  "abcd123"))
    => 4)

  (check
      (let (((o <xstring>)  "efgabcd"))
	(o suffix-length "123abcd"))
    => 4)

  (check
      (let (((o <xstring>) "aBcdefg"))
	(o prefix-length-ci  "abcd123"))
    => 4)

  (check
      (let (((o <xstring>)  "efgabCd"))
	(o suffix-length-ci "123abcd"))
    => 4)

  (check
      (let (((o <xstring>)  "abcd"))
	(o prefix? "aBcd123"))
    => #f)

  (check
      (let (((o <xstring>) "abcd"))
	(o prefix-ci?  "aBcd123"))
    => #t)

  (check
      (let (((o <xstring>)  "abcd"))
	(o suffix? "123aBcd"))
    => #f)

  (check
      (let (((o <xstring>) "abcd"))
	(o suffix-ci?  "123aBcd"))
    => #t)

  #t)


(parameterise ((check-test-name 'searching))

  (check
      (let (((o <xstring>)  "abcd"))
	(o index #\b))
    => 1)

  (check
      (let (((o <xstring>) "abcd"))
	(o index-right  #\b))
    => 1)

  (check
      (let (((o <xstring>) "bacd"))
	(o skip  #\b))
    => 1)

  (check
      (let (((o <xstring>) "acdb"))
	(o skip-right  #\b))
    => 2)

  (check
      (let (((o <xstring>) "abcbd"))
	(o count  #\b))
    => 2)

  (check
      (let (((o <xstring>) "ciao hello salut"))
	(o contains  "hello"))
    => 5)

  (check
      (let (((o <xstring>) "ciAO HELLO saLUT"))
	(o contains-ci  "hello"))
    => 5)

  #t)


(parameterise ((check-test-name 'filtering))

  (check
      (let (((o <xstring>) "abcbd"))
	(o delete  #\b))
    => "acd")

  (check
      (let (((o <xstring>) "abcbd"))
	(o filter  #\b))
    => "bb")


  #t)


(parameterise ((check-test-name 'lists-tokenise-join))

  (check
      (let (((o <xstring>) "abcd"))
	(o list*))
    => '(#\a #\b #\c #\d))

  (check
      (let (((o <xstring>) "ciao hello salut"))
	(o tokenize (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u)))
    => '("ciao" "hello" "salut"))

  (check
      (let (((o <xstring>) "c"))
	(o join '("i" "a" "o") "," 'strict-infix))
    => "c,i,a,o")

  #t)


(parameterise ((check-test-name 'xsubstring))

  (check
      (let (((o <xstring>) "ciao "))
	(o xsubstring  0 9))
    => "ciao ciao")

  (check
      (let (((o <xstring>) (string-copy "0123456789")))
	(o xcopy! "ciao " -5 5)
	o)
    => "ciao ciao ")


  #t)


(parameterise ((check-test-name 'filling))

  (check
      (let (((o <xstring>) (string-copy "abcd")))
	(o fill*! #\b)
	o)
    => "bbbb")


  #t)


(parameterise ((check-test-name 'reverse))

  (check
      (let (((o <xstring>) "abcd"))
	(o reverse))
    => "dcba")

  (check
      (let (((o <xstring>) (string-copy "abcd")))
	(o reverse!)
	o)
    => "dcba")

  #t)


(parameterise ((check-test-name 'replace))

  (check
      (let (((o <xstring>) "abcd"))
	(o replace "1234"))
    => "1234")

  #t)


(parameterise ((check-test-name 'mutating))

  (check	; whole string copy
      (let (((o <xstring>) (string-copy "123")))
	(o copy*! "abc")
	o)
    => "abc")

  (check	; whole string copy
      (let (((o <xstring>) (string-copy "123")))
	(o reverse-copy*! "abc")
	o)
    => "cba")

  (check
      (let (((o <xstring>) (string-copy "012345")))
	(o swap! 2 4)
	o)
    => "014325")

  #t)


;;;; done

(check-report)

;;; end of file
