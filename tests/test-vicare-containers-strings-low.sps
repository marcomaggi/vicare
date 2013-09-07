;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the strings library
;;;Date: Thu Jun  4, 2009
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
(import (vicare)
  (vicare containers strings low)
  (vicare containers char-sets)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers: low level strings library\n")


(parameterise ((check-test-name 'predicates))

  (check
      (string-null? "ciao")
    => #f)

  (check
      (string-null? "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(%string-every 123 "abc" 0 2))
    => '%string-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every #\a str beg end))
    => #t)

  (check
      (let* ((str "aaaab")
	     (beg 0)
	     (end (string-length str)))
	(%string-every #\a str beg end))
    => #f)

  (check
      (let* ((str "aabaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every #\a str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "aaaab")
	     (beg 0)
	     (end (string-length str)))
	(%string-every (char-set #\a) str beg end))
    => #f)

  (check
      (let* ((str "aabaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "aaaa2")
	     (beg 0)
	     (end (string-length str)))
	(%string-every char-alphabetic? str beg end))
    => #f)

  (check
      (let* ((str "aa2aa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every char-alphabetic? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(%string-any 123 "abc" 0 2))
    => '%string-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ddadd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any #\a str beg end))
    => #t)

  (check
      (let* ((str "dddda")
	     (beg 0)
	     (end (string-length str)))
	(%string-any #\a str beg end))
    => #t)

  (check
      (let* ((str "ddd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any #\a str beg end))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (let* ((str "dddaddd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "ddda")
	     (beg 0)
	     (end (string-length str)))
	(%string-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "dddd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "11a11")
	     (beg 0)
	     (end (string-length str)))
	(%string-any char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "11111a")
	     (beg 0)
	     (end (string-length str)))
	(%string-any char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "1111")
	     (beg 0)
	     (end (string-length str)))
	(%string-any char-alphabetic? str beg end))
    => #f)

  )


(parameterise ((check-test-name 'comparison-lexicographic-case-sensitive))

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "abcdef") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdB") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 "abcdB") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str "abcd")
	  (beg1 0) (end1 (string-length str))
	  (beg2 0) (end2 (string-length str)))
     (%string= str beg1 end1 str beg2 end2)))

  (check-for-true
   (let* ((str1 "12abcd") (beg1 2) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
     (%string= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
     (%string<> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is less than STR2:
  ;;
  ;;* If the character at the  mismatch index from STR1 is less than the
  ;;  character at the mismatch index from STR2.
  ;;
  ;;* The strings are equal up to the end of STR1 and STR2 is longer.

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc")  (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABCD") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABCD") (beg1 0) (end1 (string-length str1))
	  (str2 "A2CD") (beg2 0) (end2 (string-length str2)))
     (%string< str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is less than, or equal to, STR2:
  ;;
  ;;* If the strings are equal.
  ;;
  ;;* If the character at the  mismatch index from STR1 is less than the
  ;;  character at the mismatch index from STR2.
  ;;
  ;;* The strings are equal up to the end of STR1 and STR2 is longer.

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc")  (beg2 0) (end2 (string-length str2)))
     (%string<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
     (%string<= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is greater than STR2:
  ;;
  ;;* If the  character at the mismatch index from  STR1 is greater than
  ;;  the character at the mismatch index from STR2.
  ;;
  ;;* The strings are equal up to the end of STR2 and STR1 is longer.

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
     (%string> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is greater than, or equal to, STR2:
  ;;
  ;;* If the strings are equal.
  ;;
  ;;* If the  character at the mismatch index from  STR1 is greater than
  ;;  the character at the mismatch index from STR2.
  ;;
  ;;* The strings are equal up to the end of STR2 and STR1 is longer.

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
     (%string>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "ABCD") (beg2 0) (end2 (string-length str2)))
     (%string>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string>= str1 beg1 end1 str2 beg2 end2)))

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-case-insensitive))

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcda") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdb") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 "abcdb") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str "abcd")
	  (beg1 0) (end1 (string-length str))
	  (beg2 0) (end2 (string-length str)))
     (%string-ci= str beg1 end1 str beg2 end2)))

  (check-for-true
   (let* ((str1 "12abcd") (beg1 2) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
     (%string-ci= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
     (%string-ci< str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
     (%string-ci<= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "abc") (beg2 0) (end2 (string-length str2)))
     (%string-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	  (str2 "abcd") (beg2 0) (end2 (string-length str2)))
     (%string-ci>= str1 beg1 end1 str2 beg2 end2)))

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-sensitive))

  (check (%string-dictionary=? "" "")				=> #t)
  (check (%string-dictionary=? "a" "")				=> #f)
  (check (%string-dictionary=? "" "a")				=> #f)
  (check (%string-dictionary=? "ab" "a")			=> #f)
  (check (%string-dictionary=? "a" "ab")			=> #f)
  (check (%string-dictionary=? "ciao" "ciao")			=> #t)
  (check (%string-dictionary=? "ciao1" "ciao")			=> #f)
  (check (%string-dictionary=? "ciao" "ciao1")			=> #f)

  (check (%string-dictionary=? "ci ao" "ciao")			=> #t)
  (check (%string-dictionary=? "ciao" "ci ao")			=> #t)
  (check (%string-dictionary=? "ci\tao" "ciao")			=> #t)
  (check (%string-dictionary=? "ciao" "ci\tao")			=> #t)
  (check (%string-dictionary=? "ci\nao" "ciao")			=> #t)
  (check (%string-dictionary=? "ciao" "ci\nao")			=> #t)
  (check (%string-dictionary=? "ci\vao" "ciao")			=> #t)
  (check (%string-dictionary=? "ciao" "ci\tao")			=> #t)
  (check (%string-dictionary=? "ci\fao" "ciao")			=> #t)
  (check (%string-dictionary=? "ciao" "ci\fao")			=> #t)
  (check (%string-dictionary=? "ci\rao" "ciao")			=> #t)
  (check (%string-dictionary=? "ciao" "ci\rao")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string-dictionary<? "" "")				=> #f)
  (check (%string-dictionary<? "a" "")				=> #f)
  (check (%string-dictionary<? "" "a")				=> #t)
  (check (%string-dictionary<? "ab" "a")			=> #f)
  (check (%string-dictionary<? "a" "ab")			=> #t)
  (check (%string-dictionary<? "ciao" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao1" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao" "ciao1")			=> #t)

  (check (%string-dictionary<? "ci ao" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao" "ci ao")			=> #f)
  (check (%string-dictionary<? "ci\tao" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao" "ci\tao")			=> #f)
  (check (%string-dictionary<? "ci\nao" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao" "ci\nao")			=> #f)
  (check (%string-dictionary<? "ci\vao" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao" "ci\tao")			=> #f)
  (check (%string-dictionary<? "ci\fao" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao" "ci\fao")			=> #f)
  (check (%string-dictionary<? "ci\rao" "ciao")			=> #f)
  (check (%string-dictionary<? "ciao" "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string-dictionary<=? "" "")				=> #t)
  (check (%string-dictionary<=? "a" "")				=> #f)
  (check (%string-dictionary<=? "" "a")				=> #t)
  (check (%string-dictionary<=? "ab" "a")			=> #f)
  (check (%string-dictionary<=? "a" "ab")			=> #t)
  (check (%string-dictionary<=? "ciao" "ciao")			=> #t)
  (check (%string-dictionary<=? "ciao1" "ciao")			=> #f)
  (check (%string-dictionary<=? "ciao" "ciao1")			=> #t)

  (check (%string-dictionary<=? "ci ao" "ciao")			=> #t)
  (check (%string-dictionary<=? "ciao" "ci ao")			=> #t)
  (check (%string-dictionary<=? "ci\tao" "ciao")		=> #t)
  (check (%string-dictionary<=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary<=? "ci\nao" "ciao")		=> #t)
  (check (%string-dictionary<=? "ciao" "ci\nao")		=> #t)
  (check (%string-dictionary<=? "ci\vao" "ciao")		=> #t)
  (check (%string-dictionary<=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary<=? "ci\fao" "ciao")		=> #t)
  (check (%string-dictionary<=? "ciao" "ci\fao")		=> #t)
  (check (%string-dictionary<=? "ci\rao" "ciao")		=> #t)
  (check (%string-dictionary<=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (%string-dictionary>? "" "")				=> #f)
  (check (%string-dictionary>? "a" "")				=> #t)
  (check (%string-dictionary>? "" "a")				=> #f)
  (check (%string-dictionary>? "ab" "a")			=> #t)
  (check (%string-dictionary>? "a" "ab")			=> #f)
  (check (%string-dictionary>? "ciao" "ciao")			=> #f)
  (check (%string-dictionary>? "ciao1" "ciao")			=> #t)
  (check (%string-dictionary>? "ciao" "ciao1")			=> #f)

  (check (%string-dictionary>? "ci ao" "ciao")			=> #f)
  (check (%string-dictionary>? "ciao" "ci ao")			=> #f)
  (check (%string-dictionary>? "ci\tao" "ciao")			=> #f)
  (check (%string-dictionary>? "ciao" "ci\tao")			=> #f)
  (check (%string-dictionary>? "ci\nao" "ciao")			=> #f)
  (check (%string-dictionary>? "ciao" "ci\nao")			=> #f)
  (check (%string-dictionary>? "ci\vao" "ciao")			=> #f)
  (check (%string-dictionary>? "ciao" "ci\tao")			=> #f)
  (check (%string-dictionary>? "ci\fao" "ciao")			=> #f)
  (check (%string-dictionary>? "ciao" "ci\fao")			=> #f)
  (check (%string-dictionary>? "ci\rao" "ciao")			=> #f)
  (check (%string-dictionary>? "ciao" "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string-dictionary>=? "" "")				=> #t)
  (check (%string-dictionary>=? "a" "")				=> #t)
  (check (%string-dictionary>=? "" "a")				=> #f)
  (check (%string-dictionary>=? "ab" "a")			=> #t)
  (check (%string-dictionary>=? "a" "ab")			=> #f)
  (check (%string-dictionary>=? "ciao" "ciao")			=> #t)
  (check (%string-dictionary>=? "ciao1" "ciao")			=> #t)
  (check (%string-dictionary>=? "ciao" "ciao1")			=> #f)

  (check (%string-dictionary>=? "ci ao" "ciao")			=> #t)
  (check (%string-dictionary>=? "ciao" "ci ao")			=> #t)
  (check (%string-dictionary>=? "ci\tao" "ciao")		=> #t)
  (check (%string-dictionary>=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary>=? "ci\nao" "ciao")		=> #t)
  (check (%string-dictionary>=? "ciao" "ci\nao")		=> #t)
  (check (%string-dictionary>=? "ci\vao" "ciao")		=> #t)
  (check (%string-dictionary>=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary>=? "ci\fao" "ciao")		=> #t)
  (check (%string-dictionary>=? "ciao" "ci\fao")		=> #t)
  (check (%string-dictionary>=? "ci\rao" "ciao")		=> #t)
  (check (%string-dictionary>=? "ciao" "ci\rao")		=> #t)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-insensitive))

  (check (%string-dictionary-compare-ci "" "")			=> 0)
  (check (%string-dictionary-compare-ci "a" "")			=> +1)
  (check (%string-dictionary-compare-ci "" "a")			=> -1)
  (check (%string-dictionary-compare-ci "ab" "a")		=> +1)
  (check (%string-dictionary-compare-ci "a" "ab")		=> -1)
  (check (%string-dictionary-compare-ci "ciao" "ciao")		=> 0)
  (check (%string-dictionary-compare-ci "ciao1" "ciao")		=> +1)
  (check (%string-dictionary-compare-ci "ciao" "ciao1")		=> -1)
  (check (%string-dictionary-compare-ci "CIAO" "ciao")		=> 0)
  (check (%string-dictionary-compare-ci "CIAO1" "ciao")		=> +1)
  (check (%string-dictionary-compare-ci "CIAO" "ciao1")		=> -1)

  (check (%string-dictionary-compare-ci "ci ao" "ciao")		=> 0)
  (check (%string-dictionary-compare-ci "ciao" "ci ao")		=> 0)
  (check (%string-dictionary-compare-ci "ci\tao" "ciao")	=> 0)
  (check (%string-dictionary-compare-ci "ciao" "ci\tao")	=> 0)
  (check (%string-dictionary-compare-ci "ci\nao" "ciao")	=> 0)
  (check (%string-dictionary-compare-ci "ciao" "ci\nao")	=> 0)
  (check (%string-dictionary-compare-ci "ci\vao" "ciao")	=> 0)
  (check (%string-dictionary-compare-ci "ciao" "ci\tao")	=> 0)
  (check (%string-dictionary-compare-ci "ci\fao" "ciao")	=> 0)
  (check (%string-dictionary-compare-ci "ciao" "ci\fao")	=> 0)
  (check (%string-dictionary-compare-ci "ci\rao" "ciao")	=> 0)
  (check (%string-dictionary-compare-ci "ciao" "ci\rao")	=> 0)

;;; --------------------------------------------------------------------

  (check (%string-dictionary-ci=? "" "")			=> #t)
  (check (%string-dictionary-ci=? "a" "")			=> #f)
  (check (%string-dictionary-ci=? "" "a")			=> #f)
  (check (%string-dictionary-ci=? "ab" "a")			=> #f)
  (check (%string-dictionary-ci=? "a" "ab")			=> #f)
  (check (%string-dictionary-ci=? "ciao" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "ciao1" "ciao")		=> #f)
  (check (%string-dictionary-ci=? "ciao" "ciao1")		=> #f)
  (check (%string-dictionary-ci=? "CIAO" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "CIAO1" "ciao")		=> #f)
  (check (%string-dictionary-ci=? "CIAO" "ciao1")		=> #f)

  (check (%string-dictionary-ci=? "ci ao" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "ciao" "ci ao")		=> #t)
  (check (%string-dictionary-ci=? "ci\tao" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary-ci=? "ci\nao" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "ciao" "ci\nao")		=> #t)
  (check (%string-dictionary-ci=? "ci\vao" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary-ci=? "ci\fao" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "ciao" "ci\fao")		=> #t)
  (check (%string-dictionary-ci=? "ci\rao" "ciao")		=> #t)
  (check (%string-dictionary-ci=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (%string-dictionary-ci<? "" "")			=> #f)
  (check (%string-dictionary-ci<? "a" "")			=> #f)
  (check (%string-dictionary-ci<? "" "a")			=> #t)
  (check (%string-dictionary-ci<? "ab" "a")			=> #f)
  (check (%string-dictionary-ci<? "a" "ab")			=> #t)
  (check (%string-dictionary-ci<? "ciao" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao1" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao" "ciao1")		=> #t)
  (check (%string-dictionary-ci<? "CIAO" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "CIAO1" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "CIAO" "ciao1")		=> #t)

  (check (%string-dictionary-ci<? "ci ao" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao" "ci ao")		=> #f)
  (check (%string-dictionary-ci<? "ci\tao" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao" "ci\tao")		=> #f)
  (check (%string-dictionary-ci<? "ci\nao" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao" "ci\nao")		=> #f)
  (check (%string-dictionary-ci<? "ci\vao" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao" "ci\tao")		=> #f)
  (check (%string-dictionary-ci<? "ci\fao" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao" "ci\fao")		=> #f)
  (check (%string-dictionary-ci<? "ci\rao" "ciao")		=> #f)
  (check (%string-dictionary-ci<? "ciao" "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (%string-dictionary-ci<=? "" "")			=> #t)
  (check (%string-dictionary-ci<=? "a" "")			=> #f)
  (check (%string-dictionary-ci<=? "" "a")			=> #t)
  (check (%string-dictionary-ci<=? "ab" "a")			=> #f)
  (check (%string-dictionary-ci<=? "a" "ab")			=> #t)
  (check (%string-dictionary-ci<=? "ciao" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "ciao1" "ciao")		=> #f)
  (check (%string-dictionary-ci<=? "ciao" "ciao1")		=> #t)
  (check (%string-dictionary-ci<=? "CIAO" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "CIAO1" "ciao")		=> #f)
  (check (%string-dictionary-ci<=? "CIAO" "ciao1")		=> #t)

  (check (%string-dictionary-ci<=? "ci ao" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "ciao" "ci ao")		=> #t)
  (check (%string-dictionary-ci<=? "ci\tao" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary-ci<=? "ci\nao" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "ciao" "ci\nao")		=> #t)
  (check (%string-dictionary-ci<=? "ci\vao" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary-ci<=? "ci\fao" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "ciao" "ci\fao")		=> #t)
  (check (%string-dictionary-ci<=? "ci\rao" "ciao")		=> #t)
  (check (%string-dictionary-ci<=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (%string-dictionary-ci>? "" "")			=> #f)
  (check (%string-dictionary-ci>? "a" "")			=> #t)
  (check (%string-dictionary-ci>? "" "a")			=> #f)
  (check (%string-dictionary-ci>? "ab" "a")			=> #t)
  (check (%string-dictionary-ci>? "a" "ab")			=> #f)
  (check (%string-dictionary-ci>? "ciao" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "ciao1" "ciao")		=> #t)
  (check (%string-dictionary-ci>? "ciao" "ciao1")		=> #f)
  (check (%string-dictionary-ci>? "CIAO" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "CIAO1" "ciao")		=> #t)
  (check (%string-dictionary-ci>? "CIAO" "ciao1")		=> #f)

  (check (%string-dictionary-ci>? "ci ao" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "ciao" "ci ao")		=> #f)
  (check (%string-dictionary-ci>? "ci\tao" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "ciao" "ci\tao")		=> #f)
  (check (%string-dictionary-ci>? "ci\nao" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "ciao" "ci\nao")		=> #f)
  (check (%string-dictionary-ci>? "ci\vao" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "ciao" "ci\tao")		=> #f)
  (check (%string-dictionary-ci>? "ci\fao" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "ciao" "ci\fao")		=> #f)
  (check (%string-dictionary-ci>? "ci\rao" "ciao")		=> #f)
  (check (%string-dictionary-ci>? "ciao" "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (%string-dictionary-ci>=? "" "")			=> #t)
  (check (%string-dictionary-ci>=? "a" "")			=> #t)
  (check (%string-dictionary-ci>=? "" "a")			=> #f)
  (check (%string-dictionary-ci>=? "ab" "a")			=> #t)
  (check (%string-dictionary-ci>=? "a" "ab")			=> #f)
  (check (%string-dictionary-ci>=? "ciao" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao1" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao" "ciao1")		=> #f)
  (check (%string-dictionary-ci>=? "CIAO" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "CIAO1" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "CIAO" "ciao1")		=> #f)

  (check (%string-dictionary-ci>=? "ci ao" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao" "ci ao")		=> #t)
  (check (%string-dictionary-ci>=? "ci\tao" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary-ci>=? "ci\nao" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao" "ci\nao")		=> #t)
  (check (%string-dictionary-ci>=? "ci\vao" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao" "ci\tao")		=> #t)
  (check (%string-dictionary-ci>=? "ci\fao" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao" "ci\fao")		=> #t)
  (check (%string-dictionary-ci>=? "ci\rao" "ciao")		=> #t)
  (check (%string-dictionary-ci>=? "ciao" "ci\rao")		=> #t)

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-string/number-case-sensitive))

  (check (%string/numbers=? "" "")				=> #t)
  (check (%string/numbers=? "a" "")				=> #f)
  (check (%string/numbers=? "" "a")				=> #f)
  (check (%string/numbers=? "a" "a")				=> #t)
  (check (%string/numbers=? "1" "")				=> #f)
  (check (%string/numbers=? "" "1")				=> #f)
  (check (%string/numbers=? "1" "1")				=> #t)
  (check (%string/numbers=? "1" "2")				=> #f)
  (check (%string/numbers=? "2" "1")				=> #f)
  (check (%string/numbers=? "a" "ab")				=> #f)
  (check (%string/numbers=? "ab" "a")				=> #f)
  (check (%string/numbers=? "a" "a1")				=> #f)
  (check (%string/numbers=? "a1" "a")				=> #f)
  (check (%string/numbers=? "1" "1a")				=> #f)
  (check (%string/numbers=? "1a" "1")				=> #f)

  (check (%string/numbers=? "123" "45")				=> #f)
  (check (%string/numbers=? "45" "123")				=> #f)
  (check (%string/numbers=? "ciao3" "ciao10")			=> #f)
  (check (%string/numbers=? "ciao10" "ciao3")			=> #f)
  (check (%string/numbers=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%string/numbers=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%string/numbers=? "foo12" "12foo")			=> #f)
  (check (%string/numbers=? "12foo" "foo12")			=> #f)
  (check (%string/numbers=? "12bar" "foobar")			=> #f)
  (check (%string/numbers=? "12.3" "12.3")			=> #t)
  (check (%string/numbers=? "12.3" "12.10")			=> #f)
  (check (%string/numbers=? "12.10" "12.3")			=> #f)
  (check (%string/numbers=? "12.3" "12,10")			=> #f)
  (check (%string/numbers=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers<>? "" "")				=> #f)
  (check (%string/numbers<>? "a" "")				=> #t)
  (check (%string/numbers<>? "" "a")				=> #t)
  (check (%string/numbers<>? "a" "a")				=> #f)
  (check (%string/numbers<>? "1" "")				=> #t)
  (check (%string/numbers<>? "" "1")				=> #t)
  (check (%string/numbers<>? "1" "1")				=> #f)
  (check (%string/numbers<>? "1" "2")				=> #t)
  (check (%string/numbers<>? "2" "1")				=> #t)
  (check (%string/numbers<>? "a" "ab")				=> #t)
  (check (%string/numbers<>? "ab" "a")				=> #t)
  (check (%string/numbers<>? "a" "a1")				=> #t)
  (check (%string/numbers<>? "a1" "a")				=> #t)
  (check (%string/numbers<>? "1" "1a")				=> #t)
  (check (%string/numbers<>? "1a" "1")				=> #t)

  (check (%string/numbers<>? "123" "45")			=> #t)
  (check (%string/numbers<>? "45" "123")			=> #t)
  (check (%string/numbers<>? "ciao3" "ciao10")			=> #t)
  (check (%string/numbers<>? "ciao10" "ciao3")			=> #t)
  (check (%string/numbers<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers<>? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%string/numbers<>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%string/numbers<>? "foo12" "12foo")			=> #t)
  (check (%string/numbers<>? "12foo" "foo12")			=> #t)
  (check (%string/numbers<>? "12bar" "foobar")			=> #t)
  (check (%string/numbers<>? "12.3" "12.3")			=> #f)
  (check (%string/numbers<>? "12.3" "12.10")			=> #t)
  (check (%string/numbers<>? "12.10" "12.3")			=> #t)
  (check (%string/numbers<>? "12.3" "12,10")			=> #t)
  (check (%string/numbers<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers<? "" "")				=> #f)
  (check (%string/numbers<? "a" "")				=> #f)
  (check (%string/numbers<? "" "a")				=> #t)
  (check (%string/numbers<? "a" "a")				=> #f)
  (check (%string/numbers<? "1" "")				=> #f)
  (check (%string/numbers<? "" "1")				=> #t)
  (check (%string/numbers<? "1" "1")				=> #f)
  (check (%string/numbers<? "1" "2")				=> #t)
  (check (%string/numbers<? "2" "1")				=> #f)
  (check (%string/numbers<? "a" "ab")				=> #t)
  (check (%string/numbers<? "ab" "a")				=> #f)
  (check (%string/numbers<? "a" "a1")				=> #t)
  (check (%string/numbers<? "a1" "a")				=> #f)
  (check (%string/numbers<? "1" "1a")				=> #t)
  (check (%string/numbers<? "1a" "1")				=> #f)

  (check (%string/numbers<? "123" "45")				=> #f)
  (check (%string/numbers<? "45" "123")				=> #t)
  (check (%string/numbers<? "ciao3" "ciao10")			=> #t)
  (check (%string/numbers<? "ciao10" "ciao3")			=> #f)
  (check (%string/numbers<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers<? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%string/numbers<? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%string/numbers<? "foo12" "12foo")			=> #f)
  (check (%string/numbers<? "12foo" "foo12")			=> #t)
  (check (%string/numbers<? "12bar" "foobar")			=> #t)
  (check (%string/numbers<? "12.3" "12.3")			=> #f)
  (check (%string/numbers<? "12.3" "12.10")			=> #t)
  (check (%string/numbers<? "12.10" "12.3")			=> #f)
  (check (%string/numbers<? "12.3" "12,10")			=> #f)
  (check (%string/numbers<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers<=? "" "")				=> #t)
  (check (%string/numbers<=? "a" "")				=> #f)
  (check (%string/numbers<=? "" "a")				=> #t)
  (check (%string/numbers<=? "a" "a")				=> #t)
  (check (%string/numbers<=? "1" "")				=> #f)
  (check (%string/numbers<=? "" "1")				=> #t)
  (check (%string/numbers<=? "1" "1")				=> #t)
  (check (%string/numbers<=? "1" "2")				=> #t)
  (check (%string/numbers<=? "2" "1")				=> #f)
  (check (%string/numbers<=? "a" "ab")				=> #t)
  (check (%string/numbers<=? "ab" "a")				=> #f)
  (check (%string/numbers<=? "a" "a1")				=> #t)
  (check (%string/numbers<=? "a1" "a")				=> #f)
  (check (%string/numbers<=? "1" "1a")				=> #t)
  (check (%string/numbers<=? "1a" "1")				=> #f)

  (check (%string/numbers<=? "123" "45")			=> #f)
  (check (%string/numbers<=? "45" "123")			=> #t)
  (check (%string/numbers<=? "ciao3" "ciao10")			=> #t)
  (check (%string/numbers<=? "ciao10" "ciao3")			=> #f)
  (check (%string/numbers<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers<=? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%string/numbers<=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%string/numbers<=? "foo12" "12foo")			=> #f)
  (check (%string/numbers<=? "12foo" "foo12")			=> #t)
  (check (%string/numbers<=? "12bar" "foobar")			=> #t)
  (check (%string/numbers<=? "12.3" "12.3")			=> #t)
  (check (%string/numbers<=? "12.3" "12.10")			=> #t)
  (check (%string/numbers<=? "12.10" "12.3")			=> #f)
  (check (%string/numbers<=? "12.3" "12,10")			=> #f)
  (check (%string/numbers<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers>? "" "")				=> #f)
  (check (%string/numbers>? "a" "")				=> #t)
  (check (%string/numbers>? "" "a")				=> #f)
  (check (%string/numbers>? "a" "a")				=> #f)
  (check (%string/numbers>? "1" "")				=> #t)
  (check (%string/numbers>? "" "1")				=> #f)
  (check (%string/numbers>? "1" "1")				=> #f)
  (check (%string/numbers>? "1" "2")				=> #f)
  (check (%string/numbers>? "2" "1")				=> #t)
  (check (%string/numbers>? "a" "ab")				=> #f)
  (check (%string/numbers>? "ab" "a")				=> #t)
  (check (%string/numbers>? "a" "a1")				=> #f)
  (check (%string/numbers>? "a1" "a")				=> #t)
  (check (%string/numbers>? "1" "1a")				=> #f)
  (check (%string/numbers>? "1a" "1")				=> #t)

  (check (%string/numbers>? "123" "45")				=> #t)
  (check (%string/numbers>? "45" "123")				=> #f)
  (check (%string/numbers>? "ciao3" "ciao10")			=> #f)
  (check (%string/numbers>? "ciao10" "ciao3")			=> #t)
  (check (%string/numbers>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers>? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%string/numbers>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%string/numbers>? "foo12" "12foo")			=> #t)
  (check (%string/numbers>? "12foo" "foo12")			=> #f)
  (check (%string/numbers>? "12bar" "foobar")			=> #f)
  (check (%string/numbers>? "12.3" "12.3")			=> #f)
  (check (%string/numbers>? "12.3" "12.10")			=> #f)
  (check (%string/numbers>? "12.10" "12.3")			=> #t)
  (check (%string/numbers>? "12.3" "12,10")			=> #t)
  (check (%string/numbers>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers>=? "" "")				=> #t)
  (check (%string/numbers>=? "a" "")				=> #t)
  (check (%string/numbers>=? "" "a")				=> #f)
  (check (%string/numbers>=? "a" "a")				=> #t)
  (check (%string/numbers>=? "1" "")				=> #t)
  (check (%string/numbers>=? "" "1")				=> #f)
  (check (%string/numbers>=? "1" "1")				=> #t)
  (check (%string/numbers>=? "1" "2")				=> #f)
  (check (%string/numbers>=? "2" "1")				=> #t)
  (check (%string/numbers>=? "a" "ab")				=> #f)
  (check (%string/numbers>=? "ab" "a")				=> #t)
  (check (%string/numbers>=? "a" "a1")				=> #f)
  (check (%string/numbers>=? "a1" "a")				=> #t)
  (check (%string/numbers>=? "1" "1a")				=> #f)
  (check (%string/numbers>=? "1a" "1")				=> #t)

  (check (%string/numbers>=? "123" "45")			=> #t)
  (check (%string/numbers>=? "45" "123")			=> #f)
  (check (%string/numbers>=? "ciao3" "ciao10")			=> #f)
  (check (%string/numbers>=? "ciao10" "ciao3")			=> #t)
  (check (%string/numbers>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers>=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%string/numbers>=? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%string/numbers>=? "foo12" "12foo")			=> #t)
  (check (%string/numbers>=? "12foo" "foo12")			=> #f)
  (check (%string/numbers>=? "12bar" "foobar")			=> #f)
  (check (%string/numbers>=? "12.3" "12.3")			=> #t)
  (check (%string/numbers>=? "12.3" "12.10")			=> #f)
  (check (%string/numbers>=? "12.10" "12.3")			=> #t)
  (check (%string/numbers>=? "12.3" "12,10")			=> #t)
  (check (%string/numbers>=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check
      (list-sort %string/numbers<? (quote ("foo123" "foo42" "foo7")))
    => '("foo7" "foo42" "foo123"))

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-string/number-case-insensitive))

  (check (%string/numbers-compare-ci "" "")			=> 0)
  (check (%string/numbers-compare-ci "a" "")			=> +1)
  (check (%string/numbers-compare-ci "" "a")			=> -1)
  (check (%string/numbers-compare-ci "a" "a")			=> 0)
  (check (%string/numbers-compare-ci "1" "")			=> +1)
  (check (%string/numbers-compare-ci "" "1")			=> -1)
  (check (%string/numbers-compare-ci "1" "1")			=> 0)
  (check (%string/numbers-compare-ci "1" "2")			=> -1)
  (check (%string/numbers-compare-ci "2" "1")			=> +1)
  (check (%string/numbers-compare-ci "a" "ab")			=> -1)
  (check (%string/numbers-compare-ci "ab" "a")			=> +1)
  (check (%string/numbers-compare-ci "a" "a1")			=> -1)
  (check (%string/numbers-compare-ci "a1" "a")			=> +1)
  (check (%string/numbers-compare-ci "1" "1a")			=> -1)
  (check (%string/numbers-compare-ci "1a" "1")			=> +1)
  (check (%string/numbers-compare-ci "a" "A")			=> 0)
  (check (%string/numbers-compare-ci "A" "a")			=> 0)

  (check (%string/numbers-compare-ci "123" "45")		=> +1)
  (check (%string/numbers-compare-ci "45" "123")		=> -1)
  (check (%string/numbers-compare-ci "ciao3" "ciao10")		=> -1)
  (check (%string/numbers-compare-ci "ciao10" "ciao3")		=> +1)
  (check (%string/numbers-compare-ci "foo4bar3zab10" "foo4bar3zab2")	=> +1)
  (check (%string/numbers-compare-ci "foo4bar3zab2" "foo4bar3zab10")	=> -1)
  (check (%string/numbers-compare-ci "foo4bar3zab" "foo4bar10")	=> -1)
  (check (%string/numbers-compare-ci "foo4bar10" "foo4bar3zab")	=> +1)
  (check (%string/numbers-compare-ci "foo12" "12foo")		=> +1)
  (check (%string/numbers-compare-ci "12foo" "foo12")		=> -1)
  (check (%string/numbers-compare-ci "12bar" "foobar")		=> -1)
  (check (%string/numbers-compare-ci "12.3" "12.3")		=> 0)
  (check (%string/numbers-compare-ci "12.3" "12.10")		=> -1)
  (check (%string/numbers-compare-ci "12.10" "12.3")		=> +1)
  (check (%string/numbers-compare-ci "12.3" "12,10")		=> +1)
  (check (%string/numbers-compare-ci "12,10" "12.3")		=> -1)

;;; --------------------------------------------------------------------

  (check (%string/numbers-ci=? "" "")				=> #t)
  (check (%string/numbers-ci=? "a" "")				=> #f)
  (check (%string/numbers-ci=? "" "a")				=> #f)
  (check (%string/numbers-ci=? "a" "a")				=> #t)
  (check (%string/numbers-ci=? "1" "")				=> #f)
  (check (%string/numbers-ci=? "" "1")				=> #f)
  (check (%string/numbers-ci=? "1" "1")				=> #t)
  (check (%string/numbers-ci=? "1" "2")				=> #f)
  (check (%string/numbers-ci=? "2" "1")				=> #f)
  (check (%string/numbers-ci=? "a" "ab")			=> #f)
  (check (%string/numbers-ci=? "ab" "a")			=> #f)
  (check (%string/numbers-ci=? "a" "a1")			=> #f)
  (check (%string/numbers-ci=? "a1" "a")			=> #f)
  (check (%string/numbers-ci=? "1" "1a")			=> #f)
  (check (%string/numbers-ci=? "1a" "1")			=> #f)
  (check (%string/numbers-ci=? "a" "A")				=> #t)
  (check (%string/numbers-ci=? "A" "a")				=> #t)

  (check (%string/numbers-ci=? "123" "45")			=> #f)
  (check (%string/numbers-ci=? "45" "123")			=> #f)
  (check (%string/numbers-ci=? "ciao3" "ciao10")		=> #f)
  (check (%string/numbers-ci=? "ciao10" "ciao3")		=> #f)
  (check (%string/numbers-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-ci=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%string/numbers-ci=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%string/numbers-ci=? "foo12" "12foo")			=> #f)
  (check (%string/numbers-ci=? "12foo" "foo12")			=> #f)
  (check (%string/numbers-ci=? "12bar" "foobar")		=> #f)
  (check (%string/numbers-ci=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-ci=? "12.3" "12.10")			=> #f)
  (check (%string/numbers-ci=? "12.10" "12.3")			=> #f)
  (check (%string/numbers-ci=? "12.3" "12,10")			=> #f)
  (check (%string/numbers-ci=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers-ci<>? "" "")				=> #f)
  (check (%string/numbers-ci<>? "a" "")				=> #t)
  (check (%string/numbers-ci<>? "" "a")				=> #t)
  (check (%string/numbers-ci<>? "a" "a")			=> #f)
  (check (%string/numbers-ci<>? "1" "")				=> #t)
  (check (%string/numbers-ci<>? "" "1")				=> #t)
  (check (%string/numbers-ci<>? "1" "1")			=> #f)
  (check (%string/numbers-ci<>? "1" "2")			=> #t)
  (check (%string/numbers-ci<>? "2" "1")			=> #t)
  (check (%string/numbers-ci<>? "a" "ab")			=> #t)
  (check (%string/numbers-ci<>? "ab" "a")			=> #t)
  (check (%string/numbers-ci<>? "a" "a1")			=> #t)
  (check (%string/numbers-ci<>? "a1" "a")			=> #t)
  (check (%string/numbers-ci<>? "1" "1a")			=> #t)
  (check (%string/numbers-ci<>? "1a" "1")			=> #t)
  (check (%string/numbers-ci<>? "A" "a")			=> #f)
  (check (%string/numbers-ci<>? "a" "A")			=> #f)

  (check (%string/numbers-ci<>? "123" "45")			=> #t)
  (check (%string/numbers-ci<>? "45" "123")			=> #t)
  (check (%string/numbers-ci<>? "ciao3" "ciao10")		=> #t)
  (check (%string/numbers-ci<>? "ciao10" "ciao3")		=> #t)
  (check (%string/numbers-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-ci<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%string/numbers-ci<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%string/numbers-ci<>? "foo12" "12foo")		=> #t)
  (check (%string/numbers-ci<>? "12foo" "foo12")		=> #t)
  (check (%string/numbers-ci<>? "12bar" "foobar")		=> #t)
  (check (%string/numbers-ci<>? "12.3" "12.3")			=> #f)
  (check (%string/numbers-ci<>? "12.3" "12.10")			=> #t)
  (check (%string/numbers-ci<>? "12.10" "12.3")			=> #t)
  (check (%string/numbers-ci<>? "12.3" "12,10")			=> #t)
  (check (%string/numbers-ci<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-ci<? "" "")				=> #f)
  (check (%string/numbers-ci<? "a" "")				=> #f)
  (check (%string/numbers-ci<? "" "a")				=> #t)
  (check (%string/numbers-ci<? "a" "a")				=> #f)
  (check (%string/numbers-ci<? "1" "")				=> #f)
  (check (%string/numbers-ci<? "" "1")				=> #t)
  (check (%string/numbers-ci<? "1" "1")				=> #f)
  (check (%string/numbers-ci<? "1" "2")				=> #t)
  (check (%string/numbers-ci<? "2" "1")				=> #f)
  (check (%string/numbers-ci<? "a" "ab")			=> #t)
  (check (%string/numbers-ci<? "ab" "a")			=> #f)
  (check (%string/numbers-ci<? "a" "a1")			=> #t)
  (check (%string/numbers-ci<? "a1" "a")			=> #f)
  (check (%string/numbers-ci<? "1" "1a")			=> #t)
  (check (%string/numbers-ci<? "1a" "1")			=> #f)
  (check (%string/numbers-ci<? "a" "A")				=> #f)
  (check (%string/numbers-ci<? "A" "a")				=> #f)

  (check (%string/numbers-ci<? "123" "45")			=> #f)
  (check (%string/numbers-ci<? "45" "123")			=> #t)
  (check (%string/numbers-ci<? "ciao3" "ciao10")		=> #t)
  (check (%string/numbers-ci<? "ciao10" "ciao3")		=> #f)
  (check (%string/numbers-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-ci<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%string/numbers-ci<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%string/numbers-ci<? "foo12" "12foo")			=> #f)
  (check (%string/numbers-ci<? "12foo" "foo12")			=> #t)
  (check (%string/numbers-ci<? "12bar" "foobar")		=> #t)
  (check (%string/numbers-ci<? "12.3" "12.3")			=> #f)
  (check (%string/numbers-ci<? "12.3" "12.10")			=> #t)
  (check (%string/numbers-ci<? "12.10" "12.3")			=> #f)
  (check (%string/numbers-ci<? "12.3" "12,10")			=> #f)
  (check (%string/numbers-ci<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-ci<=? "" "")				=> #t)
  (check (%string/numbers-ci<=? "a" "")				=> #f)
  (check (%string/numbers-ci<=? "" "a")				=> #t)
  (check (%string/numbers-ci<=? "a" "a")			=> #t)
  (check (%string/numbers-ci<=? "1" "")				=> #f)
  (check (%string/numbers-ci<=? "" "1")				=> #t)
  (check (%string/numbers-ci<=? "1" "1")			=> #t)
  (check (%string/numbers-ci<=? "1" "2")			=> #t)
  (check (%string/numbers-ci<=? "2" "1")			=> #f)
  (check (%string/numbers-ci<=? "a" "ab")			=> #t)
  (check (%string/numbers-ci<=? "ab" "a")			=> #f)
  (check (%string/numbers-ci<=? "a" "a1")			=> #t)
  (check (%string/numbers-ci<=? "a1" "a")			=> #f)
  (check (%string/numbers-ci<=? "1" "1a")			=> #t)
  (check (%string/numbers-ci<=? "1a" "1")			=> #f)
  (check (%string/numbers-ci<=? "a" "A")			=> #t)
  (check (%string/numbers-ci<=? "A" "a")			=> #t)

  (check (%string/numbers-ci<=? "123" "45")			=> #f)
  (check (%string/numbers-ci<=? "45" "123")			=> #t)
  (check (%string/numbers-ci<=? "ciao3" "ciao10")		=> #t)
  (check (%string/numbers-ci<=? "ciao10" "ciao3")		=> #f)
  (check (%string/numbers-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-ci<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%string/numbers-ci<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%string/numbers-ci<=? "foo12" "12foo")		=> #f)
  (check (%string/numbers-ci<=? "12foo" "foo12")		=> #t)
  (check (%string/numbers-ci<=? "12bar" "foobar")		=> #t)
  (check (%string/numbers-ci<=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-ci<=? "12.3" "12.10")			=> #t)
  (check (%string/numbers-ci<=? "12.10" "12.3")			=> #f)
  (check (%string/numbers-ci<=? "12.3" "12,10")			=> #f)
  (check (%string/numbers-ci<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-ci>? "" "")				=> #f)
  (check (%string/numbers-ci>? "a" "")				=> #t)
  (check (%string/numbers-ci>? "" "a")				=> #f)
  (check (%string/numbers-ci>? "a" "a")				=> #f)
  (check (%string/numbers-ci>? "1" "")				=> #t)
  (check (%string/numbers-ci>? "" "1")				=> #f)
  (check (%string/numbers-ci>? "1" "1")				=> #f)
  (check (%string/numbers-ci>? "1" "2")				=> #f)
  (check (%string/numbers-ci>? "2" "1")				=> #t)
  (check (%string/numbers-ci>? "a" "ab")			=> #f)
  (check (%string/numbers-ci>? "ab" "a")			=> #t)
  (check (%string/numbers-ci>? "a" "a1")			=> #f)
  (check (%string/numbers-ci>? "a1" "a")			=> #t)
  (check (%string/numbers-ci>? "1" "1a")			=> #f)
  (check (%string/numbers-ci>? "1a" "1")			=> #t)
  (check (%string/numbers-ci>? "a" "A")				=> #f)
  (check (%string/numbers-ci>? "A" "a")				=> #f)

  (check (%string/numbers-ci>? "123" "45")			=> #t)
  (check (%string/numbers-ci>? "45" "123")			=> #f)
  (check (%string/numbers-ci>? "ciao3" "ciao10")		=> #f)
  (check (%string/numbers-ci>? "ciao10" "ciao3")		=> #t)
  (check (%string/numbers-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-ci>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%string/numbers-ci>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%string/numbers-ci>? "foo12" "12foo")			=> #t)
  (check (%string/numbers-ci>? "12foo" "foo12")			=> #f)
  (check (%string/numbers-ci>? "12bar" "foobar")		=> #f)
  (check (%string/numbers-ci>? "12.3" "12.3")			=> #f)
  (check (%string/numbers-ci>? "12.3" "12.10")			=> #f)
  (check (%string/numbers-ci>? "12.10" "12.3")			=> #t)
  (check (%string/numbers-ci>? "12.3" "12,10")			=> #t)
  (check (%string/numbers-ci>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers-ci>=? "" "")				=> #t)
  (check (%string/numbers-ci>=? "a" "")				=> #t)
  (check (%string/numbers-ci>=? "" "a")				=> #f)
  (check (%string/numbers-ci>=? "a" "a")			=> #t)
  (check (%string/numbers-ci>=? "1" "")				=> #t)
  (check (%string/numbers-ci>=? "" "1")				=> #f)
  (check (%string/numbers-ci>=? "1" "1")			=> #t)
  (check (%string/numbers-ci>=? "1" "2")			=> #f)
  (check (%string/numbers-ci>=? "2" "1")			=> #t)
  (check (%string/numbers-ci>=? "a" "ab")			=> #f)
  (check (%string/numbers-ci>=? "ab" "a")			=> #t)
  (check (%string/numbers-ci>=? "a" "a1")			=> #f)
  (check (%string/numbers-ci>=? "a1" "a")			=> #t)
  (check (%string/numbers-ci>=? "1" "1a")			=> #f)
  (check (%string/numbers-ci>=? "1a" "1")			=> #t)
  (check (%string/numbers-ci>=? "a" "A")			=> #t)
  (check (%string/numbers-ci>=? "A" "a")			=> #t)

  (check (%string/numbers-ci>=? "123" "45")			=> #t)
  (check (%string/numbers-ci>=? "45" "123")			=> #f)
  (check (%string/numbers-ci>=? "ciao3" "ciao10")		=> #f)
  (check (%string/numbers-ci>=? "ciao10" "ciao3")		=> #t)
  (check (%string/numbers-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-ci>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%string/numbers-ci>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%string/numbers-ci>=? "foo12" "12foo")		=> #t)
  (check (%string/numbers-ci>=? "12foo" "foo12")		=> #f)
  (check (%string/numbers-ci>=? "12bar" "foobar")		=> #f)
  (check (%string/numbers-ci>=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-ci>=? "12.3" "12.10")			=> #f)
  (check (%string/numbers-ci>=? "12.10" "12.3")			=> #t)
  (check (%string/numbers-ci>=? "12.3" "12,10")			=> #t)
  (check (%string/numbers-ci>=? "12,10" "12.3")			=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-string/number-case-sensitive))

  (check (%string/numbers-dictionary=? "" "")				=> #t)
  (check (%string/numbers-dictionary=? "a" "")				=> #f)
  (check (%string/numbers-dictionary=? "" "a")				=> #f)
  (check (%string/numbers-dictionary=? "a" "a")				=> #t)
  (check (%string/numbers-dictionary=? "1" "")				=> #f)
  (check (%string/numbers-dictionary=? "" "1")				=> #f)
  (check (%string/numbers-dictionary=? "1" "1")				=> #t)
  (check (%string/numbers-dictionary=? "1" "2")				=> #f)
  (check (%string/numbers-dictionary=? "2" "1")				=> #f)
  (check (%string/numbers-dictionary=? "a" "ab")			=> #f)
  (check (%string/numbers-dictionary=? "ab" "a")			=> #f)
  (check (%string/numbers-dictionary=? "a" "a1")			=> #f)
  (check (%string/numbers-dictionary=? "a1" "a")			=> #f)
  (check (%string/numbers-dictionary=? "1" "1a")			=> #f)
  (check (%string/numbers-dictionary=? "1a" "1")			=> #f)

  (check (%string/numbers-dictionary=? "123" "45")			=> #f)
  (check (%string/numbers-dictionary=? "45" "123")			=> #f)
  (check (%string/numbers-dictionary=? "ciao3" "ciao10")		=> #f)
  (check (%string/numbers-dictionary=? "ciao10" "ciao3")		=> #f)
  (check (%string/numbers-dictionary=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-dictionary=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%string/numbers-dictionary=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%string/numbers-dictionary=? "foo12" "12foo")			=> #f)
  (check (%string/numbers-dictionary=? "12foo" "foo12")			=> #f)
  (check (%string/numbers-dictionary=? "12bar" "foobar")		=> #f)
  (check (%string/numbers-dictionary=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-dictionary=? "12.3" "12.10")			=> #f)
  (check (%string/numbers-dictionary=? "12.10" "12.3")			=> #f)
  (check (%string/numbers-dictionary=? "12.3" "12,10")			=> #f)
  (check (%string/numbers-dictionary=? "12,10" "12.3")			=> #f)

  (check (%string/numbers-dictionary=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary<>? "" "")				=> #f)
  (check (%string/numbers-dictionary<>? "a" "")				=> #t)
  (check (%string/numbers-dictionary<>? "" "a")				=> #t)
  (check (%string/numbers-dictionary<>? "a" "a")			=> #f)
  (check (%string/numbers-dictionary<>? "1" "")				=> #t)
  (check (%string/numbers-dictionary<>? "" "1")				=> #t)
  (check (%string/numbers-dictionary<>? "1" "1")			=> #f)
  (check (%string/numbers-dictionary<>? "1" "2")			=> #t)
  (check (%string/numbers-dictionary<>? "2" "1")			=> #t)
  (check (%string/numbers-dictionary<>? "a" "ab")			=> #t)
  (check (%string/numbers-dictionary<>? "ab" "a")			=> #t)
  (check (%string/numbers-dictionary<>? "a" "a1")			=> #t)
  (check (%string/numbers-dictionary<>? "a1" "a")			=> #t)
  (check (%string/numbers-dictionary<>? "1" "1a")			=> #t)
  (check (%string/numbers-dictionary<>? "1a" "1")			=> #t)

  (check (%string/numbers-dictionary<>? "123" "45")			=> #t)
  (check (%string/numbers-dictionary<>? "45" "123")			=> #t)
  (check (%string/numbers-dictionary<>? "ciao3" "ciao10")		=> #t)
  (check (%string/numbers-dictionary<>? "ciao10" "ciao3")		=> #t)
  (check (%string/numbers-dictionary<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-dictionary<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%string/numbers-dictionary<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%string/numbers-dictionary<>? "foo12" "12foo")		=> #t)
  (check (%string/numbers-dictionary<>? "12foo" "foo12")		=> #t)
  (check (%string/numbers-dictionary<>? "12bar" "foobar")		=> #t)
  (check (%string/numbers-dictionary<>? "12.3" "12.3")			=> #f)
  (check (%string/numbers-dictionary<>? "12.3" "12.10")			=> #t)
  (check (%string/numbers-dictionary<>? "12.10" "12.3")			=> #t)
  (check (%string/numbers-dictionary<>? "12.3" "12,10")			=> #t)
  (check (%string/numbers-dictionary<>? "12,10" "12.3")			=> #t)

  (check (%string/numbers-dictionary<>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary<>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary<? "" "")				=> #f)
  (check (%string/numbers-dictionary<? "a" "")				=> #f)
  (check (%string/numbers-dictionary<? "" "a")				=> #t)
  (check (%string/numbers-dictionary<? "a" "a")				=> #f)
  (check (%string/numbers-dictionary<? "1" "")				=> #f)
  (check (%string/numbers-dictionary<? "" "1")				=> #t)
  (check (%string/numbers-dictionary<? "1" "1")				=> #f)
  (check (%string/numbers-dictionary<? "1" "2")				=> #t)
  (check (%string/numbers-dictionary<? "2" "1")				=> #f)
  (check (%string/numbers-dictionary<? "a" "ab")			=> #t)
  (check (%string/numbers-dictionary<? "ab" "a")			=> #f)
  (check (%string/numbers-dictionary<? "a" "a1")			=> #t)
  (check (%string/numbers-dictionary<? "a1" "a")			=> #f)
  (check (%string/numbers-dictionary<? "1" "1a")			=> #t)
  (check (%string/numbers-dictionary<? "1a" "1")			=> #f)

  (check (%string/numbers-dictionary<? "123" "45")			=> #f)
  (check (%string/numbers-dictionary<? "45" "123")			=> #t)
  (check (%string/numbers-dictionary<? "ciao3" "ciao10")		=> #t)
  (check (%string/numbers-dictionary<? "ciao10" "ciao3")		=> #f)
  (check (%string/numbers-dictionary<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-dictionary<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%string/numbers-dictionary<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%string/numbers-dictionary<? "foo12" "12foo")			=> #f)
  (check (%string/numbers-dictionary<? "12foo" "foo12")			=> #t)
  (check (%string/numbers-dictionary<? "12bar" "foobar")		=> #t)
  (check (%string/numbers-dictionary<? "12.3" "12.3")			=> #f)
  (check (%string/numbers-dictionary<? "12.3" "12.10")			=> #t)
  (check (%string/numbers-dictionary<? "12.10" "12.3")			=> #f)
  (check (%string/numbers-dictionary<? "12.3" "12,10")			=> #f)
  (check (%string/numbers-dictionary<? "12,10" "12.3")			=> #t)

  (check (%string/numbers-dictionary<? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary<? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary<=? "" "")				=> #t)
  (check (%string/numbers-dictionary<=? "a" "")				=> #f)
  (check (%string/numbers-dictionary<=? "" "a")				=> #t)
  (check (%string/numbers-dictionary<=? "a" "a")			=> #t)
  (check (%string/numbers-dictionary<=? "1" "")				=> #f)
  (check (%string/numbers-dictionary<=? "" "1")				=> #t)
  (check (%string/numbers-dictionary<=? "1" "1")			=> #t)
  (check (%string/numbers-dictionary<=? "1" "2")			=> #t)
  (check (%string/numbers-dictionary<=? "2" "1")			=> #f)
  (check (%string/numbers-dictionary<=? "a" "ab")			=> #t)
  (check (%string/numbers-dictionary<=? "ab" "a")			=> #f)
  (check (%string/numbers-dictionary<=? "a" "a1")			=> #t)
  (check (%string/numbers-dictionary<=? "a1" "a")			=> #f)
  (check (%string/numbers-dictionary<=? "1" "1a")			=> #t)
  (check (%string/numbers-dictionary<=? "1a" "1")			=> #f)

  (check (%string/numbers-dictionary<=? "123" "45")			=> #f)
  (check (%string/numbers-dictionary<=? "45" "123")			=> #t)
  (check (%string/numbers-dictionary<=? "ciao3" "ciao10")		=> #t)
  (check (%string/numbers-dictionary<=? "ciao10" "ciao3")		=> #f)
  (check (%string/numbers-dictionary<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-dictionary<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%string/numbers-dictionary<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%string/numbers-dictionary<=? "foo12" "12foo")		=> #f)
  (check (%string/numbers-dictionary<=? "12foo" "foo12")		=> #t)
  (check (%string/numbers-dictionary<=? "12bar" "foobar")		=> #t)
  (check (%string/numbers-dictionary<=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-dictionary<=? "12.3" "12.10")			=> #t)
  (check (%string/numbers-dictionary<=? "12.10" "12.3")			=> #f)
  (check (%string/numbers-dictionary<=? "12.3" "12,10")			=> #f)
  (check (%string/numbers-dictionary<=? "12,10" "12.3")			=> #t)

  (check (%string/numbers-dictionary<=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary<=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary>? "" "")				=> #f)
  (check (%string/numbers-dictionary>? "a" "")				=> #t)
  (check (%string/numbers-dictionary>? "" "a")				=> #f)
  (check (%string/numbers-dictionary>? "a" "a")				=> #f)
  (check (%string/numbers-dictionary>? "1" "")				=> #t)
  (check (%string/numbers-dictionary>? "" "1")				=> #f)
  (check (%string/numbers-dictionary>? "1" "1")				=> #f)
  (check (%string/numbers-dictionary>? "1" "2")				=> #f)
  (check (%string/numbers-dictionary>? "2" "1")				=> #t)
  (check (%string/numbers-dictionary>? "a" "ab")			=> #f)
  (check (%string/numbers-dictionary>? "ab" "a")			=> #t)
  (check (%string/numbers-dictionary>? "a" "a1")			=> #f)
  (check (%string/numbers-dictionary>? "a1" "a")			=> #t)
  (check (%string/numbers-dictionary>? "1" "1a")			=> #f)
  (check (%string/numbers-dictionary>? "1a" "1")			=> #t)

  (check (%string/numbers-dictionary>? "123" "45")			=> #t)
  (check (%string/numbers-dictionary>? "45" "123")			=> #f)
  (check (%string/numbers-dictionary>? "ciao3" "ciao10")		=> #f)
  (check (%string/numbers-dictionary>? "ciao10" "ciao3")		=> #t)
  (check (%string/numbers-dictionary>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-dictionary>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%string/numbers-dictionary>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%string/numbers-dictionary>? "foo12" "12foo")			=> #t)
  (check (%string/numbers-dictionary>? "12foo" "foo12")			=> #f)
  (check (%string/numbers-dictionary>? "12bar" "foobar")		=> #f)
  (check (%string/numbers-dictionary>? "12.3" "12.3")			=> #f)
  (check (%string/numbers-dictionary>? "12.3" "12.10")			=> #f)
  (check (%string/numbers-dictionary>? "12.10" "12.3")			=> #t)
  (check (%string/numbers-dictionary>? "12.3" "12,10")			=> #t)
  (check (%string/numbers-dictionary>? "12,10" "12.3")			=> #f)

  (check (%string/numbers-dictionary>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary>=? "" "")				=> #t)
  (check (%string/numbers-dictionary>=? "a" "")				=> #t)
  (check (%string/numbers-dictionary>=? "" "a")				=> #f)
  (check (%string/numbers-dictionary>=? "a" "a")			=> #t)
  (check (%string/numbers-dictionary>=? "1" "")				=> #t)
  (check (%string/numbers-dictionary>=? "" "1")				=> #f)
  (check (%string/numbers-dictionary>=? "1" "1")			=> #t)
  (check (%string/numbers-dictionary>=? "1" "2")			=> #f)
  (check (%string/numbers-dictionary>=? "2" "1")			=> #t)
  (check (%string/numbers-dictionary>=? "a" "ab")			=> #f)
  (check (%string/numbers-dictionary>=? "ab" "a")			=> #t)
  (check (%string/numbers-dictionary>=? "a" "a1")			=> #f)
  (check (%string/numbers-dictionary>=? "a1" "a")			=> #t)
  (check (%string/numbers-dictionary>=? "1" "1a")			=> #f)
  (check (%string/numbers-dictionary>=? "1a" "1")			=> #t)

  (check (%string/numbers-dictionary>=? "123" "45")			=> #t)
  (check (%string/numbers-dictionary>=? "45" "123")			=> #f)
  (check (%string/numbers-dictionary>=? "ciao3" "ciao10")		=> #f)
  (check (%string/numbers-dictionary>=? "ciao10" "ciao3")		=> #t)
  (check (%string/numbers-dictionary>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-dictionary>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%string/numbers-dictionary>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%string/numbers-dictionary>=? "foo12" "12foo")		=> #t)
  (check (%string/numbers-dictionary>=? "12foo" "foo12")		=> #f)
  (check (%string/numbers-dictionary>=? "12bar" "foobar")		=> #f)
  (check (%string/numbers-dictionary>=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-dictionary>=? "12.3" "12.10")			=> #f)
  (check (%string/numbers-dictionary>=? "12.10" "12.3")			=> #t)
  (check (%string/numbers-dictionary>=? "12.3" "12,10")			=> #t)
  (check (%string/numbers-dictionary>=? "12,10" "12.3")			=> #f)

  (check (%string/numbers-dictionary>=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary>=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-string/number-case-insensitive))

  (check (%string/numbers-dictionary-compare-ci "" "")				=> 0)
  (check (%string/numbers-dictionary-compare-ci "a" "")				=> +1)
  (check (%string/numbers-dictionary-compare-ci "" "a")				=> -1)
  (check (%string/numbers-dictionary-compare-ci "a" "a")			=> 0)
  (check (%string/numbers-dictionary-compare-ci "1" "")				=> +1)
  (check (%string/numbers-dictionary-compare-ci "" "1")				=> -1)
  (check (%string/numbers-dictionary-compare-ci "1" "1")			=> 0)
  (check (%string/numbers-dictionary-compare-ci "1" "2")			=> -1)
  (check (%string/numbers-dictionary-compare-ci "2" "1")			=> +1)
  (check (%string/numbers-dictionary-compare-ci "a" "ab")			=> -1)
  (check (%string/numbers-dictionary-compare-ci "ab" "a")			=> +1)
  (check (%string/numbers-dictionary-compare-ci "a" "a1")			=> -1)
  (check (%string/numbers-dictionary-compare-ci "a1" "a")			=> +1)
  (check (%string/numbers-dictionary-compare-ci "1" "1a")			=> -1)
  (check (%string/numbers-dictionary-compare-ci "1a" "1")			=> +1)
  (check (%string/numbers-dictionary-compare-ci "a" "A")			=> 0)
  (check (%string/numbers-dictionary-compare-ci "A" "a")			=> 0)

  (check (%string/numbers-dictionary-compare-ci "123" "45")			=> +1)
  (check (%string/numbers-dictionary-compare-ci "45" "123")			=> -1)
  (check (%string/numbers-dictionary-compare-ci "ciao3" "ciao10")		=> -1)
  (check (%string/numbers-dictionary-compare-ci "ciao10" "ciao3")		=> +1)
  (check (%string/numbers-dictionary-compare-ci "foo4bar3zab10" "foo4bar3zab2")	=> +1)
  (check (%string/numbers-dictionary-compare-ci "foo4bar3zab2" "foo4bar3zab10")	=> -1)
  (check (%string/numbers-dictionary-compare-ci "foo4bar3zab" "foo4bar10")	=> -1)
  (check (%string/numbers-dictionary-compare-ci "foo4bar10" "foo4bar3zab")	=> +1)
  (check (%string/numbers-dictionary-compare-ci "foo12" "12foo")		=> +1)
  (check (%string/numbers-dictionary-compare-ci "12foo" "foo12")		=> -1)
  (check (%string/numbers-dictionary-compare-ci "12bar" "foobar")		=> -1)
  (check (%string/numbers-dictionary-compare-ci "12.3" "12.3")			=> 0)
  (check (%string/numbers-dictionary-compare-ci "12.3" "12.10")			=> -1)
  (check (%string/numbers-dictionary-compare-ci "12.10" "12.3")			=> +1)
  (check (%string/numbers-dictionary-compare-ci "12.3" "12,10")			=> +1)
  (check (%string/numbers-dictionary-compare-ci "12,10" "12.3")			=> -1)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary-ci=? "" "")				=> #t)
  (check (%string/numbers-dictionary-ci=? "a" "")				=> #f)
  (check (%string/numbers-dictionary-ci=? "" "a")				=> #f)
  (check (%string/numbers-dictionary-ci=? "a" "a")				=> #t)
  (check (%string/numbers-dictionary-ci=? "1" "")				=> #f)
  (check (%string/numbers-dictionary-ci=? "" "1")				=> #f)
  (check (%string/numbers-dictionary-ci=? "1" "1")				=> #t)
  (check (%string/numbers-dictionary-ci=? "1" "2")				=> #f)
  (check (%string/numbers-dictionary-ci=? "2" "1")				=> #f)
  (check (%string/numbers-dictionary-ci=? "a" "ab")				=> #f)
  (check (%string/numbers-dictionary-ci=? "ab" "a")				=> #f)
  (check (%string/numbers-dictionary-ci=? "a" "a1")				=> #f)
  (check (%string/numbers-dictionary-ci=? "a1" "a")				=> #f)
  (check (%string/numbers-dictionary-ci=? "1" "1a")				=> #f)
  (check (%string/numbers-dictionary-ci=? "1a" "1")				=> #f)
  (check (%string/numbers-dictionary-ci=? "a" "A")				=> #t)
  (check (%string/numbers-dictionary-ci=? "A" "a")				=> #t)

  (check (%string/numbers-dictionary-ci=? "123" "45")				=> #f)
  (check (%string/numbers-dictionary-ci=? "45" "123")				=> #f)
  (check (%string/numbers-dictionary-ci=? "ciao3" "ciao10")			=> #f)
  (check (%string/numbers-dictionary-ci=? "ciao10" "ciao3")			=> #f)
  (check (%string/numbers-dictionary-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-dictionary-ci=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%string/numbers-dictionary-ci=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%string/numbers-dictionary-ci=? "foo12" "12foo")			=> #f)
  (check (%string/numbers-dictionary-ci=? "12foo" "foo12")			=> #f)
  (check (%string/numbers-dictionary-ci=? "12bar" "foobar")			=> #f)
  (check (%string/numbers-dictionary-ci=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-dictionary-ci=? "12.3" "12.10")			=> #f)
  (check (%string/numbers-dictionary-ci=? "12.10" "12.3")			=> #f)
  (check (%string/numbers-dictionary-ci=? "12.3" "12,10")			=> #f)
  (check (%string/numbers-dictionary-ci=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary-ci<>? "" "")				=> #f)
  (check (%string/numbers-dictionary-ci<>? "a" "")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "" "a")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "a" "a")				=> #f)
  (check (%string/numbers-dictionary-ci<>? "1" "")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "" "1")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "1" "1")				=> #f)
  (check (%string/numbers-dictionary-ci<>? "1" "2")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "2" "1")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "a" "ab")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "ab" "a")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "a" "a1")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "a1" "a")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "1" "1a")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "1a" "1")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "A" "a")				=> #f)
  (check (%string/numbers-dictionary-ci<>? "a" "A")				=> #f)

  (check (%string/numbers-dictionary-ci<>? "123" "45")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "45" "123")				=> #t)
  (check (%string/numbers-dictionary-ci<>? "ciao3" "ciao10")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "ciao10" "ciao3")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-dictionary-ci<>? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%string/numbers-dictionary-ci<>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%string/numbers-dictionary-ci<>? "foo12" "12foo")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "12foo" "foo12")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "12bar" "foobar")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "12.3" "12.3")			=> #f)
  (check (%string/numbers-dictionary-ci<>? "12.3" "12.10")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "12.10" "12.3")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "12.3" "12,10")			=> #t)
  (check (%string/numbers-dictionary-ci<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary-ci<? "" "")				=> #f)
  (check (%string/numbers-dictionary-ci<? "a" "")				=> #f)
  (check (%string/numbers-dictionary-ci<? "" "a")				=> #t)
  (check (%string/numbers-dictionary-ci<? "a" "a")				=> #f)
  (check (%string/numbers-dictionary-ci<? "1" "")				=> #f)
  (check (%string/numbers-dictionary-ci<? "" "1")				=> #t)
  (check (%string/numbers-dictionary-ci<? "1" "1")				=> #f)
  (check (%string/numbers-dictionary-ci<? "1" "2")				=> #t)
  (check (%string/numbers-dictionary-ci<? "2" "1")				=> #f)
  (check (%string/numbers-dictionary-ci<? "a" "ab")				=> #t)
  (check (%string/numbers-dictionary-ci<? "ab" "a")				=> #f)
  (check (%string/numbers-dictionary-ci<? "a" "a1")				=> #t)
  (check (%string/numbers-dictionary-ci<? "a1" "a")				=> #f)
  (check (%string/numbers-dictionary-ci<? "1" "1a")				=> #t)
  (check (%string/numbers-dictionary-ci<? "1a" "1")				=> #f)
  (check (%string/numbers-dictionary-ci<? "a" "A")				=> #f)
  (check (%string/numbers-dictionary-ci<? "A" "a")				=> #f)

  (check (%string/numbers-dictionary-ci<? "123" "45")				=> #f)
  (check (%string/numbers-dictionary-ci<? "45" "123")				=> #t)
  (check (%string/numbers-dictionary-ci<? "ciao3" "ciao10")			=> #t)
  (check (%string/numbers-dictionary-ci<? "ciao10" "ciao3")			=> #f)
  (check (%string/numbers-dictionary-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-dictionary-ci<? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%string/numbers-dictionary-ci<? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%string/numbers-dictionary-ci<? "foo12" "12foo")			=> #f)
  (check (%string/numbers-dictionary-ci<? "12foo" "foo12")			=> #t)
  (check (%string/numbers-dictionary-ci<? "12bar" "foobar")			=> #t)
  (check (%string/numbers-dictionary-ci<? "12.3" "12.3")			=> #f)
  (check (%string/numbers-dictionary-ci<? "12.3" "12.10")			=> #t)
  (check (%string/numbers-dictionary-ci<? "12.10" "12.3")			=> #f)
  (check (%string/numbers-dictionary-ci<? "12.3" "12,10")			=> #f)
  (check (%string/numbers-dictionary-ci<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary-ci<=? "" "")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "a" "")				=> #f)
  (check (%string/numbers-dictionary-ci<=? "" "a")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "a" "a")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "1" "")				=> #f)
  (check (%string/numbers-dictionary-ci<=? "" "1")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "1" "1")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "1" "2")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "2" "1")				=> #f)
  (check (%string/numbers-dictionary-ci<=? "a" "ab")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "ab" "a")				=> #f)
  (check (%string/numbers-dictionary-ci<=? "a" "a1")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "a1" "a")				=> #f)
  (check (%string/numbers-dictionary-ci<=? "1" "1a")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "1a" "1")				=> #f)
  (check (%string/numbers-dictionary-ci<=? "a" "A")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "A" "a")				=> #t)

  (check (%string/numbers-dictionary-ci<=? "123" "45")				=> #f)
  (check (%string/numbers-dictionary-ci<=? "45" "123")				=> #t)
  (check (%string/numbers-dictionary-ci<=? "ciao3" "ciao10")			=> #t)
  (check (%string/numbers-dictionary-ci<=? "ciao10" "ciao3")			=> #f)
  (check (%string/numbers-dictionary-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%string/numbers-dictionary-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%string/numbers-dictionary-ci<=? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%string/numbers-dictionary-ci<=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%string/numbers-dictionary-ci<=? "foo12" "12foo")			=> #f)
  (check (%string/numbers-dictionary-ci<=? "12foo" "foo12")			=> #t)
  (check (%string/numbers-dictionary-ci<=? "12bar" "foobar")			=> #t)
  (check (%string/numbers-dictionary-ci<=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-dictionary-ci<=? "12.3" "12.10")			=> #t)
  (check (%string/numbers-dictionary-ci<=? "12.10" "12.3")			=> #f)
  (check (%string/numbers-dictionary-ci<=? "12.3" "12,10")			=> #f)
  (check (%string/numbers-dictionary-ci<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary-ci>? "" "")				=> #f)
  (check (%string/numbers-dictionary-ci>? "a" "")				=> #t)
  (check (%string/numbers-dictionary-ci>? "" "a")				=> #f)
  (check (%string/numbers-dictionary-ci>? "a" "a")				=> #f)
  (check (%string/numbers-dictionary-ci>? "1" "")				=> #t)
  (check (%string/numbers-dictionary-ci>? "" "1")				=> #f)
  (check (%string/numbers-dictionary-ci>? "1" "1")				=> #f)
  (check (%string/numbers-dictionary-ci>? "1" "2")				=> #f)
  (check (%string/numbers-dictionary-ci>? "2" "1")				=> #t)
  (check (%string/numbers-dictionary-ci>? "a" "ab")				=> #f)
  (check (%string/numbers-dictionary-ci>? "ab" "a")				=> #t)
  (check (%string/numbers-dictionary-ci>? "a" "a1")				=> #f)
  (check (%string/numbers-dictionary-ci>? "a1" "a")				=> #t)
  (check (%string/numbers-dictionary-ci>? "1" "1a")				=> #f)
  (check (%string/numbers-dictionary-ci>? "1a" "1")				=> #t)
  (check (%string/numbers-dictionary-ci>? "a" "A")				=> #f)
  (check (%string/numbers-dictionary-ci>? "A" "a")				=> #f)

  (check (%string/numbers-dictionary-ci>? "123" "45")				=> #t)
  (check (%string/numbers-dictionary-ci>? "45" "123")				=> #f)
  (check (%string/numbers-dictionary-ci>? "ciao3" "ciao10")			=> #f)
  (check (%string/numbers-dictionary-ci>? "ciao10" "ciao3")			=> #t)
  (check (%string/numbers-dictionary-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-dictionary-ci>? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%string/numbers-dictionary-ci>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%string/numbers-dictionary-ci>? "foo12" "12foo")			=> #t)
  (check (%string/numbers-dictionary-ci>? "12foo" "foo12")			=> #f)
  (check (%string/numbers-dictionary-ci>? "12bar" "foobar")			=> #f)
  (check (%string/numbers-dictionary-ci>? "12.3" "12.3")			=> #f)
  (check (%string/numbers-dictionary-ci>? "12.3" "12.10")			=> #f)
  (check (%string/numbers-dictionary-ci>? "12.10" "12.3")			=> #t)
  (check (%string/numbers-dictionary-ci>? "12.3" "12,10")			=> #t)
  (check (%string/numbers-dictionary-ci>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%string/numbers-dictionary-ci>=? "" "")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "a" "")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "" "a")				=> #f)
  (check (%string/numbers-dictionary-ci>=? "a" "a")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "1" "")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "" "1")				=> #f)
  (check (%string/numbers-dictionary-ci>=? "1" "1")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "1" "2")				=> #f)
  (check (%string/numbers-dictionary-ci>=? "2" "1")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "a" "ab")				=> #f)
  (check (%string/numbers-dictionary-ci>=? "ab" "a")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "a" "a1")				=> #f)
  (check (%string/numbers-dictionary-ci>=? "a1" "a")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "1" "1a")				=> #f)
  (check (%string/numbers-dictionary-ci>=? "1a" "1")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "a" "A")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "A" "a")				=> #t)

  (check (%string/numbers-dictionary-ci>=? "123" "45")				=> #t)
  (check (%string/numbers-dictionary-ci>=? "45" "123")				=> #f)
  (check (%string/numbers-dictionary-ci>=? "ciao3" "ciao10")			=> #f)
  (check (%string/numbers-dictionary-ci>=? "ciao10" "ciao3")			=> #t)
  (check (%string/numbers-dictionary-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%string/numbers-dictionary-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%string/numbers-dictionary-ci>=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%string/numbers-dictionary-ci>=? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%string/numbers-dictionary-ci>=? "foo12" "12foo")			=> #t)
  (check (%string/numbers-dictionary-ci>=? "12foo" "foo12")			=> #f)
  (check (%string/numbers-dictionary-ci>=? "12bar" "foobar")			=> #f)
  (check (%string/numbers-dictionary-ci>=? "12.3" "12.3")			=> #t)
  (check (%string/numbers-dictionary-ci>=? "12.3" "12.10")			=> #f)
  (check (%string/numbers-dictionary-ci>=? "12.10" "12.3")			=> #t)
  (check (%string/numbers-dictionary-ci>=? "12.3" "12,10")			=> #t)
  (check (%string/numbers-dictionary-ci>=? "12,10" "12.3")			=> #f)

  #t)


(parameterise ((check-test-name 'mapping))

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%substring-map char-upcase str beg end))
    => "AAAA")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%substring-map char-upcase str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%substring-map! char-upcase str beg end)
	str)
    => "AAAA")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%substring-map! char-upcase str beg end)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str))
	     (result ""))
	(%substring-for-each
	 (lambda (ch)
	   (set! result
		 (string-append result
				(number->string (char->integer (char-upcase ch))))))
	 str beg end)
	result)
    => "65656565")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str))
	     (result ""))
	(%substring-for-each
	 (lambda (ch)
	   (set! result
		 (string-append result
				(number->string (char->integer (char-upcase ch))))))
	 str beg end)
	result)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str))
	     (result '()))
	(%substring-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '(3 2 1 0))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str))
	     (result '()))
	(%substring-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '())

  )


(parameterise ((check-test-name 'case))

  (check
      (let* ((str (string-copy "abcd")) (beg 0) (end (string-length str)))
	(%string-titlecase*! str beg end)
	str)
    => "Abcd")

  (check
      (let* ((str (string-copy "123abcd")) (beg 0) (end (string-length str)))
	(%string-titlecase*! str beg end)
	str)
    => "123Abcd")

  (check
      (let* ((str (string-copy "---abcd")) (beg 0) (end (string-length str)))
	(%string-titlecase*! str beg end)
	str)
    => "---Abcd")

  (check
      (let* ((str (string-copy "abcd efgh")) (beg 0) (end (string-length str)))
	(%string-titlecase*! str beg end)
	str)
    => "Abcd Efgh")

  )


(parameterise ((check-test-name 'folding))

  (check
      (let* ((str "abcd")
	     (beg 0)
	     (end (string-length str)))
	(%substring-fold-left cons '() str beg end))
    => '(#\d #\c #\b #\a))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%substring-fold-left cons '() str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd")
	     (beg 0)
	     (end (string-length str)))
	(%substring-fold-right cons '() str beg end))
    => '(#\a #\b #\c #\d))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%substring-fold-right cons '() str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (string-unfold null? car cdr '(#\a #\b #\c #\d))
    => "abcd")

  (check
      (string-unfold null? car cdr '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-unfold-right null? car cdr '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (string-unfold-right null? car cdr '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => "ABCD")

  (check
      (string-tabulate integer->char 0)
    => "")

  )


(parameterise ((check-test-name 'selecting))

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-take 2 str beg end))
    => "ab")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-take 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-take 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-take-right 2 str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-take-right 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-take-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-drop 2 str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-drop 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-drop 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-drop-right 2 str beg end))
    => "ab")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-drop-right 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-drop-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaabcd") (beg 0) (end (string-length str)))
	(%string-trim #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim #\a str beg end))
    => "")

  (check
      (let* ((str "aaabcd") (beg 0) (end (string-length str)))
	(%string-trim (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "AAAbcd") (beg 0) (end (string-length str)))
	(%string-trim char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bcdaaa") (beg 0) (end (string-length str)))
	(%string-trim-right #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-right #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-right #\a str beg end))
    => "")

  (check
      (let* ((str "cdbaaa") (beg 0) (end (string-length str)))
	(%string-trim-right (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "cdb") (beg 0) (end (string-length str)))
	(%string-trim-right (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-right (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "bcdAAA") (beg 0) (end (string-length str)))
	(%string-trim-right char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-right char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-right char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaabcdaaa") (beg 0) (end (string-length str)))
	(%string-trim-both #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-both #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-both #\a str beg end))
    => "")

  (check
      (let* ((str "aaabcdaa") (beg 0) (end (string-length str)))
	(%string-trim-both (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "bcdb") (beg 0) (end (string-length str)))
	(%string-trim-both (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-both (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "AAAbcdAAA") (beg 0) (end (string-length str)))
	(%string-trim-both char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-both char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-both char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 3 #\0 str beg end))
    => "abc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 5 #\0 str beg end))
    => "00abc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 2 #\0 str beg end))
    => "bc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 0 #\0 str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 3 #\0 str beg end))
    => "abc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 5 #\0 str beg end))
    => "abc00")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 2 #\0 str beg end))
    => "ab")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 0 #\0 str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (str2 (string-copy "12")))
	(%string-copy*! str2 0 str1 beg1 (+ 2 beg1))
	str2)
    => "ab")

  (check
      (let* ((str1 "abcd") (beg1 0) (str2 ""))
	(%string-copy*! str2 0 str1 beg1 beg1)
	str2)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str1 "abcd") (beg1 0) (str2 (string-copy "12")))
	  (%string-copy*! str2 3 str1 beg1 (+ 2 beg1))
	  str2))
    => #t)

  )


(parameterise ((check-test-name 'prefix))

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "efgabcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efgabcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abCd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "efgabCd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abCd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efgabCd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcd") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index #\b str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcd") (beg 0) (end (string-length str)))
	(%string-index char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "aBcd") (end (string-length str)))
	(%string-index char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right #\b str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index-right #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index-right (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcd") (beg 0) (end (string-length str)))
	(%string-index-right char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "aBcd") (end (string-length str)))
	(%string-index-right char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index-right char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bacd") (beg 0) (end (string-length str)))
	(%string-skip #\b str beg end))
    => 1)

  (check
      (let* ((str "bacd") (end (string-length str)))
	(%string-skip #\b str 1 end))
    => 1)

  (check
      (let* ((str "1111") (beg 0) (end (string-length str)))
	(%string-skip #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bacd") (beg 0) (end (string-length str)))
	(%string-skip (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "bacd") (end (string-length str)))
	(%string-skip (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "1010") (beg 0) (end (string-length str)))
	(%string-skip (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "Bacd") (beg 0) (end (string-length str)))
	(%string-skip char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "Bacd") (end (string-length str)))
	(%string-skip char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "ABCD") (beg 0) (end (string-length str)))
	(%string-skip char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdb") (beg 0) (end (string-length str)))
	(%string-skip-right #\b str beg end))
    => 2)

  (check
      (let* ((str "acdb") (end (string-length str)))
	(%string-skip-right #\b str 1 end))
    => 2)

  (check
      (let* ((str "1111") (beg 0) (end (string-length str)))
	(%string-skip-right #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdb") (beg 0) (end (string-length str)))
	(%string-skip-right (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str "acdb") (end (string-length str)))
	(%string-skip-right (char-set #\b #\B) str 1 end))
    => 2)

  (check
      (let* ((str "0101") (beg 0) (end (string-length str)))
	(%string-skip-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdB") (beg 0) (end (string-length str)))
	(%string-skip-right char-upper-case? str beg end))
    => 2)

  (check
      (let* ((str "acdB") (end (string-length str)))
	(%string-skip-right char-upper-case? str 1 end))
    => 2)

  (check
      (let* ((str "ABCD") (beg 0) (end (string-length str)))
	(%string-skip-right char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip-right char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-count #\b str beg end))
    => 2)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-count #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-count #\1 str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-count #\1 str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcBd") (beg 0) (end (string-length str)))
	(%string-count (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-count (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-count (char-set #\0 #\1) str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-count (char-set #\0 #\1) str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcAd") (beg 0) (end (string-length str)))
	(%string-count char-upper-case? str beg end))
    => 2)

  (check
      (let* ((str "aBcd") (end (string-length str)))
	(%string-count char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-count char-upper-case? str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-count char-upper-case? str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "hola") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "ciAO HELLO saLUT") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "HOLA") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  #t)


(parameterise ((check-test-name 'searching-and-replacing))

  (check	;no replacing because of no match
      (let ((src "abcdabcdabc")
	    (ptn "A")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 1
				    rep 0 5
				    +inf.0))
    => "abcdabcdabc")

  (check	;replacing 3 chars
      (let ((src "AbcdAbcdAbc")
	    (ptn "A")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 1
				    rep 0 5
				    +inf.0))
    => "12345bcd12345bcd12345bc")

  (check	;replacing 3 substrings
      (let ((src "bcABCbcABCbcdpqstABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 3
				    rep 0 5
				    +inf.0))
    => "bc12345bc12345bcdpqst12345bc")

;;; --------------------------------------------------------------------

  (check	;source string range
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(%string-search-and-replace src 2 (- (string-length src) 2)
				    ptn 0 3
				    rep 0 5
				    +inf.0))
    => "12345bcd12345bcd12345")

  (check	;pattern range
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "pqABCst")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 2 5
				    rep 0 5
				    +inf.0))
    => "bc12345bcd12345bcd12345bc")

  (check	;replacement range
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "pq12345st"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 3
				    rep 2 7
				    +inf.0))
    => "bc12345bcd12345bcd12345bc")

;;; --------------------------------------------------------------------

  (check	;zero replacements count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 3
				    rep 0 5
				    0))
    => "bcABCbcdABCbcdABCbc")

  (check	;one replacement count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 3
				    rep 0 5
				    1))
    => "bc12345bcdABCbcdABCbc")

  (check	;two replacements count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 3
				    rep 0 5
				    2))
    => "bc12345bcd12345bcdABCbc")

  (check	;three replacements count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(%string-search-and-replace src 0 (string-length src)
				    ptn 0 3
				    rep 0 5
				    3))
    => "bc12345bcd12345bcd12345bc")

  #t)


(parameterise ((check-test-name 'filtering))

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete #\b str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete #\0 str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-delete #\b str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete (char-set #\b #\B) str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete (char-set #\0 #\1) str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-delete (char-set #\b #\B) str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcBd") (beg 0) (end (string-length str)))
	(%string-delete char-upper-case? str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete char-upper-case? str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-delete char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter #\b str beg end))
    => "bb")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter #\0 str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-filter #\b str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter (char-set #\b #\B) str beg end))
    => "bb")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter (char-set #\0 #\1) str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-filter (char-set #\b #\B) str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcBd") (beg 0) (end (string-length str)))
	(%string-filter char-upper-case? str beg end))
    => "BB")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter char-upper-case? str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-filter char-upper-case? str beg end))
    => "")

  )


(parameterise ((check-test-name 'lists))

  (check
      (let* ((str (string-copy "abcd")) (beg 0) (end (string-length str)))
	(%string->list* str beg end))
    => '(#\a #\b #\c #\d))

  (check
      (let* ((str (string-copy "")) (beg 0) (end (string-length str)))
	(%string->list* str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-list->string '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (reverse-list->string '())
    => "")

  #f)

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'tokenize))

  (check
      (let* ((str "ciao hello salut") (beg 0) (end (string-length str)))
	(%string-tokenize (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u)
			  str beg end))
    => '("ciao" "hello" "salut"))

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-tokenize (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u)
			  str beg end))
    => '())

  (check
      (let* ((str "ciao hello salut") (beg 0) (end (string-length str)))
	(%string-tokenize (char-set) str beg end))
    => '())

  #f)

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (%string-join '("c" "i" "a" "o") "," 'infix)
    => "c,i,a,o")

  (check
      (%string-join '("c" "i" "a" "o") "," 'strict-infix)
    => "c,i,a,o")

  (check
      (%string-join '("c" "i" "a" "o") "," 'suffix)
    => "c,i,a,o,")

  (check
      (%string-join '("c" "i" "a" "o") "," 'prefix)
    => ",c,i,a,o")

;;; --------------------------------------------------------------------

  (check
      (%string-join '() "," 'infix)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(%string-join '() "," 'strict-infix))
    => #t)

  (check
      (%string-join '() "," 'suffix)
    => "")

  (check
      (%string-join '() "," 'prefix)
    => "")

;;; --------------------------------------------------------------------

  (check
      (%string-join '("c") "," 'infix)
    => "c")

  (check
      (%string-join '("c") "," 'strict-infix)
    => "c")

  (check
      (%string-join '("c") "," 'suffix)
    => "c,")

  (check
      (%string-join '("c") "," 'prefix)
    => ",c")

;;; --------------------------------------------------------------------

  (check
      (%string-join '("c" "i" "a" "o") "" 'infix)
    => "ciao")

  (check
      (%string-join '("c" "i" "a" "o") "" 'strict-infix)
    => "ciao")

  (check
      (%string-join '("c" "i" "a" "o") "" 'suffix)
    => "ciao")

  (check
      (%string-join '("c" "i" "a" "o") "" 'prefix)
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (%string-join '("c" "i" "a" "o") ",;;" 'infix)
    => "c,;;i,;;a,;;o")

  (check
      (%string-join '("c" "i" "a" "o") ",;;" 'strict-infix)
    => "c,;;i,;;a,;;o")

  (check
      (%string-join '("c" "i" "a" "o") ",;;" 'suffix)
    => "c,;;i,;;a,;;o,;;")

  (check
      (%string-join '("c" "i" "a" "o") ",;;" 'prefix)
    => ",;;c,;;i,;;a,;;o")

  )


(parameterise ((check-test-name 'replicating))

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str)))
	(%xsubstring 0 5 str beg end))
    => "ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str)))
	(%xsubstring 0 9 str beg end))
    => "ciao ciao")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str)))
	(%xsubstring -5 5 str beg end))
    => "ciao ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str)))
	(%xsubstring 2 4 str beg end))
    => "ao")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str)))
	(%xsubstring -3 7 str beg end))
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let ((str "ciao "))
	  (%xsubstring -3 7 str 3 3)))
    => #t)

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str "") (beg 0) (end (string-length str)))
	  (%xsubstring 0 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str))
	     (result (string-copy "01234")))
	(%string-xcopy! 0 5 result 0 (string-length result) str beg end)
	result)
    => "ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str))
	     (result (string-copy "012345678")))
	(%string-xcopy! 0 9 result 0 (string-length result) str beg end)
	result)
    => "ciao ciao")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str))
	     (result (string-copy "0123456789")))
	(%string-xcopy! -5 5 result 0 (string-length result) str beg end)
	result)
    => "ciao ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str))
	     (result (string-copy "01")))
	(%string-xcopy! 2 4 result 0 (string-length result) str beg end)
	result)
    => "ao")

  (check
      (let* ((str "ciao ") (beg 0) (end (string-length str))
	     (result (string-copy "0123456789")))
	(%string-xcopy! -3 7 result 0 (string-length result) str beg end)
	result)
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str "") (beg 0) (end (string-length str))
	     (result (string-copy "")))
	  (%string-xcopy! 0 5 result 0 (string-length result) str beg end)))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (string-copy "abcd")) (beg 0) (end (string-length str)))
	(%string-fill*! #\b str beg end)
	str)
    => "bbbb")

  (check
      (let* ((str (string-copy "accd")))
	(%string-fill*! #\b str 1 3)
	str)
    => "abbd")

  (check
      (let* ((str (string-copy "")))
	(%string-fill*! #\b str 0 0)
	str)
    => "")

  )


(parameterise ((check-test-name 'reverse))

  (check
      (let* ((str (string-copy "abcd")) (beg 0) (end (string-length str)))
	(%string-reverse str beg end))
    => "dcba")

  (check
      (let* ((str (string-copy "")) (beg 0) (end (string-length str)))
	(%string-reverse str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "abcd")) (beg 0) (end (string-length str)))
	(%string-reverse! str beg end)
	str)
    => "dcba")

  (check
      (let* ((str (string-copy "")) (beg 0) (end (string-length str)))
	(%string-reverse! str beg end)
	str)
    => "")

  )


(parameterise ((check-test-name 'concatenate))

  (check
      (string-concatenate '("ciao" " " "hello" " " "salut"))
    => "ciao hello salut")

  (check
      (string-concatenate '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (%string-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola" (string-length " hola"))
    => "salut hello ciao hola")

  (check
      (%string-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola" 3)
    => "salut hello ciao ho")

  (check
      (%string-concatenate-reverse '() "" 0)
    => "")


  )


(parameterise ((check-test-name 'replace))

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "1234") (beg2 0) (end2 (string-length str2)))
	(%string-replace str1 beg1 end1 str2 beg2 end2))
    => "1234")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "1234") (beg2 0) (end2 (string-length str2)))
	(%string-replace str1 2 2 str2 beg2 end2))
    => "ab1234cd")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-replace str1 2 2 str2 beg2 end2))
    => "abcd")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "1234") (beg2 0) (end2 (string-length str2)))
	(%string-replace str1 1 3 str2 beg2 end2))
    => "a1234d")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "1234") (beg2 0) (end2 (string-length str2)))
	(%string-replace str1 0 3 str2 beg2 end2))
    => "1234d")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "1234") (beg2 0) (end2 (string-length str2)))
	(%string-replace str1 1 4 str2 beg2 end2))
    => "a1234")


  )


;;;; done

(check-report)

;;; end of file
