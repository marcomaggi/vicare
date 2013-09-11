;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the bytevectors u8 library
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


(import (except (vicare)
		subbytevector-u8
		subbytevector-s8
		s8-list->bytevector
		bytevector->s8-list)
  (vicare containers bytevectors s8low)
  (vicare containers char-sets)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: bytevectors s8, low level\n")


;;;; helpers

(define S string->utf8)

(define (number->bytevector-s8 num)
  (string->utf8 (number->string num)))

(define (byte-upcase byte)
  (char->integer (char-upcase (integer->char byte))))

(define (byte-downcase byte)
  (char->integer (char-downcase (integer->char byte))))

(define (byte-titlecase byte)
  (char->integer (char-titlecase (integer->char byte))))

(define (byte-upper-case? byte)
  (char-upper-case? (integer->char byte)))

(define (byte-lower-case? byte)
  (char-lower-case? (integer->char byte)))


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
	(%bytevector-s8-every "invalid criterion" (S "abc") 0 2))
    => '%bytevector-s8-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every #\a str beg end))
    => #t)

  (check
      (let* ((str (S "aaaab"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every #\a str beg end))
    => #f)

  (check
      (let* ((str (S "aabaa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every #\a str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str (S "aaaab"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every (char-set #\a) str beg end))
    => #f)

  (check
      (let* ((str (S "aabaa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every (lambda (byte) (char-alphabetic? (integer->char byte))) str beg end))
    => #t)

  (check
      (let* ((str (S "aaaa2"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every (lambda (byte) (char-alphabetic? (integer->char byte))) str beg end))
    => #f)

  (check
      (let* ((str (S "aa2aa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-every (lambda (byte) (char-alphabetic? (integer->char byte))) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(%bytevector-s8-any "invalid criterion" (S "abc") 0 2))
    => '%bytevector-s8-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "ddadd"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any #\a str beg end))
    => #t)

  (check
      (let* ((str (S "dddda"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any #\a str beg end))
    => #t)

  (check
      (let* ((str (S "ddd"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any #\a str beg end))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "dddaddd"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str (S "ddda"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str (S "dddd"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "11a11"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any (lambda (byte) (char-alphabetic? (integer->char byte))) str beg end))
    => #t)

  (check
      (let* ((str (S "11111a"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any (lambda (byte) (char-alphabetic? (integer->char byte))) str beg end))
    => #t)

  (check
      (let* ((str (S "1111"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%bytevector-s8-any (lambda (byte) (char-alphabetic? (integer->char byte))) str beg end))
    => #f)

  #f)


(parameterise ((check-test-name 'comparison-lexicographic-case-sensitive))

  (check
      (let* ((str1 (S "abcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 (S "abcdef")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 (S "abcdA")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcdA")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2
				(lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 (S "abcdA")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcdB")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2
				(lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 (S "abcdB")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcdA")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare str1 beg1 end1 str2 beg2 end2
				(lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str (S "abcd"))
	  (beg1 0) (end1 (bytevector-length str))
	  (beg2 0) (end2 (bytevector-length str)))
     (%bytevector-s8= str beg1 end1 str beg2 end2)))

  (check-for-true
   (let* ((str1 (S "12abcd")) (beg1 2) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "a2cd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "a2cd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is less than STR2:
  ;;
  ;;* If the character at the  mismatch index from STR1 is less than the
  ;;  character at the mismatch index from STR2.
  ;;
  ;;* The bytevectors are equal up to the end of STR1 and STR2 is longer.

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abc"))  (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "ABCD")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "ABCD")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "A2CD")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8< str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is less than, or equal to, STR2:
  ;;
  ;;* If the bytevectors are equal.
  ;;
  ;;* If the character at the  mismatch index from STR1 is less than the
  ;;  character at the mismatch index from STR2.
  ;;
  ;;* The bytevectors are equal up to the end of STR1 and STR2 is longer.

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc"))  (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "a2cd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8<= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is greater than STR2:
  ;;
  ;;* If the  character at the mismatch index from  STR1 is greater than
  ;;  the character at the mismatch index from STR2.
  ;;
  ;;* The bytevectors are equal up to the end of STR2 and STR1 is longer.

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "ABcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "a2cd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is greater than, or equal to, STR2:
  ;;
  ;;* If the bytevectors are equal.
  ;;
  ;;* If the  character at the mismatch index from  STR1 is greater than
  ;;  the character at the mismatch index from STR2.
  ;;
  ;;* The bytevectors are equal up to the end of STR2 and STR1 is longer.

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "ABcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "ABCD")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "a2cd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8>= str1 beg1 end1 str2 beg2 end2)))

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-case-insensitive))

  (check
      (let* ((str1 (S "aBcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 (S "abcdA")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcda")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare-ci str1 beg1 end1 str2 beg2 end2
				   (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 (S "abcdA")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcdb")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare-ci str1 beg1 end1 str2 beg2 end2
				   (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 (S "abcdb")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcdA")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-compare-ci str1 beg1 end1 str2 beg2 end2
				   (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str (S "abcd"))
	  (beg1 0) (end1 (bytevector-length str))
	  (beg2 0) (end2 (bytevector-length str)))
     (%bytevector-s8-ci= str beg1 end1 str beg2 end2)))

  (check-for-true
   (let* ((str1 (S "12abcd")) (beg1 2) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "a2cd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "a2cd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "a2cd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci< str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "ABcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "a2cd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci<= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "ABcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "a2cd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abc")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "abc")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "ABcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 (S "a2cd")) (beg1 0) (end1 (bytevector-length str1))
	  (str2 (S "abcd")) (beg2 0) (end2 (bytevector-length str2)))
     (%bytevector-s8-ci>= str1 beg1 end1 str2 beg2 end2)))

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-sensitive))

  (check (%bytevector-s8-dictionary=? '#vu8() '#vu8())			=> #t)
  (check (%bytevector-s8-dictionary=? (S "a") '#vu8())			=> #f)
  (check (%bytevector-s8-dictionary=? '#vu8() (S "a"))			=> #f)
  (check (%bytevector-s8-dictionary=? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8-dictionary=? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ciao1") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ciao1"))		=> #f)

  (check (%bytevector-s8-dictionary=? (S "ci ao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ci ao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ci\tao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ci\tao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ci\nao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ci\nao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ci\vao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ci\tao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ci\fao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ci\fao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ci\rao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary=? (S "ciao") (S "ci\rao"))		=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary<? '#vu8() '#vu8())			=> #f)
  (check (%bytevector-s8-dictionary<? (S "a") '#vu8())			=> #f)
  (check (%bytevector-s8-dictionary<? '#vu8() (S "a"))			=> #t)
  (check (%bytevector-s8-dictionary<? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8-dictionary<? (S "a") (S "ab"))			=> #t)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao1") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ciao1"))		=> #t)

  (check (%bytevector-s8-dictionary<? (S "ci ao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ci ao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ci\tao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ci\tao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ci\nao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ci\nao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ci\vao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ci\tao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ci\fao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ci\fao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ci\rao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<? (S "ciao") (S "ci\rao"))		=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary<=? '#vu8() '#vu8())			=> #t)
  (check (%bytevector-s8-dictionary<=? (S "a") '#vu8())			=> #f)
  (check (%bytevector-s8-dictionary<=? '#vu8() (S "a"))			=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ab") (S "a"))		=> #f)
  (check (%bytevector-s8-dictionary<=? (S "a") (S "ab"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao1") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ciao1"))		=> #t)

  (check (%bytevector-s8-dictionary<=? (S "ci ao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ci ao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ci\tao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ci\tao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ci\nao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ci\nao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ci\vao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ci\tao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ci\fao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ci\fao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ci\rao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary<=? (S "ciao") (S "ci\rao"))		=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary>? '#vu8() '#vu8())			=> #f)
  (check (%bytevector-s8-dictionary>? (S "a") '#vu8())			=> #t)
  (check (%bytevector-s8-dictionary>? '#vu8() (S "a"))			=> #f)
  (check (%bytevector-s8-dictionary>? (S "ab") (S "a"))			=> #t)
  (check (%bytevector-s8-dictionary>? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao1") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ciao1"))		=> #f)

  (check (%bytevector-s8-dictionary>? (S "ci ao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ci ao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ci\tao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ci\tao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ci\nao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ci\nao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ci\vao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ci\tao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ci\fao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ci\fao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ci\rao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary>? (S "ciao") (S "ci\rao"))		=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary>=? '#vu8() '#vu8())			=> #t)
  (check (%bytevector-s8-dictionary>=? (S "a") '#vu8())			=> #t)
  (check (%bytevector-s8-dictionary>=? '#vu8() (S "a"))			=> #f)
  (check (%bytevector-s8-dictionary>=? (S "ab") (S "a"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "a") (S "ab"))		=> #f)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao1") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ciao1"))		=> #f)

  (check (%bytevector-s8-dictionary>=? (S "ci ao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ci ao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ci\tao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ci\tao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ci\nao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ci\nao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ci\vao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ci\tao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ci\fao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ci\fao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ci\rao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary>=? (S "ciao") (S "ci\rao"))		=> #t)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-insensitive))

  (check (%bytevector-s8-dictionary-ci=? '#vu8() '#vu8())		=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "a") '#vu8())		=> #f)
  (check (%bytevector-s8-dictionary-ci=? '#vu8() (S "a"))		=> #f)
  (check (%bytevector-s8-dictionary-ci=? (S "ab") (S "a"))		=> #f)
  (check (%bytevector-s8-dictionary-ci=? (S "a") (S "ab"))		=> #f)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao1") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ciao1"))	=> #f)
  (check (%bytevector-s8-dictionary-ci=? (S "CIAO") (S "ciao"))		=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "CIAO1") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci=? (S "CIAO") (S "ciao1"))	=> #f)

  (check (%bytevector-s8-dictionary-ci=? (S "ci ao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ci ao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ci\tao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ci\tao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ci\nao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ci\nao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ci\vao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ci\tao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ci\fao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ci\fao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ci\rao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci=? (S "ciao") (S "ci\rao"))	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary-ci<? '#vu8() '#vu8())		=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "a") '#vu8())		=> #f)
  (check (%bytevector-s8-dictionary-ci<? '#vu8() (S "a"))		=> #t)
  (check (%bytevector-s8-dictionary-ci<? (S "ab") (S "a"))		=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "a") (S "ab"))		=> #t)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao1") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao") (S "ciao1"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<? (S "CIAO") (S "ciao"))		=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "CIAO1") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "CIAO") (S "ciao1"))	=> #t)

  (check (%bytevector-s8-dictionary-ci<? (S "ci ao")  (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao")   (S "ci ao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ci\tao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao")   (S "ci\tao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ci\nao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao")   (S "ci\nao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ci\vao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao")   (S "ci\tao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ci\fao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao")   (S "ci\fao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ci\rao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<? (S "ciao")   (S "ci\rao"))	=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary-ci<=? '#vu8() '#vu8())		=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "a") '#vu8())		=> #f)
  (check (%bytevector-s8-dictionary-ci<=? '#vu8() (S "a"))		=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ab") (S "a"))		=> #f)
  (check (%bytevector-s8-dictionary-ci<=? (S "a") (S "ab"))		=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao1") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao") (S "ciao1"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "CIAO") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "CIAO1") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci<=? (S "CIAO") (S "ciao1"))	=> #t)

  (check (%bytevector-s8-dictionary-ci<=? (S "ci ao")  (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao")   (S "ci ao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ci\tao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao")   (S "ci\tao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ci\nao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao")   (S "ci\nao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ci\vao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao")   (S "ci\tao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ci\fao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao")   (S "ci\fao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ci\rao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci<=? (S "ciao")   (S "ci\rao"))	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary-ci>? '#vu8() '#vu8())		=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "a") '#vu8())		=> #t)
  (check (%bytevector-s8-dictionary-ci>? '#vu8()     (S "a"))		=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ab")    (S "a"))		=> #t)
  (check (%bytevector-s8-dictionary-ci>? (S "a")     (S "ab"))		=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao")  (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao1") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao")  (S "ciao1"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "CIAO")  (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "CIAO1") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>? (S "CIAO")  (S "ciao1"))	=> #f)

  (check (%bytevector-s8-dictionary-ci>? (S "ci ao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao") (S "ci ao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ci\tao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao") (S "ci\tao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ci\nao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao") (S "ci\nao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ci\vao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao") (S "ci\tao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ci\fao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao") (S "ci\fao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ci\rao") (S "ciao"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>? (S "ciao") (S "ci\rao"))	=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8-dictionary-ci>=? '#vu8() '#vu8())		=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "a") '#vu8())		=> #t)
  (check (%bytevector-s8-dictionary-ci>=? '#vu8() (S "a"))		=> #f)
  (check (%bytevector-s8-dictionary-ci>=? (S "ab") (S "a"))		=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "a") (S "ab"))		=> #f)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao1") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao") (S "ciao1"))	=> #f)
  (check (%bytevector-s8-dictionary-ci>=? (S "CIAO") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "CIAO1") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "CIAO") (S "ciao1"))	=> #f)

  (check (%bytevector-s8-dictionary-ci>=? (S "ci ao")  (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao")   (S "ci ao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ci\tao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao")   (S "ci\tao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ci\nao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao")   (S "ci\nao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ci\vao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao")   (S "ci\tao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ci\fao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao")   (S "ci\fao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ci\rao") (S "ciao"))	=> #t)
  (check (%bytevector-s8-dictionary-ci>=? (S "ciao")   (S "ci\rao"))	=> #t)

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-bytevector-s8/number-case-sensitive))

  (check (%bytevector-s8/numbers=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers=? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "a") (S "ab"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "ab") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "a") (S "a1"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "a1") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "1") (S "1a"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "1a") (S "1"))				=> #f)

  (check (%bytevector-s8/numbers=? (S "123") (S "45"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "45") (S "123"))				=> #f)
  (check (%bytevector-s8/numbers=? (S "ciao3") (S "ciao10"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "ciao10") (S "ciao3"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers=? (S "foo4bar3zab") (S "foo4bar10"))		=> #f)
  (check (%bytevector-s8/numbers=? (S "foo4bar10") (S "foo4bar3zab"))		=> #f)
  (check (%bytevector-s8/numbers=? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "12bar") (S "foobar"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers=? (S "12,10") (S "12.3"))			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers<>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers<>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers<>? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers<>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers<>? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers<>? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "a") (S "ab"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "ab") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "a") (S "a1"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "a1") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "1") (S "1a"))				=> #t)
  (check (%bytevector-s8/numbers<>? (S "1a") (S "1"))				=> #t)

  (check (%bytevector-s8/numbers<>? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "ciao3") (S "ciao10"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "ciao10") (S "ciao3"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers<>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers<>? (S "foo4bar3zab") (S "foo4bar10"))		=> #t)
  (check (%bytevector-s8/numbers<>? (S "foo4bar10") (S "foo4bar3zab"))		=> #t)
  (check (%bytevector-s8/numbers<>? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "12bar") (S "foobar"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers<>? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers<>? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers<? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers<? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers<? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers<? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers<? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers<? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers<? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers<? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers<? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers<? (S "a") (S "ab"))				=> #t)
  (check (%bytevector-s8/numbers<? (S "ab") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers<? (S "a") (S "a1"))				=> #t)
  (check (%bytevector-s8/numbers<? (S "a1") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers<? (S "1") (S "1a"))				=> #t)
  (check (%bytevector-s8/numbers<? (S "1a") (S "1"))				=> #f)

  (check (%bytevector-s8/numbers<? (S "123") (S "45"))				=> #f)
  (check (%bytevector-s8/numbers<? (S "45") (S "123"))				=> #t)
  (check (%bytevector-s8/numbers<? (S "ciao3") (S "ciao10"))			=> #t)
  (check (%bytevector-s8/numbers<? (S "ciao10") (S "ciao3"))			=> #f)
  (check (%bytevector-s8/numbers<? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers<? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers<? (S "foo4bar3zab") (S "foo4bar10"))		=> #t)
  (check (%bytevector-s8/numbers<? (S "foo4bar10") (S "foo4bar3zab"))		=> #f)
  (check (%bytevector-s8/numbers<? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers<? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers<? (S "12bar") (S "foobar"))			=> #t)
  (check (%bytevector-s8/numbers<? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers<? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers<? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers<? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers<? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers<=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers<=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers<=? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers<=? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers<=? (S "a") (S "ab"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "ab") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers<=? (S "a") (S "a1"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "a1") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers<=? (S "1") (S "1a"))				=> #t)
  (check (%bytevector-s8/numbers<=? (S "1a") (S "1"))				=> #f)

  (check (%bytevector-s8/numbers<=? (S "123") (S "45"))			=> #f)
  (check (%bytevector-s8/numbers<=? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers<=? (S "ciao3") (S "ciao10"))			=> #t)
  (check (%bytevector-s8/numbers<=? (S "ciao10") (S "ciao3"))			=> #f)
  (check (%bytevector-s8/numbers<=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers<=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers<=? (S "foo4bar3zab") (S "foo4bar10"))		=> #t)
  (check (%bytevector-s8/numbers<=? (S "foo4bar10") (S "foo4bar3zab"))		=> #f)
  (check (%bytevector-s8/numbers<=? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers<=? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers<=? (S "12bar") (S "foobar"))			=> #t)
  (check (%bytevector-s8/numbers<=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers<=? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers<=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers<=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers<=? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers>? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers>? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers>? (S "a") (S "ab"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "ab") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers>? (S "a") (S "a1"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "a1") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers>? (S "1") (S "1a"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "1a") (S "1"))				=> #t)

  (check (%bytevector-s8/numbers>? (S "123") (S "45"))				=> #t)
  (check (%bytevector-s8/numbers>? (S "45") (S "123"))				=> #f)
  (check (%bytevector-s8/numbers>? (S "ciao3") (S "ciao10"))			=> #f)
  (check (%bytevector-s8/numbers>? (S "ciao10") (S "ciao3"))			=> #t)
  (check (%bytevector-s8/numbers>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers>? (S "foo4bar3zab") (S "foo4bar10"))		=> #f)
  (check (%bytevector-s8/numbers>? (S "foo4bar10") (S "foo4bar3zab"))		=> #t)
  (check (%bytevector-s8/numbers>? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers>? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers>? (S "12bar") (S "foobar"))			=> #f)
  (check (%bytevector-s8/numbers>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers>? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers>? (S "12,10") (S "12.3"))			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers>=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers>=? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers>=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers>=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers>=? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers>=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers>=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers>=? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers>=? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers>=? (S "a") (S "ab"))				=> #f)
  (check (%bytevector-s8/numbers>=? (S "ab") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers>=? (S "a") (S "a1"))				=> #f)
  (check (%bytevector-s8/numbers>=? (S "a1") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers>=? (S "1") (S "1a"))				=> #f)
  (check (%bytevector-s8/numbers>=? (S "1a") (S "1"))				=> #t)

  (check (%bytevector-s8/numbers>=? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers>=? (S "45") (S "123"))			=> #f)
  (check (%bytevector-s8/numbers>=? (S "ciao3") (S "ciao10"))			=> #f)
  (check (%bytevector-s8/numbers>=? (S "ciao10") (S "ciao3"))			=> #t)
  (check (%bytevector-s8/numbers>=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers>=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers>=? (S "foo4bar3zab") (S "foo4bar10"))		=> #f)
  (check (%bytevector-s8/numbers>=? (S "foo4bar10") (S "foo4bar3zab"))		=> #t)
  (check (%bytevector-s8/numbers>=? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers>=? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers>=? (S "12bar") (S "foobar"))			=> #f)
  (check (%bytevector-s8/numbers>=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers>=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers>=? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers>=? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers>=? (S "12,10") (S "12.3"))			=> #f)

;;; --------------------------------------------------------------------

  (check
      (list-sort %bytevector-s8/numbers<? `(,(S "foo123") ,(S "foo42") ,(S "foo7")))
    => `(,(S "foo7") ,(S "foo42") ,(S "foo123")))

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-bytevector-s8/number-case-insensitive))

  (check (%bytevector-s8/numbers-ci=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-ci=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-ci=? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "a") (S "a1"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "a1") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "1") (S "1a"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "1a") (S "1"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "a") (S "A"))				=> #t)
  (check (%bytevector-s8/numbers-ci=? (S "A") (S "a"))				=> #t)

  (check (%bytevector-s8/numbers-ci=? (S "123") (S "45"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "45") (S "123"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "ciao3") (S "ciao10"))		=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "ciao10") (S "ciao3"))		=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "foo4bar3zab") (S "foo4bar10"))	=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "foo4bar10") (S "foo4bar3zab"))	=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "12bar") (S "foobar"))		=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-ci=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-ci=? (S "12,10") (S "12.3"))			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-ci<>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci<>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci<>? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "a") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci<>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci<>? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "1") (S "1"))			=> #f)
  (check (%bytevector-s8/numbers-ci<>? (S "1") (S "2"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "2") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "a") (S "ab"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "ab") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "a") (S "a1"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "a1") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "1") (S "1a"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "1a") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "A") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci<>? (S "a") (S "A"))			=> #f)

  (check (%bytevector-s8/numbers-ci<>? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "ciao3") (S "ciao10"))		=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "ciao10") (S "ciao3"))		=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "foo4bar3zab") (S "foo4bar10"))	=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "foo4bar10") (S "foo4bar3zab"))	=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "foo12") (S "12foo"))		=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "12foo") (S "foo12"))		=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "12bar") (S "foobar"))		=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-ci<>? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-ci<>? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-ci<? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci<? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci<? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "a") (S "ab"))			=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "a") (S "a1"))			=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "a1") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "1") (S "1a"))			=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "1a") (S "1"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "a") (S "A"))				=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "A") (S "a"))				=> #f)

  (check (%bytevector-s8/numbers-ci<? (S "123") (S "45"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "ciao3") (S "ciao10"))		=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "ciao10") (S "ciao3"))		=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "foo4bar3zab") (S "foo4bar10"))	=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "foo4bar10") (S "foo4bar3zab"))	=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "12bar") (S "foobar"))		=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-ci<? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-ci<? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-ci<=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci<=? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "a") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci<=? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "1") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "1") (S "2"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "2") (S "1"))			=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "a") (S "ab"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "a") (S "a1"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "a1") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "1") (S "1a"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "1a") (S "1"))			=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "a") (S "A"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "A") (S "a"))			=> #t)

  (check (%bytevector-s8/numbers-ci<=? (S "123") (S "45"))			=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "ciao3") (S "ciao10"))		=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "ciao10") (S "ciao3"))		=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "foo4bar3zab") (S "foo4bar10"))	=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "foo4bar10") (S "foo4bar3zab"))	=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "foo12") (S "12foo"))		=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "12foo") (S "foo12"))		=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "12bar") (S "foobar"))		=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-ci<=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-ci<=? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-ci>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci>? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci>? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "ab") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "a") (S "a1"))			=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "a1") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "1") (S "1a"))			=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "1a") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "a") (S "A"))				=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "A") (S "a"))				=> #f)

  (check (%bytevector-s8/numbers-ci>? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "45") (S "123"))			=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "ciao3") (S "ciao10"))		=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "ciao10") (S "ciao3"))		=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "foo4bar3zab") (S "foo4bar10"))	=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "foo4bar10") (S "foo4bar3zab"))	=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "12bar") (S "foobar"))		=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-ci>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-ci>? (S "12,10") (S "12.3"))			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-ci>=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci>=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "a") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-ci>=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "1") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "1") (S "2"))			=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "2") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "ab") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "a") (S "a1"))			=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "a1") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "1") (S "1a"))			=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "1a") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "a") (S "A"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "A") (S "a"))			=> #t)

  (check (%bytevector-s8/numbers-ci>=? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "45") (S "123"))			=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "ciao3") (S "ciao10"))		=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "ciao10") (S "ciao3"))		=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "foo4bar3zab") (S "foo4bar10"))	=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "foo4bar10") (S "foo4bar3zab"))	=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "foo12") (S "12foo"))		=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "12foo") (S "foo12"))		=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "12bar") (S "foobar"))		=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-ci>=? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-ci>=? (S "12,10") (S "12.3"))			=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-bytevector-s8/number-case-sensitive))

  (check (%bytevector-s8/numbers-dictionary=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary=? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "a") (S "a1"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "a1") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "1") (S "1a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "1a") (S "1"))			=> #f)

  (check (%bytevector-s8/numbers-dictionary=? (S "123") (S "45"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "45") (S "123"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "ciao3") (S "ciao10"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "ciao10") (S "ciao3"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "foo4bar3zab") (S "foo4bar10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "foo4bar10") (S "foo4bar3zab"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "12bar") (S "foobar"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "12,10") (S "12.3"))			=> #f)

  (check (%bytevector-s8/numbers-dictionary=? (S "fo o4b\tar3\nza\rb10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary=? (S "foo4bar3zab2") (S "fo o4b\tar3\nza\rb10"))	=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary<>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary<>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "a") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "1") (S "1"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<>? (S "1") (S "2"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "2") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "a") (S "ab"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "ab") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "a") (S "a1"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "a1") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "1") (S "1a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "1a") (S "1"))			=> #t)

  (check (%bytevector-s8/numbers-dictionary<>? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "ciao3") (S "ciao10"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "ciao10") (S "ciao3"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "foo4bar3zab") (S "foo4bar10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "foo4bar10") (S "foo4bar3zab"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "foo12") (S "12foo"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "12foo") (S "foo12"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "12bar") (S "foobar"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<>? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "12,10") (S "12.3"))			=> #t)

  (check (%bytevector-s8/numbers-dictionary<>? (S "fo o4b\tar3\nza\rb10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<>? (S "foo4bar3zab2") (S "fo o4b\tar3\nza\rb10"))	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary<? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary<? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary<? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "a") (S "ab"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "a") (S "a1"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "a1") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "1") (S "1a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "1a") (S "1"))			=> #f)

  (check (%bytevector-s8/numbers-dictionary<? (S "123") (S "45"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "ciao3") (S "ciao10"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "ciao10") (S "ciao3"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "foo4bar3zab") (S "foo4bar10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "foo4bar10") (S "foo4bar3zab"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "12bar") (S "foobar"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "12,10") (S "12.3"))			=> #t)

  (check (%bytevector-s8/numbers-dictionary<? (S "fo o4b\tar3\nza\rb10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary<? (S "foo4bar3zab2") (S "fo o4b\tar3\nza\rb10"))	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary<=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "a") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "1") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "1") (S "2"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "2") (S "1"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "a") (S "ab"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "ab") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "a") (S "a1"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "a1") (S "a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "1") (S "1a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "1a") (S "1"))			=> #f)

  (check (%bytevector-s8/numbers-dictionary<=? (S "123") (S "45"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "45") (S "123"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "ciao3") (S "ciao10"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "ciao10") (S "ciao3"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "foo4bar3zab") (S "foo4bar10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "foo4bar10") (S "foo4bar3zab"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "foo12") (S "12foo"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "12foo") (S "foo12"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "12bar") (S "foobar"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary<=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "12,10") (S "12.3"))			=> #t)

  (check (%bytevector-s8/numbers-dictionary<=? (S "fo o4b\tar3\nza\rb10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary<=? (S "foo4bar3zab2") (S "fo o4b\tar3\nza\rb10"))	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary>? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary>? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "ab") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "a") (S "a1"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "a1") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "1") (S "1a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "1a") (S "1"))			=> #t)

  (check (%bytevector-s8/numbers-dictionary>? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "45") (S "123"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "ciao3") (S "ciao10"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "ciao10") (S "ciao3"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "foo4bar3zab") (S "foo4bar10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "foo4bar10") (S "foo4bar3zab"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "12bar") (S "foobar"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "12,10") (S "12.3"))			=> #f)

  (check (%bytevector-s8/numbers-dictionary>? (S "fo o4b\tar3\nza\rb10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary>? (S "foo4bar3zab2") (S "fo o4b\tar3\nza\rb10"))	=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary>=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "a") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "1") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "1") (S "2"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "2") (S "1"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "a") (S "ab"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "ab") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "a") (S "a1"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "a1") (S "a"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "1") (S "1a"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "1a") (S "1"))			=> #t)

  (check (%bytevector-s8/numbers-dictionary>=? (S "123") (S "45"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "45") (S "123"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "ciao3") (S "ciao10"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "ciao10") (S "ciao3"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "foo4bar3zab") (S "foo4bar10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "foo4bar10") (S "foo4bar3zab"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "foo12") (S "12foo"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "12foo") (S "foo12"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "12bar") (S "foobar"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary>=? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "12,10") (S "12.3"))			=> #f)

  (check (%bytevector-s8/numbers-dictionary>=? (S "fo o4b\tar3\nza\rb10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary>=? (S "foo4bar3zab2") (S "fo o4b\tar3\nza\rb10"))	=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-bytevector-s8/number-case-insensitive))

  (check (%bytevector-s8/numbers-dictionary-ci=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "a") (S "ab"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "ab") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "a") (S "a1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "a1") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "1") (S "1a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "1a") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "a") (S "A"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "A") (S "a"))				=> #t)

  (check (%bytevector-s8/numbers-dictionary-ci=? (S "123") (S "45"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "45") (S "123"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "ciao3") (S "ciao10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "ciao10") (S "ciao3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "foo4bar3zab") (S "foo4bar10"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "foo4bar10") (S "foo4bar3zab"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "12bar") (S "foobar"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci=? (S "12,10") (S "12.3"))			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary-ci<>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "a") (S "ab"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "ab") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "a") (S "a1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "a1") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "1") (S "1a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "1a") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "A") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "a") (S "A"))				=> #f)

  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "123") (S "45"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "45") (S "123"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "ciao3") (S "ciao10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "ciao10") (S "ciao3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "foo4bar3zab") (S "foo4bar10"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "foo4bar10") (S "foo4bar3zab"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "12bar") (S "foobar"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<>? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary-ci<? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "a") (S "ab"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "ab") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "a") (S "a1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "a1") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "1") (S "1a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "1a") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "a") (S "A"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "A") (S "a"))				=> #f)

  (check (%bytevector-s8/numbers-dictionary-ci<? (S "123") (S "45"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "45") (S "123"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "ciao3") (S "ciao10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "ciao10") (S "ciao3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "foo4bar3zab") (S "foo4bar10"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "foo4bar10") (S "foo4bar3zab"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "12bar") (S "foobar"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary-ci<=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "a") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? '#vu8() (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "1") '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? '#vu8() (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "1") (S "2"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "2") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "a") (S "ab"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "ab") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "a") (S "a1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "a1") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "1") (S "1a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "1a") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "a") (S "A"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "A") (S "a"))				=> #t)

  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "123") (S "45"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "45") (S "123"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "ciao3") (S "ciao10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "ciao10") (S "ciao3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "foo4bar3zab") (S "foo4bar10"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "foo4bar10") (S "foo4bar3zab"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "foo12") (S "12foo"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "12foo") (S "foo12"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "12bar") (S "foobar"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "12.3") (S "12.10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "12.10") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "12.3") (S "12,10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci<=? (S "12,10") (S "12.3"))			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary-ci>? '#vu8() '#vu8())				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "a") (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "1") (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "a") (S "ab"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "ab") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "a") (S "a1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "a1") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "1") (S "1a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "1a") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "a") (S "A"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "A") (S "a"))				=> #f)

  (check (%bytevector-s8/numbers-dictionary-ci>? (S "123") (S "45"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "45") (S "123"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "ciao3") (S "ciao10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "ciao10") (S "ciao3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "foo4bar3zab") (S "foo4bar10"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "foo4bar10") (S "foo4bar3zab"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "12bar") (S "foobar"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "12.3") (S "12.3"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>? (S "12,10") (S "12.3"))			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-s8/numbers-dictionary-ci>=? '#vu8() '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "a") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? '#vu8() (S "a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "a") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "1") '#vu8())				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? '#vu8() (S "1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "1") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "1") (S "2"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "2") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "a") (S "ab"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "ab") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "a") (S "a1"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "a1") (S "a"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "1") (S "1a"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "1a") (S "1"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "a") (S "A"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "A") (S "a"))				=> #t)

  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "123") (S "45"))				=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "45") (S "123"))				=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "ciao3") (S "ciao10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "ciao10") (S "ciao3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "foo4bar3zab10") (S "foo4bar3zab2"))	=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "foo4bar3zab2") (S "foo4bar3zab10"))	=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "foo4bar3zab") (S "foo4bar10"))		=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "foo4bar10") (S "foo4bar3zab"))		=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "foo12") (S "12foo"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "12foo") (S "foo12"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "12bar") (S "foobar"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "12.3") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "12.3") (S "12.10"))			=> #f)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "12.10") (S "12.3"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "12.3") (S "12,10"))			=> #t)
  (check (%bytevector-s8/numbers-dictionary-ci>=? (S "12,10") (S "12.3"))			=> #f)

  #t)


(parameterise ((check-test-name 'mapping))

  (check
      (let* ((str (S "aaaa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-map byte-upcase str beg end))
    => (S "AAAA"))

  (check
      (let* ((str '#vu8())
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-map byte-upcase str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-map! byte-upcase str beg end)
	str)
    => (S "AAAA"))

  (check
      (let* ((str '#vu8())
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-map! byte-upcase str beg end)
	str)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa"))
	     (beg 0)
	     (end (bytevector-length str))
	     (result '#vu8()))
	(%subbytevector-s8-for-each
	 (lambda (ch)
	   (set! result
		 (bytevector-s8-append result
				       (number->bytevector-s8 (byte-upcase ch)))))
	 str beg end)
	result)
    => (S "65656565"))

  (check
      (let* ((str '#vu8())
	     (beg 0)
	     (end (bytevector-length str))
	     (result '#vu8()))
	(%subbytevector-s8-for-each
	 (lambda (ch)
	   (set! result
		 (bytevector-s8-append result
				       (number->bytevector-s8 (byte-upcase ch)))))
	 str beg end)
	result)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaaa"))
	     (beg 0)
	     (end (bytevector-length str))
	     (result '()))
	(%subbytevector-s8-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '(3 2 1 0))

  (check
      (let* ((str '#vu8())
	     (beg 0)
	     (end (bytevector-length str))
	     (result '()))
	(%subbytevector-s8-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '())

  #f)


(parameterise ((check-test-name 'case))

  (check
      (let* ((str (bytevector-copy (S "abcd"))) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-titlecase*! str beg end)
	str)
    => (S "Abcd"))

  (check
      (let* ((str (bytevector-copy (S "123abcd"))) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-titlecase*! str beg end)
	str)
    => (S "123Abcd"))

  (check
      (let* ((str (bytevector-copy (S "---abcd"))) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-titlecase*! str beg end)
	str)
    => (S "---Abcd"))

  (check
      (let* ((str (bytevector-copy (S "abcd efgh"))) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-titlecase*! str beg end)
	str)
    => (S "Abcd Efgh"))

  #f)


(parameterise ((check-test-name 'folding))

  (check
      (let* ((str (S "abcd"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-fold-left cons '() str beg end))
    => (map char->integer '(#\d #\c #\b #\a)))

  (check
      (let* ((str '#vu8())
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-fold-left cons '() str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcd"))
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-fold-right cons '() str beg end))
    => (map char->integer '(#\a #\b #\c #\d)))

  (check
      (let* ((str '#vu8())
	     (beg 0)
	     (end (bytevector-length str)))
	(%subbytevector-s8-fold-right cons '() str beg end))
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

;;; --------------------------------------------------------------------

  (check
      (bytevector-s8-tabulate (lambda (idx) (+ 65 idx)) 4)
    => (S "ABCD"))

  (check
      (bytevector-s8-tabulate integer->char 0)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'selecting))

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-take 2 str beg end))
    => (S "ab"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-take 0 str beg end))
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	  (%bytevector-s8-take 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-take-right 2 str beg end))
    => (S "cd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-take-right 0 str beg end))
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	  (%bytevector-s8-take-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-drop 2 str beg end))
    => (S "cd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-drop 0 str beg end))
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	  (%bytevector-s8-drop 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-drop-right 2 str beg end))
    => (S "ab"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-drop-right 0 str beg end))
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	  (%bytevector-s8-drop-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaabcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim #\a str beg end))
    => (S "bcd"))

  (check
      (let* ((str (S "bcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim #\a str beg end))
    => (S "bcd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim #\a str beg end))
    => '#vu8())

  (check
      (let* ((str (S "aaabcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim (char-set #\a #\b) str beg end))
    => (S "cd"))

  (check
      (let* ((str (S "bcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim (char-set #\a #\b) str beg end))
    => (S "cd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim (char-set #\a #\b) str beg end))
    => '#vu8())

  (check
      (let* ((str (S "AAAbcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim byte-upper-case? str beg end))
    => (S "bcd"))

  (check
      (let* ((str (S "bcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim byte-upper-case? str beg end))
    => (S "bcd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim byte-upper-case? str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "bcdaaa")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right 97 str beg end))
    => (S "bcd"))

  (check
      (let* ((str (S "bcdaaa")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right #\a str beg end))
    => (S "bcd"))

  (check
      (let* ((str (S "bcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right #\a str beg end))
    => (S "bcd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right #\a str beg end))
    => '#vu8())

  (check
      (let* ((str (S "cdbaaa")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right (char-set #\a #\b) str beg end))
    => (S "cd"))

  (check
      (let* ((str (S "cdb")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right (char-set #\a #\b) str beg end))
    => (S "cd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right (char-set #\a #\b) str beg end))
    => '#vu8())

  (check
      (let* ((str (S "bcdAAA")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right byte-upper-case? str beg end))
    => (S "bcd"))

  (check
      (let* ((str (S "bcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right byte-upper-case? str beg end))
    => (S "bcd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-right byte-upper-case? str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aaabcdaaa")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both #\a str beg end))
    => (S "bcd"))

  (check
      (let* ((str (S "bcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both #\a str beg end))
    => (S "bcd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both #\a str beg end))
    => '#vu8())

  (check
      (let* ((str (S "aaabcdaa")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both (char-set #\a #\b) str beg end))
    => (S "cd"))

  (check
      (let* ((str (S "bcdb")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both (char-set #\a #\b) str beg end))
    => (S "cd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both (char-set #\a #\b) str beg end))
    => '#vu8())

  (check
      (let* ((str (S "AAAbcdAAA")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both byte-upper-case? str beg end))
    => (S "bcd"))

  (check
      (let* ((str (S "bcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both byte-upper-case? str beg end))
    => (S "bcd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-trim-both byte-upper-case? str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad 3 #\0 str beg end))
    => (S "abc"))

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad 5 #\0 str beg end))
    => (S "00abc"))

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad 2 #\0 str beg end))
    => (S "bc"))

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad 0 #\0 str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad-right 3 #\0 str beg end))
    => (S "abc"))

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad-right 5 #\0 str beg end))
    => (S "abc00"))

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad-right 2 #\0 str beg end))
    => (S "ab"))

  (check
      (let* ((str (S "abc")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-pad-right 0 #\0 str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (str2 (bytevector-copy (S "12"))))
	(%bytevector-s8-copy*! str2 0 str1 beg1 (+ 2 beg1))
	str2)
    => (S "ab"))

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (str2 '#vu8()))
	(%bytevector-s8-copy*! str2 0 str1 beg1 beg1)
	str2)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str1 (S "abcd")) (beg1 0) (str2 (bytevector-copy (S "12"))))
	  (%bytevector-s8-copy*! str2 3 str1 beg1 (+ 2 beg1))
	  str2))
    => #t)

  #f)


(parameterise ((check-test-name 'prefix))

  (check
      (let* ((str1 (S "abcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 (S "aBcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "a")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "a")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 (S "1")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "2")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "abcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "efgabcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123abcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 (S "efgabcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123abCd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "a")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "a")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 (S "1")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "2")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "abcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "aBcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "aBcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 (S "aBcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "a")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "a")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 (S "1")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "2")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "abcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "efgabCd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123abCd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 (S "efgabCd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123abcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "a")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "a")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 (S "1")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "2")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 (S "abcdefg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "abcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "aBcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "aBcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "aBcd123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123abcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123aBcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123aBcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123aBcd")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "123")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 (S "efg")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  #f)


(parameterise ((check-test-name 'searching))

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index #\b str beg end))
    => 1)

  (check
      (let* ((str (S "abcd")) (end (bytevector-length str)))
	(%bytevector-s8-index #\b str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index #\1 str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str (S "abcd")) (end (bytevector-length str)))
	(%bytevector-s8-index (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aBcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index byte-upper-case? str beg end))
    => 1)

  (check
      (let* ((str (S "aBcd")) (end (bytevector-length str)))
	(%bytevector-s8-index byte-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index byte-upper-case? str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index byte-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right #\b str beg end))
    => 1)

  (check
      (let* ((str (S "abcd")) (end (bytevector-length str)))
	(%bytevector-s8-index-right #\b str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right #\1 str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str (S "abcd")) (end (bytevector-length str)))
	(%bytevector-s8-index-right (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aBcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right byte-upper-case? str beg end))
    => 1)

  (check
      (let* ((str (S "aBcd")) (end (bytevector-length str)))
	(%bytevector-s8-index-right byte-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right byte-upper-case? str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-index-right byte-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "bacd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip #\b str beg end))
    => 1)

  (check
      (let* ((str (S "bacd")) (end (bytevector-length str)))
	(%bytevector-s8-skip #\b str 1 end))
    => 1)

  (check
      (let* ((str (S "1111")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip #\1 str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "bacd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str (S "bacd")) (end (bytevector-length str)))
	(%bytevector-s8-skip (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str (S "1010")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "Bacd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip byte-upper-case? str beg end))
    => 1)

  (check
      (let* ((str (S "Bacd")) (end (bytevector-length str)))
	(%bytevector-s8-skip byte-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str (S "ABCD")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip byte-upper-case? str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip byte-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "acdb")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right #\b str beg end))
    => 2)

  (check
      (let* ((str (S "acdb")) (end (bytevector-length str)))
	(%bytevector-s8-skip-right #\b str 1 end))
    => 2)

  (check
      (let* ((str (S "1111")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right #\1 str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "acdb")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str (S "acdb")) (end (bytevector-length str)))
	(%bytevector-s8-skip-right (char-set #\b #\B) str 1 end))
    => 2)

  (check
      (let* ((str (S "0101")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "acdB")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right byte-upper-case? str beg end))
    => 2)

  (check
      (let* ((str (S "acdB")) (end (bytevector-length str)))
	(%bytevector-s8-skip-right byte-upper-case? str 1 end))
    => 2)

  (check
      (let* ((str (S "ABCD")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right byte-upper-case? str beg end))
    => #f)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-skip-right byte-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count #\b str beg end))
    => 2)

  (check
      (let* ((str (S "abcd")) (end (bytevector-length str)))
	(%bytevector-s8-count #\b str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count #\1 str beg end))
    => 0)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count #\1 str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcBd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str (S "abcd")) (end (bytevector-length str)))
	(%bytevector-s8-count (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count (char-set #\0 #\1) str beg end))
    => 0)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count (char-set #\0 #\1) str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aBcAd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count byte-upper-case? str beg end))
    => 2)

  (check
      (let* ((str (S "aBcd")) (end (bytevector-length str)))
	(%bytevector-s8-count byte-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str (S "abcd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count byte-upper-case? str beg end))
    => 0)

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-count byte-upper-case? str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "ciao hello salut")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "hello")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 (S "ciao hello salut")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "hola")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 (S "ciao hello salut")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "hello")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 (S "ciAO HELLO saLUT")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "hello")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 (S "ciao hello salut")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "HOLA")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 (S "ciao hello salut")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 '#vu8()) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "hello")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  #f)


(parameterise ((check-test-name 'filtering))

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete #\b str beg end))
    => (S "acd"))

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete #\0 str beg end))
    => (S "abcbd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete #\b str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete (char-set #\b #\B) str beg end))
    => (S "acd"))

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete (char-set #\0 #\1) str beg end))
    => (S "abcbd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete (char-set #\b #\B) str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aBcBd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete byte-upper-case? str beg end))
    => (S "acd"))

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete byte-upper-case? str beg end))
    => (S "abcbd"))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-delete byte-upper-case? str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter #\b str beg end))
    => (S "bb"))

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter #\0 str beg end))
    => '#vu8())

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter #\b str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter (char-set #\b #\B) str beg end))
    => (S "bb"))

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter (char-set #\0 #\1) str beg end))
    => '#vu8())

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter (char-set #\b #\B) str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "aBcBd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter byte-upper-case? str beg end))
    => (S "BB"))

  (check
      (let* ((str (S "abcbd")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter byte-upper-case? str beg end))
    => '#vu8())

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-filter byte-upper-case? str beg end))
    => '#vu8())

  #f)


(parameterise ((check-test-name 'lists))

  (check
      (let* ((str (bytevector-copy (S "abcd"))) (beg 0) (end (bytevector-length str)))
	(%bytevector->s8-list* str beg end))
    => (map char->integer '(#\a #\b #\c #\d)))

  (check
      (let* ((str (bytevector-copy '#vu8())) (beg 0) (end (bytevector-length str)))
	(%bytevector->s8-list* str beg end))
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
      (let* ((str (S "ciao hello salut")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-tokenize (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u)
				 str beg end))
    => `(,(S "ciao") ,(S "hello") ,(S "salut")))

  (check
      (let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-tokenize (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u)
				 str beg end))
    => '())

  (check
      (let* ((str (S "ciao hello salut")) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-tokenize (char-set) str beg end))
    => '())

  #f)

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'infix)
    => (S "c,i,a,o"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'strict-infix)
    => (S "c,i,a,o"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'suffix)
    => (S "c,i,a,o,"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",") 'prefix)
    => (S ",c,i,a,o"))

;;; --------------------------------------------------------------------

  (check
      (%bytevector-s8-join '() (S ",") 'infix)
    => '#vu8())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(%bytevector-s8-join '() (S ",") 'strict-infix))
    => #t)

  (check
      (%bytevector-s8-join '() (S ",") 'suffix)
    => '#vu8())

  (check
      (%bytevector-s8-join '() (S ",") 'prefix)
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (%bytevector-s8-join `(,(S "c")) (S ",") 'infix)
    => (S "c"))

  (check
      (%bytevector-s8-join `(,(S "c")) (S ",") 'strict-infix)
    => (S "c"))

  (check
      (%bytevector-s8-join `(,(S "c")) (S ",") 'suffix)
    => (S "c,"))

  (check
      (%bytevector-s8-join `(,(S "c")) (S ",") 'prefix)
    => (S ",c"))

;;; --------------------------------------------------------------------

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'infix)
    => (S "ciao"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'strict-infix)
    => (S "ciao"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'suffix)
    => (S "ciao"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) '#vu8() 'prefix)
    => (S "ciao"))

;;; --------------------------------------------------------------------

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'infix)
    => (S "c,;;i,;;a,;;o"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'strict-infix)
    => (S "c,;;i,;;a,;;o"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'suffix)
    => (S "c,;;i,;;a,;;o,;;"))

  (check
      (%bytevector-s8-join `(,(S "c") ,(S "i") ,(S "a") ,(S "o")) (S ",;;") 'prefix)
    => (S ",;;c,;;i,;;a,;;o"))

  #f)


(parameterise ((check-test-name 'replicating))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str)))
	(%xsubbytevector-s8 0 5 str beg end))
    => (S "ciao "))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str)))
	(%xsubbytevector-s8 0 9 str beg end))
    => (S "ciao ciao"))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str)))
	(%xsubbytevector-s8 -5 5 str beg end))
    => (S "ciao ciao "))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str)))
	(%xsubbytevector-s8 2 4 str beg end))
    => (S "ao"))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str)))
	(%xsubbytevector-s8 -3 7 str beg end))
    => (S "ao ciao ci"))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let ((str (S "ciao ")))
	  (%xsubbytevector-s8 -3 7 str 3 3)))
    => #t)

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str '#vu8()) (beg 0) (end (bytevector-length str)))
	  (%xsubbytevector-s8 0 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str))
	     (result (bytevector-copy (S "01234"))))
	(%bytevector-s8-xcopy! 0 5 result 0 (bytevector-length result) str beg end)
	result)
    => (S "ciao "))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str))
	     (result (bytevector-copy (S "012345678"))))
	(%bytevector-s8-xcopy! 0 9 result 0 (bytevector-length result) str beg end)
	result)
    => (S "ciao ciao"))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str))
	     (result (bytevector-copy (S "0123456789"))))
	(%bytevector-s8-xcopy! -5 5 result 0 (bytevector-length result) str beg end)
	result)
    => (S "ciao ciao "))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str))
	     (result (bytevector-copy (S "01"))))
	(%bytevector-s8-xcopy! 2 4 result 0 (bytevector-length result) str beg end)
	result)
    => (S "ao"))

  (check
      (let* ((str (S "ciao ")) (beg 0) (end (bytevector-length str))
	     (result (bytevector-copy (S "0123456789"))))
	(%bytevector-s8-xcopy! -3 7 result 0 (bytevector-length result) str beg end)
	result)
    => (S "ao ciao ci"))

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str '#vu8()) (beg 0) (end (bytevector-length str))
	       (result (bytevector-copy '#vu8())))
	  (%bytevector-s8-xcopy! 0 5 result 0 (bytevector-length result) str beg end)))
    => #t)

  #f)


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (bytevector-copy (S "abcd"))) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-fill*! #\b str beg end)
	str)
    => (S "bbbb"))

  (check
      (let* ((str (bytevector-copy (S "accd"))))
	(%bytevector-s8-fill*! #\b str 1 3)
	str)
    => (S "abbd"))

  (check
      (let* ((str (bytevector-copy '#vu8())))
	(%bytevector-s8-fill*! #\b str 0 0)
	str)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'reverse))

  (check
      (let* ((str (bytevector-copy (S "abcd"))) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-reverse str beg end))
    => (S "dcba"))

  (check
      (let* ((str (bytevector-copy '#vu8())) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-reverse str beg end))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-copy (S "abcd"))) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-reverse! str beg end)
	str)
    => (S "dcba"))

  (check
      (let* ((str (bytevector-copy '#vu8())) (beg 0) (end (bytevector-length str)))
	(%bytevector-s8-reverse! str beg end)
	str)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'concatenate))

  (check
      (bytevector-s8-concatenate `(,(S "ciao") ,(S " ") ,(S "hello") ,(S " ") ,(S "salut")))
    => (S "ciao hello salut"))

  (check
      (bytevector-s8-concatenate '())
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (%bytevector-s8-concatenate-reverse `(,(S "ciao") ,(S " ") ,(S "hello") ,(S " ") ,(S "salut"))
					  (S " hola") (bytevector-length (S " hola")))
    => (S "salut hello ciao hola"))

  (check
      (%bytevector-s8-concatenate-reverse `(,(S "ciao") ,(S " ") ,(S "hello") ,(S " ") ,(S "salut"))
					  (S " hola") 3)
    => (S "salut hello ciao ho"))

  (check
      (%bytevector-s8-concatenate-reverse '() '#vu8() 0)
    => '#vu8())

  #f)


(parameterise ((check-test-name 'replace))

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "1234")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-replace str1 beg1 end1 str2 beg2 end2))
    => (S "1234"))

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "1234")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-replace str1 2 2 str2 beg2 end2))
    => (S "ab1234cd"))

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 '#vu8()) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-replace str1 2 2 str2 beg2 end2))
    => (S "abcd"))

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "1234")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-replace str1 1 3 str2 beg2 end2))
    => (S "a1234d"))

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "1234")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-replace str1 0 3 str2 beg2 end2))
    => (S "1234d"))

  (check
      (let* ((str1 (S "abcd")) (beg1 0) (end1 (bytevector-length str1))
	     (str2 (S "1234")) (beg2 0) (end2 (bytevector-length str2)))
	(%bytevector-s8-replace str1 1 4 str2 beg2 end2))
    => (S "a1234"))

  #f)


;;;; done

(check-report)

;;; end of file
