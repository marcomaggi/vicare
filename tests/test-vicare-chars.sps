;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for "ikarus.chars.ss"
;;;Date: Mon Oct 31, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (prefix (vicare unsafe operations)
	  unsafe.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare char functions\n")


;;;; syntax helpers

(define-syntax catch-procedure-argument-violation
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((procedure-argument-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		#t)
	       (else E))
       (begin . ?body)))))


(parametrise ((check-test-name	'integer-to-char))

  (check
      (integer->char 0)
    => #\x0)

  (check
      (integer->char #x10FFFF)
    => #\x10FFFF)

  (check
      (integer->char #xD7FF)
    => #\xD7FF)

  (check
      (integer->char #xE000)
    => #\xE000)

  (check
      (integer->char #xE001)
    => #\xE001)

;;; --------------------------------------------------------------------

  (check
      (catch-procedure-argument-violation #f
	(integer->char #xD800))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(integer->char #xDFFF))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(integer->char #x11FFFF))
    => #t)

  #t)


(parametrise ((check-test-name	'char-to-integer))

  (check
      (char->integer #\x0)
    => 0)

  (check
      (char->integer #\x10FFFF)
    => #x10FFFF)

  (check
      (char->integer #\xD7FF)
    => #xD7FF)

  (check
      (char->integer #\xE001)
    => #xE001)

;;; --------------------------------------------------------------------

  (check
      (catch-procedure-argument-violation #f
	(char->integer 123))
    => #t)

  #t)


(parametrise ((check-test-name	'char-equal))

  (check
      (char=? #\a #\a)
    => #t)

  (check
      (char=? #\a #\b)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char=? #\a #\a #\a)
    => #t)

  (check
      (char=? #\a #\b #\b)
    => #f)

  (check
      (char=? #\b #\a #\b)
    => #f)

  (check
      (char=? #\b #\b #\a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char=? #\a #\a #\a #\a)
    => #t)

  (check
      (char=? #\a #\b #\b #\b)
    => #f)

  (check
      (char=? #\b #\a #\b #\b)
    => #f)

  (check
      (char=? #\b #\b #\a #\b)
    => #f)

  (check
      (char=? #\b #\b #\b #\a)
    => #f)

;;; --------------------------------------------------------------------
;;; arguments validation: 2 args

  (check
      (catch-procedure-argument-violation #f
	(char=? 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char=? #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch-procedure-argument-violation #f
	(char=? 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char=? #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char=? #\b #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch-procedure-argument-violation #f
	(char=? 123 #\b #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char=? #\b 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char=? #\b #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char=? #\b #\b #\b 123))
    => #t)

  #t)


(parametrise ((check-test-name	'char-not-equal))

  ;;These should test the primitive operation.
  (check-for-true	(char!=? #\A #\B))
  (check-for-false	(char!=? #\A #\A))

  ;;These should test the function rather than the primitive operation.
  (check-for-true	(apply char!=? #\A '(#\B)))
  (check-for-false	(apply char!=? #\A '(#\A)))

;;; --------------------------------------------------------------------

  (check-for-true	(char!=? #\A #\B #\C))
  (check-for-false	(char!=? #\A #\A #\A))
  (check-for-false	(char!=? #\A #\B #\B))
  (check-for-false	(char!=? #\B #\A #\B))
  (check-for-false	(char!=? #\B #\B #\A))

;;; --------------------------------------------------------------------

  (check-for-true	(char!=? #\A #\B #\C #\D))

  (check-for-false	(char!=? #\A #\A #\A #\A))

  (check-for-false	(char!=? #\A #\B #\B #\B))
  (check-for-false	(char!=? #\B #\A #\B #\B))
  (check-for-false	(char!=? #\B #\B #\A #\B))
  (check-for-false	(char!=? #\B #\B #\B #\A))

  (check-for-false	(char!=? #\A #\A #\C #\D))
  (check-for-false	(char!=? #\A #\B #\A #\D))
  (check-for-false	(char!=? #\A #\B #\C #\A))

  (check-for-false	(char!=? #\A #\B #\B #\D))
  (check-for-false	(char!=? #\A #\B #\C #\B))

  (check-for-false	(char!=? #\A #\B #\C #\C))

;;; --------------------------------------------------------------------
;;; arguments validation: 2 args

  (check
      (catch-procedure-argument-violation #f
	(char!=? 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char!=? #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch-procedure-argument-violation #f
	(char!=? 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char!=? #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char!=? #\b #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch-procedure-argument-violation #f
	(char!=? 123 #\b #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char!=? #\b 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char!=? #\b #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char!=? #\b #\b #\b 123))
    => #t)

  #t)


(parametrise ((check-test-name	'char-ci-not-equal))

  (check-for-true	(char-ci!=? #\A #\B))
  (check-for-false	(char-ci!=? #\A #\A))

  (check-for-true	(char-ci!=? #\A #\b))
  (check-for-false	(char-ci!=? #\A #\a))

;;; --------------------------------------------------------------------

  (check-for-true	(char-ci!=? #\A #\B #\C))
  (check-for-false	(char-ci!=? #\A #\A #\A))
  (check-for-false	(char-ci!=? #\A #\B #\B))
  (check-for-false	(char-ci!=? #\B #\A #\B))
  (check-for-false	(char-ci!=? #\B #\B #\A))

  (check-for-true	(char-ci!=? #\a #\B #\C))
  (check-for-false	(char-ci!=? #\A #\a #\A))
  (check-for-false	(char-ci!=? #\A #\B #\b))
  (check-for-false	(char-ci!=? #\b #\A #\B))
  (check-for-false	(char-ci!=? #\B #\b #\a))

;;; --------------------------------------------------------------------

  (check-for-true	(char-ci!=? #\A #\B #\C #\D))

  (check-for-false	(char-ci!=? #\A #\A #\A #\A))

  (check-for-false	(char-ci!=? #\A #\B #\B #\B))
  (check-for-false	(char-ci!=? #\B #\A #\B #\B))
  (check-for-false	(char-ci!=? #\B #\B #\A #\B))
  (check-for-false	(char-ci!=? #\B #\B #\B #\A))

  (check-for-false	(char-ci!=? #\A #\A #\C #\D))
  (check-for-false	(char-ci!=? #\A #\B #\A #\D))
  (check-for-false	(char-ci!=? #\A #\B #\C #\A))

  (check-for-false	(char-ci!=? #\A #\B #\B #\D))
  (check-for-false	(char-ci!=? #\A #\B #\C #\B))

  (check-for-false	(char-ci!=? #\A #\B #\C #\C))


  (check-for-true	(char-ci!=? #\A #\b #\C #\d))

  (check-for-false	(char-ci!=? #\A #\a #\a #\A))

  (check-for-false	(char-ci!=? #\A #\B #\B #\b))
  (check-for-false	(char-ci!=? #\b #\A #\B #\B))
  (check-for-false	(char-ci!=? #\B #\b #\A #\B))
  (check-for-false	(char-ci!=? #\B #\B #\b #\A))

  (check-for-false	(char-ci!=? #\A #\a #\C #\D))
  (check-for-false	(char-ci!=? #\A #\B #\a #\D))
  (check-for-false	(char-ci!=? #\a #\B #\C #\A))

  (check-for-false	(char-ci!=? #\A #\b #\b #\d))
  (check-for-false	(char-ci!=? #\a #\B #\C #\B))

  (check-for-false	(char-ci!=? #\A #\b #\c #\C))

;;; --------------------------------------------------------------------
;;; arguments validation: 2 args

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? #\b #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? 123 #\b #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? #\b 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? #\b #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char-ci!=? #\b #\b #\b 123))
    => #t)

  #t)


(parametrise ((check-test-name	'char-less))

  (check
      (char<? #\a #\a)
    => #f)

  (check
      (char<? #\a #\b)
    => #t)

  (check
      (char<? #\b #\a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char<? #\a #\a #\a)
    => #f)

  (check
      (char<? #\a #\b #\c)
    => #t)

  (check
      (char<? #\b #\a #\b)
    => #f)

  (check
      (char<? #\b #\b #\a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char<? #\a #\a #\a #\a)
    => #f)

  (check
      (char<? #\a #\b #\c #\d)
    => #t)

  (check
      (char<? #\b #\a #\b #\b)
    => #f)

  (check
      (char<? #\b #\b #\a #\b)
    => #f)

  (check
      (char<? #\b #\b #\b #\a)
    => #f)

;;; --------------------------------------------------------------------
;;; arguments validation: 2 args

  (check
      (catch-procedure-argument-violation #f
	(char<? 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<? #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch-procedure-argument-violation #f
	(char<? 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<? #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<? #\b #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch-procedure-argument-violation #f
	(char<? 123 #\b #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<? #\b 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<? #\b #\a 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<? #\b #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<? #\b #\b #\b 123))
    => #t)

  #t)


(parametrise ((check-test-name	'char-less-or-equal))

  (check
      (char<=? #\a #\a)
    => #t)

  (check
      (char<=? #\a #\b)
    => #t)

  (check
      (char<=? #\b #\a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char<=? #\a #\a #\a)
    => #t)

  (check
      (char<=? #\a #\b #\c)
    => #t)

  (check
      (char<=? #\b #\a #\b)
    => #f)

  (check
      (char<=? #\b #\b #\a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char<=? #\a #\a #\a #\a)
    => #t)

  (check
      (char<=? #\a #\b #\c #\d)
    => #t)

  (check
      (char<=? #\b #\a #\b #\b)
    => #f)

  (check
      (char<=? #\b #\b #\a #\b)
    => #f)

  (check
      (char<=? #\b #\b #\b #\a)
    => #f)

;;; --------------------------------------------------------------------
;;; arguments validation: 2 args

  (check
      (catch-procedure-argument-violation #f
	(char<=? 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<=? #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch-procedure-argument-violation #f
	(char<=? 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<=? #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<=? #\b #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch-procedure-argument-violation #f
	(char<=? 123 #\b #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<=? #\b 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<=? #\b #\a 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<=? #\b #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char<=? #\b #\b #\b 123))
    => #t)

  #t)


(parametrise ((check-test-name	'char-greater))

  (check
      (char>? #\a #\a)
    => #f)

  (check
      (char>? #\a #\b)
    => #f)

  (check
      (char>? #\b #\a)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (char>? #\a #\a #\a)
    => #f)

  (check
      (char>? #\a #\b #\b)
    => #f)

  (check
      (char>? #\b #\a #\b)
    => #f)

  (check
      (char>? #\c #\b #\a)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (char>? #\a #\a #\a #\a)
    => #f)

  (check
      (char>? #\a #\b #\b #\b)
    => #f)

  (check
      (char>? #\b #\a #\b #\b)
    => #f)

  (check
      (char>? #\b #\b #\a #\b)
    => #f)

  (check
      (char>? #\d #\c #\b #\a)
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 2 args

  (check
      (catch-procedure-argument-violation #f
	(char>? 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>? #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch-procedure-argument-violation #f
	(char>? 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>? #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>? #\b #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch-procedure-argument-violation #f
	(char>? 123 #\b #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>? #\b 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>? #\b #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>? #\b #\b #\b 123))
    => #t)

  #t)


(parametrise ((check-test-name	'char-greater-or-equal))

  (check
      (char>=? #\a #\a)
    => #t)

  (check
      (char>=? #\a #\b)
    => #f)

  (check
      (char>=? #\b #\a)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (char>=? #\a #\a #\a)
    => #t)

  (check
      (char>=? #\a #\b #\b)
    => #f)

  (check
      (char>=? #\b #\a #\b)
    => #f)

  (check
      (char>=? #\c #\b #\a)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (char>=? #\a #\a #\a #\a)
    => #t)

  (check
      (char>=? #\a #\b #\b #\b)
    => #f)

  (check
      (char>=? #\b #\a #\b #\b)
    => #f)

  (check
      (char>=? #\b #\b #\a #\b)
    => #f)

  (check
      (char>=? #\d #\c #\b #\a)
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 2 args

  (check
      (catch-procedure-argument-violation #f
	(char>=? 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>=? #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch-procedure-argument-violation #f
	(char>=? 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>=? #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>=? #\b #\b 123))
    => #t)

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch-procedure-argument-violation #f
	(char>=? 123 #\b #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>=? #\b 123 #\b #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>=? #\b #\b 123 #\b))
    => #t)

  (check
      (catch-procedure-argument-violation #f
	(char>=? #\b #\b #\b 123))
    => #t)

  #t)


(parametrise ((check-test-name	'min-max))

  (check (chmin #\A)			=> #\A)

  (check (chmin #\a #\a)		=> #\a)
  (check (chmin #\a #\b)		=> #\a)
  (check (chmin #\b #\a)		=> #\a)

  (check (chmin #\a #\b #\c)		=> #\a)

  (check (chmin #\a #\b #\c #\d)	=> #\a)

  (check (chmin #\a #\b #\c #\d #\e)	=> #\a)

;;; --------------------------------------------------------------------

  (check (chmax #\A)			=> #\A)

  (check (chmax #\a #\a)		=> #\a)
  (check (chmax #\a #\b)		=> #\b)
  (check (chmax #\b #\a)		=> #\b)

  (check (chmax #\a #\b #\c)		=> #\c)

  (check (chmax #\a #\b #\c #\d)	=> #\d)

  (check (chmax #\a #\b #\c #\d #\e)	=> #\e)

  #t)


(parametrise ((check-test-name	'unsafe))

  (check
      (unsafe.$char= #\a #\a #\a)
    => #t)

  (check
      (unsafe.$char= #\a #\b #\a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (unsafe.$char> #\a #\a)
    => #f)

  (check
      (unsafe.$char> #\c #\b #\a)
    => #t)

  (check
      (unsafe.$char> #\c #\b #\z)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (unsafe.$char>= #\a #\a)
    => #t)

  (check
      (unsafe.$char>= #\c #\b #\a)
    => #t)

  (check
      (unsafe.$char>= #\c #\b #\z)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (unsafe.$char< #\a #\a)
    => #f)

  (check
      (unsafe.$char< #\a #\b #\c)
    => #t)

  (check
      (unsafe.$char< #\c #\d #\a)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (unsafe.$char<= #\a #\a)
    => #t)

  (check
      (unsafe.$char<= #\a #\b #\c)
    => #t)

  (check
      (unsafe.$char<= #\b #\c #\a)
    => #f)

  #t)


(parametrise ((check-test-name	'misc))

  (check (char-in-ascii-range? #\c)	=> #t)
  (check (char-in-ascii-range? #\x5555)	=> #f)

;;; --------------------------------------------------------------------

  (check-for-true	(unicode-printable-char? #\a))
  (check-for-false	(unicode-printable-char? #\x00))

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch-procedure-argument-violation 'scheme-indent-function 1)
;;End:
