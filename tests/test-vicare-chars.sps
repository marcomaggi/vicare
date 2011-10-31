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
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (ikarus)
		(parameterize	parametrise))
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare char functions\n")


;;;; syntax helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (pretty-print (condition-message E)))
		(condition-irritants E))
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
      (catch #f
	(integer->char #xD800))
    => '(#xD800))

  (check
      (catch #f
	(integer->char #xDFFF))
    => '(#xDFFF))

  (check
      (catch #f
	(integer->char #x11FFFF))
    => '(#x11FFFF))

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
      (catch #f
	(char->integer 123))
    => '(123))

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
      (catch #f
	(char=? 123 #\b))
    => '(123))

  (check
      (catch #f
	(char=? #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch #f
	(char=? 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char=? #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char=? #\b #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch #f
	(char=? 123 #\b #\b #\b))
    => '(123))

  (check
      (catch #f
	(char=? #\b 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char=? #\b #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char=? #\b #\b #\b 123))
    => '(123))

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
      (catch #f
	(char<? 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<? #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch #f
	(char<? 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char<? #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<? #\b #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch #f
	(char<? 123 #\b #\b #\b))
    => '(123))

  (check
      (catch #f
	(char<? #\b 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char<? #\b #\a 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<? #\b #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<? #\b #\b #\b 123))
    => '(123))

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
      (catch #f
	(char<=? 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<=? #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch #f
	(char<=? 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char<=? #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<=? #\b #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch #f
	(char<=? 123 #\b #\b #\b))
    => '(123))

  (check
      (catch #f
	(char<=? #\b 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char<=? #\b #\a 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<=? #\b #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char<=? #\b #\b #\b 123))
    => '(123))

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
      (catch #f
	(char>? 123 #\b))
    => '(123))

  (check
      (catch #f
	(char>? #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch #f
	(char>? 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char>? #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char>? #\b #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch #f
	(char>? 123 #\b #\b #\b))
    => '(123))

  (check
      (catch #f
	(char>? #\b 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char>? #\b #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char>? #\b #\b #\b 123))
    => '(123))

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
      (catch #f
	(char>=? 123 #\b))
    => '(123))

  (check
      (catch #f
	(char>=? #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 3 args

  (check
      (catch #f
	(char>=? 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char>=? #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char>=? #\b #\b 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: 4 args

  (check
      (catch #f
	(char>=? 123 #\b #\b #\b))
    => '(123))

  (check
      (catch #f
	(char>=? #\b 123 #\b #\b))
    => '(123))

  (check
      (catch #f
	(char>=? #\b #\b 123 #\b))
    => '(123))

  (check
      (catch #f
	(char>=? #\b #\b #\b 123))
    => '(123))

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
