;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the ASCIIs library
;;;Date: Tue Jun 29, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare language-extensions ascii-chars)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: utilities for ASCII chars\n")


(parametrise ((check-test-name	'case))

  (check
      (ascii-upper-case? (char->integer #\A))
    => #t)

  (check
      (ascii-upper-case? (char->integer #\Z))
    => #t)

  (check
      (ascii-upper-case? (char->integer #\a))
    => #f)

  (check
      (ascii-upper-case? (char->integer #\0))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (ascii-title-case? (char->integer #\A))
    => #t)

  (check
      (ascii-title-case? (char->integer #\Z))
    => #t)

  (check
      (ascii-title-case? (char->integer #\a))
    => #f)

  (check
      (ascii-title-case? (char->integer #\0))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (ascii-lower-case? (char->integer #\a))
    => #t)

  (check
      (ascii-lower-case? (char->integer #\z))
    => #t)

  (check
      (ascii-lower-case? (char->integer #\A))
    => #f)

  (check
      (ascii-lower-case? (char->integer #\0))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (ascii-cased? (char->integer #\A))
    => #t)

  (check
      (ascii-cased? (char->integer #\Z))
    => #t)

  (check
      (ascii-cased? (char->integer #\a))
    => #t)

  (check
      (ascii-cased? (char->integer #\z))
    => #t)

  (check
      (ascii-cased? (char->integer #\0))
    => #f)

  (check
      (ascii-cased? (char->integer #\[))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (ascii-upcase (char->integer #\A))
    => (char->integer #\A))

  (check
      (ascii-upcase (char->integer #\a))
    => (char->integer #\A))

  (check
      (ascii-upcase (char->integer #\0))
    => (char->integer #\0))

;;; --------------------------------------------------------------------

  (check
      (ascii-downcase (char->integer #\A))
    => (char->integer #\a))

  (check
      (ascii-downcase (char->integer #\a))
    => (char->integer #\a))

  (check
      (ascii-downcase (char->integer #\0))
    => (char->integer #\0))

;;; --------------------------------------------------------------------

  (check
      (ascii-titlecase (char->integer #\A))
    => (char->integer #\A))

  (check
      (ascii-titlecase (char->integer #\a))
    => (char->integer #\A))

  (check
      (ascii-titlecase (char->integer #\0))
    => (char->integer #\0))


  #t)


(parametrise ((check-test-name	'kind))

  (check
      (ascii-numeric? (char->integer #\0))
    => #t)

  (check
      (ascii-numeric? (char->integer #\9))
    => #t)

  (check
      (ascii-numeric? (char->integer #\A))
    => #f)

  (check
      (ascii-numeric? (char->integer #\z))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (ascii-alphabetic? (char->integer #\A))
    => #t)

  (check
      (ascii-alphabetic? (char->integer #\Z))
    => #t)

  (check
      (ascii-alphabetic? (char->integer #\a))
    => #t)

  (check
      (ascii-alphabetic? (char->integer #\z))
    => #t)

  (check
      (ascii-alphabetic? (char->integer #\0))
    => #f)

  (check
      (ascii-alphabetic? (char->integer #\newline))
    => #f)

  (check
      (ascii-alphabetic? (char->integer #\"))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
