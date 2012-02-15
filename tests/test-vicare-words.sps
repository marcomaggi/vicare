;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare words)
;;;Date: Wed Feb 15, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (vicare words) words.)
  (vicare platform-constants)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing (vicare words) library\n")


(parametrise ((check-test-name	'clang))

  (check (words.unsigned-char? (+ +1 UCHAR_MAX))	=> #f)
  (check (words.unsigned-char? UCHAR_MAX)		=> #t)
  (check (words.unsigned-char? 0)			=> #t)
  (check (words.unsigned-char? -1)			=> #f)

  (check (words.signed-char? (+ +1 CHAR_MAX))		=> #f)
  (check (words.signed-char? CHAR_MAX)			=> #t)
  (check (words.signed-char? CHAR_MIN)			=> #t)
  (check (words.signed-char? (+ -1 CHAR_MIN))		=> #f)

  (check (words.signed-char? (+ +1 SCHAR_MAX))		=> #f)
  (check (words.signed-char? SCHAR_MAX)			=> #t)
  (check (words.signed-char? SCHAR_MIN)			=> #t)
  (check (words.signed-char? (+ -1 SCHAR_MIN))		=> #f)

;;; --------------------------------------------------------------------

  (check (words.unsigned-short? (+ +1 USHRT_MAX))	=> #f)
  (check (words.unsigned-short? USHRT_MAX)		=> #t)
  (check (words.unsigned-short? 0)			=> #t)
  (check (words.unsigned-short? -1)			=> #f)

  (check (words.signed-short? (+ +1 SHRT_MAX))		=> #f)
  (check (words.signed-short? SHRT_MAX)			=> #t)
  (check (words.signed-short? SHRT_MIN)			=> #t)
  (check (words.signed-short? (+ -1 SHRT_MIN))		=> #f)

;;; --------------------------------------------------------------------

  (check (words.unsigned-int? (+ +1 UINT_MAX))		=> #f)
  (check (words.unsigned-int? UINT_MAX)			=> #t)
  (check (words.unsigned-int? 0)			=> #t)
  (check (words.unsigned-int? -1)			=> #f)

  (check (words.signed-int? (+ +1 INT_MAX))		=> #f)
  (check (words.signed-int? INT_MAX)			=> #t)
  (check (words.signed-int? INT_MIN)			=> #t)
  (check (words.signed-int? (+ -1 INT_MIN))		=> #f)

;;; --------------------------------------------------------------------

  (check (words.unsigned-long? (+ +1 ULONG_MAX))	=> #f)
  (check (words.unsigned-long? ULONG_MAX)		=> #t)
  (check (words.unsigned-long? 0)			=> #t)
  (check (words.unsigned-long? -1)			=> #f)

  (check (words.signed-long? (+ +1 LONG_MAX))		=> #f)
  (check (words.signed-long? LONG_MAX)			=> #t)
  (check (words.signed-long? LONG_MIN)			=> #t)
  (check (words.signed-long? (+ -1 LONG_MIN))		=> #f)

;;; --------------------------------------------------------------------

  (check (words.unsigned-long-long? (+ +1 ULONG_LONG_MAX))	=> #f)
  (check (words.unsigned-long-long? ULONG_LONG_MAX)	=> #t)
  (check (words.unsigned-long-long? 0)			=> #t)
  (check (words.unsigned-long-long? -1)			=> #f)

  (check (words.signed-long-long? (+ +1 LONG_LONG_MAX))	=> #f)
  (check (words.signed-long-long? LONG_LONG_MAX)	=> #t)
  (check (words.signed-long-long? LONG_LONG_MIN)	=> #t)
  (check (words.signed-long-long? (+ -1 LONG_LONG_MIN))	=> #f)

  #t)


(parametrise ((check-test-name	'size_t))

  (check (words.size_t? (+ 1 SIZE_T_MAX))	=> #f)
  (check (words.size_t? SIZE_T_MAX)		=> #t)
  (check (words.size_t? 0)			=> #t)
  (check (words.size_t? -1)			=> #f)

  (check (words.ssize_t? (+ +1 SSIZE_T_MAX))	=> #f)
  (check (words.ssize_t? SSIZE_T_MAX)		=> #t)
  (check (words.ssize_t? SSIZE_T_MIN)		=> #t)
  (check (words.ssize_t? (+ -1 SSIZE_T_MIN))	=> #f)

  #t)


(parametrise ((check-test-name	'off_t))

  (check (words.off_t? (+ +1 OFF_T_MAX))	=> #f)
  (check (words.off_t? OFF_T_MAX)		=> #t)
  (check (words.off_t? OFF_T_MIN)		=> #t)
  (check (words.off_t? (+ -1 OFF_T_MIN))	=> #f)

  #t)


;;;; done

(check-report)

;;; end of file
