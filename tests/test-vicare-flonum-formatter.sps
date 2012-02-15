;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare flonum-formatter)
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
  (vicare flonum-formatter)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing library (vicare flonum-formatter)\n")


(parametrise ((check-test-name	'positive))

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	0))
	(format-flonum positive? digits expt))
    => "0.123456789")

;;; --------------------------------------------------------------------

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	1))
	(format-flonum positive? digits expt))
    => "1.23456789")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	2))
	(format-flonum positive? digits expt))
    => "12.3456789")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	3))
	(format-flonum positive? digits expt))
    => "123.456789")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	8))
	(format-flonum positive? digits expt))
    => "12345678.9")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	9))
	(format-flonum positive? digits expt))
    => "123456789.0")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	10))
	(format-flonum positive? digits expt))
    => "1.23456789e9")

;;; --------------------------------------------------------------------

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-1))
	(format-flonum positive? digits expt))
    => "0.0123456789")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-2))
	(format-flonum positive? digits expt))
    => "0.00123456789")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-3))
	(format-flonum positive? digits expt))
    => "0.000123456789")

  (check
      (let ((positive?	#t)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-4))
	(format-flonum positive? digits expt))
    => "1.23456789e-5")

  #t)


(parametrise ((check-test-name	'negative))

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	0))
	(format-flonum positive? digits expt))
    => "-0.123456789")

;;; --------------------------------------------------------------------

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	1))
	(format-flonum positive? digits expt))
    => "-1.23456789")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	2))
	(format-flonum positive? digits expt))
    => "-12.3456789")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	3))
	(format-flonum positive? digits expt))
    => "-123.456789")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	8))
	(format-flonum positive? digits expt))
    => "-12345678.9")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	9))
	(format-flonum positive? digits expt))
    => "-123456789.0")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	10))
	(format-flonum positive? digits expt))
    => "-1.23456789e9")

;;; --------------------------------------------------------------------

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-1))
	(format-flonum positive? digits expt))
    => "-0.0123456789")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-2))
	(format-flonum positive? digits expt))
    => "-0.00123456789")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-3))
	(format-flonum positive? digits expt))
    => "-0.000123456789")

  (check
      (let ((positive?	#f)
	    (digits	'(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	    (expt	-4))
	(format-flonum positive? digits expt))
    => "-1.23456789e-5")

  #f)


;;;; done

(check-report)

;;; end of file
