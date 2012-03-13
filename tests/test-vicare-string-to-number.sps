;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: few tests for string to number conversion
;;;Date: Tue Mar 13, 2012
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
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare string to number conversion\n")

(define-syntax doit
  (syntax-rules ()
    ((_ ?string ?number)
     (check (string->number ?string) => ?number))))


(parametrise ((check-test-name	'exact-integers))

  (doit "1"		+1)
  (doit "12"		+12)
  (doit "123"		+123)

  (doit "+1"		+1)
  (doit "+12"		+12)
  (doit "+123"		+123)

  (doit "-1"		-1)
  (doit "-12"		-12)
  (doit "-123"		-123)

  (doit "#e1"		+1)
  (doit "#e12"		+12)
  (doit "#e123"		+123)

  (doit "#e+1"		+1)
  (doit "#e+12"		+12)
  (doit "#e+123"	+123)

  (doit "#e-1"		-1)
  (doit "#e-12"		-12)
  (doit "#e-123"	-123)

  #t)


(parametrise ((check-test-name	'inexact-integers))

  (doit "1."		+1.0)
  (doit "12."		+12.0)
  (doit "123."		+123.0)
  (doit "123.0"		+123.0)

  (doit "+1."		+1.0)
  (doit "+12."		+12.0)
  (doit "+123."		+123.0)
  (doit "+123.0"	+123.0)

  (doit "-1."		-1.0)
  (doit "-12."		-12.0)
  (doit "-123."		-123.0)
  (doit "-123.0"	-123.0)

  (doit "#i1"		+1.0)
  (doit "#i12"		+12.0)
  (doit "#i123"		+123.0)

  (doit "#i+1"		+1.0)
  (doit "#i+12"		+12.0)
  (doit "#i+123"	+123.0)

  (doit "#i-1"		-1.0)
  (doit "#i-12"		-12.0)
  (doit "#i-123"	-123.0)

  #t)


(parametrise ((check-test-name	'flonumx))

  (doit "123.456"	+123.456)
  (doit "-123.456"	-123.456)
  (doit "+123.456"	+123.456)

  (doit ".1"		0.1)
  (doit ".12"		0.12)
  (doit ".123"		0.123)

  (doit "+.1"		0.1)
  (doit "+.12"		0.12)
  (doit "+.123"		0.123)

  (doit "-.1"		-0.1)
  (doit "-.12"		-0.12)
  (doit "-.123"		-0.123)

  #t)


(parametrise ((check-test-name	'cflonums))

  (doit "1.1+2.2i"	1.1+2.2i)
  (doit "1.+2.2i"	1.0+2.2i)
  (doit "1.1+2.i"	1.1+2.i)
  (doit "1.+2.i"	1.+2.i)
  (doit "1.+2.i"	1.+2.i)

  (doit "1.1@2.2"	1.1@2.2)
  (doit "1.@2.2"	1.0@2.2)
  (doit "1.1@2."	1.1@2.0)
  (doit "1.@2."		1.0@2.0)
  (doit "1.@2."		1.@2.)
  (doit "1.@2."		1.@2.)

  #t)


(parametrise ((check-test-name	'compnums))

  (doit "1+2i"		1+2i)
  (doit "1/2+2i"	1/2+2i)
  (doit "1+2/3i"	1+2/3i)

  (doit "1@2"		1@2)
  (doit "1.2@3"		1.2@3)
  (doit "1@2.3"		1@2.3)
  (doit "1/1@2"		1/2@2)
;;  (doit "1@2/2"		1@2/2)
;;  (doit "1/1@2/2"	1/1@2/2)

  #t)


(parametrise ((check-test-name	'misc))

;;; distinguishing between numbers and symbols

  (doit "+i"		+1i)
  (doit "-i"		-1i)
  (doit "-inf.0"		-inf.0)
  (doit "+inf.0"		+inf.0)
  (doit "-nan.0"		+nan.0)
  (doit "+nan.0"		+nan.0)

  #t)


(parametrise ((check-test-name	'base2))

  (doit "#b1101"		#b1101)

  #t)


(parametrise ((check-test-name	'base8))

  (doit "#o1234"		#o1234)

  #t)


(parametrise ((check-test-name	'base10))

  (doit "#d9876"		#d9876)

  #t)


(parametrise ((check-test-name	'base16))

  (doit "#x12AF"		#x12AF)

  #t)


(parametrise ((check-test-name	'errors))

;;; bad input characters

  (check (string->number "1a")		=> #f)
  (check (string->number "a")		=> #f)
  (check (string->number "1 ")		=> #f)
  (check (string->number " ")		=> #f)

  #t)


;;;; done

(check-report)

;;; end of file
