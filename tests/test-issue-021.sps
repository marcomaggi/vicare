;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 21, delete character is printed unescaped
;;;Date: Mon Jun 13, 2011
;;;
;;;Abstract
;;;
;;;	Reported by Göran Weinholt.
;;;
;;;	  The DEL  character is  printed unescaped.  It's  not generally
;;;	considered  printable,   and  on  my  terminal   it  looks  like
;;;	whitespace for some reason.
;;;
;;;		> "\x7f;"
;;;		""
;;;
;;;	  It  happens only  when  the #\delete  character  appears in  a
;;;	string.
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rnrs)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 21, delete character is printed unescaped\n")


(check
    (call-with-string-output-port
	(lambda (p)
	  (write #\delete p)))
  => "#\\delete")

(check
    (call-with-string-output-port
	(lambda (p)
	  (write #\x7F p)))
  => "#\\delete")

(check
    (call-with-string-output-port
	(lambda (p)
	  (write "\x7F;" p)))
  => "\"\\x7F;\"")

(check
    (call-with-string-output-port
	(lambda (p)
	  (write "\x33;" p)))
  => "\"3\"")


;;;; done

(check-report)

;;; end of file
