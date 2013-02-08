;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 48: intermediate format strings
;;;Date: Thu Feb  7, 2013
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
(import (vicare)
  (prefix (srfi :48) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI 48: intermediate format strings\n")


(parametrise ((check-test-name	'base))

  (check (srfi.format "")		=> "")
  (check (srfi.format "ciao")		=> "ciao")

;;; --------------------------------------------------------------------

  (check (srfi.format "ci ~a ao" 123)	=> "ci 123 ao")
  (check (srfi.format "ci ~s ao" 123)	=> "ci 123 ao")
  (check (srfi.format "ci ~~ ao")	=> "ci ~ ao")

  (check (srfi.format "ci ~d ao" 123)	=> "ci 123 ao")
  (check (srfi.format "ci ~o ao" 10)	=> "ci 12 ao")
  (check (srfi.format "ci ~x ao" 16)	=> "ci 10 ao")

  #t)


;;;; done

(check-report)

;;; end of file
