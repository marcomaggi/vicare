;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: helper library for the calc parser
;;;Date: Sun Aug  2, 2009
;;;
;;;Abstract
;;;
;;;	This is a helper library for the calc parser generated as a test
;;;	for the (lalr) library.  It is imported by "calc-parser.sls" and
;;;	"test-lalr.sps".
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
(library (libtest calc-parser-helper)
  (export
    table-of-variables
    evaluated-expressions)
  (import (vicare))
  (define table-of-variables
    (make-parameter #f))
  (define evaluated-expressions
    (make-parameter #f)))

;;; end of file
