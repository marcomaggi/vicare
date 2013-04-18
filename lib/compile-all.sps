;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: compile script
;;;Date: Sat Dec 10, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import
  (only (vicare platform configuration))
  (only (vicare platform words))
  (only (vicare platform errno))
  (only (vicare platform constants))
  (only (vicare platform features))
  (only (vicare platform utilities))

  (only (vicare unsafe capi))
  (only (vicare unsafe operations))
  (only (vicare unsafe unicode))

  (only (vicare numerics constants))
  (only (vicare arguments validation))
  (only (vicare syntactic-extensions))
  (only (vicare arguments general-c-buffers))
  (only (vicare cond-expand))
  (only (vicare cond-expand helpers))

  (only (vicare language-extensions amb))
  (only (vicare language-extensions coroutines))

  (only (vicare include))
  (only (vicare infix))
  (only (vicare flonum-parser))
  (only (vicare flonum-formatter))
  (only (vicare weak-hashtables))
  (only (vicare keywords))
  (only (vicare checks))
  (only (vicare custom-ports))
  (only (vicare bytevector-compounds core))
  (only (vicare bytevector-compounds))
  (only (vicare bytevector-compounds unsafe))
  (only (vicare net channels))

  (only (vicare assembler inspection))
  (only (vicare debugging compiler))
  )

;;; end of file
