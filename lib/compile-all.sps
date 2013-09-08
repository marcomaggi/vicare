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

  (only (vicare language-extensions cond-expand))
  (only (vicare language-extensions cond-expand helpers))

  (only (vicare arguments validation))
  (only (vicare arguments general-c-buffers))

  (only (vicare language-extensions syntaxes))
  (only (vicare language-extensions amb))
  (only (vicare language-extensions simple-match))
  (only (vicare language-extensions coroutines))
  (only (vicare language-extensions infix))
  (only (vicare language-extensions keywords))
  (only (vicare language-extensions sentinels))
  (only (vicare language-extensions namespaces))
  (only (vicare language-extensions custom-ports))
  (only (vicare language-extensions variables))
  (only (vicare language-extensions streams))
  (only (vicare language-extensions loops))

  (only (vicare numerics constants))
  (only (vicare numerics flonum-parser))
  (only (vicare numerics flonum-formatter))

  (only (vicare checks))

  (only (vicare bytevectors))

  (only (vicare containers auxiliary-syntaxes))
  (only (vicare containers weak-hashtables))
  (only (vicare containers knuth-morris-pratt))
  (only (vicare containers bytevector-compounds core))
  (only (vicare containers bytevector-compounds))
  (only (vicare containers bytevector-compounds unsafe))
  (only (vicare containers char-sets))
  (only (vicare containers char-sets blocks))
  (only (vicare containers char-sets categories))
  (only (vicare containers lists stx))
  (only (vicare containers lists low))
  (only (vicare containers lists))
  (only (vicare containers vectors low))
  (only (vicare containers vectors))
  (only (vicare containers strings low))
  (only (vicare containers strings))
  (only (vicare containers strings rabin-karp))
  (only (vicare containers levenshtein))

  (only (vicare net channels))

  (only (vicare assembler inspection))
  (only (vicare debugging compiler))


  (only (vicare irregex))
  (only (vicare pregexp))

  )

;;; end of file
