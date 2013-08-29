;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the syntax utility functions
;;;Date: Thu Aug 29, 2013
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
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare functions: syntax utilities\n")


(parametrise ((check-test-name	'pairs))

  (check
      (syntax-car #'(display . 1))
    (=> bound-identifier=?)
    #'display)

;;; --------------------------------------------------------------------

  (check
      (syntax-cdr #'(1 . display))
    (=> bound-identifier=?)
    #'display)

;;; --------------------------------------------------------------------

  (check
      (for-all bound-identifier=?
	(syntax->list #'(display write))
	(list #'display #'write))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
