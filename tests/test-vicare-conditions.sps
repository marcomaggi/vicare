;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for vicare condition types
;;;Date: Thu Sep 19, 2013
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
(check-display "*** testing Vicare libraries: condition types\n")


(parametrise ((check-test-name	'procedure-argument-violation))

  (check
      (procedure-argument-violation? (make-procedure-argument-violation))
    => #t)

  (check
      (assertion-violation? (make-procedure-argument-violation))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((procedure-argument-violation? E)
		 (condition-who E))
		(else E))
	(procedure-argument-violation 'ciao "message" 1 2 3))
    => 'ciao)

  (check
      (guard (E ((procedure-argument-violation? E)
		 (condition-message E))
		(else E))
	(procedure-argument-violation 'ciao "message" 1 2 3))
    => "message")

  (check
      (guard (E ((procedure-argument-violation? E)
		 (condition-irritants E))
		(else E))
	(procedure-argument-violation 'ciao "message" 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'expression-return-value-violation))

  (check
      (expression-return-value-violation? (make-expression-return-value-violation))
    => #t)

  (check
      (assertion-violation? (make-expression-return-value-violation))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((expression-return-value-violation? E)
		 (condition-who E))
		(else E))
	(expression-return-value-violation 'ciao "message" 1 2 3))
    => 'ciao)

  (check
      (guard (E ((expression-return-value-violation? E)
		 (condition-message E))
		(else E))
	(expression-return-value-violation 'ciao "message" 1 2 3))
    => "message")

  (check
      (guard (E ((expression-return-value-violation? E)
		 (condition-irritants E))
		(else E))
	(expression-return-value-violation 'ciao "message" 1 2 3))
    => '(1 2 3))

  #t)


;;;; done

(check-report)

;;; end of file
