;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for multidimensional arrays
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
  (prefix (vicare containers arrays) arrays.)
  (vicare arguments validation)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: multidimensional arrays\n")


;;;; helpers

(define who 'test)


(parametrise ((check-test-name	'coordinates))

  (check
      (arrays.coordinate? 123)
    => #t)

  (check
      (arrays.coordinate? 0)
    => #t)

  (check
      (arrays.coordinate? -123)
    => #f)

;;; --------------------------------------------------------------------

  (check-for-true
   (with-arguments-validation (who)
       ((arrays.coordinate		123))
     #t))

  (check-for-procedure-argument-violation
      (with-arguments-validation (who)
	  ((arrays.coordinate	-123))
	#f)
    '(-123))

  (check-for-procedure-argument-violation
      (with-arguments-validation (who)
	  ((arrays.list-of-coordinates	'(456 -123)))
	#f)
    '((456 -123)))

  (check-for-procedure-argument-violation
      (with-arguments-validation (who)
	  ((arrays.vector-of-coordinates	'#(456 -123)))
	#f)
    '(#(456 -123)))

  #t)


;;;; done

(check-report)

;;; end of file
