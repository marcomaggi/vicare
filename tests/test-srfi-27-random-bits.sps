;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 27: random bits
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
  (prefix (srfi :27) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI 27: random bits\n")


(parametrise ((check-test-name	'random-numbers))

  (check
      (integer? (srfi.random-integer 10))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (flonum? (srfi.random-real))
    => #t)

  #t)


(parametrise ((check-test-name	'sources))

  (check
      (srfi.random-source? srfi.default-random-source)
    => #t)

  (check
      (let ((S (srfi.make-random-source)))
	(srfi.random-source? S))
    => #t)

  (check
      (let ((S (srfi.make-random-source)))
	(srfi.random-source-randomize! S)
	(integer? ((srfi.random-source-make-integers S) 10)))
    => #t)

  (check
      (let ((S (srfi.make-random-source)))
	(flonum? ((srfi.random-source-make-reals S))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
