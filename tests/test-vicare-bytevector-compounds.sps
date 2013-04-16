;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for bytevector-compound objects
;;;Date: Tue Apr 16, 2013
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
  (vicare bytevector-compounds)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: bytevector-compounds\n")


(parametrise ((check-test-name	'base))

  (define (doit . args)
    (let ((bvcom (apply make-bytevector-compound args)))
      (list (bytevector-compound-length bvcom)
	    (bytevector-compound-total-length bvcom)
	    (bytevector-compound-empty? bvcom)
	    (bytevector-compound-filled? bvcom))))

  (check
      (doit)
    => '(0 0 #t #f))

  (check
      (doit '#vu8(1 2 3))
    => '(3 3 #f #t))

  (check
      (doit '#vu8(1 2 3) '#vu8(4 5 6) '#vu8(7 8 9))
    => '(9 9 #f #t))

  #t)


;;;; done

(check-report)

;;; end of file
