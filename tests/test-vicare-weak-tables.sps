;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for weak hashtables
;;;Date: Fri Feb  3, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare weak-hashtables)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare weak hashtables\n")


(parametrise ((check-test-name	'base))

  (check
      (weak-hashtable? (make-weak-hashtable string-hash string=?))
    => #t)

  #t)


(parametrise ((check-test-name	'base))

  (check
      (let ((T (make-weak-hashtable string-hash string=?)))
        (weak-hashtable-set! T "a" 1)
        (weak-hashtable-set! T "b" 2)
        (weak-hashtable-set! T "c" 3)
        (weak-hashtable-set! T "d" 4)
        (weak-hashtable-set! T "e" 5)
        (weak-hashtable-set! T "f" 6)
	(list (weak-hashtable-ref T "a" #f)
	      (weak-hashtable-ref T "b" #f)
	      (weak-hashtable-ref T "c" #f)
	      (weak-hashtable-ref T "d" #f)
	      (weak-hashtable-ref T "e" #f)
	      (weak-hashtable-ref T "f" #f)
	      ))
    => '(1 2 3 4 5 6))

  #t)


;;;; done

(check-report)

;;; end of file
