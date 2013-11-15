;;;
;;;Part of: Vicare/Scheme
;;;Contents: tests for the sentinel library
;;;Date: Tue Jul  7, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare language-extensions sentinels)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: sentinels\n")


(parametrise ((check-test-name	'sentinel))

  (check
      (sentinel? sentinel)
    => #t)

  (check
      (sentinel? 123)
    => #f)

  (check
      (let ((ell (list 1 2 3 4 5 sentinel)))
	(let loop ((ell ell)
		   (res '()))
	  (if (sentinel? (car ell))
	      res
	    (loop (cdr ell) (cons (car ell) res)))))
    => '(5 4 3 2 1))

  (check
      (let* ((ell  '(1 2 3 4 5))
	     (iter (let ((ell ell))
		     (lambda ()
		       (if (null? ell)
			   sentinel
			 (begin0
			   (car ell)
			   (set! ell (cdr ell))))))))
	(let loop ((res '()))
	  (let ((v (iter)))
	    (if (sentinel? v)
		res
	      (loop (cons v res))))))
    => '(5 4 3 2 1))

  (let ((s (make-sentinel)))
    (check
	(sentinel? s)
      => #t)

    (check
	(eq? s s)
      => #t)

    (check
	(eq? s sentinel)
      => #f))

  #t)


(parametrise ((check-test-name	'unspecified))

  (check-for-true
   (unspecified? unspecified))

  (check-for-false
   (specified? unspecified))

  (check-for-false
   (unspecified? 123))

  (check-for-true
   (specified? 123))

  (check
      unspecified
    => unspecified)

  #t)


(parametrise ((check-test-name	'undefined))

  (check-for-true
   (undefined? undefined))

  (check-for-false
   (defined? undefined))

  (check-for-false
   (undefined? 123))

  (check-for-true
   (defined? 123))

  (check
      undefined
    => undefined)

  #t)


;;;; done

(check-report)

;;; end of file
