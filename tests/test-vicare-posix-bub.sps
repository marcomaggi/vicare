;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare posix) BUB API
;;;Date: Tue Jul 17, 2012
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (prefix (vicare posix) px.)
  (vicare platform constants)
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, interprocess signals BUB API\n")


(parametrise ((check-test-name	'bub))

;;; use raise

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.raise SIGUSR1)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#t #f))

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#f #f))

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.raise SIGUSR2)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#f #t))

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.raise SIGUSR1)
	    (px.raise SIGUSR2)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#t #t))

;;; --------------------------------------------------------------------
;;; use kill

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.kill (px.getpid) SIGUSR1)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#t #f))

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#f #f))

  (check
      (unwind-protect
	(begin
	    (px.signal-bub-init)
	    (px.kill (px.getpid) SIGUSR2)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#f #t))

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.kill (px.getpid) SIGUSR1)
	    (px.kill (px.getpid) SIGUSR2)
	    (px.signal-bub-acquire)
	    (list (px.signal-bub-delivered? SIGUSR1)
		  (px.signal-bub-delivered? SIGUSR2)))
	(px.signal-bub-final))
    => '(#t #t))

;;; --------------------------------------------------------------------

  (check	;test clearing the flag
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.raise SIGUSR1)
	    (px.signal-bub-acquire)
	    (let ((res (px.signal-bub-delivered? SIGUSR1)))
	      (list res (px.signal-bub-delivered? SIGUSR2))))
	(px.signal-bub-final))
    => '(#t #f))

;;; --------------------------------------------------------------------

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.raise SIGUSR1)
	    (px.signal-bub-acquire)
	    (px.signal-bub-all-delivered))
	(px.signal-bub-final))
    => `(,SIGUSR1))

  (check
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (px.raise SIGUSR1)
	    (px.raise SIGUSR2)
	    (px.signal-bub-acquire)
	    (px.signal-bub-all-delivered))
	(px.signal-bub-final))
    => `(,SIGUSR1 ,SIGUSR2))

  #t)


;;;; done

(check-report)

;;; end of file
