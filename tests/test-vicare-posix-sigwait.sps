;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare posix), sigwait functions
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
  (prefix (vicare posix)
	  px.)
  (vicare platform constants)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, sigwait functions\n")


(parametrise ((check-test-name	'sigwait))

  (check
      (let ()
	(px.signal-bub-init)
	(px.raise SIGALRM)
	(let-values (((signo info) (unwind-protect
				       (px.sigwaitinfo SIGALRM)
				     (px.signal-bub-final))))
	  (list signo (and (px.struct-siginfo_t? info)
			   (px.struct-siginfo_t-si_signo info)))))
    => (list SIGALRM SIGALRM))

  #t)


(parametrise ((check-test-name	'timed-wait))

  (check
      (let ()
	(px.signal-bub-init)
	(px.raise SIGUSR1)
	(let-values
	    (((signo info)
	      (unwind-protect
		  (px.sigtimedwait SIGUSR1
				   (px.make-struct-timespec 1 0))
		(px.signal-bub-final))))
	  (list signo (and (px.struct-siginfo_t? info)
			   (px.struct-siginfo_t-si_signo info)))))
    => (list SIGUSR1 SIGUSR1))

  #t)


;;;; done

(check-report)

;;; end of file
