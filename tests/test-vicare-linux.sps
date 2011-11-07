;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test implementation of GNU+Linux functions
;;;Date: Mon Nov  7, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rename (vicare) #;(ikarus)
		(parameterize	parametrise))
  (vicare platform-constants)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare GNU+Linux functions\n")


(parametrise ((check-test-name	'termination-status))

  (check
      (let* ((child_pid #f)
	     (info      (fork (lambda (pid) ;parent
				(set! child_pid pid)
				(waitid P_PID pid WEXITED))
			      (lambda () ;child
				(exit 10)))))
	(list (= child_pid (siginfo_t-si_pid info))
	      (fixnum? (siginfo_t-si_uid info))
	      (siginfo_t-si_status  info)
	      (siginfo_t-si_signo   info)
	      (siginfo_t-si_code    info)))
	=> `(#t #t 10 ,SIGCLD ,CLD_EXITED))

;;; --------------------------------------------------------------------

  (check
      (let ((status (system "exit 0")))
	(WIFCONTINUED status))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
