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
;;;Copyright (c) 2010, 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (vicare) #;(ikarus)
  (prefix (vicare linux)
	  linux.)
  (prefix (vicare posix)
	  px.)
  (vicare platform-constants)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare GNU+Linux functions\n")


(parametrise ((check-test-name	'termination-status))

  (check
      (let* ((child_pid #f)
	     (info      (px.fork (lambda (pid) ;parent
				   (set! child_pid pid)
				   (linux.waitid P_PID pid WEXITED))
				 (lambda () ;child
				   (exit 10)))))
	(list (= child_pid (linux.struct-siginfo_t-si_pid info))
	      (fixnum? (linux.struct-siginfo_t-si_uid info))
	      (linux.struct-siginfo_t-si_status  info)
	      (linux.struct-siginfo_t-si_signo   info)
	      (linux.struct-siginfo_t-si_code    info)))
    => `(#t #t 10 ,SIGCLD ,CLD_EXITED))

;;; --------------------------------------------------------------------

  (check
      (let ((status (px.system "exit 0")))
	(linux.WIFCONTINUED status))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
