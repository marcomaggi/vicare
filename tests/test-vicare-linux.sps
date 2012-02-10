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


(import (vicare)
  (prefix (vicare linux)
	  linux.)
  (prefix (vicare posix)
	  px.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
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


(parametrise ((check-test-name	'epoll))

  (check
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (let ((epfd (linux.epoll-create)))
	      (unwind-protect
		  (let ((sizeof-struct (vector (linux.epoll-event-size))))
		    (with-local-storage sizeof-struct
		      (lambda (event)
			(linux.epoll-event-set-events!  event 0 EPOLLIN)
			(linux.epoll-event-set-data-fd! event 0 in)
			(linux.epoll-ctl epfd EPOLL_CTL_ADD in event)))
		    (px.write ou '#vu8(1))
		    (with-local-storage sizeof-struct
		      (lambda (events)
			(linux.epoll-wait epfd events 1 -1)
			(list (fx= in (linux.epoll-event-ref-data-fd events 0))
			      (linux.epoll-event-ref-events events 0))
			)))
		(px.close epfd)))
	  (px.close in)
	  (px.close ou)))
    => `(#t ,EPOLLIN))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-local-storage 'scheme-indent-function 1)
;; End:
