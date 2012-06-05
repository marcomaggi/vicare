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
	  lx.)
  (prefix (vicare posix)
	  px.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare GNU+Linux functions\n")


(parametrise ((check-test-name	'termination-status))

  (check
      (let* ((child_pid #f)
	     (info      (px.fork (lambda (pid) ;parent
				   (set! child_pid pid)
				   (lx.waitid P_PID pid WEXITED))
				 (lambda () ;child
				   (exit 10)))))
	(list (= child_pid (lx.struct-siginfo_t-si_pid info))
	      (fixnum? (lx.struct-siginfo_t-si_uid info))
	      (lx.struct-siginfo_t-si_status  info)
	      (lx.struct-siginfo_t-si_signo   info)
	      (lx.struct-siginfo_t-si_code    info)))
    => `(#t #t 10 ,SIGCLD ,CLD_EXITED))

;;; --------------------------------------------------------------------

  (check
      (let ((status (px.system "exit 0")))
	(lx.WIFCONTINUED status))
    => #f)

  #t)


(parametrise ((check-test-name	'epoll))

  (check
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (let ((epfd (lx.epoll-create)))
	      (unwind-protect
		  (let ((sizeof-struct (vector (lx.epoll-event-size))))
		    (with-local-storage sizeof-struct
		      (lambda (event)
			(lx.epoll-event-set-events!  event 0 EPOLLIN)
			(lx.epoll-event-set-data-fd! event 0 in)
			(lx.epoll-ctl epfd EPOLL_CTL_ADD in event)))
		    (px.write ou '#vu8(1))
		    (with-local-storage sizeof-struct
		      (lambda (events)
			(lx.epoll-wait epfd events 1 -1)
			(list (fx= in (lx.epoll-event-ref-data-fd events 0))
			      (lx.epoll-event-ref-events events 0))
			)))
		(px.close epfd)))
	  (px.close in)
	  (px.close ou)))
    => `(#t ,EPOLLIN))

  #t)


(parametrise ((check-test-name	'signalfd))

  (check	;no signal pending
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (let* ((mask (vector SIGUSR1 SIGUSR2))
		   (fd   (lx.signalfd -1 mask (fxior SFD_CLOEXEC SFD_NONBLOCK))))
	      (unwind-protect
		  (lx.read-signalfd-siginfo fd)
		(px.close fd))))
	(px.signal-bub-final))
    => #f)

  (check	;one signal pending
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (let* ((mask (vector SIGUSR1 SIGUSR2))
		   (fd   (lx.signalfd -1 mask (fxior SFD_CLOEXEC SFD_NONBLOCK))))
	      (unwind-protect
		  (begin
		    (px.raise SIGUSR1)
		    (let ((info (lx.read-signalfd-siginfo fd))
			  (done (lx.read-signalfd-siginfo fd)))
;;;		      (check-pretty-print info)
		      (list (lx.struct-signalfd-siginfo? info)
			    (lx.struct-signalfd-siginfo-ssi_signo info)
			    done)))
		(px.close fd))))
	(px.signal-bub-final))
    => `(#t ,SIGUSR1 #f))

  (check	;two signals pending
      (unwind-protect
	  (begin
	    (px.signal-bub-init)
	    (let* ((mask (vector SIGUSR1 SIGUSR2))
		   (fd   (lx.signalfd -1 mask (fxior SFD_CLOEXEC SFD_NONBLOCK))))
	      (unwind-protect
		  (begin
		    (px.raise SIGUSR1)
		    (px.raise SIGUSR2)
		    (let* ((info1 (lx.read-signalfd-siginfo fd))
			   (info2 (lx.read-signalfd-siginfo fd))
			   (info3 (lx.read-signalfd-siginfo fd)))
		      (list (lx.struct-signalfd-siginfo-ssi_signo info1)
			    (lx.struct-signalfd-siginfo-ssi_signo info2)
			    info3)))
		(px.close fd))))
	(px.signal-bub-final))
    => `(,SIGUSR1 ,SIGUSR2 #f))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-local-storage 'scheme-indent-function 1)
;; End:
