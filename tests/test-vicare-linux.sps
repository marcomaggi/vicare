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
;;;Copyright (c) 2010, 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (vicare linux)
	  lx.)
  (prefix (vicare posix)
	  px.)
  (vicare platform constants)
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare GNU+Linux functions\n")


;;;; helpers

(define-syntax with-temporary-file
  (syntax-rules ()
    ((_ (?pathname ?fd) . ?body)
     (let ((ptn ?pathname))
       (when (file-exists? ptn)
	 (delete-file ptn))
       (let ((?fd (px.open ptn (fxior O_CREAT O_EXCL O_RDWR) (fxior S_IRUSR S_IWUSR))))
	 (unwind-protect
	     (begin . ?body)
	   (px.close ?fd)
	   (delete-file ptn)))))))


(parametrise ((check-test-name	'cond-expand))

  (check
      (lx.cond-expand
       (lx.waitid #t)
       (else #f))
    => #t)

  #t)


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


(parametrise ((check-test-name	'resources))

  (check
      (let ((rlim (lx.prlimit (px.getpid) RLIMIT_SIGPENDING)))
;;;	(check-pretty-print rlim)
	(lx.struct-rlimit? rlim))
    => #t)

  (check
      (let* ((pid  (px.getpid))
	     (rlim (lx.prlimit pid RLIMIT_SIGPENDING)))
;;;	(check-pretty-print rlim)
	(let ((old (lx.prlimit pid RLIMIT_SIGPENDING rlim)))
;;;	  (check-pretty-print old)
	  (lx.struct-rlimit? old)))
    => #t)

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


(parametrise ((check-test-name	'timerfd))

  (define (log T)
    (let ((interval	(lx.struct-itimerspec-it_interval T))
	  (value	(lx.struct-itimerspec-it_value    T)))
      (list (lx.struct-timespec-tv_sec  interval)
	    (lx.struct-timespec-tv_nsec interval)
	    (lx.struct-timespec-tv_sec  value)
	    (lx.struct-timespec-tv_nsec value))))

;;; --------------------------------------------------------------------
;;; SETTIME tests

  (check	;settime with OLD argument
      (let* ((fd	(lx.timerfd-create CLOCK_REALTIME
					   (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	     (new	(lx.make-struct-itimerspec
			 (lx.make-struct-timespec 1 2)
			 (lx.make-struct-timespec 0 0)))
	     (old	(lx.make-struct-itimerspec
			 (lx.make-struct-timespec 0 0)
			 (lx.make-struct-timespec 0 0)))
	     (result	(unwind-protect
			    (lx.timerfd-settime fd 0 new old)
			  (px.close fd))))
	(and (eq? old result)
	     (list (log new) (log old))))
    ;;Notice that the old configuration is empty!!!
    => '((1 2 0 0) (0 0 0 0)))

  (check	;settime twice with OLD argument
      (let* ((fd	(lx.timerfd-create CLOCK_REALTIME
					   (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	     (new	(lx.make-struct-itimerspec
	     		 (lx.make-struct-timespec 1 2)
	     		 (lx.make-struct-timespec 0 0)))
	     (older	(lx.make-struct-itimerspec
	     		 (lx.make-struct-timespec 0 0)
	     		 (lx.make-struct-timespec 0 0)))
	     (newer	(lx.make-struct-itimerspec
	     		 (lx.make-struct-timespec 5 6)
	     		 (lx.make-struct-timespec 0 0)))
	     (old	(lx.make-struct-itimerspec
	     		 (lx.make-struct-timespec 0 0)
	     		 (lx.make-struct-timespec 0 0)))
	     (result1	(lx.timerfd-settime fd TFD_TIMER_ABSTIME new older))
	     (result2	(unwind-protect
	     		    (lx.timerfd-settime fd TFD_TIMER_ABSTIME newer old)
			  (px.close fd))))
	(and (eq? older result1)
	     (eq? old   result2)
	     (list (log new) (log older)
		   (log newer) (log old))))
    => '((1 2 0 0) (0 0 0 0)
	 (5 6 0 0) (1 2 0 0)))

  (check	;settime without OLD argument
      (let ((fd  (lx.timerfd-create CLOCK_REALTIME (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	    (new (lx.make-struct-itimerspec
		  (lx.make-struct-timespec 1 0)
		  (lx.make-struct-timespec 1 0))))
	(unwind-protect
	    (lx.timerfd-settime fd 0 new)
	  (px.close fd)))
    => #f)

;;; --------------------------------------------------------------------
;;; GETTIME tests

  (check	;settime then gettime with CURR argument
      (let ((fd   (lx.timerfd-create CLOCK_REALTIME (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	    (new  (lx.make-struct-itimerspec
		   (lx.make-struct-timespec 1 2)
		   (lx.make-struct-timespec 0 0)))
	    (curr (lx.make-struct-itimerspec
		   (lx.make-struct-timespec 0 0)
 		   (lx.make-struct-timespec 0 0))))
	(unwind-protect
	    (begin
	      (lx.timerfd-settime fd 0 new)
	      (lx.timerfd-gettime fd curr))
	  (px.close fd))
	(log curr))
    => '(1 2 0 0))

  (check	;settime then gettime without CURR argument
      (let* ((fd   (lx.timerfd-create CLOCK_REALTIME (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	     (new  (lx.make-struct-itimerspec
		    (lx.make-struct-timespec 1 2)
		    (lx.make-struct-timespec 0 0)))
	     (curr (unwind-protect
		       (begin
			 (lx.timerfd-settime fd 0 new)
			 (lx.timerfd-gettime fd))
		     (px.close fd))))
	(log curr))
    => '(1 2 0 0))

  #t)


(parametrise ((check-test-name	'inotify))

;;; initialise and close

  (check
      (let ((fd (lx.inotify-init)))
	(unwind-protect
	    (fixnum? fd)
	  (px.close fd)))
    => #t)

  (check
      (let ((fd (lx.inotify-init1 IN_NONBLOCK)))
	(unwind-protect
	    (fixnum? fd)
	  (px.close fd)))
    => #t)

;;; --------------------------------------------------------------------
;;; watcher creation and removal

  (check
      (let ((fd		(lx.inotify-init))
	    (ptn	"inotify.test"))
	(with-temporary-file (ptn fd1)
	  (let ((wd (lx.inotify-add-watch fd ptn IN_MODIFY)))
	    (unwind-protect
		(integer? wd)
	      (lx.inotify-rm-watch fd wd)))))
    => #t)

;;; --------------------------------------------------------------------
;;; watcher event reading

  (check
      (let ((infd	(lx.inotify-init))
	    (ptn	"inotify.test"))
	(with-temporary-file (ptn fd)
	  (let ((wd (lx.inotify-add-watch infd ptn IN_MODIFY)))
	    (unwind-protect
		(begin
		  (px.write fd #vu8(1 2 3))
		  (let-values (((r w x) (px.select-fd infd 1 0)))
		    (let ((ev (lx.inotify-read infd)))
		      (and (lx.struct-inotify-event? ev)
			   (= wd (lx.struct-inotify-event-wd ev))
			   (zero? (lx.struct-inotify-event-len ev))
			   (not (lx.struct-inotify-event-name ev))))))
	      (lx.inotify-rm-watch infd wd)))))
    => #t)

  #t)


(parametrise ((check-test-name	'ether))

  (define-constant eth0.str "20:6a:8a:f6:b5:ed")

;;; --------------------------------------------------------------------

  (check
      (bytevector? (lx.ether-aton eth0.str #f))
    => #t)

  (check
      (ascii->string (lx.ether-ntoa (lx.ether-aton eth0.str #f)))
    => eth0.str)

  (check
      (lx.ether-ntoa/string (lx.ether-aton eth0.str #f))
    => eth0.str)

;;; --------------------------------------------------------------------

  (check
      (bytevector? (lx.ether-aton-r eth0.str #f))
    => #t)

  (check
      (ascii->string (lx.ether-ntoa-r (lx.ether-aton-r eth0.str #f)))
    => eth0.str)

  (check
      (lx.ether-ntoa-r/string (lx.ether-aton-r eth0.str #f))
    => eth0.str)

;;; --------------------------------------------------------------------

  (check
      (receive (addr hostname)
	  (lx.ether-line "20:6a:8a:f6:b5:ed localhost" #f)
	(ascii->string hostname))
    => "localhost")

  (check
      (receive (addr hostname)
	  (lx.ether-line/string "20:6a:8a:f6:b5:ed localhost" #f)
	hostname)
    => "localhost")

  (check
      (receive (addr hostname)
	  (lx.ether-line/string "20:6a:8a:f6:b5:ed localhost" #f)
	(list (lx.ether-ntoa/string addr) hostname))
    => '("20:6a:8a:f6:b5:ed" "localhost"))

;;; --------------------------------------------------------------------

  (when (file-exists? "/etc/ethers")

    (check
	(lx.ether-ntoa/string (lx.ether-hostton "localhost" #f))
      => eth0.str)

    (check
	(lx.ether-ntohost/string (lx.ether-hostton "localhost" #f))
      => "localhost")

    (check
	(lx.ether-ntohost/string (lx.ether-aton eth0.str #f))
      => "localhost")

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-local-storage 'scheme-indent-function 1)
;; End:
