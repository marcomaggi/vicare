;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the simple event loop (SEL)
;;;Date: Tue Feb 21, 2012
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
  (prefix (vicare posix) px.)
  (prefix (vicare simple-event-loop) sel.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare simple event loop\n")


(parametrise ((check-test-name	'fds))

  (define (%send who fd data-string)
    (add-result `(,who send ,data-string))
    (px.write fd (string->ascii data-string)))

  (define (%recv who fd)
    (let* ((buf (make-bytevector 1024))
	   (len (px.read fd buf 1024)))
      (add-result `(,who recv ,(ascii->string (subbytevector-u8 buf 0 len))))))

  (check
      (with-result
       (let-values (((master slave) (px.socketpair PF_LOCAL SOCK_DGRAM 0)))
	 (unwind-protect
	     (begin
	       (sel.initialise)
	       (sel.writable master
		 (lambda ()
		   (%send 'master master "helo slave\n")
		   (sel.readable master
		     (lambda ()
		       (%recv 'master master)
		       (sel.writable master
			 (lambda ()
			   (%send 'master master "bye slave\n")
			   (sel.readable master
			     (lambda ()
			       (%recv 'master master)
			       (sel.leave-asap)))))))))
	       (sel.readable slave
		 (lambda ()
		   (%recv 'slave slave)
		   (sel.writable slave
		     (lambda ()
		       (%send 'slave slave "helo master\n")
		       (sel.readable slave
			 (lambda ()
			   (%recv 'slave slave)
			   (sel.writable slave
			     (lambda ()
			       (%send 'slave slave "bye master\n")))))))))
	       (sel.enter)
	       #t)
	   (px.shutdown master SHUT_RDWR)
	   (px.shutdown slave  SHUT_RDWR)
	   (sel.finalise))))
    => '(#t
	 ((master send "helo slave\n")
	  (slave  recv "helo slave\n")
	  (slave  send "helo master\n")
	  (master recv "helo master\n")
	  (master send "bye slave\n")
	  (slave  recv "bye slave\n")
	  (slave  send "bye master\n")
	  (master recv "bye master\n"))))

  (check	;forgetting
      (unwind-protect
	  (begin
	    (sel.initialise)
	    (sel.readable 0 (lambda () #f))
	    (sel.writable 1 (lambda () #f))
	    (sel.forget-fd 0)
	    (sel.forget-fd 1)
	    (sel.busy?))
	(sel.finalise))
    => #f)

  #t)


(parametrise ((check-test-name	'signals))

  (check
      (with-result
       (unwind-protect
	   (begin
	     (sel.initialise)
	     (sel.receive-signal SIGUSR1
	       (lambda ()
		 (add-result '(signal SIGUSR1))
		 (sel.leave-asap)))
	     (sel.receive-signal SIGUSR2
	       (lambda ()
		 (add-result '(signal SIGUSR2))
		 (sel.leave-asap)))
	     (px.raise SIGUSR1)
	     (sel.enter)
	     #t)
	 (sel.finalise)))
    => '(#t
	 ((signal SIGUSR1))))

  #t)


(parametrise ((check-test-name	'tasks))

  (check
      (with-result
       (unwind-protect
	   (begin
	     (sel.initialise)
	     (sel.task-fragment (lambda ()
				  (add-result '(task-1 1))
				  (lambda ()
				    (add-result '(task-1 2))
				    (lambda ()
				      (add-result '(task-1 3))
				      #f))))
	     (sel.task-fragment (lambda ()
				  (add-result '(task-2 1))
				  (lambda ()
				    (add-result '(task-2 2))
				    (lambda ()
				      (add-result '(task-2 3))
				      (sel.leave-asap)
				      #f))))
	     (sel.enter)
	     (sel.busy?))
	 (sel.finalise)))
    => '(#f
	 ((task-1 1)
	  (task-2 1)
	  (task-1 2)
	  (task-2 2)
	  (task-1 3)
	  (task-2 3))))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'sel.readable 'scheme-indent-function 1)
;; eval: (put 'sel.writable 'scheme-indent-function 1)
;; eval: (put 'sel.receive-signal 'scheme-indent-function 1)
;; End:
