;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test implementation of POSIX per-process timers
;;;Date: Sat Jul 14, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (prefix (vicare posix)
	  px.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare POSIX per-process timers\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax catch-error
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((error? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define (log T)
  (let ((interval	(px.struct-itimerspec-it_interval T))
	(value		(px.struct-itimerspec-it_value    T)))
    (list (px.struct-timespec-tv_sec  interval)
	  (px.struct-timespec-tv_nsec interval)
	  (px.struct-timespec-tv_sec  value)
	  (px.struct-timespec-tv_nsec value))))


(parametrise ((check-test-name	'settime))

  (check	;settime with OLD argument
      (let* ((fd	(px.timer-create CLOCK_REALTIME
					 (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	     (new	(px.make-struct-itimerspec
			 (px.make-struct-timespec 1 2)
			 (px.make-struct-timespec 0 0)))
	     (old	(px.make-struct-itimerspec
			 (px.make-struct-timespec 0 0)
			 (px.make-struct-timespec 0 0)))
	     (result	(unwind-protect
			    (px.timer-settime fd 0 new old)
			  (px.close fd))))
	(and (eq? old result)
	     (list (log new) (log old))))
    ;;Notice that the old configuration is empty!!!
    => '((1 2 0 0) (0 0 0 0)))

  (check	;settime twice with OLD argument
      (let* ((fd	(px.timer-create CLOCK_REALTIME
					 (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	     (new	(px.make-struct-itimerspec
	     		 (px.make-struct-timespec 1 2)
	     		 (px.make-struct-timespec 0 0)))
	     (older	(px.make-struct-itimerspec
	     		 (px.make-struct-timespec 0 0)
	     		 (px.make-struct-timespec 0 0)))
	     (newer	(px.make-struct-itimerspec
	     		 (px.make-struct-timespec 5 6)
	     		 (px.make-struct-timespec 0 0)))
	     (old	(px.make-struct-itimerspec
	     		 (px.make-struct-timespec 0 0)
	     		 (px.make-struct-timespec 0 0)))
	     (result1	(px.timer-settime fd TFD_TIMER_ABSTIME new older))
	     (result2	(unwind-protect
	     		    (px.timer-settime fd TFD_TIMER_ABSTIME newer old)
			  (px.close fd))))
	(and (eq? older result1)
	     (eq? old   result2)
	     (list (log new) (log older)
		   (log newer) (log old))))
    => '((1 2 0 0) (0 0 0 0)
	 (5 6 0 0) (1 2 0 0)))

  (check	;settime without OLD argument
      (let ((fd  (px.timer-create CLOCK_REALTIME (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	    (new (px.make-struct-itimerspec
		  (px.make-struct-timespec 1 0)
		  (px.make-struct-timespec 1 0))))
	(unwind-protect
	    (px.timer-settime fd 0 new)
	  (px.close fd)))
    => #f)

  #t)


(parametrise ((check-test-name	'gettime))

  (check	;settime then gettime with CURR argument
      (let ((fd   (px.timer-create CLOCK_REALTIME (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	    (new  (px.make-struct-itimerspec
		   (px.make-struct-timespec 1 2)
		   (px.make-struct-timespec 0 0)))
	    (curr (px.make-struct-itimerspec
		   (px.make-struct-timespec 0 0)
 		   (px.make-struct-timespec 0 0))))
	(unwind-protect
	    (begin
	      (px.timer-settime fd 0 new)
	      (px.timer-gettime fd curr))
	  (px.close fd))
	(log curr))
    => '(1 2 0 0))

  (check	;settime then gettime without CURR argument
      (let* ((fd   (px.timer-create CLOCK_REALTIME (bitwise-ior TFD_CLOEXEC TFD_NONBLOCK)))
	     (new  (px.make-struct-itimerspec
		    (px.make-struct-timespec 1 2)
		    (px.make-struct-timespec 0 0)))
	     (curr (unwind-protect
		       (begin
			 (px.timer-settime fd 0 new)
			 (px.timer-gettime fd))
		     (px.close fd))))
	(log curr))
    => '(1 2 0 0))

  #t)


;;;; done

(flush-output-port (current-output-port))
(flush-output-port (current-error-port))

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-temporary-file 'scheme-indent-function 1)
;; eval: (put 'catch 'scheme-indent-function 1)
;; eval: (put 'catch-error 'scheme-indent-function 1)
;; End:
