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
  (vicare platform constants)
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, per-process timers\n")


;;;; helpers

(define verbose-checks? #f)

(define (verbose-printf . args)
  (when verbose-checks?
    (apply printf args)
    (flush-output-port (current-output-port))))

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

(define (tlog T)
  (let ((interval	(px.struct-itimerspec-it_interval T))
	(value		(px.struct-itimerspec-it_value    T)))
    (list (px.struct-timespec-tv_sec  interval)
	  (px.struct-timespec-tv_nsec interval)
	  (px.struct-timespec-tv_sec  value)
	  (px.struct-timespec-tv_nsec value))))

(define (tlog2 T)
  (let ((interval	(px.struct-itimerspec-it_interval T))
	(value		(px.struct-itimerspec-it_value    T)))
    (list (px.struct-timespec-tv_sec  interval)
	  (px.struct-timespec-tv_nsec interval)
	  (px.struct-timespec-tv_sec  value)
	  (fixnum? (px.struct-timespec-tv_nsec value)))))


(parametrise ((check-test-name	'settime))

  (check	;settime with OLD argument
      (let* ((timer-id	(px.timer-create CLOCK_REALTIME))
	     (new	(px.make-struct-itimerspec
			 (px.make-struct-timespec 1 2)
			 (px.make-struct-timespec 10 99999999)))
	     (old	(px.make-struct-itimerspec))
	     (result	(unwind-protect
			    (px.timer-settime timer-id 0 new old)
			  (px.timer-delete timer-id))))
	(and (eq? old result)
	     (list (tlog new) (tlog old))))
    ;;Notice that the old configuration is empty!!!
    => '((1 2 10 99999999) (0 0 0 0)))

  (check	;settime twice with OLD argument
      (let* ((timer-id	(px.timer-create CLOCK_REALTIME))
	     (new	(px.make-struct-itimerspec
	     		 (px.make-struct-timespec 1 2)
	     		 (px.make-struct-timespec 10 99999999)))
	     (older	(px.make-struct-itimerspec))
	     (newer	(px.make-struct-itimerspec
	     		 (px.make-struct-timespec 5 6)
	     		 (px.make-struct-timespec 20 99999999)))
	     (old	(px.make-struct-itimerspec))
	     (result1	(px.timer-settime timer-id 0 new older))
	     (result2	(unwind-protect
	     		    (px.timer-settime timer-id 0 newer old)
			  (px.timer-delete timer-id))))
	(and (eq? older result1)
	     (eq? old   result2)
	     (list (tlog new)   (tlog  older)
		   (tlog newer) (tlog2 old))))
    => '((1 2 10 99999999) (0 0 0 0)
	 (5 6 20 99999999) (1 2 10 #t)))

  (check	;settime without OLD argument
      (let ((timer-id	(px.timer-create CLOCK_REALTIME))
	    (new	(px.make-struct-itimerspec)))
	(unwind-protect
	    (px.timer-settime timer-id 0 new)
	  (px.timer-delete timer-id)))
    => #f)

  #t)


(parametrise ((check-test-name	'gettime))

  (check	;settime then gettime with CURR argument
      (let ((timer-id	(px.timer-create CLOCK_REALTIME))
	    (new	(px.make-struct-itimerspec
			 (px.make-struct-timespec 1 2)
			 (px.make-struct-timespec 10 99999999)))
	    (curr	(px.make-struct-itimerspec)))
	(unwind-protect
	    (begin
	      (px.timer-settime timer-id 0 new)
	      (px.timer-gettime timer-id curr))
	  (px.timer-delete timer-id))
	(tlog2 curr))
    => '(1 2 10 #t))

  (check	;settime then gettime without CURR argument
      (let* ((timer-id	(px.timer-create CLOCK_REALTIME))
	     (new	(px.make-struct-itimerspec
			 (px.make-struct-timespec 1 2)
			 (px.make-struct-timespec 10 99999999)))
	     (curr	(unwind-protect
			    (begin
			      (px.timer-settime timer-id 0 new)
			      (px.timer-gettime timer-id))
			  (px.timer-delete timer-id))))
	(tlog2 curr))
    => '(1 2 10 #t))

  #t)


(parametrise ((check-test-name	'expirations))

  (define (%print-remaining-time timer-id)
    (verbose-printf "remaining time: ~a\n"
		    (px.struct-itimerspec-it_value (px.timer-gettime timer-id))))

  (define (%sleep-one-second)
    (px.nanosleep 1 0))

  (check	;follow time going by, BUB acquisition
      (let ((timer-id (px.timer-create CLOCK_REALTIME)))
	(px.signal-bub-init)
	(unwind-protect
	    (let ( ;; one event every 3 seconds
		  (period	(px.make-struct-timespec 3 0))
		  ;; the first event after 1 nanosecond
		  (offset	(px.make-struct-timespec 0 1))
		  (count	0))
	      (px.timer-settime timer-id 0
				(px.make-struct-itimerspec period offset))
	      (do ((i 0 (fx+ 1 i)))
		  ((fx= i 6)
		   count)
		(px.signal-bub-acquire)
		(let ((delivered? (px.signal-bub-delivered? SIGALRM)))
		  (when delivered?
		    (set! count (+ 1 count)))
		  (verbose-printf "received SIGALRM? ~a\n" delivered?))
		(verbose-printf "number of expirations: ~a\n" (px.timer-getoverrun timer-id))
		(%print-remaining-time timer-id)
		(%sleep-one-second)))
	  (px.timer-delete timer-id)
	  (px.signal-bub-final)))
    => 2)

  (check	;follow time going by, sigwaitinfo acquisition
      (let ((timer-id (px.timer-create CLOCK_REALTIME)))
	(px.signal-bub-init)
	(px.timer-settime timer-id 0
			  (px.make-struct-itimerspec
			   ;;one event every 1 seconds
			   (px.make-struct-timespec 1 0)
			   ;;the first event after 1 nanosecond
			   (px.make-struct-timespec 0 1)))
	(unwind-protect
	      (let next ((count 0))
		(if (fx= count 3)
		    count
		  (let-values (((signo info)
				(px.sigtimedwait SIGALRM (px.make-struct-timespec 2 0))))
		    (define was-sigalrm?
		      (= signo SIGALRM))
		    (define right-timer?
		      (= timer-id (px.struct-siginfo_t-si_value.sival_int info)))
		    (verbose-printf "received SIGALRM? ~a\n" was-sigalrm?)
		    (verbose-printf "from right timer? ~a\n" right-timer?)
		    (verbose-printf "number of expirations: ~a\n" (px.timer-getoverrun timer-id))
		    (%print-remaining-time timer-id)
		    (next (if (and was-sigalrm? right-timer?)
			      (+ 1 count)
			    count)))))
	  (px.timer-delete timer-id)
	  (px.signal-bub-final)))
    => 3)

  (check	;count expirations
      (with-result
       (let ((timer-id (px.timer-create CLOCK_REALTIME)))
	 (px.signal-bub-init)
	 (unwind-protect
	     (begin
	       (px.timer-settime timer-id 0
				 (px.make-struct-itimerspec
				  ;; one event every 1 seconds
				  (px.make-struct-timespec 1 0)
				  ;; the first event after 1 nanosecond
				  (px.make-struct-timespec 0 1)))
	       (px.nanosleep 0 1000000)
	       (px.signal-bub-acquire)
	       (let ((count (+ (if (px.signal-bub-delivered? SIGALRM) 1 0)
			       (px.timer-getoverrun timer-id))))
		 (add-result count)
		 (verbose-printf "after 0.1 seconds: ~a\n" count))
	       (px.nanosleep 3 0)
	       (px.signal-bub-acquire)
	       (let ((count (+ (if (px.signal-bub-delivered? SIGALRM) 1 0)
			       (px.timer-getoverrun timer-id))))
		 (add-result count)
		 (verbose-printf "after 3 seconds: ~a\n" count))
	       (px.nanosleep 1 0)
	       (px.signal-bub-acquire)
	       (let ((count (+ (if (px.signal-bub-delivered? SIGALRM) 1 0)
			       (px.timer-getoverrun timer-id))))
		 (add-result count)
		 (verbose-printf "after 1 second: ~a\n" count))
	       #t)
	   (px.timer-delete timer-id)
	   (px.signal-bub-final))))
    => '(#t (1 3 1)))

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
