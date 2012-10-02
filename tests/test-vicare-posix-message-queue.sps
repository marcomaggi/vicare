;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare posix), message queue
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


#!vicare
(import (vicare)
  (prefix (vicare posix)
	  px.)
  (vicare platform constants)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, message queues\n")


(parametrise ((check-test-name	'message-queue))

  (define MQ_OFLAG_1	(fxior O_CREAT O_EXCL O_RDWR O_NONBLOCK))
  (define MQ_OFLAG_2	(fxior                O_RDWR O_NONBLOCK))
  (define MQ_MODE_1	(fxior S_IRUSR S_IWUSR))
  (define MQ_ATTR_1	(px.make-struct-mq-attr 0 3 16 0))

;;; --------------------------------------------------------------------
;;; mq-setattr, mq-getattr

  (check	;mq-getattr
      (let ((name "/vicare-test-01"))
	(unwind-protect
	    (let ((mqd (px.mq-open name MQ_OFLAG_1 MQ_MODE_1 MQ_ATTR_1)))
	      (unwind-protect
		  (px.mq-getattr mqd)
		(px.mq-close mqd)))
	  (px.mq-unlink name)))
    (=> struct=?) (px.make-struct-mq-attr O_NONBLOCK 3 16 0))

  (check	;mq-setattr and check the old attributes
      (let ((name "/vicare-test-01"))
	(unwind-protect
	    (let ((mqd (px.mq-open name MQ_OFLAG_1 MQ_MODE_1 MQ_ATTR_1)))
	      (unwind-protect
		  (px.mq-setattr mqd (px.make-struct-mq-attr 0 10 8000 0))
		(px.mq-close mqd)))
	  (px.mq-unlink name)))
    (=> struct=?) (px.make-struct-mq-attr O_NONBLOCK 3 16 0))

  (check	;mq-setattr and check the new attributes
      (let ((name "/vicare-test-01"))
	(unwind-protect
	    (let ((mqd (px.mq-open name MQ_OFLAG_1 MQ_MODE_1 MQ_ATTR_1)))
	      (unwind-protect
		  (begin
		    (px.mq-setattr mqd (px.make-struct-mq-attr 0 10 8000 0))
		    (px.mq-getattr mqd))
		(px.mq-close mqd)))
	  (px.mq-unlink name)))
    (=> struct=?) (px.make-struct-mq-attr 0 3 16 0))

;;; --------------------------------------------------------------------
;;; send and receive

  (check
      (let ()
	(define name "/vicare-test-02")
	(define (parent child-pid)
	  (let ((mqd (px.mq-open name (fxior O_CREAT O_EXCL O_RDWR)
				 MQ_MODE_1 MQ_ATTR_1))
		(buf (make-bytevector 16)))
	    (unwind-protect
		(let-values (((len priority)
			      (px.mq-receive mqd buf)))
		  (guard (E (else #f))
		    (px.waitpid child-pid 0))
		  (list (subbytevector-u8 buf 0 len)
			priority))
	      (px.mq-close mqd)
	      (px.mq-unlink name))))
	(define (child)
	  (px.nanosleep 0 900000)
	  (let ((mqd (px.mq-open name O_RDWR MQ_MODE_1 MQ_ATTR_1)))
	    (unwind-protect
		(px.mq-send mqd '#ve(ascii "ciao") 1)
	      (px.mq-close mqd)))
	  (exit 0))
	(guard (E (else #f))
	  (px.mq-unlink name))
	(px.fork parent child))
    => '(#ve(ascii "ciao") 1))

;;; --------------------------------------------------------------------
;;; timed send and receive

  (check
      (let ()
	(define name "/vicare-test-02")
	(define timeout
	  (let ((T (px.clock-gettime CLOCK_REALTIME
				     (px.make-struct-timespec 0 0))))
	    (px.set-struct-timespec-tv_sec! T (+ 5 (px.struct-timespec-tv_sec T)))
	    T))
	(define (parent child-pid)
	  (let ((mqd		(px.mq-open name (fxior O_CREAT O_EXCL O_RDWR)
					    MQ_MODE_1 MQ_ATTR_1))
		(buf		(make-bytevector 16)))
	    (unwind-protect
		(let-values (((len priority)
			      (px.mq-timedreceive mqd buf timeout)))
		  (guard (E (else #f))
		    (px.waitpid child-pid 0))
		  (list (subbytevector-u8 buf 0 len)
			priority))
	      (px.mq-close mqd)
	      (px.mq-unlink name))))
	(define (child)
	  (px.nanosleep 0 900000)
	  (let ((mqd (px.mq-open name O_RDWR MQ_MODE_1 MQ_ATTR_1)))
	    (unwind-protect
		(px.mq-timedsend mqd '#ve(ascii "ciao") 1 timeout)
	      (px.mq-close mqd)))
	  (exit 0))
	(guard (E (else #f))
	  (px.mq-unlink name))
	(px.fork parent child))
    => '(#ve(ascii "ciao") 1))

  #t)


;;;; done

(check-report)

;;; end of file
