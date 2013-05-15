;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for binary net channels library
;;;Date: Fri Apr  5, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (vicare net channels)
	  chan.)
  (vicare language-extensions custom-ports)
  (vicare language-extensions coroutines)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: net channels library, binary channels\n")


;;;; helpers

(define in-port (open-bytevector-input-port '#vu8(1 2 3)))

(define ou-port
  (let-values (((port extract) (open-bytevector-output-port)))
    port))

(define io-port
  (let-values (((port1 port2) (open-binary-input/output-port-pair)))
    port1))


(parametrise ((check-test-name	'struct))

;;; input channel

  (check
      (let ((chan (chan.open-binary-input-channel in-port)))
        (list (chan.channel? chan)
	      (chan.input-channel? chan)
	      (chan.output-channel? chan)
	      (chan.input/output-channel? chan)
	      (chan.inactive-channel? chan)
	      (chan.sending-channel? chan)
	      (chan.receiving-channel? chan)))
    => '(#t #t #f #f #t #f #f))

   (check
      (let ((chan (chan.open-binary-input-channel in-port)))
        (chan.close-channel chan))
    => (void))

   (check
       (let ((chan (chan.open-binary-input-channel in-port)))
	 (chan.channel-recv-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #f #t))

   (check
       (let ((chan (chan.open-binary-input-channel in-port)))
	 (chan.channel-recv-begin! chan)
	 (chan.channel-recv-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

;;; --------------------------------------------------------------------
;;; output channel

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(let ((chan (chan.open-binary-output-channel port)))
	  (list (chan.channel? chan)
		(chan.input-channel? chan)
		(chan.output-channel? chan)
		(chan.input/output-channel? chan)
		(chan.inactive-channel? chan)
		(chan.sending-channel? chan)
		(chan.receiving-channel? chan))))
    => '(#t #f #t #f #t #f #f))

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(let ((chan (chan.open-binary-output-channel port)))
	  (chan.close-channel chan)))
    => (void))

   (check
       (let ((chan (chan.open-binary-output-channel ou-port)))
	 (chan.channel-send-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #t #f))

   (check
       (let ((chan (chan.open-binary-output-channel ou-port)))
	 (chan.channel-send-begin! chan)
	 (chan.channel-send-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

;;; --------------------------------------------------------------------
;;; input/output channel

  (check
      (let-values (((port1 port2) (open-binary-input/output-port-pair)))
	(let ((chan (chan.open-binary-input/output-channel port1)))
	  (list (chan.channel? chan)
		(chan.input-channel? chan)
		(chan.output-channel? chan)
		(chan.input/output-channel? chan)
		(chan.inactive-channel? chan)
		(chan.sending-channel? chan)
		(chan.receiving-channel? chan))))
    => '(#t #t #t #t #t #f #f))

  (check
      (let-values (((port1 port2) (open-binary-input/output-port-pair)))
	(let ((chan (chan.open-binary-input/output-channel port1)))
	  (chan.close-channel chan)))
    => (void))

   (check
       (let ((chan (chan.open-binary-input/output-channel io-port)))
	 (chan.channel-send-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #t #f))

   (check
       (let ((chan (chan.open-binary-input/output-channel io-port)))
	 (chan.channel-send-begin! chan)
	 (chan.channel-send-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

   (check
       (let ((chan (chan.open-binary-input/output-channel io-port)))
	 (chan.channel-recv-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #f #t))

   (check
       (let ((chan (chan.open-binary-input/output-channel io-port)))
	 (chan.channel-recv-begin! chan)
	 (chan.channel-recv-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

  #t)


(parametrise ((check-test-name	'config))

  (check
      (let ((chan (chan.open-binary-input-channel in-port)))
	(chan.channel-set-maximum-message-size! chan 1000))
    => (void))

;;; --------------------------------------------------------------------

  (check
      (let ((chan (chan.open-binary-input-channel in-port)))
	(chan.channel-set-expiration-time! chan (time-from-now (make-time 10 0))))
    => (void))

  (check
      (let ((chan (chan.open-binary-input-channel in-port)))
	(chan.channel-set-expiration-time! chan #f))
    => (void))

;;; --------------------------------------------------------------------

  (check
      (let ((chan (chan.open-binary-input-channel in-port)))
	(chan.channel-set-message-terminators! chan '(#vu8(1 2 3))))
    => (void))

  #t)


(parametrise ((check-test-name	'messages))

  (define (ascii-chunks str-chunks)
    (map string->ascii str-chunks))

  (define (send port chan chunks)
    ;;For  debugging purposes  we want  to flush  the output  port after
    ;;every portion is sent.
    ;;
    (chan.channel-send-begin! chan)
    (for-each-in-order
	(lambda (portion)
	  (chan.channel-send-message-portion! chan portion)
	  (flush-output-port port)
	  (yield))
      chunks)
    (chan.channel-send-end! chan))

  (define (recv chan)
    (chan.channel-recv-begin! chan)
    (let loop ()
      (define rv
	(chan.channel-recv-message-portion! chan))
      (cond ((not rv)
	     (yield)
	     (loop))
	    ((eof-object? rv)
	     (eof-object))
	    (else
	     (let ((bv (chan.channel-recv-end! chan)))
	       (ascii->string bv))))))

  (define (master-log obj)
    (add-result (list 'master-recv obj)))

  (define (slave-log obj)
    (add-result (list 'slave-recv obj)))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let-values (((master.port slave.port) (open-binary-input/output-port-pair)))
	 (coroutine	;master
	     (lambda ()
	       (let ((chan (chan.open-binary-input/output-channel master.port))
		     (log  master-log))
		 (chan.channel-set-message-terminators! chan '(#ve(ascii "\r\n\r\n")))
		 (send master.port chan
		       (ascii-chunks '("hel" "lo sla" "ve\r\n\r\n")))
		 (log (recv chan))
		 (send master.port chan
		       (ascii-chunks '("som" "e dat" "a\r" "\n"
				       "som" "e other dat" "a\r" "\n" "\r" "\n")))
		 (log (recv chan))
		 (send master.port chan (ascii-chunks '("quit\r\n\r\n")))
		 (chan.close-channel chan))))
	 (coroutine	;slave
	     (lambda ()
	       (let ((chan (chan.open-binary-input/output-channel slave.port))
		     (log  slave-log))
		 (chan.channel-set-message-terminators! chan '(#ve(ascii "\r\n\r\n")))
		 (log (recv chan))
		 (send slave.port chan (ascii-chunks '("hel" "lo mas" "ter\r\n\r\n")))
		 (log (recv chan))
		 (send slave.port chan (ascii-chunks '("OK" "\r\n" "\r\n")))
		 (log (recv chan))
		 (chan.close-channel chan))))
	 (finish-coroutines)))
    => `(,(void)
	 ((slave-recv "hello slave\r\n\r\n")
	  (master-recv "hello master\r\n\r\n")
	  (slave-recv "some data\r\nsome other data\r\n\r\n")
	  (master-recv "OK\r\n\r\n")
	  (slave-recv "quit\r\n\r\n")
	  )))

  #t)


(parametrise ((check-test-name	'sending))

  (check	;max message size error
      (let ((chan (chan.open-binary-output-channel ou-port)))
	(chan.channel-set-maximum-message-size! chan 2)
	(chan.channel-send-begin! chan)
	(guard (E (else
		   (list (who-condition? E)
			 (message-condition? E)
			 (chan.channel-condition? E)
			 (chan.maximum-message-size-exceeded-condition? E)
			 )))
	  (chan.channel-send-message-portion! chan '#vu8(1 2 3))))
    => '(#t #t #t #t))

;;; --------------------------------------------------------------------

  (check	;timeout expired error
      (let ((chan (chan.open-binary-output-channel ou-port)))
	(chan.channel-set-expiration-time! chan (current-time))
	(chan.channel-send-begin! chan)
	(guard (E (else
		   (list (who-condition? E)
			 (message-condition? E)
			 (chan.channel-condition? E)
			 (chan.delivery-timeout-expired-condition? E)
			 )))
	  (chan.channel-send-message-portion! chan '#vu8(1 2 3))))
    => '(#t #t #t #t))

  #t)


(parametrise ((check-test-name	'receiving))

  (check	;max message size error
      (let ((chan (chan.open-binary-input-channel (open-bytevector-input-port '#vu8(1 2 3)))))
	(chan.channel-set-maximum-message-size! chan 2)
	(chan.channel-recv-begin! chan)
	(guard (E (else
		   (list (who-condition? E)
			 (message-condition? E)
			 (chan.channel-condition? E)
			 (chan.maximum-message-size-exceeded-condition? E)
			 )))
	  (chan.channel-recv-message-portion! chan)))
    => '(#t #t #t #t))

;;; --------------------------------------------------------------------

  (check	;timeout expired error
      (let ((chan (chan.open-binary-input-channel in-port)))
	(chan.channel-set-expiration-time! chan (current-time))
	(chan.channel-recv-begin! chan)
	(guard (E (else
		   (list (who-condition? E)
			 (message-condition? E)
			 (chan.channel-condition? E)
			 (chan.delivery-timeout-expired-condition? E)
			 )))
	  (chan.channel-recv-message-portion! chan)))
    => '(#t #t #t #t))

  #t)


;;;; done

(check-report)

;;; end of file
