;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for channels library
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


#!r6rs
(import (vicare)
  (prefix (vicare net channels)
	  chan.)
  (vicare custom-ports)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare channels library\n")


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
      (let ((chan (chan.open-input-channel in-port)))
        (list (chan.channel? chan)
	      (chan.input-channel? chan)
	      (chan.output-channel? chan)
	      (chan.input/output-channel? chan)
	      (chan.inactive-channel? chan)
	      (chan.sending-channel? chan)
	      (chan.receiving-channel? chan)))
    => '(#t #t #f #f #t #f #f))

   (check
      (let ((chan (chan.open-input-channel in-port)))
        (chan.close-channel chan))
    => (void))

   (check
       (let ((chan (chan.open-input-channel in-port)))
	 (chan.channel-reception-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #f #t))

   (check
       (let ((chan (chan.open-input-channel in-port)))
	 (chan.channel-reception-begin! chan)
	 (chan.channel-reception-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

;;; --------------------------------------------------------------------
;;; output channel

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(let ((chan (chan.open-output-channel port)))
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
	(let ((chan (chan.open-output-channel port)))
	  (chan.close-channel chan)))
    => (void))

   (check
       (let ((chan (chan.open-output-channel ou-port)))
	 (chan.channel-sending-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #t #f))

   (check
       (let ((chan (chan.open-output-channel ou-port)))
	 (chan.channel-sending-begin! chan)
	 (chan.channel-sending-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

;;; --------------------------------------------------------------------
;;; input/output channel

  (check
      (let-values (((port1 port2) (open-binary-input/output-port-pair)))
	(let ((chan (chan.open-input/output-channel port1)))
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
	(let ((chan (chan.open-input/output-channel port1)))
	  (chan.close-channel chan)))
    => (void))

   (check
       (let ((chan (chan.open-input/output-channel io-port)))
	 (chan.channel-sending-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #t #f))

   (check
       (let ((chan (chan.open-input/output-channel io-port)))
	 (chan.channel-sending-begin! chan)
	 (chan.channel-sending-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

   (check
       (let ((chan (chan.open-input/output-channel io-port)))
	 (chan.channel-reception-begin! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#f #f #t))

   (check
       (let ((chan (chan.open-input/output-channel io-port)))
	 (chan.channel-reception-begin! chan)
	 (chan.channel-reception-end! chan)
	 (list (chan.inactive-channel? chan)
	       (chan.sending-channel? chan)
	       (chan.receiving-channel? chan)))
     => '(#t #f #f))

  #t)


(parametrise ((check-test-name	'config))

  (check
      (let ((chan (chan.open-input-channel in-port)))
	(chan.channel-set-maximum-message-size! chan 1000))
    => (void))

  (check
      (let ((chan (chan.open-input-channel in-port)))
	(chan.channel-set-expiration-time! chan (time-from-now (make-time 10 0))))
    => (void))

  (check
      (let ((chan (chan.open-input-channel in-port)))
	(chan.channel-set-message-terminator! chan '#vu8(1 2 3)))
    => (void))

  #t)


;;;; done

(check-report)

;;; end of file
