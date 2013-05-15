;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for net channels with sockets
;;;Date: Sat May 11, 2013
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
  (prefix (vicare net channels) chan.)
  (prefix (vicare posix) px.)
  (vicare platform constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: textual net channels with network sockets\n")


;;;; helpers

(define (make-socket-ports-and-channels)
  (receive (sock1 sock2)
      (px.socketpair AF_LOCAL SOCK_STREAM 0)
    (let* ((port1 (make-textual-socket-input/output-port sock1 "sock1" (native-transcoder)))
	   (port2 (make-textual-socket-input/output-port sock2 "sock2" (native-transcoder)))
	   (chan1 (chan.open-textual-input/output-channel port1 port1))
	   (chan2 (chan.open-textual-input/output-channel port2 port2)))
      (values port1 port2 chan1 chan2))))

(define (channel-send-full-message chan full-binary-message)
  (chan.channel-send-begin! chan)
  (chan.channel-send-message-portion! chan full-binary-message)
  (chan.channel-send-end! chan))

(define (channel-recv-full-message chan)
  (chan.channel-recv-begin! chan)
  (let loop ()
    (define rv
      (chan.channel-recv-message-portion! chan))
    (cond ((eof-object? rv)
	   (eof-object))
	  ((not rv)
	   (loop))
	  (else
	   (chan.channel-recv-end! chan)))))


(parametrise ((check-test-name	'basic))

  #;(check	;transmit full message
      (with-compensations
	(receive (port1 port2 chan1 chan2)
	    (make-socket-ports-and-channels)
	  (push-compensation
	   (close-port port1)
	   (close-port port2)
	   (chan.close-channel chan1)
	   (chan.close-channel chan2))
	  (channel-send-full-message chan2 "ciao\r\n")
	  #;(channel-recv-full-message chan1)))
    => '#ve(ascii "ciao\r\n"))

  #;(check	;transmit message portion, then close
      (with-compensations
	(receive (port1 port2 chan1 chan2)
	    (make-socket-ports-and-channels)
	  (push-compensation
	   (close-port port1)
	   (chan.close-channel chan1))

	  ;;Send  message portion  without  terminator,  then close  the
	  ;;port.
	  (chan.channel-send-begin! chan2)
	  (chan.channel-send-message-portion! chan2 "ciao")
	  (chan.close-channel chan2)
	  (close-port port2)

	  ;;We expect this reading to get EOF
	  (channel-recv-full-message chan1)))
    => (eof-object))

  #t)


;;;; done

(check-report)

;;; end of file
