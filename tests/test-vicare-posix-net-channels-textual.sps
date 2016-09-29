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
;;;Copyright (C) 2013, 2014, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-vicare-posix-net-channels-textual)
  (options typed-language)
  (import (vicare)
    (vicare net channels)
    (prefix (vicare posix) px.)
    (vicare platform constants)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: textual net channels with POSIX network sockets\n")


;;;; helpers

(define-constant TRAN
  (make-transcoder (utf-8-codec) (eol-style none)))

(define (make-socket-ports-and-channels)
  (receive (sock1 sock2)
      (px.socketpair AF_LOCAL SOCK_STREAM 0)
    (let* ((port1 (make-textual-socket-input/output-port sock1 "sock1" TRAN))
	   (port2 (make-textual-socket-input/output-port sock2 "sock2" TRAN))
	   (chan1 (new <textual-input/output-channel> port1 port1))
	   (chan2 (new <textual-input/output-channel> port2 port2)))
      (push-compensation (close-port port1))
      (push-compensation (close-port port2))
      (push-compensation (delete chan1))
      (push-compensation (delete chan2))
      (port-set-non-blocking-mode! port1)
      (port-set-non-blocking-mode! port2)
      (values chan1 chan2))))


(parametrise ((check-test-name	'basic))

  (check	;transmit full message
      (with-compensations
	(receive (chan1 chan2)
	    (make-socket-ports-and-channels)
	  (.send-full-message chan2 "ciao\r\n")
	  (.recv-full-message chan1)))
    => "ciao\r\n")

  (check	;transmit message portion, then close
      (with-compensations
	(receive (chan1 chan2)
	    (make-socket-ports-and-channels)
	  ;;Send  message portion  without  terminator,  then close  the
	  ;;port.
	  (.send-begin! chan2)
	  (.send-message-portion! chan2 "ciao")
	  (close-port (.connect-ou-port chan2))
	  ;;We expect this reading to get EOF
	  (.recv-full-message chan1)))
    => (eof-object))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
