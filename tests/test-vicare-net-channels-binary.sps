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
;;;Copyright (C) 2013, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-net-channels-binary)
  (options typed-language)
  (import (vicare)
    (vicare net channels)
    (vicare language-extensions custom-ports)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: net channels library, binary channels\n")


;;;; helpers

(define in-port (open-bytevector-input-port '#vu8(1 2 3)))

(define ou-port
  (receive (port extract)
      (open-bytevector-output-port)
    port))

(define io-port
  (receive (port1 port2)
      (open-binary-input/output-port-pair)
    port1))


(parametrise ((check-test-name	'object))

;;; input channel

  (check
      (let ((chan (new <binary-input-only-channel> in-port)))
        (list (is-a? chan <channel>)
	      (is-a? chan <input-channel>)
	      (is-a? chan <output-channel>)
	      (.inactive?  chan)
	      (.sending?   chan)
	      (.receiving? chan)))
    => '(#t #t #f #t #f #f))

  (check
       (let ((chan (new <binary-input-only-channel> in-port)))
	 (void-object? (delete chan)))
    => #t)

   (check
       (let ((chan (new <binary-input-only-channel> in-port)))
	 (.recv-begin! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#f #f #t))

   (check
       (let ((chan (new <binary-input-only-channel> in-port)))
	 (.recv-begin! chan)
	 (.recv-end! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#t #f #f))

;;; --------------------------------------------------------------------
;;; output channel

  (check
      (receive (port extract)
	  (open-bytevector-output-port)
	(let ((chan (new <binary-output-only-channel> port)))
	  (list (is-a? chan <channel>)
                (is-a? chan <input-channel>)
		(is-a? chan <output-channel>)
		(.inactive? chan)
		(.sending? chan)
		(.receiving? chan))))
    => '(#t #f #t #t #f #f))

  (check
      (receive (port extract)
	  (open-bytevector-output-port)
	(let ((chan (new <binary-output-only-channel> port)))
	  (void-object? (delete chan))))
    => #t)

   (check
       (let ((chan (new <binary-output-only-channel> ou-port)))
	 (.send-begin! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#f #t #f))

   (check
       (let ((chan (new <binary-output-only-channel> ou-port)))
	 (.send-begin! chan)
	 (.send-end! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#t #f #f))

  #t)


(parametrise ((check-test-name	'config))

  (check
      (let ((chan (new <binary-input-only-channel> in-port)))
        (void-object? (.maximum-message-size chan 1000)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((chan	(new <binary-input-only-channel> in-port))
	    (ET		(.+ (current-time) (new <time> 10 0))))
	(.expiration-time chan ET)
	(equal? ET (.expiration-time chan)))
    => #t)

  (check
      (let ((chan	(new <binary-input-only-channel> in-port))
	    (ET		(faraway-time)))
	(.expiration-time chan ET)
	(equal? ET (.expiration-time chan)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((chan (new <binary-input-only-channel> in-port)))
	(.message-terminators chan '#(#vu8(1 2 3)))
	(.message-terminators chan))
    => '#(#vu8(1 2 3)))

  (void))


(parametrise ((check-test-name	'messages))

  (define (ascii-chunks str-chunks)
    (map string->ascii str-chunks))

  (define (send port chan chunks)
    ;;For  debugging purposes  we want  to flush  the output  port after
    ;;every portion is sent.
    ;;
    (.send-begin! chan)
    (for-each-in-order
	(lambda (portion)
	  (.send-message-portion! chan portion)
	  (flush-output-port port)
	  (yield))
      chunks)
    (.send-end! chan))

  (define (recv chan)
    (.recv-begin! chan)
    (let loop ()
      (let ((rv (.recv-message-portion! chan)))
	(cond ((not rv)
	       (yield)
	       (loop))
	      ((eof-object? rv)
	       (eof-object))
	      (else
	       (let ((bv (.recv-end! chan)))
		 (ascii->string bv)))))))

  (define (master-log obj)
    (add-result (list 'master-recv obj)))

  (define (slave-log obj)
    (add-result (list 'slave-recv obj)))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(receive (master.port slave.port)
	    (open-binary-input/output-port-pair)
	  (coroutine ;master
	      (lambda ()
		(let ((chan (new <binary-input/output-channel> master.port master.port))
		      (log  master-log))
		  (.message-terminators chan '#(#ve(ascii "\r\n\r\n")))
		  (send master.port chan
			(ascii-chunks '("hel" "lo sla" "ve\r\n\r\n")))
		  (log (recv chan))
		  (send master.port chan
			(ascii-chunks '("som" "e dat" "a\r" "\n"
					"som" "e other dat" "a\r" "\n" "\r" "\n")))
		  (log (recv chan))
		  (send master.port chan (ascii-chunks '("quit\r\n\r\n")))
		  (delete chan)
		  (close-port master.port))))
	  (coroutine ;slave
	      (lambda ()
		(let ((chan (new <binary-input/output-channel> slave.port slave.port))
		      (log  slave-log))
		  (.message-terminators chan '#(#ve(ascii "\r\n\r\n")))
		  (log (recv chan))
		  (send slave.port chan (ascii-chunks '("hel" "lo mas" "ter\r\n\r\n")))
		  (log (recv chan))
		  (send slave.port chan (ascii-chunks '("OK" "\r\n" "\r\n")))
		  (log (recv chan))
		  (delete chan)
		  (close-port slave.port))))
	  (void-object? (finish-coroutines))))
    => `(#t
	 ((slave-recv "hello slave\r\n\r\n")
	  (master-recv "hello master\r\n\r\n")
	  (slave-recv "some data\r\nsome other data\r\n\r\n")
	  (master-recv "OK\r\n\r\n")
	  (slave-recv "quit\r\n\r\n")
	  )))

  #t)


(parametrise ((check-test-name	'sending))

  (check	;max message size error
      (let ((chan (new <binary-output-only-channel> ou-port)))
	(.maximum-message-size chan 2)
	(.send-begin! chan)
	(try
	    (.send-message-portion! chan '#vu8(1 2 3))
	  (catch E
	    ((&maximum-message-size-exceeded)
	     #t)
	    (else E))))
    => #t)

;;; --------------------------------------------------------------------

  (check	;timeout expired error
      (let ((chan (new <binary-output-only-channel> ou-port)))
        (.expiration-time chan (current-time))
	(.send-begin! chan)
	(try
	    (.send-message-portion! chan '#vu8(1 2 3))
	  (catch E
	    ((&delivery-timeout-expired)
	     #t)
	    (else E))))
    => #t)

  #t)


(parametrise ((check-test-name	'receiving))

  (check	;max message size error
      (let ((chan (new <binary-input-only-channel> (open-bytevector-input-port '#vu8(1 2 3)))))
	(.maximum-message-size chan 2)
	(.recv-begin! chan)
	(try
	    (.recv-message-portion! chan)
	  (catch E
	    ((&maximum-message-size-exceeded)
	     #t)
	    (else E))))
    => #t)

;;; --------------------------------------------------------------------

  (check	;timeout expired error
      (let ((chan (new <binary-input-only-channel> in-port)))
	(.expiration-time chan (current-time))
	(.recv-begin! chan)
	(try
	    (.recv-message-portion! chan)
	  (catch E
	    ((&delivery-timeout-expired)
	     #t)
	    (else E))))
    => #t)

  #!void)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
